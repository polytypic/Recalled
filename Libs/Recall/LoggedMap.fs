[<AutoOpen>]
module internal Recall.LoggedMap

open Hopac
open Hopac.Infixes
open Hopac.Extensions
open Hopac.Job.Infixes
open Hopac.Alt.Infixes
open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open System.Threading

type IOReq<'k, 'v> =
 | Log of int * 'k * 'v
 | Close of IVar<unit>

type Entry<'v> = {
  mutable idx: int
  mutable var: IVar<option<'v>>
}

type LoggedMap<'k, 'v> when 'k: equality = {
  addIdx: ref<int>
  ready: IVar<Option<'v>>
  dict: Dictionary<'k, Entry<'v>>
  ioReqs: Mailbox<IOReq<'k, 'v>>
}

module LoggedMap =
  let addFile logDir = logDir + "/.add"
  let remFile logDir = logDir + "/.rem"

  let inline inc i = let j = !i + 1 in i := j ; j

  let tryFind (t: LoggedMap<'k, 'v>) (k: 'k) : Alt<Option<'v>> = Alt.delay <| fun () ->
    lock t.dict <| fun () ->
    match t.dict.TryGetValue k with
     | Nothing ->
       if IVar.Now.isFull t.ready then
         asAlt t.ready
       else
         let v = ivar ()
         t.dict.Add (k, {idx = -1; var = v})
         asAlt v
     | Just entry ->
       asAlt entry.var

  let add (t: LoggedMap<'k, 'v>) (k: 'k) (v: 'v) =
    t.ready >>= fun _ ->
    let entry = {idx = 0; var = IVar.Now.createFull (Some v)}
    let idx =
      lock t.dict <| fun () ->
        let i = inc t.addIdx
        match t.dict.TryGetValue k with
         | Nothing ->
           entry.idx <- i
           t.dict.Add (k, entry)
           0
         | Just old ->
           let idx = old.idx
           old.idx <- i
           old.var <- entry.var
           idx
    t.ioReqs <<-+ Log (idx, k, v)

  let close t =
    let reply = ivar ()
    t.ioReqs <<-+ Close reply >>.
    reply

  let readAllBytes path = job {
    use s = new FileStream (path,
                            FileMode.OpenOrCreate,
                            FileAccess.ReadWrite,
                            FileShare.ReadWrite,
                            8192,
                            true)
    let n = int32 s.Length
    let buffer = Array.zeroCreate n
    let i = ref 0
    while !i < n do
      let! n = s.ReadAsync (buffer, !i, n - !i)
      do if n <= 0 then
           failwithf "Couldn't read the whole file \"%s\"" path
         else
           i := !i + n
    return buffer
  }

  let server (t: LoggedMap<'k, 'v>) (logDir: string) (domPU: PU<'k>) (codPU: PU<'v>) = job {
    try
      let addPath = addFile logDir
      let remPath = remFile logDir

      let! remDataPromise = Promise.queue <| job {
        // XXX Optimize
        let! buffer = readAllBytes remPath
        let n = buffer.Length / 4
        let data = Array.zeroCreate (n+1)
        for i=0 to n-1 do
          data.[i] <- BitConverter.ToInt32 (buffer, i*4)
        do data.[n] <- Int32.MaxValue
        return data
      }

      let! addBuffer = readAllBytes addPath
      use addStream = new MemoryStream (addBuffer)
      use addReader = new BinaryReader (addStream)

      let! remData = remDataPromise
      let remIdx = ref 0

      while addStream.Position <> addStream.Length do
        let k = domPU.Unpickle addReader
        let v = Some (codPU.Unpickle addReader)

        let addIdx = inc t.addIdx

        do while remData.[!remIdx] < addIdx do
             remIdx := !remIdx + 1

        if addIdx < remData.[!remIdx] then
          let entry = {idx = addIdx; var = IVar.Now.createFull v}
          do lock t.dict <| fun () ->
            match t.dict.TryGetValue k with
             | Nothing ->
               t.dict.Add (k, entry)
             | Just request ->
               if IVar.Now.isFull entry.var then
                 failwith "Bug"
               request.idx <- entry.idx
               entry.var <- request.var
          if not (IVar.Now.isFull entry.var) then
            do! entry.var <-= v
          else
            return ()

      // Find out which concurrent find operations were unsatisfied.
      let unsatisfied = ResizeArray<_> ()

      // Must keep map locked...
      do Monitor.Enter t.dict

      do for kvI in t.dict do
           let entry = kvI.Value
           if not (IVar.Now.isFull entry.var) then
             unsatisfied.Add kvI
         for kvI in unsatisfied do
           t.dict.Remove kvI.Key |> ignore

      /// ...until can declare read phase ready.
      do! t.ready <-= None
      do Monitor.Exit t.dict

      /// Answer the unsatisfied finds.
      do! unsatisfied
          |> Seq.iterJob (fun kvI ->
             kvI.Value.var <-= None)

      let openWriter path =
        let s = new FileStream (path, FileMode.Open, FileAccess.ReadWrite, FileShare.ReadWrite)
        do s.Seek (0L, SeekOrigin.End) |> ignore
        new BinaryWriter (s)

      let remWriter = openWriter remPath
      let addWriter = openWriter addPath

      printfn "Log: %d" (!t.addIdx)

      return!
        Job.foreverServer
         (Job.tryWith
           (t.ioReqs >>= function
             | Log (old, k, v) ->
               if 0 < old then
                 remWriter.Write (old)
               domPU.Dopickle (addWriter, k)
               codPU.Dopickle (addWriter, v)
               Job.unit ()
             | Close reply ->
               remWriter.Close ()
               addWriter.Close ()
               reply <-= ())
           (fun e ->
             printf "Log: %A %s\n" e e.StackTrace
             Job.unit ()))

    with e ->
      return! IVar.fillFailure t.ready e
  }

  let create (logDir: string) (domPU: PU<'k>) (codPU: PU<'v>) = job {
    do Directory.CreateDirectory logDir |> ignore
    let t =
     {addIdx = ref 0
      ready = ivar ()
      dict = Dictionary<_, _> ()
      ioReqs = mb ()}
    do! Job.queue (server t logDir domPU codPU)
    return t
  }
