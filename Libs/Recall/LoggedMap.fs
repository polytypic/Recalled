namespace Recall

open Microsoft.FSharp.NativeInterop
open Hopac
open Hopac.Infixes
open Hopac.Extensions
open Hopac.Job.Infixes
open Hopac.Alt.Infixes
open System
open System.Diagnostics
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open System.Threading

module LoggedMap =
  type Info = {
      DepKeyDigests: array<Digest>
      DepDigest: Digest
      BobDigest: Digest
      BobOffset: PtrInt
      BobSize: int
      AddOffset: PtrInt
    }

  type Entry = {
      mutable Idx: int
      mutable Info: IVar<option<Info>>
    }

  type LoggedMap = {
      AddIdx: MVar<int>
      Ready: IVar<option<Info>>
      Dict: Dictionary<Digest, Entry>
      AddBuf: MemMapBuf.MemMapBuf
      RemBuf: MemMapBuf.MemMapBuf
      BobBuf: MemMapBuf.MemMapBuf
    }

  let close loggedMap =
    loggedMap.AddIdx >>= fun _ ->
    Promise.startAsAlt
     (MemMapBuf.close loggedMap.AddBuf >>= fun addClosed ->
      MemMapBuf.close loggedMap.RemBuf >>= fun remClosed ->
      MemMapBuf.close loggedMap.BobBuf >>= fun bobClosed ->
      addClosed >>. remClosed >>. bobClosed)

  let tryFind (loggedMap: LoggedMap)
              (keyDigest: Digest) : Alt<option<Info>> = Alt.delay <| fun () ->
    Monitor.Enter loggedMap.Dict
    match loggedMap.Dict.TryGetValue keyDigest with
     | Nothing ->
       if IVar.Now.isFull loggedMap.Ready then
         Monitor.Exit loggedMap.Dict
         upcast loggedMap.Ready
       else
         let info = ivar ()
         loggedMap.Dict.Add (keyDigest, {Idx = -1; Info = info})
         Monitor.Exit loggedMap.Dict
         upcast info
     | Just entry ->
       Monitor.Exit loggedMap.Dict
       upcast entry.Info

  let readFun (loggedMap: LoggedMap)
              (readFun: nativeptr<byte> -> 'x) : Job<'x> =
    MemMapBuf.accessFun loggedMap.BobBuf readFun

  let justWrite (loggedMap: LoggedMap)
                (entry: Entry)
                (keyDigest: Digest)
                (depKeyDigests: array<Digest>)
                (depDigest: Digest)
                (bobSize: int)
                (bobWrite: nativeptr<byte> -> unit)
                (addOffs: PtrInt)
                (bobOffs: PtrInt) =
    MemMapBuf.accessFun loggedMap.BobBuf (fun ptr ->
      let ptr = NativePtr.ofNativeInt (NativePtr.toNativeInt ptr + nativeint bobOffs)
      bobWrite ptr
      let pos = ref 0
      let inline notImplemented () = raise <| NotImplementedException ()
      let bufStream =
        {new Stream () with
          override this.CanRead with get () = true
          override this.CanSeek with get () = false
          override this.CanWrite with get () = false
          override this.Write (_, _, _) = notImplemented ()
          override this.Seek (_, _) = notImplemented ()
          override this.Flush () = ()
          override this.Length with get () = int64 bobSize
          override this.Position
            with get () = int64 (!pos)
             and set v = notImplemented ()
          override this.SetLength _ = notImplemented ()
          override this.ReadByte () =
            let p = !pos
            if p < bobSize then
              pos := p + 1
              int (NativePtr.get ptr p)
            else
              -1
          override this.Read (buffer, offset, count) =
            let p = !pos
            let n = min count (bobSize - !pos)
            for i=0 to n-1 do
              buffer.[offset+i] <- NativePtr.get ptr (p+i)
            pos := p + n
            n}
      Digest.Stream bufStream) >>= fun (bobDigest: Digest) ->

    MemMapBuf.accessFun loggedMap.AddBuf (fun ptr ->
      let p = int64 (NativePtr.toNativeInt ptr) + addOffs

      let inline writeDigest (dig: Digest) p =
        p
        |> writeIfChanged dig.Lo
        |> writeIfChanged dig.Hi

      let mutable p =
        p
        |> writeDigest keyDigest
        |> writeDigest bobDigest
        |> writeIfChanged bobSize
        |> writeIfChanged depKeyDigests.Length

      if 0 <> depKeyDigests.Length then
        for i=0 to depKeyDigests.Length-1 do
          p <- writeDigest depKeyDigests.[i] p
        p <- writeDigest depDigest p) >>= fun () ->
    let info = {
        DepKeyDigests = depKeyDigests
        DepDigest = depDigest
        BobDigest = bobDigest
        BobOffset = bobOffs
        BobSize = bobSize
        AddOffset = addOffs
      }
    entry.Info <-= Some info >>%
    info

  let justAdd (loggedMap: LoggedMap)
              (entry: Entry)
              (keyDigest: Digest)
              (depKeyDigests: array<Digest>)
              (depDigest: Digest)
              (bobSize: int)
              (bobWrite: nativeptr<byte> -> unit) : Job<Info> =
    let addEntrySize =
      0L
      // keyDigest
      |> incBy sizeof<Digest>
      // bobDigest
      |> incBy sizeof<Digest>
      // bobSize
      |> incBy sizeof<int32>
      // depKeyDigests
      |> incBy sizeof<int32>
    let addEntrySize =
      if 0 <> depKeyDigests.Length then
        addEntrySize
        |> incBy (depKeyDigests.Length * sizeof<Digest>)
        // depDigest
        |> incBy sizeof<Digest>
      else
        addEntrySize
    let addEntrySize = int addEntrySize

    loggedMap.AddIdx >>= fun idx ->
    let idx = idx + 1
    MemMapBuf.append loggedMap.BobBuf sizeof<int64> bobSize >>= fun bobOffs ->
    MemMapBuf.append loggedMap.AddBuf sizeof<int64> addEntrySize >>= fun addOffs ->
    loggedMap.AddIdx <<-= idx >>= fun () ->

    entry.Idx <- idx

    justWrite loggedMap entry keyDigest depKeyDigests depDigest bobSize bobWrite addOffs bobOffs

  let add (loggedMap: LoggedMap)
          (keyDigest: Digest)
          (depKeyDigests: array<Digest>)
          (depDigest: Digest)
          (bobSize: int)
          (bobWrite: nativeptr<byte> -> unit) : Job<Info> =
    loggedMap.Ready >>= fun _ ->
    let entry = {Idx = 0; Info = ivar ()}
    Monitor.Enter loggedMap.Dict
    match loggedMap.Dict.TryGetValue keyDigest with
     | Nothing ->
       loggedMap.Dict.Add (keyDigest, entry)
       Monitor.Exit loggedMap.Dict
       justAdd loggedMap entry keyDigest depKeyDigests depDigest bobSize bobWrite
     | Just old ->
       Monitor.Exit loggedMap.Dict
       old.Info >>= function
        | None -> failwith "Bug"
        | Some oldInfo ->
          if oldInfo.BobSize = bobSize &&
             oldInfo.DepKeyDigests.Length = depKeyDigests.Length then

            justWrite loggedMap entry keyDigest depKeyDigests depDigest
             bobSize bobWrite oldInfo.AddOffset oldInfo.BobOffset

          else
            let remIdx = old.Idx
            old.Info <- entry.Info

            MemMapBuf.append loggedMap.RemBuf sizeof<int> sizeof<int> >>= fun remOffs ->
            MemMapBuf.accessFun
             loggedMap.RemBuf
             (fun ptr ->
                int64 (NativePtr.toNativeInt ptr) + remOffs
                |> writeIfChanged remIdx
                |> ignore) >>.
            justAdd loggedMap entry keyDigest depKeyDigests depDigest bobSize bobWrite

  type ReadInfo = {
      mutable addIdx: int
      mutable bobOffset: int64
      mutable keyDigest: Digest
      mutable bobDigest: Digest
      mutable bobSize: int
      mutable numDeps: int
      mutable depDigest: Digest
    }

  let inline readDigest (dig: byref<Digest>) p =
    p
    |> readTo (&dig.Lo)
    |> readTo (&dig.Hi)

  let server (loggedMap: LoggedMap) = job {
    try
      do! MemMapBuf.accessJob loggedMap.AddBuf <| fun ptr -> job {
            let start = int64 (NativePtr.toNativeInt ptr)
            let stop = start + MemMapBuf.size loggedMap.AddBuf

            let pos = ref start

            let ri : ReadInfo = {
                addIdx    = 0
                bobOffset = 0L
                keyDigest = Unchecked.defaultof<_>
                bobDigest = Unchecked.defaultof<_>
                bobSize   = 0
                numDeps   = 0
                depDigest = Unchecked.defaultof<_>
              }

            while !pos < stop do

              let addOffset = !pos - start

              do pos := !pos
                        |> readDigest &ri.keyDigest
                        |> readDigest &ri.bobDigest
                        |> readTo &ri.bobSize
                        |> readTo &ri.numDeps

              let depKeyDigests = Array.zeroCreate ri.numDeps
              do if 0 <> ri.numDeps then
                   for i=0 to ri.numDeps-1 do
                     pos := !pos |> readDigest (&depKeyDigests.[i])
                   pos := !pos |> readDigest (&ri.depDigest)
                 else
                   ri.depDigest <- Unchecked.defaultof<_>

              do ri.addIdx <- ri.addIdx + 1

              let info = Some {
                  DepKeyDigests = depKeyDigests
                  DepDigest = ri.depDigest
                  BobDigest = ri.bobDigest
                  BobOffset = ri.bobOffset
                  BobSize = ri.bobSize
                  AddOffset = addOffset
                }

              do ri.bobOffset <- alignTo sizeof<int64> (incBy ri.bobSize ri.bobOffset)

              let entry = {Idx = ri.addIdx; Info = IVar.Now.createFull info}

              do Monitor.Enter loggedMap.Dict
              match loggedMap.Dict.TryGetValue ri.keyDigest with
               | Nothing ->
                 do loggedMap.Dict.Add (ri.keyDigest, entry)
                    Monitor.Exit loggedMap.Dict
               | Just request ->
                 do Monitor.Exit loggedMap.Dict
                    if IVar.Now.isFull request.Info then
                      failwith "Bug"
                    request.Idx <- entry.Idx
                 return! request.Info <-= info

            do! loggedMap.AddIdx <<-= ri.addIdx
          }

      // Find out which concurrent find operations were unsatisfied.
      let unsatisfied = ResizeArray<_> ()

      // Must keep map locked...
      do Monitor.Enter loggedMap.Dict

      do for kvI in loggedMap.Dict do
           let entry = kvI.Value
           if not (IVar.Now.isFull entry.Info) then
             unsatisfied.Add kvI
         for kvI in unsatisfied do
           loggedMap.Dict.Remove kvI.Key |> ignore

      /// ...until can declare read phase ready.
      do! loggedMap.Ready <-= None
      do Monitor.Exit loggedMap.Dict

      /// Answer the unsatisfied finds.
      do! unsatisfied
          |> Seq.iterJob (fun kvI ->
             kvI.Value.Info <-= None)

    with e ->
      return! IVar.fillFailure loggedMap.Ready e
  }

  let inline addFile logDir = logDir + "/.add"
  let inline remFile logDir = logDir + "/.rem"
  let inline bobFile logDir = logDir + "/.bob"

  let create (logDir: string) = job {
    do Directory.CreateDirectory logDir |> ignore
    let! addBuf = MemMapBuf.create (addFile logDir)
    let! remBuf = MemMapBuf.create (remFile logDir)
    let! bobBuf = MemMapBuf.create (bobFile logDir)
    let loggedMap =
     {AddIdx = mvar ()
      Ready = ivar ()
      Dict = Dictionary<_, _> (DigestEqualityComparer ())
      AddBuf = addBuf
      RemBuf = remBuf
      BobBuf = bobBuf}
    do! Job.queue (server loggedMap)
    return loggedMap
  }
