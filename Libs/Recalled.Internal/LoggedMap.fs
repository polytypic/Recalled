namespace Recalled.Internal

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
      Digest.Bytes (ptr, bobSize)) >>= fun (bobDigest: Digest) ->

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

    let bobSize' = (bobSize + 7) &&& ~~~ 7 // Ensure BobBuf always aligned size

    loggedMap.AddIdx >>= fun idx ->
    let idx = idx + 1
    MemMapBuf.append loggedMap.BobBuf sizeof<int64> bobSize' >>= fun bobOffs ->
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
    if 0UL = (keyDigest.Lo ||| keyDigest.Hi) then
      failwith "Sorry, key digest of Zero not allowed!"
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
      mutable remIdx: int
      mutable pos: int64
      mutable stop: int64
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

  let rec findLastNon0 ptr cnt =
    if 0 < cnt && 0 = NativePtr.get ptr (cnt-1) then
      findLastNon0 ptr (cnt-1)
    else
      cnt

  let inline swap (x: byref<'x>) (y: byref<'x>) =
    let t = x
    x <- y
    y <- t

  let partitionMed3 (ptr: nativeptr<int32>) start stop =
    let inline get i = NativePtr.get ptr i
    let inline set i x = NativePtr.set ptr i x
    let inline swp i j =
      let t = get i
      set i (get j)
      set j t
    let mutable min = get start
    let mutable mid = get (start + (stop - start) / 2)
    let mutable max = get (stop - 1)
    if mid < min then swap &min &mid
    if max < mid then
      swap &mid &max
      if mid < min then swap &min &mid
    let mutable i = start
    let mutable j = stop - 1
    while get j > mid do j <- j-1
    while get i < mid do i <- i+1
    while i < j do
      swp i j
      j <- j-1 ; while get j > mid do j <- j-1
      i <- i+1 ; while get i < mid do i <- i+1
    j+1

  let rec quickSortMed3 (ptr: nativeptr<int32>) start stop =
    if 2 <= stop - start then
      let middle = partitionMed3 ptr start stop
      quickSortMed3 ptr start middle
      quickSortMed3 ptr middle stop

  let server (loggedMap: LoggedMap) = job {
    try
      do! MemMapBuf.accessJob loggedMap.RemBuf <| fun remBufPtr -> job {
          let remBufPtr : nativeptr<int32> =
            NativePtr.ofNativeInt (NativePtr.toNativeInt remBufPtr)
          let remBufCnt =
            int32 (MemMapBuf.size loggedMap.RemBuf / int64 sizeof<int32>)

          let remBufCnt = findLastNon0 remBufPtr remBufCnt

          do MemMapBuf.Unsafe.truncate loggedMap.RemBuf
              (int64 sizeof<int32> * int64 remBufCnt)

          do quickSortMed3 remBufPtr 0 remBufCnt

          let! _ = MemMapBuf.flush loggedMap.RemBuf

          do! MemMapBuf.accessJob loggedMap.AddBuf <| fun addBufPtr -> job {
              let addBufBeg = int64 (NativePtr.toNativeInt addBufPtr)
              let addBufEnd = addBufBeg + MemMapBuf.size loggedMap.AddBuf

              let ri : ReadInfo = {
                  remIdx    = 0
                  pos       = addBufBeg
                  stop      = addBufEnd - 16L // Allow reading an extra hash
                  addIdx    = 0
                  bobOffset = 0L
                  keyDigest = Unchecked.defaultof<_>
                  bobDigest = Unchecked.defaultof<_>
                  bobSize   = 0
                  numDeps   = 0
                  depDigest = Unchecked.defaultof<_>
                }

              let inline getRem () =
                let i = ri.remIdx
                if i < remBufCnt then
                  NativePtr.get remBufPtr i
                else
                  Int32.MaxValue

              let inline nextRem () =
                ri.remIdx <- ri.remIdx + 1

              while ri.pos < ri.stop do
                let addOffset = ri.pos - addBufBeg

                let pos' =
                  ri.pos
                  |> readDigest &ri.keyDigest

                if 0UL = (ri.keyDigest.Lo ||| ri.keyDigest.Hi) then
                  // Apparently log was not closed properly, so cut here
                  do ri.stop <- ri.pos
                else
                  do ri.pos <- pos'
                               |> readDigest &ri.bobDigest
                               |> readTo &ri.bobSize
                               |> readTo &ri.numDeps

                  let depKeyDigests = Array.zeroCreate ri.numDeps
                  do if 0 <> ri.numDeps then
                       for i=0 to ri.numDeps-1 do
                         ri.pos <- ri.pos |> readDigest (&depKeyDigests.[i])
                       ri.pos <- ri.pos |> readDigest (&ri.depDigest)
                     else
                       ri.depDigest <- Unchecked.defaultof<_>

                  let addIdx = ri.addIdx + 1
                  do ri.addIdx <- addIdx

                  do while getRem () < addIdx do
                       nextRem ()

                  if getRem () <> addIdx then
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

              // Truncate buffers in case they were not properly closed last time.
              do MemMapBuf.Unsafe.truncate loggedMap.AddBuf (ri.pos - addBufBeg)
              do MemMapBuf.Unsafe.truncate loggedMap.BobBuf ri.bobOffset
            }
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

      // ...until can declare read phase ready.
      do! loggedMap.Ready <-= None
      do Monitor.Exit loggedMap.Dict

      // Answer the unsatisfied finds.
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
