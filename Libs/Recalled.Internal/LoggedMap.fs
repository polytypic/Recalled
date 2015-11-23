namespace Recalled.Internal

open Microsoft.FSharp.NativeInterop
open Hopac
open Hopac.Infixes
open Hopac.Extensions
open System
open System.Diagnostics
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open System.Threading

module LoggedMap =
  type Info = {
      mutable DepKeyDigests: array<Digest>
      mutable DepDigest: Digest
      mutable BobDigest: Digest
      mutable BobOffset: PtrInt
      mutable BobSize: int
      mutable AddOffset: PtrInt
    }

  type Entry = {
      mutable Idx: int
      mutable Live: bool
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
    Promise.start
     (MemMapBuf.close loggedMap.AddBuf >>= fun addClosed ->
      MemMapBuf.close loggedMap.RemBuf >>= fun remClosed ->
      MemMapBuf.close loggedMap.BobBuf >>= fun bobClosed ->
      addClosed >>=. remClosed >>=. bobClosed)

  let tryFind (loggedMap: LoggedMap)
              (keyDigest: Digest) = Alt.prepareFun <| fun () ->
    Monitor.Enter loggedMap.Dict
    match loggedMap.Dict.TryGetValue keyDigest with
     | Nothing ->
       if IVar.Now.isFull loggedMap.Ready then
         Monitor.Exit loggedMap.Dict
         loggedMap.Ready
       else
         let info = IVar ()
         loggedMap.Dict.Add (keyDigest, {Idx = -1; Live = true; Info = info})
         Monitor.Exit loggedMap.Dict
         info
     | Just entry ->
       Monitor.Exit loggedMap.Dict
       entry.Live <- true
       entry.Info

  let readFun (loggedMap: LoggedMap)
              (readFun: nativeptr<byte> -> 'x) : Job<'x> =
    MemMapBuf.accessFun loggedMap.BobBuf readFun

  let remDead (loggedMap: LoggedMap) : Job<Alt<unit>> =
    failwith "XXX"

  let justWrite (loggedMap: LoggedMap)
                (entry: Entry)
                (keyDigest: Digest)
                (depKeyDigests: array<Digest>)
                (depDigest: Digest)
                (bobSize: int)
                (bobWrite: nativeptr<byte> -> unit)
                (addOffs: PtrInt)
                (bobOffs: PtrInt) =
    let info = {
        DepKeyDigests = depKeyDigests
        DepDigest = depDigest
        BobDigest = Unchecked.defaultof<_>
        BobOffset = bobOffs
        BobSize = bobSize
        AddOffset = addOffs
      }

    MemMapBuf.accessFun loggedMap.BobBuf (fun ptr ->
      let ptr = NativePtr.ofNativeInt (NativePtr.toNativeInt ptr + nativeint bobOffs)
      bobWrite ptr
      Digest.Bytes (ptr, bobSize, &info.BobDigest)) >>= fun () ->

    MemMapBuf.accessFun loggedMap.AddBuf (fun ptr ->
      let p = int64 (NativePtr.toNativeInt ptr) + addOffs

      let inline writeDigest (dig: Digest) p =
        p
        |> writeIfNot dig.Lo
        |> writeIfNot dig.Hi

      let mutable p =
        p
        |> writeDigest keyDigest
        |> writeDigest info.BobDigest
        |> writeIfNot bobSize
        |> writeIfNot depKeyDigests.Length

      if 0 <> depKeyDigests.Length then
        for i=0 to depKeyDigests.Length-1 do
          p <- writeDigest depKeyDigests.[i] p
        p <- writeDigest info.DepDigest p) >>= fun () ->
    entry.Info *<= Some info >>-.
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
      |> skipBy sizeof<Digest>
      // bobDigest
      |> skipBy sizeof<Digest>
      // bobSize
      |> skipBy sizeof<int32>
      // depKeyDigests
      |> skipBy sizeof<int32>
    let addEntrySize =
      if 0 <> depKeyDigests.Length then
        addEntrySize
        |> skipBy (depKeyDigests.Length * sizeof<Digest>)
        // depDigest
        |> skipBy sizeof<Digest>
      else
        addEntrySize
    let addEntrySize = int addEntrySize

    let bobSize' = (bobSize + 7) &&& ~~~ 7 // Ensure BobBuf always aligned size

    loggedMap.AddIdx >>= fun idx ->
    let idx = idx + 1
    MemMapBuf.append loggedMap.BobBuf sizeof<int64> bobSize' >>= fun bobOffs ->
    MemMapBuf.append loggedMap.AddBuf sizeof<int64> addEntrySize >>= fun addOffs ->
    loggedMap.AddIdx *<<= idx >>= fun () ->

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
    let entry = {Idx = 0; Live = true; Info = IVar ()}
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
                |> writeIfNot remIdx
                |> ignore) >>=.
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

  let rec quickSortMed3Fun (ptr: nativeptr<int32>) start stop =
    if 2 <= stop - start then
      let middle = partitionMed3 ptr start stop
      if middle - start < stop - middle then
        quickSortMed3Fun ptr start middle
        quickSortMed3Fun ptr middle stop
      else
        quickSortMed3Fun ptr middle stop
        quickSortMed3Fun ptr start middle

  let rec quickSortMed3Job (ptr: nativeptr<int32>) start stop = job {
    if stop - start < 30 then
      return quickSortMed3Fun ptr start stop
    else
      let middle = partitionMed3 ptr start stop
      let! wait = Promise.queue (quickSortMed3Job ptr start middle)
      do! quickSortMed3Job ptr middle stop
      return! wait
  }

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

          do! quickSortMed3Job remBufPtr 0 remBufCnt

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

                    do ri.bobOffset <-
                         skipTo sizeof<int64> (skipBy ri.bobSize ri.bobOffset)

                    let entry = {
                        Idx = ri.addIdx
                        Live = false
                        Info = IVar.Now.createFull info
                      }

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
                       return! request.Info *<= info

              do! loggedMap.AddIdx *<<= ri.addIdx

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
      do! loggedMap.Ready *<= None
      do Monitor.Exit loggedMap.Dict

      // Answer the unsatisfied finds.
      do! unsatisfied
          |> Seq.iterJob (fun kvI ->
             kvI.Value.Info *<= None)

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
     {AddIdx = MVar ()
      Ready = IVar ()
      Dict = Dictionary<_, _> (DigestEqualityComparer ())
      AddBuf = addBuf
      RemBuf = remBuf
      BobBuf = bobBuf}
    do! Job.queue (server loggedMap)
    return loggedMap
  }
