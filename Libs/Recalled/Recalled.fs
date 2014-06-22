namespace Recalled

open Recalled.Internal
open Microsoft.FSharp.NativeInterop
open System
open System.Collections.Generic
open System.Collections.Concurrent
open Hopac
open Hopac.Infixes
open Hopac.Alt.Infixes
open Hopac.Job.Infixes
open Hopac.Extensions

exception CanceledOnFailure

type Logged =
  abstract Id: string
  abstract ZeroIfEq: byref<Digest> -> uint64
  abstract CopyKey: byref<Digest> -> unit
  abstract Info: IVar<LoggedMap.Info>

type Log = {
    Failed: IVar<unit>
    Latch: Latch
    Failures: ResizeArray<exn>
    Old: LoggedMap.LoggedMap
    New: ConcurrentDictionary<Digest, Logged>
  }

type [<Sealed>] Logged<'x> =
  interface Logged with
    override this.Id = this.id
    override this.ZeroIfEq (other) = Digest.ZeroIfEq (&this.key, &other)
    override this.CopyKey (other) = other <- this.key
    override this.Info = this.info
  val id: string
  [<DefaultValue>] val mutable key: Digest
  val info: IVar<LoggedMap.Info>
  val mutable value: option<'x>
  new (id) = {id = id; info = ivar (); value = None}

type Update<'x> =
  | Value of 'x
  | Job of Job<Update<'x>>
  | Required of Logged * (unit -> Update<'x>)
  | GetLog of (Log -> Job<Update<'x>>)
  | GetDigest of (Digest -> Update<'x>)
  | GetThis of (Log -> Logged -> int -> Job<Update<'x>>)

type WithLog<'x> = Log -> Job<'x>
type LogAs<'x> = LogAs of (Log -> Job<Logged * 'x>)
type Log<'x> = Log of (Log -> Logged -> int -> Job<Logged * 'x>)

type UpdateBuilder () =
  member this.Delay (u2xU: unit -> Update<'x>) : Update<'x> =
    Job (Job.thunk u2xU)

  member this.Return (x: 'x) : Update<'x> =
    Value x

  member this.ReturnFrom (xU: Update<'x>) : Update<'x> =
    xU
  member this.ReturnFrom (LogAs xAs: LogAs<'x>) : Update<'x> =
    GetLog <| fun log ->
    xAs log |>> fun (r, x) -> Required (r, fun () -> Value x)
  member this.ReturnFrom (Log xL: Log<'x>) : Update<'x> =
    GetThis <| fun log logged i ->
    xL log logged i |>> fun (r, x) -> Required (r, fun () -> Value x)
  member this.ReturnFrom (xJ: Job<'x>) : Update<'x> =
    Job (xJ |>> Value)

  member this.Bind (xU: Update<'x>, x2yU: 'x -> Update<'y>) : Update<'y> =
    match xU with
     | Value x -> x2yU x
     | Job xUJ -> Job (xUJ |>> fun xU -> this.Bind (xU, x2yU))
     | Required (l, xU) -> Required (l, fun () -> this.Bind (xU (), x2yU))
     | GetLog l2xUJ -> GetLog (fun l -> l2xUJ l |>> fun xU -> this.Bind (xU, x2yU))
     | GetDigest d2xU -> GetDigest (fun d -> this.Bind (d2xU d, x2yU))
     | GetThis lliJ -> GetThis (fun log logged i -> lliJ log logged i |>> fun xU -> this.Bind (xU, x2yU))
  member this.Bind (LogAs xAs: LogAs<'x>, x2yU: 'x -> Update<'y>) : Update<'y> =
    GetLog <| fun log ->
    xAs log |>> fun (d, x) -> Required (d, fun () -> x2yU x)
  member this.Bind (Log xL: Log<'x>, x2yU: 'x -> Update<'y>) : Update<'y> =
    GetThis <| fun log logged i ->
    xL log logged i |>> fun (d, x) -> Required (d, fun () -> x2yU x)
  member this.Bind (xJ: Job<'x>, x2yU:'x -> Update<'y>) : Update<'y> =
    Job (xJ |>> x2yU)

  member this.Combine (uU: Update<unit>, xU: Update<'x>) : Update<'x> =
    this.Bind (uU, fun () -> xU)

  member this.Zero () : Update<unit> = Value ()

module internal Do =
  let asLogged (log: Log) (id: string) (xU: Update<'x>) : Job<Logged * Logged<'x>> =
    Job.delay <| fun () ->
      Latch.Now.increment log.Latch

      let logged = Logged<'x> (id)
      Digest.String (id, &logged.key)

      let was = log.New.GetOrAdd (logged.key, logged :> Logged)

      if not (LanguagePrimitives.PhysicalEquality was (logged :> Logged)) then
        match was with
         | :? Logged<'x> as logged' when logged'.id = id ->
           // Someone got here first.
           Latch.decrement log.Latch >>% (logged' :> Logged, logged')
         | _ ->
           Job.tryFinallyJob
             (Job.thunk (fun () -> failwithf "Id digest collision: '%s' '%s'" was.Id logged.id))
             (Latch.decrement log.Latch)
      else
        let xPU = PU.Get ()

        let newDeps = ResizeArray<Logged> ()

        let cancel () =
          IVar.fillFailure logged.info CanceledOnFailure >>.
          Latch.decrement log.Latch

        let failure (e: exn) =
          match e with
           | CanceledOnFailure ->
             cancel ()
           | e ->
             let e' = Exception (sprintf "Failure building %s" id, e)
             lock log.Failures <| fun () -> log.Failures.Add e'
             IVar.tryFill log.Failed () >>.
             IVar.fillFailure logged.info e' >>.
             Latch.decrement log.Latch

        let finish info =
          logged.info <-= info >>= fun () ->
          Latch.decrement log.Latch

        let rec depDigest sum i =
          if newDeps.Count <= i then
            Job.result sum
          else
            newDeps.[i].Info >>= fun info ->
            Digest.Combine (sum, &info.BobDigest)
            depDigest sum (i+1)

        let complete x =
          logged.value <- Some x
          depDigest (ref logged.key) 0 >>= fun depDigest ->
          let depKeys = Array.zeroCreate newDeps.Count
          for i=0 to depKeys.Length-1 do
            newDeps.[i].CopyKey (&depKeys.[i])
          LoggedMap.add log.Old logged.key depKeys (!depDigest)
           (xPU.Size x)
           (fun ptr ->
              xPU.Dopickle (x, ptr) |> ignore) >>=
          finish

        let rec build xU =
          if IVar.Now.isFull log.Failed then
            cancel ()
          else
            match xU with
             | Value x ->
               complete x
             | Job xUJ ->
               xUJ >>= build
             | Required (newDep, xU) ->
               newDeps.Add newDep
               build (xU ())
             | GetLog log2xUJ ->
               log2xUJ log >>= build
             | GetDigest digest2xU ->
               depDigest (ref logged.key) 0 >>= fun digest ->
               build (digest2xU (!digest))
             | GetThis lli2xUJ ->
               lli2xUJ log logged newDeps.Count >>= build

        let inline reuse (oldInfo: LoggedMap.Info) =
          finish oldInfo

        Job.queue
         (Job.tryWith
           (LoggedMap.tryFind log.Old logged.key >>= function
             | None ->
               build xU
             | Some old when old.DepKeyDigests.Length = 0 ->
               build xU
             | Some oldInfo ->
               newDeps.Capacity <- oldInfo.DepKeyDigests.Length

               let rec checkDeps xU =
                 if IVar.Now.isFull log.Failed then
                   cancel ()
                 else
                   match xU with
                    | Value x ->
                      complete x
                    | Job xUJ ->
                      xUJ >>= checkDeps
                    | Required (newDep, xU) ->
                      newDeps.Add newDep
                      if 0UL <> newDep.ZeroIfEq (&oldInfo.DepKeyDigests.[newDeps.Count-1]) then
                        build (xU ())
                      else
                        if oldInfo.DepKeyDigests.Length <= newDeps.Count then
                          depDigest (ref logged.key) 0 >>= fun newDepDigest ->
                          if 0UL = Digest.ZeroIfEq (newDepDigest, &oldInfo.DepDigest) then
                            reuse oldInfo
                          else
                            build (xU ())
                        else
                          checkDeps (xU ())
                    | GetLog log2xUJ ->
                      log2xUJ log >>= checkDeps
                    | GetDigest digest2xU ->
                      depDigest (ref logged.key) 0 >>= fun digest ->
                      build (digest2xU (!digest))
                    | GetThis lli2xUJ ->
                      lli2xUJ log logged newDeps.Count >>= checkDeps
               checkDeps xU)
           failure) >>%
        (logged :> Logged, logged)

type LogAsBuilder (id) =
  inherit UpdateBuilder ()
  member this.Run (xU: Update<'x>) : LogAs<Logged<'x>> =
    LogAs <| fun log -> Do.asLogged log id xU

type LogBuilder () =
  inherit UpdateBuilder ()
  member this.Run (xU: Update<'x>) : Log<Logged<'x>> =
    Log <| fun log logged i ->
    Do.asLogged log (sprintf "%d: %s" i logged.Id) xU

type WithLogBuilder () =
  member inline this.Delay (u2xW: unit -> WithLog<'x>) : WithLog<'x> =
    fun log -> u2xW () log

  member inline this.Return (x: 'x) : WithLog<'x> =
    fun log -> Job.result x

  member inline this.ReturnFrom (xW: WithLog<'x>) : WithLog<'x> =
    xW
  member this.ReturnFrom (LogAs xAs: LogAs<'x>) : WithLog<'x> =
    fun log -> xAs log |>> fun (_, x) -> x
  member inline this.ReturnFrom (xJ: Job<'x>) : WithLog<'x> =
    fun _ -> xJ

  member inline this.Bind (xW: WithLog<'x>, x2yW: 'x -> WithLog<'y>) : WithLog<'y> =
    fun log -> xW log >>= fun x -> x2yW x log
  member this.Bind (LogAs xAs: LogAs<'x>, x2yW: 'x -> WithLog<'y>) : WithLog<'y> =
    fun log -> xAs log >>= fun (_, x) -> x2yW x log
  member inline this.Bind (xJ: Job<'x>, x2yW: 'x -> WithLog<'y>) : WithLog<'y> =
    fun log -> xJ >>= fun x -> x2yW x log

  member inline this.Combine (uW: WithLog<unit>, xW: WithLog<'x>) : WithLog<'x> =
    this.Bind (uW, fun () -> xW)

  member inline this.TryFinally (xW: WithLog<'x>, fin: unit -> unit) : WithLog<'x> =
    fun log -> Job.tryFinallyFun (Job.delayWith xW log) fin
  member inline this.TryWith (xW: WithLog<'x>, e2xW: exn -> WithLog<'x>) : WithLog<'x> =
    fun log -> Job.tryWith (Job.delayWith xW log) (fun e -> e2xW e log)

  member inline this.Using (x: 'x, x2yW: 'x -> WithLog<'y>) : WithLog<'y> =
    fun log -> Job.using x (fun x -> x2yW x log)

  member this.For (xs: seq<'x>, x2uW: 'x -> WithLog<unit>) : WithLog<unit> =
    fun log -> Seq.iterJob (fun x -> x2uW x log) xs

  member this.While (c: unit -> bool, uW: WithLog<unit>) : WithLog<unit> =
    fun log -> Job.whileDo c (Job.delayWith uW log)

  member inline this.Zero () : WithLog<unit> =
    fun log -> Job.unit ()

type RunWithLogBuilder (logDir: string) =
  inherit WithLogBuilder ()
  member this.Run (xW: WithLog<'x>) : Job<'x> = job {
    let! loggedMap = LoggedMap.create logDir
    let latch = Latch.Now.create 1
    let log =
      {Failed = ivar ()
       Latch = latch
       Failures = ResizeArray<_> ()
       Old = loggedMap
       New = ConcurrentDictionary<_, _> (DigestEqualityComparer ())}
    let! result = xW log
    let! () = Latch.decrement latch
    let! () = Latch.await latch
    let! closedAlt = LoggedMap.close log.Old
    let! () = closedAlt
    do lock log.Failures <| fun () ->
       if log.Failures.Count > 0 then
         raise (AggregateException log.Failures)
    return result
  }

[<AutoOpen>]
module Recalled =
  let recall (logDir: string) : RunWithLogBuilder =
    RunWithLogBuilder (logDir)

  let logged = WithLogBuilder ()

  let update = UpdateBuilder ()

  let logAs (id: string) : LogAsBuilder = LogAsBuilder (id)

  let log : LogBuilder = LogBuilder ()

  let watch (x: 'x) : Log<unit> =
    Log <| fun log logged i ->
    Do.asLogged log (sprintf "%d: %s" i logged.Id) (Value x) |>> fun (d, x) -> (d, ())

  let digest: Update<Digest> =
    GetDigest (fun digest -> Value digest)

  let read (xL: Logged<'x>) : Update<'x> =
    Job (xL.info >>= fun info ->
         match xL.value with
          | Some value ->
            xL.value <- None
            Job.result (Value value)
          | None ->
            Job.result
             (GetLog (fun log ->
               let xPU = PU.Get ()
               LoggedMap.readFun log.Old
                (fun ptr ->
                  let ptr =
                    NativePtr.toNativeInt ptr + nativeint info.BobOffset
                    |> NativePtr.ofNativeInt
                  xPU.Unpickle ptr) |>> fun value ->
               Value value)))

  let wait (LogAs xLW: LogAs<Logged<'x>>) : LogAs<'x> =
    LogAs <| fun log ->
    xLW log >>= fun (d, xL) ->
    xL.info >>= fun info ->
    match xL.value with
     | Some value ->
       xL.value <- None
       Job.result (d, value)
     | None ->
       let xPU = PU.Get ()
       LoggedMap.readFun log.Old
        (fun ptr ->
          let ptr =
            NativePtr.toNativeInt ptr + nativeint info.BobOffset
            |> NativePtr.ofNativeInt
          xPU.Unpickle ptr) |>> fun value ->
       (d, value)

  let getCancelAlt: WithLog<Alt<unit>> =
    fun log -> Job.result (log.Failed :> Alt<_>)

module Seq =
  let mapUpdate (x2yU: 'x -> Update<'y>) (xs: seq<'x>) : Update<ResizeArray<'y>> =
    Job << Job.thunk <| fun () ->
    let xs = xs.GetEnumerator ()
    let ys = ResizeArray<_> ()
    let rec loop () =
      if xs.MoveNext () then
        update.Bind (x2yU xs.Current, fun y ->
                     ys.Add y
                     loop ())
      else
        xs.Dispose ()
        Value ys
    loop ()
  let mapLogAs (x2yAs: _ -> LogAs<_>) xs =
    mapUpdate (x2yAs >> update.ReturnFrom) xs
