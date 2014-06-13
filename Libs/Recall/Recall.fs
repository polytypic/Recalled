namespace Recall

open Microsoft.FSharp.NativeInterop
open System
open System.Collections.Concurrent
open Hopac
open Hopac.Infixes
open Hopac.Alt.Infixes
open Hopac.Job.Infixes
open Hopac.Extensions

exception CanceledOnFailure

type Logged =
  abstract Id: string
  abstract Key: Digest
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
    override this.Key = this.key
    override this.Info = this.info
  val id: string
  val key: Digest
  val info: IVar<LoggedMap.Info>
  val mutable value: option<'x>
  new (id, key) = {id = id; key = key; info = ivar (); value = None}

type Update<'x> =
  | Value of 'x
  | Job of Job<Update<'x>>
  | Required of Logged * Update<'x>
  | GetLog of (Log -> Update<'x>)
  | GetDigest of (Digest -> Update<'x>)
  | GetThis of (Log -> Logged -> int -> Update<'x>)

type WithLog<'x> = Log -> Job<'x>
type LogAs<'x> = LogAs of WithLog<Logged * 'x>

type UpdateBuilder () =
  member this.Delay (u2xU: unit -> Update<'x>) : Update<'x> =
    Job (Job.thunk u2xU)

  member this.Return (x: 'x) : Update<'x> =
    Value x

  member this.ReturnFrom (xU: Update<'x>) : Update<'x> =
    xU
  member this.ReturnFrom (LogAs xAs: LogAs<'x>) : Update<'x> =
    GetLog <| fun log ->
    Job (xAs log |>> fun (d, x) -> Required (d, Value x))
  member this.ReturnFrom (xJ: Job<'x>) : Update<'x> =
    Job (xJ |>> Value)

  member this.Bind (xU: Update<'x>, x2yU: 'x -> Update<'y>) : Update<'y> =
    match xU with
     | Value x -> x2yU x
     | Job xUJ -> Job (xUJ |>> fun xU -> this.Bind (xU, x2yU))
     | Required (l, xU) -> Required (l, this.Bind (xU, x2yU))
     | GetLog l2xU -> GetLog (fun l -> this.Bind (l2xU l, x2yU))
     | GetDigest d2xU -> GetDigest (fun d -> this.Bind (d2xU d, x2yU))
     | GetThis lliJ -> GetThis (fun log logged i -> this.Bind (lliJ log logged i, x2yU))
  member this.Bind (LogAs xAs: LogAs<'x>, x2yU: 'x -> Update<'y>) : Update<'y> =
    GetLog <| fun log ->
    Job (xAs log |>> fun (d, x) -> Required (d, x2yU x))
  member this.Bind (xJ: Job<'x>, x2yU:'x -> Update<'y>) : Update<'y> =
    Job (xJ |>> x2yU)

  member this.Combine (uU: Update<unit>, xU: Update<'x>) : Update<'x> =
    this.Bind (uU, fun () -> xU)

  member this.Zero () : Update<unit> = Value ()

module internal Do =
  let asLogged (log: Log) (id: string) (xU: Update<'x>) : Job<Logged<'x>> =
    Job.delay <| fun () ->
      Latch.Now.increment log.Latch

      let key = Digest.String id

      let logged = Logged<'x> (id, key)
      let was = log.New.GetOrAdd (key, logged :> Logged)

      if not (LanguagePrimitives.PhysicalEquality was (logged :> Logged)) then
        match was with
         | :? Logged<'x> as logged' ->
           // Someone got here first.
           Latch.decrement log.Latch >>% logged'
         | _ ->
           Job.tryFinallyJob
             (Job.thunk (fun () -> failwithf "Id digest collision: %s %s!" was.Id logged.id))
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
            depDigest (sum ^^^ info.BobDigest) (i+1)  /// XXX Combine more robustly?

        let complete x =
          logged.value <- Some x
          depDigest Digest.Zero 0 >>= fun depDigest ->
          let depKeys = Array.init newDeps.Count (fun i -> newDeps.[i].Key)
          LoggedMap.add log.Old key depKeys depDigest
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
               build xU
             | GetLog log2xUJ ->
               build (log2xUJ log)
             | GetDigest digest2xUJ ->
               failwith "XXX Implement intermediate digest"
             | GetThis lli2xUJ ->
               build (lli2xUJ log logged newDeps.Count)

        let inline reuse (oldInfo: LoggedMap.Info) =
          finish oldInfo

        Job.queue
         (Job.tryWith
           (LoggedMap.tryFind log.Old key >>= function
             | None ->
               build xU
             | Some old when old.DepKeyDigests.Length = 0 ->
               build xU
             | Some oldInfo ->
               newDeps.Capacity <- oldInfo.DepKeyDigests.Length

               let rec checkDeps xU =
                 if oldInfo.DepKeyDigests.Length <= newDeps.Count then
                   depDigest Digest.Zero 0 >>= fun newDepDigest ->
                   if newDepDigest = oldInfo.DepDigest then
                     reuse oldInfo
                   else
                     build xU
                 else
                   if IVar.Now.isFull log.Failed then
                     cancel ()
                   else
                     match xU with
                      | Value x ->
                        complete x
                      | Job xUJ ->
                        xUJ >>= checkDeps
                      | Required (newDep, xU) ->
                        let oldDepKey = oldInfo.DepKeyDigests.[newDeps.Count]
                        newDeps.Add newDep
                        if newDep.Key <> oldDepKey then
                          build xU
                        else
                          checkDeps xU
                      | GetLog log2xUJ ->
                        checkDeps (log2xUJ log)
                      | GetDigest digest2xUJ ->
                        failwith "XXX Implement intermediate digest"
                      | GetThis lli2xUJ ->
                        checkDeps (lli2xUJ log logged newDeps.Count)
               checkDeps xU)
           failure) >>%
        logged

//type LogBuilder () =
//  inherit UpdateBuilder ()
//  member this.Run (xU: Update<'x>) : Update<Logged<'x>> =
//    GetThis <| fun log logged i ->
//    Do.asLogged log (sprintf "%d: %s" i logged.Id) xU |>> fun logged ->
//    Required (logged :> Logged, Job.result (Value logged))

type LogAsBuilder (id) =
  inherit UpdateBuilder ()
  member this.Run (xU: Update<'x>) : LogAs<Logged<'x>> =
    LogAs <| fun log -> Do.asLogged log id xU |>> fun logged -> (logged :> Logged, logged)

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

//  member this.For (xs: seq<'x>, x2uW: 'x -> WithLog<unit>) : WithLog<unit> =
//    WithLog <| fun log -> Seq.iterJob (fun x -> x2uW x |> function WithLog f -> f log) xs

//  member this.While (c: unit -> bool, WithLog uW: WithLog<unit>) : WithLog<unit> =
//    WithLog <| fun log -> Job.whileDo c (Job.delayWith uW log)

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

//module Seq =
//  let mapWithLog (x2yW: 'x -> WithLog<'y>) (xs: seq<'x>) : Update<ResizeArray<'y>> =
//    failwith "XXX"

[<AutoOpen>]
module Recall =
  let recall (logDir: string) : RunWithLogBuilder =
    RunWithLogBuilder (logDir)

  let logged = WithLogBuilder ()

  let update = UpdateBuilder ()

  let logAs (id: string) : LogAsBuilder = LogAsBuilder (id)

  //let log : LogBuilder = LogBuilder ()

//  let watch (x: 'x) : Update<unit> = update { // XXX Optimize
//    let! _ = log { return x }
//    return ()
//  }

  let digest: Update<Digest> =
    GetDigest (fun digest -> Value digest)

  let read (xL: Logged<'x>) : Update<'x> =
    Job (xL.info >>= fun info ->
         match xL.value with
          | Some value ->
            Job.result (Value value)
          | None ->
            Job.result
             (GetLog (fun log ->
               let xPU = PU.Get ()
               Job (LoggedMap.readFun log.Old
                     (fun ptr ->
                       let ptr =
                         NativePtr.toNativeInt ptr + nativeint info.BobOffset
                         |> NativePtr.ofNativeInt
                       xPU.Unpickle ptr) |>> fun value ->
                    Value value))))

  let wait (LogAs xLW: LogAs<Logged<'x>>) : LogAs<'x> =
    LogAs <| fun log ->
    xLW log >>= fun (d, xL) ->
    xL.info >>= fun info ->
    match xL.value with
     | Some value ->
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
