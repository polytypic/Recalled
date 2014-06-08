namespace Recall

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
  abstract Digest: IVar<Digest>

type Log = {
    Failed: IVar<unit>
    Latch: Latch
    Failures: ResizeArray<exn>
    Old: LoggedMap<Digest, array<byte> * Digest * array<Digest>>
    New: ConcurrentDictionary<Digest, Logged>
  }

type [<Sealed>] Logged<'x> =
  interface Logged with
    override this.Id = this.id
    override this.Key = this.key
    override this.Digest = this.digest
  val id: string
  val key: Digest
  val digest: IVar<Digest>
  [<DefaultValue>]
  val mutable value: 'x
  new (id, key) = {id = id; key = key; digest = ivar ()}

type Update<'x> =
  | Value of 'x
  | Job of Job<Update<'x>>
  | Required of Logged * Job<Update<'x>>
  | GetLog of (Log -> Job<Update<'x>>)
  | GetDigest of (Digest -> Job<Update<'x>>)
  | GetThis of (Log -> Logged -> int -> Job<Update<'x>>)

type WithLog<'x> = Log -> Job<'x>

type UpdateBuilder () =
  member this.Delay (u2xU: unit -> Update<'x>) : Update<'x> =
    Job (Job.thunk u2xU)

  member this.Return (x: 'x) : Update<'x> =
    Value x

  member this.ReturnFrom (xU: Update<'x>) : Update<'x> =
    xU
  member this.ReturnFrom (xW: WithLog<'x>) : Update<'x> =
    GetLog (fun log -> xW log |>> Value)
  member this.ReturnFrom (xJ: Job<'x>) : Update<'x> =
    Job (xJ |>> Value)

  member this.Bind (xU: Update<'x>, x2yU: 'x -> Update<'y>) : Update<'y> =
    let inline cont (xUJ: Job<Update<'x>>) = xUJ |>> fun xU -> this.Bind (xU, x2yU)
    match xU with
     | Value x -> x2yU x
     | Job xUJ -> Job (cont xUJ)
     | Required (l, xUJ) -> Required (l, cont xUJ)
     | GetLog l2xUJ -> GetLog (fun l -> l2xUJ l |> cont)
     | GetDigest d2xUJ -> GetDigest (fun d -> d2xUJ d |> cont)
     | GetThis lliJ -> GetThis (fun log logged i -> lliJ log logged i |> cont)
  member this.Bind (xW: WithLog<'x>, x2yU: 'x -> Update<'y>) : Update<'y> =
    GetLog (fun log -> xW log |>> x2yU)
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
          IVar.fillFailure logged.digest CanceledOnFailure >>.
          Latch.decrement log.Latch

        let failure (e: exn) =
          match e with
           | CanceledOnFailure ->
             cancel ()
           | e ->
             let e' = Exception (sprintf "Failure building %s" id, e)
             lock log.Failures <| fun () -> log.Failures.Add e'
             IVar.tryFill log.Failed () >>.
             IVar.fillFailure logged.digest e' >>.
             Latch.decrement log.Latch

        let finish digest =
          logged.digest <-= digest >>= fun () ->
          Latch.decrement log.Latch

        let rec depDigest sum i =
          if newDeps.Count <= i then
            Job.result sum
          else
            newDeps.[i].Digest >>= fun dig ->
            depDigest (sum ^^^ dig) (i+1)  /// XXX Combine more robustly?

        let complete oldPickle x =
          logged.value <- x
          let pickleStream = new System.IO.MemoryStream ()
          xPU.Dopickle (new System.IO.BinaryWriter (pickleStream), x)
          pickleStream.Position <- 0L
          let digest = Digest.Stream pickleStream
          let pickle = pickleStream.ToArray ()
          match oldPickle with
           | Some oldPickle when oldPickle = pickle ->
             // Nothing changed.
             finish digest
           | _ ->
             // Store barely enough data to be able to check next time.
             depDigest Digest.Zero 0 >>= fun depDigest ->
             let depKeys = Array.init newDeps.Count (fun i -> newDeps.[i].Key)
             LoggedMap.add log.Old key (pickle, depDigest, depKeys) >>= fun () ->
             finish digest

        let rec build oldPickle xU =
          if IVar.Now.isFull log.Failed then
            cancel ()
          else
            match xU with
             | Value x ->
               complete oldPickle x
             | Job xUJ ->
               xUJ >>= build oldPickle
             | Required (newDep, xUJ) ->
               newDeps.Add newDep
               xUJ >>= build oldPickle
             | GetLog log2xUJ ->
               log2xUJ log >>= build oldPickle
             | GetDigest digest2xUJ ->
               failwith "XXX Implement intermediate digest"
             | GetThis lli2xUJ ->
               lli2xUJ log logged newDeps.Count >>= build oldPickle

        let reuse (oldPickle: array<byte>) =
          let mutable p = 0
          xPU.Unpickle (oldPickle, &p, &logged.value)
          finish (Digest.Bytes oldPickle)

        Job.queue
         (Job.tryWith
           (LoggedMap.tryFind log.Old key >>= function
             | None ->
               // Previously unknown.
               build None xU
             | Some (oldPickle, depDigest, [||]) ->
               // Primitive computation.
               build (Some oldPickle) xU
             | Some (oldPickle, oldDepDigest, oldDeps) ->
               // Previously run.
               newDeps.Capacity <- oldDeps.Length

               let rec checkDeps xU =
                 if oldDeps.Length <= newDeps.Count then
                   // Check digest.
                   depDigest Digest.Zero 0 >>= fun newDepDigest ->
                   if newDepDigest = oldDepDigest then
                     reuse oldPickle
                   else
                     build None xU // Something has changed.
                 else
                   if IVar.Now.isFull log.Failed then
                     cancel ()
                   else
                     match xU with
                      | Value x ->
                        complete None x // Dependencies dropped.
                      | Job xUJ ->
                        xUJ >>= checkDeps
                      | Required (newDep, xUJ) ->
                        let oldDepKey = oldDeps.[newDeps.Count]
                        newDeps.Add newDep
                        if newDep.Key <> oldDepKey then
                          build None xU // New dependency.
                        else
                          checkDeps xU
                      | GetLog log2xUJ ->
                        log2xUJ log >>= checkDeps
                      | GetDigest digest2xUJ ->
                        failwith "XXX Implement intermediate digest"
                      | GetThis lli2xUJ ->
                        lli2xUJ log logged newDeps.Count >>= checkDeps
               checkDeps xU)
           failure) >>%
        logged

type LogBuilder () =
  inherit UpdateBuilder ()
  member this.Run (xU: Update<'x>) : Update<Logged<'x>> =
    GetThis <| fun log logged i ->
    Do.asLogged log (sprintf "%d: %s" i logged.Id) xU |>> Value

type LogAsBuilder (id) =
  inherit UpdateBuilder ()
  member this.Run (xU: Update<'x>) : WithLog<Logged<'x>> = fun log ->
    Do.asLogged log id xU

type WithLogBuilder () =
  member inline this.Delay (u2xW: unit -> WithLog<'x>) : WithLog<'x> =
    fun log -> u2xW () log

  member inline this.Return (x: 'x) : WithLog<'x> =
    fun log -> Job.result x

  member inline this.ReturnFrom (xW: WithLog<'x>) : WithLog<'x> =
    xW
  member inline this.ReturnFrom (xJ: Job<'x>) : WithLog<'x> =
    fun _ -> xJ

  member inline this.Bind (xW: WithLog<'x>, x2yW: 'x -> WithLog<'y>) : WithLog<'y> =
    fun log -> xW log >>= fun x -> x2yW x log
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

  member inline this.For (xs: seq<'x>, x2uW: 'x -> WithLog<unit>) : WithLog<unit> =
    fun log -> Seq.iterJob (fun x -> x2uW x log) xs

  member inline this.While (c: unit -> bool, uW: WithLog<unit>) : WithLog<unit> =
    fun log -> Job.whileDo c (Job.delayWith uW log)

  member inline this.Zero () : WithLog<unit> =
    fun log -> Job.unit ()

type RunWithLogBuilder (logDir: string) =
  inherit WithLogBuilder ()
  member this.Run (xW: WithLog<'x>) : Job<'x> = job {
    let domPU = PU.Get ()
    let codPU = PU.Get ()
    let! loggedMap = LoggedMap.create logDir domPU codPU
    let latch = Latch.Now.create 1
    let log =
      {Failed = ivar ()
       Latch = latch
       Failures = ResizeArray<_> ()
       Old = loggedMap
       New = ConcurrentDictionary<_, _> ()}
    let! result = xW log
    let! () = Latch.decrement latch
    let! () = Latch.await latch
    let! () = LoggedMap.close log.Old
    do lock log.Failures <| fun () ->
       if log.Failures.Count > 0 then
         raise (AggregateException log.Failures)
    return result
  }

module Seq =
  let mapWithLog (x2yW: 'x -> WithLog<'y>) (xs: seq<'x>) : WithLog<ResizeArray<'y>> =
    fun log -> xs |> Seq.mapJob (fun x -> x2yW x log)

[<AutoOpen>]
module Recall =
  let recall (logDir: string) : RunWithLogBuilder =
    RunWithLogBuilder (logDir)

  let logged = WithLogBuilder ()

  let update = UpdateBuilder ()

  let logAs (id: string) : LogAsBuilder = LogAsBuilder (id)

  let log : LogBuilder = LogBuilder ()

  let watch (x: 'x) : Update<unit> = update { // XXX Optimize
    let! _ = log { return x }
    return ()
  }

  let digest: Update<Digest> =
    GetDigest (fun digest -> Job.result (Value digest))

  let readAsAlt (xL: Logged<'x>) : Alt<'x> =
    xL.digest |>>? fun _ -> xL.value

  let readAsJob (xL: Logged<'x>) : Job<'x> =
    xL.digest |>> fun _ -> xL.value

  let wait (xLW: WithLog<Logged<'x>>) : WithLog<'x> =
    fun log -> xLW log >>= readAsJob

  let getCancelAlt: WithLog<Alt<unit>> =
    fun log -> Job.result (log.Failed :> Alt<_>)
