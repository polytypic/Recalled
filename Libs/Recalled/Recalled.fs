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

module DigestSet =
  type [<Class; AllowNullLiteral>] t =
    val lo: uint64
    val hi: uint64
    new (lo, hi) = {lo = lo; hi = hi}
  type Branch =
    inherit t
    val lr: t
    val hr: t
    new (lo, hi, lr, hr) = {inherit t (lo, hi); lr = lr; hr = hr}
  let empty : t = null
  let rec contains' lo' hi' (t: t) =
    match t with
     | null -> false
     | :? Branch as t ->
       if   hi' < t.hi then contains' lo' hi' t.lr
       elif t.hi < hi' then contains' lo' hi' t.hr
       elif lo' < t.lo then contains' lo' hi' t.lr
       else t.lo = lo' || contains' lo' hi' t.hr
     | t ->
       0uL = (lo' ^^^ t.lo ||| hi' ^^^ t.hi)
  let inline contains (d: byref<Digest>) t =
    contains' d.Lo d.Hi t
  type Add = struct
      val id: string
      val is: string
      val lo: uint64
      val hi: uint64
      new (id, is, lo, hi) = {id=id;is=is;lo=lo;hi=hi}
    end
  let rec add' (s: byref<Add>) (t: t) =
    match t with
     | null ->
       new t (s.lo, s.hi)
     | :? Branch as t ->
       let mutable lr = t.lr
       let mutable hr = t.hr
       if   s.hi < t.hi then lr <- add' &s lr
       elif t.hi < s.hi then hr <- add' &s hr
       elif s.lo < t.lo then lr <- add' &s lr
       elif t.lo < s.lo then hr <- add' &s hr
       else failwithf "%A is %s" s.id s.is
       Branch (t.lo, t.hi, lr, hr) :> t
     | t ->
       let mutable lr = null
       let mutable hr = null
       if   s.hi < t.hi then hr <- t
       elif t.hi < s.hi then lr <- t
       elif s.lo < t.lo then hr <- t
       elif t.lo < s.lo then lr <- t
       else failwithf "%A is %s" s.id s.is
       Branch (s.lo, s.hi, lr, hr) :> t
  let inline add id is (d: byref<Digest>) t =
    let mutable s = Add (id, is, d.Lo, d.Hi)
    add' &s t

type Result =
  abstract Id: string
  abstract Contains: DigestSet.t -> bool
  abstract ZeroIfEq: byref<Digest> -> uint64
  abstract CopyKey: byref<Digest> -> unit
  abstract Info: IVar<LoggedMap.Info>

type Log = {
    Failed: IVar<unit>
    Latch: Latch
    Failures: ResizeArray<exn>
    Old: LoggedMap.LoggedMap
    mutable New: DigestDict<Result>
  }

type [<Sealed>] Result<'x> =
  interface Result with
    override this.Id = this.id
    override this.Contains (rs) = DigestSet.contains &this.key rs
    override this.ZeroIfEq (other) = Digest.ZeroIfEq (&this.key, &other)
    override this.CopyKey (other) = other <- this.key
    override this.Info = this.info
  val id: string
  val pu: PU<'x>
  [<DefaultValue>] val mutable key: Digest
  val info: IVar<LoggedMap.Info>
  val mutable value: option<'x>
  new (id, pu) = {id = id; pu = pu; info = ivar (); value = None}

type Update<'x> =
  | Value of 'x
  | Job of Job<Update<'x>>
  | Read of Result * Job<Update<'x>>
  | Required of Result * (unit -> Update<'x>)
  | GetLog of (Log -> DigestSet.t -> Job<Update<'x>>)
  | GetDigest of (Digest -> Job<Update<'x>>)
  | GetThis of (Log -> Result -> int -> Job<Update<'x>>)

type WithLog<'x> = Log -> Job<'x>

type Logged<'x> = LogAs of (Log -> DigestSet.t -> Job<Result * 'x>)

type UpdateBuilder () =
  member this.Delay (u2xU: unit -> Update<'x>) : Update<'x> =
    Job (Job.thunk u2xU)

  member this.Return (x: 'x) : Update<'x> =
    Value x

  member this.ReturnFrom (xU: Update<'x>) : Update<'x> =
    xU
  member this.ReturnFrom (LogAs xAs: Logged<'x>) : Update<'x> =
    GetLog <| fun log rs ->
    xAs log rs |>> fun (r, x) -> Required (r, fun () -> Value x)
  member this.ReturnFrom (xJ: Job<'x>) : Update<'x> =
    Job (xJ |>> Value)
    
  member this.Bind (xU: Update<'x>, x2yU: 'x -> Update<'y>) : Update<'y> =
    let inline bindJ (xUJ: Job<Update<_>>) =
      xUJ |>> fun xU -> this.Bind (xU, x2yU)
    match xU with
     | Value x -> x2yU x
     | Job xUJ -> Job (bindJ xUJ)
     | Read (r, xUJ) -> Read (r, bindJ xUJ)
     | Required (l, xU) -> Required (l, fun () -> this.Bind (xU (), x2yU))
     | GetLog lrs2xUJ -> GetLog (fun l rs -> lrs2xUJ l rs |> bindJ)
     | GetDigest d2xUJ -> GetDigest (d2xUJ >> bindJ)
     | GetThis lri2xUJ -> GetThis (fun l r i -> lri2xUJ l r i |> bindJ)
  member this.Bind (LogAs xAs: Logged<'x>, x2yU: 'x -> Update<'y>) : Update<'y> =
    GetLog <| fun log rs ->
    xAs log rs |>> fun (d, x) -> Required (d, fun () -> x2yU x)
  member this.Bind (xJ: Job<'x>, x2yU:'x -> Update<'y>) : Update<'y> =
    Job (xJ |>> x2yU)

  member this.Combine (uU: Update<unit>, xU: Update<'x>) : Update<'x> =
    this.Bind (uU, fun () -> xU)

  member this.Zero () : Update<unit> = Value ()

module internal Do =
  type MkResult<'x> () =
    inherit ByRefToValue<Struct<string, Digest, PU<'x>>, Result> ()
    override this.Invoke (p) =
      let res = Result<'x> (p.T1, p.T3)
      res.key <- p.T2
      res :> Result

  let asLogged (log: Log)
               (rs: DigestSet.t)
               (id: string)
               (pu: PU<'x>)
               (xU: Update<'x>) : Job<Result * Result<'x>> =
    let mutable key = Unchecked.defaultof<_>
    Digest.String (id, &key)
    let mutable p = Struct<string, Digest, PU<'x>>(id, key, pu)

    let mutable added = false

    let res =
      DigestDict.getOrAdd &log.New &key &p
       (Default<MkResult<_>>.Get () :> ByRefToValue<_, _>) &added

    if not added then
      match res with
       | :? Result<'x> as res when res.id = id ->
         if DigestSet.contains &res.key rs then
           failwithf "%A is cyclic" id

         // Someone got here first.
         Job.result (res :> Result, res)
       | _ ->
         failwithf "Id digest collision: '%s' '%s'" res.Id id
    else
      let res = res :?> Result<'x>

      Latch.Now.increment log.Latch

      let newDeps = ResizeArray<Result> ()

      let cancel () =
        IVar.fillFailure res.info CanceledOnFailure >>.
        Latch.decrement log.Latch

      let failure (e: exn) =
        match e with
         | CanceledOnFailure ->
           cancel ()
         | e ->
           let e' = Exception (sprintf "Failure building %s" id, e)
           lock log.Failures <| fun () -> log.Failures.Add e'
           IVar.tryFill log.Failed () >>.
           IVar.fillFailure res.info e' >>.
           Latch.decrement log.Latch

      let finish info =
        res.info <-= info >>= fun () ->
        Latch.decrement log.Latch

      let rec depDigest sum i =
        if newDeps.Count <= i then
          Job.result sum
        else
          newDeps.[i].Info >>= fun info ->
          Digest.Combine (sum, &info.BobDigest)
          depDigest sum (i+1)

      let complete x =
        res.value <- Some x
        depDigest (ref res.key) 0 >>= fun depDigest ->
        let depKeys = Array.zeroCreate newDeps.Count
        for i=0 to depKeys.Length-1 do
          newDeps.[i].CopyKey (&depKeys.[i])
        LoggedMap.add log.Old res.key depKeys (!depDigest)
         (res.pu.Size x)
         (fun ptr -> res.pu.Dopickle (x, ptr) |> ignore) >>=
        finish

      let inline reuse (oldInfo: LoggedMap.Info) =
        finish oldInfo

      Job.queue
       (Job.tryWith
         (LoggedMap.tryFind log.Old res.key >>= fun s ->
          let rs = DigestSet.add id "cyclic" &res.key rs

          let rec build xU =
            if IVar.Now.isFull log.Failed then
              cancel ()
            else
              match xU with
               | Value x ->
                 complete x
               | Job xUJ ->
                 xUJ >>= build
               | Read (r, xUJ) ->
                 if not (newDeps.Contains r) then
                   failwithf "%A read but not dependend upon" r.Id
                 xUJ >>= build
               | Required (newDep, xU) ->
                 if newDep.Contains rs then
                   failwithf "%A is cyclic" newDep.Id
                 newDeps.Add newDep
                 build (xU ())
               | GetLog lrs2xUJ ->
                 lrs2xUJ log rs >>= build
               | GetDigest d2xUJ ->
                 depDigest (ref res.key) 0 >>= fun digest ->
                 d2xUJ (!digest) >>= build
               | GetThis lri2xUJ ->
                 lri2xUJ log res newDeps.Count >>= build

          match s with 
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
                  | Read (r, xUJ) ->
                    if not (newDeps.Contains r) then
                      failwithf "%A read but not dependend upon" r.Id
                    xUJ >>= checkDeps
                  | Required (newDep, xU) ->
                    if newDep.Contains rs then
                      failwithf "%A is cyclic" newDep.Id
                    newDeps.Add newDep
                    if 0UL <> newDep.ZeroIfEq (&oldInfo.DepKeyDigests.[newDeps.Count-1]) then
                      build (xU ())
                    else
                      if oldInfo.DepKeyDigests.Length <= newDeps.Count then
                        depDigest (ref res.key) 0 >>= fun newDepDigest ->
                        if 0UL = Digest.ZeroIfEq (newDepDigest, &oldInfo.DepDigest) then
                          reuse oldInfo
                        else
                          build (xU ())
                      else
                        checkDeps (xU ())
                  | GetLog lrs2xUJ ->
                    lrs2xUJ log rs >>= checkDeps
                  | GetDigest d2xUJ ->
                    depDigest (ref res.key) 0 >>= fun digest ->
                    d2xUJ (!digest) >>= checkDeps
                  | GetThis lri2xUJ ->
                    lri2xUJ log res newDeps.Count >>= checkDeps
             checkDeps xU)
         failure) >>%
      (res :> Result, res)

type LoggedBuilder (id) =
  inherit UpdateBuilder ()
  member this.Run (xU: Update<'x>) : Logged<Result<'x>> =
    LogAs <| fun log rs -> Do.asLogged log rs id (PU.Get ()) xU

type WithLogBuilder () =
  member inline this.Delay (u2xW: unit -> WithLog<'x>) : WithLog<'x> =
    fun log -> u2xW () log

  member inline this.Return (x: 'x) : WithLog<'x> =
    fun log -> Job.result x

  member inline this.ReturnFrom (xW: WithLog<'x>) : WithLog<'x> =
    xW
  member this.ReturnFrom (LogAs xAs: Logged<'x>) : WithLog<'x> =
    fun log -> xAs log DigestSet.empty |>> fun (_, x) -> x
  member inline this.ReturnFrom (xJ: Job<'x>) : WithLog<'x> =
    fun _ -> xJ

  member inline this.Bind (xW: WithLog<'x>, x2yW: 'x -> WithLog<'y>) : WithLog<'y> =
    fun log -> xW log >>= fun x -> x2yW x log
  member this.Bind (LogAs xAs: Logged<'x>, x2yW: 'x -> WithLog<'y>) : WithLog<'y> =
    fun log -> xAs log DigestSet.empty >>= fun (_, x) -> x2yW x log
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
       New = DigestDict.empty}
    let! result = xW log
    do! Latch.decrement latch
    do! Latch.await latch
    let! closedAlt = LoggedMap.close log.Old
    do! closedAlt
    do lock log.Failures <| fun () ->
       if log.Failures.Count > 0 then
         raise (AggregateException log.Failures)
    return result
  }

[<AutoOpen>]
module Recalled =
  let recall (logDir: string) : RunWithLogBuilder =
    RunWithLogBuilder (logDir)

  let withLog = WithLogBuilder ()

  let update = UpdateBuilder ()

  let logPU id xPU xU =
    LogAs <| fun log rs ->
    Do.asLogged log rs id xPU xU

  let inline log (id: string) = LoggedBuilder (id)

  let watchPU (xPU: PU<'x>) (x: 'x) : Update<unit> =
    GetThis <| fun log res i ->
    Do.asLogged log DigestSet.empty (sprintf "%d: %s" i res.Id) xPU (Value x) |>> fun (d, x) ->
    Required (d, Value)

  let watch (x: 'x) : Update<unit> = watchPU (PU.Get ()) x

  let digest: Update<Digest> =
    GetDigest (Job.result << Value)

  let idOf (xR: Result<_>) = xR.id

  let req (xR: Result<_>) =
    Required (xR :> Result, Value)

  let read (xR: Result<'x>) : Update<'x> =
    Read (xR,
          xR.info >>= fun info ->
          match xR.value with
           | Some value ->
             xR.value <- None
             Job.result (Value value)
           | None ->
             Job.result
              (GetLog <| fun log _ ->
               LoggedMap.readFun log.Old
                (fun ptr ->
                  let ptr =
                    NativePtr.toNativeInt ptr + nativeint info.BobOffset
                    |> NativePtr.ofNativeInt
                  xR.pu.Unpickle ptr) |>> fun value ->
               Value value))

  let dep (LogAs xAs: Logged<_>) : Logged<unit> =
    LogAs <| fun log rs ->
    xAs log rs |>> fun (d, _) -> (d, ())

  let wait (LogAs xLW: Logged<Result<'x>>) : Logged<'x> =
    LogAs <| fun log rs ->
    xLW log rs >>= fun (d, xR: Result<_>) ->
    xR.info >>= fun info ->
    match xR.value with
     | Some value ->
       xR.value <- None
       Job.result (d, value)
     | None ->
       LoggedMap.readFun log.Old
        (fun ptr ->
          let ptr =
            NativePtr.toNativeInt ptr + nativeint info.BobOffset
            |> NativePtr.ofNativeInt
          xR.pu.Unpickle ptr) |>> fun value ->
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
  let mapLogged (x2yAs: _ -> Logged<_>) xs =
    mapUpdate (x2yAs >> update.ReturnFrom) xs
