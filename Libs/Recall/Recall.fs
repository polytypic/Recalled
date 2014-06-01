namespace Recall

open System.Collections.Concurrent
open Hopac
open Hopac.Alt.Infixes
open Hopac.Job.Infixes
open Hopac.Extensions

exception CanceledOnFailure

type Digest = struct
    val lo: uint64
    val hi: uint64
    new (lo, hi) = {lo = lo; hi = hi}
  end

type Logged =
  abstract Id: unit -> string
  abstract Key: unit -> Digest
  abstract Digest: unit -> IVar<Digest>

type Log = {
    Failed: IVar<unit>
    Latch: Latch
    Failures: ResizeArray<exn>
    //Old: 
    New: ConcurrentDictionary<Digest, Logged>
  }

type [<Sealed>] Logged<'x> =
  interface Logged with
    override this.Id () = this.id
    override this.Key () = this.key
    override this.Digest () = this.digest
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
  | Logged of (Log -> Job<Update<'x>>)

type WithLog<'x> = Log -> Job<'x>

type UpdateBuilder () =
  member this.Delay (u2xU: unit -> Update<'x>) : Update<'x> =
    Job (Job.thunk u2xU)
  member this.Return (x: 'x) : Update<'x> =
    Value x
  member this.ReturnFrom (xU: Update<'x>) : Update<'x> =
    xU
  member this.ReturnFrom (xJ: Job<'x>) : Update<'x> =
    Job (xJ |>> Value)
  member this.Bind (xU: Update<'x>, x2yU: 'x -> Update<'y>) : Update<'y> =
    let inline cont (xUJ: Job<Update<'x>>) = xUJ |>> fun xU -> this.Bind (xU, x2yU)
    match xU with
     | Value x -> x2yU x
     | Job xUJ -> Job (cont xUJ)
     | Required (l, xUJ) -> Required (l, cont xUJ)
     | Logged l2xUJ -> Logged (fun l -> l2xUJ l |> cont)
  member this.Bind (xL: Logged<'x>, x2yU: 'x -> Update<'y>) : Update<'y> =
    Required (xL, xL.digest |>> fun _ -> x2yU xL.value)
  member this.Bind (xLW: WithLog<Logged<'x>>, x2yU: 'x -> Update<'y>) : Update<'y> =
    Logged (fun log -> xLW log |>> fun xL -> this.Bind (xL, x2yU))
  member this.Bind (xJ: Job<'x>, x2yU:'x -> Update<'y>) : Update<'y> =
    Job (xJ |>> x2yU)

type LoggedBuilder (id) =
  inherit UpdateBuilder ()
  member this.Run (xU: Update<'x>) : WithLog<Logged<'x>> =
    fun log -> failwith "XXX"

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

type RunWithLogBuilder (log: Log) =
  inherit WithLogBuilder ()
  member this.Run (xW: WithLog<'x>) : Job<'x> =
    xW log

[<AutoOpen>]
module Recall =
  let recall (logDir: string) : RunWithLogBuilder =
    failwith "XXX"

  let logged = WithLogBuilder ()

  let log (id: string) : LoggedBuilder = LoggedBuilder (id)

  let read (xL: Logged<'x>) : Alt<'x> =
    xL.digest |>>? fun _ -> xL.value

  let getCancelAlt: Update<Alt<unit>> =
    Logged (fun log -> Job.result (Value (log.Failed :> Alt<_>)))
