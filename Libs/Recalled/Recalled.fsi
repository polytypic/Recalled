/// Recalled is a library for defining persistent, incremental, parallel
/// computations such as build systems.  The main goals for Recalled are to make
/// it straightforward to define such computations and to scale such
/// computations.
///
/// The central concept of Recalled is that of a logged computation, represented
/// by the `Logged<'x>` type constructor.  A logged computation is defined and
/// executed in such a manner that after it has been run to completion, the
/// computation can be recreated, possibly in a separate run of the program, and
/// its result can be recovered without actually running the recreated
/// computation to completion, assuming Recalled finds nothing that has changed
/// that could change the result of the computation.
///
/// A correctly defined logged computation is recomputed by Recalled if it needs
/// to be recomputed, because its result may have changed.  Recalled makes it
/// easy for the programmer to write correct logged computations, but does not
/// strictly enforce correctness.  Strictly enforcing correctness is essentially
/// impossible to do in a impure language in a convenient manner, because it
/// precludes the use of built-in lambda expressions.  Therefore Recalled
/// chooses convenience over cumbersomeness.  To define a correct logged
/// computation, the programmer simply needs to make sure that any input that
/// may change the output of a computation is essentially seen by Recalled.
///
/// Logged computations are defined and executed using lightweight threads,
/// called jobs, provided by the Hopac library.  This directly allows logged
/// computations to scale to multiple cores to exploit parallelism.  The use of
/// lightweight threads also allows a logged computation to perform long latency
/// operations, e.g. to perform distributed operations over a network of
/// machines or to perform other forms of asynchronous IO, while allowing full
/// use of the cores of the local machine to process other computations.
namespace Recalled

open System
open Hopac

/// Represents a single primitive step or a sequence of steps of a possibly
/// logged computation.
type Update<'x>

/// Represents the result of a logged computation.
type Result<'x>

/// Represents a computation whose result is logged.
type Logged<'x>

/// Represents a persistent storage.
type Log

/// Represents a parallel computation with a log.
type WithLog<'x> = Log -> Job<'x>

/// Builder for updates or steppable computations.
#if DOC
///
/// During a steppable computation, binding the value of a logged computation
/// makes the current computation dependent on the result of the bound logged
/// computation.  This way Recalled learns about the input that may change the
/// output of a computation.
///
/// Note that steppable computations allow only a limited set of computational
/// patterns, because a steppable computation is not guaranteed to be run to
/// completion.  Specifically, while a single step of a computation is
/// guaranteed to be run to completion after it has been started, a sequence of
/// steps is not guaranteed to be run to completion.  Because of this,
/// constructs such as `use` cannot be given simple intuitive semantics and are
/// therefore not supported.  However, within a single step, even one defined as
/// a job, such constructs can be used.
///
/// The following computational patterns are supported:
///
///> ... ; ...
///> do ...
///> do! ...
///> if ... then ...
///> if ... then ... else ...
///> let! ... = ... | job | log
///> return ...
///> return! ... | job | log
///
/// In the above, an ellipsis denotes either an update or an ordinary
/// expression.  An update can directly bind jobs as well as logged
/// computations.
#endif
type UpdateBuilder =
  new: unit -> UpdateBuilder

  member Delay: (unit -> Update<'x>) -> Update<'x>

  member Return: 'x -> Update<'x>

  member ReturnFrom: Update<'x> -> Update<'x>
  member ReturnFrom: Logged<'x> -> Update<'x>
  member ReturnFrom:    Job<'x> -> Update<'x>

  member Bind: Update<'x> * ('x -> Update<'y>) -> Update<'y>
  member Bind: Logged<'x> * ('x -> Update<'y>) -> Update<'y>
  member Bind:    Job<'x> * ('x -> Update<'y>) -> Update<'y>

  member Combine: Update<unit> * Update<'x> -> Update<'x>

  member Zero: unit -> Update<unit>

/// Builder for a logged computation.
type [<Class>] LoggedBuilder =
  inherit UpdateBuilder
  new: string -> LoggedBuilder
  member Run: Update<'x> -> Logged<Result<'x>>

/// Builder for parallel computations with a log.  A computation with a log is
/// executed in a context with a log for logging individual logged computations.
///
/// The following computational patterns are supported:
///
///> ... ; ...
///> do ...
///> do! ...
///> for ... = ... to ... do ...
///> for ... in ... do ...
///> if ... then ...
///> if ... then ... else ...
///> let ... = ... in ...
///> let! ... = ... | job | log
///> match ... with ...
///> return ...
///> return! ... | job | log
///> try ... finally ...
///> try ... with ...
///> use ... in ...
///> use! ... in ...
///> while ... do ...
///
/// In the above, an ellipsis denotes a computation with a log or an ordinary
/// expression.  A computation with a log can directly bind jobs as well as
/// logged computations.  However, updates cannot be bound directly.
type WithLogBuilder =
  new: unit -> WithLogBuilder

  member inline Delay: (unit -> WithLog<'x>) -> WithLog<'x>

  member inline Return: 'x -> WithLog<'x>

  member inline ReturnFrom: WithLog<'x> -> WithLog<'x>
  member ReturnFrom: Logged<'x> -> WithLog<'x>
  member inline ReturnFrom: Job<'x> -> WithLog<'x>

  member inline Bind: WithLog<'x> * ('x -> WithLog<'y>) -> WithLog<'y>
  member Bind: Logged<'x> * ('x -> WithLog<'y>) -> WithLog<'y>
  member inline Bind: Job<'x> * ('x -> WithLog<'y>) -> WithLog<'y>

  member inline Combine: WithLog<unit> * WithLog<'x> -> WithLog<'x>

  member inline TryFinally: WithLog<'x> * (unit -> unit) -> WithLog<'x>
  member inline TryWith: WithLog<'x> * (exn -> WithLog<'x>) -> WithLog<'x>

  member inline Using: 'x * ('x -> WithLog<'y>) -> WithLog<'y> when 'x :> IDisposable

  member For: seq<'x> * ('x -> WithLog<unit>) -> WithLog<unit>

  member While: (unit -> bool) * WithLog<unit> -> WithLog<unit>

  member inline Zero: unit -> WithLog<unit>

/// Builder for running a parallel computation with a log.
type [<Class>] RunWithLogBuilder =
  inherit WithLogBuilder
  member Run: WithLog<'x> -> Job<'x>

/// Additional operations for sequences.
module Seq =
  /// Creates an update that maps a given update operation over a sequence and
  /// returns a new sequence with the results.
  val mapUpdate: ('x -> Update<'y>) -> seq<'x> -> Update<ResizeArray<'y>>

  /// Creates an update that maps a given logged operation over a sequence and
  /// returns a new sequence with the results.
  val mapLogged: ('x -> Logged<'y>) -> seq<'x> -> Update<ResizeArray<'y>>

/// Operations for defining computations that can be Recalled.
[<AutoOpen>]
module Recalled =
  /// Returns a builder that creates a job that creates a new or reads an
  /// existing computation log stored in the specified directory, creates and
  /// runs the computation with the log and then waits until all the logged
  /// computations have either finished successfully or some computations have
  /// failed and the rest have been canceled.  In case all computations finished
  /// successfully, the result value is produced.  Otherwise an exception is
  /// raised with details on the failed computations.
  val recall: logDir: string -> RunWithLogBuilder

  /// A builder for parallel computations with a log.
  val withLog: WithLogBuilder

  /// A builder for defining updates or partial logged computations whose
  /// results are not logged.
  val update: UpdateBuilder

  /// Returns a computation that logs the dependencies and the result of the
  /// given update with the specified identity.  Computations with the same
  /// identity are assumed to be the same computations.
#if DOC
  ///
  /// The first time a logged computations is created, it is always run to
  /// completion.  When a logged computation with the same identity is recreated
  /// it may or may not be run to completion.
  ///
  /// In case the log indicates that the recreated computation has no
  /// dependencies to other logged computations then the computation is run to
  /// completion every time it is recreated.  This makes it convenient to
  /// essentially create new primitive operations and is also quite logical as a
  /// computation that has no inputs must either be a constant or it must use
  /// some encapsulated effects to compute its output.
  ///
  /// If, however, the log has a non empty sequence of dependencies to other
  /// logged computations, then the recreated computation is run to completion
  /// only if the sequence of dependencies changes or the result of any one of
  /// those logged computations has changed.  This works correctly as long as
  /// any input that may change the result of the computation are bound as other
  /// logged computations within the defined logged computation.
#endif
  val logPU: id: string -> PU<'x> -> Update<'x> -> Logged<Result<'x>>

  /// `log id { ... }` is equivalent to `logPU id (PU.Get ()) (update { ... })`.
  val inline log: id: string -> LoggedBuilder

  /// Returns a computation that logs the given value as a dependency.
  val watchPU: PU<'x> -> 'x -> Update<unit>

  /// `watch x` is equivalent to `watch (PU.Get ()) x`.
  val watch: 'x -> Update<unit>

  /// Provides an intermediate digest of the current point in a logged
  /// computation.
#if DOC
  ///
  /// The intermediate digest includes the identity of the current computation
  /// as well as digests of the results of all the dependencies of the current
  /// computation.  If you ask the intermediate digest after logging all
  /// dependencies and just before computing the result of the current logged
  /// computation, then the intermediate digest effectively identifies the
  /// result.  You could, for example, then use the digest to request a
  /// previously computed result from a server.
#endif
  val digest: Update<Internal.Digest>

  /// Returns a computation that adds the logged result as a dependency.
  val req: Result<_> -> Update<unit>

  /// Returns an update that waits for the result of the logged operation, but
  /// does not add the computation as a dependency.  It is an error to `read` a
  /// result without depending upon the computation that produced the result.
  val read: Result<'x> -> Update<'x>

  /// Returns the identity with which the computation has been logged.
  val idOf: Result<_> -> string

  /// Returns a computation that adds the given computation as a dependency,
  /// but ignores the result of the computation.
#if DOC
  /// In other words,
  ///
  ///> do! dep computation
  ///
  /// is equivalent to
  ///
  ///> let! _ = computation
#endif
  val dep: Logged<_> -> Logged<unit>

  /// Returns a computation that adds the given computation as a dependency,
  /// waits for and returns the result of the computation.
#if DOC
  ///
  /// In other words,
  ///
  ///> let! x = wait computation
  ///
  /// is roughly equivalent to
  ///
  ///> let! xL = computation
  ///> let! x = read xL
  ///
  /// Note when using `wait` rather than `read` the current computation cannot
  /// proceed in parallel with the waited computation.  Of course, if the result
  /// is needed immediately, it makes no difference.
#endif
  val wait: Logged<Result<'x>> -> Logged<'x>

  /// Provides an alternative that becomes enabled if some computation within
  /// the whole computation with a log has failed.  This allows long running
  /// computation steps to cancel themselves cleanly without having to wait
  /// until the completion of the step.
#if DOC
  ///
  /// Note that Recalled automatically cancels computations in case of failure.
  /// Explicit cancellation is unlikely to provide any benefits except in case
  /// of computations that perform long running embedded operations.
#endif
  val getCancelAlt: WithLog<Alt<unit>>
