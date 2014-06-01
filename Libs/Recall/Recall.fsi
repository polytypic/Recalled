/// Recall is a library for defining persistent, incremental, parallel
/// computations such as build systems.  The main goals for Recall are to make
/// it convenient to define such computations and to be able to scale such
/// computations.
///
/// The central concept of Recall is that of a logged computation, represented
/// by the `Logged<'x>` type constructor.  A logged computation is defined and
/// executed in such a manner that after it has been run to completion, the
/// computation can be recreated, possibly in a separate run of the program, and
/// its result can be recovered without actually running the recreated
/// computation to completion, assuming Recall finds nothing that has changed
/// that could change the result of the computation.
///
/// A correctly defined logged computation is recomputed by Recall if it needs
/// to be recomputed, because its result may have changed.  Recall makes it easy
/// for the programmer to write correct logged computations, but does not
/// strictly enforce correctness.  Strictly enforcing correctness is essentially
/// impossible to do in a non-pure language in a convenient manner, because it
/// precludes the use of built-in lambda expressions.  Therefore Recall chooses
/// convenience over cumbersomeness.  To define a correct logged computation,
/// the programmer simply needs to make sure that any input that may change the
/// output of a computation is essentially seen by Recall.
///
/// Logged computations are defined and executed using lightweight threads,
/// called jobs, provided by the Hopac library.  This directly allows logged
/// computations to scale to multiple cores to exploit parallelism.  The use of
/// lightweight threads also allows a logged computation to perform long latency
/// operations, e.g. to perform distributed operations over a network of
/// machines or to perform other forms of asynchronous IO, while allowing full
/// use of the cores of the local machine to process other computations.
namespace Recall

open Hopac

/// Represents a single primitive step or a sequence of steps of a possibly
/// logged computation.
type Update<'x>

/// Represents a logged computation.
#if DOC
///
/// During a logged computation, binding the value of another logged computation
/// makes the current logged computation dependent on the result of the bound
/// logged computation.  This way Recall learns about the input that may change
/// the output of a computation.
#endif
type Logged<'x>

/// Represents a parallel computation with a log.
type WithLog<'x>

/// Builder for steppable computations.
#if DOC
///
/// Note that steppable computations allow only a limited set of computational
/// patterns, because a steppable computation is not guaranteed to be run to
/// completion.  Specifically, while a single step of a computation is
/// guaranteed to be run to completion after it has been started, a sequence of
/// steps is not guaranteed to be run to completion.  Because of this,
/// constructs such as `use` cannot be given meaningful semantics.  However,
/// within a single step, even one defined as a job, such constructs can be
/// used.
#endif
type UpdateBuilder =
  new: unit -> UpdateBuilder

  member Delay: (unit -> Job<Update<'x>>) -> Job<Update<'x>>

  member Return: 'x -> Job<Update<'x>>

  member ReturnFrom:  Job<Update<'x>> -> Job<Update<'x>>
  member ReturnFrom: EmbeddedJob<'x>  -> Job<Update<'x>>

  member Bind:  Job<Update<'x>> * ('x -> Job<Update<'y>>) -> Job<Update<'y>>
  member Bind:  Job<Logged<'x>> * ('x -> Job<Update<'y>>) -> Job<Update<'y>>
  member Bind:      Logged<'x>  * ('x -> Job<Update<'y>>) -> Job<Update<'y>>
  member Bind: EmbeddedJob<'x>  * ('x -> Job<Update<'y>>) -> Job<Update<'y>>

/// Builder for logged computations.  A logged computation is essentially a
/// steppable computation, whose steps are logged, while it is being executed.
type LoggedBuilder =
  inherit UpdateBuilder
  new: unit -> LoggedBuilder
  member Run: Job<Update<'x>> -> Job<Logged<'x>>

/// Builder for parallel computations with a log.  A computation with a log is
/// executed in a context with a log for logging individual logged computations.
type WithLogBuilder =
  new: unit -> WithLogBuilder

  member Delay: (unit -> Job<WithLog<'x>>) -> Job<WithLog<'x>>

  member Return: 'x -> Job<WithLog<'x>>

  member ReturnFrom: Job<WithLog<'x>> -> Job<WithLog<'x>>
  member ReturnFrom: EmbeddedJob<'x>  -> Job<WithLog<'x>>

  member Bind: Job<WithLog<'x>> * ('x -> Job<WithLog<'y>>) -> Job<WithLog<'y>>
  member Bind: EmbeddedJob<'x>  * ('x -> Job<WithLog<'y>>) -> Job<WithLog<'y>>

  member TryFinally: Job<WithLog<'x>> * (unit -> unit) -> Job<WithLog<'x>>
  member TryWith: Job<WithLog<'x>> * (exn -> Job<WithLog<'x>>) -> Job<WithLog<'x>>

  member Using: 'x * ('x -> Job<WithLog<'x>>) -> Job<WithLog<'x>>

  member For: seq<'x> * ('x -> Job<WithLog<unit>>) -> Job<WithLog<unit>>

  member While: (unit -> bool) * Job<WithLog<unit>> -> Job<unit>

  member Zero: unit -> Job<WithLog<unit>>

  member ReturnFrom: Job<Logged<'x>> -> Job<WithLog<Logged<'x>>>
  member Bind: Job<Logged<'x>> * (Logged<'x> -> Job<WithLog<'y>>) -> Job<WithLog<'y>>

/// Builder for running a parallel computations with a log.
type [<Class>] RunWithLogBuilder =
  inherit WithLogBuilder
  member Run: Job<WithLog<'x>> -> Job<'x>

/// Operations for defining computations with Recall.
[<AutoOpen>]
module Recall =
  /// Creates a job that creates a new or reads an existing computation log
  /// stored in the specified directory, creates and runs the given update
  /// computation and then waits until all the logged computations have either
  /// finished successfully or some computations have failed and the rest have
  /// been canceled.  In case all computations finished successfully, the result
  /// value is produced.  Otherwise an exception is raised with details on the
  /// failed computations.
  val recall: logDir: string -> RunWithLogBuilder

  /// A builder for parallel computations with a log.
  val logged: WithLogBuilder

  /// Returns a builder for creating a new logged computation with the given
  /// identity, which must be unique.  The result of the computation is
  /// persisted to a computation log along with dependencies to other logged
  /// computations.
#if DOC
  ///
  /// The first time a logged computations is created, it is always run to
  /// completion.  When a logged computation with the same identity is recreated
  /// it may or may not be run to completion.
  ///
  /// In case the log indicates that the recreated computation has no
  /// dependencies to other logged computations then computation is performed
  /// every time it is recreated.  This makes it convenient to essentially
  /// create new primitive operations and is also quite logical as a computation
  /// that has no inputs must either be a constant or it must use some hidden
  /// effects to compute its output.
  ///
  /// If, however, the log has a non empty sequence of dependencies to other
  /// logged computations, then the recreated computation is performed to
  /// completion only if the sequence of dependencies changes or the result of
  /// any one of those logged computations has changed.  This works correctly as
  /// long as any input that may change the result of the computation are bound
  /// as other logged computations within the defined logged computation.
#endif
  val log: id: 'id -> LoggedBuilder

  /// Returns an operation for reading the result of a logged computation.
  val read: Logged<'x> -> Job<WithLog<'x>>

  /// Provides an alternative that becomes enabled if some computation within
  /// the whole logged computation has failed.  This allows long running
  /// computation steps to cancel themselves cleanly without having to wait
  /// until the completion of the step.
#if DOC
  ///
  /// Note that Recall automatically cancels computations in case of failure.
  /// Explicit cancellation is unlikely to provide any benefits except in case
  /// of computations that perform long running embedded asynchronous
  /// operations.
#endif
  val getCancelAlt: Update<Alt<unit>>
