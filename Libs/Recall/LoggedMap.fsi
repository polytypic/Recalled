namespace Recall

open Hopac

/// Operations on logged maps.
#if DOC
///
/// This module is mainly for internal use of the Recall library and associated
/// tools.  Proper use of this module requires intimate knowledge of the
/// internals of Recall.
#endif
module LoggedMap =
  /// Represents a logged map.
#if DOC
  ///
  /// Recall uses a logged map to log information on computations, including
  /// information on dependencies of computations and and the results of
  /// computations, stored as separate binary objects or bob.  The term bob
  /// is used, because storage of both large and small objects is supported.
  ///
  /// Operations on logged maps work incrementally.  Operations that change the
  /// contents of the map append new data to the end of the log files.  When an
  /// existing logged map is opened, the logged operations are effectively
  /// replayed to reconstruct the contents of the logged map.
#endif
  type LoggedMap

  /// Represents information on an entry stored in a logged map.
#if DOC
  ///
  /// Please note that this information is for internal use of the Recall
  /// library and associated tools.
#endif
  type Info = {
      /// Digests of the identities of the dependencies of the computation.
      DepKeyDigests: array<Digest>
      /// A combined digest of the results of all the dependencies.
      DepDigest: Digest
      /// Digest of the result of the computation.
      BobDigest: Digest
      /// Offset to the serialized result of the computation in the bob buffer
      /// of the log.
      BobOffset: PtrInt
      /// Size of the serialized result in bytes.
      BobSize: int
      /// Offset to the entry in the add buffer of the log.
      AddOffset: PtrInt
    }

  /// Creates a new or opens an existing logged map.  In case an existing logged
  /// map is opened, control is returned immediately to the caller, but a
  /// process runs to reconstruct the logged map in the background.
  val create: logDir: string
           -> Job<LoggedMap>

  /// Closes the logged map.  This must be called explicitly and the caller must
  /// wait for the alternative to make sure everything written to the logged map
  /// really will be persisted.
  val close: loggedMap: LoggedMap -> Job<Alt<unit>>

  /// Tries to find an entry from the logged map.  The alternative becomes
  /// available as soon as it is known whether the logged map contains the
  /// desired entry.
  val tryFind: loggedMap: LoggedMap
            -> keyDigest: Digest
            -> Alt<option<Info>>

  /// Adds an entry to the logged map.
  val add: loggedMap: LoggedMap
        -> keyDigest: Digest
        -> depKeyDigests: array<Digest>
        -> depDigest: Digest
        -> bobSize: int
        -> bobWrite: (nativeptr<byte> -> unit)
        -> Job<Info>

  /// Grants direct access to the bob storage of the logged map.
  val readFun: loggedMap: LoggedMap
            -> readFun: (nativeptr<byte> -> 'x)
            -> Job<'x>
