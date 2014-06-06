namespace Recall

open System.IO

/// Represents a digest or hash of some data or value.
#if DOC
///
/// Note that the digest algorithms used with Recall do not need to be
/// cryptographically secure, but should ideally make collisions highly
/// unlikely.
#endif
type Digest = struct
    val Lo: uint64
    val Hi: uint64
    new: lo: uint64 * hi: uint64 -> Digest
  end

/// Represents a capability to serialize values of type `'x`.
type [<AbstractClass>] PU<'x> =
  /// Empty default constructor.
  new: unit -> PU<'x>

  /// Writes given value to the specified binary writer.
  abstract Dopickle: BinaryWriter * 'x -> unit

  /// Reads a value from the given binary reader.
  abstract Unpickle: BinaryReader -> 'x
