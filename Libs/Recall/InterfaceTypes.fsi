namespace Recall

open System.IO

/// Represents a digest or hash of some data or value.
#if DOC
///
/// Note that the digest algorithms used with Recall do not need to be
/// cryptographically secure, but should ideally make collisions highly
/// unlikely.
#endif
type [<NoComparison; CustomEquality>] Digest = struct
    val mutable Lo: uint64
    val mutable Hi: uint64

    new: lo: uint64 * hi: uint64 -> Digest
    new: bytes: array<byte> -> Digest

    static member Zero: Digest

    static member (^^^): Digest * Digest -> Digest

    static member Bytes: array<byte> -> Digest
    static member String: string -> Digest
    static member Stream: Stream -> Digest
  end

type [<Class>] DigestEqualityComparer =
 interface System.Collections.Generic.IEqualityComparer<Digest>
 new: unit -> DigestEqualityComparer

/// Represents a capability to serialize values of type `'x`.
#if DOC
///
/// This class is designed for high-performance serialization with memory mapped
/// files.  In most cases client code should not need to implement this class
/// directly as the `PU` class provides inference rules to generate instances of
/// this class for a wide variety of F# types.
#endif
type [<AbstractClass>] PU<'x> =
  /// Empty default constructor.
  new: unit -> PU<'x>

  /// Compute the pickled size of the given value.
  abstract Size: 'x -> int

  /// Conditionally write value to memory starting at the specified address.
  /// Memory at the specified address is first read and only written to if it
  /// differs from the value being written.
#if DOC
  ///
  /// The caller is responsible for ensuring that the pointer points to a region
  /// of memory that has at least the number of bytes of space as returned by
  /// `Size`.
#endif
  abstract Dopickle: 'x * nativeptr<byte> -> unit

  /// Read value from memory starting at the specified address.
  abstract Unpickle: nativeptr<byte> -> 'x
