namespace Recall

open Microsoft.FSharp.NativeInterop
open System.IO
open System.Collections.Generic

/// Represents a digest or hash of some data or value.
///
#if DOC
/// WARNING:  User code should never need to modify digest values.  Recall
/// computes digests of anything and everything and having the ability to
/// mutate digests values internally makes a difference.
///
/// Note that the digest algorithms used with Recall do not need to be
/// cryptographically secure, but should ideally make collisions highly
/// unlikely.  Currently Recall uses the MurmurHash3 algorithm to compute
/// digests, but the digest algorithm may be changed in the future.
#endif
type [<NoComparison; CustomEquality>] Digest = struct
    /// Bits 0 to 63 of the digest.
    val mutable Lo: uint64
    /// Bits 64 to 127 of the digest.
    val mutable Hi: uint64

    /// Creates a new digest value.
    new: lo: uint64 * hi: uint64 -> Digest

    /// The zero digest value.
    static member Zero: Digest

    /// Combine two digests.
    static member (^^^): Digest * Digest -> Digest

    /// Computes a digest of the specified region of memory.
    static member Bytes: ptr: nativeptr<byte> * count: int -> Digest

    /// Computes a digest of the given string.
    static member String: string -> Digest
  end

/// Equality comparer for digests.
type [<Class>] DigestEqualityComparer =
 interface IEqualityComparer<Digest>

 /// An empty default constructor.
 new: unit -> DigestEqualityComparer

/// Represents a capability to serialize values of type `'x`.
#if DOC
///
/// This class has a very low level interface and is designed to make it
/// possible to implement nearly optimal serialization to and deserialization
/// from memory mapped files.  This is important, because Recall serializes
/// all the results of logged computations and potentially deserializes large
/// numbers of those results.
///
/// In most cases client code should not need to implement this class directly
/// as the `PU` class provides inference rules to generate instances of this
/// class for a wide variety of F# types.
#endif
type [<AbstractClass>] PU<'x> =
  /// Empty default constructor.
  new: unit -> PU<'x>

  /// Compute the serialized size of the given value.
  abstract Size: 'x -> int

  /// Conditionally serialize given value to memory starting at the specified
  /// address.
#if DOC
  ///
  /// The caller is responsible for ensuring that the pointer points to a region
  /// of memory that has at least the number of bytes of space as returned by
  /// `Size`.  The `DoPickle` implementation must not write to memory outside of
  /// that range.
  ///
  /// Memory at the specified address should be first read and only written to
  /// if it differs from the value being written.  This allows the space for a
  /// previous value of the same type and size to be reused and, at the same
  /// time, make sure that persistent storage is only written to when the stored
  /// value is different.
#endif
  abstract Dopickle: 'x * nativeptr<byte> -> unit

  /// Deserialize value from memory starting at the specified address.
  abstract Unpickle: nativeptr<byte> -> 'x
