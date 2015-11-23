/// The Recalled.Internal library implements the internal mechanisms used by the
/// Recalled library and associated tools.  The Recalled.Internal library is not
/// meant to be directly used by programs written with the Recalled library.
namespace Recalled.Internal

open Microsoft.FSharp.NativeInterop
open System.IO
open System.Collections.Generic

/// Represents a digest or hash of some data or value.
///
#if DOC
/// Note that user code should never need to directly modify digest values.  The
/// interface for manipulating digest values is very low level and imperative,
/// because Recalled computes digests of anything and everything and any
/// performance difference on digest operations has a noticeable effect on
/// the overall performance of Recalled.
///
/// Note that the digest algorithms used with Recalled do not need to be
/// cryptographically secure, but should ideally make collisions highly
/// unlikely.  Currently Recalled uses the `MurmurHash3` algorithm to compute
/// digests, but the digest algorithm may be changed in the future.
#endif
type [<NoComparison; CustomEquality>] Digest = struct
    /// Bits 0 to 63 of the digest.
    val mutable Lo: uint64
    /// Bits 64 to 127 of the digest.
    val mutable Hi: uint64

    /// Returns zero if the digests are equal.  Neither of the digests are
    /// modified.
    static member inline ZeroIfEq: byref<Digest> * byref<Digest> -> uint64

    /// Combine two digests.  The result is written to the first digest and the
    /// second digest is not modified.
    static member inline Combine: byref<Digest> * byref<Digest> -> unit

    /// Computes a digest of the specified region of memory.  The result is
    /// written to the specified digest variable.
    static member inline Bytes: ptr: nativeptr<byte> * count: int * byref<Digest> -> unit

    /// Computes a digest of the given string.  The result is written to the
    /// specified digest variable.
    static member inline String: string * byref<Digest> -> unit
  end

/// Equality comparer for digests.
type [<Class>] DigestEqualityComparer =
 interface IEqualityComparer<Digest>

 /// An empty default constructor.
 new: unit -> DigestEqualityComparer

/// A mutable dictionary of digests.
type DigestDict<'v>

/// Operations on digest dictionaries.
module DigestDict =
  /// The empty digest dictionary.
  val empty: DigestDict<'v>

  /// Atomically gets the value associated with the digest or 
  val getOrAdd: dict: byref<DigestDict<'v>>
             -> digest: byref<Digest>
             -> seed: byref<'r>
             -> create: ByRefToValue<'r, 'v>
             -> added: byref<bool>
             -> 'v
