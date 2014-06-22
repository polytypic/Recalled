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
/// User code should never need to modify digest values.  Recalled computes
/// digests of anything and everything and having the ability to mutate digests
/// values internally makes a difference.
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

    static member inline ZeroIfEq: byref<Digest> * byref<Digest> -> uint64

    /// Combine two digests.
    static member inline Combine: byref<Digest> * byref<Digest> -> unit

    /// Computes a digest of the specified region of memory.
    static member inline Bytes: ptr: nativeptr<byte> * count: int * byref<Digest> -> unit

    /// Computes a digest of the given string.
    static member inline String: string * byref<Digest> -> unit
  end

/// Equality comparer for digests.
type [<Class>] DigestEqualityComparer =
 interface IEqualityComparer<Digest>

 /// An empty default constructor.
 new: unit -> DigestEqualityComparer
