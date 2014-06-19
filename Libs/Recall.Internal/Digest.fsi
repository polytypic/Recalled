/// The Recall.Internal library implements the internal mechanisms used by the
/// Recall library and associated tools.  The Recall.Internal library is not
/// meant to be directly used by programs written with the Recall library.
namespace Recall.Internal

open Microsoft.FSharp.NativeInterop
open System.IO
open System.Collections.Generic

/// Represents a digest or hash of some data or value.
///
#if DOC
/// User code should never need to modify digest values.  Recall computes
/// digests of anything and everything and having the ability to mutate digests
/// values internally makes a difference.
///
/// Note that the digest algorithms used with Recall do not need to be
/// cryptographically secure, but should ideally make collisions highly
/// unlikely.  Currently Recall uses the `MurmurHash3` algorithm to compute
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
