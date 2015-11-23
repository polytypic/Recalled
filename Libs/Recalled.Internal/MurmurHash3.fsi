namespace Recalled.Internal

open Microsoft.FSharp.NativeInterop

/// Implementation of the MurmurHash3 algorithm for Recalled.
module MurmurHash3 =
  /// Computes a 128-bit hash of the specified block of memory and stores the
  /// lower and higher 64-bits of the result to the specified variables.
  val bytes: data: nativeptr<byte>
          -> length: int
          -> seed: uint32
          -> lo: byref<uint64>
          -> hi: byref<uint64>
          -> unit

  /// Computes a 128-bit hash of the specified string and stores the lower and
  /// higher 64-bits of the result to the specified variables.
  val string: string: string
           -> seed: uint32
           -> lo: byref<uint64>
           -> hi: byref<uint64>
           -> unit
