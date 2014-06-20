namespace Recalled.Internal

open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

module MurmurHash3 =
  let inline private ROTL64 (x: uint64) r = (x <<< r) ||| (x >>> (64 - r))

  let inline private fmix64 (k: uint64) =
    let k = k ^^^ (k >>> 33)
    let k = k * 0xff51afd7ed558ccdUL
    let k = k ^^^ (k >>> 33)
    let k = k * 0xc4ceb9fe1a85ec53UL
    let k = k ^^^ (k >>> 33)
    k

  let bytes (data: nativeptr<byte>)
            (len: int)
            (seed: uint32)
            (lo: byref<uint64>)
            (hi: byref<uint64>) =
    let nblocks = len / 16

    let mutable h1 = uint64 seed
    let mutable h2 = uint64 seed

    let c1 = 0x87c37b91114253d5UL
    let c2 = 0x4cf5ad432745937fUL

    let dataAddr = NativePtr.toNativeInt data

    let mutable blockPtr : nativeptr<uint64> =
      NativePtr.ofNativeInt dataAddr
    let tail = NativePtr.add data (nblocks*16)

    while NativePtr.toNativeInt blockPtr <> NativePtr.toNativeInt tail do
      // XXX Potentially misaligned.
      let mutable k1 = NativePtr.get blockPtr 0
      let mutable k2 = NativePtr.get blockPtr 1
      blockPtr <- NativePtr.add blockPtr 2

      k1 <- k1 * c1
      k1 <- ROTL64 k1 31
      k1 <- k1 * c2
      h1 <- h1 ^^^ k1

      h1 <- ROTL64 h1 27
      h1 <- h1 + h2
      h1 <- h1 * 5UL + 0x52dce729UL

      k2 <- k2 * c2
      k2 <- ROTL64 k2 33
      k2 <- k2 * c1
      h2 <- h2 ^^^ k2

      h2 <- ROTL64 h2 31
      h2 <- h2 + h1
      h2 <- h2 * 5UL + 0x38495ab5UL

    let mutable k1 = 0UL
    let mutable k2 = 0UL

    let len' = len &&& 15

    if 15 <= len' then k2 <-                uint64 (NativePtr.get tail 14)
    if 14 <= len' then k2 <- (k2 <<< 8) ^^^ uint64 (NativePtr.get tail 13)
    if 13 <= len' then k2 <- (k2 <<< 8) ^^^ uint64 (NativePtr.get tail 12)
    if 12 <= len' then k2 <- (k2 <<< 8) ^^^ uint64 (NativePtr.get tail 11)
    if 11 <= len' then k2 <- (k2 <<< 8) ^^^ uint64 (NativePtr.get tail 10)
    if 10 <= len' then k2 <- (k2 <<< 8) ^^^ uint64 (NativePtr.get tail  9)
    if  9 <= len' then k2 <- (k2 <<< 8) ^^^ uint64 (NativePtr.get tail  8)
                       k2 <- k2 * c2
                       k2 <- ROTL64 k2 33
                       k2 <- k2 * c1
                       h2 <- h2 ^^^ k2

    if  8 <= len' then k1 <-                uint64 (NativePtr.get tail 7)
    if  7 <= len' then k1 <- (k1 <<< 8) ^^^ uint64 (NativePtr.get tail 6)
    if  6 <= len' then k1 <- (k1 <<< 8) ^^^ uint64 (NativePtr.get tail 5)
    if  5 <= len' then k1 <- (k1 <<< 8) ^^^ uint64 (NativePtr.get tail 4)
    if  4 <= len' then k1 <- (k1 <<< 8) ^^^ uint64 (NativePtr.get tail 3)
    if  3 <= len' then k1 <- (k1 <<< 8) ^^^ uint64 (NativePtr.get tail 2)
    if  2 <= len' then k1 <- (k1 <<< 8) ^^^ uint64 (NativePtr.get tail 1)
    if  1 <= len' then k1 <- (k1 <<< 8) ^^^ uint64 (NativePtr.get tail 0)
                       k1 <- k1 * c1
                       k1 <- ROTL64 k1 31
                       k1 <- k1 * c2
                       h1 <- h1 ^^^ k1

    h1 <- h1 ^^^ uint64 len
    h2 <- h2 ^^^ uint64 len

    h1 <- h1 + h2
    h2 <- h2 + h1

    h1 <- fmix64 h1
    h2 <- fmix64 h2

    h1 <- h1 + h2
    h2 <- h2 + h1

    lo <- h1
    hi <- h2

  let string (string: string)
             (seed: uint32)
             (lo: byref<uint64>)
             (hi: byref<uint64>) =
    let gch = GCHandle.Alloc (string, GCHandleType.Pinned)
    let ptr = NativePtr.ofNativeInt (gch.AddrOfPinnedObject ())
    bytes (ptr) (string.Length * 2) (seed) (&lo) (&hi)
    gch.Free ()
