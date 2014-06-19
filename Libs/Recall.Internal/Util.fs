[<AutoOpen>]
module Recall.Internal.Util

open Microsoft.FSharp.NativeInterop

////////////////////////////////////////////////////////////////////////////////

let inline (|Just|Nothing|) (got, x) = if got then Just x else Nothing

////////////////////////////////////////////////////////////////////////////////

type PtrInt = int64
// Operations on plain int64 values tend to compile to better code (on 64-bit
// platforms) than operations on the nativeint struct type.

let inline alignTo (s: int) (p: PtrInt) : PtrInt =
  (p + int64 s - 1L) &&& - (int64 s)
let inline incBy (s: int) (p: PtrInt) : PtrInt =
  p + int64 s

let inline writeIfChanged (x: 'x) (p: PtrInt) =
  let ptr = NativePtr.ofNativeInt (nativeint p)
  let x' = NativePtr.read ptr
  if x <> x' then
    NativePtr.write ptr x
  incBy sizeof<'x> p

let inline read (p: PtrInt) =
  NativePtr.read (NativePtr.ofNativeInt (nativeint p))

let inline readTo (x: byref<'x>) (p: PtrInt) =
  x <- NativePtr.read (NativePtr.ofNativeInt (nativeint p))
  incBy sizeof<'x> p
