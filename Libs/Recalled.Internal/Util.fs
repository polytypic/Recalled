[<AutoOpen>]
module Recalled.Internal.Util

open Microsoft.FSharp.NativeInterop

////////////////////////////////////////////////////////////////////////////////

let inline (|Just|Nothing|) (got, x) = if got then Just x else Nothing

////////////////////////////////////////////////////////////////////////////////

type [<AbstractClass>] ByRefToValue<'d, 'r> () =
  abstract Invoke: byref<'d> -> 'r

////////////////////////////////////////////////////////////////////////////////

type Default<'t when 't: (new: unit -> 't)> () =
  static let instance = new 't ()
  static member Get () = instance

////////////////////////////////////////////////////////////////////////////////

type [<Struct>] Struct<'t1, 't2> (t1: 't1, t2: 't2) =
  member this.T1 = t1
  member this.T2 = t2

type [<Struct>] Struct<'t1, 't2, 't3> (t1: 't1, t2: 't2, t3: 't3) =
  member this.T1 = t1
  member this.T2 = t2
  member this.T3 = t3

////////////////////////////////////////////////////////////////////////////////

type PtrInt = int64
// Operations on plain int64 values tend to compile to better code (on 64-bit
// platforms) than operations on the nativeint struct type.

let inline skipTo (s: int) (p: PtrInt) : PtrInt =
  (p + int64 s - 1L) &&& - (int64 s)
let inline skipBy (s: int) (p: PtrInt) : PtrInt =
  p + int64 s

let inline writeIfNot (x: 'x) (p: PtrInt) =
  let ptr = NativePtr.ofNativeInt (nativeint p)
  if x <> NativePtr.read ptr then
    NativePtr.write ptr x
  skipBy sizeof<'x> p

let inline clearIfNotTo (s: int) (p: PtrInt) =
  let inline clearBy (bit: int) (zero: 'x) (p: PtrInt) =
    if bit < s then
      if 0 <> (int p &&& bit) then
        writeIfNot zero p
      else
        p
    else
      p
  p
  |> clearBy 1 0uy
  |> clearBy 2 0us
  |> clearBy 4 0u

let inline read (p: PtrInt) =
  NativePtr.read (NativePtr.ofNativeInt (nativeint p))

let inline readTo (x: byref<'x>) (p: PtrInt) =
  x <- NativePtr.read (NativePtr.ofNativeInt (nativeint p))
  skipBy sizeof<'x> p
