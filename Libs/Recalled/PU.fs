namespace Recalled

open Recalled.Internal
open Microsoft.FSharp.NativeInterop
open System.Numerics
open System
open System.IO
open Infers
open Infers.Rep

type [<AbstractClass>] PU<'x> () =
  abstract Size: 'x -> int
  abstract Dopickle: 'x * nativeptr<byte> -> unit
  abstract Unpickle: nativeptr<byte> -> 'x

module PU =
  type [<AbstractClass>] InternalPU<'x> () =
    abstract Size: byref<'x> * PtrInt -> PtrInt
    abstract Dopickle: byref<'x> * PtrInt -> PtrInt
    abstract Unpickle: PtrInt * byref<'x> -> PtrInt

  type OpenPU<'x> = O of InternalPU<'x>

  type RecPU<'x> () =
    inherit InternalPU<'x> ()
    [<DefaultValue>] val mutable impl: InternalPU<'x>
    override t.Size (x, i) = t.impl.Size (&x, i)
    override t.Dopickle (x, p) = t.impl.Dopickle (&x, p)
    override t.Unpickle (p, x) = t.impl.Unpickle (p, &x)

  type ProductPU<'e, 'r, 'o, 't> = P of InternalPU<'e>
  type UnionPU<'p, 'o, 't> = U of list<InternalPU<'t>>

  [<AutoOpen>]
  module internal UtilPU =
    module Unsafe =
      let inline bitcast (x: 'x) : 'y =
        let p = NativePtr.stackalloc sizeof<'x>
        NativePtr.write p x
        let p = p |> NativePtr.toNativeInt |> NativePtr.ofNativeInt
        NativePtr.read p

    module Float32 =
      let inline toUInt32 (x: float32) : uint32 = Unsafe.bitcast x
      let inline ofUInt32 (x: uint32) : float32 = Unsafe.bitcast x

    module Float =
      let inline toUInt64 (x: float) : uint64 = Unsafe.bitcast x
      let inline ofUInt64 (x: uint64) : float = Unsafe.bitcast x

    let inline mkNative () : OpenPU<'x> =
      O {new InternalPU<'x> () with
           override t.Size (_, p) =
             p
             |> skipTo sizeof<'x>
             |> skipBy sizeof<'x>
           override t.Dopickle (x, p) =
             p
             |> clearIfNotTo sizeof<'x>
             |> writeIfNot x
           override t.Unpickle (p, x) =
             p
             |> skipTo sizeof<'x>
             |> readTo &x}

    let inline mkNativeBy (toNative: 'x -> 'n) (ofNative: 'n -> 'x) =
      O {new InternalPU<'x> () with
           override t.Size (_, p) =
             p
             |> skipTo sizeof<'n>
             |> skipBy sizeof<'n>
           override t.Dopickle (x, p) =
             p
             |> clearIfNotTo sizeof<'n>
             |> writeIfNot (toNative x)
           override t.Unpickle (p, x) =
             let mutable (n: 'n) = Unchecked.defaultof<_>
             let p =
               p
               |> skipTo sizeof<'n>
               |> readTo &n
             x <- ofNative n
             p}

    let inline mkBytesBy (toBytes: 'x -> array<byte>)
                         (ofBytes: array<byte> -> 'x) =
      O {new InternalPU<'x> () with
           override t.Size (x, p) =
             let x = toBytes x
             p
             |> skipTo sizeof<int>
             |> skipBy (sizeof<int> + x.Length)
           override t.Dopickle (x, p) =
             let x = toBytes x
             let mutable p =
               p
               |> clearIfNotTo sizeof<int>
               |> writeIfNot x.Length
             for i=0 to x.Length-1 do
               p <- writeIfNot x.[i] p
             p
           override t.Unpickle (p, x) =
             let mutable p = skipTo sizeof<int> p
             let n = read p
             p <- skipBy sizeof<int> p
             let bs = Array.zeroCreate n
             for i=0 to n-1 do
               p <- readTo &bs.[i] p
             x <- ofBytes bs
             p}

    let inline mkConst (value: 'x) =
      {new InternalPU<'x> () with
         override t.Size (x, p) = p
         override t.Dopickle (x, p) = p
         override t.Unpickle (p, x) = x <- value ; p}

    let inline mkSeq (toArray: 'xs -> array<'x>)
                     (ofArray: array<'x> -> 'xs)
                     (O e: OpenPU<'x>) =
      O {new InternalPU<'xs> () with
           override t.Size (x, p) =
             let x = toArray x
             let mutable p =
               p
               |> skipTo sizeof<int>
               |> skipBy sizeof<int>
             for i=0 to x.Length-1 do
               p <- e.Size (&x.[i], p)
             p
           override t.Dopickle (x, p) =
             let x = toArray x
             let mutable p =
               p
               |> clearIfNotTo sizeof<int>
               |> writeIfNot x.Length
             for i=0 to x.Length-1 do
               p <- e.Dopickle (&x.[i], p)
             p
           override t.Unpickle (p, x) =
             let mutable n = 0
             let mutable p =
               p
               |> skipTo sizeof<int>
               |> readTo &n
             let a = Array.zeroCreate n
             for i=0 to n-1 do
               p <- e.Unpickle (p, &a.[i])
             x <- ofArray a
             p}

    let inline mkElemOrField (O t: OpenPU<'e>) =
      P t

    let inline mkProduct (P f:  ProductPU<     'e,      Pair<'e, 'r>, 'o, 't>)
                         (P fs: ProductPU<         'r ,          'r , 'o, 't>)
                              : ProductPU<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 't> =
      P {new InternalPU<Pair<'e, 'r>> () with
           override t.Size (ffs, p) =
             let p = f.Size (&ffs.Elem, p)
             fs.Size (&ffs.Rest, p)
           override t.Dopickle (ffs, p) =
             let p = f.Dopickle (&ffs.Elem, p)
             fs.Dopickle (&ffs.Rest, p)
           override t.Unpickle (p, ffs) =
             let p = f.Unpickle (p, &ffs.Elem)
             fs.Unpickle (p, &ffs.Rest)}

    let inline mkTupleOrNonRecursiveRecord (asP: AsPairs<'p, 'o, 't>)
                                           (P ppu: ProductPU<'p, 'p, 'o, 't>) =
      {new InternalPU<'t> () with
         override t.Size (x, p) =
           let mutable px = asP.ToPairs x
           ppu.Size (&px, p)
         override t.Dopickle (x, p) =
           let mutable px = asP.ToPairs x
           ppu.Dopickle (&px, p)
         override t.Unpickle (p, x) =
           let mutable px = Unchecked.defaultof<_>
           let p = ppu.Unpickle (p, &px)
           x <- asP.Create (&px)
           p}

    let inline mkUnion (asC: AsChoices<'s, 't>) (U u: UnionPU<'s, 's, 't>) =
      let cs = Array.ofList u
      if 256 < cs.Length then
        failwith "Unions with more than 256 cases are not yet supported"
      O {new InternalPU<'t> () with
           override t.Size (x, p) =
             let p = skipBy sizeof<uint8> p
             let i = asC.Tag x
             cs.[i].Size (&x, p)
           override t.Dopickle (x, p) =
             let i = asC.Tag x
             let p = writeIfNot (uint8 i) p
             cs.[i].Dopickle (&x, p)
           override t.Unpickle (p, x) =
             let i = int32 (read p : uint8)
             let p = skipBy sizeof<uint8> p
             cs.[i].Unpickle (p, &x)}

  type [<Rep>] PU () =
    inherit Rules ()

    static member toPU (O pu: OpenPU<'x>) : PU<'x> =
      {new PU<'x> () with
         override t.Size (x) =
           let mutable x = x
           let sz = pu.Size (&x, 0L)
           if sz > int64 Int32.MaxValue then
             failwith "Over 2GB objects are not supported."
           int sz
         override t.Dopickle (x, p) =
           let mutable x = x
           pu.Dopickle (&x, int64 (NativePtr.toNativeInt p)) |> ignore
         override t.Unpickle (p) =
           let mutable x = Unchecked.defaultof<_>
           pu.Unpickle (int64 (NativePtr.toNativeInt p), &x) |> ignore
           x}

    static member fix () : Rec<OpenPU<'x>> =
      let r = RecPU<'x> ()
      let o = O r
      {new Rec<OpenPU<'x>> () with
         override p.Get () = o
         override p.Set (O x) = r.impl <- x}

    static member unit: OpenPU<unit> = mkConst () |> O

    static member bool: OpenPU<bool> = mkNative ()

    static member int8: OpenPU<int8> = mkNative ()
    static member int16: OpenPU<int16> = mkNative ()
    static member int32: OpenPU<int32> = mkNative ()
    static member int64: OpenPU<int64> = mkNative ()

    static member uint8: OpenPU<uint8> = mkNative ()
    static member uint16: OpenPU<uint16> = mkNative ()
    static member uint32: OpenPU<uint32> = mkNative ()
    static member uint64: OpenPU<uint64> = mkNative ()

    static member float32: OpenPU<float32> =
      mkNativeBy Float32.toUInt32 Float32.ofUInt32
    static member float64: OpenPU<float> =
      mkNativeBy Float.toUInt64 Float.ofUInt64

    static member char: OpenPU<char> = mkNative ()
    static member string: OpenPU<string> =
      O {new InternalPU<string> () with
           override t.Size (x, p) =
             p
             |> skipTo sizeof<int>
             |> skipBy (sizeof<int> + sizeof<char> * x.Length)
           override t.Dopickle (x, p) =
             let mutable p =
               p
               |> clearIfNotTo sizeof<int>
               |> writeIfNot x.Length
             for i=0 to x.Length-1 do
               p <- writeIfNot x.[i] p
             p
           override t.Unpickle (p, x) =
             let mutable p = skipTo sizeof<int> p
             let n = read p
             p <- skipBy sizeof<int> p
             let cs = Array.zeroCreate n
             for i=0 to n-1 do
               p <- readTo &cs.[i] p
             x <- String (cs)
             p}

    static member DateTime: OpenPU<DateTime> =
      mkNativeBy (fun d -> d.Ticks) (fun t -> DateTime t)

    static member Digest: OpenPU<Digest> =
      O {new InternalPU<Digest> () with
           override t.Size (x, p) =
             p
             |> skipTo sizeof<uint64>
             |> skipBy (2 * sizeof<uint64>)
           override t.Dopickle (x, p) =
             p
             |> clearIfNotTo sizeof<uint64>
             |> writeIfNot x.Lo
             |> writeIfNot x.Hi
           override t.Unpickle (p, x) =
             p
             |> skipTo sizeof<uint64>
             |> readTo &x.Lo
             |> readTo &x.Hi}
             
    static member BigInteger: OpenPU<BigInteger> =
      mkBytesBy (fun x -> x.ToByteArray ()) (fun x -> BigInteger (x))

    static member bytes: OpenPU<array<byte>> =
      mkBytesBy id id

    static member list (t: OpenPU<'x>) : OpenPU<list<'x>> =
      mkSeq List.toArray List.ofArray t

    static member array (t: OpenPU<'a>) : OpenPU<array<'a>> =
      mkSeq id id t

    static member case (m: Case<Empty, 'o, 't>) : UnionPU<Empty, 'o, 't> =
      U [mkConst (let mutable n = Unchecked.defaultof<_> in m.Create (&n))]
    static member case (m: Case<'p, 'o, 't>, p: ProductPU<'p, 'p, 'o, 't>) = 
      U [mkTupleOrNonRecursiveRecord m p] : UnionPU<'p, 'o, 't>

    static member choice (U c:  UnionPU<       'p,      Choice<'p, 'o>, 't>,
                          U cs: UnionPU<           'o ,            'o , 't>) =
      U (c @ cs)              : UnionPU<Choice<'p, 'o>, Choice<'p, 'o>, 't>

    static member sum (asC: AsChoices<'s, 't>, u: UnionPU<'s, 's, 't>) =
      mkUnion asC u

    static member pair (f:  ProductPU<     'e,      Pair<'e, 'r>, 'o, 't>,
                        fs: ProductPU<         'r ,          'r , 'o, 't>) =
      mkProduct f fs      : ProductPU<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 't>
      
    static member elem (_: Elem<'e, 'r, 'o, 't>, t: OpenPU<'e>) =
      mkElemOrField t : ProductPU<'e, 'r, 'o, 't>

    static member product (asP: AsPairs<'p, 'o, 't>,
                           p: ProductPU<'p, 'p, 'o, 't>) =
      O (mkTupleOrNonRecursiveRecord asP p)

  let pu<'x> = generateDFS<PU, PU<'x>>
