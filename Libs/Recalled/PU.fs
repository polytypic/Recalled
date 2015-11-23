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

type [<AbstractClass>] InternalPU<'x> () =
  abstract Size: byref<'x> * PtrInt -> PtrInt
  abstract Dopickle: byref<'x> * PtrInt -> PtrInt
  abstract Unpickle: PtrInt * byref<'x> -> PtrInt

type OpenPU<'x> = O of InternalPU<'x>

type RecPU<'x> () =
  inherit InternalPU<'x> ()
  [<DefaultValue>] val mutable impl: InternalPU<'x>
  override this.Size (x, i) = this.impl.Size (&x, i)
  override this.Dopickle (x, p) = this.impl.Dopickle (&x, p)
  override this.Unpickle (p, x) = this.impl.Unpickle (p, &x)

type ProductPU<'e, 'es, 'p> = P of InternalPU<'e>
type UnionPU<'c, 'cs, 'u> = U of list<InternalPU<'u>>

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
         override this.Size (_, p) =
           p
           |> skipTo sizeof<'x>
           |> skipBy sizeof<'x>
         override this.Dopickle (x, p) =
           p
           |> clearIfNotTo sizeof<'x>
           |> writeIfNot x
         override this.Unpickle (p, x) =
           p
           |> skipTo sizeof<'x>
           |> readTo &x}

  let inline mkNativeBy (toNative: 'x -> 'n) (ofNative: 'n -> 'x) : OpenPU<'x> =
    O {new InternalPU<'x> () with
         override this.Size (_, p) =
           p
           |> skipTo sizeof<'n>
           |> skipBy sizeof<'n>
         override this.Dopickle (x, p) =
           p
           |> clearIfNotTo sizeof<'n>
           |> writeIfNot (toNative x)
         override this.Unpickle (p, x) =
           let mutable (n: 'n) = Unchecked.defaultof<_>
           let p =
             p
             |> skipTo sizeof<'n>
             |> readTo &n
           x <- ofNative n
           p}

  let inline mkBytesBy (toBytes: 'x -> array<byte>) (ofBytes: array<byte> -> 'x) : OpenPU<'x> =
    O {new InternalPU<'x> () with
         override this.Size (x, p) =
           let x = toBytes x
           p
           |> skipTo sizeof<int>
           |> skipBy (sizeof<int> + x.Length)
         override this.Dopickle (x, p) =
           let x = toBytes x
           let mutable p =
             p
             |> clearIfNotTo sizeof<int>
             |> writeIfNot x.Length
           for i=0 to x.Length-1 do
             p <- writeIfNot x.[i] p
           p
         override this.Unpickle (p, x) =
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
       override this.Size (x, p) = p
       override this.Dopickle (x, p) = p
       override this.Unpickle (p, x) = x <- value ; p}

  let inline mkSeq (toArray: 'xs -> array<'x>) (ofArray: array<'x> -> 'xs) (O e: OpenPU<'x>) =
    O {new InternalPU<'xs> () with
         override this.Size (x, p) =
           let x = toArray x
           let mutable p =
             p
             |> skipTo sizeof<int>
             |> skipBy sizeof<int>
           for i=0 to x.Length-1 do
             p <- e.Size (&x.[i], p)
           p
         override this.Dopickle (x, p) =
           let x = toArray x
           let mutable p =
             p
             |> clearIfNotTo sizeof<int>
             |> writeIfNot x.Length
           for i=0 to x.Length-1 do
             p <- e.Dopickle (&x.[i], p)
           p
         override this.Unpickle (p, x) =
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

  let inline mkProduct (P f:  ProductPU<    'f,       And<'f, 'fs>, 'r>)
                       (P fs: ProductPU<        'fs ,         'fs , 'r>)
                            : ProductPU<And<'f, 'fs>, And<'f, 'fs>, 'r> =
    P {new InternalPU<And<'f, 'fs>> () with
         override this.Size (ffs, p) =
           let p = f.Size (&ffs.Elem, p)
           fs.Size (&ffs.Rest, p)
         override this.Dopickle (ffs, p) =
           let p = f.Dopickle (&ffs.Elem, p)
           fs.Dopickle (&ffs.Rest, p)
         override this.Unpickle (p, ffs) =
           let p = f.Unpickle (p, &ffs.Elem)
           fs.Unpickle (p, &ffs.Rest)}

  let inline mkTupleOrNonRecursiveRecord (asProduct: AsProduct<'p, 't>)
                                         (P ppu: ProductPU<'p, 'p, 't>) =
    {new InternalPU<'t> () with
       override this.Size (x, p) =
         let mutable px = asProduct.ToProduct x
         ppu.Size (&px, p)
       override this.Dopickle (x, p) =
         let mutable px = asProduct.ToProduct x
         ppu.Dopickle (&px, p)
       override this.Unpickle (p, x) =
         let mutable px = Unchecked.defaultof<_>
         let p = ppu.Unpickle (p, &px)
         x <- asProduct.Create (&px)
         p}

  let inline mkUnion (m: Union<'u>) (U u: UnionPU<'c, 'c, 'u>) =
    let cs = Array.ofList u
    if 256 < cs.Length then
      failwith "Unions with more than 256 cases are not yet supported"
    O {new InternalPU<'u> () with
         override this.Size (x, p) =
           let p = skipBy sizeof<uint8> p
           let i = m.Tag x
           cs.[i].Size (&x, p)
         override this.Dopickle (x, p) =
           let i = m.Tag x
           let p = writeIfNot (uint8 i) p
           cs.[i].Dopickle (&x, p)
         override this.Unpickle (p, x) =
           let i = int32 (read p : uint8)
           let p = skipBy sizeof<uint8> p
           cs.[i].Unpickle (p, &x)}

type [<InferenceRules (StaticMap = StaticMap.Results)>] PU () =
  static member makePU () : PU<'x> =
    match Engine.TryGenerate (PU ()) with
     | None -> failwithf "PU: Unsupported type %A" typeof<'x>
     | Some pu -> pu

  static member Get () = StaticMap<PU>.Memoize PU.makePU

  member this.toPU (O pu: OpenPU<'x>) : PU<'x> =
    {new PU<'x> () with
       override this.Size (x) =
         let mutable x = x
         let sz = pu.Size (&x, 0L)
         if sz > int64 Int32.MaxValue then
           failwith "Over 2GB objects are not supported."
         int sz
       override this.Dopickle (x, p) =
         let mutable x = x
         pu.Dopickle (&x, int64 (NativePtr.toNativeInt p)) |> ignore
       override this.Unpickle (p) =
         let mutable x = Unchecked.defaultof<_>
         pu.Unpickle (int64 (NativePtr.toNativeInt p), &x) |> ignore
         x}

  member this.fix () : Rec<OpenPU<'x>> =
    let r = RecPU<'x> ()
    let o = O r
    {new Rec<OpenPU<'x>> () with
       override p.Get () = o
       override p.Set (O x) = r.impl <- x}

  member this.unit: OpenPU<unit> = mkConst () |> O

  member this.bool: OpenPU<bool> = mkNative ()

  member this.int8: OpenPU<int8> = mkNative ()
  member this.int16: OpenPU<int16> = mkNative ()
  member this.int32: OpenPU<int32> = mkNative ()
  member this.int64: OpenPU<int64> = mkNative ()

  member this.uint8: OpenPU<uint8> = mkNative ()
  member this.uint16: OpenPU<uint16> = mkNative ()
  member this.uint32: OpenPU<uint32> = mkNative ()
  member this.uint64: OpenPU<uint64> = mkNative ()

  member this.float32: OpenPU<float32> =
    mkNativeBy Float32.toUInt32 Float32.ofUInt32
  member this.float64: OpenPU<float> =
    mkNativeBy Float.toUInt64 Float.ofUInt64

  member this.char: OpenPU<char> = mkNative ()
  member this.string: OpenPU<string> =
    O {new InternalPU<string> () with
         override this.Size (x, p) =
           p
           |> skipTo sizeof<int>
           |> skipBy (sizeof<int> + sizeof<char> * x.Length)
         override this.Dopickle (x, p) =
           let mutable p =
             p
             |> clearIfNotTo sizeof<int>
             |> writeIfNot x.Length
           for i=0 to x.Length-1 do
             p <- writeIfNot x.[i] p
           p
         override this.Unpickle (p, x) =
           let mutable p = skipTo sizeof<int> p
           let n = read p
           p <- skipBy sizeof<int> p
           let cs = Array.zeroCreate n
           for i=0 to n-1 do
             p <- readTo &cs.[i] p
           x <- String (cs)
           p}

  member this.DateTime: OpenPU<DateTime> =
    mkNativeBy (fun d -> d.Ticks) (fun t -> DateTime t)

  member this.Digest: OpenPU<Digest> =
    O {new InternalPU<Digest> () with
         override this.Size (x, p) =
           p
           |> skipTo sizeof<uint64>
           |> skipBy (2 * sizeof<uint64>)
         override this.Dopickle (x, p) =
           p
           |> clearIfNotTo sizeof<uint64>
           |> writeIfNot x.Lo
           |> writeIfNot x.Hi
         override this.Unpickle (p, x) =
           p
           |> skipTo sizeof<uint64>
           |> readTo &x.Lo
           |> readTo &x.Hi}
           
  member this.BigInteger: OpenPU<BigInteger> =
    mkBytesBy (fun x -> x.ToByteArray ()) (fun x -> BigInteger (x))

  member this.bytes: OpenPU<array<byte>> =
    mkBytesBy id id

  member this.list (t: OpenPU<'x>) : OpenPU<list<'x>> =
    mkSeq List.toArray List.ofArray t

  member this.array (t: OpenPU<'a>) : OpenPU<array<'a>> =
    mkSeq id id t

  member this.case (m: Case<Empty, 'cs, 'u>)
                  : UnionPU<Empty, 'cs, 'u> =
    U [mkConst (let mutable n = Unchecked.defaultof<_> in m.Create (&n))]
  member this.case (m: Case<'ls, 'cs, 'u>,
                    p: ProductPU<'ls, 'ls, 'u>)
                  : UnionPU<'ls, 'cs, 'u> = 
    U [mkTupleOrNonRecursiveRecord m p]

  member this.plus (U c:  UnionPU<       'c,       Choice<'c, 'cs>, 'u>,
                    U cs: UnionPU<           'cs ,            'cs , 'u>)
                        : UnionPU<Choice<'c, 'cs>, Choice<'c, 'cs>, 'u> =
    U (c @ cs)

  member this.union (_: Rep,
                     m: Union<'u>,
                     _: AsChoice<'c, 'u>,
                     u: UnionPU<'c, 'c, 'u>) : OpenPU<'u> =
    mkUnion m u

  member this.times (f:  ProductPU< 'f,       And<'f, 'fs>, 'r>,
                     fs: ProductPU<     'fs ,         'fs , 'r>)
                    : ProductPU<And<'f, 'fs>, And<'f, 'fs>, 'r> =
    mkProduct f fs
    
  member this.elem (_: Elem<'e, 'p, 't>,
                    t: OpenPU<'e>)
                  : ProductPU<'e, 'p, 't> =
    mkElemOrField t

  member this.product (_: Rep,
                       _: Product<'t>,
                       m: AsProduct<'p, 't>,
                       p: ProductPU<'p, 'p, 't>) : OpenPU<'t> =
    O (mkTupleOrNonRecursiveRecord m p)
