namespace Recall

open Microsoft.FSharp.NativeInterop
open System.Numerics
open System
open System.IO
open Infers
open Infers.Rep

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
  type Unique =
   | Unique

  let inline memoize (mk: unit -> 'x) =
    match StaticMap<Unique, option<'x>>.Get () with
     | None ->
       let x = mk ()
       StaticMap<Unique, option<'x>>.Set (Some x)
       x
     | Some x -> x

  let inline mkNative () : OpenPU<'x> =
    O {new InternalPU<'x> () with
         override this.Size (_, p) = incBy sizeof<'x> (alignTo sizeof<'x> p)
         override this.Dopickle (x, p) =
           p
           |> alignTo sizeof<'x>
           |> writeIfChanged x
         override this.Unpickle (p, x) =
           p
           |> alignTo sizeof<'x>
           |> readTo (&x)}

  let inline mkNativeBy (toNative: 'x -> 'n) (ofNative: 'n -> 'x) : OpenPU<'x> =
    O {new InternalPU<'x> () with
         override this.Size (_, p) = incBy sizeof<'n> (alignTo sizeof<'n> p)
         override this.Dopickle (x, p) =
           p
           |> alignTo sizeof<'n>
           |> writeIfChanged (toNative x)
         override this.Unpickle (p, x) =
           let mutable (n: 'n) = Unchecked.defaultof<_>
           let p =
             p
             |> alignTo sizeof<'n>
             |> readTo (&n)
           x <- ofNative n
           p}

  let inline mkBytesBy (toBytes: 'x -> array<byte>) (ofBytes: array<byte> -> 'x) : OpenPU<'x> =
    O {new InternalPU<'x> () with
         override this.Size (x, p) =
           let x = toBytes x
           p
           |> alignTo sizeof<int>
           |> incBy (sizeof<int> + x.Length)
         override this.Dopickle (x, p) =
           let x = toBytes x
           let mutable p =
             p
             |> alignTo sizeof<int>
             |> writeIfChanged x.Length
           for i=0 to x.Length-1 do
             p <- writeIfChanged x.[i] p
           p
         override this.Unpickle (p, x) =
           let mutable p = alignTo sizeof<int> p
           let n = read p
           p <- incBy sizeof<int> p
           let bs = Array.zeroCreate n
           for i=0 to n-1 do
             bs.[i] <- read p
             p <- incBy sizeof<byte> p
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
             |> alignTo sizeof<int>
             |> incBy sizeof<int>
           for i=0 to x.Length-1 do
             p <- e.Size (&x.[i], p)
           p
         override this.Dopickle (x, p) =
           let x = toArray x
           let mutable p =
             p
             |> alignTo sizeof<int>
             |> writeIfChanged x.Length
           for i=0 to x.Length-1 do
             p <- e.Dopickle (&x.[i], p)
           p
         override this.Unpickle (p, x) =
           let mutable n = 0
           let mutable p =
             p
             |> alignTo sizeof<int>
             |> readTo (&n)
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
           let p = incBy sizeof<uint8> p
           let i = m.Tag x
           cs.[i].Size (&x, p)
         override this.Dopickle (x, p) =
           let i = m.Tag x
           let p = writeIfChanged (uint8 i) p
           cs.[i].Dopickle (&x, p)
         override this.Unpickle (p, x) =
           let i = int32 (read p : uint8)
           let p = incBy sizeof<uint8> p
           cs.[i].Unpickle (p, &x)}

type [<InferenceRules>] PU () =
  static member makePU () : PU<'x> =
    lock typeof<PU> <| fun () ->
      match Engine.TryGenerate (PU ()) with
        | None -> failwithf "PU: Unsupported type %A" typeof<'x>
        | Some pu -> pu

  static member Get () = memoize PU.makePU

  member this.toPU (O pu: OpenPU<'x>) : PU<'x> = memoize <| fun () ->
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

  member this.unit: OpenPU<unit> = memoize (mkConst >> O)

  member this.bool: OpenPU<bool> = memoize mkNative

  member this.int8: OpenPU<int8> = memoize mkNative
  member this.int16: OpenPU<int16> = memoize mkNative
  member this.int32: OpenPU<int32> = memoize mkNative
  member this.int64: OpenPU<int64> = memoize mkNative

  member this.uint8: OpenPU<uint8> = memoize mkNative
  member this.uint16: OpenPU<uint16> = memoize mkNative
  member this.uint32: OpenPU<uint32> = memoize mkNative
  member this.uint64: OpenPU<uint64> = memoize mkNative

  member this.float32: OpenPU<float32> = memoize mkNative // XXX NaN
  member this.float64: OpenPU<float  > = memoize mkNative // XXX NaN

  member this.char: OpenPU<char> = memoize mkNative
  member this.string: OpenPU<string> = memoize <| fun () ->
    O {new InternalPU<string> () with
         override this.Size (x, p) =
           alignTo sizeof<int> p
           |> incBy (sizeof<int> + sizeof<char> * x.Length)
         override this.Dopickle (x, p) =
           let mutable p =
             p
             |> alignTo sizeof<int>
             |> writeIfChanged x.Length
           for i=0 to x.Length-1 do
             p <- writeIfChanged x.[i] p
           p
         override this.Unpickle (p, x) =
           let mutable p = alignTo sizeof<int> p
           let n = read p
           p <- incBy sizeof<int> p
           let cs = Array.zeroCreate n
           for i=0 to n-1 do
             cs.[i] <- read p
             p <- incBy sizeof<char> p
           x <- String (cs)
           p}

  member this.DateTime: OpenPU<DateTime> = memoize <| fun () ->
    mkNativeBy (fun d -> d.Ticks) (fun t -> DateTime t)

  member this.Digest: OpenPU<Digest> = memoize <| fun () -> 
    O {new InternalPU<Digest> () with
         override this.Size (x, p) =
           p
           |> alignTo sizeof<uint64>
           |> incBy (2 * sizeof<uint64>)
         override this.Dopickle (x, p) =
           p
           |> alignTo sizeof<uint64>
           |> writeIfChanged x.Lo
           |> writeIfChanged x.Hi
         override this.Unpickle (p, x) =
           let mutable p = alignTo sizeof<uint64> p
           x.Lo <- read p
           p <- incBy sizeof<uint64> p
           x.Hi <- read p
           incBy sizeof<uint64> p}
           
  member this.BigInteger: OpenPU<BigInteger> = memoize <| fun () ->
    mkBytesBy (fun x -> x.ToByteArray ()) (fun x -> BigInteger (x))

  member this.bytes: OpenPU<array<byte>> = memoize <| fun () ->
    mkBytesBy id id

  member this.list (t: OpenPU<'x>) : OpenPU<list<'x>> = memoize <| fun () ->
    mkSeq List.toArray List.ofArray t

  member this.array (t: OpenPU<'a>) : OpenPU<array<'a>> = memoize <| fun () ->
    mkSeq id id t

  member this.case (m: Case<Empty, 'cs, 'u>)
                  : UnionPU<Empty, 'cs, 'u> = memoize <| fun () ->
    U [mkConst (let mutable n = Unchecked.defaultof<_> in m.Create (&n))]
  member this.case (m: Case<'ls, 'cs, 'u>,
                    p: ProductPU<'ls, 'ls, 'u>)
                  : UnionPU<'ls, 'cs, 'u> = memoize <| fun () -> 
    U [mkTupleOrNonRecursiveRecord m p]

  member this.plus (U c:  UnionPU<       'c,       Choice<'c, 'cs>, 'u>,
                    U cs: UnionPU<           'cs ,            'cs , 'u>)
                        : UnionPU<Choice<'c, 'cs>, Choice<'c, 'cs>, 'u> = memoize <| fun () -> 
    U (c @ cs)

  member this.union (_: Rep,
                     m: Union<'u>,
                     _: AsChoice<'c, 'u>,
                     u: UnionPU<'c, 'c, 'u>) : OpenPU<'u> = memoize <| fun () -> 
    mkUnion m u

  member this.times (f:  ProductPU< 'f,       And<'f, 'fs>, 'r>,
                     fs: ProductPU<     'fs ,         'fs , 'r>)
                    : ProductPU<And<'f, 'fs>, And<'f, 'fs>, 'r> = memoize <| fun () -> 
    mkProduct f fs
    
  member this.elem (_: Elem<'e, 'p, 't>,
                    t: OpenPU<'e>)
                  : ProductPU<'e, 'p, 't> = memoize <| fun () -> 
    mkElemOrField t

  member this.product (_: Rep,
                       _: Product<'t>,
                       m: AsProduct<'p, 't>,
                       p: ProductPU<'p, 'p, 't>) : OpenPU<'t> = memoize <| fun () -> 
    O (mkTupleOrNonRecursiveRecord m p)
