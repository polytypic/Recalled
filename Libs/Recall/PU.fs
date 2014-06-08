namespace Recall

open System.Numerics
open System
open System.IO
open Infers
open Infers.Rep

type RecPU<'a> () =
  inherit PU<'a> ()
  [<DefaultValue>] val mutable impl: PU<'a>
  override t.Dopickle (w, x) = t.impl.Dopickle (w, x)
  override t.Unpickle (r) = t.impl.Unpickle r
  override t.Unpickle (b, p, x) = t.impl.Unpickle (b, &p, &x)

type [<AbstractClass>] ByRefPU<'e, 'es, 'p> () =
  abstract Dopickle: BinaryWriter * byref<'e> -> unit
  abstract Unpickle: BinaryReader * byref<'e> -> unit
  abstract Unpickle: array<byte> * byref<int> * byref<'e> -> unit

type ProductPU<'e, 'es, 'p> = P of ByRefPU<'e, 'es, 'p>
type UnionPU<'c, 'cs, 'u> = U of list<PU<'u>>

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

  let inline mk (size: int)
                (read: array<byte> -> int -> 'x)
                (unpickle: BinaryReader -> 'x)
                (dopickle: BinaryWriter -> 'x -> unit) : PU<'x> =
    {new PU<'x> () with
       override this.Dopickle (w, x) = dopickle w x
       override this.Unpickle (r) = unpickle r
       override this.Unpickle (b, p, x) =
         x <- read b p
         p <- p + size}

  let inline mkConst x = mk 0 (fun _ _ -> x) (fun _ -> x) (fun _ _ -> ())

  let inline mkSeq (toArray: 'xs -> array<'x>) (ofArray: array<'x> -> 'xs) (e: PU<'x>) =
    {new PU<'xs> () with
       override this.Dopickle (w, xs) =
         let xs = toArray xs
         w.Write xs.Length
         xs |> Array.iter (fun x -> e.Dopickle (w, x))
       override this.Unpickle (r) =
         let n = r.ReadInt32 ()
         let xs = Array.zeroCreate n
         for i=0 to n-1 do
           xs.[i] <- e.Unpickle r
         ofArray xs
       override this.Unpickle (b, p, x) =
         let n = BitConverter.ToInt32 (b, p)
         p <- p + 4
         let xs = Array.zeroCreate n
         for i=0 to n-1 do
           e.Unpickle (b, &p, &xs.[i])
         x <- ofArray xs}

  let inline mkElemOrField (t: PU<'e>) =
    P {new ByRefPU<'e, 'p, 't> () with
         override this.Dopickle (w, e) = t.Dopickle (w, e)
         override this.Unpickle (r, e) = e <- t.Unpickle (r)
         override this.Unpickle (b, p, e) = t.Unpickle (b, &p, &e)}

  let inline mkProduct (P f:  ProductPU<    'f,       And<'f, 'fs>, 'r>)
                       (P fs: ProductPU<        'fs ,         'fs , 'r>)
                            : ProductPU<And<'f, 'fs>, And<'f, 'fs>, 'r> =
    P {new ByRefPU<And<'f, 'fs>, And<'f, 'fs>, 'r> () with
         override this.Dopickle (w, ffs) =
           f.Dopickle (w, &ffs.Elem)
           fs.Dopickle (w, &ffs.Rest)
         override this.Unpickle (r, ffs) =
           f.Unpickle (r, &ffs.Elem)
           fs.Unpickle (r, &ffs.Rest)
         override this.Unpickle (b, p, ffs) =
           f.Unpickle (b, &p, &ffs.Elem)
           fs.Unpickle (b, &p, &ffs.Rest)}

  let inline mkTupleOrNonRecursiveRecord (asProduct: AsProduct<'p, 't>)
                                         (P ppu: ProductPU<'p, 'p, 't>) =
    {new PU<'t> () with
       override this.Dopickle (w, x) =
         let mutable px = Unchecked.defaultof<_>
         asProduct.Extract (x, &px)
         ppu.Dopickle (w, &px)
       override this.Unpickle (r) =
         let mutable px = Unchecked.defaultof<_>
         ppu.Unpickle (r, &px)
         asProduct.Create (&px)
       override this.Unpickle (b, p, x) =
         let mutable px = Unchecked.defaultof<_>
         ppu.Unpickle (b, &p, &px)
         x <- asProduct.Create (&px)}

  let inline mkUnion (m: Union<'u>) (U u: UnionPU<'c, 'c, 'u>) =
    let cs = Array.ofList u
    if 256 < cs.Length then
      failwith "Unions with more than 256 cases are not yet supported"
    {new PU<'u> () with
       override this.Dopickle (w, x) =
         let i = m.Tag x
         w.Write (uint8 i)
         cs.[i].Dopickle (w, x)
       override this.Unpickle (r) =
         let i = int32 (r.ReadByte ())
         cs.[i].Unpickle r
       override this.Unpickle (b, p, x) =
         let i = int32 b.[p]
         p <- p + 1
         cs.[i].Unpickle (b, &p, &x)}

type [<InferenceRules>] PU () =
  static member makePU () : PU<'x> =
    lock typeof<PU> <| fun () ->
      match Engine.TryGenerate (PU ()) with
        | None -> failwithf "PU: Unsupported type %A" typeof<'x>
        | Some pu -> pu

  static member Get () = memoize PU.makePU

  member this.unit: PU<unit> = memoize <| fun () -> mkConst ()

  member this.fix () : Rec<PU<'x>> =
    let r = RecPU<'x>()
    {new Rec<PU<'x>> () with
       override p.Get () = r :> PU<'x>
       override p.Set (t) = r.impl <- t}

  member this.bool: PU<bool> = memoize <| fun () ->
    mk 1 (fun b p -> BitConverter.ToBoolean (b, p)) (fun r -> r.ReadBoolean ()) (fun w x -> w.Write x)

  member this.int8: PU<int8> = memoize <| fun () ->
    mk 1 (fun b p -> int8 b.[p]) (fun r -> r.ReadSByte ()) (fun w x -> w.Write x)
  member this.int16: PU<int16> = memoize <| fun () ->
    mk 2 (fun b p -> BitConverter.ToInt16 (b, p)) (fun r -> r.ReadInt16 ()) (fun w x -> w.Write x)
  member this.int32: PU<int32> = memoize <| fun () ->
    mk 4 (fun b p -> BitConverter.ToInt32 (b, p)) (fun r -> r.ReadInt32 ()) (fun w x -> w.Write x)
  member this.int64: PU<int64> = memoize <| fun () ->
    mk 8 (fun b p -> BitConverter.ToInt64 (b, p)) (fun r -> r.ReadInt64 ()) (fun w x -> w.Write x)

  member this.uint8:  PU<uint8> = memoize <| fun () ->
    mk 1 (fun b p -> b.[p]) (fun r -> r.ReadByte   ()) (fun w x -> w.Write x)
  member this.uint16: PU<uint16> = memoize <| fun () ->
    mk 2 (fun b p -> uint16 (BitConverter.ToInt16 (b, p))) (fun r -> r.ReadUInt16 ()) (fun w x -> w.Write x)
  member this.uint32: PU<uint32> = memoize <| fun () ->
    mk 4 (fun b p -> uint32 (BitConverter.ToInt32 (b, p))) (fun r -> r.ReadUInt32 ()) (fun w x -> w.Write x)
  member this.uint64: PU<uint64> = memoize <| fun () ->
    mk 8 (fun b p -> uint64 (BitConverter.ToInt64 (b, p))) (fun r -> r.ReadUInt64 ()) (fun w x -> w.Write x)

  member this.float32: PU<float32> = memoize <| fun () ->
    mk 4 (fun b p -> BitConverter.ToSingle (b, p)) (fun r -> r.ReadSingle ()) (fun w x -> w.Write x)
  member this.float64: PU<float> = memoize <| fun () ->
    mk 8 (fun b p -> BitConverter.ToDouble (b, p)) (fun r -> r.ReadDouble ()) (fun w x -> w.Write x)

  member this.char: PU<char> = memoize <| fun () ->
    mk 2 (fun b p -> BitConverter.ToChar (b, p)) (fun r -> r.ReadChar ()) (fun w x -> w.Write x)
  member this.string: PU<string> = memoize <| fun () ->
    mk 0 (fun b p -> failwith "XXX") (fun r -> r.ReadString ()) (fun w x -> w.Write x)

  member this.DateTime: PU<DateTime> = memoize <| fun () -> 
    mk 8 (fun b p -> DateTime (BitConverter.ToInt64 (b, p))) (fun r -> DateTime (r.ReadInt64 ())) (fun w x -> w.Write x.Ticks)

  member this.Digest: PU<Digest> = memoize <| fun () -> 
    {new PU<Digest> () with
       override this.Dopickle (w: BinaryWriter, x) =
         w.Write x.Lo
         w.Write x.Hi
       override this.Unpickle (r: BinaryReader) =
         let lo = r.ReadUInt64 ()
         let hi = r.ReadUInt64 ()
         Digest (lo, hi)
       override this.Unpickle (b, p, x) =
         let i = p
         x.Lo <- uint64 (BitConverter.ToInt64 (b, i))
         x.Hi <- uint64 (BitConverter.ToInt64 (b, i+8))
         p <- i + 16}

  member this.BigInteger: PU<BigInteger> = memoize <| fun () ->
    {new PU<BigInteger> () with
       override this.Dopickle (w: BinaryWriter, x) =
         let bytes = x.ToByteArray ()
         w.Write bytes.Length
         w.Write bytes
       override this.Unpickle (r: BinaryReader) =
         let n = r.ReadInt32 ()
         BigInteger (r.ReadBytes n)
       override this.Unpickle (b, p, x) =
         let n = BitConverter.ToInt32 (b, p)
         let bs = Array.sub b (p+4) n
         p <- p + 4 + n
         x <- BigInteger (bs)}

  member this.list (t: PU<'a>) : PU<list<'a>> = memoize <| fun () ->
    mkSeq List.toArray List.ofArray t

  member this.array (t: PU<'a>) : PU<array<'a>> = memoize <| fun () ->
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
                     u: UnionPU<'c, 'c, 'u>) : PU<'u> = memoize <| fun () -> 
    mkUnion m u

  member this.times (f:  ProductPU< 'f,       And<'f, 'fs>, 'r>,
                     fs: ProductPU<     'fs ,         'fs , 'r>)
                    : ProductPU<And<'f, 'fs>, And<'f, 'fs>, 'r> = memoize <| fun () -> 
    mkProduct f fs
    
  member this.elem (_: Elem<'e, 'p, 't>,
                    t: PU<'e>)
                : ProductPU<'e, 'p, 't> = memoize <| fun () -> 
    mkElemOrField t

  member this.product (_: Rep,
                       _: Product<'t>,
                       m: AsProduct<'p, 't>,
                       p: ProductPU<'p, 'p, 't>) : PU<'t> = memoize <| fun () -> 
    mkTupleOrNonRecursiveRecord m p
