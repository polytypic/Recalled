namespace Recall

open Microsoft.FSharp.Core.Operators.Unchecked
open System
open System.IO
open Infers
open Infers.Rep

type RecPU<'a> () =
  inherit PU<'a> ()
  [<DefaultValue>] val mutable impl: PU<'a>
  override t.Dopickle (w, x) = t.impl.Dopickle (w, x)
  override t.Unpickle r = t.impl.Unpickle r

type [<AbstractClass>] ByRefPU<'e, 'es, 'p> () =
  abstract Dopickle: BinaryWriter * byref<'e> -> unit
  abstract Unpickle: BinaryReader * byref<'e> -> unit

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

  let inline mk (unpickle: BinaryReader -> 'a)
                (dopickle: BinaryWriter -> 'a -> unit) : PU<'a> =
    {new PU<'a> () with
      override t.Dopickle (w, x) = dopickle w x
      override t.Unpickle (r) = unpickle r}

  let inline mkConst x = mk (fun _ -> x) (fun _ _ -> ())

  let inline mkSeq ofArray (e: PU<'a>) =
    mk (fun r ->
          let n = r.ReadInt32 ()
          let xs = Array.zeroCreate n
          for i=0 to n-1 do
            xs.[i] <- e.Unpickle r
          ofArray xs)
       (fun w xs ->
          w.Write (Seq.length xs)
          xs |> Seq.iter (fun x -> e.Dopickle (w, x)))

  let inline mkElemOrField (t: PU<'e>) =
    P {new ByRefPU<'e, 'p, 't> () with
        override p.Dopickle (w, e) = t.Dopickle (w, e)
        override p.Unpickle (r, e) = e <- t.Unpickle (r)}

  let inline mkProduct (P f:  ProductPU<    'f,       And<'f, 'fs>, 'r>)
                       (P fs: ProductPU<        'fs ,         'fs , 'r>)
                            : ProductPU<And<'f, 'fs>, And<'f, 'fs>, 'r> =
    P {new ByRefPU<And<'f, 'fs>, And<'f, 'fs>, 'r> () with
        override p.Dopickle (w, ffs) =
         f.Dopickle (w, &ffs.Elem)
         fs.Dopickle (w, &ffs.Rest)
        override p.Unpickle (r, ffs) =
         f.Unpickle (r, &ffs.Elem)
         fs.Unpickle (r, &ffs.Rest)}

  let inline mkTupleOrNonRecursiveRecord (m: AsProduct<'p, 't>) (P p: ProductPU<'p, 'p, 't>) =
    mk (fun r ->
          let mutable px = defaultof<_>
          p.Unpickle (r, &px)
          m.Create (&px))
       (fun w x ->
          let mutable px = defaultof<_>
          m.Extract (x, &px)
          p.Dopickle (w, &px))

  let inline mkUnion (m: Union<'u>) (U u: UnionPU<'c, 'c, 'u>) =
    let cs = Array.ofList u
    let inline mk readTag writeTag =
      mk (fun r ->
            let i = readTag r
            cs.[i].Unpickle r)
         (fun w x ->
            let i = m.Tag x
            writeTag w i
            cs.[i].Dopickle (w, x))
    if m.Arity <= 256 then
      mk (fun r -> int (r.ReadByte ())) (fun w i -> w.Write (byte i))
    else
      mk (fun r -> r.ReadInt32 ()) (fun w i -> w.Write i)

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
    mk (fun r -> r.ReadBoolean ()) (fun w x -> w.Write x)

  member this.int8: PU<int8> = memoize <| fun () ->
    mk (fun r -> r.ReadSByte ()) (fun w x -> w.Write x)
  member this.int16: PU<int16> = memoize <| fun () ->
    mk (fun r -> r.ReadInt16 ()) (fun w x -> w.Write x)
  member this.int32: PU<int32> = memoize <| fun () ->
    mk (fun r -> r.ReadInt32 ()) (fun w x -> w.Write x)
  member this.int64: PU<int64> = memoize <| fun () ->
    mk (fun r -> r.ReadInt64 ()) (fun w x -> w.Write x)

  member this.uint8:  PU<uint8> = memoize <| fun () ->
    mk (fun r -> r.ReadByte   ()) (fun w x -> w.Write x)
  member this.uint16: PU<uint16> = memoize <| fun () ->
    mk (fun r -> r.ReadUInt16 ()) (fun w x -> w.Write x)
  member this.uint32: PU<uint32> = memoize <| fun () ->
    mk (fun r -> r.ReadUInt32 ()) (fun w x -> w.Write x)
  member this.uint64: PU<uint64> = memoize <| fun () ->
    mk (fun r -> r.ReadUInt64 ()) (fun w x -> w.Write x)

  member this.float32: PU<float32> = memoize <| fun () ->
    mk (fun r -> r.ReadSingle ()) (fun w x -> w.Write x)
  member this.float64: PU<float> = memoize <| fun () ->
    mk (fun r -> r.ReadDouble ()) (fun w x -> w.Write x)

  member this.char: PU<char> = memoize <| fun () ->
    mk (fun r -> r.ReadChar ()) (fun w x -> w.Write x)
  member this.string: PU<string> = memoize <| fun () ->
    mk (fun r -> r.ReadString ()) (fun w x -> w.Write x)

  member this.DateTime: PU<DateTime> = memoize <| fun () -> 
    mk (fun r -> DateTime (r.ReadInt64 ())) (fun w x -> w.Write x.Ticks)

  member this.Digest: PU<Digest> = memoize <| fun () -> 
    {new PU<Digest> () with
      override this.Dopickle (w: BinaryWriter, x) =
        w.Write x.Lo
        w.Write x.Hi
      override this.Unpickle (r: BinaryReader) =
        let lo = r.ReadUInt64 ()
        let hi = r.ReadUInt64 ()
        Digest (lo, hi)}

  member this.list (t: PU<'a>) : PU<list<'a>> = memoize <| fun () ->
    mkSeq List.ofArray t

  member this.array (t: PU<'a>) : PU<array<'a>> = memoize <| fun () ->
    mkSeq id t

  member this.case (m: Case<Empty, 'cs, 'u>)
                  : UnionPU<Empty, 'cs, 'u> = memoize <| fun () ->
    U [mkConst (let mutable n = defaultof<_> in m.Create (&n))]
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
