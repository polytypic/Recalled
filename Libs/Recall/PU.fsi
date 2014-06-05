namespace Recall

module PU =
  open System
  open System.IO
  open Infers
  open Infers.Rep

  val dopickle: BinaryWriter -> 'a -> unit
  val unpickle: BinaryReader -> 'a

  type [<AbstractClass>] PU<'a> =
    abstract Dopickle: BinaryWriter * 'a -> unit
    abstract Unpickle: BinaryReader -> 'a

  type [<AbstractClass>] ProductPU<'e, 'es, 'p> =
    abstract Dopickle: BinaryWriter * byref<'e> -> unit
    abstract Unpickle: BinaryReader * byref<'e> -> unit

  type UnionPU<'c, 'cs, 'u>

  type [<InferenceRules>] PU =
    new: unit -> PU

    // Rec ---------------------------------------------------------------------

    member fix: unit -> Rec<PU<'x>>

    // Base Types --------------------------------------------------------------

    member unit: PU<unit>

    member bool: PU<bool>

    member int8:  PU<int8>
    member int16: PU<int16>
    member int32: PU<int32>
    member int64: PU<int64>

    member uint8:  PU<uint8>
    member uint16: PU<uint16>
    member uint32: PU<uint32>
    member uint64: PU<uint64>

    member float32: PU<float32>
    member float64: PU<float>

    member char: PU<char>
    member string: PU<string>

    member DateTime: PU<DateTime>

    // Special optimizations ---------------------------------------------------

    member list: PU<'a> -> PU<list<'a>>

    // Refs and Arrays ---------------------------------------------------------

    member array: PU<'a> -> PU<array<'a>>

    // Discriminated Unions ----------------------------------------------------

    member case: Case<Empty, 'cs, 'u>                           -> UnionPU<Empty, 'cs, 'u>
    member case: Case<  'ls, 'cs, 'u> * ProductPU<'ls, 'ls, 'u> -> UnionPU<  'ls, 'cs, 'u>

    member plus: UnionPU<       'c      , Choice<'c, 'cs>, 'u>
               * UnionPU<           'cs ,            'cs , 'u>
              -> UnionPU<Choice<'c, 'cs>, Choice<'c, 'cs>, 'u>

    member union: Rep * Union<'u> * AsChoice<'c, 'u> * UnionPU<'c, 'c, 'u> -> PU<'u>

    // Tuples and Records ------------------------------------------------------

    member elem: Elem<'e, 'p, 't> * PU<'e> -> ProductPU<'e, 'p, 't>

    member times: ProductPU<    'e      , And<'e, 'es>, 't>
                * ProductPU<        'es ,         'es , 't>
               -> ProductPU<And<'e, 'es>, And<'e, 'es>, 't>

    member product: Rep * Product<'t> * AsProduct<'p, 't> * ProductPU<'p, 'p, 't> -> PU<'t>
