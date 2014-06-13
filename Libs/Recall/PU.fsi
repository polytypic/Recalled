namespace Recall

open System.Numerics
open System
open System.IO
open Infers
open Infers.Rep

type OpenPU<'x>
type ProductPU<'e, 'es, 'p>
type UnionPU<'c, 'cs, 'u>

/// Provides inference rules for a datatype generic serialization capability.
type [<InferenceRules>] PU =
  /// Returns a previously generated serialization capability or attempts to
  /// generate one for a given type.
  static member Get: unit -> PU<'x>

  // ---------------------------------------------------------------------------

  new: unit -> PU

  // ---------------------------------------------------------------------------

  member toPU: OpenPU<'x> -> PU<'x>

  // Rec -----------------------------------------------------------------------

  member fix: unit -> Rec<OpenPU<'x>>

  // Base Types ----------------------------------------------------------------

  member unit: OpenPU<unit>

  member bool: OpenPU<bool>

  member int8:  OpenPU<int8>
  member int16: OpenPU<int16>
  member int32: OpenPU<int32>
  member int64: OpenPU<int64>

  member uint8:  OpenPU<uint8>
  member uint16: OpenPU<uint16>
  member uint32: OpenPU<uint32>
  member uint64: OpenPU<uint64>

  member float32: OpenPU<float32>
  member float64: OpenPU<float>

  member char: OpenPU<char>
  member string: OpenPU<string>

  member DateTime: OpenPU<DateTime>

  member Digest: OpenPU<Digest>

  member BigInteger: OpenPU<BigInteger>

  member bytes: OpenPU<array<byte>>

  // Special optimizations -----------------------------------------------------

  member list: OpenPU<'a> -> OpenPU<list<'a>>

  // Refs and Arrays -----------------------------------------------------------

  member array: OpenPU<'a> -> OpenPU<array<'a>>

  // Discriminated Unions ------------------------------------------------------

  member case: Case<Empty, 'cs, 'u>                           -> UnionPU<Empty, 'cs, 'u>
  member case: Case<  'ls, 'cs, 'u> * ProductPU<'ls, 'ls, 'u> -> UnionPU<  'ls, 'cs, 'u>

  member plus: UnionPU<       'c      , Choice<'c, 'cs>, 'u>
             * UnionPU<           'cs ,            'cs , 'u>
            -> UnionPU<Choice<'c, 'cs>, Choice<'c, 'cs>, 'u>

  member union: Rep * Union<'u> * AsChoice<'c, 'u> * UnionPU<'c, 'c, 'u> -> OpenPU<'u>

  // Tuples and Records --------------------------------------------------------

  member elem: Elem<'e, 'p, 't> * OpenPU<'e> -> ProductPU<'e, 'p, 't>

  member times: ProductPU<    'e      , And<'e, 'es>, 't>
              * ProductPU<        'es ,         'es , 't>
             -> ProductPU<And<'e, 'es>, And<'e, 'es>, 't>

  member product: Rep * Product<'t> * AsProduct<'p, 't> * ProductPU<'p, 'p, 't> -> OpenPU<'t>
