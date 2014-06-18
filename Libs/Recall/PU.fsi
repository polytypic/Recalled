namespace Recall

open System.Numerics
open System
open System.IO
open Infers
open Infers.Rep

/// Represents a capability to serialize values of type `'t` and is open to be
/// combined and extended.
type OpenPU<'t>

/// Represents a capability to serialize a part of a product type.
type ProductPU<'e, 'es, 't>

/// Represents a capability to serialize a subset of a union type.
type UnionPU<'c, 'cs, 't>

/// Provides inference rules for a datatype generic serialization capability.
#if DOC
///
/// The methods of this class aside from `Get` are ordinarily not called
/// directly by client code, but are rather called indirectly by `Get` to
/// construct the desired serialization capability.
///
/// Please note that the method signatures can be seen as a specification of
/// which types are supported by the inference rules.
#endif
type [<InferenceRules>] PU =
  /// Returns a previously generated serialization capability or attempts to
  /// generate one for the specified type `'t`.
  static member Get: unit -> PU<'t>

  // ---------------------------------------------------------------------------

  /// Empty default constructor.
  new: unit -> PU

  // ---------------------------------------------------------------------------

  /// Completes the serialization capability for a serializable type.
  member toPU: OpenPU<'t> -> PU<'t>

  // Rec -----------------------------------------------------------------------

  /// Capability to compute serialization capabilities for recursive types.
  member fix: unit -> Rec<OpenPU<'t>>

  // Base Types ----------------------------------------------------------------

  /// Serialization capability for the `unit` type.
  member unit: OpenPU<unit>

  /// Serialization capability for the `bool` type.
  member bool: OpenPU<bool>

  /// Serialization capability for the `int8` type.
  member int8: OpenPU<int8>
  /// Serialization capability for the `int16` type.
  member int16: OpenPU<int16>
  /// Serialization capability for the `int32` type.
  member int32: OpenPU<int32>
  /// Serialization capability for the `int64` type.
  member int64: OpenPU<int64>

  /// Serialization capability for the `uint8` type.
  member uint8: OpenPU<uint8>
  /// Serialization capability for the `uint16` type.
  member uint16: OpenPU<uint16>
  /// Serialization capability for the `uint32` type.
  member uint32: OpenPU<uint32>
  /// Serialization capability for the `uint64` type.
  member uint64: OpenPU<uint64>

  /// Serialization capability for the `float32` type.
  member float32: OpenPU<float32>
  /// Serialization capability for the `float` type.
  member float64: OpenPU<float>

  /// Serialization capability for the `char` type.
  member char: OpenPU<char>
  /// Serialization capability for the `string` type.
  member string: OpenPU<string>

  /// Serialization capability for the `DateTime` type.
  member DateTime: OpenPU<DateTime>

  /// Serialization capability for the `Digest` type.
  member Digest: OpenPU<Digest>

  /// Serialization capability for the `BigInteger` type.
  member BigInteger: OpenPU<BigInteger>

  /// Serialization capability for the `array<byte>` type.  Unlike the general
  /// `array` rule, this version is optimized for arrays of bytes.
  member bytes: OpenPU<array<byte>>

  // Special optimizations -----------------------------------------------------

  /// Serialization capability for lists of serializable values.  Unlike the
  /// general rules for arbitrary union types, this version treats the list as a
  /// sequence of elements and is typically more efficient.
  member list: OpenPU<'t> -> OpenPU<list<'t>>

  // Refs and Arrays -----------------------------------------------------------

  /// Serialization capabability for arrays of serializable values.
  member array: OpenPU<'t> -> OpenPU<array<'t>>

  // Discriminated Unions ------------------------------------------------------

  /// Serialization capability for an empty case of a union type.
  member case: Case<Empty, 'cs, 't>
         -> UnionPU<Empty, 'cs, 't>

  /// Serialization capability for a non-empty case of a union type.
  member case: Case<'ls,      'cs, 't>
        * ProductPU<'ls, 'ls,      't>
         -> UnionPU<'ls,      'cs, 't>

  /// Serialization capability for multiple cases of a union type.
  member plus: UnionPU<       'c      , Choice<'c, 'cs>, 't>
             * UnionPU<           'cs ,            'cs , 't>
            -> UnionPU<Choice<'c, 'cs>, Choice<'c, 'cs>, 't>

  /// Serialization capability for an arbitrary union type.
  member union: Rep
            * Union<          't>
         * AsChoice<'cs,      't>
          * UnionPU<'cs, 'cs, 't>
          -> OpenPU<          't>

  // Tuples and Records --------------------------------------------------------

  /// Serialization capability for an element of a product type.
  member elem: Elem<'e, 'es, 't>
           * OpenPU<'e         >
       -> ProductPU<'e, 'es, 't>

  /// Serialization capability for multiple elements of a product type.
  member times: ProductPU<    'e      , And<'e, 'es>, 't>
              * ProductPU<        'es ,         'es , 't>
             -> ProductPU<And<'e, 'es>, And<'e, 'es>, 't>

  /// Serialization capability for an arbitrary product type.
  member product: Rep
            * Product<          't>
          * AsProduct<'es,      't>
          * ProductPU<'es, 'es, 't>
            -> OpenPU<          't>
