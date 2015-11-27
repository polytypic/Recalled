namespace Recalled

open System.Numerics
open System
open System.IO
open Infers
open Infers.Rep

/// Represents a capability to serialize values of type `'x`.
#if DOC
///
/// This class has a very low level interface and is designed to make it
/// possible to implement nearly optimal serialization to and deserialization
/// from memory mapped files.  This is important, because Recalled serializes
/// all the results of logged computations and potentially deserializes large
/// numbers of those results.
///
/// In most cases client code should not need to implement this class directly
/// as the `PU` class provides inference rules to generate instances of this
/// class for a wide variety of F# types.
#endif
type [<AbstractClass>] PU<'x> =
  /// Empty default constructor.
  new: unit -> PU<'x>

  /// Compute the serialized size of the given value.
  abstract Size: 'x -> int

  /// Conditionally serialize given value to memory starting at the specified
  /// address.
#if DOC
  ///
  /// The caller is responsible for ensuring that the pointer points to a region
  /// of memory that has at least the number of bytes of space as returned by
  /// `Size`.  The `DoPickle` implementation must not write to memory outside of
  /// that range.
  ///
  /// Memory at the specified address should be first read and only written to
  /// if it differs from the value being written.  This allows the space for a
  /// previous value of the same type and size to be reused and, at the same
  /// time, make sure that persistent storage is only written to when the stored
  /// value is different.
#endif
  abstract Dopickle: 'x * nativeptr<byte> -> unit

  /// Deserialize value from memory starting at the specified address.
  abstract Unpickle: nativeptr<byte> -> 'x

module PU =

  /// Generate PU for given type.
  val pu<'x> : PU<'x>

  /// Represents a capability to serialize values of type `'t` and is open to be
  /// combined and extended.
  type OpenPU<'t>

  /// Represents a capability to serialize a part of a product type.
  type ProductPU<'e, 'r, 'o, 't>

  /// Represents a capability to serialize a subset of a union type.
  type UnionPU<'p, 'o, 't>

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
  type PU =
    inherit Rules
    new: unit -> PU

    // -------------------------------------------------------------------------

    static member toPU: OpenPU<'t> -> PU<'t>

    // Rec ---------------------------------------------------------------------

    static member fix: unit -> Rec<OpenPU<'t>>

    // Base Types --------------------------------------------------------------

    static member unit: OpenPU<unit>

    static member bool: OpenPU<bool>

    static member int8: OpenPU<int8>
    static member int16: OpenPU<int16>
    static member int32: OpenPU<int32>
    static member int64: OpenPU<int64>

    static member uint8: OpenPU<uint8>
    static member uint16: OpenPU<uint16>
    static member uint32: OpenPU<uint32>
    static member uint64: OpenPU<uint64>

    static member float32: OpenPU<float32>
    static member float64: OpenPU<float>

    static member char: OpenPU<char>
    static member string: OpenPU<string>

    static member DateTime: OpenPU<DateTime>

    static member Digest: OpenPU<Internal.Digest>

    static member BigInteger: OpenPU<BigInteger>

    static member bytes: OpenPU<array<byte>>

    // Special optimizations ---------------------------------------------------

    static member list: OpenPU<'t> -> OpenPU<list<'t>>

    // Refs and Arrays ---------------------------------------------------------

    static member array: OpenPU<'t> -> OpenPU<array<'t>>

    // Discriminated Unions ----------------------------------------------------

    static member case: Case<Empty, 'o, 't>
                  -> UnionPU<Empty, 'o, 't>

    static member case: Case<'p,      'o, 't>
                 * ProductPU<'p, 'p,  'o,  't>
                  -> UnionPU<'p,      'o, 't>

    static member choice: UnionPU<       'p     , Choice<'p, 'o>, 't>
                        * UnionPU<           'o ,            'o , 't>
                       -> UnionPU<Choice<'p, 'o>, Choice<'p, 'o>, 't>

    static member sum: AsChoices<'s,     't>
                       * UnionPU<'s, 's, 't>
                       -> OpenPU<        't>

    // Tuples and Records ------------------------------------------------------

    static member elem: Elem<'e, 'r, 'o, 't>
                    * OpenPU<'e            >
                -> ProductPU<'e, 'r, 'o, 't>

    static member pair: ProductPU<     'e     , Pair<'e, 'r>, 'o, 't>
                      * ProductPU<         'r ,          'r , 'o, 't>
                     -> ProductPU<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 't>

    static member product: AsPairs<'p,     'o, 't>
                       * ProductPU<'p, 'p, 'o, 't>
                         -> OpenPU<            't>
