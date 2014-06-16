namespace Recall

open Microsoft.FSharp.NativeInterop
open System
open System.IO
open System.Security.Cryptography

type [<NoComparison; CustomEquality>] Digest = struct
    val mutable Lo: uint64
    val mutable Hi: uint64

    new (lo, hi) = {Lo = lo; Hi = hi}

    new (bytes: array<byte>) =
      assert (16 = bytes.Length)
      {Lo = BitConverter.ToUInt64 (bytes, 0)
       Hi = BitConverter.ToUInt64 (bytes, 8)}

    static member Zero = Digest (0UL, 0UL)

    static member (^^^) (l: Digest, r: Digest) =
      Digest (l.Lo ^^^ r.Lo, l.Hi ^^^ r.Hi)

    static member Bytes (ptr: nativeptr<byte>, len: int) : Digest =
      let mutable lo = 0UL
      let mutable hi = 0UL
      MurmurHash3.bytes ptr len 0u (&lo) (&hi)
      Digest (lo, hi)

    static member String (string: string) : Digest =
      let mutable lo = 0UL
      let mutable hi = 0UL
      MurmurHash3.string string 0u (&lo) (&hi)
      Digest (lo, hi)

    override this.ToString () =
      sprintf "%016x%016x" this.Hi this.Lo
    override this.GetHashCode () = int this.Lo
    override this.Equals (other) =
     match other with
      | :? Digest as other ->
        0uL = (this.Lo ^^^ other.Lo ||| this.Hi ^^^ other.Hi)
      | _ ->
        false
  end

type [<Class>] DigestEqualityComparer () =
 interface System.Collections.Generic.IEqualityComparer<Digest> with
  override this.GetHashCode (dig) = int dig.Lo
  override this.Equals (lhs, rhs) =
    0uL = (lhs.Lo ^^^ rhs.Lo ||| lhs.Hi ^^^ rhs.Hi)

type [<AbstractClass>] PU<'x> () =
  abstract Size: 'x -> int
  abstract Dopickle: 'x * nativeptr<byte> -> unit
  abstract Unpickle: nativeptr<byte> -> 'x
