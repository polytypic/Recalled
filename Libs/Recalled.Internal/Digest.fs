namespace Recalled.Internal

open Microsoft.FSharp.NativeInterop
open System
open System.IO
open System.Security.Cryptography

type [<NoComparison; CustomEquality>] Digest = struct
    val mutable Lo: uint64
    val mutable Hi: uint64

    static member inline ZeroIfEq (lhs: byref<Digest>, rhs: byref<Digest>) =
      (lhs.Lo ^^^ rhs.Lo ||| lhs.Hi ^^^ rhs.Hi)

    static member inline Combine (l': byref<Digest>, r: byref<Digest>) =
      let llo = l'.Lo
      let lhi = l'.Hi
      let llo = (llo + llo) ^^^ (lhi >>> 63)
      let lhi = (lhi + lhi) ^^^ (llo >>> 63)
      l'.Lo <- llo ^^^ r.Lo
      l'.Hi <- lhi ^^^ r.Hi

    static member inline Bytes (ptr: nativeptr<byte>, len: int, res: byref<Digest>) =
      MurmurHash3.bytes ptr len 0u &res.Lo &res.Hi

    static member inline String (string: string, res: byref<Digest>) =
      MurmurHash3.string string 0u &res.Lo &res.Hi

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
