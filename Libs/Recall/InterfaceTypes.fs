namespace Recall

open System
open System.IO
open System.Security.Cryptography

module Digester =
  type State = {
      MD5: MD5
      mutable Buffer: array<byte>
    }

  let md5 =
    new System.Threading.ThreadLocal<State> (fun () ->
      {MD5 = MD5.Create ()
       Buffer = Array.zeroCreate 65536})

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

    static member Bytes (bytes: array<byte>) : Digest =
      let md5 = Digester.md5.Value.MD5
      Digest (md5.ComputeHash bytes)

    static member String (string: string) : Digest =
      let state = Digester.md5.Value
      let n = System.Text.Encoding.UTF8.GetBytes (string, 0, string.Length, state.Buffer, 0)
      Digest (state.MD5.ComputeHash (state.Buffer, 0, n))
      
    static member Stream (stream: Stream) : Digest =
      let md5 = Digester.md5.Value.MD5
      Digest (md5.ComputeHash stream)

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
