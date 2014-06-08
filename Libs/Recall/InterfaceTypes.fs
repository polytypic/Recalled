namespace Recall

open System
open System.IO
open System.Security.Cryptography

module Digester =
  let md5 = new System.Threading.ThreadLocal<MD5> (fun () -> MD5.Create ())

type Digest = struct
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
      let md5 = Digester.md5.Value
      Digest (md5.ComputeHash bytes)

    static member String (string: string) : Digest =
      Digest.Bytes (System.Text.Encoding.UTF8.GetBytes string)
      
    static member Stream (stream: Stream) : Digest =
      let md5 = Digester.md5.Value
      Digest (md5.ComputeHash stream)
  end

type [<AbstractClass>] PU<'x> () =
  abstract Dopickle: BinaryWriter * 'x -> unit
  abstract Unpickle: BinaryReader -> 'x
  abstract Unpickle: array<byte> * byref<int> * byref<'x> -> unit
