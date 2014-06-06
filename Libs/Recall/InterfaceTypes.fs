namespace Recall

open System.IO

type Digest = struct
    val Lo: uint64
    val Hi: uint64
    new (lo, hi) = {Lo = lo; Hi = hi}
  end

type [<AbstractClass>] PU<'x> () =
  abstract Dopickle: BinaryWriter * 'x -> unit
  abstract Unpickle: BinaryReader -> 'x
