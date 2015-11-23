namespace Recalled.Internal

open Microsoft.FSharp.NativeInterop
open System
open System.IO
open System.IO.MemoryMappedFiles
open System.Threading
open Hopac
open Hopac.Infixes

module Cond =
  let inline wait (var: byref<_>) cond =
    if cond () then
      Job.unit ()
    else
      var <- IVar ()
      if cond () then Job.unit () else var :> Job<_>

  let inline signal var cond =
    if cond then IVar.tryFill var () else Job.unit ()

module MemMapBuf =
  type Access =
    abstract Do: IVar<'x> * (nativeptr<byte> -> #Job<'x>) -> Job<unit>

  type Req =
   | Access of (Access -> Job<unit>)
   | Append of align: int * size: int * IVar<PtrInt>
   | Close of IVar<unit>
   | Flush of IVar<unit>

  type MemMapBuf = {
      path: string
      mutable size: PtrInt
      mutable capacity: PtrInt
      mutable file: MemoryMappedFile
      mutable view: MemoryMappedViewAccessor
      mutable ptr: nativeptr<byte>
      reqs: Ch<Req>
      mutable numAccessors: int
      mutable isFree: IVar<unit>
    }

  let size (buf: MemMapBuf) =
    buf.size

  module Unsafe =
    let truncate (buf: MemMapBuf) (size: PtrInt) =
      if size < 0L then
        failwith "MemMapBuf.Unsafe.truncate: Negative size"
      elif buf.size < size then
        failwithf "MemMapBuf.Unsafe.truncate: Cannot increase buffer size from %d to %d" buf.size size
      else
        buf.size <- size

  let getFileLengthOr0 path =
    if File.Exists path then
      let info = FileInfo (path)
      info.Length
    else
      0L

  let setFileLength path newLength =
    let newLength = int64 newLength
    use s = new FileStream (path, FileMode.OpenOrCreate)
    s.SetLength newLength

  let doClose buf =
    buf.view.SafeMemoryMappedViewHandle.ReleasePointer ()
    buf.ptr <- Unchecked.defaultof<_>

    buf.view.Flush ()
    buf.view.Dispose ()
    buf.view <- null

    buf.file.Dispose ()
    buf.file <- null

    setFileLength buf.path buf.size

  let doOpenWithCapacity buf newCapacity =
    setFileLength buf.path newCapacity
    buf.file <-
      MemoryMappedFile.CreateFromFile (
        buf.path,
        FileMode.OpenOrCreate,
        null,
        int64 newCapacity)
    buf.capacity <- newCapacity
    buf.view <- buf.file.CreateViewAccessor ()
    buf.ptr <- Unchecked.defaultof<_>
    buf.view.SafeMemoryMappedViewHandle.AcquirePointer (&buf.ptr)

  let create (path: string) = Job.delay <| fun () ->
    let size = getFileLengthOr0 path
    let buf = {
      path = path
      size = size
      capacity = size
      file = null
      view = null
      ptr = Unchecked.defaultof<_>
      numAccessors = 0
      isFree = IVar ()
      reqs = Ch ()
    }
    doOpenWithCapacity buf (if size = 0L then 65536L else size)
    buf.reqs >>= function
      | Flush reply ->
        Cond.wait (&buf.isFree) (fun () -> 0 = buf.numAccessors) >>= fun () ->
        buf.view.Flush ()
        reply *<= ()
      | Close reply ->
        Cond.wait (&buf.isFree) (fun () -> 0 = buf.numAccessors) >>= fun () ->
        doClose buf
        reply *<= ()
      | Access access ->
        Interlocked.Increment &buf.numAccessors |> ignore
        Job.queue << access <| {new Access with
          member t.Do (reply, access) =
            Job.tryInDelay (fun () -> access buf.ptr)
              (fun x -> reply *<= x)
              (fun e -> reply *<=! e) >>= fun () ->
            Cond.signal buf.isFree (0 = Interlocked.Decrement &buf.numAccessors)}
      | Append (align, size, reply) ->
        let offs = buf.size |> skipTo align
        let size = int64 size
        Job.whenDo (buf.capacity - offs < size)
          (Cond.wait (&buf.isFree) (fun () -> 0 = buf.numAccessors) >>- fun () ->
           doClose buf
           doOpenWithCapacity
             buf (max (offs + size) (buf.capacity + buf.capacity))) >>= fun () ->
        buf.size <- offs + size
        reply *<= offs
    |> Job.foreverServer >>-. buf

  let close buf = buf.reqs *<-=>- Close

  let flush buf = buf.reqs *<-=>- Flush

  let accessJob buf access =
    buf.reqs *<-=>- fun reply -> Access <| fun s -> s.Do (reply, access)

  let accessFun buf access = access >> Job.result |> accessJob buf

  let append buf align size =
    buf.reqs *<-=>- fun reply -> Append (align, size, reply)
