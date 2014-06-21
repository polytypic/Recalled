namespace Recalled.Internal

open Microsoft.FSharp.NativeInterop
open System
open System.IO
open System.IO.MemoryMappedFiles
open System.Threading
open Hopac
open Hopac.Job.Infixes
open Hopac.Alt.Infixes
open Hopac.Infixes

module Cond =
  let inline wait (var: byref<_>) cond =
    if cond () then
      Job.unit ()
    else
      var <- ivar ()
      if cond () then Job.unit () else var :> Job<_>

  let inline signal var cond =
    if cond then IVar.tryFill var () else Job.unit ()

module MemMapBuf =
  type Req =
   | Access of (nativeptr<byte> -> Job<unit>)
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
      reqs: Mailbox<Req>
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

  let doClose buf =
    buf.view.SafeMemoryMappedViewHandle.ReleasePointer ()
    buf.ptr <- Unchecked.defaultof<_>

    buf.view.Flush ()
    buf.view.Dispose ()
    buf.view <- null

    buf.file.Dispose ()
    buf.file <- null

    use f = new FileStream (buf.path, FileMode.Open)
    f.SetLength (int64 buf.size)

  let doOpenWithCapacity buf newCapacity =
    buf.file <- MemoryMappedFile.CreateFromFile (buf.path, FileMode.OpenOrCreate, null, int64 newCapacity)
    buf.capacity <- newCapacity
    buf.view <- buf.file.CreateViewAccessor ()
    buf.ptr <- Unchecked.defaultof<_>
    buf.view.SafeMemoryMappedViewHandle.AcquirePointer (&buf.ptr)

  let server buf =
    buf.reqs >>= function
     | Flush reply ->
       Cond.wait (&buf.isFree) (fun () -> 0 = buf.numAccessors) >>= fun () ->
       buf.view.Flush ()
       reply <-= ()
     | Close reply ->
       Cond.wait (&buf.isFree) (fun () -> 0 = buf.numAccessors) >>= fun () ->
       doClose buf
       reply <-= ()
     | Access access ->
       Interlocked.Increment &buf.numAccessors |> ignore
       Job.queue (access buf.ptr >>= fun () ->
         Cond.signal buf.isFree (0 = Interlocked.Decrement &buf.numAccessors))
     | Append (align, size, reply) ->
       let offs = buf.size |> skipTo align
       let size = int64 size
       Job.whenDo (buf.capacity - offs < size)
         (Cond.wait (&buf.isFree) (fun () -> 0 = buf.numAccessors) |>> fun () ->
          doClose buf
          doOpenWithCapacity buf (max (offs + size) (buf.capacity + buf.capacity))) >>= fun () ->
       buf.size <- offs + size
       reply <-= offs

  let create (path: string) = Job.delay <| fun () ->
    let size =
      if File.Exists path then
        let info = FileInfo (path)
        info.Length
      else
        0L
    let buf = {
      path = path
      size = size
      capacity = size
      file = null
      view = null
      ptr = Unchecked.defaultof<_>
      numAccessors = 0
      isFree = ivar ()
      reqs = mb ()
    }
    doOpenWithCapacity buf (if size = 0L then 65536L else size)
    Job.foreverServer (server buf) >>%
    buf

  let close buf : Job<Alt<unit>> = Job.delay <| fun () ->
    let reply = ivar ()
    buf.reqs <<-+ Close reply >>% upcast reply

  let flush buf : Job<Alt<unit>> = Job.delay <| fun () ->
    let reply = ivar ()
    buf.reqs <<-+ Flush reply >>% upcast reply

  let accessFun buf access = Job.delay <| fun () ->
    let reply = ivar ()
    let access ptr =
      try reply <-= access ptr with e -> IVar.fillFailure reply e
    buf.reqs <<-+ Access access >>. reply

  let accessJob buf access = Job.delay <| fun () ->
    let reply = ivar ()
    let access ptr =
      Job.tryIn <| Job.delayWith access ptr
       <| fun x -> reply <-= x
       <| fun e -> IVar.fillFailure reply e
    buf.reqs <<-+ Access access >>. reply

  let append buf align size = Job.delay <| fun () ->
    let reply = ivar ()
    buf.reqs <<-+ Append (align, size, reply) >>. reply
