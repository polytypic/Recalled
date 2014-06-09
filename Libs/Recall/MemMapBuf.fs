namespace Recall

open Microsoft.FSharp.NativeInterop
open System
open System.IO
open System.IO.MemoryMappedFiles
open System.Threading
open Hopac
open Hopac.Job.Infixes
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
   | Read of (nativeptr<byte> -> Job<unit>)
   | Append of nativeint * (nativeptr<byte> -> nativeint -> Job<unit>)
   | Close of IVar<unit>

  type MemMapBuf = {
      path: string
      mutable size: nativeint
      mutable capacity: nativeint
      mutable file: MemoryMappedFile
      mutable view: MemoryMappedViewAccessor
      mutable ptr: nativeptr<byte>
      reqs: Mailbox<Req>
      mutable numAccessors: int
      mutable isFree: IVar<unit>
    }

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
     | Close reply ->
       Cond.wait (&buf.isFree) (fun () -> 0 = buf.numAccessors) >>= fun () ->
       doClose buf
       reply <-= ()
     | Read read ->
       Interlocked.Increment &buf.numAccessors |> ignore
       Job.queue (read buf.ptr >>= fun () ->
         Cond.signal buf.isFree (0 = Interlocked.Decrement &buf.numAccessors))
     | Append (size, append) ->
       Job.whenDo (buf.capacity - buf.size < size)
         (Cond.wait (&buf.isFree) (fun () -> 0 = buf.numAccessors) |>> fun () ->
          doClose buf
          doOpenWithCapacity buf (max (buf.size + size) (buf.capacity + buf.capacity))) >>= fun () ->
       let offs = buf.size
       buf.size <- buf.size + size
       Interlocked.Increment &buf.numAccessors |> ignore
       Job.queue (append buf.ptr (nativeint offs) >>= fun () ->
          Cond.signal buf.isFree (0 = Interlocked.Decrement &buf.numAccessors))

  let create (path: string) = Job.delay <| fun () ->
    let size =
      if File.Exists path then
        let info = FileInfo (path)
        nativeint info.Length
      else
        0n
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
    doOpenWithCapacity buf (if size = 0n then 65536n else size)
    Job.foreverServer (server buf) >>%
    buf

  let close buf = Job.delay <| fun () ->
    let reply = ivar ()
    buf.reqs <<-+ Close reply >>. reply

  let readFun buf read = Job.delay <| fun () ->
    let reply = ivar ()
    let read ptr =
      try reply <-= read ptr with e -> IVar.fillFailure reply e
    buf.reqs <<-+ Read read >>. reply

  let readJob buf read = Job.delay <| fun () ->
    let reply = ivar ()
    let read ptr =
      Job.tryIn <| Job.delayWith read ptr
       <| fun x -> reply <-= x
       <| fun e -> IVar.fillFailure reply e
    buf.reqs <<-+ Read read >>. reply

  let appendFun buf size append = Job.delay <| fun () ->
    let reply = ivar ()
    let append ptr off =
      try reply <-= append ptr off with e -> IVar.fillFailure reply e
    buf.reqs <<-+ Append (size, append) >>. reply
