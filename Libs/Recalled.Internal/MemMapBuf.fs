namespace Recalled.Internal

open System
open System.IO
open System.IO.MemoryMappedFiles
open Hopac
open Hopac.Infixes

module MemMapBuf =
  type Access =
    abstract Do: IVar<'x> * (nativeptr<byte> -> #Job<'x>) -> Job<unit>

  type MemMapBuf =
    {sizeCh: Ch<IVar<int64>>
     truncateCh: Ch<int64 * IVar<unit>>
     closeCh: Ch<IVar<unit>>
     flushCh: Ch<IVar<unit>>
     accessCh: Ch<Access -> Job<unit>>
     appendCh: Ch<int * int * IVar<PtrInt>>}

  type State =
    {n: int
     capacity: PtrInt
     size: PtrInt
     file: MemoryMappedFile
     view: MemoryMappedViewAccessor
     ptr: nativeptr<byte>}

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

  let create (path: string) = Job.delay <| fun () ->
    let mmb = {sizeCh = Ch ()
               truncateCh = Ch ()
               closeCh = Ch ()
               flushCh = Ch ()
               accessCh = Ch ()
               appendCh = Ch ()}
    let finishedCh = Ch ()

    let doClose state =
      state.view.SafeMemoryMappedViewHandle.ReleasePointer ()
      state.view.Flush ()
      state.view.Dispose ()
      state.file.Dispose ()
      setFileLength path state.size

    let doOpen newCapacity newSize =
      setFileLength path newCapacity
      let file =
        MemoryMappedFile.CreateFromFile (
          path,
          FileMode.OpenOrCreate,
          null,
          int64 newCapacity)
      let view = file.CreateViewAccessor ()
      let mutable ptr = Unchecked.defaultof<_>
      view.SafeMemoryMappedViewHandle.AcquirePointer (&ptr)
      {n = 0
       capacity = newCapacity
       size = newSize
       file = file
       view = view
       ptr = ptr}

    let busy st =
      [| finishedCh ^->. {st with n = st.n-1}
         mmb.accessCh ^=> fun access ->
           Job.queue << access <| {new Access with
             member t.Do (reply, access) =
               Job.tryInDelay <| fun () -> access st.ptr
                 <| fun x -> reply *<= x
                 <| fun e -> reply *<=! e
               >>=. finishedCh *<- ()} >>-. {st with n = st.n+1}
         mmb.appendCh ^=> fun (align, size, reply) ->
           let offs = st.size |> skipTo align
           let size = int64 size
           let newSize = offs + size
           let finish state = reply *<= offs >>-. state
           if st.capacity < newSize
           then Job.forN st.n finishedCh >>= fun () ->
                doClose st
                doOpen <| max newSize (2L * st.capacity) <| newSize |> finish
           else finish {st with size = newSize}
         mmb.sizeCh ^=> fun reply ->
           reply *<= st.size >>-. st
         mmb.truncateCh ^=> fun (size, reply) ->
           let err msg = reply *<=! Exception (msg) >>-. st
           if size < 0L then
             sprintf "Trucate given negative size %d" size |> err
           elif st.size < size then
             sprintf "Truncate buffer size from %d to %d" st.size size |> err
           else
             reply *<= () >>-. {st with size = size} |] |> Alt.choosy
    let idle st =
      [| busy st
         mmb.flushCh ^=> fun reply ->
           st.view.Flush ()
           reply *<= () >>-. st
         mmb.closeCh ^=> fun reply ->
           doClose st
           reply *<= () >>= Job.abort |] |> Alt.choosy

    let size = getFileLengthOr0 path
    let size = if size = 0L then 65536L else size
    Job.iterateServer <| doOpen size size <| fun st ->
          if st.n = 0 then idle st else busy st
    >>-. mmb

  let size buf = buf.sizeCh *<-=>- id
  module Unsafe =
    let truncate buf size = buf.truncateCh *<-=>- fun reply -> (size, reply)
  let close buf = buf.closeCh *<-=>- id
  let flush buf = buf.flushCh *<-=>- id
  let accessJob buf access =
    buf.accessCh *<-=>- fun reply s -> s.Do (reply, access)
  let accessFun buf access = access >> Job.result |> accessJob buf
  let append buf align size =
    buf.appendCh *<-=>- fun reply -> (align, size, reply)
