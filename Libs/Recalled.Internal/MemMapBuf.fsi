namespace Recalled.Internal

open Hopac

/// Operations on memory mapped buffers.
module MemMapBuf =
  /// Represents a memory mapped buffer that is persisted to a file.
  type MemMapBuf

  /// Creates a memory mapped buffer from and persisted to the specified file.
  val create: path: string -> Job<MemMapBuf>

  /// Returns the current size of the buffer.
  val size: buf: MemMapBuf -> Alt<PtrInt>

  /// Unsafe operations on memory mapped buffers.
  module Unsafe =
    /// Reduce the size of buffer.  This operation is safe only when the memory
    /// mapped buffer is not concurrenly accessed by other threads.
    val truncate: buf: MemMapBuf -> PtrInt -> Alt<unit>

  /// Closes the memory mapped buffer.  This must be called explicitly and the
  /// caller must wait for the alternative to make sure anything written to the
  /// buffer really will be persisted to the underlying file.
  val close: buf: MemMapBuf -> Alt<unit>

  /// Waits until the buffer is not being accessed and flushes the buffer.  Note
  /// that if you call this from inside an `accessJob` and wait for the reply
  /// then you have a deadlock.
  val flush: buf: MemMapBuf -> Alt<unit>

  /// Grants access to read/write arbitrarily from/to the buffer for the
  /// duration of the given function.  The function is passed the buffer start
  /// address.  After the function returns, the address will no longer be valid.
  val accessFun: buf: MemMapBuf
              -> readFun: (nativeptr<byte> -> 'x)
              -> Alt<'x>

  /// Grants access to read/write arbitrarily from/to the buffer for the
  /// duration of the given job.  The job is passed the buffer start address.
  /// After the job returns, the address will no longer be valid.
  val accessJob: buf: MemMapBuf
              -> readJob: (nativeptr<byte> -> #Job<'x>)
              -> Alt<'x>

  /// Allocates space from the end of the buffer and return offset to the start
  /// of the allocated space.
  val append: buf: MemMapBuf
           -> align: int
           -> size: int
           -> Alt<PtrInt>
