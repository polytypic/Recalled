namespace Recall

open Hopac

/// Operations on memory mapped buffers.
module MemMapBuf =
  /// Represents a memory mapped buffer that is persisted to a file.
  type MemMapBuf

  /// Creates a memory mapped buffer from and persisted to the specified file.
  val create: path: string -> Job<MemMapBuf>

  /// Closes the memory mapped buffer.  This must be called explicitly to make
  /// sure anything written to the buffer really will be persisted to the
  /// underlying file.
  val close: MemMapBuf -> Job<unit>

  /// Grants access to read arbitrarily from the buffer for the duration of the
  /// given function.  The function is passed the buffer start address.  After
  /// the function returns, the address will no longer be valid.
  val readFun: MemMapBuf -> (nativeptr<byte> -> 'x) -> Job<'x>

  /// Grants access to read arbitrarily from the buffer for the duration of the
  /// given job.  The job is passed the buffer start address.  After the job
  /// returns, the address will no longer be valid.
  val readJob: MemMapBuf -> (nativeptr<byte> -> Job<'x>) -> Job<'x>

  /// Allocates space from the end of the buffer and grants write access to that
  /// portion.  The function is passed the buffer start address and the offset 
  /// to the newly reserved section.  After the function returns, the address
  /// will no longer be valid, but the offset can be combined with a buffer
  /// address obtained later to access the written data.
  val appendFun: MemMapBuf
              -> size: nativeint
              -> (nativeptr<byte> -> nativeint -> 'x)
              -> Job<'x>
