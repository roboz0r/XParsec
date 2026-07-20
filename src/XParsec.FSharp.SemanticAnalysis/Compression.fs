namespace XParsec.FSharp.SemanticAnalysis

open System.IO
open System.IO.Compression

/// Whole-blob (de)compression for cached artifacts. A pure SIZE optimization: the cache keys on
/// input hashes (see `Hashing`), never on the stored blob, so compression can neither cause a
/// wrong hit nor be a correctness determinant — the only obligation is the round-trip
/// `decompress (compress b) = b` for every `b`. That decoupling is why the codec sits behind this
/// seam and can be swapped (a native `zstd` was the plan's placeholder) without touching any
/// caller. Brotli is chosen because it is BCL (`System.IO.Compression`, zero new dependency) and
/// competitive at these small per-file blob sizes.
[<RequireQualifiedAccess>]
module Compression =

    /// Brotli-compress `bytes`. A fresh `MemoryStream` collects the compressed output; the
    /// `BrotliStream` must be disposed BEFORE reading the stream so its trailer is flushed.
    let compress (bytes: byte[]) : byte[] =
        use output = new MemoryStream()

        (use brotli = new BrotliStream(output, CompressionMode.Compress, leaveOpen = true)
         brotli.Write(bytes, 0, bytes.Length))

        output.ToArray()

    /// Inverse of `compress`: inflate the Brotli blob back to the original bytes. Reads the
    /// decompressing stream to end into a `MemoryStream` (the decompressed length is not carried
    /// in the frame, so `CopyTo` drains it).
    let decompress (bytes: byte[]) : byte[] =
        use input = new MemoryStream(bytes)
        use brotli = new BrotliStream(input, CompressionMode.Decompress)
        use output = new MemoryStream()
        brotli.CopyTo output
        output.ToArray()
