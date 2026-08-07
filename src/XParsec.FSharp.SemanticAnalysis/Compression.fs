namespace XParsec.FSharp.SemanticAnalysis

open System.IO
open System.IO.Compression

[<RequireQualifiedAccess>]
module Compression =

    /// The `BrotliStream` is scoped to dispose BEFORE `output` is read, so its trailer is flushed.
    let compress (bytes: byte[]) : byte[] =
        use output = new MemoryStream()

        (use brotli = new BrotliStream(output, CompressionMode.Compress, leaveOpen = true)
         brotli.Write(bytes, 0, bytes.Length))

        output.ToArray()

    let decompress (bytes: byte[]) : byte[] =
        use input = new MemoryStream(bytes)
        use brotli = new BrotliStream(input, CompressionMode.Decompress)
        use output = new MemoryStream()
        brotli.CopyTo output
        output.ToArray()
