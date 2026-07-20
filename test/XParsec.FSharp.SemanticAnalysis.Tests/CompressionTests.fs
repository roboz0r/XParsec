module XParsec.FSharp.SemanticAnalysis.Tests.CompressionTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// Compression is a pure size optimization decoupled from cache correctness (the key hashes
// inputs, not the blob), so the ONLY contract to gate is the exact round-trip
// `decompress (compress b) = b`. Exercise it over representative blobs — empty, a fixed small
// run, a large low-entropy run, and a REAL frozen-file blob — then a light sanity check that a
// realistic blob actually shrinks.

/// A real `FrozenCodec.flatten` blob: freeze a small program the shared contract stack resolves,
/// then flatten it. This is the representative payload the cache actually stores.
let private realFrozenBlob () : byte[] =
    let src =
        "let add x y = x + y\nlet twice f x = f (f x)\nlet answer = twice (add 1) 40\n"

    let lexed, file = parseFile src
    let frozen = Pipeline.analyseFor "TestAsm" realProvider.Value src lexed file
    FrozenCodec.flatten frozen

[<Tests>]
let tests =
    testList
        "Compression"
        [
            test "round-trips empty bytes" {
                let b = [||]
                Expect.equal (Compression.decompress (Compression.compress b)) b "empty survives"
            }

            test "round-trips a small fixed blob" {
                let b = [| 0uy; 1uy; 2uy; 255uy; 128uy; 42uy |]
                Expect.equal (Compression.decompress (Compression.compress b)) b "small blob survives"
            }

            test "round-trips a large low-entropy blob" {
                let b = Array.init 200_000 (fun i -> byte (i % 251))
                Expect.equal (Compression.decompress (Compression.compress b)) b "large blob survives"
            }

            test "round-trips a real frozen-file blob" {
                let b = realFrozenBlob ()
                Expect.equal (Compression.decompress (Compression.compress b)) b "frozen blob survives"
            }

            // A light sanity check, not a hard ratio: a realistic frozen blob has enough
            // redundancy (repeated keys, offsets, tags) that Brotli must shrink it. If it does
            // not, the seam is not compressing at all.
            test "a realistic frozen blob compresses smaller" {
                let b = realFrozenBlob ()
                let z = Compression.compress b
                Expect.isGreaterThan b.Length z.Length "compressed frozen blob is smaller than the raw blob"
            }
        ]
