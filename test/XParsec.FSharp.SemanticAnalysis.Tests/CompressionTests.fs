module XParsec.FSharp.SemanticAnalysis.Tests.CompressionTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// Compression is a pure size optimization over an already-encoded blob, so the contract to
// gate is the exact round-trip `decompress (compress b) = b`.

/// A real flattened blob — the payload the wire format carries.
let private realFrozenBlob () : byte[] =
    FrozenCodec.flatten (freezeFor "let add x y = x + y\nlet twice f x = f (f x)\nlet answer = twice (add 1) 40\n")

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

            // Not a ratio: a frozen blob's repeated keys, offsets and tags mean any shrink at
            // all, and a failure here says the seam is not compressing.
            test "a realistic frozen blob compresses smaller" {
                let b = realFrozenBlob ()
                let z = Compression.compress b
                Expect.isGreaterThan b.Length z.Length "compressed frozen blob is smaller than the raw blob"
            }
        ]
