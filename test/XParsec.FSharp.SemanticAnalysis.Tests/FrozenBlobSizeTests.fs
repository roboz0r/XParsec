module XParsec.FSharp.SemanticAnalysis.Tests.FrozenBlobSizeTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// Gates the STORED artifact, `Compression.compress (FrozenCodec.flatten f)`; the raw blob is an
// intermediate nobody keeps. Compressed output shifts with Brotli versions and payload
// reordering, so the ceilings are measured figures with headroom: they catch a LARGE inflation.

/// The raw and compressed lengths of `src`'s frozen blob, in that order.
let private blobSizes (src: string) : struct (int * int) =
    let raw = FrozenCodec.flatten (freezeFor src)
    struct (raw.Length, (Compression.compress raw).Length)

/// One measured program and the ceiling its compressed blob must stay under.
type private SizedProgram =
    {
        Name: string
        Source: string
        Ceiling: int
    }

/// The shapes that dominate a real file's blob, each with its measured ceiling.
let private programs =
    [
        {
            Name = "curried fns"
            Source = "let add x y = x + y\nlet twice f x = f (f x)\nlet total = twice (add 1) 40\n"
            Ceiling = 780
        }
        {
            Name = "record type + literal + field get"
            Source = "type R = { X: int; Y: int }\nlet r = { X = 1; Y = 2 }\nlet getX (v: R) = v.X\n"
            Ceiling = 360
        }
        {
            Name = "match + for-to + mutable accumulator"
            Source =
                "let classify x =\n    match x with\n    | 0 -> 1\n    | _ -> 2\n\nlet sumTo n =\n    let mutable t = 0\n    for i = 1 to n do\n        t <- t + i\n    t\n"
            Ceiling = 780
        }
    ]

[<Tests>]
let tests =
    testList
        "Frozen blob size"
        [
            for p in programs do
                test p.Name {
                    let struct (raw, compressed) = blobSizes p.Source

                    // Reported on failure so a re-measure needs no instrumentation run.
                    Expect.isLessThan
                        compressed
                        p.Ceiling
                        (sprintf "%s: compressed %d bytes (raw %d) exceeds the recorded ceiling" p.Name compressed raw)
                }
        ]
