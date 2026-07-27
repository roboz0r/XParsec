module XParsec.FSharp.SemanticAnalysis.Tests.FrozenBlobSizeTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// The wire-format size gate. The stored artifact is `Compression.compress (FrozenCodec.flatten
// f)`, so the number that matters is the COMPRESSED length — the raw blob is an intermediate
// nobody keeps, and Brotli absorbing the encoding's redundancy is exactly why the columns are
// free to store plain `int` ids instead of hand-rolled varints.
//
// The ceilings are measured figures with headroom, not targets. Compressed output is not stable
// across Brotli versions or minor payload reorderings, so a tight assertion would be noise; the
// job here is to catch a format change that inflates the blob by a LARGE factor.
//
// Measured, same three programs (compressed bytes):
//
//   | program                             | tree codec | pool columns |
//   |-------------------------------------|-----------:|-------------:|
//   | curried fns                         |        391 |          475 |
//   | record type + literal + field get   |        214 |          258 |
//   | match + for-to + mutable accumulator|        382 |          468 |
//
// (The pool figures moved down from 504 / 282 / 508 as data the blob already carried
// elsewhere came out: the redundant `BinderNamings` column, the re-pooled `ValRepr`
// tuple-group patterns, and then the three SHAPE columns — a node's tag is a projection
// of its payload, so it was a byte per node per domain for a fact the payload's own tag
// already stored. The ceilings below are unchanged: they exist to catch a LARGE
// inflation, not to track the number.)
//
// The pool form is ~22% LARGER, and that is inherent to it rather than a defect: the recursive
// tree codec encodes the tree spine implicitly in its nesting (zero bytes), whereas the columnar
// form must name every child edge explicitly — a length prefix plus a 4-byte dense id per edge,
// per domain — which is precisely what buys O(1) id-indexed access. The compensating size work is
// interning `FrozenType`/`SymbolKey`/`TypeKey` (each recurs thousands of times across the
// columns and is stored verbatim today); it is deliberately a separate change, since the cache
// key hashes INPUTS rather than the blob, so size is decoupled from correctness.

/// Freeze `src` and return the raw and compressed blob lengths — the pair the cache actually
/// trades off (`FrozenCache` stores the compressed form).
let private blobSizes (src: string) : struct (int * int) =
    let lexed, file = parseFile src
    let frozen = Pipeline.analyseFor "TestAsm" realProvider.Value src lexed file
    let raw = FrozenCodec.flatten frozen
    struct (raw.Length, (Compression.compress raw).Length)

/// A small representative set spanning the shapes that dominate a real file's blob: curried
/// bindings and applications, a type declaration with fields (a `DeclPayload.Type`, carried
/// opaquely), and control flow with several binders — each with its measured ceiling.
let private programs =
    [
        "curried fns", "let add x y = x + y\nlet twice f x = f (f x)\nlet answer = twice (add 1) 40\n", 640
        "record type + literal + field get",
        "type R = { X: int; Y: int }\nlet r = { X = 1; Y = 2 }\nlet getX (v: R) = v.X\n",
        360
        "match + for-to + mutable accumulator",
        "let classify x =\n    match x with\n    | 0 -> 1\n    | _ -> 2\n\nlet sumTo n =\n    let mutable t = 0\n    for i = 1 to n do\n        t <- t + i\n    t\n",
        640
    ]

[<Tests>]
let tests =
    testList
        "Frozen blob size"
        [
            for name, src, ceiling in programs do
                test name {
                    let struct (raw, compressed) = blobSizes src

                    // Reported on failure so a re-measure needs no instrumentation run.
                    Expect.isLessThan
                        compressed
                        ceiling
                        (sprintf "%s: compressed %d bytes (raw %d) exceeds the recorded ceiling" name compressed raw)
                }
        ]
