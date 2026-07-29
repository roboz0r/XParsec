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
// Measured, same three programs (compressed bytes). "ty columns" interned only a node's own
// type; "all types" interns everything a payload, a side table or a declaration shape embeds
// as well, which is the point at which no type is written structurally anywhere; "CSR
// children" is the child columns in compressed-sparse-row form:
//
//   | program                             | tree codec | pool columns | ty columns | all types | CSR children |
//   |-------------------------------------|-----------:|-------------:|-----------:|----------:|-------------:|
//   | curried fns                         |        391 |          475 |        418 |       387 |          385 |
//   | record type + literal + field get   |        214 |          255 |        217 |       204 |          207 |
//   | match + for-to + mutable accumulator|        382 |          469 |        376 |       366 |          371 |
//
// CSR is FLAT on the wire, and necessarily so — its raw form is 8 bytes larger per column
// (one length prefix for `Start` and one for `Ids`, against the jagged form's one for the
// outer array), and the per-slot length prefixes it drops are exactly the deltas of `Start`,
// so the same numbers are written either way. It is a HEAP change: two arrays per column
// instead of one per node.
//
// (The pool figures moved down from 504 / 282 / 508 as data the blob already carried
// elsewhere came out: the redundant `BinderNamings` column, the re-pooled `ValRepr`
// tuple-group patterns, and then the three SHAPE columns — a node's tag is a projection
// of its payload, so it was a byte per node per domain for a fact the payload's own tag
// already stored. Turning the two per-binder scalars into `BinderColumn`s then traded 4 id
// bytes per ENTRY for one presence byte per BINDER, which is a small win where the entries
// are dense against the binder pool and a small loss where they are sparse. The ceilings
// below are unchanged throughout: they exist to catch a LARGE inflation, not to track the
// number.)
//
// The columnar form is inherently spine-heavier than the recursive tree codec, which encodes
// the tree spine implicitly in its nesting (zero bytes) where the columns must name every
// child edge explicitly — a 4-byte dense id per edge, per domain, which is precisely what
// buys O(1) id-indexed access. What paid that back is HASH-CONSING the types: a `FrozenType`
// (and the `SymbolKey`/`TypeKey` cluster it reaches) recurs at thousands of nodes and was
// stored verbatim at each, and is now one row plus a 4-byte id per occurrence. The columns
// are now BELOW the tree codec on all three, with the id-indexed access kept.
//
// Nothing in the type domain is written structurally any more, so there is no more of this
// particular win left: the only thing that spells a type out is the row table itself, once
// per distinct type per unit. Nor is the SPINE further compressible without giving something
// up — a child edge IS a 4-byte id and every one of them is named. What remains is to stop
// PAYING for the ids at load: a flat int column is mmap-able, which is what step 9 is about,
// and no re-encoding buys that.

/// Freeze `src` and return the raw and compressed blob lengths — the pair the cache actually
/// trades off (`FrozenCache` stores the compressed form).
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

/// A small representative set spanning the shapes that dominate a real file's blob: curried
/// bindings and applications, a type declaration whose member bodies are named by pool id
/// (a `DeclPayload.Type`), and control flow with several binders — each with its measured
/// ceiling.
let private programs =
    [
        {
            Name = "curried fns"
            Source = "let add x y = x + y\nlet twice f x = f (f x)\nlet answer = twice (add 1) 40\n"
            Ceiling = 640
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
            Ceiling = 640
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
