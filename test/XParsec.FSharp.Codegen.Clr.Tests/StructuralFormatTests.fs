module XParsec.FSharp.Codegen.Clr.Tests.StructuralFormatTests

open Expecto
open Vesper
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The `%A` layout engine in isolation — no compiler. Hand-written
// `IStructuralFormattable` impls (compiled by fsc here) drive the layout engine,
// the same declarative sink the backend synthesises against. The oracle is the
// spec (copy-pasteable Vesper source), not F#'s `sprintf "%A"` — we deliberately
// diverge.
//
// These drive the **Vesper-compiled** `StructuralPrinter` (`structural-printer.fs`)
// via reflection (`structuralPrint` / `structuralPrintSized` in `TestHelpers`) — the
// real backend-emitted engine, loaded from the `buildPackage`-produced
// `Vesper.Printf.dll`. The `Sem*` `IStructuralFormattable` impls still bind the Core
// interfaces at compile time; only the engine entry point is the self-hosted one.

/// `structuralPrint v 80` — the default 80-column budget (most values stay flat).
let private flat (v: obj) = structuralPrint v 80

/// Force breaking with a tiny budget.
let private narrow (v: obj) = structuralPrint v 5

// ---- the semantic protocol (BeginRecord/Field/BeginCase/Child) ----
// These hand-written impls drive the sink's semantic members directly — the only
// recursion vocabulary a `Format` body has (the layout-op recursion entries were
// retired in Phase C). The sink reconstructs the `{ F = ·; G = · }` / `Case ·` forms.

// A record driven semantically: `Field name` marks each label, `Child` supplies
// the value. The sink owns the `{ … }` / `+2` hang policy.
type SemPoint =
    {
        PX: int
        PY: string
    }

    interface IStructuralFormattable with
        member this.Format(sink: IFormatSink) =
            sink.BeginRecord()
            sink.Field("X")
            sink.Child(box this.PX)
            sink.Field("Y")
            sink.Child(box this.PY)
            sink.EndRecord()

// An option-shaped DU driven semantically: `BeginCase name; (Child payload); EndCase`.
// The sink decides nullary vs single-payload and the single-payload parenthesisation
// (`Some (Some 3)` but not `Some 3` / `Some None`) from the observed child count + the
// application-shaped mark.
type SemOpt =
    | SemNone
    | SemSome of obj

    interface IStructuralFormattable with
        member this.Format(sink: IFormatSink) =
            match this with
            | SemNone ->
                sink.BeginCase("None")
                sink.EndCase()
            | SemSome v ->
                sink.BeginCase("Some")
                sink.Child(v)
                sink.EndCase()

// A record whose second field is itself a `Child` — exercises the pending-label
// invariant: the outer `Inner = ` label must be emitted before recursing, so the
// nested record's first `Field` cannot clobber it.
type SemBox =
    {
        BLabel: string
        BInner: obj
    }

    interface IStructuralFormattable with
        member this.Format(sink: IFormatSink) =
            sink.BeginRecord()
            sink.Field("Label")
            sink.Child(box this.BLabel)
            sink.Field("Inner")
            sink.Child(this.BInner)
            sink.EndRecord()

// A two-payload case, to exercise the tuple arm `Pair (a, b)`.
type SemPair =
    | SemPair of obj * obj

    interface IStructuralFormattable with
        member this.Format(sink: IFormatSink) =
            match this with
            | SemPair(a, b) ->
                sink.BeginCase("Pair")
                sink.Child(a)
                sink.Child(b)
                sink.EndCase()

[<Tests>]
let tests =
    testList
        "StructuralFormat"
        [
            testList
                "atoms"
                [
                    test "int" { Expect.equal (flat (box 42)) "42" "int" }
                    test "negative int" { Expect.equal (flat (box -3)) "-3" "negative int" }
                    test "bool" { Expect.equal (flat (box true)) "true" "lowercase bool" }
                    test "float gets a .0" { Expect.equal (flat (box 3.0)) "3.0" "whole float reads as float" }
                    test "float fraction" { Expect.equal (flat (box 3.5)) "3.5" "fractional float" }
                    test "nan" { Expect.equal (flat (box (0.0 / 0.0))) "nan" "nan spelling" }
                    test "string is quoted" { Expect.equal (flat (box "hi")) "\"hi\"" "quoted string" }
                    test "char is quoted" { Expect.equal (flat (box 'c')) "'c'" "quoted char" }
                    test "null" { Expect.equal (structuralPrint null 80) "null" "null" }
                    test "string escapes" {
                        Expect.equal (flat (box "a\"b\nc")) "\"a\\\"b\\nc\"" "quote + newline escaped"
                    }
                ]

            testList
                "primitives round-trip"
                // The expected strings are the suffix spellings the XParsec.FSharp
                // lexer accepts (`Lexing.getIntTokenFromSpan` / `getDecimalFloatTokenFromSpan`,
                // the canonical authority) — each re-lexes at the *same value and
                // type*, which is the round-trip the engine owes for atoms. The type
                // suffix is what makes it round-trip: bare `5` would lex as `int32`,
                // not `int64` / `byte`. They also equal `sprintf "%A"` (asserted below
                // for the integral forms, which never use exponent notation).
                [
                    test "int32 (no suffix)" { Expect.equal (flat (box 5)) "5" "int32" }
                    test "sbyte" { Expect.equal (flat (box 5y)) "5y" "sbyte suffix" }
                    test "byte" { Expect.equal (flat (box 5uy)) "5uy" "byte suffix" }
                    test "int16" { Expect.equal (flat (box 5s)) "5s" "int16 suffix" }
                    test "uint16" { Expect.equal (flat (box 5us)) "5us" "uint16 suffix" }
                    test "uint32" { Expect.equal (flat (box 5u)) "5u" "uint32 suffix" }
                    test "int64" { Expect.equal (flat (box 5L)) "5L" "int64 suffix" }
                    test "uint64" { Expect.equal (flat (box 5UL)) "5UL" "uint64 suffix" }
                    test "nativeint" { Expect.equal (flat (box 5n)) "5n" "nativeint suffix" }
                    test "unativeint" { Expect.equal (flat (box 5un)) "5un" "unativeint suffix" }
                    test "decimal" { Expect.equal (flat (box 1.5m)) "1.5M" "decimal suffix" }
                    test "decimal whole" { Expect.equal (flat (box 5m)) "5M" "decimal whole suffix" }
                    test "float32 fraction" { Expect.equal (flat (box 1.5f)) "1.5f" "float32 suffix" }
                    test "float32 whole gets .0f" { Expect.equal (flat (box 3.0f)) "3.0f" "float32 .0 + f" }
                    test "negative int64" { Expect.equal (flat (box -3L)) "-3L" "sign before suffix" }
                    test "float32 nan" { Expect.equal (flat (box (0.0f / 0.0f))) "nanf" "nanf spelling" }
                    test "float32 infinity" { Expect.equal (flat (box (1.0f / 0.0f))) "infinityf" "infinityf" }

                    // The suffixed forms equal F#'s own `%A` (integers never use
                    // exponent notation, so this is an exact oracle — unlike floats,
                    // whose shortest-round-trip representation can diverge cosmetically).
                    test "matches sprintf %A for integrals" {
                        for actual, expected in
                            [
                                flat (box 5y), sprintf "%A" 5y
                                flat (box 5uy), sprintf "%A" 5uy
                                flat (box 5s), sprintf "%A" 5s
                                flat (box 5us), sprintf "%A" 5us
                                flat (box 5u), sprintf "%A" 5u
                                flat (box 5L), sprintf "%A" 5L
                                flat (box 5UL), sprintf "%A" 5UL
                                flat (box 1.5m), sprintf "%A" 1.5m
                            ] do
                            Expect.equal actual expected "engine atom = F# %A literal"
                    }
                ]

            testList
                "collections"
                [
                    test "list flat (slice-4 form)" {
                        Expect.equal (flat (box [ 1; 2; 3 ])) "[1; 2; 3]" "copy-pasteable list literal"
                    }
                    test "list broken" {
                        Expect.equal (narrow (box [ 1; 2; 3 ])) "[\n  1;\n  2;\n  3\n]" "indented, dedented closer"
                    }
                    test "tuple flat" { Expect.equal (flat (box (1, "a"))) "(1, \"a\")" "tuple" }
                    test "empty list" { Expect.equal (flat (box ([]: int list))) "[]" "empty list" }
                ]

            testList
                "policy"
                [
                    test "width 0 never breaks" {
                        Expect.equal
                            (structuralPrint (box [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ]) 0)
                            "[1; 2; 3; 4; 5; 6; 7; 8; 9; 10]"
                            "the %0A flat mode"
                    }
                    test "cycle is truncated" {
                        let xs = System.Collections.Generic.List<obj>()
                        xs.Add(xs)
                        Expect.equal (flat (box xs)) "[...]" "self-reference renders as ..."
                    }
                ]

            // `%.NA` — the PrintSize node budget (F# sformat.fs `countNodes`). Each
            // leaf spends one unit; composites don't. Past the budget the engine
            // truncates with `...`. The collection cases match F#'s `sprintf "%.NA"`.
            testList
                "size budget (%.NA)"
                [
                    test "size 2 truncates a list after 2 leaves" {
                        Expect.equal
                            (structuralPrintSized (box [ 1; 2; 3; 4; 5 ]) 80 2)
                            "[1; 2; ...]"
                            "two elements then ..."
                    }
                    test "size 0 truncates immediately" {
                        Expect.equal (structuralPrintSized (box [ 1; 2; 3 ]) 80 0) "..." "nothing fits"
                    }
                    test "size above the content prints in full" {
                        Expect.equal (structuralPrintSized (box [ 1; 2; 3 ]) 80 10) "[1; 2; 3]" "budget not reached"
                    }
                    test "the budget is shared across a nested list" {
                        Expect.equal
                            (structuralPrintSized (box [ [ 1; 2 ]; [ 3; 4 ]; [ 5; 6 ] ]) 80 3)
                            "[[1; 2]; [3; ...]; ...]"
                            "3 leaves spent, then nested + outer ..."
                    }
                    test "a tuple truncates per leaf (preserving arity)" {
                        Expect.equal
                            (structuralPrintSized (box (1, 2, 3)) 80 1)
                            "(1, ..., ...)"
                            "one leaf, the rest ... (matches F#)"
                    }
                ]

            // The NEW semantic sink members (BeginRecord/Field/BeginCase/Child/…),
            // driven directly since the emitter does not yet call them. Output must
            // match the layout the equivalent layout-op impls produce.
            testList
                "semantic protocol"
                [
                    test "record renders like the layout ops" {
                        Expect.equal (flat (box { PX = 1; PY = "a" })) "{ X = 1; Y = \"a\" }" "{ X = 1; Y = \"a\" }"
                    }
                    test "record breaks under the budget" {
                        Expect.equal
                            (narrow (box { PX = 1; PY = "a" }))
                            "{ X = 1;\n  Y = \"a\" }"
                            "record breaks under the budget"
                    }
                    test "nullary case is a bare identifier" { Expect.equal (flat (box SemNone)) "None" "None" }
                    test "single atom payload does not parenthesise" {
                        Expect.equal (flat (box (SemSome(box 3)))) "Some 3" "Some 3"
                    }
                    test "single negative-literal payload does not parenthesise" {
                        Expect.equal
                            (flat (box (SemSome(box -3))))
                            "Some -3"
                            "Some -3 (adjacent minus lexes as a literal)"
                    }
                    test "single case payload parenthesises (application-shaped)" {
                        Expect.equal (flat (box (SemSome(box (SemSome(box 3)))))) "Some (Some 3)" "Some (Some 3)"
                    }
                    test "single nullary-case payload does not parenthesise" {
                        Expect.equal (flat (box (SemSome(box SemNone)))) "Some None" "Some None"
                    }
                    test "single list payload does not parenthesise" {
                        Expect.equal (flat (box (SemSome(box [ 1; 2 ])))) "Some [1; 2]" "Some [1; 2]"
                    }
                    test "single record payload does not parenthesise" {
                        Expect.equal
                            (flat (box (SemSome(box { PX = 1; PY = "a" }))))
                            "Some { X = 1; Y = \"a\" }"
                            "Some { X = 1; Y = \"a\" }"
                    }
                    test "two-field payload renders as a tuple" {
                        Expect.equal (flat (box (SemPair(box 1, box "a")))) "Pair (1, \"a\")" "Pair (1, \"a\")"
                    }
                    test "tuple-arm components are not individually parenthesised" {
                        Expect.equal
                            (flat (box (SemPair(box (SemSome(box 1)), box (SemSome(box 2))))))
                            "Pair (Some 1, Some 2)"
                            "Pair (Some 1, Some 2)"
                    }
                    test "an application-shaped tuple case parenthesises as a lone payload" {
                        Expect.equal
                            (flat (box (SemSome(box (SemPair(box 1, box 2))))))
                            "Some (Pair (1, 2))"
                            "Some (Pair (1, 2))"
                    }
                    test "nested record field does not clobber the outer label" {
                        Expect.equal
                            (flat (
                                box
                                    {
                                        BLabel = "a"
                                        BInner = box { PX = 1; PY = "b" }
                                    }
                            ))
                            "{ Label = \"a\"; Inner = { X = 1; Y = \"b\" } }"
                            "outer Inner = label survives the nested record's first Field"
                    }
                ]
        ]
