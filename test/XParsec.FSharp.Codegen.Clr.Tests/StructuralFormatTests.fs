module XParsec.FSharp.Codegen.Clr.Tests.StructuralFormatTests

open Expecto
open Vesper
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The `%A` layout engine in isolation —
// no compiler. Hand-written `IStructuralFormattable` impls (compiled by fsc here)
// drive the layout engine, the same declarative sink the backend synthesises
// against. The oracle is the spec (copy-pasteable Vesper source), not F#'s
// `sprintf "%A"` — we deliberately diverge.
//
// PP7 step 2: these drive the **Vesper-compiled** `StructuralPrinter`
// (`structural-printer.fs`) via reflection (`structuralPrint` / `structuralPrintSized`
// in `TestHelpers`), not the C# `Vesper.StructuralPrinter` that fsc would bind here.
// The `Point`/`Opt` `IStructuralFormattable` impls still bind the Core interfaces at
// compile time; only the engine entry point is now the self-hosted one. The
// cycle-truncation test thus exercises the Vesper cons-list `ReferenceEquals` scan.

/// `structuralPrint v 80` — the default 80-column budget (most values stay flat).
let private flat (v: obj) = structuralPrint v 80

/// Force breaking with a tiny budget.
let private narrow (v: obj) = structuralPrint v 5

// A record with a hand-written Format: `{ X = <int>; Y = <string> }`.
type Point =
    {
        X: int
        Y: string
    }

    interface IStructuralFormattable with
        member this.Format(sink: IFormatSink) =
            sink.BeginGroup()
            sink.Text("{ X = ")
            sink.BeginNest(2)
            sink.FormatChild(box this.X)
            sink.Text(";")
            sink.Line()
            sink.Text("Y = ")
            sink.FormatChild(this.Y)
            sink.EndNest()
            sink.Text(" }")
            sink.EndGroup()

// An option-shaped DU: `None` / `Some <payload>`. The payload is rendered in
// argument position (`FormatArg`), so a nested `Some` parenthesizes but a
// negative literal (a single atom token) does not.
type Opt =
    | None0
    | Some0 of obj

    interface IStructuralFormattable with
        member this.Format(sink: IFormatSink) =
            match this with
            | None0 -> sink.Text("None")
            | Some0 v ->
                sink.BeginApplication()
                sink.Text("Some ")
                sink.FormatArg(v)
                sink.EndApplication()

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
                "structural"
                [
                    test "record flat" {
                        Expect.equal (flat (box { X = 1; Y = "a" })) "{ X = 1; Y = \"a\" }" "record flat"
                    }
                    test "record broken" {
                        Expect.equal
                            (narrow (box { X = 1; Y = "a" }))
                            "{ X = 1;\n  Y = \"a\" }"
                            "record breaks under the budget"
                    }
                    test "DU no parens for simple payload" {
                        Expect.equal (flat (box (Some0(box 3)))) "Some 3" "Some 3"
                    }
                    test "DU parenthesizes a nested application" {
                        Expect.equal (flat (box (Some0(box (Some0(box 3)))))) "Some (Some 3)" "Some (Some 3)"
                    }
                    test "DU does NOT parenthesize a negative literal" {
                        Expect.equal
                            (flat (box (Some0(box -3))))
                            "Some -3"
                            "Some -3 (adjacent minus lexes as a literal)"
                    }
                    test "nullary DU case" { Expect.equal (flat (box None0)) "None" "None" }
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
        ]
