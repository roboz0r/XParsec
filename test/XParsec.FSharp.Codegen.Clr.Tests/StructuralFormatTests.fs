module XParsec.FSharp.Codegen.Clr.Tests.StructuralFormatTests

open Expecto
open Vesper

// Step 1 of vesper-printf-percentA-plan: the `%A` layout engine in isolation —
// no compiler. Hand-written `IStructuralFormattable` impls (compiled by fsc here)
// drive `StructuralPrinter.Print`, the same declarative sink the backend will
// synthesize against in step 3. The oracle is the spec (copy-pasteable Vesper
// source), not F#'s `sprintf "%A"` — we deliberately diverge (D-A/D-B).

/// `Print v 80` — the default 80-column budget (most values stay flat).
let private flat (v: obj) = StructuralPrinter.Print(v, 80)

/// Force breaking with a tiny budget.
let private narrow (v: obj) = StructuralPrinter.Print(v, 5)

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
                    test "null" { Expect.equal (StructuralPrinter.Print(null, 80)) "null" "null" }
                    test "string escapes" {
                        Expect.equal (flat (box "a\"b\nc")) "\"a\\\"b\\nc\"" "quote + newline escaped"
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
                            (StructuralPrinter.Print(box [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ], 0))
                            "[1; 2; 3; 4; 5; 6; 7; 8; 9; 10]"
                            "the %0A flat mode"
                    }
                    test "cycle is truncated" {
                        let xs = System.Collections.Generic.List<obj>()
                        xs.Add(xs)
                        Expect.equal (flat (box xs)) "[...]" "self-reference renders as ..."
                    }
                ]
        ]
