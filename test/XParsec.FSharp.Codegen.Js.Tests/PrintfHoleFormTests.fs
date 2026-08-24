module XParsec.FSharp.Codegen.Js.Tests.PrintfHoleFormTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis

// `classify` is the accept-gate both backends share: it either classifies a hole into
// a `HoleForm` or defers it to the cold path. Byte-parity of the classified output is
// pinned by the per-target specifier suites; what is left here is a non-vacuity guard.

/// Every specifier string the matrix probes: `%` + flags + optional width +
/// optional precision + a type char. Many combinations do not parse at all,
/// and the classifier defers many of those that do.
let private specifiers: string list =
    [
        let flagSets = [ ""; "-"; "0"; "+"; " "; "-0"; "+0"; "+ " ]
        let widths = [ ""; "5"; "12" ]
        let precisions = [ ""; ".0"; ".2"; ".6" ]

        let typeChars =
            [
                'b'
                's'
                'c'
                'd'
                'i'
                'u'
                'x'
                'X'
                'o'
                'B'
                'e'
                'E'
                'f'
                'g'
                'G'
                'M'
                'O'
                'A'
            ]

        for flags in flagSets do
            for w in widths do
                for prec in precisions do
                    for tc in typeChars do
                        yield "%" + flags + w + prec + string tc
    ]

[<Tests>]
let tests =
    testList
        "PrintfHoleForm"
        [
            test "the matrix exercises both classified and deferred holes" {
                let classified =
                    specifiers
                    |> List.choose (fun spec ->
                        match Lexing.parseFormatSpecifier spec with
                        | ValueNone -> None
                        | ValueSome p -> Some(PrintfHoleForm.classify p)
                    )

                let lowerable (v: PrintfHoleForm.HoleVerdict) =
                    match v with
                    | PrintfHoleForm.HoleVerdict.Lowerable _ -> true
                    | PrintfHoleForm.HoleVerdict.Residual
                    | PrintfHoleForm.HoleVerdict.SignLeftAlignZeroPad
                    | PrintfHoleForm.HoleVerdict.OversizedDimension -> false

                Expect.isGreaterThan (classified |> List.filter lowerable |> List.length) 0 "some holes classify"

                Expect.isGreaterThan (classified |> List.filter (lowerable >> not) |> List.length) 0 "some holes defer"
            }
        ]
