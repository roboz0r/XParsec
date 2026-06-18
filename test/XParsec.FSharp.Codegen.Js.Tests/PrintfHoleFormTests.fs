module XParsec.FSharp.Codegen.Js.Tests.PrintfHoleFormTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis

// printf-shared-core-plan.md: the per-hole classification lives in
// `SemanticAnalysis.PrintfHoleForm` as a *semantic* `HoleForm` — the single
// accept-gate (`tryClassify`) shared by the Freeze lowering decision and both
// codegen backends. The CLR-only `(HoleKind, .NET-format, alignment)` projection
// is `Codegen.Clr.ClrHoleFormat.toDotNetFormat`.
//
// Step (a)'s equivalence net compared `tryClassify` against the legacy
// `PrintfSpec.tryHoleFormat` oracle; step (d) deleted that oracle, so byte-parity
// is now pinned end-to-end by `PrintfDifferentialTests` / `PrintfHappyPathTests`
// (CLR) and `PrintfPhase4Tests` (JS Node). What remains here is a light
// non-vacuity guard: the classifier must both accept and defer across the matrix.

/// Every specifier string the matrix probes: `%` + flags + optional width +
/// optional precision + a type char. Many combinations don't parse or aren't
/// faithful — the classifier defers those.
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
            test "the matrix actually exercises both faithful and deferred holes" {
                // Guards against a vacuous classifier (e.g. one that accepts or
                // defers everything): there must be real `Some` classifications AND
                // real `ValueNone` deferrals among the parseable specifiers.
                let classified =
                    specifiers
                    |> List.choose (fun spec ->
                        match Lexing.parseFormatSpecifier spec with
                        | ValueNone -> None
                        | ValueSome p -> Some(PrintfHoleForm.tryClassify p)
                    )

                Expect.isGreaterThan
                    (classified |> List.filter (fun c -> c.IsSome) |> List.length)
                    0
                    "some holes classify"

                Expect.isGreaterThan (classified |> List.filter (fun c -> c.IsNone) |> List.length) 0 "some holes defer"
            }
        ]
