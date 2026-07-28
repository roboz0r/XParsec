module XParsec.FSharp.SemanticAnalysis.Tests.DiagnosticCodeTests

open Expecto
// Ahead of the SemanticAnalysis open so the bare `Diagnostic` stays the semantic one; this
// is here for the parser's `DiagnosticCode`, which `Kind.Parse` wraps.
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// A `DiagCode.FSharp` number is a claim that fsc files the SAME verdict under the SAME
// number, so a program refused here is refused there for the same stated reason. The claim
// is only worth making if it is pinned: the mapping lives in one `match`, and one edit to it
// silently re-labels every diagnostic of that kind.
//
// These assert the RENDERING (where a wrong format stops matching fsc at all) and the
// handful of mappings that were read out of the F# compiler sources — `FSComp.txt`'s
// numbered entries and `CompilerDiagnostics.fs`'s `DiagnosticNumber`. They do not restate
// the whole table: the exhaustive `match` is its own record, and a test that copied it would
// just be a second place to edit.

let private codeOf (k: Kind) : DiagCode = (Diagnostic.nowhere k).Code

[<Tests>]
let tests =
    testList
        "DiagnosticCode"
        [
            // fsc writes its numbers zero-padded to four digits. `FS5` would name nothing.
            test "an fsc number renders the way fsc writes it" {
                Expect.equal (DiagCode.render (DiagCode.FSharp 5)) "FS0005" "padded to four digits"
                Expect.equal (DiagCode.render (DiagCode.FSharp 39)) "FS0039" "padded to four digits"
                Expect.equal (DiagCode.render (DiagCode.FSharp 954)) "FS0954" "padded to four digits"
                Expect.equal (DiagCode.render (DiagCode.FSharp 3217)) "FS3217" "already four digits"
            }

            test "this compiler's own families render verbatim" {
                Expect.equal (DiagCode.render (DiagCode.Vesper "V240")) "V240" "conformance family"
                Expect.equal (DiagCode.render (DiagCode.Vesper "LEX")) "LEX" "front-end refusal"
            }

            // The empty string is a DISPLAY choice for "no published number", decided here.
            // It is deliberately not a value a producer or consumer ever holds — that is the
            // whole point of `Unpublished` being a case.
            test "an unpublished code renders empty" {
                Expect.equal (DiagCode.render DiagCode.Unpublished) "" "nothing to print"
            }

            // Each of these was read out of the F# compiler rather than chosen. The comment
            // names the exception or resource it came from, so a disagreement is checkable.
            test "the verdicts fsc also files are filed under fsc's number" {
                let expected =
                    [
                        // `UndefinedName` — fsc files the whole unresolved-name family here.
                        Kind.UndefinedType "Nope", DiagCode.FSharp 39
                        Kind.NoMember("Widget", MemberNoun.Field, "nope"), DiagCode.FSharp 39
                        Kind.NoCase(CaseOwner.Union, "Shape", "Blob"), DiagCode.FSharp 39
                        Kind.UnknownNominalType(NominalKind.Record, "R"), DiagCode.FSharp 39
                        Kind.UnresolvedQualifiedName "A.B.c", DiagCode.FSharp 39
                        // `TyconBadArgs`
                        Kind.TypeArgArity("Map", 2, 1), DiagCode.FSharp 33
                        // `UnionCaseWrongArguments`
                        Kind.ConstructorArity("Some", 1, 2), DiagCode.FSharp 19
                        Kind.NullaryConstructorPattern("Some", 1), DiagCode.FSharp 19
                        // `FieldNotMutable`
                        Kind.ImmutableFieldAssignment "X", DiagCode.FSharp 5
                        // `tcInvalidEnumerationLiteral`
                        Kind.EnumCaseNotConstant, DiagCode.FSharp 886
                        // `ErrorFromAddingTypeEquation` — a measure reconciles through the
                        // type equation, which is where fsc reports it failing.
                        Kind.MeasureMismatch("m", "s"), DiagCode.FSharp 1
                        Kind.DimensionlessMeasureMismatch "kg", DiagCode.FSharp 1
                        // `InvalidRuntimeCoercion`
                        Kind.DowncastUnrelated("int", "string"), DiagCode.FSharp 7
                        // `TypeTestUnnecessary` — one number for both "tells you nothing"
                        // warnings, as in fsc.
                        Kind.RedundantDowncast "int", DiagCode.FSharp 67
                        Kind.UnrelatedTypeTest("int", "string"), DiagCode.FSharp 67
                        // `MatchIncomplete`
                        Kind.IncompleteAnonUnionMatch [ "a" ], DiagCode.FSharp 25
                        // `tcTypeDefinitionIsCyclicThroughInheritance` — fsc's message for
                        // 954 names a struct field or inheritance relation, which is this
                        // case. The inheritance WALK's own finding has no fsc counterpart.
                        Kind.CyclicType("A", TypeCycle.Immediate), DiagCode.FSharp 954
                        Kind.CyclicType("A", TypeCycle.Inheritance), DiagCode.Unpublished
                    ]

                for kind, code in expected do
                    Expect.equal (codeOf kind) code (sprintf "%A" kind)
            }

            // The parser publishes its own vocabulary and this compiler forwards it whole
            // rather than renumbering it, so a parse verdict is never an `FS` number.
            test "a forwarded parse verdict keeps the parser's own code" {
                Expect.equal
                    (codeOf (Kind.Parse DiagnosticCode.MissingExpression))
                    (DiagCode.Vesper "MissingExpression")
                    "forwarded, not renumbered"
            }

            // `Kind.Message` is the un-migrated tail: a sentence built at a call site, which
            // by construction has no classification to publish a number for.
            test "an un-migrated message publishes no code" {
                Expect.equal (codeOf (Kind.Message "anything")) DiagCode.Unpublished "nothing to publish"
            }

            // An internal break is not a verdict about the program, so it must never wear an
            // fsc number — that would send a user to look up a rule they have not broken —
            // and it must say whose fault it is.
            test "an internal break publishes no code and announces itself as one" {
                let d =
                    Diagnostic.nowhere (Kind.Internal(InternalBreak.UnflattenedModule "Validation"))

                Expect.equal d.Code DiagCode.Unpublished "nothing for a user to look up"
                Expect.stringStarts d.Message "internal compiler error:" "says whose fault it is"
                Expect.equal d.Severity Severity.Error "still blocks emission"
            }
        ]
