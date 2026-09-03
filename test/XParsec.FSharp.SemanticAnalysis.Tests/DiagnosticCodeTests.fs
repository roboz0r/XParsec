module XParsec.FSharp.SemanticAnalysis.Tests.DiagnosticCodeTests

open Expecto
// Ahead of the SemanticAnalysis open so the bare `Diagnostic` stays the semantic one; this
// open is here for the parser's `DiagnosticCode`, which `Kind.Parse` wraps.
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// A `DiagCode.FSharp` number claims fsc files the SAME verdict under the SAME number. The
// mappings below were read out of `FSComp.txt`'s numbered entries and
// `CompilerDiagnostics.fs`'s `DiagnosticNumber`, not chosen.

let private codeOf (k: Kind) : DiagCode = (Diagnostic.nowhere k).Code

[<Tests>]
let tests =
    testList
        "DiagnosticCode"
        [
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
            test "an unpublished code renders empty" {
                Expect.equal (DiagCode.render DiagCode.Unpublished) "" "nothing to print"
            }

            test "the verdicts fsc also files are filed under fsc's number" {
                let expected =
                    [
                        // `UndefinedName` — fsc files the whole unresolved-name family here.
                        Kind.UndefinedType "Nope", DiagCode.FSharp 39
                        Kind.NoMember("Widget", MemberNoun.Field, "nope"), DiagCode.FSharp 39
                        Kind.NoCase(CaseOwner.Union, "Shape", "Blob"), DiagCode.FSharp 39
                        Kind.UnknownNominalType(NominalKind.Record, "R"), DiagCode.FSharp 39
                        Kind.UnresolvedQualifiedName "A.B.c", DiagCode.FSharp 39
                        // `tcModuleAbbreviationForNamespace`
                        Kind.AbbreviatedNamespace "System.Collections", DiagCode.FSharp 965
                        // `DuplicateModuleSpecification`
                        Kind.DuplicateModule "Test.A.Dup", DiagCode.FSharp 248
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
                        // `tcTypeDefinitionIsCyclicThroughInheritance` — one number for both
                        // relations, as in fsc: `type A() = inherit A()`, the mutual `and`
                        // form, `[<Struct>] type A = { x: A }` and the mutual struct pair all
                        // report 954 under `dotnet fsi`.
                        Kind.CyclicType("A", TypeCycle.StructField), DiagCode.FSharp 954
                        Kind.CyclicType("A", TypeCycle.Inheritance), DiagCode.FSharp 954
                        // `tcTypeDefinitionIsCyclic` — the abbreviation route is fsc's other
                        // number (`type A = B and B = A` reports 953).
                        Kind.CyclicType("A", TypeCycle.Abbreviation), DiagCode.FSharp 953
                        // `tcFieldNameIsUsedModeThanOnce` and
                        // `tcFieldNameConflictsWithGeneratedNameForAnonymousField` — fsc
                        // files both under 3176.
                        Kind.UnionCaseFieldNameClash("a", UnionFieldNameClash.Declared), DiagCode.FSharp 3176
                        Kind.UnionCaseFieldNameClash("Item2", UnionFieldNameClash.AnonymousSpelling),
                        DiagCode.FSharp 3176
                    ]

                for kind, code in expected do
                    Expect.equal (codeOf kind) code (sprintf "%A" kind)
            }

            test "a forwarded parse verdict keeps the parser's own code" {
                Expect.equal
                    (codeOf (Kind.Parse DiagnosticCode.MissingExpression))
                    (DiagCode.Parse "MissingExpression")
                    "forwarded, not renumbered"
            }

            // `Kind.Message` carries a sentence built at a call site — no classification to
            // publish a number for.
            test "an un-migrated message publishes no code" {
                Expect.equal (codeOf (Kind.Message "anything")) DiagCode.Unpublished "nothing to publish"
            }

            // An internal break is not a verdict about the program: an fsc number would send
            // a user to look up a rule they have not broken.
            test "an internal break publishes no code and announces itself as one" {
                let d =
                    Diagnostic.nowhere (Kind.Internal(InternalBreak.UnflattenedModule "Validation"))

                Expect.equal d.Code DiagCode.Unpublished "nothing for a user to look up"
                Expect.stringStarts d.Message "internal compiler error:" "says whose fault it is"
                Expect.equal d.Severity Severity.Error "still blocks emission"
            }
        ]
