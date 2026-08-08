module XParsec.FSharp.SemanticAnalysis.Tests.AttributeIdentityTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// An attribute name is RESOLVED as a type and its `TypeKey` compared against the
// `Vesper.Core` marker identities, so these pin what only an identity can decide:
// which declaration a spelling reached. `realProvider` composes the real
// `compiler-attributes.fsi`, so `Vesper.ReferenceEqualityAttribute` is genuinely
// resolvable, bare or qualified, with or without the suffix.

let private analyse (input: string) =
    let lexed, file = parseFile input
    let ctx = PassContext(realProvider.Value, Hashing.originSourceOfText lexed)
    Desugar.run ctx file
    NameResolution.run ctx file
    ctx

let private src (lines: string list) = String.concat "\n" lines

[<Tests>]
let tests =
    testList
        "Attribute identity"
        [
            test "a bare marker resolves through the Attribute suffix rule" {
                let ctx = analyse (src [ "[<ReferenceEquality>]"; "type Point = { X: int }" ])

                Expect.equal
                    (expectRecord ctx "Point").EqualitySupport
                    EqualityVerdict.Reference
                    "[<ReferenceEquality>] reaches Vesper.ReferenceEqualityAttribute"
            }

            test "the marker's own declared name resolves" {
                let ctx =
                    analyse (src [ "[<ReferenceEqualityAttribute>]"; "type Point = { X: int }" ])

                Expect.equal
                    (expectRecord ctx "Point").EqualitySupport
                    EqualityVerdict.Reference
                    "the suffixed spelling names the same type"
            }

            test "a qualified path resolves through the type it names" {
                let ctx =
                    analyse (src [ "[<Vesper.ReferenceEquality>]"; "type Point = { X: int }" ])

                Expect.equal
                    (expectRecord ctx "Point").EqualitySupport
                    EqualityVerdict.Reference
                    "the qualifier is honoured because it resolves"
            }

            test "a qualified path that names no type is blamed for spelling a marker" {
                // The old leaf-segment rule accepted this on the strength of its last
                // segment alone. Nothing declares `Microsoft.FSharp.Core` here, so it
                // decodes to nothing — and silence would ship the very default the author
                // wrote the attribute to refuse.
                let ctx =
                    analyse (src [ "[<Microsoft.FSharp.Core.ReferenceEquality>]"; "type Point = { X: int }" ])

                Expect.equal
                    (expectRecord ctx "Point").EqualitySupport
                    EqualityVerdict.Structural
                    "an unresolved name leaves the record's structural default"

                match ctx.Diagnostics |> Diagnostic.errors |> List.map (fun d -> d.Message) with
                | [ msg ] ->
                    Expect.stringContains
                        msg
                        "Microsoft.FSharp.Core.ReferenceEquality"
                        "the diagnostic quotes the path as written"
                | other -> failtestf "expected exactly one error, got %A" other
            }

            test "an unresolved attribute that spells no marker stays silently ignored" {
                // Most of F#'s attribute vocabulary is declared nowhere in the Vesper
                // contract, so blaming every unresolved name would blame every library file.
                let ctx = analyse (src [ "[<AutoOpen>]"; "type Point = { X: int }" ])

                Expect.isEmpty (ctx.Diagnostics |> Diagnostic.errors) "an undeclared non-marker is not an error"
            }

            test "a same-named user type does NOT take the compiler marker's meaning" {
                // THE case a short-name match cannot decide. `ReferenceEquality` here
                // reaches the user's own class — a local claim beats the contract — so
                // it is their attribute, not Vesper's, and the record keeps its default.
                let ctx =
                    analyse (
                        src
                            [
                                "namespace Mine"
                                ""
                                "type ReferenceEqualityAttribute() ="
                                "    member this.M () = 1"
                                ""
                                "[<ReferenceEquality>]"
                                "type Point = { X: int }"
                            ]
                    )

                Expect.equal
                    (expectRecord ctx "Point").EqualitySupport
                    EqualityVerdict.Structural
                    "the user's ReferenceEqualityAttribute cannot hijack the compiler's"
            }

            test "a same-named user type does not take the comparison marker either" {
                let ctx =
                    analyse (
                        src
                            [
                                "namespace Mine"
                                ""
                                "type StructuralComparisonAttribute() ="
                                "    member this.M () = 1"
                                ""
                                "[<StructuralComparison>]"
                                "type Point = { X: int }"
                            ]
                    )

                Expect.equal
                    (expectRecord ctx "Point").ComparisonSupport
                    ComparisonVerdict.NoComparison
                    "comparison stays opt-in when the marker was never named"
            }

            test "the eq / comp axes still decode independently" {
                let ctx =
                    analyse (src [ "[<Vesper.CustomEquality; NoComparison>]"; "type Point = { X: int }" ])

                let info = expectRecord ctx "Point"
                Expect.equal info.EqualitySupport EqualityVerdict.Custom "qualified CustomEquality"
                Expect.equal info.ComparisonSupport ComparisonVerdict.NoComparison "bare NoComparison"
            }
        ]
