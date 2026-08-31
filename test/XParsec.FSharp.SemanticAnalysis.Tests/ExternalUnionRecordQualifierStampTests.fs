module XParsec.FSharp.SemanticAnalysis.Tests.ExternalUnionRecordQualifierStampTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private gadgetKey = SymbolKeyOps.qualifiedTypeKeyOf "Tests.Gadget" 0

/// Union `Tests.Colour`, record `Tests.Widget` and class `Tests.Gadget` sit in the
/// ambient namespace `Tests`; union `Other.Palette` needs an explicit `open Other`.
/// `Gadget.Make` is the one declared member, so every `.Nope` below misses.
let private provider: IExternalSymbolProvider =
    providerOfSurface (fun b ->
        publishUnion b (SymbolKeyOps.qualifiedTypeKeyOf "Tests.Colour" 0) []
        publishUnion b (SymbolKeyOps.qualifiedTypeKeyOf "Other.Palette" 0) []

        publishClass
            b
            gadgetKey
            [
                mkStaticProperty gadgetKey "Make" (FTConst(RuntimeNames.intKey, EqArray.empty))
            ]

        publishRecord b (SymbolKeyOps.qualifiedTypeKeyOf "Tests.Widget" 0) []

        b.ImplicitOpens <- [ SymbolKeyOps.assemblyAutoOpen "Tests" ]
    )

let private analyse (input: string) = analyseNameRes provider input

/// Run every pass so the member-miss diagnostic (raised in Unification) fires.
let private diagnose (input: string) : PassContext =
    let ctx, file = analyseNameRes provider input
    Unification.run ctx file
    ctx

[<Tests>]
let tests =
    testList
        "ExternalUnionRecordQualifier"
        [
            test "union qualifier with an unresolved member name is stamped" {
                let ctx, file = analyse "let x = Colour.Nope"
                let e = firstBindingExpr file

                Expect.isTrue
                    ((ResolvedStamps.tryUnionRecordQualifier ctx.Resolution.Resolved (CstKeys.ofExpr e)).IsSome)
                    "Colour.Nope — union qualifier stamped"
            }

            test "record qualifier with an unresolved member name is stamped" {
                let ctx, file = analyse "let x = Widget.Nope"
                let e = firstBindingExpr file

                Expect.isTrue
                    ((ResolvedStamps.tryUnionRecordQualifier ctx.Resolution.Resolved (CstKeys.ofExpr e)).IsSome)
                    "Widget.Nope — record qualifier stamped"
            }

            // NameResolution reports a class qualifier's miss itself; this stamp is the
            // union/record channel Unification reports through.
            test "class qualifier is not stamped" {
                let ctx, file = analyse "let x = Gadget.Nope"
                let e = firstBindingExpr file

                Expect.isFalse
                    ((ResolvedStamps.tryUnionRecordQualifier ctx.Resolution.Resolved (CstKeys.ofExpr e)).IsSome)
                    "Gadget.Nope — class qualifier not stamped"
            }

            test "unknown qualifier is not stamped" {
                let ctx, file = analyse "let x = Unknown.Nope"
                let e = firstBindingExpr file

                Expect.isFalse
                    ((ResolvedStamps.tryUnionRecordQualifier ctx.Resolution.Resolved (CstKeys.ofExpr e)).IsSome)
                    "Unknown.Nope — unknown qualifier not stamped"
            }

            test "union qualifier whose namespace is not opened is not stamped" {
                let ctx, file = analyse "let x = Palette.Nope"
                let e = firstBindingExpr file

                Expect.isFalse
                    ((ResolvedStamps.tryUnionRecordQualifier ctx.Resolution.Resolved (CstKeys.ofExpr e)).IsSome)
                    "Palette.Nope — namespace Other unopened, not stamped"
            }

            test "union qualifier is stamped once its namespace is opened" {
                let ctx, file = analyse "open Other\nlet x = Palette.Nope"
                let e = firstBindingExpr file

                Expect.isTrue
                    ((ResolvedStamps.tryUnionRecordQualifier ctx.Resolution.Resolved (CstKeys.ofExpr e)).IsSome)
                    "Palette.Nope — stamped under open Other"
            }

            test "an unresolved member name on a union qualifier raises the member-miss diagnostic" {
                let ctx = diagnose "let x = Colour.Nope"

                Expect.isTrue
                    (ctx.Diagnostics
                     |> Seq.exists (fun d -> d.Message.Contains "has no value or member 'Nope'"))
                    "Colour.Nope diagnosed as a missing member"
            }

            // The shape carries the class's full member list, so a name absent from it is a
            // miss, as `dotnet fsi` reports it.
            test "an unresolved member name on a class qualifier raises the member-miss diagnostic" {
                let ctx = diagnose "let x = Gadget.Nope"

                Expect.isTrue
                    (ctx.Diagnostics
                     |> Seq.exists (fun d -> d.Message.Contains "has no value or member 'Nope'"))
                    "Gadget.Nope diagnosed as a missing member"
            }

            // `Make` is a static PROPERTY, so `Gadget.Make` is the value itself and the
            // binding type-checks outright.
            test "a declared static member on a class qualifier resolves cleanly" {
                let ctx = diagnose "let x = Gadget.Make"

                Expect.isEmpty
                    (ctx.Diagnostics |> Diagnostic.errors)
                    "Gadget.Make is declared, so the whole binding checks"
            }
        ]
