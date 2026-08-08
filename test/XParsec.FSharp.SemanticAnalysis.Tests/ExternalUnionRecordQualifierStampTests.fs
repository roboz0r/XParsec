module XParsec.FSharp.SemanticAnalysis.Tests.ExternalUnionRecordQualifierStampTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

/// Union `Tests.Colour`, record `Tests.Widget` and class `Tests.Gadget` sit in the
/// ambient namespace `Tests`; union `Other.Palette` needs an explicit `open Other`.
/// No union case resolves at all, so every `.Nope` tail below is an unresolved member.
let private provider: IExternalSymbolProvider =
    ExternalSymbolProviders.ofNamedLeaf
        { ExternalSymbolProviders.NamedLeaf.empty with
            TryLookupType =
                fun n ->
                    match n with
                    | "Tests.Colour" -> ValueSome(ExternalTypeShape.Union(0, [||], [||], SymbolOrigin.Empty))
                    | "Other.Palette" -> ValueSome(ExternalTypeShape.Union(0, [||], [||], SymbolOrigin.Empty))
                    | "Tests.Widget" -> ValueSome(ExternalTypeShape.Record(0, [||], SymbolOrigin.Empty))
                    | "Tests.Gadget" ->
                        ValueSome(ExternalTypeShape.Class(ExternalClassShape.basic (0, false, SymbolOrigin.Empty)))
                    | _ -> ValueNone
            AmbientOpenPrefixes = [ "Tests" ]
        }

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
            test "union qualifier with unresolved tail is stamped" {
                let ctx, file = analyse "let x = Colour.Nope"
                let e = firstBindingExpr file

                Expect.isTrue
                    (ctx.Resolution.ExternalUnionRecordQualifier.ContainsKey(CstKeys.ofExpr e))
                    "Colour.Nope — union qualifier stamped"
            }

            test "record qualifier with unresolved tail is stamped" {
                let ctx, file = analyse "let x = Widget.Nope"
                let e = firstBindingExpr file

                Expect.isTrue
                    (ctx.Resolution.ExternalUnionRecordQualifier.ContainsKey(CstKeys.ofExpr e))
                    "Widget.Nope — record qualifier stamped"
            }

            // A class may carry unmodelled static fields, so an unresolved tail on one
            // stays a fresh TyVar rather than becoming a miss.
            test "class qualifier is not stamped" {
                let ctx, file = analyse "let x = Gadget.Nope"
                let e = firstBindingExpr file

                Expect.isFalse
                    (ctx.Resolution.ExternalUnionRecordQualifier.ContainsKey(CstKeys.ofExpr e))
                    "Gadget.Nope — class qualifier not stamped"
            }

            test "unknown qualifier is not stamped" {
                let ctx, file = analyse "let x = Unknown.Nope"
                let e = firstBindingExpr file

                Expect.isFalse
                    (ctx.Resolution.ExternalUnionRecordQualifier.ContainsKey(CstKeys.ofExpr e))
                    "Unknown.Nope — unknown qualifier not stamped"
            }

            test "union qualifier whose namespace is not opened is not stamped" {
                let ctx, file = analyse "let x = Palette.Nope"
                let e = firstBindingExpr file

                Expect.isFalse
                    (ctx.Resolution.ExternalUnionRecordQualifier.ContainsKey(CstKeys.ofExpr e))
                    "Palette.Nope — namespace Other unopened, not stamped"
            }

            test "union qualifier is stamped once its namespace is opened" {
                let ctx, file = analyse "open Other\nlet x = Palette.Nope"
                let e = firstBindingExpr file

                Expect.isTrue
                    (ctx.Resolution.ExternalUnionRecordQualifier.ContainsKey(CstKeys.ofExpr e))
                    "Palette.Nope — stamped under open Other"
            }

            test "unresolved tail on a union qualifier raises the member-miss diagnostic" {
                let ctx = diagnose "let x = Colour.Nope"

                Expect.isTrue
                    (ctx.Diagnostics
                     |> Seq.exists (fun d -> d.Message.Contains "has no value or member 'Nope'"))
                    "Colour.Nope diagnosed as a missing member"
            }

            test "unresolved tail on a class qualifier raises no member-miss diagnostic" {
                let ctx = diagnose "let x = Gadget.Nope"

                Expect.isFalse
                    (ctx.Diagnostics
                     |> Seq.exists (fun d -> d.Message.Contains "has no value or member"))
                    "Gadget.Nope — no member-miss diagnostic for a class qualifier"
            }
        ]
