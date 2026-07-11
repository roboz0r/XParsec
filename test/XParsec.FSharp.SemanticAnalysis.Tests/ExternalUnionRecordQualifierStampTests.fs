module XParsec.FSharp.SemanticAnalysis.Tests.ExternalUnionRecordQualifierStampTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// NameResolution — the one resolve-once layer — classifies a ≥2-segment qualified
// reference `Q.member` whose qualifier `Q` names an external UNION or RECORD and
// stamps `Q`'s resolved key under the node's `NodeKey`
// (`Resolution.ExternalUnionRecordQualifier`). Unification's
// `tryQualifiedExternalMemberMiss` READS that stamp (presence) to raise "Type 'Q'
// has no value or member 'm'" for an unresolved tail, instead of re-resolving the
// qualifier through the resolver-face `TryLookupType(string)` at inference time. A
// CLASS qualifier is NOT stamped: its unmodelled-static silence stays a fresh TyVar.

/// A provider that knows a union `Tests.Colour` and a record `Tests.Widget` (whose
/// namespace `Tests` is AUTO-OPENED), a class `Tests.Gadget`, and a union
/// `Other.Palette` whose namespace `Other` is NOT ambient (short name `Palette`
/// resolves only under an explicit `open Other`). No union case ever resolves, so a
/// qualified tail is always an unresolved member.
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
            // A union qualifier with an unresolved tail: `Colour.Nope` (head `Colour`
            // qualifies to the auto-opened `Tests.Colour`) stamps the qualifier key so
            // Unification diagnoses the member miss.
            test "union qualifier with unresolved tail is stamped" {
                let ctx, file = analyse "let x = Colour.Nope"
                let e = firstBindingExpr file

                Expect.isTrue
                    (ctx.Resolution.ExternalUnionRecordQualifier.ContainsKey(CstKeys.ofExpr e))
                    "Colour.Nope — union qualifier stamped"
            }

            // A record qualifier is stamped identically (records also expose no static
            // fields).
            test "record qualifier with unresolved tail is stamped" {
                let ctx, file = analyse "let x = Widget.Nope"
                let e = firstBindingExpr file

                Expect.isTrue
                    (ctx.Resolution.ExternalUnionRecordQualifier.ContainsKey(CstKeys.ofExpr e))
                    "Widget.Nope — record qualifier stamped"
            }

            // A CLASS qualifier is NOT stamped: a class may carry unmodelled static
            // fields, so its unresolved tail stays a fresh TyVar (silent), not a miss.
            test "class qualifier is not stamped" {
                let ctx, file = analyse "let x = Gadget.Nope"
                let e = firstBindingExpr file

                Expect.isFalse
                    (ctx.Resolution.ExternalUnionRecordQualifier.ContainsKey(CstKeys.ofExpr e))
                    "Gadget.Nope — class qualifier not stamped"
            }

            // An unknown qualifier resolves to no external type — not stamped.
            test "unknown qualifier is not stamped" {
                let ctx, file = analyse "let x = Unknown.Nope"
                let e = firstBindingExpr file

                Expect.isFalse
                    (ctx.Resolution.ExternalUnionRecordQualifier.ContainsKey(CstKeys.ofExpr e))
                    "Unknown.Nope — unknown qualifier not stamped"
            }

            // The opens gate: `Other.Palette`'s namespace `Other` is not auto-opened, so
            // the short qualifier `Palette` does not resolve — `Palette.Nope` unstamped.
            test "union qualifier whose namespace is not opened is not stamped" {
                let ctx, file = analyse "let x = Palette.Nope"
                let e = firstBindingExpr file

                Expect.isFalse
                    (ctx.Resolution.ExternalUnionRecordQualifier.ContainsKey(CstKeys.ofExpr e))
                    "Palette.Nope — namespace Other unopened, not stamped"
            }

            // Positive: the SAME access stamps once its namespace is explicitly opened.
            test "union qualifier is stamped once its namespace is opened" {
                let ctx, file = analyse "open Other\nlet x = Palette.Nope"
                let e = firstBindingExpr file

                Expect.isTrue
                    (ctx.Resolution.ExternalUnionRecordQualifier.ContainsKey(CstKeys.ofExpr e))
                    "Palette.Nope — stamped under open Other"
            }

            // End-to-end: the stamp drives Unification's member-miss diagnostic.
            test "unresolved tail on a union qualifier raises the member-miss diagnostic" {
                let ctx = diagnose "let x = Colour.Nope"

                Expect.isTrue
                    (ctx.Diagnostics
                     |> Seq.exists (fun d -> d.Message.Contains "has no value or member 'Nope'"))
                    "Colour.Nope diagnosed as a missing member"
            }

            // The class-qualifier miss is silent (no member-miss diagnostic) — a class's
            // unmodelled static field is not a resolution error.
            test "unresolved tail on a class qualifier raises no member-miss diagnostic" {
                let ctx = diagnose "let x = Gadget.Nope"

                Expect.isFalse
                    (ctx.Diagnostics
                     |> Seq.exists (fun d -> d.Message.Contains "has no value or member"))
                    "Gadget.Nope — no member-miss diagnostic for a class qualifier"
            }
        ]
