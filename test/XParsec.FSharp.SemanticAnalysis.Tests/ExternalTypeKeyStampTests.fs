module XParsec.FSharp.SemanticAnalysis.Tests.ExternalTypeKeyStampTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private widgetKey = SymbolKeyOps.qualifiedTypeKeyOf "Tests.Widget" 0
let private boxKey = SymbolKeyOps.qualifiedTypeKeyOf "Tests.Box" 1

/// `Tests` is ambient because the real prelude auto-opens the package namespace.
let private provider: IExternalSymbolProvider =
    providerOfSurface (fun b ->
        publishClass
            b
            widgetKey
            [
                mkStaticProperty widgetKey "Make" (FTConst(RuntimeNames.intKey, EqArray.empty))
            ]

        // `Box<'T>.Empty : Box<'T>` — the static's own type names the declaring typar.
        publishClass
            b
            boxKey
            [
                mkStaticProperty boxKey "Empty" (FTClass(boxKey, EqArray.ofList [ FTTypar(TyparAxis.Declaring, 0) ]))
            ]

        b.AmbientOpenPrefixes <- [ "Tests" ]
    )

let private analyse (input: string) = analyseNameRes provider input

let private isStamped (ctx: PassContext) (e: Expr<SyntaxToken>) : bool =
    ctx.Resolution.ResolvedType.ContainsKey(CstKeys.ofExpr e)

let private isExternalTypeName (ctx: PassContext) (nameKey: NodeKey) : bool =
    match ctx.Resolution.TypeRefVerdicts.TryGetValue nameKey with
    | ValueSome(TypeRefVerdict.ExternalType _) -> true
    | _ -> false

let private isStaticQualifier (ctx: PassContext) (e: Expr<SyntaxToken>) : bool =
    (ResolvedStamps.tryStaticQualifier ctx.Resolution.Resolved (CstKeys.ofExpr e)).IsSome

[<Tests>]
let tests =
    testList
        "ExternalTypeKeyStamp"
        [
            test "static-member qualifier prefix is stamped" {
                let ctx, file = analyse "let x = Widget.Make"
                let e = firstBindingExpr file
                Expect.isTrue (isStaticQualifier ctx e) "Widget.Make qualifier prefix key stamped"
                Expect.isFalse (isStamped ctx e) "a static-member node is NOT a whole-name ResolvedType stamp"
            }

            test "external ctor-sugar application is stamped" {
                let ctx, file = analyse "let f = Widget \"a\""

                let fn =
                    match firstBindingExpr file with
                    | Expr.App(funcExpr = fn)
                    | Expr.HighPrecedenceApp(funcExpr = fn) -> fn
                    | other -> failwithf "expected an application, got %A" other

                Expect.isTrue (isStamped ctx fn) "Widget ctor-sugar type key stamped"
            }

            // Stamped at the written arity: the provider knows `Box` only as ``Tests.Box`1``.
            test "generic static qualifier name is stamped" {
                let ctx, file = analyse "let e = Box<int>.Empty"

                let fn =
                    match firstBindingExpr file with
                    | Expr.DotLookup(expr = Expr.TypeApp(expr = h)) -> h
                    | other -> failwithf "expected a DotLookup on a TypeApp, got %A" other

                Expect.isTrue (isStamped ctx fn) "Box<int> qualifier type key stamped"
            }

            // The type in `new T(…)` is a `Type` node, so it carries the general
            // written-type verdict — hence `TypeRefVerdicts`, not `ResolvedType`.
            test "new-expression external class is stamped" {
                let ctx, file = analyse "let w = new Widget(\"a\")"

                let nameKey =
                    match firstBindingExpr file with
                    | Expr.New(typ = t) -> (CstKeys.ofTypeRef t).Value.Site.Key
                    | other -> failwithf "expected Expr.New, got %A" other

                Expect.isTrue (isExternalTypeName ctx nameKey) "new Widget(...) type key stamped"
            }

            test "unknown new-expression type is not stamped" {
                let ctx, file = analyse "let w = new Unknown(\"a\")"

                let nameKey =
                    match firstBindingExpr file with
                    | Expr.New(typ = t) -> (CstKeys.ofTypeRef t).Value.Site.Key
                    | other -> failwithf "expected Expr.New, got %A" other

                Expect.isFalse (isExternalTypeName ctx nameKey) "unknown new type is not stamped"
            }

            test "unknown external name is not stamped" {
                let ctx, file = analyse "let x = Unknown.Member"
                let e = firstBindingExpr file
                Expect.isFalse (isStamped ctx e) "unknown name is not a ResolvedType stamp"
                Expect.isFalse (isStaticQualifier ctx e) "unknown name is not a qualifier-prefix stamp"
            }

            // The first hit in candidate order IS what the name resolves to; its shape is
            // checked only afterwards. `Thing` is a union under `Early` and a class under
            // `Late`, so it resolves to the union and no constructible-class stamp appears.
            test "classification commits to the first hit — a shadowed class stays shadowed" {
                let shadowingProvider: IExternalSymbolProvider =
                    providerOfSurface (fun b ->
                        publishUnion b (SymbolKeyOps.qualifiedTypeKeyOf "Early.Thing" 0) []
                        publishClass b (SymbolKeyOps.qualifiedTypeKeyOf "Late.Thing" 0) []
                        // Candidate order is this list's order — `Early` wins.
                        b.AmbientOpenPrefixes <- [ "Early"; "Late" ]
                    )

                let ctx, file = analyseNameRes shadowingProvider "let x = Thing 1"

                let fn =
                    match firstBindingExpr file with
                    | Expr.App(funcExpr = f) -> f
                    | other -> failwithf "expected Expr.App, got %A" other

                Expect.isFalse
                    (isStamped ctx fn)
                    "`Thing` names the UNION under the winning open — it is not stamped as a constructible class"

                Expect.isFalse
                    (ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "Unresolved"))
                    "the name resolves externally (to the union), so no unresolved-name diagnostic"
            }
        ]
