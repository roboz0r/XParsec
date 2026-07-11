module XParsec.FSharp.SemanticAnalysis.Tests.ExternalTypeKeyStampTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// NameResolution — the one resolve-once layer — resolves an expression-position
// external TYPE identity (opens-aware, longest-type-prefix) ONCE and stamps its
// `SymbolKey` in `Resolution.ResolvedType`, keyed by the head expr's `NodeKey`.
// Unification's `tryExternalTypeReceiver` / `splitExternalClassPrefix` /
// `tryInferExternalCtorApp` READ that stamp and do a key-addressed store-face
// member/ctor lookup instead of re-running `OpenScope.tryQualify` + a string
// provider lookup at inference time. A MISSED stamp is a resolution failure (the
// consumer no longer re-resolves), so these tests assert the stamp is present at
// the three representative expression positions: a static member on a named type,
// an external ctor-sugar head, and a generic external-type static receiver.

/// A provider that knows two external classes in namespace `Tests` (auto-opened via
/// `AmbientOpenPrefixes`, as the real prelude opens the package namespace): a
/// non-generic `Tests.Widget` and a generic `Tests.Box`1`. `ofNamedLeaf` derives the
/// store face, so the consumers that confirm a stamped key's shape by key
/// (`inferNew`'s Class check) resolve against the same table.
let private provider: IExternalSymbolProvider =
    ExternalSymbolProviders.ofNamedLeaf
        { ExternalSymbolProviders.NamedLeaf.empty with
            TryLookupType =
                fun n ->
                    match n with
                    | "Tests.Widget" ->
                        ValueSome(ExternalTypeShape.Class(ExternalClassShape.basic (0, false, SymbolOrigin.Empty)))
                    | "Tests.Box`1" ->
                        ValueSome(ExternalTypeShape.Class(ExternalClassShape.basic (1, false, SymbolOrigin.Empty)))
                    | _ -> ValueNone
            AmbientOpenPrefixes = [ "Tests" ]
        }

let private analyse (input: string) = analyseNameRes provider input

let private isStamped (ctx: PassContext) (e: Expr<SyntaxToken>) : bool =
    ctx.Resolution.ResolvedType.ContainsKey(CstKeys.ofExpr e)

let private isStaticReceiver (ctx: PassContext) (e: Expr<SyntaxToken>) : bool =
    ctx.Resolution.ExternalStaticReceiver.ContainsKey(CstKeys.ofExpr e)

[<Tests>]
let tests =
    testList
        "ExternalTypeKeyStamp"
        [
            // A folded static-member LongIdent: the receiver PREFIX (`Widget`) is
            // stamped in the DEDICATED receiver table (not `ResolvedType`, so a ctor-app
            // consumer never mistakes it for a constructible head);
            // `splitExternalClassPrefix` reads it and looks the member up by key.
            test "static-member receiver prefix is stamped" {
                let ctx, file = analyse "let x = Widget.Make"
                let e = firstBindingExpr file
                Expect.isTrue (isStaticReceiver ctx e) "Widget.Make receiver prefix key stamped"
                Expect.isFalse (isStamped ctx e) "a static-member node is NOT a whole-name ResolvedType stamp"
            }

            // The `new`-less ctor-sugar head (`Widget "a"`): `tryInferExternalCtorApp`
            // reads the head's stamped key to construct by key.
            test "external ctor-sugar head is stamped" {
                let ctx, file = analyse "let f = Widget \"a\""

                let head =
                    match firstBindingExpr file with
                    | Expr.App(funcExpr = fn)
                    | Expr.HighPrecedenceApp(funcExpr = fn) -> fn
                    | other -> failwithf "expected an application head, got %A" other

                Expect.isTrue (isStamped ctx head) "Widget ctor-sugar head type key stamped"
            }

            // A generic external-type static receiver (`Box<int>.Empty`): NameResolution's
            // `Expr.TypeApp` visit stamps the receiver head at its exact arity;
            // `tryExternalTypeReceiver` reads it.
            test "generic static receiver head is stamped" {
                let ctx, file = analyse "let e = Box<int>.Empty"

                let head =
                    match firstBindingExpr file with
                    | Expr.DotLookup(expr = Expr.TypeApp(expr = h)) -> h
                    | other -> failwithf "expected a DotLookup on a TypeApp, got %A" other

                Expect.isTrue (isStamped ctx head) "Box<int> receiver head type key stamped"
            }

            // The `new T(…)` head: the written type `t` is a `Type` node, so its head
            // carries the general `ResolvedTypeHead` stamp (written by
            // `stampExprEmbeddedTypes`' `Expr.New` arm) — no dedicated `new`-head
            // table. `inferNew`'s `TyConst` arm reads the head stamp and confirms the
            // CLASS shape by key instead of re-resolving the written spelling through
            // opens at inference time (the written-platform-class ctor opt-in).
            test "new-head external class is stamped" {
                let ctx, file = analyse "let w = new Widget(\"a\")"

                let headKey =
                    match firstBindingExpr file with
                    | Expr.New(typ = t) -> (CstKeys.ofTypeHead t).Value.Key
                    | other -> failwithf "expected Expr.New, got %A" other

                Expect.isTrue
                    (ctx.Resolution.ResolvedTypeHead.ContainsKey headKey)
                    "new Widget(...) head type key stamped"
            }

            // A `new` head the provider does not know as a class is not stamped —
            // `inferNew`'s `TyConst` arm then falls to the intrinsic constructible-surface
            // path (`new exn "boom"`), never re-resolving a spelling.
            test "unknown new-head is not stamped" {
                let ctx, file = analyse "let w = new Unknown(\"a\")"

                let headKey =
                    match firstBindingExpr file with
                    | Expr.New(typ = t) -> (CstKeys.ofTypeHead t).Value.Key
                    | other -> failwithf "expected Expr.New, got %A" other

                Expect.isFalse (ctx.Resolution.ResolvedTypeHead.ContainsKey headKey) "unknown new head is not stamped"
            }

            // A head the provider does not know is not stamped — the consumer then
            // declines (a resolution failure surfaces, it never re-resolves).
            test "unknown external head is not stamped" {
                let ctx, file = analyse "let x = Unknown.Member"
                let e = firstBindingExpr file
                Expect.isFalse (isStamped ctx e) "unknown head is not a ResolvedType stamp"
                Expect.isFalse (isStaticReceiver ctx e) "unknown head is not a receiver-prefix stamp"
            }

            // THE CLASSIFICATION RULE, pinned. A written name is classified against the
            // external universe ONCE: the first hit in candidate order IS what the name
            // names, and only then is its shape checked. It does NOT keep probing past a
            // hit whose shape a particular consumer happens to dislike.
            //
            // Here `Thing` is a UNION under the higher-priority open and a CLASS under
            // the lower-priority one. Under first-hit-wins the name means the union, so
            // the ctor-sugar class stamp is absent — even though a class of that name is
            // reachable. A shape-FILTERED scan would instead skip the union and stamp the
            // shadowed class, i.e. resolve one spelling to a different entity than the
            // suppression logic saw. That disagreement is the whole thing the
            // resolve-once layer exists to make impossible, so the shadowed class must
            // stay shadowed.
            test "classification commits to the first hit — a shadowed class stays shadowed" {
                let shadowingProvider: IExternalSymbolProvider =
                    ExternalSymbolProviders.ofNamedLeaf
                        { ExternalSymbolProviders.NamedLeaf.empty with
                            TryLookupType =
                                fun n ->
                                    match n with
                                    | "Early.Thing" ->
                                        ValueSome(ExternalTypeShape.Union(0, [||], [||], SymbolOrigin.Empty))
                                    | "Late.Thing" ->
                                        ValueSome(
                                            ExternalTypeShape.Class(
                                                ExternalClassShape.basic (0, false, SymbolOrigin.Empty)
                                            )
                                        )
                                    | _ -> ValueNone
                            // Candidate order is this list's order — `Early` wins.
                            AmbientOpenPrefixes = [ "Early"; "Late" ]
                        }

                let ctx, file = analyseNameRes shadowingProvider "let x = Thing 1"

                let head =
                    match firstBindingExpr file with
                    | Expr.App(funcExpr = f) -> f
                    | other -> failwithf "expected Expr.App, got %A" other

                Expect.isFalse
                    (isStamped ctx head)
                    "`Thing` names the UNION under the winning open — it is not stamped as a constructible class"

                // It still resolves to *something* external, so it is not diagnosed as an
                // unresolved name: the suppression reads the same one hit the stamp did.
                Expect.isFalse
                    (ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "Unresolved"))
                    "the name resolves externally (to the union), so no unresolved-name diagnostic"
            }
        ]
