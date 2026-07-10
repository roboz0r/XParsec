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
/// non-generic `Tests.Widget` and a generic `Tests.Box`1`. Only the resolver-face
/// `TryLookupType(string)` is exercised (NameResolution stamps on type resolution);
/// the store face is unused by these front-end stamp assertions.
let private provider: IExternalSymbolProvider =
    { new IExternalSymbolProvider

      interface IExternalSymbolResolver with
          member _.TryLookup _ = ValueNone

          member _.TryLookupType(n: string) =
              match n with
              | "Tests.Widget" ->
                  ValueSome(ExternalTypeShape.Class(ExternalClassShape.basic (0, false, SymbolOrigin.Empty)))
              | "Tests.Box`1" ->
                  ValueSome(ExternalTypeShape.Class(ExternalClassShape.basic (1, false, SymbolOrigin.Empty)))
              | _ -> ValueNone

          member _.TryLookupUnionCase _ = ValueNone
          member _.AmbientOpenPrefixes = [ "Tests" ]
      interface IExternalSymbolStore with
          member _.TryLookupType(_: SymbolKey) = ValueNone
          member _.TryLookupMember(_, _) = ValueNone
          member _.TryLookupMembers(_, _) = [||]
          member _.TryLookupIndexSignature _ = []
          member _.TryLookupInlineBody _ = ValueNone
          member _.IntrinsicReverseCanon = Map.empty
          member _.IntrinsicForwardRepr = ExternalSymbols.emptyForwardRepr
    }

let private analyse (input: string) : PassContext * ImplementationFile<SyntaxToken> =
    let lexed, file = parseFile input
    let ctx = PassContext(provider, input, lexed)
    Desugar.run ctx file
    NameResolution.run ctx file
    ctx, file

/// The RHS expression of the file's first `let` binding.
let private firstBindingExpr (file: ImplementationFile<SyntaxToken>) : Expr<SyntaxToken> =
    CstWalk.implFileElems file
    |> Seq.pick (fun m ->
        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) when bindings.Length > 0 ->
            Some bindings.[0].expr
        | _ -> None
    )

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

            // The `new T(…)` head: NameResolution's dedicated `new`-head resolve stamps
            // the written external CLASS on the `Expr.New` node so `inferNew` reads the
            // ctor catalogue by key instead of re-resolving the written spelling through
            // opens at inference time (the written-platform-class ctor opt-in).
            test "new-head external class is stamped" {
                let ctx, file = analyse "let w = new Widget(\"a\")"

                match firstBindingExpr file with
                | Expr.New _ as e -> Expect.isTrue (isStamped ctx e) "new Widget(...) head type key stamped"
                | other -> failwithf "expected Expr.New, got %A" other
            }

            // A `new` head the provider does not know as a class is not stamped —
            // `inferNew`'s `TyConst` arm then falls to the intrinsic constructible-surface
            // path (`new exn "boom"`), never re-resolving a spelling.
            test "unknown new-head is not stamped" {
                let ctx, file = analyse "let w = new Unknown(\"a\")"

                match firstBindingExpr file with
                | Expr.New _ as e -> Expect.isFalse (isStamped ctx e) "unknown new head is not stamped"
                | other -> failwithf "expected Expr.New, got %A" other
            }

            // A head the provider does not know is not stamped — the consumer then
            // declines (a resolution failure surfaces, it never re-resolves).
            test "unknown external head is not stamped" {
                let ctx, file = analyse "let x = Unknown.Member"
                let e = firstBindingExpr file
                Expect.isFalse (isStamped ctx e) "unknown head is not a ResolvedType stamp"
                Expect.isFalse (isStaticReceiver ctx e) "unknown head is not a receiver-prefix stamp"
            }
        ]
