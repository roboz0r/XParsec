module XParsec.FSharp.SemanticAnalysis.Tests.ExternalSymbolStampTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// NameResolution — the one resolve-once layer — resolves an external value /
// operator SPELLING (opens-aware) ONCE and stamps its full `ExternalSymbol` in
// `Resolution.ExternalSymbolStamp`, keyed by the use-site's `NodeKey`.
// Unification's `InferIdentExpr` (value / `(+)`-value arms) and `InferApp`
// (`inferInfix` / `inferPrefix` / `inferDynamic*`) READ that stamp and call
// `instantiateSymbol` on it, instead of re-running `OpenScope.tryResolve` +
// `TryLookup(string)` at inference time. A MISSED stamp is a resolution failure
// (the consumer no longer re-resolves), so these tests assert the stamp is
// present at each representative value/operator position — and absent for a
// spelling the provider does not know.

/// A provider that knows one dotted *value* (`A.B.thing`), the qualified
/// operator `A.B.(+)` (compiled `A.B.op_Addition`), and the bare intrinsic
/// operators `op_Addition` / `op_Dynamic` / `op_DynamicAssignment` (ambient, as
/// the real prelude auto-opens `Vesper.Core`'s operator module).
let private provider: IExternalSymbolProvider =
    let mono name =
        ValueSome(ExternalSymbols.monoFrozen name (FTConst(RuntimeNames.intKey, EqArray.empty)))

    ExternalSymbolProviders.ofNamedLeaf
        { ExternalSymbolProviders.NamedLeaf.empty with
            TryLookup =
                fun n ->
                    match n with
                    | "A.B.thing" -> mono "thing"
                    | "A.B.op_Addition" -> mono "op_Addition"
                    | "op_Addition" -> mono "op_Addition"
                    | "op_Dynamic" -> mono "op_Dynamic"
                    | "op_DynamicAssignment" -> mono "op_DynamicAssignment"
                    | _ -> ValueNone
        }

let private analyse (input: string) = analyseNameRes provider input

let private isStamped (ctx: PassContext) (e: Expr<SyntaxToken>) : bool =
    ctx.Resolution.ExternalSymbolStamp.ContainsKey(CstKeys.ofExpr e)

[<Tests>]
let tests =
    testList
        "ExternalSymbolStamp"
        [
            // A dotted external value ref (`A.B.thing`): `inferIdentDefault` reads the
            // stamp and instantiates the scheme by key.
            test "dotted external value ref is stamped" {
                let ctx, file = analyse "let x = A.B.thing"
                let e = firstBindingExpr file
                Expect.isTrue (isStamped ctx e) "A.B.thing value symbol stamped"
            }

            // `(+)` used as a value: `inferIdent`'s operator-value arm reads the stamp.
            test "bare operator-as-value is stamped" {
                let ctx, file = analyse "let f = (+)"
                let e = firstBindingExpr file
                Expect.isTrue (isStamped ctx e) "(+) operator-value symbol stamped"
            }

            // `A.B.(+)` — the qualified operator value form.
            test "qualified operator-as-value is stamped" {
                let ctx, file = analyse "let h = A.B.(+)"
                let e = firstBindingExpr file
                Expect.isTrue (isStamped ctx e) "A.B.(+) operator-value symbol stamped"
            }

            // A binary operator application (`1 + 2`): `inferInfix` reads the stamp for
            // both the scheme (`instantiateSymbol`) and its `sym.Key` (→ `IntrinsicKey`).
            test "infix operator application is stamped" {
                let ctx, file = analyse "let g = 1 + 2"

                match firstBindingExpr file with
                | Expr.InfixApp _ as e -> Expect.isTrue (isStamped ctx e) "1 + 2 operator symbol stamped"
                | other -> failwithf "expected Expr.InfixApp, got %A" other
            }

            // A dynamic member access (`x?foo`): `inferDynamicLookup` reads the
            // `op_Dynamic` stamp on the `DynamicLookup` node.
            test "dynamic-access operator is stamped" {
                let ctx, file = analyse "let d = fun x -> x?foo"

                match firstBindingExpr file with
                | Expr.Fun(expr = (Expr.DynamicLookup _ as body)) ->
                    Expect.isTrue (isStamped ctx body) "x?foo op_Dynamic symbol stamped"
                | other -> failwithf "expected a lambda over Expr.DynamicLookup, got %A" other
            }

            // A dynamic set (`x?foo <- 1`): `inferDynamicSet` reads the
            // `op_DynamicAssignment` stamp on the enclosing `Assignment` node.
            test "dynamic-set operator is stamped" {
                let ctx, file = analyse "let s = fun x -> x?foo <- 1"

                match firstBindingExpr file with
                | Expr.Fun(expr = (Expr.Assignment _ as body)) ->
                    Expect.isTrue (isStamped ctx body) "x?foo <- 1 op_DynamicAssignment symbol stamped"
                | other -> failwithf "expected a lambda over Expr.Assignment, got %A" other
            }

            // An operator the provider does not surface (`op_Multiply`) is not stamped —
            // `inferInfix` then reports "No definition for '*' found", never re-resolving.
            test "unknown infix operator is not stamped" {
                let ctx, file = analyse "let m = 1 * 2"

                match firstBindingExpr file with
                | Expr.InfixApp _ as e -> Expect.isFalse (isStamped ctx e) "unknown op_Multiply is not stamped"
                | other -> failwithf "expected Expr.InfixApp, got %A" other
            }

            // A value the provider does not know is not stamped — `inferIdentDefault`
            // falls to its ctor / static / error path exactly as the provider miss did.
            test "unknown value ref is not stamped" {
                let ctx, file = analyse "let u = A.B.nope"
                let e = firstBindingExpr file
                Expect.isFalse (isStamped ctx e) "unknown value is not stamped"
            }
        ]
