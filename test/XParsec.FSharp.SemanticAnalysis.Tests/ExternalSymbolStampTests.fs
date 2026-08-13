module XParsec.FSharp.SemanticAnalysis.Tests.ExternalSymbolStampTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

/// The bare `op_*` spellings resolve unqualified here, as the real prelude auto-opens
/// `Vesper.Core`'s operator module. A qualified operator compiles to `A.B.op_Addition`.
let private provider: IExternalSymbolProvider =
    let mono name =
        ValueSome(
            ExternalSymbols.monoFrozen (SymbolKeyOps.inNamespace "") name (FTConst(RuntimeNames.intKey, EqArray.empty))
        )

    ExternalSymbolProviders.ofNamedChannels
        { ExternalSymbolProviders.NamedChannels.empty with
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
            test "dotted external value ref is stamped" {
                let ctx, file = analyse "let x = A.B.thing"
                let e = firstBindingExpr file
                Expect.isTrue (isStamped ctx e) "A.B.thing value symbol stamped"
            }

            test "bare operator-as-value is stamped" {
                let ctx, file = analyse "let f = (+)"
                let e = firstBindingExpr file
                Expect.isTrue (isStamped ctx e) "(+) operator-value symbol stamped"
            }

            test "qualified operator-as-value is stamped" {
                let ctx, file = analyse "let h = A.B.(+)"
                let e = firstBindingExpr file
                Expect.isTrue (isStamped ctx e) "A.B.(+) operator-value symbol stamped"
            }

            test "infix operator application is stamped" {
                let ctx, file = analyse "let g = 1 + 2"

                match firstBindingExpr file with
                | Expr.InfixApp _ as e -> Expect.isTrue (isStamped ctx e) "1 + 2 operator symbol stamped"
                | other -> failwithf "expected Expr.InfixApp, got %A" other
            }

            test "dynamic-access operator is stamped" {
                let ctx, file = analyse "let d = fun x -> x?foo"

                match firstBindingExpr file with
                | Expr.Fun(expr = (Expr.DynamicLookup _ as body)) ->
                    Expect.isTrue (isStamped ctx body) "x?foo op_Dynamic symbol stamped"
                | other -> failwithf "expected a lambda over Expr.DynamicLookup, got %A" other
            }

            // `x?foo <- 1` stamps on the enclosing `Assignment`, not the inner lookup.
            test "dynamic-set operator is stamped" {
                let ctx, file = analyse "let s = fun x -> x?foo <- 1"

                match firstBindingExpr file with
                | Expr.Fun(expr = (Expr.Assignment _ as body)) ->
                    Expect.isTrue (isStamped ctx body) "x?foo <- 1 op_DynamicAssignment symbol stamped"
                | other -> failwithf "expected a lambda over Expr.Assignment, got %A" other
            }

            // The fixture surfaces no `op_Multiply`, so `1 * 2` finds nothing to stamp.
            test "unknown infix operator is not stamped" {
                let ctx, file = analyse "let m = 1 * 2"

                match firstBindingExpr file with
                | Expr.InfixApp _ as e -> Expect.isFalse (isStamped ctx e) "unknown op_Multiply is not stamped"
                | other -> failwithf "expected Expr.InfixApp, got %A" other
            }

            test "unknown value ref is not stamped" {
                let ctx, file = analyse "let u = A.B.nope"
                let e = firstBindingExpr file
                Expect.isFalse (isStamped ctx e) "unknown value is not stamped"
            }
        ]
