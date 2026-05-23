module XParsec.FSharp.Codegen.Clr.Tests.Slice3Tests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Thin-slice #3:
//
//   let inline succ x = x + 1
//   printfn "%d" (succ 41)
//
// `inline` needs no function value at runtime: the retained body is expanded
// into the call site and beta-reduced against the args, lowering to a local
// slot + the `add` intrinsic — only slice-2 mechanics. The work is entirely
// in the TAST walker (`Emit`) plus `Inline.freshen`; no IL / metadata / provider
// change. NodeKey freshening is what makes nested call sites (`succ (succ x)`)
// each get their own slot instead of clobbering one.

[<Tests>]
let tests =
    testList
        "Slice3"
        [
            // ---- Milestone 2: the expression-level `Let` lowering ----

            test "`printfn \"%d\" (let y = 41 in y + 1)` prints 42 (Let, no inline machinery)" {
                let _, artifact = compileSource "Slice3Let" "printfn \"%d\" (let y = 41 in y + 1)"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "42" "the inner let-bound y is reloaded and incremented"
            }

            // ---- Milestone 3: inline expansion at the call site ----

            test "the inline sample analyses clean and splits into an inline Let + a Var call site" {
                let tast = analyse "let inline succ x = x + 1\nprintfn \"%d\" (succ 41)"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls with
                | [ TDecl.Let(TPat.NamedSimple(kSucc, _), TExpr.Lambda _, true, TyFun(TyConst "int", TyConst "int"))
                    TDecl.Expression(TExpr.Format(FormatSink.ToStdOut true, segs, _), _) ] ->
                    // `printfn "%d" (succ 41)` lowers to a `%d` hole whose arg is
                    // the not-yet-expanded `succ 41` call (inline expansion runs
                    // later, in `Emit.lower`).
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(_, TExpr.App(TExpr.Var(kUse, _), TExpr.Const(TConstValue.Int 41, _), _)) ] ->
                        Expect.equal kUse kSucc "the call site `Var` references the inline binding's NodeKey"
                    | other -> failtestf "unexpected Format segments: %A" other
                | other -> failtestf "unexpected slice-3 TAST: %A" other
            }

            test "the full inline sample compiles, runs, prints 42, exits 0" {
                let _, artifact =
                    compileSource "Slice3Sample" "let inline succ x = x + 1\nprintfn \"%d\" (succ 41)"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "42" "succ 41 expanded to (let x = 41 in x + 1)"
            }

            test "nested inline expansion prints 42 (proves NodeKey freshening)" {
                // Without freshening the inner and outer expansions share the
                // parameter's NodeKey — and thus its local slot — and the
                // program computes 41 instead of 42.
                let _, artifact =
                    compileSource "Slice3Nested" "let inline succ x = x + 1\nprintfn \"%d\" (succ (succ 40))"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "42" "succ (succ 40) = 42, not 41"
            }
        ]
