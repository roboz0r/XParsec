module XParsec.FSharp.Codegen.Clr.Tests.BindingTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `let` binding forms, one row per form. Rows use `\n` rather than triple-quotes so
// the table stays column-aligned.

[<Tests>]
let tests =
    testList
        "Bindings"
        [
            for src, expected in
                [
                    // a top-level value bound, reloaded, printed
                    "let x = 5\nprintfn \"%d\" x", "5"
                    // `let … in` as an expression
                    "printfn \"%d\" (let y = 41 in y + 1)", "42"
                    // a binding read on a later binding's RHS. A same-name second `let`
                    // (shadowing) is FS0037 at module level, `dotnet fsi`-probed, so the
                    // sequence uses distinct names.
                    "let x = 1\nlet y = x + 10\nprintfn \"%d\" y", "11"
                    // two independent top-level bindings
                    "let a = 2\nlet b = 3\nprintfn \"%d\" (a * b)", "6"
                    // an inner binding inside a function body
                    "let f x = let y = x + 1 in y * 2\nprintfn \"%d\" (f 4)", "10"
                    // `let rec` (if-driven, no match)
                    "let rec sumTo n = if n = 0 then 0 else n + sumTo (n - 1)\nprintfn \"%d\" (sumTo 5)", "15"
                    // `let inline`
                    "let inline sq x = x * x\nprintfn \"%d\" (sq 7)", "49"
                    // a function inside a nested module, called qualified: the bare spelling
                    // is FS0039 outside `M`, `dotnet fsi`-probed
                    "module M =\n    let twice x = x + x\nprintfn \"%d\" (M.twice 21)", "42"
                ] -> test src { runs expected src }

            yield
                test "a let-decl + use analyses to a Let plus a Format hole referencing its NodeKey" {
                    let tast = analyse "let x = 1 + 2\nprintfn \"%d\" x"
                    Expect.isEmpty tast.Diagnostics "no diagnostics"

                    // `1 + 2` is matched as `_`: under the real `Vesper.Core` contract
                    // `(+)` inline-expands to an `ILIntrinsic "add"` over two synth lets.
                    match tast.Decls with
                    | EqList [ TDecl.Let(TPat.NamedSimple(kx, _, _, _), _, false, _, _)
                               TDecl.Expression(TExpr.Format(FormatSink.ToStdOut true, segs, _, _), _) ] ->
                        match EqArray.toList segs with
                        | [ FormatSeg.Hole(hole, TExpr.Var(kxUse, _, _)) ] ->
                            Expect.equal kxUse kx "the hole's `Var` references the let-bound NodeKey"

                            Expect.equal
                                hole.Ty
                                (TyConst(RuntimeNames.intKey, EqArray.empty))
                                "the %d hole types as int"
                        | other -> failtestf "unexpected Format segments: %A" other
                    | _ -> failtestf "unexpected let-decl TAST: %A" (EqArray.toList tast.Decls)
                }
        ]
