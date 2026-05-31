module XParsec.FSharp.Codegen.Clr.Tests.BindingTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Layer 1 behavioral corpus: `let` binding forms — top-level value, `let … in`,
// shadowing, `let rec`, `let inline`, inner (function-body) bindings, and a
// nested-module member accessed unqualified. This table is the broad net that a
// binding-resolution or slot-allocation regression trips first; the anchor
// beneath it (former `Slice2`) pins the TAST shape — a `let`-bound NodeKey that a
// later `Var` use resolves to. Multi-line rows use `\n` rather than triple-quotes
// so the table stays column-aligned.

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
                    // shadowing: the second `x` sees the first on its RHS
                    "let x = 1\nlet x = x + 10\nprintfn \"%d\" x", "11"
                    // two independent top-level bindings
                    "let a = 2\nlet b = 3\nprintfn \"%d\" (a * b)", "6"
                    // an inner binding inside a function body
                    "let f x = let y = x + 1 in y * 2\nprintfn \"%d\" (f 4)", "10"
                    // `let rec` (if-driven, no match)
                    "let rec sumTo n = if n = 0 then 0 else n + sumTo (n - 1)\nprintfn \"%d\" (sumTo 5)", "15"
                    // `let inline`
                    "let inline sq x = x * x\nprintfn \"%d\" (sq 7)", "49"
                    // a function inside a nested module, called unqualified
                    "module M =\n    let twice x = x + x\nprintfn \"%d\" (twice 21)", "42"
                ] -> test src { runs expected src }

            // The TAST anchor (former Slice2): a `let x = 1 + 2` decl splits off
            // and the `printfn "%d" x` hole's `Var` resolves to the bound NodeKey.
            yield
                test "a let-decl + use analyses to a Let plus a Format hole referencing its NodeKey" {
                    let tast = analyse "let x = 1 + 2\nprintfn \"%d\" x"
                    Expect.isEmpty tast.Diagnostics "no diagnostics"

                    match tast.Decls with
                    | EqList [ TDecl.Let(TPat.NamedSimple(kx, _),
                                         TExpr.App(TExpr.App(TExpr.External("op_Addition", _, _),
                                                             TExpr.Const(TConstValue.Int 1, _),
                                                             _),
                                                   TExpr.Const(TConstValue.Int 2, _),
                                                   _),
                                         false,
                                         _)
                               TDecl.Expression(TExpr.Format(FormatSink.ToStdOut true, segs, _), _) ] ->
                        match EqArray.toList segs with
                        | [ FormatSeg.Hole(hole, TExpr.Var(kxUse, _)) ] ->
                            Expect.equal kxUse kx "the hole's `Var` references the let-bound NodeKey"
                            Expect.equal hole.Ty (TyConst "int") "the %d hole types as int"
                        | other -> failtestf "unexpected Format segments: %A" other
                    | other -> failtestf "unexpected let-decl TAST: %A" other
                }
        ]
