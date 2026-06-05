module XParsec.FSharp.Codegen.Clr.Tests.ClosureTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Layer 1 behavioral corpus: closures. Slice5Tests + CapturedMutableTests hold
// the deep anchors (closure-type emission, the generic-closure `GenericParam`
// rows, the captured-`Vesper.Ref` cell). This is the broad net over the
// observable runtime behaviours: a non-capturing lambda value, a value capture,
// an inline lambda in argument position, a returned (generic) closure, and a
// captured mutable surviving across invocations.
//
// `runs` already forces `Vesper.Core.dll` into the default load context (via
// `withCore`, which the compile path injects), so the captured-`Ref` and
// `Vesper.Fun` references resolve at load without an explicit force here.

[<Tests>]
let tests =
    testList
        "Closures"
        [
            for src, expected in
                [
                    // a non-capturing lambda bound to a value, then Invoked
                    "let f = fun x -> x + 1\nprintfn \"%d\" (f 41)", "42"
                    // capture a value from the enclosing scope
                    "let n = 10\nlet g = fun x -> x + n\nprintfn \"%d\" (g 41)", "51"
                    // an inline lambda applied directly in argument position
                    "printfn \"%d\" ((fun x -> x + 1) 5)", "6"
                    // a returned (generic) closure capturing its argument
                    "let mkConst x =\n    let f = fun () -> x\n    f\nlet always10 = mkConst 10\nprintfn \"%d\" (always10 ())",
                    "10"
                ] -> test src { runs expected src }

            // a captured `let mutable` cell shared across three invocations of
            // the escaping closure (promotion to Vesper.Ref)
            yield
                test "captured mutable counter: three invocations share the cell" {
                    runsLines
                        [ "1"; "2"; "3" ]
                        (String.concat
                            "\n"
                            [
                                "let mkCounter () ="
                                "    let mutable n = 0"
                                "    fun () ->"
                                "        n <- n + 1"
                                "        n"
                                "let c = mkCounter ()"
                                "printfn \"%d\" (c ())"
                                "printfn \"%d\" (c ())"
                                "printfn \"%d\" (c ())"
                            ])
                }

            // `(+)` resolves as a *value* — an `External op_Addition` the call site
            // references — before eta-reification gives it nested `Vesper.Fun`
            // closures (the runtime shape lives in `SelfHostTests`). Former Slice5 M3.
            yield
                test "`let add = (+)` analyses clean to an External op_Addition value" {
                    let tast = analyse "let add = (+)\nprintfn \"%d\" (add 40 2)"
                    Expect.isEmpty tast.Diagnostics "no diagnostics — (+) resolves as a value"

                    match tast.Decls with
                    | EqList [ TDecl.Let(TPat.NamedSimple(kAdd, _), TExpr.External("op_Addition", _, _), false, _)
                               TDecl.Expression(TExpr.Format(FormatSink.ToStdOut true, segs, _), _) ] ->
                        match EqArray.toList segs with
                        | [ FormatSeg.Hole(_,
                                           TExpr.App(TExpr.App(TExpr.Var(kUse, _), TExpr.Const(TConstValue.Int 40, _), _),
                                                     TExpr.Const(TConstValue.Int 2, _),
                                                     _)) ] ->
                            Expect.equal kUse kAdd "the call site references the (+) binding"
                        | other -> failtestf "unexpected segments: %A" other
                    | other -> failtestf "unexpected (+)-as-value TAST: %A" other
                }
        ]
