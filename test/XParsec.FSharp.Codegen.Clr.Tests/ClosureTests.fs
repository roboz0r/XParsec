module XParsec.FSharp.Codegen.Clr.Tests.ClosureTests

open Expecto
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
            // the escaping closure (records-plan §B7 promotion to Vesper.Ref)
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
        ]
