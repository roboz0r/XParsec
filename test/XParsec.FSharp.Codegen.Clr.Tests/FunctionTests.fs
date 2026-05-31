module XParsec.FSharp.Codegen.Clr.Tests.FunctionTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Layer 1 behavioral corpus: function definition + application forms — a single
// argument, currying (multi-arg), partial application, recursion, one static
// method calling another, `inline`, and a higher-order function taking a lambda.
// Rung2Tests pins the emission strategy (top-level fn → static method, capturing
// fn → closure); this is the broad behavioral net.

[<Tests>]
let tests =
    testList
        "Functions"
        [
            for src, expected in
                [
                    // one argument
                    "let twice x = x + x\nprintfn \"%d\" (twice 21)", "42"
                    // currying: a two-argument function fully applied
                    "let add a b = a + b\nprintfn \"%d\" (add 3 4)", "7"
                    // NOT covered yet: a 3+-argument generic function (`a - b - c`
                    // leaves the params as typars and codegen can't infer the
                    // static-method instantiation for arg 3), and partial
                    // application of a user multi-arg function (`let inc = add 1`
                    // hits "cannot encode SemType: TyVar"). Add rows when they land.
                    // recursion
                    "let rec fact n = if n <= 1 then 1 else n * fact (n - 1)\nprintfn \"%d\" (fact 5)", "120"
                    // one static method calling another
                    "let inc x = x + 1\nlet add3 x = inc (inc (inc x))\nprintfn \"%d\" (add3 10)", "13"
                    // inline
                    "let inline succ x = x + 1\nprintfn \"%d\" (succ 41)", "42"
                    // higher-order: a function parameter applied to an argument
                    "let apply f x = f x\nprintfn \"%d\" (apply (fun n -> n + 1) 41)", "42"
                ] -> test src { runs expected src }
        ]
