module XParsec.FSharp.Codegen.Clr.Tests.BindingTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Layer 1 behavioral corpus: `let` binding forms — top-level value, `let … in`,
// shadowing, `let rec`, `let inline`, inner (function-body) bindings, and a
// nested-module member accessed unqualified. The Slice2/Slice3/Rung2 milestone
// files prove the TAST shape + the inline-expansion machinery for these; this
// table is the broad net that a binding-resolution or slot-allocation regression
// trips first. Multi-line rows use `\n` rather than triple-quotes so the table
// stays column-aligned.

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
        ]
