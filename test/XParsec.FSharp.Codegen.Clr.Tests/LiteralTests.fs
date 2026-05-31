module XParsec.FSharp.Codegen.Clr.Tests.LiteralTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Layer 1 behavioral corpus (docs/codegen-test-strategy-plan.md): primitive
// literals routed through the printf specifier that matches their type. Each
// row is one `runs` assertion named by its source — the table *is* the coverage
// map for "which literal forms reach IL and print their value". A break in
// literal lowering (int boxing, char/byte conv, decimal const decoding) shows
// up here as a punctual red row rather than buried in a milestone test.

[<Tests>]
let tests =
    testList
        "Literals"
        [
            for src, expected in
                [
                    // int
                    """printfn "%d" 42""", "42"
                    """printfn "%d" 0""", "0"
                    """printfn "%d" 1000000""", "1000000"
                    // a negative int literal is a const, not unary negation
                    """printfn "%d" (-7)""", "-7"
                    // byte literal (suffix uy) prints its value through %d
                    """printfn "%d" 200uy""", "200"
                    // bool
                    """printfn "%b" true""", "true"
                    """printfn "%b" false""", "false"
                    // char
                    """printfn "%c" 'A'""", "A"
                    """printfn "%c" '*'""", "*"
                    // string
                    // escaped (not triple-quoted): the source ends in a `"`, which
                    // would collide with the `"""` terminator
                    "printfn \"%s\" \"hello\"", "hello"
                    // float (fixed-point so the assertion is exact)
                    """printfn "%.2f" 3.5""", "3.50"
                    """printfn "%.1f" 0.0""", "0.0"
                    // decimal (suffix M)
                    """printfn "%M" 2.5M""", "2.5"
                    """printfn "%M" 42M""", "42"
                ] -> test src { runs expected src }
        ]
