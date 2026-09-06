module XParsec.FSharp.Codegen.Clr.Tests.ComparisonTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `= <> < > <= >=` printed as a `bool` through `%b`: one row per operator per
// truth value, plus char and float rows so a regression in the comparison opcode
// for a non-int element type is punctual.

[<Tests>]
let tests =
    testList
        "Comparison"
        [
            for src, expected in
                [
                    """printfn "%b" (2 = 2)""", "true"
                    """printfn "%b" (2 = 3)""", "false"
                    """printfn "%b" (2 <> 3)""", "true"
                    """printfn "%b" (2 <> 2)""", "false"
                    """printfn "%b" (2 < 3)""", "true"
                    """printfn "%b" (3 < 2)""", "false"
                    """printfn "%b" (3 > 2)""", "true"
                    """printfn "%b" (2 > 3)""", "false"
                    // `<=` / `>=` are the negated forms: `ceq (cgt x y) false`
                    """printfn "%b" (2 <= 2)""", "true"
                    """printfn "%b" (3 <= 2)""", "false"
                    """printfn "%b" (2 >= 2)""", "true"
                    """printfn "%b" (2 >= 3)""", "false"
                    // char: `clt` over the char's int value
                    """printfn "%b" ('a' < 'b')""", "true"
                    """printfn "%b" ('b' < 'a')""", "false"
                    """printfn "%b" ('b' = 'b')""", "true"
                    // float: IEEE `clt`/`cgt`
                    """printfn "%b" (1.5 < 2.5)""", "true"
                    """printfn "%b" (2.5 = 2.5)""", "true"
                ] -> test src { runs expected src }
        ]
