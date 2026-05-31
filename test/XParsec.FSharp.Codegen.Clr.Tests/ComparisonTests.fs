module XParsec.FSharp.Codegen.Clr.Tests.ComparisonTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Layer 1 behavioral corpus: the comparison family (`= <> < > <= >=`) printed
// directly as a `bool` through `%b`. OperatorRoutingTests holds the Layer-2
// anchor that pins *which* IL each routes to (`clt`/`cgt`/`ceq` + negations);
// this is the broad, cheap net — one row per operator per truth value, plus a
// couple of non-int element types so a regression in the comparison opcode for
// char/float is punctual.

[<Tests>]
let tests =
    testList
        "Comparison"
        [
            for src, expected in
                [
                    // equality / inequality on int
                    """printfn "%b" (2 = 2)""", "true"
                    """printfn "%b" (2 = 3)""", "false"
                    """printfn "%b" (2 <> 3)""", "true"
                    """printfn "%b" (2 <> 2)""", "false"
                    // strict ordering
                    """printfn "%b" (2 < 3)""", "true"
                    """printfn "%b" (3 < 2)""", "false"
                    """printfn "%b" (3 > 2)""", "true"
                    """printfn "%b" (2 > 3)""", "false"
                    // inclusive ordering (the negated forms)
                    """printfn "%b" (2 <= 2)""", "true"
                    """printfn "%b" (3 <= 2)""", "false"
                    """printfn "%b" (2 >= 2)""", "true"
                    """printfn "%b" (2 >= 3)""", "false"
                    // char ordering (clt over the char's int value)
                    """printfn "%b" ('a' < 'b')""", "true"
                    """printfn "%b" ('b' = 'b')""", "true"
                    // float comparison (IEEE clt/cgt)
                    """printfn "%b" (1.5 < 2.5)""", "true"
                    """printfn "%b" (2.5 = 2.5)""", "true"
                ] -> test src { runs expected src }
        ]
