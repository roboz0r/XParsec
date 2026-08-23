module XParsec.FSharp.Codegen.Clr.Tests.LiteralTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// One row per literal form, printed through the specifier matching its type.

[<Tests>]
let tests =
    testList
        "Literals"
        [
            for src, expected in
                [
                    """printfn "%d" 42""", "42"
                    """printfn "%d" 0""", "0"
                    """printfn "%d" 1000000""", "1000000"
                    // a negative int literal is a const, not unary negation
                    """printfn "%d" (-7)""", "-7"
                    """printfn "%b" true""", "true"
                    """printfn "%b" false""", "false"
                    """printfn "%c" 'A'""", "A"
                    """printfn "%c" '*'""", "*"
                    // Escaped, not triple-quoted: this source ends in a `"`, which would
                    // close the `"""` terminator early.
                    "printfn \"%s\" \"hello\"", "hello"
                    // Fixed-point widths keep the expected output exact.
                    """printfn "%.2f" 3.5""", "3.50"
                    """printfn "%.1f" 0.0""", "0.0"
                    """printfn "%M" 2.5M""", "2.5"
                    """printfn "%M" 42M""", "42"
                ] -> test src { runs expected src }

            // `%d` types its argument over the integer family, so every integral kind prints
            // through the one specifier.
            yield
                test """`printfn "%d"` takes every integral kind""" {
                    runs "200" """printfn "%d" 200uy"""
                    runs "-1" """printfn "%d" -1y"""
                    runs "70000" """printfn "%d" 70000L"""
                    runs "ff" """printfn "%x" 255uy"""
                }
        ]
