module XParsec.FSharp.Codegen.Clr.Tests.ControlFlowTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

[<Tests>]
let tests =
    testList
        "ControlFlow"
        [
            for src, expected in
                [
                    "printfn \"%d\" (if true then 1 else 2)", "1"
                    "printfn \"%d\" (if false then 1 else 2)", "2"
                    "printfn \"%d\" (if 3 > 2 then 10 else 20)", "10"
                    // elif chain: first / middle / last arm
                    "let classify n = if n < 0 then -1 elif n = 0 then 0 else 1\nprintfn \"%d\" (classify (-5))", "-1"
                    "let classify n = if n < 0 then -1 elif n = 0 then 0 else 1\nprintfn \"%d\" (classify 0)", "0"
                    "let classify n = if n < 0 then -1 elif n = 0 then 0 else 1\nprintfn \"%d\" (classify 7)", "1"
                    "printfn \"%d\" (if false then 1 else (if true then 2 else 3))", "2"
                ] -> test src { runs expected src }

            // A missing `else` is `else ()`, so the whole `if` is `unit` and the
            // then-branch must be too.
            yield
                test "if-then (no else): taken branch runs the side effect" { runs "1" "if true then printfn \"%d\" 1" }
            yield
                test "if-then (no else): untaken branch skipped, sequencing continues" {
                    runsLines [ "after" ] "if false then printfn \"skip\"\nprintfn \"after\""
                }
            yield
                // The synthesized `unit` is the innermost else; each elif nests around it.
                test "if-then/elif (no else): middle arm runs" {
                    runs "mid" "if false then printfn \"a\" elif true then printfn \"mid\""
                }

            yield
                test "two top-level printfn run in source order" {
                    runsLines [ "a"; "b" ] "printfn \"a\"\nprintfn \"b\""
                }

            yield
                test "for-in over Enumerable.Range prints the elements in order" {
                    runsLines [ "1"; "2"; "3" ] "for x in System.Linq.Enumerable.Range(1, 3) do\n    printfn \"%d\" x"
                }
        ]
