module XParsec.FSharp.Codegen.Clr.Tests.ControlFlowTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Layer 1 behavioral corpus: control flow that the backend emits today —
// `if/then/else`, `elif` chains, top-level sequencing, and `for … in` over a
// BCL `IEnumerable<'T>` (the `ForIn` path; `System.Linq.Enumerable.Range`
// sources a populated sequence without an external instance-method call).
//
// NOT covered here, by design: `while` and integer `for i in 1..n` (the `ForTo`
// node) are representable in the TAST but hit the `buildExpr` catch-all in
// Codegen.Clr/EmitExpr.fs ("unsupported expression") — there is no IL emission
// for them yet, so a `runs` row would fail at codegen rather than exercise a
// supported path. When they land, add their rows here.

[<Tests>]
let tests =
    testList
        "ControlFlow"
        [
            // single-line if/elif rows
            for src, expected in
                [
                    "printfn \"%d\" (if true then 1 else 2)", "1"
                    "printfn \"%d\" (if false then 1 else 2)", "2"
                    // a comparison driving the branch
                    "printfn \"%d\" (if 3 > 2 then 10 else 20)", "10"
                    // elif chain — first / middle / last arm
                    "let classify n = if n < 0 then -1 elif n = 0 then 0 else 1\nprintfn \"%d\" (classify (-5))", "-1"
                    "let classify n = if n < 0 then -1 elif n = 0 then 0 else 1\nprintfn \"%d\" (classify 0)", "0"
                    "let classify n = if n < 0 then -1 elif n = 0 then 0 else 1\nprintfn \"%d\" (classify 7)", "1"
                    // nested if in the else branch
                    "printfn \"%d\" (if false then 1 else (if true then 2 else 3))", "2"
                ] -> test src { runs expected src }

            // if-then with NO else: the missing else is `else ()`, so the whole
            // expression is `unit` and the then-branch must be `unit`. Freeze
            // synthesizes the `unit` else; codegen reuses the ordinary IfThenElse arm.
            yield
                test "if-then (no else): taken branch runs the side effect" { runs "1" "if true then printfn \"%d\" 1" }
            yield
                test "if-then (no else): untaken branch skipped, sequencing continues" {
                    runsLines [ "after" ] "if false then printfn \"skip\"\nprintfn \"after\""
                }
            yield
                // elif with no final else exercises the no-else fold: the synthesized
                // `unit` is the innermost else, each elif nests around it.
                test "if-then/elif (no else): middle arm runs" {
                    runs "mid" "if false then printfn \"a\" elif true then printfn \"mid\""
                }

            // top-level sequencing: two statements run in order
            yield
                test "two top-level printfn run in source order" {
                    runsLines [ "a"; "b" ] "printfn \"a\"\nprintfn \"b\""
                }

            // `for … in` over a populated BCL IEnumerable<int> walks in order
            yield
                test "for-in over Enumerable.Range prints the elements in order" {
                    runsLines [ "1"; "2"; "3" ] "for x in System.Linq.Enumerable.Range(1, 3) do\n    printfn \"%d\" x"
                }
        ]
