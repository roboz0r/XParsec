module XParsec.FSharp.Codegen.Clr.Tests.PrintfSpecTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Each driver is run through the Vesper-compiled `Vesper.Printf.dll` handler in
// its own collectible ALC, and its stdout is asserted equal to the pinned
// structural spec. The `%A` cases exercise the `structural-printer.clr.fs` port; the
// plain printf cases exercise `formatter.clr.fs`.
//
// Every input here is ACYCLIC except the final self-referential-record case,
// which pins the cons-list cycle-detection back-edge (`...`).

[<Tests>]
let tests =
    testList
        "PrintfSpec"
        [
            // ---- `%A` structural engine (structural-printer.clr.fs) ----
            test "`%A` of an int" { runsEq "42" "printfn \"%A\" 42" }

            test "`%A` of a string (quoted)" { runsEq "\"hi\"" "printfn \"%A\" \"hi\"" }

            test "`%A` of a bool" { runsEq "true" "printfn \"%A\" true" }

            test "`%A` of a char (quoted)" { runsEq "'x'" "printfn \"%A\" 'x'" }

            test "`%A` of a float" { runsEq "3.0" "printfn \"%A\" 3.0" }

            test "`%A` of a list (flat)" { runsEq "[1; 2; 3]" "printfn \"%A\" [ 1; 2; 3 ]" }

            test "`%0A` of a list (never break)" { runsEq "[1; 2; 3]" "printfn \"%0A\" [ 1; 2; 3 ]" }

            test "`%A` of a tuple" { runsEq "(1, 2)" "printfn \"%A\" (1, 2)" }

            test "`%.2A` truncates a list after 2 nodes" { runsEq "[1; 2; ...]" "printfn \"%.2A\" [ 1; 2; 3; 4; 5 ]" }

            test "`%.0A` truncates immediately" { runsEq "..." "printfn \"%.0A\" [ 1; 2; 3 ]" }

            test "`%A` of a record" {
                runsEq "{ X = 1; Y = \"a\" }" "type R = { X: int; Y: string }\nprintfn \"%A\" { X = 1; Y = \"a\" }"
            }

            test "`%A` of a record breaks under a tiny width budget" {
                runsEq "{ X = 1;\n  Y = \"a\" }" "type R = { X: int; Y: string }\nprintfn \"%5A\" { X = 1; Y = \"a\" }"
            }

            test "`%A` of a nullary DU case" { runsEq "N" "type Opt = | N | S of int\nlet v = N\nprintfn \"%A\" v" }

            test "`%A` of a payload DU case" { runsEq "S 3" "type Opt = | N | S of int\nlet v = S 3\nprintfn \"%A\" v" }

            test "`%A` of a nested DU application (parenthesised arg)" {
                runsEq "S (S N)" "type Opt = | N | S of Opt\nlet v = S (S N)\nprintfn \"%A\" v"
            }

            test "`%A` of a record nested in a list" {
                runsEq "[{ X = 1 }; { X = 2 }]" "type R = { X: int }\nprintfn \"%A\" [ { X = 1 }; { X = 2 } ]"
            }

            // A self-referential record (`n.Next` points back at `n`) is a CYCLIC
            // input. The visited-set (cons-list + `Object.ReferenceEquals`) renders
            // `...` at the back-edge rather than recursing forever.
            test "`%A` of a self-referential record (cycle truncated)" {
                runsEq
                    "{ Next = ... }"
                    "type Node = { mutable Next: obj }\nlet n = { Next = null }\nn.Next <- (n :> obj)\nprintfn \"%A\" n"
            }

            // ---- plain printf (formatter.clr.fs) ----
            test "literal `printfn`" { runsEq "hi" "printfn \"hi\"" }

            test "`%s`" { runsEq "world" "printfn \"%s\" \"world\"" }

            test "multi-hole `%d and %s`" { runsEq "7 and x" "printfn \"%d and %s\" 7 \"x\"" }

            test "`%.2f` invariant fixed-point" { runsEq "3.14" "printfn \"%.2f\" 3.14159" }

            test "`%5d` right-justify" { runsEq "   42" "printfn \"%5d\" 42" }

            test "`%x` lowercase hex" { runsEq "ff" "printfn \"%x\" 255" }

            test "`%b` bool text" { runsEq "true" "printfn \"%b\" true" }
        ]
