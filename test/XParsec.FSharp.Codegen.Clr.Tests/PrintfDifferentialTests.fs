module XParsec.FSharp.Codegen.Clr.Tests.PrintfDifferentialTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// PP7e — the ALC-separable differential test (printf-port-steps.md). Each driver
// is run through BOTH `Vesper.Printf.dll` handlers — the committed C# one and the
// `buildPackage`-produced fully-Vesper one — each in its own collectible ALC, and
// their stdout is asserted byte-identical (and, where pinned, equal to the
// structural spec). This is the bring-up safety net that proves the Vesper handler
// matches the C# one before PP7f drops the C# `.csproj`.
//
// Every input here is ACYCLIC, so the dropped visited-set (cycle detection, PP7f)
// makes no difference — the depth-guard output is byte-identical to the C# engine.
// The `%A` cases exercise the `structural-printer.fs` port (the part being
// self-hosted); the plain printf cases exercise `formatter.fs`.

[<Tests>]
let tests =
    testList
        "PrintfDifferential"
        [
            // ---- `%A` structural engine (structural-printer.fs) ----
            test "`%A` of an int" { runsDifferentialEq "42" "printfn \"%A\" 42" }

            test "`%A` of a string (quoted)" { runsDifferentialEq "\"hi\"" "printfn \"%A\" \"hi\"" }

            test "`%A` of a bool" { runsDifferentialEq "true" "printfn \"%A\" true" }

            test "`%A` of a char (quoted)" { runsDifferentialEq "'x'" "printfn \"%A\" 'x'" }

            test "`%A` of a float" { runsDifferentialEq "3.0" "printfn \"%A\" 3.0" }

            test "`%A` of a list (flat)" { runsDifferentialEq "[1; 2; 3]" "printfn \"%A\" [ 1; 2; 3 ]" }

            test "`%0A` of a list (never break)" { runsDifferentialEq "[1; 2; 3]" "printfn \"%0A\" [ 1; 2; 3 ]" }

            test "`%A` of a tuple" { runsDifferentialEq "(1, 2)" "printfn \"%A\" (1, 2)" }

            test "`%.2A` truncates a list after 2 nodes" {
                runsDifferentialEq "[1; 2; ...]" "printfn \"%.2A\" [ 1; 2; 3; 4; 5 ]"
            }

            test "`%.0A` truncates immediately" { runsDifferentialEq "..." "printfn \"%.0A\" [ 1; 2; 3 ]" }

            test "`%A` of a record" {
                runsDifferentialEq
                    "{ X = 1; Y = \"a\" }"
                    "type R = { X: int; Y: string }\nprintfn \"%A\" { X = 1; Y = \"a\" }"
            }

            test "`%A` of a record breaks under a tiny width budget" {
                runsDifferentialEq
                    "{ X = 1;\n  Y = \"a\" }"
                    "type R = { X: int; Y: string }\nprintfn \"%5A\" { X = 1; Y = \"a\" }"
            }

            test "`%A` of a nullary DU case" {
                runsDifferentialEq "N" "type Opt = | N | S of int\nlet v = N\nprintfn \"%A\" v"
            }

            test "`%A` of a payload DU case" {
                runsDifferentialEq "S 3" "type Opt = | N | S of int\nlet v = S 3\nprintfn \"%A\" v"
            }

            test "`%A` of a nested DU application (parenthesised arg)" {
                runsDifferentialEq "S (S N)" "type Opt = | N | S of Opt\nlet v = S (S N)\nprintfn \"%A\" v"
            }

            test "`%A` of a record nested in a list" {
                runsDifferentialEq
                    "[{ X = 1 }; { X = 2 }]"
                    "type R = { X: int }\nprintfn \"%A\" [ { X = 1 }; { X = 2 } ]"
            }

            // ---- plain printf (formatter.fs) ----
            test "literal `printfn`" { runsDifferentialEq "hi" "printfn \"hi\"" }

            test "`%s`" { runsDifferentialEq "world" "printfn \"%s\" \"world\"" }

            test "multi-hole `%d and %s`" { runsDifferentialEq "7 and x" "printfn \"%d and %s\" 7 \"x\"" }

            test "`%.2f` invariant fixed-point" { runsDifferentialEq "3.14" "printfn \"%.2f\" 3.14159" }

            test "`%5d` right-justify" { runsDifferentialEq "   42" "printfn \"%5d\" 42" }

            test "`%x` lowercase hex" { runsDifferentialEq "ff" "printfn \"%x\" 255" }

            test "`%b` bool text" { runsDifferentialEq "true" "printfn \"%b\" true" }
        ]
