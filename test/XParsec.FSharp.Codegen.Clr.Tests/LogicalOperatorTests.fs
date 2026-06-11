module XParsec.FSharp.Codegen.Clr.Tests.LogicalOperatorTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `&&` / `||` are NOT compiler intrinsics: they are `val inline` operators whose
// bodies live in `Vesper.Core/ops-std.fs` (`if e1 then e2 else false` /
// `if e1 then true else e2`), spliced at each use site by the cross-package
// inline pass exactly like the arithmetic / equality families — nothing about the
// operator is hard-coded into the compiler.
//
// SHORT-CIRCUIT is preserved by the inliner, not by the operator: a parameter used
// exactly once inside a conditional branch is substituted by name rather than
// eager `let`-bound (`InlineExpansion.usedOnceInBranch`), so the right operand is
// evaluated only on demand. The load-bearing proofs below put a `1 / 0` in the
// right operand — it throws `DivideByZeroException` if evaluated, so reaching exit
// 0 with the expected output proves it was skipped.

let private truthTable: Test list =
    [
        for src, expected in
            [
                """printfn "%b" (true && true)""", "true"
                """printfn "%b" (true && false)""", "false"
                """printfn "%b" (false && true)""", "false"
                """printfn "%b" (false || false)""", "false"
                """printfn "%b" (false || true)""", "true"
                """printfn "%b" (true || false)""", "true"
                // left-associative chaining
                """printfn "%b" (true && true && false)""", "false"
                """printfn "%b" (false || false || true)""", "true"
            ] -> test src { runs expected src }
    ]

[<Tests>]
let tests =
    testList
        "LogicalOperators"
        (truthTable
         @ [
             // ---- short-circuit proofs (load-bearing) -----------------------
             // The left operand comes through a function (`no`/`yes`) so the `&&`/`||`
             // genuinely branches at runtime rather than being constant-folded.
             test "`&&` does not evaluate its right operand when the left is false" {
                 runs "false" "let no (b: bool) = b\nprintfn \"%b\" (no false && (1 / 0 = 0))"
             }

             test "`||` does not evaluate its right operand when the left is true" {
                 runs "true" "let yes (b: bool) = b\nprintfn \"%b\" (yes true || (1 / 0 = 0))"
             }

             // Sanity: when the guard admits it, the right operand DOES run — proof
             // the operator is the real library body, not a fold to a constant.
             test "`&&` evaluates its right operand when the left is true" {
                 runs "true" "let yes (b: bool) = b\nprintfn \"%b\" (yes true && (2 / 2 = 1))"
             }

             // ---- contract anchor -------------------------------------------
             test "`&&` / `||` bodies are collected from ops-std.fs as cross-package inlines" {
                 let inlines = SymbolProviders.contractInlineBodies defaultManifests
                 Expect.isTrue (Map.containsKey "op_BooleanAnd" inlines) "op_BooleanAnd body sourced from ops-std.fs"
                 Expect.isTrue (Map.containsKey "op_BooleanOr" inlines) "op_BooleanOr body sourced from ops-std.fs"
             }
         ])
