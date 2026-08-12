module XParsec.FSharp.Codegen.Clr.Tests.LogicalOperatorTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `&&` / `||` are library operators, not compiler intrinsics: `let inline (&&) (e1: bool)
// ([<CallAtMostOnce>] e2: bool) = if e1 then e2 else false` in `Vesper.Core/ops-std.fs`.
// The attribute short-circuits: the argument is spliced at its one use, not `let`-bound.

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
             // `1 / 0` on the right throws if evaluated, so exit 0 proves it was skipped.
             // The left operand comes through `no`/`yes` so the branch is a runtime one
             // and cannot be constant-folded away.
             test "`&&` does not evaluate its right operand when the left is false" {
                 runs "false" "let no (b: bool) = b\nprintfn \"%b\" (no false && (1 / 0 = 0))"
             }

             test "`||` does not evaluate its right operand when the left is true" {
                 runs "true" "let yes (b: bool) = b\nprintfn \"%b\" (yes true || (1 / 0 = 0))"
             }

             test "`&&` evaluates its right operand when the left is true" {
                 runs "true" "let yes (b: bool) = b\nprintfn \"%b\" (yes true && (2 / 2 = 1))"
             }

             // The attribute is general: a user combinator marked the same way
             // short-circuits too, so the laziness is not `&&`/`||`-specific.
             test "a user `[<CallAtMostOnce>]` inline parameter short-circuits its argument" {
                 runs
                     "false"
                     (String.concat
                         "\n"
                         [
                             "let no (b: bool) = b"
                             "let inline myAnd (a: bool) ([<CallAtMostOnce>] b: bool) : bool = if a then b else false"
                             "printfn \"%b\" (myAnd (no false) (1 / 0 = 0))"
                         ])
             }

             // The same `if`-shape body without the attribute is strict, so `1 / 0`
             // is evaluated and throws. A shape-based short-circuit would skip it
             // and print `false` instead.
             test "an unmarked `if`-shape inline parameter is strict (no shape-based short-circuit)" {
                 runtimeThrows
                     "DivideByZero"
                     (String.concat
                         "\n"
                         [
                             "let no (b: bool) = b"
                             "let inline strictAnd (a: bool) (b: bool) : bool = if a then b else false"
                             "printfn \"%b\" (strictAnd (no false) (1 / 0 = 0))"
                         ])
             }

             test "`&&` / `||` bodies are collected from ops-std.fs as cross-package inlines" {
                 let inlines = ClrSymbolProviders.contractInlineBodies defaultPackages
                 Expect.isTrue (Map.containsKey "op_BooleanAnd" inlines) "op_BooleanAnd body sourced from ops-std.fs"
                 Expect.isTrue (Map.containsKey "op_BooleanOr" inlines) "op_BooleanOr body sourced from ops-std.fs"

                 let andAttrs = inlines.["op_BooleanAnd"].ParamAttrs
                 Expect.isTrue (andAttrs.Length >= 2) "op_BooleanAnd has two parameters' attrs"
                 Expect.isFalse andAttrs.[0].CallAtMostOnce "left operand is strict"
                 Expect.isTrue andAttrs.[1].CallAtMostOnce "right operand is [<CallAtMostOnce>]"
             }
         ])
