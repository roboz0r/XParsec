module XParsec.FSharp.Codegen.Clr.Tests.LogicalOperatorTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `&&` / `||` are NOT compiler intrinsics: they are `val inline` operators whose
// bodies live in `Vesper.Core/ops-std.fs` (`if e1 then e2 else false` /
// `if e1 then true else e2`), spliced at each use site by the cross-package
// inline pass exactly like the arithmetic / equality families — nothing about the
// operator is hard-coded into the compiler.
//
// SHORT-CIRCUIT is DECLARED, not inferred: the right operand is marked
// `[<CallAtMostOnce>]` in `ops-std.fs`, so the inliner splices it at its single
// (`Elaborate`-validated linear) use rather than eager `let`-binding it — the
// right operand is evaluated at most once and on demand. The attribute is the
// general mechanism (any `inline` library combinator can use it); `&&`/`||` are
// just its first clients, and nothing about the operator is hard-coded into the
// compiler. The load-bearing proofs below put a `1 / 0` in the right operand — it
// throws `DivideByZeroException` if evaluated, so reaching exit 0 with the
// expected output proves it was skipped.

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

             // ---- the attribute is GENERAL, not `&&`/`||`-specific -----------
             // A user `inline` combinator whose right operand is `[<CallAtMostOnce>]`
             // short-circuits by the exact same mechanism — proof the laziness is
             // driven by the declared attribute, not by anything special about the
             // logical operators. `1 / 0` on the right throws if evaluated; exit 0
             // proves it was skipped when the left is false.
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

             // ---- the review's regression, now fixed -------------------------
             // An UNMARKED parameter with the SAME `if`-shape body is strict: F#
             // semantics, every argument evaluated. Before the attribute drove the
             // decision, the inliner short-circuited any once-in-branch parameter by
             // body shape, so this program would have (wrongly) skipped `1 / 0` and
             // printed `false`. Now the right operand is eagerly evaluated and the
             // program throws `DivideByZeroException`.
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

             // ---- contract anchor -------------------------------------------
             test "`&&` / `||` bodies are collected from ops-std.fs as cross-package inlines" {
                 let inlines = SymbolProviders.contractInlineBodies defaultManifests
                 Expect.isTrue (Map.containsKey "op_BooleanAnd" inlines) "op_BooleanAnd body sourced from ops-std.fs"
                 Expect.isTrue (Map.containsKey "op_BooleanOr" inlines) "op_BooleanOr body sourced from ops-std.fs"

                 // The right operand carries the `[<CallAtMostOnce>]` flag that makes
                 // the splice short-circuit — positionally, parameter index 1.
                 let andAttrs = inlines.["op_BooleanAnd"].ParamAttrs
                 Expect.isTrue (andAttrs.Length >= 2) "op_BooleanAnd has two parameters' attrs"
                 Expect.isFalse andAttrs.[0].CallAtMostOnce "left operand is strict"
                 Expect.isTrue andAttrs.[1].CallAtMostOnce "right operand is [<CallAtMostOnce>]"
             }
         ])
