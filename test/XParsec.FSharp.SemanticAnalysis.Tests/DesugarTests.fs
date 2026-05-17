module XParsec.FSharp.SemanticAnalysis.Tests.DesugarTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    let ctx = PassContext(MockBuiltins.provider, input, lexed)
    Desugar.run ctx file
    ctx

[<Tests>]
let tests =
    testList
        "Desugar"
        [
            test "InfixApp with `+` -> OpName \"op_Addition\"" {
                // "let x = 1 + 2" — InfixApp's first sub-expr is `1` at offset 8.
                let ctx = analyse "let x = 1 + 2"
                let infixKey = NodeKey.ofSource 8 NodeKind.ExprInfixApp

                match ctx.Desugared.TryGetValue infixKey with
                | ValueSome(DesugaredForm.OpName name) -> Expect.equal name "op_Addition" "compiled op name"
                | ValueNone -> failtest "no DesugaredForm entry for InfixApp"
            }

            test "InfixApp with `-` -> OpName \"op_Subtraction\"" {
                let ctx = analyse "let x = 5 - 2"
                let infixKey = NodeKey.ofSource 8 NodeKind.ExprInfixApp

                match ctx.Desugared.TryGetValue infixKey with
                | ValueSome(DesugaredForm.OpName name) -> Expect.equal name "op_Subtraction" "compiled op name"
                | ValueNone -> failtest "no DesugaredForm entry"
            }

            test "non-operator expressions write no Desugared entry" {
                let ctx = analyse "let x = 42"
                Expect.equal ctx.Desugared.Count 0 "Desugared should be empty"
            }

            test "nested InfixApp records both operators" {
                // "let x = 1 + 2 * 3" — outer InfixApp at offset 8 ('+'),
                // inner InfixApp at offset 12 ('*' RHS).
                // Left-associative parsing makes outer left = '1 + 2', so
                // the outer-most InfixApp spans the whole expression and
                // starts at offset 8; the inner is `2 * 3` at offset 12.
                let ctx = analyse "let x = 1 + 2 * 3"
                Expect.isGreaterThanOrEqual ctx.Desugared.Count 2 "at least two ops"
            }
        ]
