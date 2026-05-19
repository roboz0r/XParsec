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
                // "let x = 1 + 2" — InfixApp keys on the operator's offset (`+` at offset 10).
                let ctx = analyse "let x = 1 + 2"
                let infixKey = NodeKey.ofSource 10 NodeKind.ExprInfixApp

                match ctx.Desugared.TryGetValue infixKey with
                | ValueSome(DesugaredForm.OpName name) -> Expect.equal name "op_Addition" "compiled op name"
                | other -> failtestf "expected OpName entry, got %A" other
            }

            test "InfixApp with `-` -> OpName \"op_Subtraction\"" {
                let ctx = analyse "let x = 5 - 2"
                let infixKey = NodeKey.ofSource 10 NodeKind.ExprInfixApp

                match ctx.Desugared.TryGetValue infixKey with
                | ValueSome(DesugaredForm.OpName name) -> Expect.equal name "op_Subtraction" "compiled op name"
                | other -> failtestf "expected OpName entry, got %A" other
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

            test "list literal `[1; 2]` records DesugaredForm.ListLiteral" {
                // "let xs = [1; 2]" — the EnclosedBlock opens with `[` at offset 9.
                let ctx = analyse "let xs = [1; 2]"
                let blockKey = NodeKey.ofSource 9 NodeKind.ExprEnclosedBlock

                match ctx.Desugared.TryGetValue blockKey with
                | ValueSome DesugaredForm.ListLiteral -> ()
                | other -> failtestf "expected ListLiteral, got %A" other
            }

            test "array literal `[|1; 2|]` records DesugaredForm.ArrayLiteral" {
                let ctx = analyse "let xs = [|1; 2|]"
                let blockKey = NodeKey.ofSource 9 NodeKind.ExprEnclosedBlock

                match ctx.Desugared.TryGetValue blockKey with
                | ValueSome DesugaredForm.ArrayLiteral -> ()
                | other -> failtestf "expected ArrayLiteral, got %A" other
            }

            test "empty list `[]` records ListLiteral on its EmptyBlock node" {
                let ctx = analyse "let xs = []"
                let blockKey = NodeKey.ofSource 9 NodeKind.ExprEmptyBlock

                match ctx.Desugared.TryGetValue blockKey with
                | ValueSome DesugaredForm.ListLiteral -> ()
                | other -> failtestf "expected ListLiteral on EmptyBlock, got %A" other
            }
        ]
