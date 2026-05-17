module XParsec.FSharp.SemanticAnalysis.Tests.FreezeTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyse MockBuiltins.provider input lexed file

[<Tests>]
let tests =
    testList
        "Freeze"
        [
            test "`let x = 1` -> single TDecl.Let with TConst Int 1" {
                let tast = analyse "let x = 1"
                Expect.equal tast.Decls.Length 1 "one decl"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.Const(TConstValue.Int 1, ty), letTy) ->
                    Expect.equal ty MockBuiltins.tyInt "value type"
                    Expect.equal letTy MockBuiltins.tyInt "binding type"
                | other -> failtestf "unexpected: %A" other
            }

            test "`let b = true` -> TConst Bool true" {
                let tast = analyse "let b = true"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.Const(TConstValue.Bool true, ty), _) ->
                    Expect.equal ty MockBuiltins.tyBool "value type bool"
                | other -> failtestf "unexpected: %A" other
            }

            test "`let f = fun x -> x + 1` -> Lambda over App chain with External operator" {
                let tast = analyse "let f = fun x -> x + 1"
                let intTy = MockBuiltins.tyInt
                let intToInt = TyFun(intTy, intTy)

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.Lambda(_, body, lamTy), declTy) ->
                    Expect.equal lamTy intToInt "lambda type int -> int"
                    Expect.equal declTy intToInt "decl type int -> int"

                    match body with
                    | TExpr.App(TExpr.App(TExpr.External("op_Addition", opTy), TExpr.Var(_, leftTy), partialTy),
                                TExpr.Const(TConstValue.Int 1, rightTy),
                                resultTy) ->
                        Expect.equal opTy (TyFun(intTy, TyFun(intTy, intTy))) "op type"
                        Expect.equal leftTy intTy "lhs param ref type"
                        Expect.equal partialTy (TyFun(intTy, intTy)) "partial type"
                        Expect.equal rightTy intTy "rhs literal type"
                        Expect.equal resultTy intTy "result type"
                    | _ -> failtestf "unexpected body shape: %A" body
                | other -> failtestf "unexpected decl: %A" other
            }

            test "function-form `let f x = x + 1` produces same shape as fun-form" {
                let tast = analyse "let f x = x + 1"
                let intToInt = TyFun(MockBuiltins.tyInt, MockBuiltins.tyInt)

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.Lambda(_, _body, lamTy), declTy) ->
                    Expect.equal lamTy intToInt "lambda type"
                    Expect.equal declTy intToInt "decl type"
                | other -> failtestf "unexpected: %A" other
            }

            test "`let result = let id = fun x -> x in id 42` -> nested Let with App" {
                let tast = analyse "let result = let id = fun x -> x in id 42"

                match tast.Decls.[0] with
                | TDecl.Let(_, value, declTy) ->
                    Expect.equal declTy MockBuiltins.tyInt "result : int"

                    match value with
                    | TExpr.Let(_, TExpr.Lambda _, TExpr.App(TExpr.Var _, TExpr.Const(TConstValue.Int 42, _), _), _) ->
                        ()
                    | _ -> failtestf "unexpected value: %A" value
                | other -> failtestf "unexpected: %A" other
            }

            test "TDecl.Let binding NodeKey matches headPat NodeKey" {
                let tast = analyse "let x = 1"
                let expected = NodeKey.ofSource 4 NodeKind.PatIdent

                match tast.Decls.[0] with
                | TDecl.Let(bindingKey, _, _) -> Expect.equal bindingKey expected "binding key"
                | other -> failtestf "unexpected: %A" other
            }

            test "TVar references the original headPat NodeKey" {
                // "let x = 1\nlet y = x" — y's RHS references x's binding key.
                let tast = analyse "let x = 1\nlet y = x"
                let xKey = NodeKey.ofSource 4 NodeKind.PatIdent

                match tast.Decls with
                | [ _; TDecl.Let(_, TExpr.Var(refKey, _), _) ] -> Expect.equal refKey xKey "y refs x"
                | _ -> failtestf "unexpected decls: %A" tast.Decls
            }

            test "diagnostics propagate from earlier passes" {
                let tast = analyse "let x = undefined"

                Expect.isGreaterThanOrEqual tast.Diagnostics.Length 1 "unresolved diagnostic reaches the TAST"
            }
        ]
