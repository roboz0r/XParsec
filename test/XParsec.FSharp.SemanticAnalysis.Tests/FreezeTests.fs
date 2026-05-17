module XParsec.FSharp.SemanticAnalysis.Tests.FreezeTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyse MockBuiltins.provider input lexed file

let private declType (tast: TastFile) : SemType =
    match tast.Decls with
    | [ TDecl.Let(_, _, ty) ] -> ty
    | other -> failwithf "expected single TDecl.Let, got %A" other

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

                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = fun v1 -> (v1 + 1)" "TAST shape"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.Lambda(_, _, lamTy), declTy) ->
                    Expect.equal lamTy intToInt "lambda type int -> int"
                    Expect.equal declTy intToInt "decl type int -> int"
                | other -> failtestf "unexpected decl: %A" other
            }

            test "function-form `let f x = x + 1` produces same shape as fun-form" {
                let tast = analyse "let f x = x + 1"
                let intToInt = TyFun(MockBuiltins.tyInt, MockBuiltins.tyInt)

                Expect.equal
                    (TastShape.prettyDecl tast.Decls.[0])
                    "let v0 = fun v1 -> (v1 + 1)"
                    "TAST shape matches fun-form"

                match tast.Decls.[0] with
                | TDecl.Let(_, _, declTy) -> Expect.equal declTy intToInt "decl type"
                | other -> failtestf "unexpected: %A" other
            }

            test "`let result = let id = fun x -> x in id 42` -> nested Let with App" {
                let tast = analyse "let result = let id = fun x -> x in id 42"

                Expect.equal
                    (TastShape.prettyDecl tast.Decls.[0])
                    "let v0 = let v1 = fun v2 -> v2 in (v1 42)"
                    "TAST shape"

                Expect.equal (declType tast) MockBuiltins.tyInt "result : int"
            }

            test "TDecl.Let binding NodeKey matches headPat NodeKey" {
                let tast = analyse "let x = 1"
                let expected = NodeKey.ofSource 4 NodeKind.PatIdent

                match tast.Decls.[0] with
                | TDecl.Let(TPat.NamedSimple(bindingKey, _), _, _) -> Expect.equal bindingKey expected "binding key"
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
