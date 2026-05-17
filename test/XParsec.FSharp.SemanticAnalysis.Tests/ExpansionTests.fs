module XParsec.FSharp.SemanticAnalysis.Tests.ExpansionTests

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
        "Expansion"
        [
            // ---- Extended operator coverage ----

            test "`<` types as int -> int -> bool, result is bool" {
                let tast = analyse "let b = 1 < 2"
                Expect.equal (declType tast) MockBuiltins.tyBool "b : bool"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "`&&` between two bool literals" {
                let tast = analyse "let b = true && false"
                Expect.equal (declType tast) MockBuiltins.tyBool "b : bool"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "`/` and `%` type as int -> int -> int" {
                let tast = analyse "let r = 10 / 3 % 2"
                Expect.equal (declType tast) MockBuiltins.tyInt "r : int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "`=` on int operands types as bool" {
                let tast = analyse "let b = 1 = 1"
                Expect.equal (declType tast) MockBuiltins.tyBool "b : bool"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            // ---- PrefixApp (unary -) ----

            test "unary minus on int types as int" {
                // `-x` (where x is a variable) forces a PrefixApp; `-5` would
                // be folded into a negative literal by the parser.
                let tast = analyse "let f x = -x"
                let intToInt = TyFun(MockBuiltins.tyInt, MockBuiltins.tyInt)
                // Without a use-site forcing x : int, x stays polymorphic. Use
                // the operator to constrain it — `let f x = -x + 0`.
                let tast = analyse "let f x = -x + 0"
                Expect.equal (declType tast) intToInt "f : int -> int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "unary minus translates to External op_UnaryNegation App" {
                let tast = analyse "let f x = -x + 0"

                match tast.Decls.[0] with
                | TDecl.Let(_,
                            TExpr.Lambda(_,
                                         TExpr.App(TExpr.App(TExpr.External("op_Addition", _),
                                                             TExpr.App(TExpr.External("op_UnaryNegation", _),
                                                                       TExpr.Var _,
                                                                       _),
                                                             _),
                                                   TExpr.Const(TConstValue.Int 0, _),
                                                   _),
                                         _),
                            _) -> ()
                | other -> failtestf "unexpected: %A" other
            }

            // ---- IfThenElse ----

            test "if true then 1 else 2 types as int" {
                let tast = analyse "let r = if true then 1 else 2"
                Expect.equal (declType tast) MockBuiltins.tyInt "r : int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "if condition must be bool" {
                let tast = analyse "let r = if 1 then 2 else 3"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "cond=int triggers a mismatch diagnostic"
            }

            test "if-then-else translates to TExpr.IfThenElse" {
                let tast = analyse "let r = if true then 1 else 2"

                match tast.Decls.[0] with
                | TDecl.Let(_,
                            TExpr.IfThenElse(TExpr.Const(TConstValue.Bool true, _),
                                             TExpr.Const(TConstValue.Int 1, _),
                                             TExpr.Const(TConstValue.Int 2, _),
                                             iteTy),
                            _) -> Expect.equal iteTy MockBuiltins.tyInt "ITE result type"
                | other -> failtestf "unexpected: %A" other
            }

            test "if-then-else with non-trivial branches: let abs n = if n < 0 then -n else n" {
                let tast = analyse "let abs n = if n < 0 then -n else n"
                let intToInt = TyFun(MockBuiltins.tyInt, MockBuiltins.tyInt)
                Expect.equal (declType tast) intToInt "abs : int -> int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            // ---- Combined ----

            test "compound: bool from && and comparison" {
                let tast = analyse "let inRange x = x > 0 && x < 100"
                let intToBool = TyFun(MockBuiltins.tyInt, MockBuiltins.tyBool)
                Expect.equal (declType tast) intToBool "inRange : int -> bool"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }
        ]
