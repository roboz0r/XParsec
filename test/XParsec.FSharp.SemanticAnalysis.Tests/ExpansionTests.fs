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

                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = fun v1 -> ((-v1) + 0)" "TAST shape"
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

                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = if true then 1 else 2" "TAST shape"

                Expect.equal (declType tast) MockBuiltins.tyInt "ITE result type"
            }

            test "if-then-else with non-trivial branches: let abs n = if n < 0 then -n else n" {
                let tast = analyse "let abs n = if n < 0 then -n else n"
                let intToInt = TyFun(MockBuiltins.tyInt, MockBuiltins.tyInt)
                Expect.equal (declType tast) intToInt "abs : int -> int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            // ---- Tuples ----

            test "pair of ints types as int * int" {
                let tast = analyse "let p = 1, 2"

                Expect.equal (declType tast) (TyTuple [ MockBuiltins.tyInt; MockBuiltins.tyInt ]) "p : int * int"

                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = (1, 2)" "TAST shape"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "triple of mixed types" {
                let tast = analyse "let t = 1, true, 2 + 3"

                Expect.equal
                    (declType tast)
                    (TyTuple [ MockBuiltins.tyInt; MockBuiltins.tyBool; MockBuiltins.tyInt ])
                    "t : int * bool * int"

                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = (1, true, (2 + 3))" "TAST shape"
            }

            test "tuple constrains element types via context" {
                // `n + 1` forces n : int. The tuple expression then has int * bool * int,
                // and the function-form binding gives `f : int -> int * bool * int`.
                let tast = analyse "let f n = n + 1, true, n"

                Expect.equal
                    (declType tast)
                    (TyFun(MockBuiltins.tyInt, TyTuple [ MockBuiltins.tyInt; MockBuiltins.tyBool; MockBuiltins.tyInt ]))
                    "f : int -> int * bool * int"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            // ---- Sequential ----

            test "`()` literal types as unit" {
                let tast = analyse "let x = ()"
                Expect.equal (declType tast) MockBuiltins.tyUnit "x : unit"
                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = ()" "TAST shape"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "sequential returns last expression's type" {
                // `(); x + 1` evaluates () (unit, discarded) then returns x + 1 (int).
                let tast = analyse "let f x = (); x + 1"
                let intToInt = TyFun(MockBuiltins.tyInt, MockBuiltins.tyInt)
                Expect.equal (declType tast) intToInt "f : int -> int"

                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = fun v1 -> ((); (v1 + 1))" "TAST shape"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "non-unit expression in non-tail position emits diagnostic" {
                // `1; 2` — first element is int, not unit. Triggers a mismatch.
                let tast = analyse "let r = 1; 2"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "non-unit head triggers mismatch"
            }

            // ---- TypeAnnotation ----

            test "(e : int) constrains e to int" {
                let tast = analyse "let f x = (x : int) + 1"
                let intToInt = TyFun(MockBuiltins.tyInt, MockBuiltins.tyInt)
                Expect.equal (declType tast) intToInt "f : int -> int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "annotation conflict triggers mismatch" {
                let tast = analyse "let b = (1 : bool)"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "int literal annotated as bool fails"
            }

            test "function-type annotation: ((f) : int -> int)" {
                // Note the extra parens around `fun x -> x` — without them
                // the `:` binds to `x`, giving `(int -> int) -> (int -> int)`.
                let tast = analyse "let g = ((fun x -> x) : int -> int)"
                let intToInt = TyFun(MockBuiltins.tyInt, MockBuiltins.tyInt)
                Expect.equal (declType tast) intToInt "g : int -> int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            // ---- let rec ----

            test "let rec: self-reference resolves" {
                // Without `rec`, the inner `f` would be unresolved (or shadowed).
                let tast = analyse "let rec f x = if x = 0 then 0 else f (x - 1)"
                let intToInt = TyFun(MockBuiltins.tyInt, MockBuiltins.tyInt)
                Expect.equal (declType tast) intToInt "f : int -> int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "let rec without rec keyword: self-reference unresolved" {
                let tast = analyse "let f x = if x = 0 then 0 else f (x - 1)"

                let hasUnresolved =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "Unresolved")

                Expect.isTrue hasUnresolved "inner f without rec doesn't resolve"
            }

            test "let rec … and …: mutual recursion both see each other" {
                let tast =
                    analyse
                        "let rec even n = if n = 0 then true else odd (n - 1)\nand odd n = if n = 0 then false else even (n - 1)"

                Expect.isEmpty tast.Diagnostics "no diagnostics for mutual recursion"

                Expect.equal tast.Decls.Length 2 "two decls"
            }

            test "occurs check rejects let rec f x = f" {
                // f's headPat tv unifies with TyFun(tvX, tv) — tv occurs in the RHS.
                let tast = analyse "let rec f x = f"

                let hasOccurs =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "Occurs check")

                Expect.isTrue hasOccurs "self-returning rec function triggers occurs check"
            }

            // ---- Combined ----

            test "compound: bool from && and comparison" {
                let tast = analyse "let inRange x = x > 0 && x < 100"
                let intToBool = TyFun(MockBuiltins.tyInt, MockBuiltins.tyBool)
                Expect.equal (declType tast) intToBool "inRange : int -> bool"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }
        ]
