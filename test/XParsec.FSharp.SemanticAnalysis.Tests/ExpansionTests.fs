module XParsec.FSharp.SemanticAnalysis.Tests.ExpansionTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyse MockBuiltins.provider input lexed file

let private declType (tast: TastFile) : SemType =
    match tast.Decls with
    | EqList [ TDecl.Let(_, _, _, ty) ] -> ty
    | _ -> failwithf "expected single TDecl.Let, got %A" tast.Decls

[<Tests>]
let tests =
    testList
        "Expansion"
        [
            test "`<` types as int -> int -> bool, result is bool" {
                let tast = analyse "let b = 1 < 2"
                Expect.equal (declType tast) BuiltinTypes.tyBool "b : bool"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "`&&` between two bool literals" {
                let tast = analyse "let b = true && false"
                Expect.equal (declType tast) BuiltinTypes.tyBool "b : bool"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "`/` and `%` type as int -> int -> int" {
                let tast = analyse "let r = 10 / 3 % 2"
                Expect.equal (declType tast) BuiltinTypes.tyInt "r : int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "`=` on int operands types as bool" {
                let tast = analyse "let b = 1 = 1"
                Expect.equal (declType tast) BuiltinTypes.tyBool "b : bool"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "unary minus on int types as int" {
                // `-x` (where x is a variable) forces a PrefixApp; `-5` would
                // be folded into a negative literal by the parser.
                let tast = analyse "let f x = -x"
                let intToInt = TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)
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

            test "if true then 1 else 2 types as int" {
                let tast = analyse "let r = if true then 1 else 2"
                Expect.equal (declType tast) BuiltinTypes.tyInt "r : int"
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

                Expect.equal (declType tast) BuiltinTypes.tyInt "ITE result type"
            }

            test "if-then-else with non-trivial branches: let abs n = if n < 0 then -n else n" {
                let tast = analyse "let abs n = if n < 0 then -n else n"
                let intToInt = TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)
                Expect.equal (declType tast) intToInt "abs : int -> int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "pair of ints types as int * int" {
                let tast = analyse "let p = 1, 2"

                Expect.equal
                    (declType tast)
                    (TyTuple(EqArray.ofList [ BuiltinTypes.tyInt; BuiltinTypes.tyInt ]))
                    "p : int * int"

                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = (1, 2)" "TAST shape"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "triple of mixed types" {
                let tast = analyse "let t = 1, true, 2 + 3"

                Expect.equal
                    (declType tast)
                    (TyTuple(EqArray.ofList [ BuiltinTypes.tyInt; BuiltinTypes.tyBool; BuiltinTypes.tyInt ]))
                    "t : int * bool * int"

                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = (1, true, (2 + 3))" "TAST shape"
            }

            test "tuple constrains element types via context" {
                // `n + 1` forces n : int. The tuple expression then has int * bool * int,
                // and the function-form binding gives `f : int -> int * bool * int`.
                let tast = analyse "let f n = n + 1, true, n"

                Expect.equal
                    (declType tast)
                    (TyFun(
                        BuiltinTypes.tyInt,
                        TyTuple(EqArray.ofList [ BuiltinTypes.tyInt; BuiltinTypes.tyBool; BuiltinTypes.tyInt ])
                    ))
                    "f : int -> int * bool * int"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "`()` literal types as unit" {
                let tast = analyse "let x = ()"
                Expect.equal (declType tast) BuiltinTypes.tyUnit "x : unit"
                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = ()" "TAST shape"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "sequential returns last expression's type" {
                // `(); x + 1` evaluates () (unit, discarded) then returns x + 1 (int).
                let tast = analyse "let f x = (); x + 1"
                let intToInt = TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)
                Expect.equal (declType tast) intToInt "f : int -> int"

                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = fun v1 -> ((); (v1 + 1))" "TAST shape"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "non-unit expression in non-tail position emits diagnostic" {
                // First element is int, not unit → mismatch.
                let tast = analyse "let r = 1; 2"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "non-unit head triggers mismatch"
            }

            test "(e : int) constrains e to int" {
                let tast = analyse "let f x = (x : int) + 1"
                let intToInt = TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)
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
                let intToInt = TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)
                Expect.equal (declType tast) intToInt "g : int -> int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "let rec: self-reference resolves" {
                // Without `rec`, the inner `f` would be unresolved (or shadowed).
                let tast = analyse "let rec f x = if x = 0 then 0 else f (x - 1)"
                let intToInt = TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)
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

            test "compound: bool from && and comparison" {
                let tast = analyse "let inRange x = x > 0 && x < 100"
                let intToBool = TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyBool)
                Expect.equal (declType tast) intToBool "inRange : int -> bool"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "`while true do ()` types as unit" {
                let tast = analyse "let r = while true do ()"
                Expect.equal (declType tast) BuiltinTypes.tyUnit "r : unit"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = while true do ()" "while TAST shape"
            }

            test "while condition must be bool" {
                let tast = analyse "let r = while 1 do ()"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "int cond triggers mismatch"
            }

            test "while body must be unit" {
                let tast = analyse "let r = while true do 1"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "int body triggers mismatch"
            }

            test "`for i = 1 to 10 do ()` types as unit" {
                let tast = analyse "let r = for i = 1 to 10 do ()"
                Expect.equal (declType tast) BuiltinTypes.tyUnit "r : unit"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "for-to loop variable bound as int and visible in body" {
                // `n` is the function arg; the body uses both `n` and the loop var `i`.
                let tast = analyse "let f n = for i = 0 to n do ()"
                let intToUnit = TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyUnit)
                Expect.equal (declType tast) intToUnit "f : int -> unit"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "for-to body referencing loop var: `for i = 1 to 10 do i + 0 |> ignore` is too rich; use simpler check" {
                // Body uses `i` in a sequential whose first element constrains via unit.
                // `(); i = 0` — `i = 0` is bool, but the body needs unit, so fails.
                let tast = analyse "let r = for i = 1 to 10 do i = 0"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "non-unit body triggers mismatch"
            }

            test "for-to range must be int" {
                let tast = analyse "let r = for i = true to false do ()"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "bool range triggers mismatch"
            }

            test "float literal types as float" {
                let tast = analyse "let pi = 3.14"
                Expect.equal (declType tast) BuiltinTypes.tyFloat "pi : float"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.Const(TConstValue.Float v, _), _, _) ->
                    Expect.floatClose Accuracy.medium v 3.14 "value preserved"
                | other -> failtestf "unexpected: %A" other
            }

            test "int64 literal types as int64" {
                let tast = analyse "let big = 1L"
                Expect.equal (declType tast) BuiltinTypes.tyInt64 "big : int64"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.Const(TConstValue.Int64 1L, _), _, _) -> ()
                | other -> failtestf "unexpected: %A" other
            }

            test "byte literal types as byte" {
                let tast = analyse "let b = 255uy"
                Expect.equal (declType tast) BuiltinTypes.tyByte "b : byte"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.Const(TConstValue.Byte 255uy, _), _, _) -> ()
                | other -> failtestf "unexpected: %A" other
            }

            test "type mismatch: 1 + 1L (int + int64)" {
                let tast = analyse "let r = 1 + 1L"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "int + int64 triggers mismatch"
            }

            test "plain string literal types as string" {
                let tast = analyse "let s = \"hello\""
                Expect.equal (declType tast) BuiltinTypes.tyString "s : string"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.Const(TConstValue.String text, _), _, _) ->
                    Expect.equal text "hello" "string body preserved"
                | other -> failtestf "unexpected: %A" other
            }

            test "verbatim string literal types as string" {
                let tast = analyse "let s = @\"C:\\foo\""
                Expect.equal (declType tast) BuiltinTypes.tyString "s : string"
            }

            test "`fun _ -> 0` types as 'a -> int" {
                // The wildcard's TypeVar stays free, so the function type is
                // 'a -> int. We just check that the body returns int and no
                // diagnostics fired.
                let tast = analyse "let k = fun _ -> 0"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match declType tast with
                | TyFun(_, TyConst "int") -> ()
                | other -> failtestf "expected 'a -> int, got %A" other
            }

            test "`let f _ x = x + 1`: wildcard arg + named arg" {
                let tast = analyse "let f _ x = x + 1"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match declType tast with
                | TyFun(_, TyFun(TyConst "int", TyConst "int")) -> ()
                | other -> failtestf "expected 'a -> int -> int, got %A" other
            }

            test "lambda with tuple param: `fun (a, b) -> a + b`" {
                let tast = analyse "let f = fun (a, b) -> a + b"
                let intType = BuiltinTypes.tyInt
                let expected = TyFun(TyTuple(EqArray.ofList [ intType; intType ]), intType)
                Expect.equal (declType tast) expected "f : int * int -> int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = fun (v1, v2) -> (v1 + v2)" "TAST shape"
            }

            test "function-form let with tuple arg: `let f (a, b) = a * b`" {
                let tast = analyse "let f (a, b) = a * b"
                let intType = BuiltinTypes.tyInt
                let expected = TyFun(TyTuple(EqArray.ofList [ intType; intType ]), intType)
                Expect.equal (declType tast) expected "f : int * int -> int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "let-in with tuple-destructuring head: `let (a, b) = 1, 2 in a + b`" {
                let tast = analyse "let r = let (a, b) = 1, 2 in a + b"
                Expect.equal (declType tast) BuiltinTypes.tyInt "r : int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "tuple pattern with wildcard: `fun (_, b) -> b + 1`" {
                let tast = analyse "let f = fun (_, b) -> b + 1"
                // First element stays polymorphic — only second is constrained.
                match declType tast with
                | TyFun(TyTuple args, TyConst "int") when
                    args.Length = 2
                    && (
                        match args.[1] with
                        | TyConst "int" -> true
                        | _ -> false
                    )
                    ->
                    ()
                | other -> failtestf "expected 'a * int -> int, got %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "nested tuple pattern: `fun ((a, b), c) -> a + b + c`" {
                let tast = analyse "let f = fun ((a, b), c) -> a + b + c"
                let i = BuiltinTypes.tyInt

                let expected =
                    TyFun(TyTuple(EqArray.ofList [ TyTuple(EqArray.ofList [ i; i ]); i ]), i)

                Expect.equal (declType tast) expected "f : (int * int) * int -> int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "match on int with int patterns and int body types as int" {
                let tast = analyse "let f x = match x with | 0 -> 1 | _ -> 2"
                let intToInt = TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)
                Expect.equal (declType tast) intToInt "f : int -> int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "match arms must have compatible body types" {
                let tast = analyse "let f x = match x with | 0 -> 1 | _ -> true"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "int vs bool branches trigger mismatch"
            }

            test "match arm binding visible in body" {
                let tast = analyse "let f x = match x with | y -> y + 1"
                let intToInt = TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)
                Expect.equal (declType tast) intToInt "f : int -> int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "match guard must be bool" {
                // `y + 0` forces y : int, so the guard `y + 0` is int → must be bool → mismatch.
                let tast = analyse "let f x = match x with | y when y + 0 -> 1 | _ -> 0"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "non-bool guard triggers mismatch"
            }

            test "match arm bool guard typechecks" {
                let tast = analyse "let f x = match x with | y when y > 0 -> y | _ -> 0"
                let intToInt = TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)
                Expect.equal (declType tast) intToInt "f : int -> int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "match on tuple with tuple pattern" {
                let tast = analyse "let f p = match p with | (a, b) -> a + b"
                let i = BuiltinTypes.tyInt
                let expected = TyFun(TyTuple(EqArray.ofList [ i; i ]), i)
                Expect.equal (declType tast) expected "f : int * int -> int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "match on bool with bool patterns" {
                let tast = analyse "let toInt b = match b with | true -> 1 | false -> 0"
                let boolToInt = TyFun(BuiltinTypes.tyBool, BuiltinTypes.tyInt)
                Expect.equal (declType tast) boolToInt "toInt : bool -> int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "match TAST shape renders" {
                let tast = analyse "let f x = match x with | 0 -> 1 | _ -> 2"

                Expect.equal
                    (TastShape.prettyDecl tast.Decls.[0])
                    "let v0 = fun v1 -> match v1 with | 0 -> 1 | _ -> 2"
                    "Match TAST shape"
            }

            test "`function` shorthand: `function | 0 -> 1 | _ -> 2`" {
                let tast = analyse "let f = function | 0 -> 1 | _ -> 2"
                let intToInt = TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)
                Expect.equal (declType tast) intToInt "f : int -> int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "tuple pattern surfaces in TPat shape" {
                let tast = analyse "let f (a, b) = a + b"

                match tast.Decls.[0] with
                | TDecl.Let(TPat.Tuple(EqList [ TPat.NamedSimple(_, _); TPat.NamedSimple(_, _) ], _), _, _, _) ->
                    // For function-form let, `f`'s own pattern is NamedSimple and the
                    // tuple sits on the Lambda — not on the TDecl.Let.
                    failtest "did not expect TDecl.Let to be the tuple pattern itself"
                | TDecl.Let(TPat.NamedSimple _,
                            TExpr.Lambda(TPat.Tuple(EqList [ TPat.NamedSimple _; TPat.NamedSimple _ ], _), _, _),
                            _,
                            _) -> ()
                | other -> failtestf "unexpected: %A" other
            }
        ]
