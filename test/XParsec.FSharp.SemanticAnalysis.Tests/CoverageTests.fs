module XParsec.FSharp.SemanticAnalysis.Tests.CoverageTests

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
        "Coverage"
        [
            // ---- Pipe operators ----

            test "`x |> f` types as the result of applying f to x" {
                let tast = analyse "let f x = x + 1\nlet r = 1 |> f"

                let resultDecl =
                    match tast.Decls with
                    | [ _; d ] -> d
                    | other -> failwithf "expected two decls, got %A" other

                match resultDecl with
                | TDecl.Let(_, _, ty) -> Expect.equal ty MockBuiltins.tyInt "r : int"
                | other -> failtestf "unexpected: %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "pipe rendered as |> in TAST" {
                let tast = analyse "let f x = x + 1\nlet r = 1 |> f"

                let resultDecl =
                    match tast.Decls with
                    | [ _; d ] -> d
                    | other -> failwithf "expected two decls, got %A" other

                Expect.stringContains (TastShape.prettyDecl resultDecl) "1 |>" "pipe rendered symbolically"
            }

            test "`f <| x` types same as `f x`" {
                let tast = analyse "let f x = x + 1\nlet r = f <| 1"

                let resultDecl =
                    match tast.Decls with
                    | [ _; d ] -> d
                    | other -> failwithf "expected two decls, got %A" other

                match resultDecl with
                | TDecl.Let(_, _, ty) -> Expect.equal ty MockBuiltins.tyInt "r : int"
                | other -> failtestf "unexpected: %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "pipe shape mismatch emits diagnostic" {
                // `1 |> 2` — right-hand side must be a function.
                let tast = analyse "let r = 1 |> 2"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "pipe of int |> int triggers mismatch"
            }

            // ---- Compose operators ----

            test "`f >> g` types as 'a -> 'c" {
                // f : int -> int, g : int -> int, so f >> g : int -> int.
                let tast = analyse "let f x = x + 1\nlet g x = x * 2\nlet h = f >> g"

                let hDecl =
                    match tast.Decls with
                    | [ _; _; d ] -> d
                    | other -> failwithf "expected three decls, got %A" other

                match hDecl with
                | TDecl.Let(_, _, ty) ->
                    Expect.equal ty (TyFun(MockBuiltins.tyInt, MockBuiltins.tyInt)) "h : int -> int"
                | other -> failtestf "unexpected: %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "`g << f` types as 'a -> 'c" {
                let tast = analyse "let f x = x + 1\nlet g x = x * 2\nlet h = g << f"

                let hDecl =
                    match tast.Decls with
                    | [ _; _; d ] -> d
                    | other -> failwithf "expected three decls, got %A" other

                match hDecl with
                | TDecl.Let(_, _, ty) ->
                    Expect.equal ty (TyFun(MockBuiltins.tyInt, MockBuiltins.tyInt)) "h : int -> int"
                | other -> failtestf "unexpected: %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "compose with mismatched arms emits diagnostic" {
                // f : int -> int, g : bool -> int — middle types don't agree.
                let tast = analyse "let f x = x + 1\nlet g (b: bool) = 0\nlet h = f >> g"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "incompatible arms trigger mismatch"
            }

            // ---- TryWith / TryFinally ----

            test "`try body with | _ -> body2` types as body's type" {
                let tast = analyse "let r = try 1 with | _ -> 2"
                Expect.equal (declType tast) MockBuiltins.tyInt "r : int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "try-with arm-type mismatch is diagnosed" {
                let tast = analyse "let r = try 1 with | _ -> true"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "incompatible arms trigger mismatch"
            }

            test "`try body finally cleanup`" {
                let tast = analyse "let r = try 1 finally ()"
                Expect.equal (declType tast) MockBuiltins.tyInt "r : int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "try-finally cleanup must be unit" {
                let tast = analyse "let r = try 1 finally 2"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "non-unit cleanup triggers mismatch"
            }

            test "try-with TAST shape" {
                let tast = analyse "let r = try 1 with | _ -> 2"

                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = try 1 with | _ -> 2" "try-with shape"
            }

            test "try-finally TAST shape" {
                let tast = analyse "let r = try 1 finally ()"

                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = try 1 finally ()" "try-finally shape"
            }

            // ---- Assignment ----

            test "`x <- y` types as unit" {
                // We use a function so the LHS and RHS both involve typed bindings.
                let tast = analyse "let f x y = x <- y"
                // Without mutability tracking, the type checker just enforces
                // LHS = RHS and the result is unit. So f : 'a -> 'a -> unit.
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match declType tast with
                | TyFun(_, TyFun(_, TyConst "unit")) -> ()
                | other -> failtestf "expected 'a -> 'a -> unit, got %A" other
            }

            test "assignment unifies left and right" {
                // x is int (constrained by + 0); RHS is bool — should mismatch.
                let tast = analyse "let f x = x + 0; x <- true"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "LHS/RHS type mismatch reported"
            }

            test "assignment TAST shape" {
                let tast = analyse "let f x y = x <- y"

                Expect.stringContains (TastShape.prettyDecl tast.Decls.[0]) "<-" "renders <-"
            }

            // ---- Pat.Typed ----

            test "typed pattern annotates parameter type" {
                let tast = analyse "let f (x: int) = x"
                let expected = TyFun(MockBuiltins.tyInt, MockBuiltins.tyInt)
                Expect.equal (declType tast) expected "f : int -> int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "typed pattern conflict triggers mismatch" {
                // x is int by annotation; using it as bool would mismatch.
                let tast = analyse "let f (x: int) = x + true"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "int param + bool triggers mismatch"
            }

            test "typed pattern in tuple: `let f ((x: int), b) = b`" {
                let tast = analyse "let f ((x: int), b) = x + b"
                let intTy = MockBuiltins.tyInt
                let expected = TyFun(TyTuple [ intTy; intTy ], intTy)
                Expect.equal (declType tast) expected "f : int * int -> int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            // ---- Pat.Or ----

            test "or-pattern with literal alternatives" {
                let tast = analyse "let f x = match x with | 0 | 1 -> true | _ -> false"
                let intToBool = TyFun(MockBuiltins.tyInt, MockBuiltins.tyBool)
                Expect.equal (declType tast) intToBool "f : int -> bool"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "or-pattern with incompatible alternatives is diagnosed" {
                // `0 | true` — int vs bool patterns can't unify.
                let tast = analyse "let f x = match x with | 0 | true -> 1 | _ -> 2"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "or-pattern type mismatch reported"
            }

            // ---- Multi-segment qualified names ----

            test "unknown qualified name still emits diagnostic" {
                let tast = analyse "let r = Foo.bar"

                let hasUnresolved =
                    tast.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "Unresolved qualified name")

                Expect.isTrue hasUnresolved "qualified diagnostic emitted"
            }

            // ---- HighPrecedenceApp ----

            test "high-precedence app `f(x)` types like `f x`" {
                let tast = analyse "let f x = x + 1\nlet r = f(3)"

                let resultDecl =
                    match tast.Decls with
                    | [ _; d ] -> d
                    | other -> failwithf "expected two decls, got %A" other

                match resultDecl with
                | TDecl.Let(_, _, ty) -> Expect.equal ty MockBuiltins.tyInt "r : int"
                | other -> failtestf "unexpected: %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "high-precedence app TAST shape collapses to App" {
                let tast = analyse "let f x = x + 1\nlet r = f(3)"

                let resultDecl =
                    match tast.Decls with
                    | [ _; d ] -> d
                    | other -> failwithf "expected two decls, got %A" other

                Expect.equal (TastShape.prettyDecl resultDecl) "let v0 = (v1 3)" "f(3) renders as (f 3)"
            }

            test "high-precedence app on non-function emits mismatch" {
                let tast = analyse "let r = 1(2)"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "applying an int triggers mismatch"
            }

            // ---- Range / SteppedRange ----

            test "`1..10` types as seq<int>" {
                let tast = analyse "let r = 1..10"
                Expect.equal (declType tast) MockBuiltins.tySeqInt "r : seq<int>"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "stepped range `1..2..10` types as seq<int>" {
                let tast = analyse "let r = 1..2..10"
                Expect.equal (declType tast) MockBuiltins.tySeqInt "r : seq<int>"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "range endpoints must be int" {
                let tast = analyse "let r = true..false"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "bool endpoints trigger mismatch"
            }

            test "range step must be int" {
                let tast = analyse "let r = 1..true..10"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "bool step triggers mismatch"
            }

            test "range constrains adjacent context: `let r = (1..n) ; n + 0` forces n : int" {
                // The variable `n` flows through both the range endpoint
                // (forcing int) and the use site `n + 0` (also int).
                let tast = analyse "let f n = let r = 1..n in n + 0"
                let intToInt = TyFun(MockBuiltins.tyInt, MockBuiltins.tyInt)
                Expect.equal (declType tast) intToInt "f : int -> int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "range TAST shape" {
                let tast = analyse "let r = 1..10"

                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = (1..10)" "range shape"
            }

            test "stepped range TAST shape" {
                let tast = analyse "let r = 1..2..10"

                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = (1..2..10)" "stepped range shape"
            }

            // ---- ForIn over ranges ----

            test "`for i in 1..10 do ()` types as unit with no diagnostic" {
                let tast = analyse "let r = for i in 1..10 do ()"
                Expect.equal (declType tast) MockBuiltins.tyUnit "r : unit"
                // The range special case suppresses the "not implemented" Info.
                Expect.isEmpty tast.Diagnostics "no diagnostics for range-source for-in"
            }

            test "for-in over range binds the pattern as int" {
                // Body uses `i` as int — would mismatch if pattern stayed unconstrained.
                let tast = analyse "let r = for i in 1..10 do let _ = i + 0 in ()"
                Expect.isEmpty tast.Diagnostics "no diagnostics; i is int"
            }

            test "for-in over non-range still emits Info" {
                let tast = analyse "let f xs = for x in xs do ()"

                let hasInfo = tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "for-in")

                Expect.isTrue hasInfo "non-range for-in still flags the gap"
            }

            test "for-in over parenthesised range still recognised" {
                let tast = analyse "let r = for i in (1..10) do ()"
                Expect.equal (declType tast) MockBuiltins.tyUnit "r : unit"
                Expect.isEmpty tast.Diagnostics "parens around range don't disable the special case"
            }

            // ---- Null ----

            test "`null` types as a free TypeVar" {
                let tast = analyse "let n = null"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match declType tast with
                | TyVar _ -> ()
                | other -> failtestf "expected free TypeVar, got %A" other
            }

            test "null TAST shape" {
                let tast = analyse "let n = null"
                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = null" "null shape"
            }

            // ---- Pat.EmptyBlock ----

            test "`let () = ()` types and translates" {
                let tast = analyse "let () = ()"
                Expect.equal (declType tast) MockBuiltins.tyUnit "binding type is unit"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "`let () = expr` rejects non-unit RHS" {
                let tast = analyse "let () = 1"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "non-unit RHS triggers mismatch"
            }

            test "match arm with `()` pattern types as unit" {
                let tast = analyse "let f x = match x with | () -> 0"
                let unitToInt = TyFun(MockBuiltins.tyUnit, MockBuiltins.tyInt)
                Expect.equal (declType tast) unitToInt "f : unit -> int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "empty-block pattern translates to TPat.Const Unit" {
                let tast = analyse "let f x = match x with | () -> 0"

                match tast.Decls.[0] with
                | TDecl.Let(_,
                            TExpr.Lambda(_,
                                         TExpr.Match(_,
                                                     [ {
                                                           Pat = TPat.Const(TConstValue.Unit, _)
                                                       } ],
                                                     _),
                                         _),
                            _) -> ()
                | other -> failtestf "unexpected: %A" other
            }

            test "qualified name resolves through provider" {
                // Build a custom provider that knows `Math.pi`.
                let provider: IExternalSymbolProvider =
                    { new IExternalSymbolProvider with
                        member _.TryLookup name =
                            if name = "Math.pi" then
                                ValueSome(ExternalSymbols.mono name MockBuiltins.tyFloat)
                            else
                                MockBuiltins.provider.TryLookup name
                    }

                let input = "let r = Math.pi"
                let lexed, file = parseFile input
                let tast = Pipeline.analyse provider input lexed file

                Expect.equal (declType tast) MockBuiltins.tyFloat "r : float"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }
        ]
