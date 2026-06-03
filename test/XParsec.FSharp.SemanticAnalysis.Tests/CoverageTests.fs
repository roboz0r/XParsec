module XParsec.FSharp.SemanticAnalysis.Tests.CoverageTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyse MockBuiltins.provider input lexed file

let private declType (tast: TastFile) : SemType =
    // A surfaced `TDecl.Type` (rung 2: unions) is ignored here — these tests
    // assert the *value* binding's inferred type.
    let valueDecls =
        EqArray.toList tast.Decls
        |> List.filter (fun d ->
            match d with
            | TDecl.Type _ -> false
            | _ -> true
        )

    match valueDecls with
    | [ TDecl.Let(_, _, _, ty) ] -> ty
    | other -> failwithf "expected single TDecl.Let, got %A" other

[<Tests>]
let tests =
    testList
        "Coverage"
        [
            test "`x |> f` types as the result of applying f to x" {
                let tast = analyse "let f x = x + 1\nlet r = 1 |> f"

                let resultDecl =
                    match tast.Decls with
                    | EqList [ _; d ] -> d
                    | _ -> failwithf "expected two decls, got %A" tast.Decls

                match resultDecl with
                | TDecl.Let(_, _, _, ty) -> Expect.equal ty BuiltinTypes.tyInt "r : int"
                | other -> failtestf "unexpected: %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "pipe rendered as |> in TAST" {
                let tast = analyse "let f x = x + 1\nlet r = 1 |> f"

                let resultDecl =
                    match tast.Decls with
                    | EqList [ _; d ] -> d
                    | _ -> failwithf "expected two decls, got %A" tast.Decls

                Expect.stringContains (TastShape.prettyDecl resultDecl) "1 |>" "pipe rendered symbolically"
            }

            test "`f <| x` types same as `f x`" {
                let tast = analyse "let f x = x + 1\nlet r = f <| 1"

                let resultDecl =
                    match tast.Decls with
                    | EqList [ _; d ] -> d
                    | _ -> failwithf "expected two decls, got %A" tast.Decls

                match resultDecl with
                | TDecl.Let(_, _, _, ty) -> Expect.equal ty BuiltinTypes.tyInt "r : int"
                | other -> failtestf "unexpected: %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "pipe shape mismatch emits diagnostic" {
                // RHS of |> must be a function.
                let tast = analyse "let r = 1 |> 2"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "pipe of int |> int triggers mismatch"
            }

            test "`f >> g` types as 'a -> 'c" {
                let tast = analyse "let f x = x + 1\nlet g x = x * 2\nlet h = f >> g"

                let hDecl =
                    match tast.Decls with
                    | EqList [ _; _; d ] -> d
                    | _ -> failwithf "expected three decls, got %A" tast.Decls

                match hDecl with
                | TDecl.Let(_, _, _, ty) ->
                    Expect.equal ty (TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)) "h : int -> int"
                | other -> failtestf "unexpected: %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "`g << f` types as 'a -> 'c" {
                let tast = analyse "let f x = x + 1\nlet g x = x * 2\nlet h = g << f"

                let hDecl =
                    match tast.Decls with
                    | EqList [ _; _; d ] -> d
                    | _ -> failwithf "expected three decls, got %A" tast.Decls

                match hDecl with
                | TDecl.Let(_, _, _, ty) ->
                    Expect.equal ty (TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)) "h : int -> int"
                | other -> failtestf "unexpected: %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "compose with mismatched arms emits diagnostic" {
                // Middle types disagree (int result vs bool arg).
                let tast = analyse "let f x = x + 1\nlet g (b: bool) = 0\nlet h = f >> g"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "incompatible arms trigger mismatch"
            }

            test "`try body with | _ -> body2` types as body's type" {
                let tast = analyse "let r = try 1 with | _ -> 2"
                Expect.equal (declType tast) BuiltinTypes.tyInt "r : int"
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
                Expect.equal (declType tast) BuiltinTypes.tyInt "r : int"
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

            test "`x <- y` types as unit" {
                // The LHS must be a mutable binding (otherwise Validation flags it —
                // see ValidationTests).
                let tast = analyse "let r = let mutable x = 0 in x <- 1"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
                Expect.equal (declType tast) BuiltinTypes.tyUnit "r : unit"
            }

            test "assignment unifies left and right" {
                // x is int (constrained by + 0); RHS is bool → mismatch.
                let tast = analyse "let f x = x + 0; x <- true"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "LHS/RHS type mismatch reported"
            }

            test "assignment TAST shape" {
                let tast = analyse "let f x y = x <- y"

                Expect.stringContains (TastShape.prettyDecl tast.Decls.[0]) "<-" "renders <-"
            }

            test "typed pattern annotates parameter type" {
                let tast = analyse "let f (x: int) = x"
                let expected = TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)
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
                let intTy = BuiltinTypes.tyInt
                let expected = TyFun(TyTuple(EqArray.ofList [ intTy; intTy ]), intTy)
                Expect.equal (declType tast) expected "f : int * int -> int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "or-pattern with literal alternatives" {
                let tast = analyse "let f x = match x with | 0 | 1 -> true | _ -> false"
                let intToBool = TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyBool)
                Expect.equal (declType tast) intToBool "f : int -> bool"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "or-pattern with incompatible alternatives is diagnosed" {
                // int vs bool patterns can't unify.
                let tast = analyse "let f x = match x with | 0 | true -> 1 | _ -> 2"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "or-pattern type mismatch reported"
            }

            test "unknown qualified name still emits diagnostic" {
                let tast = analyse "let r = Foo.bar"

                let hasUnresolved =
                    tast.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "Unresolved qualified name")

                Expect.isTrue hasUnresolved "qualified diagnostic emitted"
            }

            test "high-precedence app `f(x)` types like `f x`" {
                let tast = analyse "let f x = x + 1\nlet r = f(3)"

                let resultDecl =
                    match tast.Decls with
                    | EqList [ _; d ] -> d
                    | _ -> failwithf "expected two decls, got %A" tast.Decls

                match resultDecl with
                | TDecl.Let(_, _, _, ty) -> Expect.equal ty BuiltinTypes.tyInt "r : int"
                | other -> failtestf "unexpected: %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "high-precedence app TAST shape collapses to App" {
                let tast = analyse "let f x = x + 1\nlet r = f(3)"

                let resultDecl =
                    match tast.Decls with
                    | EqList [ _; d ] -> d
                    | _ -> failwithf "expected two decls, got %A" tast.Decls

                Expect.equal (TastShape.prettyDecl resultDecl) "let v0 = (v1 3)" "f(3) renders as (f 3)"
            }

            test "high-precedence app on non-function emits mismatch" {
                let tast = analyse "let r = 1(2)"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "applying an int triggers mismatch"
            }

            test "`1..10` types as seq<int>" {
                let tast = analyse "let r = 1..10"
                Expect.equal (declType tast) BuiltinTypes.tySeqInt "r : seq<int>"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "stepped range `1..2..10` types as seq<int>" {
                let tast = analyse "let r = 1..2..10"
                Expect.equal (declType tast) BuiltinTypes.tySeqInt "r : seq<int>"
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
                let intToInt = TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)
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

            test "`for i in 1..10 do ()` types as unit with no diagnostic" {
                let tast = analyse "let r = for i in 1..10 do ()"
                Expect.equal (declType tast) BuiltinTypes.tyUnit "r : unit"
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
                Expect.equal (declType tast) BuiltinTypes.tyUnit "r : unit"
                Expect.isEmpty tast.Diagnostics "parens around range don't disable the special case"
            }

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

            test "`let () = ()` types and translates" {
                let tast = analyse "let () = ()"
                Expect.equal (declType tast) BuiltinTypes.tyUnit "binding type is unit"
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
                let unitToInt = TyFun(BuiltinTypes.tyUnit, BuiltinTypes.tyInt)
                Expect.equal (declType tast) unitToInt "f : unit -> int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "empty-block pattern translates to TPat.Const Unit" {
                let tast = analyse "let f x = match x with | () -> 0"

                match tast.Decls.[0] with
                | TDecl.Let(_,
                            TExpr.Lambda(_,
                                         TExpr.Match(_,
                                                     EqList [ {
                                                                  Pat = TPat.Const(TConstValue.Unit, _)
                                                              } ],
                                                     _),
                                         _),
                            _,
                            _) -> ()
                | other -> failtestf "unexpected: %A" other
            }

            test "record literal TAST shape" {
                let tast = analyse "type R = { X: int; Y: int }\nlet r = { X = 1; Y = 2 }"

                // Records-plan §B1: `type R = { … }` now surfaces as a `TDecl.Type`
                // (the front-end gap codegen-plan §B1 closed), so the value binding
                // is the *second* decl.
                let resultDecl =
                    match tast.Decls with
                    | EqList [ _; d ] -> d
                    | _ -> failwithf "expected [type; let], got %A" tast.Decls

                Expect.equal (TastShape.prettyDecl resultDecl) "let v0 = { X = 1; Y = 2 }" "record cons shape"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "field access TAST shape" {
                let tast = analyse "type R = { X: int }\nlet f (r: R) = r.X"

                let resultDecl =
                    match tast.Decls with
                    | EqList [ _; d ] -> d
                    | _ -> failwithf "expected [type; let], got %A" tast.Decls

                Expect.equal (TastShape.prettyDecl resultDecl) "let v0 = fun v1 -> v1.X" "field get shape"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "field assignment TAST shape" {
                let tast = analyse "type R = { mutable X: int }\nlet f (r: R) = r.X <- 5"

                let resultDecl =
                    match tast.Decls with
                    | EqList [ _; d ] -> d
                    | _ -> failwithf "expected [type; let], got %A" tast.Decls

                Expect.equal (TastShape.prettyDecl resultDecl) "let v0 = fun v1 -> v1.X <- 5" "field set shape"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "record clone TAST shape" {
                let tast =
                    analyse "type R = { X: int; Y: int }\nlet p = { X = 1; Y = 2 }\nlet q = { p with Y = 5 }"

                // The type decl now surfaces too (records-plan §B1) — [type; p; q].
                let qDecl =
                    match tast.Decls with
                    | EqList [ _; _; d ] -> d
                    | _ -> failwithf "expected [type; p; q], got %A" tast.Decls

                Expect.equal (TastShape.prettyDecl qDecl) "let v0 = { v1 with Y = 5 }" "record clone shape"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "record pattern TAST shape" {
                let tast =
                    analyse "type R = { X: int; Y: int }\nlet f r = match r with | { X = x; Y = y } -> x + y"

                let resultDecl =
                    match tast.Decls with
                    | EqList [ _; d ] -> d
                    | _ -> failwithf "expected [type; let], got %A" tast.Decls

                Expect.stringContains (TastShape.prettyDecl resultDecl) "{ X = " "record pattern rendered"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "nullary ctor TAST shape" {
                let tast = analyse "type S = | Point\nlet p = Point"

                // The union surfaces as a `TDecl.Type` (rung 2); the value binding follows.
                let resultDecl =
                    match tast.Decls with
                    | EqList [ _; d ] -> d
                    | _ -> failwithf "expected [type; let], got %A" tast.Decls

                Expect.equal (TastShape.prettyDecl resultDecl) "let v0 = Point" "nullary ctor shape"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "single-arg ctor TAST shape" {
                let tast = analyse "type S = | Circle of float\nlet c = Circle 1.0"

                let resultDecl =
                    match tast.Decls with
                    | EqList [ _; d ] -> d
                    | _ -> failwithf "expected [type; let], got %A" tast.Decls

                Expect.equal (TastShape.prettyDecl resultDecl) "let v0 = Circle 1" "single-arg ctor shape"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "multi-arg ctor TAST shape" {
                let tast = analyse "type S = | Rect of float * float\nlet r = Rect(2.0, 3.0)"

                let resultDecl =
                    match tast.Decls with
                    | EqList [ _; d ] -> d
                    | _ -> failwithf "expected [type; let], got %A" tast.Decls

                Expect.equal (TastShape.prettyDecl resultDecl) "let v0 = Rect(2, 3)" "multi-arg ctor shape"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "ctor pattern TAST shape" {
                let tast =
                    analyse
                        "type S = | Circle of float | Point\nlet area s = match s with | Circle r -> r | Point -> 0.0"

                let resultDecl =
                    match tast.Decls with
                    | EqList [ _; d ] -> d
                    | _ -> failwithf "expected [type; let], got %A" tast.Decls

                let rendered = TastShape.prettyDecl resultDecl
                Expect.stringContains rendered "Circle v" "Circle r arm rendered"
                Expect.stringContains rendered "Point" "Point arm rendered"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "generic record literal carries arg-bearing type" {
                let tast = analyse "type Box<'a> = { Value: 'a }\nlet b = { Value = 1 }"
                Expect.equal (declType tast) (TyRecord("Box", EqArray.singleton BuiltinTypes.tyInt)) "b : Box<int>"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "generic ctor application carries arg-bearing type" {
                let tast = analyse "type Option<'a> = | Some of 'a | None\nlet s = Some 1"
                Expect.equal (declType tast) (TyUnion("Option", EqArray.singleton BuiltinTypes.tyInt)) "s : Option<int>"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "generic record-field access resolves via substitution" {
                let tast = analyse "type Box<'a> = { Value: 'a }\nlet f (b : Box<int>) = b.Value"

                let expected =
                    TyFun(TyRecord("Box", EqArray.singleton BuiltinTypes.tyInt), BuiltinTypes.tyInt)

                Expect.equal (declType tast) expected "f : Box<int> -> int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "TAST: monomorphic abbreviation erases to underlying type" {
                let tast = analyse "type Name = string\nlet n : Name = \"x\""
                Expect.equal (declType tast) BuiltinTypes.tyString "declType is string"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "TAST: generic abbreviation literal expands to tuple" {
                let tast = analyse "type Pair<'a> = 'a * 'a\nlet p : Pair<int> = (1, 2)"
                let expected = TyTuple(EqArray.ofList [ BuiltinTypes.tyInt; BuiltinTypes.tyInt ])
                Expect.equal (declType tast) expected "declType is int * int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "TAST: `new Point(3, 4)` shapes as TExpr.New" {
                let tast =
                    analyse "type Point(x: int, y: int) =\n    member this.X = x\nlet p = new Point(3, 4)"

                let valExpr =
                    tast.Decls
                    |> EqArray.toList
                    |> List.tryPick (
                        function
                        | TDecl.Let(_, v, _, _) -> Some v
                        | _ -> None
                    )
                    |> Option.defaultWith (fun () -> failwithf "expected a let, got %A" tast.Decls)

                match valExpr with
                | TExpr.New(name, args, ty) ->
                    Expect.equal name "Point" "class name"
                    Expect.equal args.Length 2 "two ctor args"
                    Expect.equal ty (TyClass("Point", EqArray.empty)) "ty is TyClass Point"
                | _ -> failtestf "expected TExpr.New, got %A" valExpr

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "TAST: property read shapes as PropertyGet" {
                let tast =
                    analyse "type Point(x: int, y: int) =\n    member this.X = x\nlet f (p : Point) = p.X"

                let valExpr =
                    tast.Decls
                    |> EqArray.toList
                    |> List.tryPick (
                        function
                        | TDecl.Let(_, v, _, _) -> Some v
                        | _ -> None
                    )
                    |> Option.defaultWith (fun () -> failwithf "expected a let, got %A" tast.Decls)

                let body =
                    match valExpr with
                    | TExpr.Lambda(_, body, _) -> body
                    | _ -> failtestf "expected TExpr.Lambda, got %A" valExpr

                match body with
                | TExpr.PropertyGet(_, key, _, ty) ->
                    Expect.equal (SymbolKeyOps.simpleName key) "X" "property name"
                    Expect.equal ty BuiltinTypes.tyInt "property type"
                | _ -> failtestf "expected PropertyGet, got %A" body

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "TAST: method call shapes as MethodCall" {
                let tast =
                    analyse
                        "type Point(x: int, y: int) =\n    member this.Magnitude () = x * x + y * y\nlet m (p : Point) = p.Magnitude()"

                let valExpr =
                    tast.Decls
                    |> EqArray.toList
                    |> List.tryPick (
                        function
                        | TDecl.Let(_, v, _, _) -> Some v
                        | _ -> None
                    )
                    |> Option.defaultWith (fun () -> failwithf "expected a let, got %A" tast.Decls)

                let body =
                    match valExpr with
                    | TExpr.Lambda(_, body, _) -> body
                    | _ -> failtestf "expected TExpr.Lambda, got %A" valExpr

                match body with
                | TExpr.MethodCall(_, key, _, args, ty) ->
                    Expect.equal (SymbolKeyOps.simpleName key) "Magnitude" "method name"
                    Expect.equal args.Length 0 "no args (unit-arg fold)"
                    Expect.equal ty BuiltinTypes.tyInt "method return"
                | _ -> failtestf "expected MethodCall, got %A" body

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "TAST: `C.Origin` shapes as StaticPropertyGet" {
                let tast = analyse "type C() =\n    static member Origin = 42\nlet o = C.Origin"

                let valExpr =
                    tast.Decls
                    |> EqArray.toList
                    |> List.tryPick (
                        function
                        | TDecl.Let(_, v, _, _) -> Some v
                        | _ -> None
                    )
                    |> Option.defaultWith (fun () -> failwithf "expected a let, got %A" tast.Decls)

                match valExpr with
                | TExpr.StaticPropertyGet(SymbolKey.MemberKey(decl, name, _, _), ty) ->
                    Expect.equal (SymbolKeyOps.simpleName decl) "C" "class name"
                    Expect.equal name "Origin" "property name"
                    Expect.equal ty BuiltinTypes.tyInt "ty is int"
                | _ -> failtestf "expected StaticPropertyGet, got %A" valExpr

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "TAST: `C.M(1)` shapes as StaticMethodCall" {
                let tast =
                    analyse "type C() =\n    static member M (x: int) = x + 1\nlet r = C.M(1)"

                let valExpr =
                    tast.Decls
                    |> EqArray.toList
                    |> List.tryPick (
                        function
                        | TDecl.Let(_, v, _, _) -> Some v
                        | _ -> None
                    )
                    |> Option.defaultWith (fun () -> failwithf "expected a let, got %A" tast.Decls)

                match valExpr with
                | TExpr.StaticMethodCall(SymbolKey.MemberKey(decl, methodName, _, _), args, ty) ->
                    Expect.equal (SymbolKeyOps.simpleName decl) "C" "class name"
                    Expect.equal methodName "M" "method name"
                    Expect.equal args.Length 1 "one arg"
                    Expect.equal ty BuiltinTypes.tyInt "method return"
                | _ -> failtestf "expected StaticMethodCall, got %A" valExpr

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            // Step 1.4 of vesper-set-sprint-plan: `TypeDefn.Class` surfaces as
            // `TDecl.Type` with `TTypeKind.Class` carrying the ctor params and
            // member list. `fields`/`baseType`/`interfaces` slots stay empty in B-1.
            test "TAST: class surfaces as TTypeKind.Class" {
                let tast =
                    analyse "type Point(x: int, y: int) =\n    member this.Magnitude () = x * x + y * y"

                let typeDecl =
                    tast.Decls
                    |> EqArray.toList
                    |> List.tryPick (
                        function
                        | TDecl.Type t -> Some t
                        | _ -> None
                    )
                    |> Option.defaultWith (fun () -> failwithf "expected a TDecl.Type, got %A" tast.Decls)

                Expect.equal typeDecl.Name "Point" "type name"
                Expect.equal typeDecl.TypeParams.Length 0 "no generic typars"

                match typeDecl.Kind with
                | TTypeKind.Class(fields,
                                  ctorParams,
                                  members,
                                  baseType,
                                  interfaces,
                                  isSealed,
                                  staticLets,
                                  secondaryCtors,
                                  baseCtorCall) ->
                    Expect.equal baseCtorCall ValueNone "no inherit clause ⇒ no base-ctor call"
                    Expect.equal fields.Length 0 "B-1 has no instance fields"
                    Expect.equal staticLets.Length 0 "no static lets on this class"
                    Expect.equal secondaryCtors.Length 0 "no secondary ctors on this class"
                    Expect.equal ctorParams.Length 2 "two ctor params"
                    Expect.equal (ctorParams.[0].Name) "x" "first param name"
                    Expect.equal (ctorParams.[0].Type) BuiltinTypes.tyInt "first param type"
                    Expect.equal (ctorParams.[1].Name) "y" "second param name"
                    Expect.equal (ctorParams.[1].Type) BuiltinTypes.tyInt "second param type"
                    Expect.equal members.Length 1 "one member"
                    Expect.equal (members.[0].Name) "Magnitude" "member name"
                    Expect.isFalse (members.[0].IsStatic) "instance member"
                    Expect.equal (members.[0].Kind) TMemberKind.Method "method kind"
                    Expect.equal (members.[0].ReturnTy) BuiltinTypes.tyInt "method returns int"
                    Expect.equal baseType ValueNone "B-1 leaves baseType ValueNone"
                    Expect.equal interfaces.Length 0 "B-1 has no interface impls"
                    Expect.isFalse isSealed "no [<Sealed>] ⇒ not sealed"
                | other -> failtestf "expected TTypeKind.Class, got %A" other

                Expect.equal typeDecl.EqualitySupport EqualityVerdict.Reference "classes default to reference equality"
                Expect.equal typeDecl.ComparisonSupport ComparisonVerdict.NoComparison "no comparison"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "TAST: generic class surfaces with declaring typars" {
                let tast = analyse "type Box<'a>(value: 'a) =\n    member this.Value = value"

                let typeDecl =
                    tast.Decls
                    |> EqArray.toList
                    |> List.tryPick (
                        function
                        | TDecl.Type t -> Some t
                        | _ -> None
                    )
                    |> Option.defaultWith (fun () -> failwithf "expected a TDecl.Type, got %A" tast.Decls)

                Expect.equal typeDecl.Name "Box" "type name"
                Expect.equal (EqArray.toList typeDecl.TypeParams) [ "'a" ] "one declared typar"

                match typeDecl.Kind with
                | TTypeKind.Class(_, ctorParams, members, _, _, _, _, _, _) ->
                    Expect.equal ctorParams.Length 1 "one ctor param"
                    Expect.equal (ctorParams.[0].Name) "value" "ctor param name"
                    // The declaring typar freezes to `TempTypar(Declaring, 0)`, which
                    // the backend reads as a `GenericTypeParameter` index.
                    Expect.equal (ctorParams.[0].Type) (TempTypar(TyparAxis.Declaring, 0)) "ctor param type marker"
                    Expect.equal members.Length 1 "one member"
                    Expect.equal (members.[0].Name) "Value" "member name"
                    Expect.equal (members.[0].ReturnTy) (TempTypar(TyparAxis.Declaring, 0)) "member returns the typar"
                | other -> failtestf "expected TTypeKind.Class, got %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            // vesper-set-sprint-plan §1.8 / B-10: a `static let` surfaces in
            // `TTypeKind.Class.staticLets` with its inferred type, and a member
            // reference to it lowers to `TExpr.StaticFieldGet`.
            test "TAST: `static let` surfaces in TTypeKind.Class.staticLets" {
                let tast = analyse "type C() =\n    static let x = 42\n    static member Get () = x"

                let typeDecl =
                    tast.Decls
                    |> EqArray.toList
                    |> List.tryPick (
                        function
                        | TDecl.Type t -> Some t
                        | _ -> None
                    )
                    |> Option.defaultWith (fun () -> failwithf "expected a TDecl.Type, got %A" tast.Decls)

                match typeDecl.Kind with
                | TTypeKind.Class(_, _, members, _, _, _, staticLets, _, _) ->
                    Expect.equal staticLets.Length 1 "one static let"
                    Expect.equal (staticLets.[0].Name) "x" "static-let name"
                    Expect.equal (staticLets.[0].Type) BuiltinTypes.tyInt "static-let type inferred to int"

                    // The `Get` member body reads the static field.
                    let getBody = members.[0].Body

                    match getBody with
                    | TExpr.StaticFieldGet(declKey, name, _) ->
                        Expect.equal (SymbolKeyOps.simpleName declKey) "C" "static-field class"
                        Expect.equal name "x" "static-field name"
                    | other -> failtestf "expected StaticFieldGet body, got %A" other
                | other -> failtestf "expected TTypeKind.Class, got %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            // The per-instantiation generic-static-let lowering is deferred
            // (vesper-set-sprint-plan §1.8 risk register): a `static let` on a
            // generic class is diagnosed and dropped.
            test "TAST: `static let` on a generic class is diagnosed (deferred)" {
                let tast =
                    analyse "type Box<'a>() =\n    static let x = 42\n    static member Get () = x"

                Expect.isNonEmpty tast.Diagnostics "generic static let is rejected"

                Expect.isTrue
                    (tast.Diagnostics
                     |> List.exists (fun d -> d.Message.Contains "static let" && d.Message.Contains "generic"))
                    "diagnostic mentions the deferred generic static let"
            }

            // vesper-set-sprint-plan §1.9 / B-11: a `new(...)` overload surfaces in
            // `TTypeKind.Class.secondaryCtors` with its params and the primary-ctor
            // chain arguments; the primary ctor list is unaffected.
            test "TAST: secondary constructor surfaces in TTypeKind.Class.secondaryCtors" {
                let tast = analyse "type C(x: int) =\n    new() = C(0)\n    member this.X = x"

                let typeDecl =
                    tast.Decls
                    |> EqArray.toList
                    |> List.tryPick (
                        function
                        | TDecl.Type t -> Some t
                        | _ -> None
                    )
                    |> Option.defaultWith (fun () -> failwithf "expected a TDecl.Type, got %A" tast.Decls)

                match typeDecl.Kind with
                | TTypeKind.Class(_, ctorParams, _, _, _, _, _, secondaryCtors, _) ->
                    Expect.equal ctorParams.Length 1 "primary ctor has one param"
                    Expect.equal secondaryCtors.Length 1 "one secondary ctor"
                    Expect.equal (secondaryCtors.[0].Params.Length) 0 "new() takes no params"
                    Expect.equal (secondaryCtors.[0].Lets.Length) 0 "no let-preamble"
                    Expect.equal (secondaryCtors.[0].PrimaryArgs.Length) 1 "chain passes one arg to the primary ctor"
                | other -> failtestf "expected TTypeKind.Class, got %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            // Inheritance-plan test 22 (Step 2.6): a `base.M(...)` access carries
            // the `CallVia.Base` discriminator so codegen emits a non-virtual
            // `call` against the parent slot; an ordinary `this.M(...)` stays
            // `CallVia.Self`.
            test "TAST: `base.M ()` carries CallVia.Base" {
                let tast =
                    analyse
                        "type Base() =\n    member this.M () = 1\ntype Derived() =\n    inherit Base()\n    override this.M () = base.M() + 1"

                let vias = ResizeArray<CallVia>()

                let collect =
                    { TastWalk.identityIter with
                        VisitExpr =
                            fun _ e ->
                                match e with
                                | TExpr.MethodCall(_, key, via, _, _) when SymbolKeyOps.simpleName key = "M" ->
                                    vias.Add via
                                | _ -> ()

                                true
                    }

                for d in EqArray.toList tast.Decls do
                    match d with
                    | TDecl.Type t ->
                        match t.Kind with
                        | TTypeKind.Class(_, _, members, _, _, _, _, _, _) ->
                            for m in EqArray.toList members do
                                TastWalk.iterExpr collect m.Body
                        | _ -> ()
                    | _ -> ()

                Expect.contains vias CallVia.Base "the base.M() call dispatches via CallVia.Base"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "qualified name resolves through provider" {
                let provider: IExternalSymbolProvider =
                    { new IExternalSymbolProvider with
                        member _.TryLookup name =
                            if name = "Math.pi" then
                                ValueSome(ExternalSymbols.mono name BuiltinTypes.tyFloat)
                            else
                                MockBuiltins.provider.TryLookup name

                        member _.TryLookupType _ = ValueNone
                        member _.TryLookupMember(_, _) = ValueNone
                        member _.TryLookupMembers(_, _) = [||]
                        member _.TryLookupUnionCase _ = ValueNone
                        member _.AmbientOpenPrefixes = []
                        member _.TryLookupInlineBody _ = ValueNone
                        member _.TryLookupInlineBodyByName _ = ValueNone
                    }

                let input = "let r = Math.pi"
                let lexed, file = parseFile input
                let tast = Pipeline.analyse provider input lexed file

                Expect.equal (declType tast) BuiltinTypes.tyFloat "r : float"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            // --- Phase 2 / B-4: cast TAST shapes (Step 2.4) ---

            test "TAST: `:>` shapes as Upcast" {
                let tast =
                    analyse "type B() =\n    member this.X = 1\ntype D() =\n    inherit B()\nlet s = (new D()) :> B"

                let last = EqArray.toList tast.Decls |> List.last
                Expect.stringContains (TastShape.prettyDecl last) ":> B" "upcast rendered"
            }

            test "TAST: `:?>` shapes as Downcast" {
                let tast =
                    analyse
                        "type B() =\n    member this.X = 1\ntype D() =\n    inherit B()\nlet d = ((new D()) :> B) :?> D"

                let last = EqArray.toList tast.Decls |> List.last
                Expect.stringContains (TastShape.prettyDecl last) ":?> D" "downcast rendered"
            }

            test "TAST: `:?` shapes as TypeTest" {
                let tast =
                    analyse
                        "type B() =\n    member this.X = 1\ntype D() =\n    inherit B()\nlet t = ((new D()) :> B) :? D"

                let last = EqArray.toList tast.Decls |> List.last
                Expect.stringContains (TastShape.prettyDecl last) ":? D" "type test rendered"
            }
        ]
