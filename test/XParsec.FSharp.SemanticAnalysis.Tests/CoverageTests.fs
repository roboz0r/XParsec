module XParsec.FSharp.SemanticAnalysis.Tests.CoverageTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSemFor testCompiling realProvider.Value (LexedFile.ofText lexed) file

/// Every instance/static method-call key in the file's value bindings, in pre-order.
let private callKeys (tast: TastFile) : ResizeArray<SymbolKey> =
    let calls = ResizeArray<SymbolKey>()

    let it =
        { TastWalk.identityIter with
            VisitExpr =
                fun _ e ->
                    match e with
                    | TExpr.MethodCall(_, k, _, _, _, _)
                    | TExpr.StaticMethodCall(k, _, _, _, _) -> calls.Add k
                    | _ -> ()

                    true
        }

    for d in EqArray.toList tast.Decls do
        match d with
        | TDecl.Let(_, value, _, _) -> TastWalk.iterExpr it value
        | _ -> ()

    calls

let private soleClass (tast: TastFile) : TClass =
    let found =
        [
            for d in tast.Decls do
                match d with
                | TDecl.Type td ->
                    match td.Kind with
                    | TTypeKind.Class c -> yield c
                    | _ -> ()
                | _ -> ()
        ]

    match found with
    | [ c ] -> c
    | other -> failwithf "expected exactly one class declaration, got %d" (List.length other)

let private declType (tast: TastFile) : SemType =
    // A surfaced `TDecl.Type` is ignored: the assertion is about the value binding.
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

/// A range materialises no seq value, so it is legal ONLY as the direct source of a
/// `for i in a..b do` counted loop; a first-class range value, or a stepped range
/// (which has no counted lowering), is rejected at elaboration.
let private hasRangeValueError (tast: TastFile) =
    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "first-class value")

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

            test "pipe specializes the served |>" {
                let tast = analyse "let f x = x + 1\nlet r = 1 |> f"

                let piped =
                    tast.Specializations
                    |> Seq.exists (fun s -> SymbolKeyOps.simpleName s.Key.Template = DisplayName "op_PipeRight")

                Expect.isTrue piped "the pipe reached specialization under its own key"
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

            test "try-finally cleanup must be file" {
                let tast = analyse "let r = try 1 finally 2"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "non-file cleanup triggers mismatch"
            }

            test "try-with TAST shape" {
                let tast = analyse "let r = try 1 with | _ -> 2"

                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = try 1 with | _ -> 2" "try-with shape"
            }

            test "try-finally TAST shape" {
                let tast = analyse "let r = try 1 finally ()"

                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = try 1 finally ()" "try-finally shape"
            }

            test "`x <- y` types as file" {
                // The LHS must be a mutable binding, or validation flags the write.
                let tast = analyse "let r = let mutable x = 0 in x <- 1"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
                Expect.equal (declType tast) BuiltinTypes.tyUnit "r : unit"
            }

            test "assignment unifies left and right" {
                // The assignment pins x to bool, so `x + 0`'s trait search reaches bool
                // and finds no `+`: the diagnostic exists only because `<-` unified.
                let tast = analyse "let f x = x + 0; x <- true"

                let hasMismatch =
                    tast.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "The type 'bool' does not support the operator '+'")

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
                // x is int by annotation, so `x + true` searches bool for `+` and finds
                // no member.
                let tast = analyse "let f (x: int) = x + true"

                let hasMismatch =
                    tast.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "The type 'bool' does not support the operator '+'")

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

            // The feature gap is reported once, by the pass that cannot lower it. The
            // alternatives' bound variables are in scope, so the arm body does not also
            // raise an unresolved-identifier on the way there.
            test "or-pattern that binds names is the only diagnostic" {
                let tast =
                    analyse "let f p =\n    match p with\n    | (1, a) | (2, a) -> a\n    | _ -> 0\n"

                let msgs = tast.Diagnostics |> Seq.map (fun d -> d.Message) |> List.ofSeq

                Expect.equal
                    msgs
                    [ "not yet supported: or-patterns that bind names (e.g. `(1, x) | (2, x)`)" ]
                    "one diagnostic, naming the gap"
            }

            // The head is reported once, by the pass that resolves it. Its sub-patterns bind, so
            // the arm body does not also raise an unresolved-identifier on the way there.
            test "undefined pattern discriminator is the only diagnostic" {
                let tast = analyse "let f p =\n    match p with\n    | Foo x -> x\n    | _ -> 0\n"

                let msgs = tast.Diagnostics |> Seq.map (fun d -> d.Message) |> List.ofSeq

                Expect.equal msgs [ "The pattern discriminator 'Foo' is not defined" ] "one diagnostic, naming the head"
            }

            // A head binding nothing raised no diagnostic at all, so elaboration — which asserts
            // rather than diagnoses, and degrades only after a reported error — faulted instead.
            test "undefined pattern discriminator binding nothing is diagnosed, not faulted" {
                let tast = analyse "let f p =\n    match p with\n    | Foo.Bar -> 1\n    | _ -> 0\n"

                let msgs = tast.Diagnostics |> Seq.map (fun d -> d.Message) |> List.ofSeq

                Expect.equal
                    msgs
                    [ "The pattern discriminator 'Foo.Bar' is not defined" ]
                    "one diagnostic, naming the whole dotted head"
            }

            test "a union-case pattern's bound variables are still in scope" {
                let tast =
                    analyse
                        "type Shape = Circle of int | Square of int\nlet area s =\n    match s with\n    | Circle r -> r\n    | Square w -> w\n"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
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

            test "`for i in 1..10` counted loop is accepted (no range diagnostic)" {
                let tast = analyse "let f () = for i in 1..10 do ()"
                Expect.isFalse (hasRangeValueError tast) "a direct range for-in source is supported"
            }

            test "`1..10` as a value is rejected (no first-class range)" {
                let tast = analyse "let r = 1..10"
                Expect.isTrue (hasRangeValueError tast) "range bound to a value is unsupported"
            }

            test "stepped range `1..2..10` as a value is rejected" {
                let tast = analyse "let r = 1..2..10"
                Expect.isTrue (hasRangeValueError tast) "stepped range value is unsupported"
            }

            test "stepped range `for i in 1..2..10` is rejected (no counted lowering)" {
                let tast = analyse "let f () = for i in 1..2..10 do ()"
                Expect.isTrue (hasRangeValueError tast) "a stepped range has no counted lowering"
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
                // `n` is constrained by both the range endpoint and the use site `n + 0`, so
                // the endpoint constraint holds even though binding `r` is itself rejected.
                let tast = analyse "let f n = let r = 1..n in n + 0"
                let intToInt = TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)
                Expect.equal (declType tast) intToInt "f : int -> int"
                Expect.isTrue (hasRangeValueError tast) "binding the range to a value is unsupported"
            }

            test "range TAST shape" {
                let tast = analyse "let r = 1..10"

                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = (1..10)" "range shape"
            }

            test "stepped range TAST shape" {
                let tast = analyse "let r = 1..2..10"

                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = (1..2..10)" "stepped range shape"
            }

            test "`for i in 1..10 do ()` types as file with no diagnostic" {
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
                Expect.equal (declType tast) BuiltinTypes.tyUnit "binding type is file"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "`let () = expr` rejects non-file RHS" {
                let tast = analyse "let () = 1"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "non-file RHS triggers mismatch"
            }

            test "match arm with `()` pattern types as file" {
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
                                                                  Pat = TPat.Const(TConstValue.Unit, _, _)
                                                              } ],
                                                     _,
                                                     _),
                                         _,
                                         _),
                            _,
                            _) -> ()
                | other -> failtestf "unexpected: %A" other
            }

            test "record literal TAST shape" {
                let tast = analyse "type R = { X: int; Y: int }\nlet r = { X = 1; Y = 2 }"

                // `type R = { … }` surfaces as a `TDecl.Type`, so `r` is the second decl.
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

                // The type decl surfaces too — [type; p; q].
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

                // The union surfaces as a `TDecl.Type`; the value binding follows.
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
                | TExpr.New(name, _, args, ty, _) ->
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
                    | TExpr.Lambda(_, body, _, _) -> body
                    | _ -> failtestf "expected TExpr.Lambda, got %A" valExpr

                match body with
                | TExpr.PropertyGet(_, key, _, ty, _) ->
                    Expect.equal (SymbolKeyOps.simpleName key) (DisplayName "X") "property name"
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
                    | TExpr.Lambda(_, body, _, _) -> body
                    | _ -> failtestf "expected TExpr.Lambda, got %A" valExpr

                match body with
                | TExpr.MethodCall(_, key, _, args, ty, _) ->
                    Expect.equal (SymbolKeyOps.simpleName key) (DisplayName "Magnitude") "method name"
                    Expect.equal args.Length 0 "no args (file-arg fold)"
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
                | TExpr.StaticPropertyGet(SymbolKey.Member { Decl = decl; Name = name }, _, ty, _) ->
                    Expect.equal (SymbolKeyOps.bareName decl.Name) "C" "class name"
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
                | TExpr.StaticMethodCall(SymbolKey.Member { Decl = decl; Name = methodName }, _, args, ty, _) ->
                    Expect.equal (SymbolKeyOps.bareName decl.Name) "C" "class name"
                    Expect.equal methodName "M" "method name"
                    Expect.equal args.Length 1 "one arg"
                    Expect.equal ty BuiltinTypes.tyInt "method return"
                | _ -> failtestf "expected StaticMethodCall, got %A" valExpr

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "TAST: `Box<int>.P` carries the written declaring instantiation" {
                let tast =
                    analyse "type Box<'T>() =\n    static member P with get () : int = 5\nlet r = Box<int>.P"

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
                | TExpr.StaticPropertyGet(_, declArgs, _, _) ->
                    Expect.equal (EqArray.toList declArgs) [ BuiltinTypes.tyInt ] "declArgs is the written <int>"
                | _ -> failtestf "expected StaticPropertyGet, got %A" valExpr

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "TAST: `Box<string>.P <- 3` pins the setter's 'T to string, so the int write errors" {
                let tast =
                    analyse (
                        String.concat
                            "\n"
                            [
                                "type Box<'T>() ="
                                "    static member P with get () : int = 0 and set (w: 'T) = ()"
                                "Box<string>.P <- 3"
                            ]
                    )

                Expect.isTrue
                    (tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch"))
                    (sprintf "the written <string> rejects the int value, got %A" tast.Diagnostics)
            }

            test "TAST: a static access's written type-arg arity mismatch is diagnosed" {
                let tast =
                    analyse "type Box<'T>() =\n    static member P with get () : int = 5\nlet r = Box<int, string>.P"

                Expect.isTrue
                    (tast.Diagnostics
                     |> Seq.exists (fun d -> d.Message.Contains "expects 1 type argument(s) but got 2"))
                    (sprintf "written arity 2 against declared 1 diagnoses, got %A" tast.Diagnostics)
            }

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
                | TTypeKind.Class c ->
                    Expect.equal c.Fields.Length 0 "no instance fields on this class"
                    Expect.equal c.StaticPreamble.Length 0 "no static preamble on this class"
                    Expect.equal c.InstancePreamble.Length 0 "no instance preamble on this class"
                    Expect.equal c.SecondaryCtors.Length 0 "no secondary ctors on this class"
                    Expect.equal c.CtorParams.Length 2 "two ctor params"
                    Expect.equal (c.CtorParams.[0].Name) "x" "first param name"
                    Expect.equal (c.CtorParams.[0].Type) BuiltinTypes.tyInt "first param type"
                    Expect.equal (c.CtorParams.[1].Name) "y" "second param name"
                    Expect.equal (c.CtorParams.[1].Type) BuiltinTypes.tyInt "second param type"
                    Expect.equal c.Members.Length 1 "one member"
                    Expect.equal (c.Members.[0].Name) "Magnitude" "member name"
                    Expect.isFalse (c.Members.[0].IsStatic) "instance member"
                    Expect.equal (c.Members.[0].Kind) TMemberKind.Method "method kind"
                    Expect.equal (c.Members.[0].ReturnTy) BuiltinTypes.tyInt "method returns int"
                    Expect.isTrue c.Base.IsNone "no inherit clause ⇒ no base node"
                    Expect.equal c.Interfaces.Length 0 "no interface impls on this class"
                    Expect.isFalse c.Declared.IsSealed "no [<Sealed>] ⇒ not sealed"
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
                Expect.equal (EqArray.toList (TTypeParam.names typeDecl.TypeParams)) [ "'a" ] "one declared typar"

                match typeDecl.Kind with
                | TTypeKind.Class c ->
                    Expect.equal c.CtorParams.Length 1 "one ctor param"
                    Expect.equal (c.CtorParams.[0].Name) "value" "ctor param name"
                    Expect.equal (c.CtorParams.[0].Type) (TyTypar(TyparAxis.Declaring, 0)) "ctor param type marker"
                    Expect.equal c.Members.Length 1 "one member"
                    Expect.equal (c.Members.[0].Name) "Value" "member name"
                    Expect.equal (c.Members.[0].ReturnTy) (TyTypar(TyparAxis.Declaring, 0)) "member returns the typar"
                | other -> failtestf "expected TTypeKind.Class, got %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "TAST: `static let` surfaces in TTypeKind.Class.StaticPreamble" {
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
                | TTypeKind.Class c ->
                    match TPreambleEntryG.lets (EqArray.toList c.StaticPreamble) with
                    | [ sl ] ->
                        Expect.equal sl.Name "x" "static-let name"
                        Expect.equal sl.Type BuiltinTypes.tyInt "static-let type inferred to int"
                    | other -> failtestf "expected one static let, got %A" other

                    // The `Get` member body reads the static field.
                    let getBody = c.Members.[0].Body

                    match getBody with
                    | TExpr.StaticFieldGet(declKey, name, _, _) ->
                        Expect.equal (SymbolKeyOps.typeSimpleName declKey) (DisplayName "C") "static-field class"
                        Expect.equal name "x" "static-field name"
                    | other -> failtestf "expected StaticFieldGet body, got %A" other
                | other -> failtestf "expected TTypeKind.Class, got %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "TAST: `static let` on a generic class surfaces with no diagnostic" {
                let tast =
                    analyse "type Box<'a>() =\n    static let x = 42\n    static member Get () = x"

                Expect.isEmpty tast.Diagnostics "generic static let is accepted"

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
                | TTypeKind.Class c ->
                    match TPreambleEntryG.lets (EqArray.toList c.StaticPreamble) with
                    | [ sl ] -> Expect.equal sl.Name "x" "the generic class's `static let` surfaces in the preamble"
                    | other -> failtestf "expected one static let, got %A" other
                | other -> failtestf "expected TTypeKind.Class, got %A" other
            }

            // A preamble binding carries its argument patterns, so `let f x = …` binds a
            // FUNCTION value — `int -> int`, not an `int` whose value is `x + 1`.
            test "TAST: a preamble `let f x = …` surfaces as a function value" {
                let tast =
                    analyse "type C() =\n    static let f x = x + 1\n    static member Get () = f 1"

                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match soleClass tast with
                | c ->
                    match TPreambleEntryG.lets (EqArray.toList c.StaticPreamble) with
                    | [ sl ] ->
                        Expect.equal sl.Name "f" "static-let name"
                        Expect.equal sl.Type (TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)) "int -> int"

                        match sl.Init with
                        | TExpr.Lambda _ -> ()
                        | other -> failtestf "expected a Lambda initialiser, got %A" other
                    | other -> failtestf "expected one static let, got %A" other
            }

            // An instance `let` is a private instance field, the same lowering a primary-ctor
            // param gets: both its initialiser and every member read go through `this`.
            test "TAST: an instance `let` surfaces in InstancePreamble and lowers to a field" {
                let tast = analyse "type C(x: int) =\n    let a = x + 1\n    member _.A = a"

                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let c = soleClass tast

                match TPreambleEntryG.lets (EqArray.toList c.InstancePreamble) with
                | [ l ] ->
                    Expect.equal l.Name "a" "instance-let name"
                    Expect.equal l.Type BuiltinTypes.tyInt "instance-let type inferred to int"
                    Expect.isFalse l.IsMutable "not mutable"
                | other -> failtestf "expected one instance let, got %A" other

                match c.Members.[0].Body with
                | TExpr.FieldGet(TExpr.Var(k, _, _), name, _, _) ->
                    Expect.equal k (BoundVarKey.identity c.ThisKey) "the member reads the field off `this`"
                    Expect.equal name "a" "field name"
                | other -> failtestf "expected a FieldGet body, got %A" other
            }

            // A preamble `let mutable` IS the field, so a write must be a field STORE: a
            // `TExpr.Let` bound variable would be promoted to a ref cell, forking the
            // storage away from the field every member reads.
            test "TAST: a write to an instance `let mutable` lowers to a FieldSet" {
                let tast =
                    analyse "type C() =\n    let mutable c = 0\n    do c <- c + 1\n    member _.Bump () = c <- c + 1"

                Expect.isEmpty tast.Diagnostics (sprintf "no diagnostics: %A" (List.ofSeq tast.Diagnostics))

                let cls = soleClass tast

                match TPreambleEntryG.lets (EqArray.toList cls.InstancePreamble) with
                | [ l ] -> Expect.isTrue l.IsMutable "`let mutable` ⇒ a writable field"
                | other -> failtestf "expected one instance let, got %A" other

                Expect.equal cls.InstancePreamble.Length 2 "the `do` keeps its place in the sequence"

                match cls.InstancePreamble.[1] with
                | TPreambleEntry.Do _ -> ()
                | other -> failtestf "expected a Do entry, got %A" other

                match cls.Members.[0].Body with
                | TExpr.FieldSet(TExpr.Var(k, _, _), name, _, _, _) ->
                    Expect.equal k (BoundVarKey.identity cls.ThisKey) "the write stores through `this`"
                    Expect.equal name "c" "field name"
                | other -> failtestf "expected a FieldSet body, got %A" other
            }

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
                | TTypeKind.Class c ->
                    Expect.equal c.CtorParams.Length 1 "primary ctor has one param"
                    Expect.equal c.SecondaryCtors.Length 1 "one secondary ctor"
                    Expect.equal (c.SecondaryCtors.[0].Params.Length) 0 "new() takes no params"
                    Expect.equal (c.SecondaryCtors.[0].Lets.Length) 0 "no let-preamble"

                    match c.SecondaryCtors.[0].Body with
                    | TSecondaryCtorBodyG.Chain primaryArgs ->
                        Expect.equal primaryArgs.Length 1 "chain passes one arg to the primary ctor"
                    | other -> failtestf "expected a chain-form secondary ctor, got %A" other
                | other -> failtestf "expected TTypeKind.Class, got %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            // A `base.M(...)` access carries `CallVia.Base` (a non-virtual call against
            // the parent slot); an ordinary `this.M(...)` stays `CallVia.Self`.
            test "TAST: `base.M ()` carries CallVia.Base" {
                let tast =
                    analyse
                        "type Base() =\n    member this.M () = 1\ntype Derived() =\n    inherit Base()\n    override this.M () = base.M() + 1"

                let vias = ResizeArray<CallVia<SemType>>()

                let collect =
                    { TastWalk.identityIter with
                        VisitExpr =
                            fun _ e ->
                                match e with
                                | TExpr.MethodCall(_, key, via, _, _, _) when
                                    SymbolKeyOps.simpleName key = DisplayName "M"
                                    ->
                                    vias.Add via
                                | _ -> ()

                                true
                    }

                for d in EqArray.toList tast.Decls do
                    match d with
                    | TDecl.Type t ->
                        match t.Kind with
                        | TTypeKind.Class c ->
                            for m in EqArray.toList c.Members do
                                TastWalk.iterExpr collect m.Body
                        | _ -> ()
                    | _ -> ()

                Expect.contains vias CallVia.Base "the base.M() call dispatches via CallVia.Base"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "qualified name resolves through provider" {
                // Composed OVER the real contract rather than delegating one channel to it:
                // the mention walk reads `float`'s shape through the TYPE channel, which a
                // single-channel wrapper would leave empty.
                let provider: IExternalSymbolProvider =
                    ExternalSymbolProviders.composite
                        [
                            providerOfValues
                                [
                                    ExternalSymbols.monoFrozen
                                        (ModuleContainer.InModule(SymbolKeyOps.moduleInNamespace "" "Math"))
                                        "pi"
                                        (FrozenTypeBridge.toFrozen BuiltinTypes.tyFloat)
                                ]
                            realProvider.Value
                        ]

                let input = "let r = Math.pi"
                let lexed, file = parseFile input

                let tast =
                    Pipeline.analyseSemFor testCompiling provider (LexedFile.ofText lexed) file

                Expect.equal (declType tast) BuiltinTypes.tyFloat "r : float"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            // --- cast TAST shapes ---

            test "TAST: `:>` shapes as Upcast" {
                let tast =
                    analyse "type B() =\n    member this.X = 1\ntype D() =\n    inherit B()\nlet s = (new D()) :> B"

                let last = EqArray.last tast.Decls
                Expect.stringContains (TastShape.prettyDecl last) ":> B" "upcast rendered"
            }

            test "TAST: `:?>` shapes as Downcast" {
                let tast =
                    analyse
                        "type B() =\n    member this.X = 1\ntype D() =\n    inherit B()\nlet d = ((new D()) :> B) :?> D"

                let last = EqArray.last tast.Decls
                Expect.stringContains (TastShape.prettyDecl last) ":?> D" "downcast rendered"
            }

            // `T | null` downcasts EXACTLY as its non-null part `T`. `obj` HAS proper
            // subtypes, so `(x: obj | null) :?> C` is admitted.
            test "`:?>` from `obj | null` is permitted (downcasts as `obj`)" {
                let tast =
                    analyse "type C() =\n    member this.X = 1\nlet g (x: obj | null) = x :?> C\n"

                Expect.isEmpty tast.Diagnostics "nullable-obj downcast is admitted"
            }

            // `string | null` downcasts as `string`, which is sealed, so the coercion is
            // impossible (F#'s FS0016). The `null` member does NOT rescue it.
            test "`:?>` from `string | null` is rejected (downcasts as sealed `string`)" {
                let tast =
                    analyse "type C() =\n    member this.X = 1\nlet g (x: string | null) = x :?> C\n"

                Expect.isTrue
                    (tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "downcast"))
                    "a nullable-string downcast to an unrelated type is rejected"
            }

            // `:?` reads a nullable-reference source exactly as `:?>` does: the relatedness
            // check is the same one, run over the same stripped source.
            test "`:?` on `obj | null` is permitted (tests as `obj`)" {
                let tast =
                    analyse "type C() =\n    member this.X = 1\nlet g (x: obj | null) = x :? C\n"

                Expect.isEmpty tast.Diagnostics "nullable-obj type test is admitted"
            }

            test "`:?` on `string | null` against an unrelated type is rejected" {
                let tast =
                    analyse "type C() =\n    member this.X = 1\nlet g (x: string | null) = x :? C\n"

                Expect.isTrue
                    (tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "Type test"))
                    "a nullable-string test against an unrelated type is rejected"
            }

            test "TAST: `:?` shapes as TypeTest" {
                let tast =
                    analyse
                        "type B() =\n    member this.X = 1\ntype D() =\n    inherit B()\nlet t = ((new D()) :> B) :? D"

                let last = EqArray.last tast.Decls
                Expect.stringContains (TastShape.prettyDecl last) ":? D" "type test rendered"
            }

            // --- Numeric spelling aliases dealias to their canonical intrinsic identity:
            // a written `int8` annotation unifies with the canonical literal `5y : sbyte`.
            // A mismatch would mean the alias leaked as a `TyConst` of its own.

            let aliasResolvesTo (src: string) (canonical: SemType) (label: string) =
                let tast = analyse src
                Expect.isEmpty tast.Diagnostics (label + ": no mismatch")
                Expect.equal (declType tast) canonical label

            test "alias int8 resolves to sbyte" {
                aliasResolvesTo "let r = (5y : int8)" BuiltinTypes.tySByte "int8 = sbyte"
            }

            test "alias uint8 resolves to byte" {
                aliasResolvesTo "let r = (5uy : uint8)" BuiltinTypes.tyByte "uint8 = byte"
            }

            test "alias uint resolves to uint32" {
                aliasResolvesTo "let r = (5u : uint)" BuiltinTypes.tyUInt32 "uint = uint32"
            }

            test "alias int32 resolves to int" {
                aliasResolvesTo "let r = (5 : int32)" BuiltinTypes.tyInt "int32 = int"
            }

            test "alias single resolves to float32" {
                aliasResolvesTo "let r = (5.0f : single)" BuiltinTypes.tyFloat32 "single = float32"
            }

            test "alias double resolves to float" {
                aliasResolvesTo "let r = (5.0 : double)" BuiltinTypes.tyFloat "double = float"
            }

            // A written `bigint` annotation resolves to the CONTRACT intrinsic (canon
            // `Vesper.bigint`) read off the resolved shape via the provider, not to an
            // opaque `TyConst(ns="")`. Annotation only: `42I` is a custom numeric literal.
            test "written `bigint` annotation resolves to the contract intrinsic" {
                let tast = analyse "let f (x: bigint) = x"

                match declType tast with
                | TyFun(TyConst(a, aArgs), TyConst(b, bArgs)) ->
                    Expect.isTrue (aArgs.IsEmpty && bArgs.IsEmpty) "bigint is nullary"
                    Expect.equal (SymbolKeyOps.typeSimpleName a) (DisplayName "bigint") "param : bigint"
                    Expect.equal (SymbolKeyOps.typeSimpleName b) (DisplayName "bigint") "result : bigint"
                | other -> failtestf "expected bigint -> bigint, got %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "an overloaded user member call carries a total, overload-distinguishing key" {
                // The two `Show` calls lower to `MethodCall` nodes whose keys DIFFER: the
                // frozen `MemberKey.ArgSig` distinguishes `Show(int)` from `Show(string)`.
                let tast =
                    analyse
                        "type Printer() =\n    member this.Show(x: int) = x\n    member this.Show(x: string) = true\nlet p = Printer()\nlet a = p.Show(1)\nlet b = p.Show(\"hi\")"

                let calls = ResizeArray<SymbolKey>()

                let it =
                    { TastWalk.identityIter with
                        VisitExpr =
                            fun _ e ->
                                match e with
                                | TExpr.MethodCall(key = k) -> calls.Add k
                                | _ -> ()

                                true
                    }

                for d in EqArray.toList tast.Decls do
                    match d with
                    | TDecl.Let(_, value, _, _) -> TastWalk.iterExpr it value
                    | _ -> ()

                // The single-`FTConst` parameter type name of a member key, if any.
                let argTyName (k: SymbolKey) : string option =
                    match k with
                    | SymbolKey.Member mk when mk.ArgSig.Length = 1 ->
                        match mk.ArgSig.[0] with
                        | FTConst(sk, _) ->
                            let (DisplayName n) = SymbolKeyOps.typeSimpleName sk
                            Some n
                        | _ -> None
                    | _ -> None

                Expect.equal calls.Count 2 "two overloaded Show calls lowered to MethodCall nodes"
                Expect.notEqual calls.[0] calls.[1] "the two Show overloads carry DISTINCT keys"
                Expect.equal (argTyName calls.[0]) (Some "int") "p.Show(1) keyed on Show(int)"
                Expect.equal (argTyName calls.[1]) (Some "string") "p.Show(\"hi\") keyed on Show(string)"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "a non-overloaded local instance method call mints a TOTAL member key" {
                // No overload set, so the fallback mints from the resolved member itself:
                // `ArgSig` is the DECLARED parameter type `int`, never a placeholder, and
                // `MethodTyparArity` is the member's real `0`.
                let tast =
                    analyse "type C() =\n    member this.Inc (x: int) = x + 1\nlet f (c: C) = c.Inc(3)"

                match List.ofSeq (callKeys tast) with
                | [ SymbolKey.Member mk ] ->
                    Expect.equal mk.Name "Inc" "member name"
                    Expect.equal mk.Kind MemberKind.Method "method kind"
                    Expect.equal mk.MethodTyparArity 0 "no method typars"
                    Expect.equal mk.ArgSig.Length 1 "one declared value parameter"

                    match mk.ArgSig.[0] with
                    | FTConst(sk, _) ->
                        Expect.equal
                            (SymbolKeyOps.typeSimpleName sk)
                            (DisplayName "int")
                            "arg type is the declared int, not a placeholder"
                    | other -> failtestf "expected an FTConst int arg type, got %A" other
                | other -> failtestf "expected a single MethodCall member key, got %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "a static method call mints a TOTAL member key" {
                let tast =
                    analyse "type C() =\n    static member M (x: int) = x + 1\nlet r = C.M(2)"

                match List.ofSeq (callKeys tast) with
                | [ SymbolKey.Member mk ] ->
                    Expect.equal mk.Name "M" "member name"
                    Expect.equal mk.Kind MemberKind.Method "method kind"
                    Expect.equal mk.MethodTyparArity 0 "no method typars"

                    match mk.ArgSig |> EqArray.toList with
                    | [ FTConst(sk, _) ] ->
                        Expect.equal (SymbolKeyOps.typeSimpleName sk) (DisplayName "int") "arg type is the declared int"
                    | other -> failtestf "expected a single FTConst int arg type, got %A" other
                | other -> failtestf "expected a single StaticMethodCall member key, got %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "an interface-dispatched method call mints a TOTAL member key" {
                // `'T :> IShow` coerces the object argument's typar to a local interface,
                // so the key must resolve `Show` on the INTERFACE, not on the typar.
                let tast =
                    analyse
                        "type IShow =\n    abstract member Show: int -> int\nlet f (x: 'T when 'T :> IShow) = x.Show(1)"

                match List.ofSeq (callKeys tast) with
                | [ SymbolKey.Member mk ] ->
                    Expect.equal mk.Name "Show" "member name"
                    Expect.equal mk.Kind MemberKind.Method "method kind"

                    match mk.ArgSig |> EqArray.toList with
                    | [ FTConst(sk, _) ] ->
                        Expect.equal (SymbolKeyOps.typeSimpleName sk) (DisplayName "int") "arg type is the declared int"
                    | other -> failtestf "expected a single FTConst int arg type, got %A" other
                | other -> failtestf "expected a single interface MethodCall member key, got %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            // --- `totalMemberKey` EXTERNAL arm: operand-precise overload discrimination ---
            // A singular lookup collapses an overload set to an arbitrary best-by-arity pick,
            // so the stub gives `M` two same-arity overloads differing only in operand type.
            let extDeclKey = SymbolKeyOps.qualifiedTypeKeyOf "Vec2" 0
            let intFt = FTConst(RuntimeNames.intKey, EqArray.empty)
            let stringFt = FTConst(RuntimeNames.stringKey, EqArray.empty)
            let unitFt = FTConst(RuntimeNames.unitKey, EqArray.empty)

            // A static two-parameter member `M(int, p2)` on external `Vec2`, keyed by its
            // parameter shape so two overloads mint DISTINCT `MemberKey`s.
            let extMember2 (p2: FrozenType) : ExternalMember =
                let ps = EqArray.ofList [ intFt; p2 ]

                { ExternalMember.OfKey(SymbolKeyOps.memberKeyOf extDeclKey "M" ps 0 MemberKind.Method) with
                    IsStatic = true
                    Signature = TestHelpers.mkSignature 0 0 (FTTuple ps) unitFt
                }

            // The stub channels layered OVER `realProvider`: `Vec2` resolves through the stub,
            // everything else falls through. `Vec2` is NOT a local type, so `totalMemberKey`
            // takes the external arm. The trivial file initialises what the picker reads.
            let extCtx (stub: IExternalSymbolProvider) : PassContext =
                let provider = ExternalSymbolProviders.composite [ stub; realProvider.Value ]

                let lexed, file = parseFile "let _ = 0"

                let ctx = PassContext(provider, LexedFile.ofText lexed, testCompiling)

                Passes.NameResolution.run ctx file
                Passes.Unification.run ctx file
                ctx

            test "the external arm discriminates a same-arity overload by operand type" {
                let mII = extMember2 intFt // M(int, int)    — the arbitrary singular collapse
                let mIS = extMember2 stringFt // M(int, string) — the operand-matching overload

                let ctx =
                    extCtx (
                        TestHelpers.membersProvider (fun declaringType name ->
                            if declaringType = "Vec2" && name = "M" then
                                EqArray.ofSeq [ mII; mIS ]
                            else
                                EqArray.empty
                        )
                    )

                // No ground operands ⇒ keep the best-by-arity singular pick, `M(int, int)`.
                let collapsed = LocalMemberKeys.totalMemberKey ctx extDeclKey "M" ValueNone

                Expect.equal
                    collapsed
                    (ValueSome(SymbolKey.Member mII.Key))
                    "ValueNone operands keep the singular collapse"

                // Ground operands `(int, string)` select the overload the collapse MISSES.
                let operands =
                    LocalMemberKeys.externalOperands ctx.Store [||] [ BuiltinTypes.tyInt; BuiltinTypes.tyString ]

                let minted = LocalMemberKeys.totalMemberKey ctx extDeclKey "M" operands
                Expect.equal minted (ValueSome(SymbolKey.Member mIS.Key)) "ground operands mint the M(int, string) key"
                Expect.notEqual minted collapsed "the operand-precise pick differs from the singular collapse"
            }

            test "the external arm preserves the singular pick for a non-overloaded member" {
                // A name with ONE overload: `TryLookupMembers` returns a singleton, so the
                // mint is forced and equals `TryLookupMember`'s, operands or not.
                let m1 =
                    { ExternalMember.OfKey(
                          SymbolKeyOps.memberKeyOf extDeclKey "N" (EqArray.singleton intFt) 0 MemberKind.Method
                      ) with
                        IsStatic = true
                        Signature = TestHelpers.mkSignature 0 0 intFt unitFt
                    }

                let ctx =
                    extCtx (
                        TestHelpers.membersProvider (fun declaringType name ->
                            if declaringType = "Vec2" && name = "N" then
                                EqArray.singleton m1
                            else
                                EqArray.empty
                        )
                    )

                let expected = ValueSome(SymbolKey.Member m1.Key)

                let operands =
                    LocalMemberKeys.externalOperands ctx.Store [||] [ BuiltinTypes.tyInt ]

                Expect.equal
                    (LocalMemberKeys.totalMemberKey ctx extDeclKey "N" operands)
                    expected
                    "single external overload mints its sole key with operands"

                Expect.equal
                    (LocalMemberKeys.totalMemberKey ctx extDeclKey "N" ValueNone)
                    expected
                    "and the same key without operands"
            }

            test "a method call's key carries its real MethodTyparArity, not the collapsed 0" {
                // `MethodTyparArity` is what tells `M<'a>` and `M<'a, 'b>` apart: the mint
                // reads the member's real count, so `Id<'a>` keys 1 and `Plain` keys 0.
                let tast =
                    analyse
                        "type C() =\n    member this.Id<'a> (x: 'a) = x\n    member this.Plain (x: int) = x\nlet c = C()\nlet a = c.Id(3)\nlet b = c.Plain(4)"

                let arityOf name =
                    callKeys tast
                    |> Seq.tryPick (fun k ->
                        match k with
                        | SymbolKey.Member mk when mk.Name = name -> Some mk.MethodTyparArity
                        | _ -> None
                    )

                Expect.equal (arityOf "Id") (Some 1) "generic Id<'a> mints MethodTyparArity 1"
                Expect.equal (arityOf "Plain") (Some 0) "non-generic Plain mints MethodTyparArity 0"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }
        ]
