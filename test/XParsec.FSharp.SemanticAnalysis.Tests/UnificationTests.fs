module XParsec.FSharp.SemanticAnalysis.Tests.UnificationTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    let ctx = PassContext(MockBuiltins.provider, input, lexed)
    Desugar.run ctx file
    NameResolution.run ctx file
    Unification.run ctx file
    ctx

let private typeOf (ctx: PassContext) (key: NodeKey) : SemType =
    match ctx.TypeVar.TryGetValue key with
    | ValueSome tv -> Unification.zonk (TyVar tv)
    | ValueNone -> failwithf "no TypeVar entry for %O" key

[<Tests>]
let tests =
    testList
        "Unification"
        [
            test "integer literal types as int" {
                // "let x = 1" — pattern x at offset 4, RHS literal at offset 8.
                let ctx = analyse "let x = 1"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) MockBuiltins.tyInt "x : int"
            }

            test "infix `+` types as int -> int -> int -> int (mono operator)" {
                // "let x = 1 + 2" — pat x's type should be int.
                let ctx = analyse "let x = 1 + 2"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) MockBuiltins.tyInt "x : int"
            }

            test "lambda body type propagates to function type" {
                // "let f = fun x -> x + 1" — f : int -> int.
                let ctx = analyse "let f = fun x -> x + 1"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                let expected = TyFun(MockBuiltins.tyInt, MockBuiltins.tyInt)
                Expect.equal (typeOf ctx patKey) expected "f : int -> int"
            }

            test "function-form let infers parameter type from body" {
                // "let f x = x + 1" — f : int -> int.
                let ctx = analyse "let f x = x + 1"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                let expected = TyFun(MockBuiltins.tyInt, MockBuiltins.tyInt)
                Expect.equal (typeOf ctx patKey) expected "f : int -> int"
            }

            test "application instantiates identity to argument type" {
                // "let result = let id = fun x -> x in id 42" — wrap in a named
                // binding so the let-in is unambiguously an expression (avoids the
                // parser disambiguating into a different top-level shape). `result`
                // pat is at offset 4.
                let ctx = analyse "let result = let id = fun x -> x in id 42"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) MockBuiltins.tyInt "result : int"
            }

            test "type mismatch on int + bool emits a diagnostic" {
                let ctx = analyse "let x = 1 + true"

                let hasMismatch =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "Type mismatch diagnostic emitted"
            }

            test "ident `true` types as bool via provider" {
                let ctx = analyse "let b = true"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) MockBuiltins.tyBool "b : bool"
            }

            test "nested infix: 1 + 2 * 3 types as int" {
                let ctx = analyse "let x = 1 + 2 * 3"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) MockBuiltins.tyInt "x : int"
            }

            // ---- Records ----

            test "record literal infers record type from field set" {
                // "type R = { X: int; Y: int }\nlet r = { X = 1; Y = 2 }"
                //  Newline at 27, "let r = " puts pat r at offset 32.
                let ctx = analyse "type R = { X: int; Y: int }\nlet r = { X = 1; Y = 2 }"

                let patKey = NodeKey.ofSource 32 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyRecord("R", [])) "r : TyRecord R"

                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "ambiguous field set requires qualifier" {
                let ctx =
                    analyse "type R = { X: int; Y: int }\ntype S = { X: int; Y: int }\nlet r = { X = 1; Y = 2 }"

                let hasAmbig =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "ambiguous")

                Expect.isTrue hasAmbig "ambiguous-field-set diagnostic emitted"
            }

            test "unknown field set diagnoses" {
                let ctx = analyse "type R = { X: int; Y: int }\nlet r = { X = 1; Z = 3 }"

                let hasUnknown =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "No record type matches")

                Expect.isTrue hasUnknown "unknown-field-set diagnostic emitted"
            }

            test "field access on annotated parameter types as the field type" {
                let ctx = analyse "type R = { X: int }\nlet f (r: R) = r.X"

                let patKey = NodeKey.ofSource 24 NodeKind.PatIdent
                let expected = TyFun(TyRecord("R", []), MockBuiltins.tyInt)
                Expect.equal (typeOf ctx patKey) expected "f : R -> int"

                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "field access on free TyVar pinned by use" {
                // `let f r = r.X in let _ = f { X = 1 }` — the call pins r to R.
                let ctx = analyse "type R = { X: int }\nlet f r = r.X\nlet u = f { X = 1 }"

                let fatigueFree =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "Cannot resolve field")

                Expect.isFalse fatigueFree "no unresolved-field diagnostic when use pins receiver"
            }

            test "record clone types as source record" {
                // type R = { X: int; Y: int }
                // let p = { X = 1; Y = 2 }
                // let q = { p with Y = 5 }
                let ctx =
                    analyse "type R = { X: int; Y: int }\nlet p = { X = 1; Y = 2 }\nlet q = { p with Y = 5 }"

                // p : R at offset 32, q : R at offset 57.
                let qKey = NodeKey.ofSource 57 NodeKind.PatIdent
                Expect.equal (typeOf ctx qKey) (TyRecord("R", [])) "q : R"

                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "record clone with unknown field diagnoses" {
                let ctx = analyse "type R = { X: int }\nlet p = { X = 1 }\nlet q = { p with Z = 5 }"

                let hasNoField =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "has no field")

                Expect.isTrue hasNoField "unknown-field diagnostic emitted"
            }

            // ---- Discriminated unions ----

            test "nullary ctor reference types as the union" {
                // "type S = | Point\nlet p = Point"
                //  Newline at 16, "let p = " puts pat p at offset 21.
                let ctx = analyse "type S = | Point\nlet p = Point"

                let patKey = NodeKey.ofSource 21 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyUnion("S", [])) "p : S"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "single-arg ctor application types as the union" {
                // "type S = | Circle of float\nlet c = Circle 1.0"
                //  Newline at 26, "let c = " puts pat c at offset 31.
                let ctx = analyse "type S = | Circle of float\nlet c = Circle 1.0"

                let patKey = NodeKey.ofSource 31 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyUnion("S", [])) "c : S"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "multi-arg ctor application takes a tuple" {
                // "type S = | Rect of float * float\nlet r = Rect(2.0, 3.0)"
                //  Newline at 32, "let r = " puts pat r at offset 37.
                let ctx = analyse "type S = | Rect of float * float\nlet r = Rect(2.0, 3.0)"

                let patKey = NodeKey.ofSource 37 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyUnion("S", [])) "r : S"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "ctor used as a value types as a function" {
                // "type S = | Circle of float\nlet f = Circle"
                //  Newline at 26, "let f = " puts pat f at offset 31.
                let ctx = analyse "type S = | Circle of float\nlet f = Circle"

                let patKey = NodeKey.ofSource 31 NodeKind.PatIdent
                let expected = TyFun(MockBuiltins.tyFloat, TyUnion("S", []))
                Expect.equal (typeOf ctx patKey) expected "f : float -> S"
            }

            test "ctor pattern unifies scrutinee with TyUnion" {
                // "type S = | Circle of float\nlet area s = match s with | Circle r -> r"
                //  Receiver `s` is the function parameter; via the `Circle r` arm,
                //  scrutinee unifies with TyUnion("S", []). `area` has type `S -> float`.
                let ctx =
                    analyse "type S = | Circle of float\nlet area s = match s with | Circle r -> r"

                // "type S = | Circle of float\n" is 27 chars. "let area s = " puts area pat at 31 and s param at 36.
                let areaKey = NodeKey.ofSource 31 NodeKind.PatIdent
                let expected = TyFun(TyUnion("S", []), MockBuiltins.tyFloat)
                Expect.equal (typeOf ctx areaKey) expected "area : S -> float"
            }

            test "ambiguous ctor name requires qualifier" {
                // Two unions share an `Ok` case.
                let ctx = analyse "type R1 = | Ok of int\ntype R2 = | Ok of float\nlet x = Ok 1"

                let hasAmbig =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "Ambiguous constructor")

                Expect.isTrue hasAmbig "ambiguous-ctor diagnostic emitted"
            }

            test "qualified ctor resolves an ambiguous case name" {
                let ctx =
                    analyse "type R1 = | Ok of int\ntype R2 = | Ok of float\nlet x = R2.Ok 1.0"

                // Pat x starts at offset 50: 21 (type R1...) + 1 (\n) + 23 (type R2...) + 1 (\n) + 4 ("let ").
                let patKey = NodeKey.ofSource 50 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyUnion("R2", [])) "x : R2"
            }

            // ---- Generics ----

            test "generic record literal pins typar to int" {
                // `let b = { Value = 1 }` against `type Box<'a> = { Value: 'a }`
                let ctx = analyse "type Box<'a> = { Value: 'a }\nlet b = { Value = 1 }"
                // "type Box<'a> = { Value: 'a }\n" is 29 chars. "let b = " puts pat at 33.
                let patKey = NodeKey.ofSource 33 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyRecord("Box", [ MockBuiltins.tyInt ])) "b : Box<int>"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "generic record annotation pins typar to string" {
                let ctx =
                    analyse "type Box<'a> = { Value: 'a }\nlet b : Box<string> = { Value = \"x\" }"

                let patKey = NodeKey.ofSource 33 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyRecord("Box", [ MockBuiltins.tyString ])) "b : Box<string>"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "generic record annotation mismatch with literal diagnoses" {
                // Annotation says Box<int>, literal field is "x" (string).
                let ctx =
                    analyse "type Box<'a> = { Value: 'a }\nlet b : Box<int> = { Value = \"x\" }"

                let hasMismatch =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "annotation/literal mismatch reported"
            }

            test "generic ctor pins typar to int" {
                // `Some 1` against `type Option<'a> = | Some of 'a | None`
                let ctx = analyse "type Option<'a> = | Some of 'a | None\nlet s = Some 1"
                // "type Option<'a> = | Some of 'a | None\n" is 38 chars. pat at 42.
                let patKey = NodeKey.ofSource 42 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyUnion("Option", [ MockBuiltins.tyInt ])) "s : Option<int>"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "generic ctor annotation pins typar to string" {
                let ctx =
                    analyse "type Option<'a> = | Some of 'a | None\nlet n : Option<string> = None"

                let patKey = NodeKey.ofSource 42 NodeKind.PatIdent

                Expect.equal (typeOf ctx patKey) (TyUnion("Option", [ MockBuiltins.tyString ])) "n : Option<string>"

                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "two parameter typars share 'a identity" {
                // `let pair (x: 'a) (y: 'a) = x, y` — pair : 'a -> 'a -> ('a * 'a)
                // Use site `pair 1 "hello"` triggers a mismatch (int vs string for 'a).
                let ctx =
                    analyse "let pair (x: 'a) (y: 'a) : 'a * 'a = x, y\nlet _ = pair 1 \"hello\""

                let hasMismatch =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "shared 'a flags int/string mismatch"
            }

            test "two bindings have independent 'a typars" {
                let ctx =
                    analyse "let id1 (x: 'a) = x\nlet id2 (y: 'a) = y\nlet a = id1 1\nlet b = id2 true"

                Expect.isEmpty ctx.Diagnostics "no diagnostics — typars are per-binding"
            }

            test "wrong-arity generic type diagnoses" {
                let ctx =
                    analyse "type Box<'a> = { Value: 'a }\nlet b : Box<int, string> = { Value = 1 }"

                let hasArity =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "expects 1 type argument")

                Expect.isTrue hasArity "arity-mismatch diagnostic emitted"
            }

            test "field access on generic record substitutes typar" {
                // `let f (b : Box<int>) = b.Value` — f : Box<int> -> int
                let ctx = analyse "type Box<'a> = { Value: 'a }\nlet f (b : Box<int>) = b.Value"
                // Pat f at offset 33.
                let patKey = NodeKey.ofSource 33 NodeKind.PatIdent
                let expected = TyFun(TyRecord("Box", [ MockBuiltins.tyInt ]), MockBuiltins.tyInt)
                Expect.equal (typeOf ctx patKey) expected "f : Box<int> -> int"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "two record literals of same generic type use independent typars" {
                // `let x : Box<int> = { Value = 1 }; let y : Box<string> = { Value = "s" }`
                // No mismatch — independent instantiations.
                let ctx =
                    analyse
                        "type Box<'a> = { Value: 'a }\nlet x : Box<int> = { Value = 1 }\nlet y : Box<string> = { Value = \"s\" }"

                Expect.isEmpty ctx.Diagnostics "no diagnostics across independent uses"
            }
        ]
