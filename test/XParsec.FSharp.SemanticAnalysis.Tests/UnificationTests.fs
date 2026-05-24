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
                // pat x at offset 4.
                let ctx = analyse "let x = 1"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) MockBuiltins.tyInt "x : int"
            }

            test "infix `+` types as int -> int -> int -> int (mono operator)" {
                let ctx = analyse "let x = 1 + 2"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) MockBuiltins.tyInt "x : int"
            }

            test "lambda body type propagates to function type" {
                let ctx = analyse "let f = fun x -> x + 1"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                let expected = TyFun(MockBuiltins.tyInt, MockBuiltins.tyInt)
                Expect.equal (typeOf ctx patKey) expected "f : int -> int"
            }

            test "function-form let infers parameter type from body" {
                let ctx = analyse "let f x = x + 1"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                let expected = TyFun(MockBuiltins.tyInt, MockBuiltins.tyInt)
                Expect.equal (typeOf ctx patKey) expected "f : int -> int"
            }

            test "application instantiates identity to argument type" {
                // Wrap in a named binding so the let-in is unambiguously an
                // expression (else the parser picks a different top-level shape).
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

            test "record literal infers record type from field set" {
                // pat r at 32: 27-char type decl + "let r = ".
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
                // The use `f { X = 1 }` pins r to R.
                let ctx = analyse "type R = { X: int }\nlet f r = r.X\nlet u = f { X = 1 }"

                let fatigueFree =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "Cannot resolve field")

                Expect.isFalse fatigueFree "no unresolved-field diagnostic when use pins receiver"
            }

            test "record clone types as source record" {
                let ctx =
                    analyse "type R = { X: int; Y: int }\nlet p = { X = 1; Y = 2 }\nlet q = { p with Y = 5 }"

                // q pat at offset 57.
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

            test "nullary ctor reference types as the union" {
                // pat p at 21: 16-char type decl + "let p = ".
                let ctx = analyse "type S = | Point\nlet p = Point"

                let patKey = NodeKey.ofSource 21 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyUnion("S", [])) "p : S"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "single-arg ctor application types as the union" {
                // pat c at 31: 26-char type decl + "let c = ".
                let ctx = analyse "type S = | Circle of float\nlet c = Circle 1.0"

                let patKey = NodeKey.ofSource 31 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyUnion("S", [])) "c : S"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "multi-arg ctor application takes a tuple" {
                // pat r at 37: 32-char type decl + "let r = ".
                let ctx = analyse "type S = | Rect of float * float\nlet r = Rect(2.0, 3.0)"

                let patKey = NodeKey.ofSource 37 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyUnion("S", [])) "r : S"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "ctor used as a value types as a function" {
                // pat f at 31: 26-char type decl + "let f = ".
                let ctx = analyse "type S = | Circle of float\nlet f = Circle"

                let patKey = NodeKey.ofSource 31 NodeKind.PatIdent
                let expected = TyFun(MockBuiltins.tyFloat, TyUnion("S", []))
                Expect.equal (typeOf ctx patKey) expected "f : float -> S"
            }

            test "ctor pattern unifies scrutinee with TyUnion" {
                // Via the `Circle r` arm the scrutinee unifies with TyUnion("S", []),
                // so `area : S -> float`.
                let ctx =
                    analyse "type S = | Circle of float\nlet area s = match s with | Circle r -> r"

                // area pat at 31 (27-char type decl + "let "), s param at 36.
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

            test "generic record literal pins typar to int" {
                let ctx = analyse "type Box<'a> = { Value: 'a }\nlet b = { Value = 1 }"
                // pat at 33: 29-char type decl + "let b = ".
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
                let ctx = analyse "type Option<'a> = | Some of 'a | None\nlet s = Some 1"
                // pat at 42: 38-char type decl + "let ".
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
                let ctx = analyse "type Box<'a> = { Value: 'a }\nlet f (b : Box<int>) = b.Value"
                // pat f at offset 33.
                let patKey = NodeKey.ofSource 33 NodeKind.PatIdent
                let expected = TyFun(TyRecord("Box", [ MockBuiltins.tyInt ]), MockBuiltins.tyInt)
                Expect.equal (typeOf ctx patKey) expected "f : Box<int> -> int"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "two record literals of same generic type use independent typars" {
                // No mismatch — independent instantiations.
                let ctx =
                    analyse
                        "type Box<'a> = { Value: 'a }\nlet x : Box<int> = { Value = 1 }\nlet y : Box<string> = { Value = \"s\" }"

                Expect.isEmpty ctx.Diagnostics "no diagnostics across independent uses"
            }

            test "monomorphic abbreviation transparently unifies" {
                // pat n at 23: 19-char type decl + "let ".
                let ctx = analyse "type Name = string\nlet n : Name = \"x\""
                let patKey = NodeKey.ofSource 23 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) MockBuiltins.tyString "n : string"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "generic abbreviation expands" {
                let ctx = analyse "type Pair<'a> = 'a * 'a\nlet p : Pair<int> = (1, 2)"
                // pat p at 28: 24-char type decl + "let ".
                let patKey = NodeKey.ofSource 28 NodeKind.PatIdent
                let expected = TyTuple [ MockBuiltins.tyInt; MockBuiltins.tyInt ]
                Expect.equal (typeOf ctx patKey) expected "p : int * int"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "chained abbreviation expands transitively" {
                let ctx = analyse "type A = B\ntype B = int\nlet x : A = 1"
                // pat x at 28: 11 + 13 char type decls + "let ".
                let patKey = NodeKey.ofSource 28 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) MockBuiltins.tyInt "x : int"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "order-independent within a module" {
                // Pair declared after its first use as IntPair.
                let ctx =
                    analyse "type IntPair = Pair<int>\ntype Pair<'a> = 'a * 'a\nlet p : IntPair = (1, 2)"
                // pat p at 53: 25 + 24 char type decls + "let ".
                let patKey = NodeKey.ofSource 53 NodeKind.PatIdent
                let expected = TyTuple [ MockBuiltins.tyInt; MockBuiltins.tyInt ]
                Expect.equal (typeOf ctx patKey) expected "p : int * int"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "cycle diagnoses without infinite-looping" {
                let ctx = analyse "type A = B\ntype B = A"

                let hasCyclic = ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "cyclic")

                Expect.isTrue hasCyclic "cycle diagnostic emitted"
            }

            test "arity mismatch on generic abbreviation diagnoses" {
                let ctx = analyse "type Pair<'a> = 'a * 'a\nlet p : Pair<int, bool> = (1, 2)"

                let hasArity =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "expects 1 type argument")

                Expect.isTrue hasArity "arity-mismatch diagnostic emitted"
            }

            test "abbreviation to function type" {
                let ctx = analyse "type Endo<'a> = 'a -> 'a\nlet inc : Endo<int> = fun x -> x + 1"
                // pat inc at 29: 25-char type decl + "let ".
                let patKey = NodeKey.ofSource 29 NodeKind.PatIdent
                let expected = TyFun(MockBuiltins.tyInt, MockBuiltins.tyInt)
                Expect.equal (typeOf ctx patKey) expected "inc : int -> int"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "abbreviation referencing a record" {
                let ctx =
                    analyse "type Box<'a> = { Value: 'a }\ntype IntBox = Box<int>\nlet b : IntBox = { Value = 1 }"
                // pat b at 56: 29 + 23 char type decls + "let ".
                let patKey = NodeKey.ofSource 56 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyRecord("Box", [ MockBuiltins.tyInt ])) "b : Box<int>"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "abbreviation inside a record field type" {
                let ctx =
                    analyse "type IntPair = int * int\ntype R = { Pair: IntPair }\nlet r = { Pair = (1, 2) }"
                // pat r at 56: 25 + 27 char type decls + "let ".
                let patKey = NodeKey.ofSource 56 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyRecord("R", [])) "r : R"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "implicit free typar in abbreviation diagnoses" {
                let ctx = analyse "type Bad = 'a"

                let hasFree =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "Free type parameter")

                Expect.isTrue hasFree "implicit free typar diagnosed"
            }

            test "construct via new" {
                let ctx =
                    analyse "type Point(x: int, y: int) =\n    member this.X = x\nlet p = new Point(3, 4)"
                // pat p at 55: 29 + 22 char decl lines + "let ".
                let patKey = NodeKey.ofSource 55 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyClass("Point", [])) "p : TyClass Point"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "construct via ctor-as-function (no new)" {
                let ctx =
                    analyse "type Point(x: int, y: int) =\n    member this.X = x\nlet p = Point(3, 4)"

                let patKey = NodeKey.ofSource 55 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyClass("Point", [])) "p : TyClass Point"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "constructor argument type mismatch diagnoses" {
                let ctx =
                    analyse "type Point(x: int, y: int) =\n    member this.X = x\nlet p = new Point(3, true)"

                let hasMismatch =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "ctor-arg mismatch diagnosed"
            }

            test "property read pins receiver via annotation" {
                let ctx =
                    analyse "type Point(x: int, y: int) =\n    member this.X = x\nlet f (p : Point) = p.X"
                // pat f at 55: 29 + 22 char decl lines + "let ".
                let patKey = NodeKey.ofSource 55 NodeKind.PatIdent
                let expected = TyFun(TyClass("Point", []), MockBuiltins.tyInt)
                Expect.equal (typeOf ctx patKey) expected "f : Point -> int"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "method invocation types as return type" {
                let ctx =
                    analyse
                        "type Point(x: int, y: int) =\n    member this.Magnitude () = x * x + y * y\nlet m (p : Point) = p.Magnitude()"
                // pat m at 78: 29 + 45 char decl lines + "let ".
                let patKey = NodeKey.ofSource 78 NodeKind.PatIdent
                let expected = TyFun(TyClass("Point", []), MockBuiltins.tyInt)
                Expect.equal (typeOf ctx patKey) expected "m : Point -> int"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "member on free TyVar pinned by use" {
                let ctx =
                    analyse "type Point(x: int) =\n    member this.X = x\nlet f p = p.X\nlet _ = f (new Point(3))"

                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "generic class instantiation" {
                let ctx =
                    analyse "type Box<'a>(value: 'a) =\n    member this.Value = value\nlet b = Box(1)"
                // pat b at 60: 26 + 30 char decl lines + "let ".
                let patKey = NodeKey.ofSource 60 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyClass("Box", [ MockBuiltins.tyInt ])) "b : Box<int>"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "generic class annotation pins typar" {
                let ctx =
                    analyse
                        "type Box<'a>(value: 'a) =\n    member this.Value = value\nlet b : Box<string> = Box(\"hi\")"

                let patKey = NodeKey.ofSource 60 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyClass("Box", [ MockBuiltins.tyString ])) "b : Box<string>"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "method body sees other members via this" {
                let ctx =
                    analyse
                        "type C(x: int) =\n    member this.Inner () = x\n    member this.Outer () = this.Inner ()\nlet u = (new C(1)).Outer()"

                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "auto-property types from initialiser" {
                let ctx = analyse "type C() =\n    member val Origin = (0, 0)\nlet c = new C()"

                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "static property read via class name" {
                let ctx = analyse "type C() =\n    static member Origin = (0, 0)\nlet o = C.Origin"
                // pat o at 49: 11 + 34 char decl lines + "let ".
                let patKey = NodeKey.ofSource 49 NodeKind.PatIdent
                let expected = TyTuple [ MockBuiltins.tyInt; MockBuiltins.tyInt ]
                Expect.equal (typeOf ctx patKey) expected "o : int * int"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "static method call via class name" {
                let ctx =
                    analyse "type C() =\n    static member Plus (x: int) = x + 1\nlet r = C.Plus(2)"

                let hasErr = ctx.Diagnostics |> Seq.exists (fun d -> d.Severity = Error)
                Expect.isFalse hasErr "no error diagnostics"
            }

            test "static member on receiver instance diagnoses" {
                // F# spec: static members are accessed via the type name, not an instance.
                let ctx =
                    analyse "type C() =\n    static member M () = 1\nlet c = new C()\nlet r = c.M()"

                let hasStaticDiag =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "static")

                Expect.isTrue hasStaticDiag "instance.staticMember access diagnoses"
            }

            test "list literal `[1; 2; 3]` types as `list<int>`" {
                let ctx = analyse "let xs = [1; 2; 3]"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent

                let expected = TyRecord("Microsoft.FSharp.Collections.list", [ MockBuiltins.tyInt ])

                Expect.equal (typeOf ctx patKey) expected "xs : list<int>"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "array literal `[|1; 2; 3|]` types as `int[]`" {
                let ctx = analyse "let xs = [|1; 2; 3|]"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                let expected = TyRecord("Microsoft.FSharp.Core.[]", [ MockBuiltins.tyInt ])
                Expect.equal (typeOf ctx patKey) expected "xs : int[]"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "empty list `[]` types as `list<'a>` (element TyVar stays free)" {
                let ctx = analyse "let xs = []"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent

                match typeOf ctx patKey with
                | TyRecord("Microsoft.FSharp.Collections.list", [ TyVar _ ]) -> ()
                | other -> failtestf "expected list<free TyVar>, got %A" other
            }

            test "list literal element types must unify" {
                let ctx = analyse "let xs = [1; true]"

                let hasMismatch =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "type-mismatch diagnostic emitted"
            }

            test "`[| 1; 2 ]` (mismatched close) emits semantic-analysis diagnostic" {
                // Parser virtual-inserts `|]` after seeing the real `]`, plus
                // its own UnclosedDelimiter diagnostic. The semantic-analysis
                // backstop must surface the breakage on `ctx.Diagnostics` so
                // downstream consumers that don't read the parser stream
                // (Freeze, codegen) still see a problem.
                let ctx = analyse "let xs = [| 1; 2 ]"

                let hasCloseDiag =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "closing delimiter")

                Expect.isTrue hasCloseDiag "mismatched-delimiter diagnostic emitted"
            }
        ]
