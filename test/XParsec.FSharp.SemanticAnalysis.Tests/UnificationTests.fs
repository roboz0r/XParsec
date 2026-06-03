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
    match ctx.Bindings.TypeVar.TryGetValue key with
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
                Expect.equal (typeOf ctx patKey) BuiltinTypes.tyInt "x : int"
            }

            test "infix `+` types as int -> int -> int -> int (mono operator)" {
                let ctx = analyse "let x = 1 + 2"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) BuiltinTypes.tyInt "x : int"
            }

            test "lambda body type propagates to function type" {
                let ctx = analyse "let f = fun x -> x + 1"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                let expected = TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)
                Expect.equal (typeOf ctx patKey) expected "f : int -> int"
            }

            test "function-form let infers parameter type from body" {
                let ctx = analyse "let f x = x + 1"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                let expected = TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)
                Expect.equal (typeOf ctx patKey) expected "f : int -> int"
            }

            test "application instantiates identity to argument type" {
                // Wrap in a named binding so the let-in is unambiguously an
                // expression (else the parser picks a different top-level shape).
                let ctx = analyse "let result = let id = fun x -> x in id 42"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) BuiltinTypes.tyInt "result : int"
            }

            test "type mismatch on int + bool emits a diagnostic" {
                let ctx = analyse "let x = 1 + true"

                let hasMismatch =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "Type mismatch diagnostic emitted"
            }

            test "Using a TyUnknown-typed external value emits a use-site diagnostic" {
                // A contract val whose signature named an out-of-scope type bakes a
                // `TyUnknown` leaf. Referencing that symbol must fire a diagnostic when
                // its `TyUnknown` type reaches unification — not silently succeed.
                let brokenProvider =
                    { new IExternalSymbolProvider with
                        member _.TryLookup name =
                            if name = "broken" then
                                ValueSome(ExternalSymbols.mono "broken" (TyUnknown "Missing.Thing"))
                            else
                                ValueNone

                        member _.TryLookupType _ = ValueNone
                        member _.TryLookupMember(_, _) = ValueNone
                        member _.TryLookupMembers(_, _) = [||]
                        member _.TryLookupUnionCase _ = ValueNone
                        member _.AmbientOpenPrefixes = []
                        member _.TryLookupInlineBody _ = ValueNone
                        member _.TryLookupInlineBodyByName _ = ValueNone
                    }

                let provider = ExternalSymbols.composite [ brokenProvider; MockBuiltins.provider ]
                let input = "let y = broken"
                let lexed, file = parseFile input
                let ctx = PassContext(provider, input, lexed)
                Desugar.run ctx file
                NameResolution.run ctx file
                Unification.run ctx file

                let hasUnknownDiag =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "could not be resolved")

                Expect.isTrue
                    hasUnknownDiag
                    (sprintf
                        "use-site TyUnknown diagnostic expected; diagnostics: %A"
                        (ctx.Diagnostics |> Seq.map (fun d -> d.Message) |> Seq.toList))
            }

            test "ident `true` types as bool via provider" {
                let ctx = analyse "let b = true"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) BuiltinTypes.tyBool "b : bool"
            }

            test "nested infix: 1 + 2 * 3 types as int" {
                let ctx = analyse "let x = 1 + 2 * 3"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) BuiltinTypes.tyInt "x : int"
            }

            test "record literal infers record type from field set" {
                // pat r at 32: 27-char type decl + "let r = ".
                let ctx = analyse "type R = { X: int; Y: int }\nlet r = { X = 1; Y = 2 }"

                let patKey = NodeKey.ofSource 32 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyRecord("R", EqArray.empty)) "r : TyRecord R"

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
                let expected = TyFun(TyRecord("R", EqArray.empty), BuiltinTypes.tyInt)
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
                Expect.equal (typeOf ctx qKey) (TyRecord("R", EqArray.empty)) "q : R"

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
                Expect.equal (typeOf ctx patKey) (TyUnion("S", EqArray.empty)) "p : S"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "single-arg ctor application types as the union" {
                // pat c at 31: 26-char type decl + "let c = ".
                let ctx = analyse "type S = | Circle of float\nlet c = Circle 1.0"

                let patKey = NodeKey.ofSource 31 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyUnion("S", EqArray.empty)) "c : S"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "multi-arg ctor application takes a tuple" {
                // pat r at 37: 32-char type decl + "let r = ".
                let ctx = analyse "type S = | Rect of float * float\nlet r = Rect(2.0, 3.0)"

                let patKey = NodeKey.ofSource 37 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyUnion("S", EqArray.empty)) "r : S"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "ctor used as a value types as a function" {
                // pat f at 31: 26-char type decl + "let f = ".
                let ctx = analyse "type S = | Circle of float\nlet f = Circle"

                let patKey = NodeKey.ofSource 31 NodeKind.PatIdent
                let expected = TyFun(BuiltinTypes.tyFloat, TyUnion("S", EqArray.empty))
                Expect.equal (typeOf ctx patKey) expected "f : float -> S"
            }

            test "ctor pattern unifies scrutinee with TyUnion" {
                // Via the `Circle r` arm the scrutinee unifies with TyUnion("S", EqArray.empty),
                // so `area : S -> float`.
                let ctx =
                    analyse "type S = | Circle of float\nlet area s = match s with | Circle r -> r"

                // area pat at 31 (27-char type decl + "let "), s param at 36.
                let areaKey = NodeKey.ofSource 31 NodeKind.PatIdent
                let expected = TyFun(TyUnion("S", EqArray.empty), BuiltinTypes.tyFloat)
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
                Expect.equal (typeOf ctx patKey) (TyUnion("R2", EqArray.empty)) "x : R2"
            }

            test "generic record literal pins typar to int" {
                let ctx = analyse "type Box<'a> = { Value: 'a }\nlet b = { Value = 1 }"
                // pat at 33: 29-char type decl + "let b = ".
                let patKey = NodeKey.ofSource 33 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyRecord("Box", EqArray.singleton BuiltinTypes.tyInt)) "b : Box<int>"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "generic record annotation pins typar to string" {
                let ctx =
                    analyse "type Box<'a> = { Value: 'a }\nlet b : Box<string> = { Value = \"x\" }"

                let patKey = NodeKey.ofSource 33 NodeKind.PatIdent

                Expect.equal
                    (typeOf ctx patKey)
                    (TyRecord("Box", EqArray.singleton BuiltinTypes.tyString))
                    "b : Box<string>"

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

                Expect.equal
                    (typeOf ctx patKey)
                    (TyUnion("Option", EqArray.singleton BuiltinTypes.tyInt))
                    "s : Option<int>"

                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "generic ctor annotation pins typar to string" {
                let ctx =
                    analyse "type Option<'a> = | Some of 'a | None\nlet n : Option<string> = None"

                let patKey = NodeKey.ofSource 42 NodeKind.PatIdent

                Expect.equal
                    (typeOf ctx patKey)
                    (TyUnion("Option", EqArray.singleton BuiltinTypes.tyString))
                    "n : Option<string>"

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

                let expected =
                    TyFun(TyRecord("Box", EqArray.singleton BuiltinTypes.tyInt), BuiltinTypes.tyInt)

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
                Expect.equal (typeOf ctx patKey) BuiltinTypes.tyString "n : string"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "generic abbreviation expands" {
                let ctx = analyse "type Pair<'a> = 'a * 'a\nlet p : Pair<int> = (1, 2)"
                // pat p at 28: 24-char type decl + "let ".
                let patKey = NodeKey.ofSource 28 NodeKind.PatIdent
                let expected = TyTuple(EqArray.ofList [ BuiltinTypes.tyInt; BuiltinTypes.tyInt ])
                Expect.equal (typeOf ctx patKey) expected "p : int * int"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "chained abbreviation expands transitively" {
                let ctx = analyse "type A = B\ntype B = int\nlet x : A = 1"
                // pat x at 28: 11 + 13 char type decls + "let ".
                let patKey = NodeKey.ofSource 28 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) BuiltinTypes.tyInt "x : int"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "order-independent within a module" {
                // Pair declared after its first use as IntPair.
                let ctx =
                    analyse "type IntPair = Pair<int>\ntype Pair<'a> = 'a * 'a\nlet p : IntPair = (1, 2)"
                // pat p at 53: 25 + 24 char type decls + "let ".
                let patKey = NodeKey.ofSource 53 NodeKind.PatIdent
                let expected = TyTuple(EqArray.ofList [ BuiltinTypes.tyInt; BuiltinTypes.tyInt ])
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
                let expected = TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)
                Expect.equal (typeOf ctx patKey) expected "inc : int -> int"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "abbreviation referencing a record" {
                let ctx =
                    analyse "type Box<'a> = { Value: 'a }\ntype IntBox = Box<int>\nlet b : IntBox = { Value = 1 }"
                // pat b at 56: 29 + 23 char type decls + "let ".
                let patKey = NodeKey.ofSource 56 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyRecord("Box", EqArray.singleton BuiltinTypes.tyInt)) "b : Box<int>"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "abbreviation inside a record field type" {
                let ctx =
                    analyse "type IntPair = int * int\ntype R = { Pair: IntPair }\nlet r = { Pair = (1, 2) }"
                // pat r at 56: 25 + 27 char type decls + "let ".
                let patKey = NodeKey.ofSource 56 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyRecord("R", EqArray.empty)) "r : R"
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
                Expect.equal (typeOf ctx patKey) (TyClass("Point", EqArray.empty)) "p : TyClass Point"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "construct via ctor-as-function (no new)" {
                let ctx =
                    analyse "type Point(x: int, y: int) =\n    member this.X = x\nlet p = Point(3, 4)"

                let patKey = NodeKey.ofSource 55 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyClass("Point", EqArray.empty)) "p : TyClass Point"
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
                let expected = TyFun(TyClass("Point", EqArray.empty), BuiltinTypes.tyInt)
                Expect.equal (typeOf ctx patKey) expected "f : Point -> int"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "method invocation types as return type" {
                let ctx =
                    analyse
                        "type Point(x: int, y: int) =\n    member this.Magnitude () = x * x + y * y\nlet m (p : Point) = p.Magnitude()"
                // pat m at 78: 29 + 45 char decl lines + "let ".
                let patKey = NodeKey.ofSource 78 NodeKind.PatIdent
                let expected = TyFun(TyClass("Point", EqArray.empty), BuiltinTypes.tyInt)
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
                Expect.equal (typeOf ctx patKey) (TyClass("Box", EqArray.singleton BuiltinTypes.tyInt)) "b : Box<int>"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "generic class annotation pins typar" {
                let ctx =
                    analyse
                        "type Box<'a>(value: 'a) =\n    member this.Value = value\nlet b : Box<string> = Box(\"hi\")"

                let patKey = NodeKey.ofSource 60 NodeKind.PatIdent

                Expect.equal
                    (typeOf ctx patKey)
                    (TyClass("Box", EqArray.singleton BuiltinTypes.tyString))
                    "b : Box<string>"

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
                let expected = TyTuple(EqArray.ofList [ BuiltinTypes.tyInt; BuiltinTypes.tyInt ])
                Expect.equal (typeOf ctx patKey) expected "o : int * int"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "static method call via class name" {
                let ctx =
                    analyse "type C() =\n    static member Plus (x: int) = x + 1\nlet r = C.Plus(2)"

                let hasErr = ctx.Diagnostics |> Seq.exists (fun d -> d.Severity = Severity.Error)
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

                let expected =
                    SemType.TyRecord(RuntimeNames.fsharpCoreListKey, EqArray.singleton BuiltinTypes.tyInt)

                Expect.equal (typeOf ctx patKey) expected "xs : list<int>"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "array literal `[|1; 2; 3|]` types as `int[]`" {
                let ctx = analyse "let xs = [|1; 2; 3|]"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent

                let expected =
                    TyConst(RuntimeNames.arrayName 1, EqArray.singleton BuiltinTypes.tyInt)

                Expect.equal (typeOf ctx patKey) expected "xs : int[]"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "empty list `[]` types as `list<'a>` (element TyVar stays free)" {
                let ctx = analyse "let xs = []"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent

                match typeOf ctx patKey with
                | TyRecord("Microsoft.FSharp.Collections.list`1", args) when
                    args.Length = 1
                    && (
                        match args.[0] with
                        | TyVar _ -> true
                        | _ -> false
                    )
                    ->
                    ()
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

            // vesper-set-sprint-plan §1.6 / B-8: `[<AllowNullLiteral>]` lets
            // `let x: C = null` unify without diagnostics. The current
            // implementation relies on the existing fresh-TyVar behaviour for
            // `null` — the surrounding annotation drives the link to `TyClass`.
            test "[<AllowNullLiteral>] permits `let x: C = null`" {
                let ctx =
                    analyse "[<AllowNullLiteral>]\ntype C() = member this.M () = 1\nlet x : C = null"

                Expect.isEmpty ctx.Diagnostics "no diagnostics on null-binding"
            }

            // --- Phase 2 / B-4: base-ctor typing + `base` binding (Step 2.2) ---
            // Step 2.2 types `inherit Base(args)` against the parent's primary
            // ctor and mints the `base` TyVar. Inherited member access / `base.M()`
            // resolution gate on Step 2.3's member-chain walk.

            test "base-ctor argument types correctly against parent ctor" {
                let ctx =
                    analyse "type B(x: int) =\n    member this.X = x\ntype D(y: int) =\n    inherit B(y)"

                Expect.isEmpty ctx.Diagnostics "no diagnostics when base-ctor arg matches"
            }

            test "base-ctor argument type mismatch diagnoses" {
                let ctx =
                    analyse "type B(x: int) =\n    member this.X = x\ntype D(s: string) =\n    inherit B(s)"

                let hasMismatch =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "base-ctor arg mismatch diagnosed"
            }

            test "`base` binding linked to parent type" {
                let ctx =
                    analyse
                        "type B() =\n    member this.M () = 1\ntype D() =\n    inherit B()\n    member this.N () = 2"

                match ctx.Types.Class.TryGetValue "D" with
                | true, info -> Expect.equal (typeOf ctx info.BaseKey) (TyClass("B", EqArray.empty)) "base : B"
                | false, _ -> failtest "class type D not registered"
            }

            test "generic base-ctor arg types under parent typar substitution" {
                let ctx =
                    analyse
                        "type Box<'a>(v: 'a) =\n    member this.V = v\ntype IntBox(n: int) =\n    inherit Box<int>(n)"

                Expect.isEmpty ctx.Diagnostics "int arg matches Box<int>'s 'a"
            }

            test "generic base-ctor arg mismatch diagnoses" {
                let ctx =
                    analyse
                        "type Box<'a>(v: 'a) =\n    member this.V = v\ntype BadBox(s: string) =\n    inherit Box<int>(s)"

                let hasMismatch =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "string arg vs Box<int>'s int 'a diagnosed"
            }

            // --- Phase 2 / B-4: member-chain lookup + `subsumes` (Step 2.3) ---
            // `resolveFieldStep` / `drainPendingDotAccess` recurse into the
            // parent's members on a derived-class miss; override declarations on
            // the derived class shadow the inherited member of the same name.

            test "inherited member access resolves through parent" {
                let input =
                    "type B() =\n    member this.X = 1\ntype D() =\n    inherit B()\nlet d = new D()\nlet n = d.X"

                let ctx = analyse input
                let patKey = NodeKey.ofSource (input.IndexOf "n = d.X") NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) BuiltinTypes.tyInt "n : int — B.X reached on a D"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "override shadows inherited member of same name" {
                // Both `B.M` and `D.M` are `unit -> int`; the access must resolve
                // (derived table searched first) without a chain miss.
                let ctx =
                    analyse
                        "type B() =\n    member this.M () = 1\ntype D() =\n    inherit B()\n    override this.M () = 2\nlet r = (new D()).M()"

                Expect.isEmpty ctx.Diagnostics "no diagnostics — override resolves"
            }

            test "`base.M()` types through the parent's member" {
                let ctx =
                    analyse
                        "type B() =\n    member this.M () = 1\ntype D() =\n    inherit B()\n    override this.M () = base.M() + 1"

                Expect.isEmpty ctx.Diagnostics "base.M : unit -> int reached through B"
            }

            test "generic inheritance substitutes parent typar from derived args" {
                let input =
                    "type Box<'a>(v: 'a) =\n    member this.V = v\ntype IntBox(n: int) =\n    inherit Box<int>(n)\nlet b = new IntBox(1)\nlet r = b.V"

                let ctx = analyse input
                let patKey = NodeKey.ofSource (input.IndexOf "r = b.V") NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) BuiltinTypes.tyInt "r : int — Box<int>.V's 'a bound to int"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            // --- Phase 2 / B-4: `:>` / `:?` / `:?>` arms (Step 2.4) ---

            test "`:>` upcast to declared base types as the base" {
                let input =
                    "type B() =\n    member this.X = 1\ntype D() =\n    inherit B()\nlet s = (new D()) :> B"

                let ctx = analyse input
                let patKey = NodeKey.ofSource (input.IndexOf "s = ") NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyClass("B", EqArray.empty)) "s : B"
                Expect.isEmpty ctx.Diagnostics "no diagnostics — D <: B"
            }

            test "`:>` upcast between unrelated types diagnoses" {
                let ctx =
                    analyse
                        "type A() =\n    member this.X = 1\ntype B() =\n    member this.Y = 2\nlet s = (new A()) :> B"

                let hasUpcastErr =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "upcast")

                Expect.isTrue hasUpcastErr "unrelated upcast diagnosed"
            }

            test "`:?>` downcast types as the target type" {
                let input =
                    "type B() =\n    member this.X = 1\ntype D() =\n    inherit B()\nlet d = ((new D()) :> B) :?> D"

                let ctx = analyse input
                let patKey = NodeKey.ofSource (input.IndexOf "d = ") NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyClass("D", EqArray.empty)) "d : D"
                Expect.isEmpty ctx.Diagnostics "no diagnostics — D <: B downcast is valid"
            }

            test "`:?>` downcast to unrelated type diagnoses" {
                let ctx =
                    analyse
                        "type A() =\n    member this.X = 1\ntype B() =\n    member this.Y = 2\nlet d = (new B()) :?> A"

                let hasDowncastErr =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "downcast")

                Expect.isTrue hasDowncastErr "unrelated downcast diagnosed"
            }

            test "`:?` type test types as bool" {
                let input =
                    "type B() =\n    member this.X = 1\ntype D() =\n    inherit B()\nlet t = ((new D()) :> B) :? D"

                let ctx = analyse input
                let patKey = NodeKey.ofSource (input.IndexOf "t = ") NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) BuiltinTypes.tyBool "t : bool"
                Expect.isEmpty ctx.Diagnostics "no diagnostics — B and D are related"
            }

            test "`:?` type test on unrelated types warns" {
                let ctx =
                    analyse
                        "type A() =\n    member this.X = 1\ntype B() =\n    member this.Y = 2\nlet t = (new A()) :? B"

                let hasWarning =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Severity = Severity.Warning && d.Message.Contains "always false")

                Expect.isTrue hasWarning "unrelated type test warns (not errors)"
            }
        ]
