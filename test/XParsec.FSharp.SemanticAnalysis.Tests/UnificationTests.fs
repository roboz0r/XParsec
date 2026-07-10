module XParsec.FSharp.SemanticAnalysis.Tests.UnificationTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    let ctx = PassContext(realProvider.Value, input, lexed)
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

            // `null` / `undefined` are distinct absence sentinels (the members of a
            // TS-style `T | null` / `T | undefined`), NOT folded onto `unit` — even
            // though `unit` *also* lowers to JS `undefined` at the VALUE level. The
            // type identities stay distinct: each resolves to its own opaque
            // `TyConst`, so neither unifies with `unit`.
            test "`undefined` and `unit` resolve to distinct types and do not unify" {
                let ctx = analyse "let f (x: undefined) : unit = x"
                // `x : undefined`, pat at offset 7.
                let patKey = NodeKey.ofSource 7 NodeKind.PatIdent

                Expect.equal (typeOf ctx patKey) (TyConst(RuntimeNames.undefinedKey, EqArray.empty)) "x : undefined"

                let hasMismatch =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "returning an `undefined`-typed value as `unit` must mismatch"
            }

            test "`null` and `unit` resolve to distinct types and do not unify" {
                let ctx = analyse "let f (x: unit) : null = x"

                let hasMismatch =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "returning a `unit`-typed value as `null` must mismatch"
            }

            test "`null` and `undefined` are distinct types and do not unify" {
                let ctx = analyse "let f (x: null) : undefined = x"

                let hasMismatch =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "`null` and `undefined` must not unify"
            }

            test "Using a TyUnknown-typed external value emits a use-site diagnostic" {
                // A contract val whose signature named an out-of-scope type bakes a
                // `TyUnknown` leaf. Referencing that symbol must fire a diagnostic when
                // its `TyUnknown` type reaches unification — not silently succeed.
                let brokenProvider =
                    { new IExternalSymbolProvider

                      interface IExternalSymbolResolver with
                          member _.TryLookup name =
                              if name = "broken" then
                                  ValueSome(ExternalSymbols.monoFrozen "broken" (FTUnknown "Missing.Thing"))
                              else
                                  ValueNone

                          member _.TryLookupType(_: string) = ValueNone
                          member _.TryLookupUnionCase _ = ValueNone
                          member _.AmbientOpenPrefixes = []
                      interface IExternalSymbolStore with
                          member _.TryLookupType(_: SymbolKey) = ValueNone
                          member _.TryLookupMember(_, _) = ValueNone
                          member _.TryLookupMembers(_, _) = [||]
                          member _.TryLookupIndexSignature _ = []
                          member _.TryLookupInlineBody _ = ValueNone
                          member _.IntrinsicReverseCanon = Map.empty
                          member _.IntrinsicForwardRepr = ExternalSymbols.emptyForwardRepr
                    }

                let provider = ExternalSymbols.composite [ brokenProvider; realProvider.Value ]
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

            // G11: a generic member's *signature* annotation may name the enclosing
            // class typar. `inferBinding` mints a fresh typar scope per binding; it
            // must seed it with the class typars (via `Resolution.EnclosingTypars`)
            // first, else `translateType` on the annotation finds an empty strict
            // scope and falsely diagnoses "Free type parameter 'a".
            test "G11: instance member signature names the class typar" {
                let ctx =
                    analyse
                        "type Box<'a>(value: 'a) =\n    member this.Value = value\n    member this.Wrap (x: 'a) : Box<'a> = Box(x)"

                let hasFree =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "Free type parameter")

                Expect.isFalse hasFree "class typar in member signature is in scope"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "G11: static member signature names the class typar" {
                let ctx =
                    analyse
                        "type Box<'a>(value: 'a) =\n    member this.Value = value\n    static member Of (x: 'a) : Box<'a> = Box(x)"

                let hasFree =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "Free type parameter")

                Expect.isFalse hasFree "class typar in static member signature is in scope"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            // G12: a generic member may introduce an *implicit* type parameter —
            // one named only in a param/return annotation, neither a class typar nor
            // an explicit `<'U>` on the member. MemberRegistration must register it
            // into `MethodTypeParams`, and Unification must keep it in scope across
            // the member-body walk (signature *and* nested lets) — else strict member
            // scope falsely diagnoses "Free type parameter 'U".
            test "G12: implicit member typar in return annotation" {
                let ctx =
                    analyse
                        "type Box<'a>(value: 'a) =\n    member this.Value = value\n    member this.Map (f: 'a -> 'b) : Box<'b> = Box(f value)"

                let hasFree =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "Free type parameter")

                Expect.isFalse hasFree "implicit member typar 'b is in scope"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "G12: implicit member typar in a nested-let body annotation" {
                // `'b` is named in the return *and* in `Comparer<'b>` inside a nested
                // `let` in the body — it must persist past the member's own binding
                // into nested scopes (the set.fs `s.Map` shape).
                let ctx =
                    analyse
                        "type Box<'a>(value: 'a) =\n    member this.Value = value\n    member this.Map (f: 'a -> 'b) : Box<'b> =\n        let g : 'b -> 'b = fun x -> x\n        Box(g (f value))"

                let hasFree =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "Free type parameter")

                Expect.isFalse hasFree "implicit member typar 'b stays in scope in nested let"
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
                    TyConst(RuntimeNames.arrayKey 1, EqArray.singleton BuiltinTypes.tyInt)

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

            // B-8: `[<AllowNullLiteral>]` lets `let x: C = null` unify without
            // diagnostics. The current implementation relies on the existing
            // fresh-TyVar behaviour for `null` — the surrounding annotation drives
            // the link to `TyClass`.
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

            // An intrinsic-class base (`exn`): the inherit args are checked against the
            // CONTRACT `.ctor`s riding the provider's `IntrinsicClass` shape
            // (`new: message: string -> exn`), so a mis-typed arg is a source diagnostic
            // here, not a codegen internal error.

            test "inherit exn(message) types against the contract base ctor" {
                let ctx = analyse "type MyErr(m: string) =\n    inherit exn(m)"

                Expect.isEmpty ctx.Diagnostics "string arg matches exn's `new: string -> exn`"
            }

            test "inherit exn with a mis-typed argument diagnoses" {
                let ctx = analyse "type MyErr() =\n    inherit exn(42)"

                let hasCtorError =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "constructor" && d.Message.Contains "exn")

                Expect.isTrue hasCtorError "int arg matches no exn contract ctor (string / unit)"
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

            test "instance member on arity-overloaded class resolves per arity" {
                // `Box`1`/`Box`2` overload one short name by arity; the bare alias is
                // withdrawn. `walkClassBodies` must resolve each class by its arity-key
                // to name-resolve its member bodies — a bare-name miss would skip BOTH
                // classes' bodies, leaving `this`/ctor params unbound so the member type
                // decouples from the class typar (`.Peek` would type as a free var).
                // `.Peek` returns the last typar, so the two arities yield distinct types.
                let input =
                    "type Box<'a>(v: 'a) =\n    member this.Peek = v\n"
                    + "type Box<'a, 'b>(x: 'a, y: 'b) =\n    member this.Peek = y\n"
                    + "let f (one: Box<int>) =\n    let a = one.Peek\n    a\n"
                    + "let g (two: Box<bool, string>) =\n    let b = two.Peek\n    b"

                let ctx = analyse input
                let aKey = NodeKey.ofSource (input.IndexOf "a = one.Peek") NodeKind.PatIdent
                let bKey = NodeKey.ofSource (input.IndexOf "b = two.Peek") NodeKind.PatIdent
                Expect.equal (typeOf ctx aKey) BuiltinTypes.tyInt "a : int — Box`1.Peek is the 'a"
                Expect.equal (typeOf ctx bKey) BuiltinTypes.tyString "b : string — Box`2.Peek is the 'b"
                Expect.isEmpty ctx.Diagnostics (sprintf "no diagnostics — both arities resolve: %A" ctx.Diagnostics)
            }

            test "typar-interface member walk resolves per generic arity" {
                // `IBox`1` and `IBox`2` overload one short name by arity; the bare
                // alias is withdrawn. A typar constrained to a specific arity
                // (`'S :> IBox<int>`) must resolve `.Peek()` through the arity-keyed
                // interface, not a bare-name strip. `Peek` returns the LAST typar on
                // each, so per-arity resolution yields distinct results — an
                // arity-blind walk would collapse them.
                let src =
                    String.concat
                        "\n"
                        [
                            "type IBox<'a> ="
                            "    abstract member Peek : unit -> 'a"
                            "type IBox<'a, 'b> ="
                            "    abstract member Peek : unit -> 'b"
                            "let f (x: 'S when 'S :> IBox<int>) ="
                            "    let a = x.Peek()"
                            "    a"
                            "let g (y: 'S when 'S :> IBox<bool, string>) ="
                            "    let b = y.Peek()"
                            "    b"
                        ]

                let ctx = analyse src
                let aKey = NodeKey.ofSource (src.IndexOf "a = x.Peek()") NodeKind.PatIdent
                let bKey = NodeKey.ofSource (src.IndexOf "b = y.Peek()") NodeKind.PatIdent
                Expect.equal (typeOf ctx aKey) BuiltinTypes.tyInt "a : int — IBox`1.Peek is the 'a"
                Expect.equal (typeOf ctx bKey) BuiltinTypes.tyString "b : string — IBox`2.Peek is the 'b"
                Expect.isEmpty ctx.Diagnostics (sprintf "no diagnostics — both arities resolve: %A" ctx.Diagnostics)
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

            test "`:>` upcast resolves interfaces of an arity-overloaded local host" {
                // `Base`1`/`Base`2` overload one short name by arity; the bare alias is
                // withdrawn. Upcasting a `Base<int, string>` value to the interface it
                // declares drives the subtype walk (`tryUpcastWitness` →
                // `subtypeInterfacesOf`) onto the arity-2 host. Resolving its
                // `interface … with` impls must key on the receiver's `SymbolKey`, not a
                // bare-name strip — a bare read misses the withdrawn alias, finds no
                // witness, and diagnoses a spurious upcast failure.
                let input =
                    String.concat
                        "\n"
                        [
                            "type I ="
                            "    abstract member Tag : int"
                            "type Base<'a>(v: 'a) ="
                            "    interface I with"
                            "        member this.Tag = 1"
                            "type Base<'a, 'b>(x: 'a, y: 'b) ="
                            "    interface I with"
                            "        member this.Tag = 2"
                            "let f (b: Base<int, string>) ="
                            "    let i = b :> I"
                            "    i"
                        ]

                let ctx = analyse input
                let patKey = NodeKey.ofSource (input.IndexOf "i = b :> I") NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyClass("I", EqArray.empty)) "i : I — Base`2 <: I resolves per arity"
                Expect.isEmpty ctx.Diagnostics (sprintf "no diagnostics — Base`2 declares I: %A" ctx.Diagnostics)
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

            // Stage 3 of the anonymous-union plan: the
            // front door. `translateType` maps the CST `Type.UnionType` / `Type.Null`
            // surface to a canonical `TyOr` via `mkUnion`. No assignability yet
            // (Stage 4+), so a union only enters here through an *annotation* on a
            // parameter, whose fresh TyVar links to it without any subtyping — the
            // function's domain is the translated union. We assert on the translated
            // `SemType`, not on any call type-checking.
            let unionDomainOf (input: string) : SemType =
                let ctx = analyse input
                let patKey = NodeKey.ofSource (input.IndexOf "f ") NodeKind.PatIdent

                match typeOf ctx patKey with
                | TyFun(dom, _) -> dom
                | other -> failtestf "expected f : _ -> _, got %A" other

            test "int | string translates to the canonical TyOr [int; string]" {
                let dom = unionDomainOf "let f (x: int | string) = x"
                Expect.equal dom (mkUnion [ BuiltinTypes.tyInt; BuiltinTypes.tyString ]) "f domain is int | string"
            }

            test "string | int translates to the SAME canonical union (order-insensitive)" {
                let a = unionDomainOf "let f (x: int | string) = x"
                let b = unionDomainOf "let f (x: string | int) = x"
                Expect.equal b a "string | int ≡ int | string after translate"

                match b with
                | TyOr ms -> Expect.equal ms.Members.Length 2 "two distinct members"
                | other -> failtestf "expected a TyOr, got %A" other
            }

            test "int | null translates with the reserved `null` member present" {
                let dom = unionDomainOf "let f (x: int | null) = x"

                match dom with
                | TyOr ms ->
                    Expect.equal ms.Members.Length 2 "two members"

                    Expect.contains
                        (EqSet.toList ms.Members)
                        (TyConst(RuntimeNames.nullKey, EqArray.empty))
                        "the reserved `null` literal type is a member"
                | other -> failtestf "expected int | null to be a TyOr, got %A" other
            }

            // The parser now grows `a | b | c` (TypeParsing.pUnionType loops into a
            // left-nested CST chain); translateType already fed that tree to mkUnion,
            // so a >2-case union canonicalises to a 3-member TyOr.
            test "int | string | bool translates to a 3-member canonical TyOr" {
                let dom = unionDomainOf "let f (x: int | string | bool) = x"

                Expect.equal
                    dom
                    (mkUnion [ BuiltinTypes.tyInt; BuiltinTypes.tyString; BuiltinTypes.tyBool ])
                    "f domain is int | string | bool"

                match dom with
                | TyOr ms -> Expect.equal ms.Members.Length 3 "three distinct members"
                | other -> failtestf "expected a TyOr, got %A" other
            }

            // Stage 4 of the anonymous-union plan: the
            // directional `subsumes` query learns union membership — the first
            // user-visible behaviour. `unify` is untouched; these are read-only
            // calls (no `Link` mutation), asserted directly. All three relations
            // are covered, including the negative `A | B ⋠ A`.
            let subsumeCtx = analyse "let x = 1"
            let int = BuiltinTypes.tyInt
            let str = BuiltinTypes.tyString
            let boolTy = BuiltinTypes.tyBool

            let subsumes a b =
                UnificationSubsume.subsumes subsumeCtx a b

            test "member → union: a member is Equal to the union it belongs to" {
                Expect.equal
                    (subsumes int (mkUnion [ int; str ]))
                    UnificationSubsume.SubsumeOutcome.Equal
                    "int ≤ (int | string) is Equal (int is a member)"
            }

            test "member → union: a non-member is Unrelated" {
                Expect.equal
                    (subsumes boolTy (mkUnion [ int; str ]))
                    UnificationSubsume.SubsumeOutcome.Unrelated
                    "bool ⋠ (int | string)"
            }

            test "union → union: a narrower union is a Subtype of a wider one" {
                Expect.equal
                    (subsumes (mkUnion [ int; str ]) (mkUnion [ int; str; boolTy ]))
                    UnificationSubsume.SubsumeOutcome.Subtype
                    "(int | string) ≤ (int | string | bool)"
            }

            test "union → union: identical canonical member sets are Equal" {
                Expect.equal
                    (subsumes (mkUnion [ int; str ]) (mkUnion [ str; int ]))
                    UnificationSubsume.SubsumeOutcome.Equal
                    "(int | string) ≤ (string | int) is Equal (order-insensitive)"
            }

            test "union → union: a member outside the target makes it Unrelated" {
                Expect.equal
                    (subsumes (mkUnion [ int; str ]) (mkUnion [ int; boolTy ]))
                    UnificationSubsume.SubsumeOutcome.Unrelated
                    "(int | string) ⋠ (int | bool)"
            }

            test "union → member: a union does NOT subsume one of its members" {
                Expect.equal
                    (subsumes (mkUnion [ int; str ]) int)
                    UnificationSubsume.SubsumeOutcome.Unrelated
                    "(int | string) ⋠ int — the consumer must narrow first"
            }

            // Stage 5 of the anonymous-union plan:
            // committing coercion at expected-type positions. The annotation sites
            // (let return / parameter `Pat.Typed`) switched from symmetric `unify` to
            // directional `unifyAnnotation`, so a value flows into a union-typed slot
            // the annotation writes down — while every non-union annotation (`obj`, a
            // base class, a plain nominal) still grounds via symmetric `unify`. The
            // assignment site `x <- e` stays on `unify` — inference never synthesises
            // a union.
            test "let binding annotated with a union accepts a member value" {
                let ctx = analyse "let x: int | string = 1"
                Expect.isEmpty ctx.Diagnostics "int flows into (int | string) annotation"
            }

            test "let binding annotated with a reordered union accepts the other member" {
                let ctx = analyse "let x: string | int = \"a\""
                Expect.isEmpty ctx.Diagnostics "string flows into (string | int) annotation"
            }

            test "a union-typed parameter accepts arguments of each member" {
                let ctx = analyse "let f (x: int | string) = 0\nlet a = f 1\nlet b = f \"a\""
                Expect.isEmpty ctx.Diagnostics "f 1 and f \"a\" both check against (int | string)"
            }

            test "passing a member into a union slot does NOT narrow the slot" {
                // `f 1` returns the *union* `int | string`, not `int` — the slot
                // accepted `1` by assignability without unifying the parameter down
                // to the actual. (The body returns `x`, so the return type is the
                // parameter's union.)
                let ctx = analyse "let f (x: int | string) = x\nlet y = f 1"

                let yKey =
                    NodeKey.ofSource (("let f (x: int | string) = x\nlet y = f 1").IndexOf "y =") NodeKind.PatIdent

                Expect.equal (typeOf ctx yKey) (mkUnion [ int; str ]) "f 1 : int | string"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "a non-member value is still rejected against a union annotation" {
                let ctx = analyse "let x: int | string = true"

                let hasMismatch =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Severity = Severity.Error)

                Expect.isTrue hasMismatch "bool ⋠ (int | string) — annotation rejects it"
            }

            test "assignment stays symmetric: `x <- 1` on a string is an error" {
                // The assignment site was deliberately NOT switched to `unifyArg` —
                // it has no annotation, so inference must not silently widen to
                // `string | int`. This is the principality rule made mechanical.
                let ctx = analyse "let mutable x = \"\"\nx <- 1"

                let hasMismatch =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "`x <- 1` against a string binding stays a hard error"
            }

            test "a union slot accepts a value by assignability WITHOUT pinning its typar" {
                // The no-box-pin invariant generalised from `obj`: a value that
                // subsumes into a member flows in without `unify`, so the actual's
                // typar stays free. A plain `unify` against the union would link the
                // var; the `TyOr` arm of `tryCoerceUpcast` must not.
                let tv = TypeVar()
                let actual = TyVar tv
                let target = mkUnion [ TyVar tv; int ]
                let key = NodeKey.ofSource 0 NodeKind.PatIdent
                let accepted = UnificationEngine.tryCoerceUpcast subsumeCtx key actual target
                Expect.isTrue accepted "the union slot accepts the value"
                Expect.equal tv.Link ValueNone "the actual's typar is left free (no pin)"
            }

            // Stage 6 of the anonymous-union plan:
            // constraint reduction. `equality` and `comparison` are deliberately
            // asymmetric (§Constraints): a union satisfies EQUALITY iff *every*
            // member does — generic `=` is total on the union's repr (cross-member
            // is `false`, never throws) — but any real (≥2-member) union FAILS
            // COMPARISON outright, because generic `compare` *throws* across distinct
            // runtime types, so an individually-comparable member set is still
            // non-comparable as a whole. `checkConstraint` is read-only here (no
            // `Link` mutation), so the outcomes are asserted by direct calls.
            let checkConstraint kind ty =
                let c: SemanticConstraint =
                    {
                        Kind = kind
                        DeclKey = NodeKey.ofSource 0 NodeKind.PatIdent
                    }

                UnificationEngine.checkConstraint subsumeCtx c ty

            test "equality on (int | string) is Satisfied — every member is equatable" {
                Expect.equal
                    (checkConstraint SemanticConstraintKind.Equality (mkUnion [ int; str ]))
                    UnificationEngine.ConstraintOutcome.Satisfied
                    "int | string supports equality (both members do)"
            }

            test "equality on a union with a function member is Violated" {
                // A `TyFun` arm supports no structural equality, so the all-members
                // reduction fails for the whole union.
                Expect.equal
                    (checkConstraint SemanticConstraintKind.Equality (mkUnion [ int; TyFun(int, int) ]))
                    UnificationEngine.ConstraintOutcome.Violated
                    "int | (int -> int) — the function arm breaks equality"
            }

            test "equality on a union with an unresolved member Defers" {
                // A free member is "unknown yet": the reduction defers so the
                // constraint re-fires when that member's TyVar Links.
                Expect.equal
                    (checkConstraint SemanticConstraintKind.Equality (mkUnion [ int; TyVar(TypeVar()) ]))
                    UnificationEngine.ConstraintOutcome.Defer
                    "int | 'a — defers on the free member"
            }

            test "comparison on (int | string) is Violated though each member is comparable" {
                // Unlike equality, comparison does NOT reduce member-wise: generic
                // `compare` throws across distinct runtime types, so a heterogeneous
                // union is non-comparable as a whole — admitting it would let
                // `List.sort` on a `(int | string) list` type-check then throw.
                Expect.equal
                    (checkConstraint SemanticConstraintKind.Comparison (mkUnion [ int; str ]))
                    UnificationEngine.ConstraintOutcome.Violated
                    "int | string fails comparison even though int and string each support it"
            }

            // Stage 7 of the anonymous-union plan:
            // binder narrowing + closed-union exhaustiveness. A `match` on a `TyOr`
            // scrutinee narrows each `:? M as x` arm to `M`, narrows a fall-through
            // catch-all to the residual `mkUnion (ts \ matched)`, and — because the
            // union is *closed* — warns when the arms leave a member uncovered.
            let hasUnionExhaustivenessWarning (ctx: PassContext) =
                ctx.Diagnostics
                |> Seq.exists (fun d -> d.Severity = Severity.Warning && d.Message.Contains "anonymous union")

            test "an exhaustive type-test match on a union checks with no warning" {
                // The seed milestone: both members tested, so the match is provably
                // exhaustive and the binders `i`/`s` narrow to `int`/`string`.
                let ctx =
                    analyse
                        "let f (x: int | string) =\n    match x with\n    | :? int as i -> i\n    | :? string as s -> 0"

                Expect.isEmpty ctx.Diagnostics "exhaustive union match — no diagnostics"
                Expect.isFalse (hasUnionExhaustivenessWarning ctx) "no non-exhaustiveness warning"
            }

            test "a union match missing a member warns (closed-union exhaustiveness)" {
                let ctx =
                    analyse "let f (x: int | string) =\n    match x with\n    | :? int as i -> i"

                Expect.isTrue (hasUnionExhaustivenessWarning ctx) "missing `string` arm warns"

                let warning =
                    ctx.Diagnostics
                    |> Seq.find (fun d -> d.Severity = Severity.Warning && d.Message.Contains "anonymous union")

                Expect.isTrue (warning.Message.Contains "string") "the warning names the uncovered member"
            }

            test "a fall-through catch-all binds the narrowed residual union" {
                // After `:? int` catches `int`, the residual is `int | string \ int`
                // = `string`, so `other` binds at `string` (a collapsed singleton),
                // not the full union — and the catch-all makes the match exhaustive.
                let input =
                    "let f (x: int | string) =\n    match x with\n    | :? int as i -> i\n    | other -> 0"

                let ctx = analyse input
                let otherKey = NodeKey.ofSource (input.IndexOf "other") NodeKind.PatIdent
                Expect.equal (typeOf ctx otherKey) BuiltinTypes.tyString "other : string (residual)"
                Expect.isFalse (hasUnionExhaustivenessWarning ctx) "catch-all makes it exhaustive"
            }

            // Stage 8 of the anonymous-union plan: the
            // closing freeze round-trip + backend handoff. No codegen — the front
            // end must hand a well-formed `FTOr` (in canonical order) to the backend
            // boundary. Stage 1 already mapped `TyOr → FTOr` in `freezeTy`; this is
            // the *end-to-end* assertion through a real annotated binding, run all
            // the way through `Pipeline.analyse` (every pass + the final
            // `SemType → FrozenType` freeze).
            let freezeDecls (input: string) : Frozen.TastFile =
                let lexed, file = parseFile input
                Pipeline.analyse realProvider.Value input lexed file

            // The frozen type of the (sole) top-level `let f` binding.
            let frozenLetTy (file: Frozen.TastFile) : FrozenType =
                file.Decls
                |> EqArray.toList
                |> List.tryPick (fun d ->
                    match d with
                    // Both a function binding (`let f … = …`) and a plain value
                    // freeze to `Let`, carrying the binding's frozen type as `ty`.
                    | TDeclG.Let(ty = ty) -> Some ty
                    | _ -> None
                )
                |> Option.defaultWith (fun () -> failtest "expected a frozen `let` decl")

            test "an annotated `int | string` binding freezes to a canonical FTOr signature" {
                // The seed milestone's signature: domain AND return both `int |
                // string`, so the frozen decl type is `FTFun(FTOr, FTOr)`. The
                // expected `FrozenType` is built by freezing the *same* canonical
                // SemType (`toFrozen ∘ mkUnion`), so the member order under test is
                // exactly the canonical one `freeze` preserves — not a hand-written
                // guess at the sort order.
                let file = freezeDecls "let f (x: int | string) : int | string = x"
                let union = mkUnion [ BuiltinTypes.tyInt; BuiltinTypes.tyString ]
                let expected = toFrozen (TyFun(union, union))
                Expect.equal (frozenLetTy file) expected "f freezes to (int | string) -> (int | string)"
            }

            test "the frozen union carries both members as FTOr in canonical order" {
                // Pin the `FTOr` shape directly (not just via the round-trip equality
                // above): the domain is an `FTOr` of exactly the two `FTConst`
                // leaves, sorted to the canonical order `mkUnion` produces.
                let file = freezeDecls "let f (x: int | string) : int | string = x"

                match frozenLetTy file with
                | FTFun(FTOr members, _) ->
                    let canonical =
                        match toFrozen (mkUnion [ BuiltinTypes.tyInt; BuiltinTypes.tyString ]) with
                        | FTOr ms -> ms
                        | other -> failtestf "expected the canonical union to freeze to FTOr, got %A" other

                    Expect.equal members canonical "domain members are the canonical FTOr [int; string]"
                | other -> failtestf "expected f's domain to freeze to an FTOr, got %A" other
            }

            test "the frozen FTOr round-trips through the SemType bridge" {
                // The backend handoff contract rests on `ofFrozen`/`toFrozen` being
                // mutual inverses (FrozenTypeTests proves it on synthetic samples);
                // assert it holds on a union that travelled the *real* freeze.
                let frozen = frozenLetTy (freezeDecls "let f (x: int | string) : int | string = x")
                Expect.equal (toFrozen (ofFrozen frozen)) frozen "ofFrozen >> toFrozen = id on the frozen signature"
            }
        ]
