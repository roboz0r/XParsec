module XParsec.FSharp.SemanticAnalysis.Tests.UnificationGenericsTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers
open XParsec.FSharp.SemanticAnalysis.Tests.UnificationTestHelpers

[<Tests>]
let tests =
    testList
        "Unification.Generics"
        [
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

            test "generic type named bare back-fills fresh typars" {
                // `Box` is arity 1; naming it with NO arguments is the lenient tail — the
                // any-arity claim answers and its single typar is back-filled with a fresh
                // TyVar, which the record literal then pins to int. No arity diagnostic.
                let ctx = analyse "type Box<'a> = { Value: 'a }\nlet b : Box = { Value = 1 }"
                // pat b at 33: 28-char type decl + "\n" + "let ".
                let patKey = NodeKey.ofSource 33 NodeKind.PatIdent

                Expect.equal
                    (typeOf ctx patKey)
                    (TyRecord("Box", EqArray.singleton BuiltinTypes.tyInt))
                    "b : Box<int> — bare name back-filled and pinned"

                Expect.isEmpty ctx.Diagnostics "no diagnostics — a bare generic name is lenient"
            }

            test "generic intrinsic named bare back-fills an element arg, not empty" {
                // A GENERIC intrinsic (`Vec<'a>`) written with NO args must still carry one
                // back-filled TyVar so its `TyConst` has the arity the type declares. A niladic
                // `TyConst(k, [])` is malformed: it fails every arg-count-matched unification
                // (`Engine`) and the downstream array-element guard. The bare tail resolves it
                // through the SAME back-fill as every other kind — no intrinsic special case.
                let ctx = analyse "type Vec<'a> = (# \"System.Int32\" #)\nlet f (v : Vec) = v"
                // pat f at 40: 35-char type decl + "\n" + "let ".
                let patKey = NodeKey.ofSource 40 NodeKind.PatIdent

                match typeOf ctx patKey with
                | TyFun(TyConst(_, args), _) ->
                    Expect.equal args.Length 1 "bare generic intrinsic carries one back-filled arg, not []"
                | other -> failtestf "expected TyFun over a TyConst, got %A" other
            }

            test "arity-0 type given a type argument diagnoses yet still names the local type" {
                // `Color<int>` — a niladic union written with a stray type argument. The
                // any-arity claim reaches the local `Color`, blames the arity ("expects 0 …")
                // and still resolves to it, never falling through to an undefined-type verdict.
                let ctx = analyse "type Color = | Red | Green\nlet f (c : Color<int>) = c"

                let hasArity =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "expects 0 type argument")

                Expect.isTrue hasArity "arity-0-with-args diagnostic emitted and blames the local type"
            }

            test "arity-0 enum given a type argument is blamed, not left an unresolved head" {
                // An enum is the kind the OLD cascade had no arm for: `E<int>` fell straight
                // through to the undefined-head verdict, silently NOT blaming the arity of a
                // type that plainly exists. Routed through the same any-arity claim as every
                // other kind, it now blames the arity. The "expects 0 type argument" text is
                // itself the proof: the old path could not emit it for an enum.
                let ctx = analyse "type Dir = | Up = 0 | Down = 1\nlet f (d : Dir<int>) = d"

                let hasArity =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "expects 0 type argument")

                Expect.isTrue hasArity "an enum at the wrong arity is blamed like every other kind"
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
                let ctx = analyse "type B = int\ntype A = B\nlet x : A = 1"
                // pat x at 28: 13 + 11 char type decls + "let ".
                let patKey = NodeKey.ofSource 28 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) BuiltinTypes.tyInt "x : int"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "abbreviation of a generic type declared above it" {
                let ctx =
                    analyse "type Pair<'a> = 'a * 'a\ntype IntPair = Pair<int>\nlet p : IntPair = (1, 2)"
                // pat p at 53: 24 + 25 char type decls + "let ".
                let patKey = NodeKey.ofSource 53 NodeKind.PatIdent
                let expected = TyTuple(EqArray.ofList [ BuiltinTypes.tyInt; BuiltinTypes.tyInt ])
                Expect.equal (typeOf ctx patKey) expected "p : int * int"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            // An alias cycle is only WRITABLE inside one `type … and …` group: file-order
            // scoping means an alias can only name a type declared above it, and a cycle
            // needs a back-edge.
            test "cycle diagnoses without infinite-looping" {
                let ctx = analyse "type A = B\nand B = A"

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
        ]
