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

            test "generic type written bare back-fills fresh typars" {
                // `Box` is arity 1 but named bare: its typar is back-filled with a fresh
                // TyVar, which the record literal then pins to `int`. No arity diagnostic.
                let ctx = analyse "type Box<'a> = { Value: 'a }\nlet b : Box = { Value = 1 }"
                // pat b at 33: 28-char type decl + "\n" + "let ".
                let patKey = NodeKey.ofSource 33 NodeKind.PatIdent

                Expect.equal
                    (typeOf ctx patKey)
                    (TyRecord("Box", EqArray.singleton BuiltinTypes.tyInt))
                    "b : Box<int> — bare name back-filled and pinned"

                Expect.isEmpty ctx.Diagnostics "no diagnostics — a bare generic name is lenient"
            }

            test "generic intrinsic written bare back-fills an element arg, not empty" {
                // `Vec<'a>` written bare must still carry one back-filled TyVar: a niladic
                // `TyConst(k, [])` has the wrong arity, so it fails every arg-count-matched
                // unification and the array-element guard downstream.
                let ctx = analyse "type Vec<'a> = (# \"System.Int32\" #)\nlet f (v : Vec) = v"
                // pat f at 40: 35-char type decl + "\n" + "let ".
                let patKey = NodeKey.ofSource 40 NodeKind.PatIdent

                match typeOf ctx patKey with
                | TyFun(TyConst(_, args), _) ->
                    Expect.equal args.Length 1 "bare generic intrinsic carries one back-filled arg, not []"
                | other -> failtestf "expected TyFun over a TyConst, got %A" other
            }

            test "arity-0 type given a type argument diagnoses yet still resolves to the local type" {
                let ctx = analyse "type Color = | Red | Green\nlet f (c : Color<int>) = c"

                let hasArity =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "expects 0 type argument")

                Expect.isTrue hasArity "arity-0-with-args diagnostic emitted and blames the local type"
            }

            test "arity-0 enum given a type argument is blamed, not left an unresolved type" {
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

            // A cycle is only WRITABLE inside one `type … and …` group: file-order scoping
            // lets an alias name only a type declared above it, and a cycle needs a back-edge.
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

            // The oracle rejects `id<string> 3` with FS0001 ("This expression was expected to
            // have type 'string' but here has type 'int'").
            test "explicit type application on a generic function rejects a mismatched argument" {
                let ctx = analyse "let id<'a> (x: 'a) = x\nlet bad = id<string> 3"

                Expect.isTrue (ctx.Diagnostics |> Seq.exists Diagnostic.isError) "id<string> 3 is a type error"
            }

            test "explicit type application on a generic function admits a matching argument" {
                let ctx = analyse "let id<'a> (x: 'a) = x\nlet ok = id<string> \"a\""
                // pat ok at 27: 23-char first line + "let ".
                let patKey = NodeKey.ofSource 27 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) BuiltinTypes.tyString "ok : string"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "explicit type application on a user generic function pins its argument" {
                let ctx = analyse "let f<'a> (x: 'a) = x\nlet ok = f<int> 3"
                // pat ok at 26: 22-char first line + "let ".
                let patKey = NodeKey.ofSource 26 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) BuiltinTypes.tyInt "ok : int"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            // F# accepts a postfix application through a qualified name; this compiler models
            // only the single-segment form, and says so rather than leaving a free TyVar.
            test "postfix type application through a dotted name reports the shape as unsupported" {
                let ctx = analyse "module A =\n    type T<'a> = { v: 'a }\n\nlet f (x: int A.T) = x"

                Expect.isTrue
                    (ctx.Diagnostics
                     |> Seq.exists (fun d ->
                         match d.Kind with
                         | Kind.NotYetSupported feature -> feature.Contains "postfix type application"
                         | _ -> false
                     ))
                    (sprintf
                        "expected a NotYetSupported diagnostic for `int A.T`, got: %A"
                        (ctx.Diagnostics |> Seq.map (fun d -> d.Message) |> List.ofSeq))
            }

            test "single-segment postfix application still resolves" {
                let ctx = analyse "type T<'a> = { v: 'a }\nlet f (x: int T) = x"
                Expect.isEmpty (errors ctx) "no errors — `int T` is the modelled postfix form"
            }

            test "implicit free typar in abbreviation diagnoses" {
                let ctx = analyse "type Bad = 'a"

                let hasFree =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "Free type parameter")

                Expect.isTrue hasFree "implicit free typar diagnosed"
            }
        ]
