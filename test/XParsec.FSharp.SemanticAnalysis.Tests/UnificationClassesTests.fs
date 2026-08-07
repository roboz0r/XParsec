module XParsec.FSharp.SemanticAnalysis.Tests.UnificationClassesTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers
open XParsec.FSharp.SemanticAnalysis.Tests.UnificationTestHelpers

[<Tests>]
let tests =
    testList
        "Unification.Classes"
        [
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

            // A generic member's *signature* annotation may name the enclosing
            // class typar. `inferBinding` mints a fresh typar scope per binding; it
            // must seed it with the class typars (via `Resolution.EnclosingTypars`)
            // first, else `translateType` on the annotation finds an empty strict
            // scope and falsely diagnoses "Free type parameter 'a".
            test "instance member signature names the class typar" {
                let ctx =
                    analyse
                        "type Box<'a>(value: 'a) =\n    member this.Value = value\n    member this.Wrap (x: 'a) : Box<'a> = Box(x)"

                let hasFree =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "Free type parameter")

                Expect.isFalse hasFree "class typar in member signature is in scope"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "static member signature names the class typar" {
                let ctx =
                    analyse
                        "type Box<'a>(value: 'a) =\n    member this.Value = value\n    static member Of (x: 'a) : Box<'a> = Box(x)"

                let hasFree =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "Free type parameter")

                Expect.isFalse hasFree "class typar in static member signature is in scope"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            // A generic member may introduce an *implicit* type parameter —
            // one named only in a param/return annotation, neither a class typar nor
            // an explicit `<'U>` on the member. MemberRegistration must register it
            // into `MethodTypeParams`, and Unification must keep it in scope across
            // the member-body walk (signature *and* nested lets) — else strict member
            // scope falsely diagnoses "Free type parameter 'U".
            test "implicit member typar in return annotation" {
                let ctx =
                    analyse
                        "type Box<'a>(value: 'a) =\n    member this.Value = value\n    member this.Map (f: 'a -> 'b) : Box<'b> = Box(f value)"

                let hasFree =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "Free type parameter")

                Expect.isFalse hasFree "implicit member typar 'b is in scope"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "implicit member typar in a nested-let body annotation" {
                // `'b` is named in the return *and* in `Comparer<'b>` inside a nested
                // `let` in the body — it must persist past the member's own binding
                // into nested scopes (the set.clr.fs `s.Map` shape).
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

                let hasErr = ctx.Diagnostics |> Seq.exists Diagnostic.isError
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
                // (Elaborate, codegen) still see a problem.
                let ctx = analyseRecovered "let xs = [| 1; 2 ]"

                let hasCloseDiag =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "closing delimiter")

                Expect.isTrue hasCloseDiag "mismatched-delimiter diagnostic emitted"
            }

            // `[<AllowNullLiteral>]` lets `let x: C = null` unify without
            // diagnostics. The current implementation relies on the existing
            // fresh-TyVar behaviour for `null` — the surrounding annotation drives
            // the link to `TyClass`.
            test "[<AllowNullLiteral>] permits `let x: C = null`" {
                let ctx =
                    analyse "[<AllowNullLiteral>]\ntype C() = member this.M () = 1\nlet x : C = null"

                Expect.isEmpty ctx.Diagnostics "no diagnostics on null-binding"
            }
        ]
