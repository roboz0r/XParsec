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

            test "property read pins the object argument via annotation" {
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

            // A member signature may name the enclosing class typar. The fresh typar scope
            // minted per binding must be seeded with the class typars first, else the
            // annotation hits an empty strict scope and falsely diagnoses "Free type parameter".
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

            // An *implicit* member typar — named only in a param/return annotation, neither a
            // class typar nor an explicit `<'b>` on the member — must be registered and stay in
            // scope for the whole member body, else strict scope diagnoses "Free type parameter".
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
                // `'b` is named in the return *and* in the nested `let g : 'b -> 'b`, so it
                // must persist past the member's own binding into nested scopes.
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

            test "static member on an object-argument instance diagnoses" {
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

            // `'T[]` declares `interface seq<'T>`, so an array subsumes to the capability at
            // an argument position exactly as a cons-list does. The front end refused this
            // until the array carried the declaration, while both backends could already
            // lower it — a CLR `T[]` implements `IEnumerable<T>` and a JS array is iterable.
            test "an `int[]` argument subsumes to a `seq<int>` parameter" {
                let ctx =
                    analyse "let count (s: seq<int>) : int = 0\nlet f (a: int[]) : int = count a"

                Expect.isEmpty ctx.Diagnostics "an array is accepted where `seq<int>` is asked for"
            }

            test "an `int[]` argument does NOT subsume to `seq<string>`" {
                let ctx =
                    analyse "let count (s: seq<string>) : int = 0\nlet f (a: int[]) : int = count a"

                Expect.isNonEmpty ctx.Diagnostics "the element type still has to match"
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
                // The parser virtual-inserts `|]` and reports it on its own stream. Semantic
                // analysis must repeat the breakage on `ctx.Diagnostics`, which is all a
                // downstream consumer reads.
                let ctx = analyseRecovered "let xs = [| 1; 2 ]"

                let hasCloseDiag =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "closing delimiter")

                Expect.isTrue hasCloseDiag "mismatched-delimiter diagnostic emitted"
            }

            test "parameterless getter types as the property's value" {
                let ctx = analyse "type C() =\n    member this.P with get () = 1"

                let info = expectClass ctx "C"
                let m = info.Members |> Array.find (fun mm -> mm.Name = "P")

                Expect.equal (typeOf ctx m.DeclSite.Key) (TyConst(RuntimeNames.intKey, EqArray.empty)) "P : int"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "indexed getter takes its index as a parameter" {
                let ctx = analyse "type C() =\n    member this.Item with get (i: int) = i"

                let info = expectClass ctx "C"
                let m = info.Members |> Array.find (fun mm -> mm.Name = "get_Item")
                let intTy = TyConst(RuntimeNames.intKey, EqArray.empty)

                Expect.equal (typeOf ctx m.DeclSite.Key) (TyFun(intTy, intTy)) "get_Item : int -> int"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "an abstract property signature types both of its slots" {
                let ctx = analyse "type C() =\n    abstract P: int with get, set"

                let info = expectClass ctx "C"

                // A slot has no body binding, so its type is read off the registry entry.
                let slotTy n =
                    Unification.zonk ctx.Store (info.Members |> Array.find (fun mm -> mm.Name = n)).Type

                let intTy = TyConst(RuntimeNames.intKey, EqArray.empty)
                let unitTy = TyConst(RuntimeNames.unitKey, EqArray.empty)

                Expect.equal (slotTy "P") intTy "P : int"
                Expect.equal (slotTy "set_P") (TyFun(intTy, unitTy)) "set_P : int -> unit"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "an abstract indexed property signature types index-then-value on its setter" {
                let ctx = analyse "type C() =\n    abstract Item: int -> string with get, set"

                let info = expectClass ctx "C"

                // A slot has no body binding, so its type is read off the registry entry.
                let slotTy n =
                    Unification.zonk ctx.Store (info.Members |> Array.find (fun mm -> mm.Name = n)).Type

                let intTy = TyConst(RuntimeNames.intKey, EqArray.empty)
                let stringTy = TyConst(RuntimeNames.stringKey, EqArray.empty)
                let unitTy = TyConst(RuntimeNames.unitKey, EqArray.empty)

                Expect.equal (slotTy "get_Item") (TyFun(intTy, stringTy)) "get_Item : int -> string"

                Expect.equal
                    (slotTy "set_Item")
                    (TyFun(intTy, TyFun(stringTy, unitTy)))
                    "set_Item : int -> string -> unit"

                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "a declared indexer types `x.[i]` as its element type" {
                let src =
                    "type C(v: int) =\n    member this.Item with get (i: int) = i + v\nlet c = C(1)\nlet x = c.[2]"

                let ctx = analyse src
                Expect.equal (typeOf ctx (keyOfLet src "x")) BuiltinTypes.tyInt "x : int"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "a generic indexer reads at the object argument's type args" {
                let src =
                    "type Box<'T>(v: 'T) =\n    member this.Item with get (i: int) = v\nlet b = Box(\"s\")\nlet x = b.[0]"

                let ctx = analyse src
                Expect.equal (typeOf ctx (keyOfLet src "x")) BuiltinTypes.tyString "x : string"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "a record's declared indexer types `r.[i]` too" {
                let src =
                    "type R =\n    { N: int }\n    member this.Item with get (i: int) = i + this.N\nlet r = { N = 1 }\nlet x = r.[2]"

                let ctx = analyse src
                Expect.equal (typeOf ctx (keyOfLet src "x")) BuiltinTypes.tyInt "x : int"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            // Nothing readable is named `Item`, so the write is typed from `set_Item` alone.
            test "a write-only indexer types `x.[i] <- v` from its setter" {
                let src =
                    "type C() =\n    let mutable q = 0\n    member this.Item with set (i: int) (w: int) = q <- i + w\nlet c = C()\nlet u = (c.[2] <- 3)"

                let ctx = analyse src
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            // The qualifier is a TYPE, so there is no object argument to resolve a setter from:
            // the write goes through the static `set_P` the resolved qualifier declares.
            test "a static property write types `C.P <- v` from its static setter" {
                let src =
                    "type C() =\n    static let mutable q = 0\n    static member P with get () = q and set (w: int) = q <- w\nlet u = (C.P <- 3)"

                let ctx = analyse src
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            // Nothing is named `Q` but the setter, so the name resolves through `set_Q` alone.
            test "a write-only static property types `C.Q <- v` from its setter" {
                let src =
                    "type C() =\n    static let mutable q = 0\n    static member Q with set (w: int) = q <- w\nlet u = (C.Q <- 3)"

                let ctx = analyse src
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            // `C<int>.P` parses as `DotLookup(TypeApp …)`, not the folded LongIdent the bare
            // form takes, so the write reaches its setter down a second classification arm.
            test "a static property write types `C<int>.P <- v` from its static setter" {
                let src =
                    "type C<'T>() =\n    static let mutable q = 0\n    static member P with get () = q and set (w: int) = q <- w\nlet u = (C<int>.P <- 3)"

                let ctx = analyse src
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            // A record's augmentation bears statics exactly as a class's does.
            test "a static property write types `R.P <- v` on a record" {
                let src =
                    "type R =\n    { X: int }\n    static member P with get () = 0 and set (w: int) = ()\nlet u = (R.P <- 3)"

                let ctx = analyse src
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            // `set_Q : i -> v -> unit` wants an index that `C.Q <- 3` supplies nothing for, so
            // the setter's ARITY is what rejects the write.
            test "a write to an INDEXED static property says the index is missing" {
                let src =
                    "type C() =\n    static let mutable q = 0\n    static member Q with set (i: int) (w: int) = q <- i + w\nlet u = (C.Q <- 3)"

                let ctx = analyse src

                Expect.equal
                    (ctx.Diagnostics |> Seq.map (fun d -> d.Message) |> List.ofSeq)
                    [ "'Q' is an indexed property; a write must supply its index" ]
                    "one diagnostic, naming the index rather than a type mismatch"
            }

            // `set_Q` makes the NAME resolve, so a read gets past name resolution; the error
            // it then earns names the reason rather than reporting an unknown name.
            test "a read of a write-only static property says so" {
                let src =
                    "type C() =\n    static let mutable q = 0\n    static member Q with set (w: int) = q <- w\nlet x = C.Q"

                let ctx = analyse src

                Expect.equal
                    (ctx.Diagnostics |> Seq.map (fun d -> d.Message) |> List.ofSeq)
                    [ "Property 'C.Q' is write-only" ]
                    "one diagnostic, naming the property rather than the qualified name"
            }

            // The array declares its own `get_Item`, so it reaches the same accessor lookup
            // every other indexable type does.
            test "an array index is unaffected by the accessor lookup" {
                let src = "let xs = [| 1; 2 |]\nlet x = xs.[1]"

                let ctx = analyse src
                Expect.equal (typeOf ctx (keyOfLet src "x")) BuiltinTypes.tyInt "x : int"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            // The miss names the OBJECT ARGUMENT's type. Before the array carried its own
            // accessor, an unindexable object argument was unified against `'T[]` instead and
            // reported `Type mismatch: [] vs C` — a complaint about the wrong type.
            test "a type declaring no indexer reports the miss against its own type" {
                let ctx = analyse "type C() =\n    member this.M () = 1\nlet c = C()\nlet x = c.[0]"

                Expect.contains
                    (ctx.Diagnostics |> Seq.map (fun d -> d.Message) |> List.ofSeq)
                    "Type 'C' has no instance member 'get_Item'"
                    "the diagnostic names `C`, not the array intrinsic"
            }

            // A WRITE blames the accessor a write needs. The LHS walk types the element off the
            // getter, so it must stay silent when there is none, or the miss reads `get_Item`.
            test "a write to a type declaring no indexer blames `set_Item`, once" {
                let ctx =
                    analyse "type C() =\n    member this.M () = 1\nlet c = C()\nlet u = (c.[0] <- 1)"

                let messages = ctx.Diagnostics |> Seq.map (fun d -> d.Message) |> List.ofSeq

                Expect.equal messages [ "Type 'C' has no instance member 'set_Item'" ] "one miss, naming the setter"
            }

            // `string` declares `Item` with a GETTER only, so the read half resolves and only
            // the write is missing.
            test "a write to a read-only external indexer blames `set_Item`" {
                let ctx = analyse "let f (s: string) : unit = s.[0] <- 'x'"

                let messages = ctx.Diagnostics |> Seq.map (fun d -> d.Message) |> List.ofSeq

                Expect.equal
                    messages
                    [ "Type 'string' has no instance member 'set_Item'" ]
                    "the getter resolved; only the setter is missing"
            }

            // `null` types as a fresh TyVar that the annotation links to `TyClass C`; nothing
            // yet consults the attribute, so this passes without it too.
            test "[<AllowNullLiteral>] permits `let x: C = null`" {
                let ctx =
                    analyse "[<AllowNullLiteral>]\ntype C() = member this.M () = 1\nlet x : C = null"

                Expect.isEmpty ctx.Diagnostics "no diagnostics on null-binding"
            }
        ]
