module XParsec.FSharp.SemanticAnalysis.Tests.UnificationInheritanceTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers
open XParsec.FSharp.SemanticAnalysis.Tests.UnificationTestHelpers

[<Tests>]
let tests =
    testList
        "Unification.Inheritance"
        [
            // --- base-ctor typing + `base` binding ---

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

                match TypeRegistry.tryClass ctx.Types UseSite.unbounded "D" with
                | ValueSome info ->
                    Expect.equal
                        (typeOf ctx (BoundVarKey.identity info.BaseKey))
                        (TyClass("B", EqArray.empty))
                        "base : B"
                | ValueNone -> failtest "class type D not registered"
            }

            // The registration-time mini translation (`resolveInheritArgName`) falls back to an
            // opaque `TyConst` for a name nothing resolves, where the ordinary type walk
            // diagnoses `UndefinedType` (see `TypeRefVerdictTests`).
            ptest "GAP an unresolvable inherit type-argument mints an opaque TyConst undiagnosed" {
                let ctx =
                    analyse "type Box<'a>(v: obj) =\n    member this.V = v\ntype D() =\n    inherit Box<Gadget>(obj ())"

                let errors =
                    ctx.Diagnostics |> Diagnostic.errors |> Seq.map (fun d -> d.Kind) |> List.ofSeq

                Expect.equal errors [ Kind.UndefinedType "Gadget" ] "the written name is diagnosed where it is written"
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

            // Inheriting the intrinsic `exn` checks the args against its contract ctors
            // (`new: message: string -> exn`), so a mis-typed arg diagnoses here, not in codegen.

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

            // --- member-chain lookup + `subsumes` ---

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

            test "a local type sharing an intrinsic's name wins in an inherit argument" {
                let input =
                    "type Box<'a>(v: 'a) =\n    member this.V = v\n"
                    + "type exn = { X: int }\n"
                    + "type W(v: exn) =\n    inherit Box<exn>(v)\n"
                    + "let w = W({ X = 1 })\nlet r = w.V.X"

                let ctx = analyse input
                let patKey = NodeKey.ofSource (input.IndexOf "r = w.V.X") NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) BuiltinTypes.tyInt "r : int — 'a bound to the local record"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "instance member on arity-overloaded class resolves per arity" {
                // `Box`1`/`Box`2` share a short name, so each class's body must be walked by
                // its arity-key: a bare-name miss leaves `this`/ctor params unbound and
                // `.Peek` — the last typar of each — would type as a free var.
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
                // A typar constrained to one arity (`'S :> IBox<int>`) must resolve `.Peek()`
                // through the arity-keyed interface, not a bare-name strip. `Peek` returns the
                // LAST typar of each, so an arity-blind walk would collapse int and string.
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

            // --- an `inherit` clause's type ARGUMENT resolves as any other type position ---

            test "a qualified inherit type argument resolves to the type it names" {
                // `inherit Base<A.T>` instantiates `'a` at `A.T`, so `W`'s parameter is `A.T`
                // and an `int` argument is FS0001. A free TyVar here would take the `int`.
                let ctx =
                    analyse (
                        String.concat
                            "\n"
                            [
                                "module A ="
                                "    type T = { X: int }"
                                "type Base<'a>(n: int) ="
                                "    member this.W (y: 'a) = y"
                                "type D() ="
                                "    inherit Base<A.T>(1)"
                                "let d = D()"
                                "let bad = d.W 42"
                            ]
                    )

                let es = errors ctx

                Expect.isTrue
                    (es |> List.exists (fun d -> d.Message.Contains "mismatch"))
                    (sprintf "int against A.T is a mismatch; diagnostics were %A" es)
            }

            test "an inherit type argument at the wrong arity reports the arity alone" {
                // `T` is claimed only at arity 1, so the written `T` is FS0033. Its args are
                // fitted to the claim's arity, leaving a nominal that unifies with `T<int>`
                // instead of a same-key mismatch reported beside the arity error.
                let ctx =
                    analyse (
                        String.concat
                            "\n"
                            [
                                "type Base<'a>(x: 'a) ="
                                "    member this.V = x"
                                "type T<'a>(y: 'a) ="
                                "    member this.Y = y"
                                "type D(t: T<int>) ="
                                "    inherit Base<T>(t)"
                            ]
                    )

                let es = errors ctx |> List.map (fun d -> d.Message)

                Expect.equal
                    es
                    [ "Type 'T' expects 1 type argument(s) but got 0" ]
                    (sprintf "the arity error alone; diagnostics were %A" es)
            }

            // --- an `inherit` clause's PARENT resolves as any other type position ---

            test "a qualified inherit parent resolves to the class it names" {
                let ctx =
                    analyse (
                        String.concat
                            "\n"
                            [
                                "module A ="
                                "    type Base<'a>(v: 'a) ="
                                "        member this.V = v"
                                "type D() ="
                                "    inherit A.Base<int>(1)"
                            ]
                    )

                Expect.isEmpty (errors ctx) "a qualified parent resolves"

                match (expectClass ctx "D").Base with
                | ValueSome b -> Expect.equal b.Parent.Key.Name "Base" "D's base is A.Base<int>"
                | ValueNone -> failtest "D records no base"
            }

            test "a qualified inherit parent's ctor argument is checked" {
                let ctx =
                    analyse (
                        String.concat
                            "\n"
                            [
                                "module A ="
                                "    type Base<'a>(v: 'a) ="
                                "        member this.V = v"
                                "type D() ="
                                "    inherit A.Base<int>(\"s\")"
                            ]
                    )

                let es = errors ctx

                Expect.isTrue
                    (es |> List.exists (fun d -> d.Message.Contains "mismatch"))
                    (sprintf "string against int is a mismatch; diagnostics were %A" es)
            }

            test "an abbreviation of a class is an inheritable parent" {
                let ctx =
                    analyse (
                        String.concat
                            "\n"
                            [
                                "type B(x: int) ="
                                "    member this.X = x"
                                "type Alias = B"
                                "type D() ="
                                "    inherit Alias(1)"
                            ]
                    )

                Expect.isEmpty (errors ctx) "the abbreviation resolves to the class"

                match (expectClass ctx "D").Base with
                | ValueSome b -> Expect.equal b.Parent.Key.Name "B" "D's base is the aliased class"
                | ValueNone -> failtest "D records no base"
            }

            test "an abbreviation instantiating a generic class is an inheritable parent" {
                let ctx =
                    analyse (
                        String.concat
                            "\n"
                            [
                                "type Box<'a>(v: 'a) ="
                                "    member this.V = v"
                                "type IntBox = Box<int>"
                                "type D() ="
                                "    inherit IntBox(1)"
                            ]
                    )

                Expect.isEmpty (errors ctx) "the abbreviation resolves to Box<int>"

                match (expectClass ctx "D").Base with
                | ValueSome b -> Expect.equal b.Parent.Nominal.Args (EqArray.singleton intTy) "instantiated at int"
                | ValueNone -> failtest "D records no base"
            }

            test "a record is not an inheritable parent" {
                let ctx =
                    analyse "type R = { X: int }\ntype D() =\n    inherit R\n    member this.M () = 1"

                let es = errors ctx |> List.map (fun d -> d.Message)

                Expect.equal
                    es
                    [ "Cannot inherit from type 'R', because only classes are inheritable" ]
                    (sprintf "the kind rejection alone; diagnostics were %A" es)

                Expect.isTrue (expectClass ctx "D").Base.IsNone "the record is not recorded as D's base"
            }

            test "a union is not an inheritable parent" {
                let ctx =
                    analyse "type U =\n    | A\n    | B\ntype D() =\n    inherit U\n    member this.M () = 1"

                let es = errors ctx |> List.map (fun d -> d.Message)

                Expect.equal
                    es
                    [ "Cannot inherit from type 'U', because only classes are inheritable" ]
                    (sprintf "the kind rejection alone; diagnostics were %A" es)
            }

            test "an enum is not an inheritable parent" {
                let ctx =
                    analyse "type E =\n    | A = 1\n    | B = 2\ntype D() =\n    inherit E\n    member this.M () = 1"

                let es = errors ctx |> List.map (fun d -> d.Message)

                Expect.equal
                    es
                    [ "Cannot inherit from type 'E', because only classes are inheritable" ]
                    (sprintf "the kind rejection alone; diagnostics were %A" es)
            }

            test "a local interface is not an inheritable parent" {
                let ctx =
                    analyse "type I =\n    abstract M: int\ntype D() =\n    inherit I\n    member this.M () = 1"

                let es = errors ctx |> List.map (fun d -> d.Message)

                Expect.equal
                    es
                    [ "Cannot inherit from interface 'I'; implement it with 'interface I with'" ]
                    (sprintf "the interface rejection alone; diagnostics were %A" es)
            }

            test "an unknown inherit parent is an undefined type" {
                let ctx = analyse "type D() =\n    inherit Gadget()\n    member this.M () = 1"

                let kinds = errors ctx |> List.map (fun d -> d.Kind)

                Expect.equal kinds [ Kind.UndefinedType "Gadget" ] "FS0039 at the written name, once"
                Expect.isTrue (expectClass ctx "D").Base.IsNone "no base is recorded"
            }

            // The contract declares `Attribute`'s ctor, as it does `exn`'s.

            test "the heritable `Attribute` primitive is inherited by canon" {
                let ctx = analyse "type MarkAttribute() =\n    inherit Attribute()"

                Expect.isEmpty (errors ctx) "Attribute() checks against the contract ctor"

                match (expectClass ctx "MarkAttribute").Base with
                | ValueSome {
                                Parent = BaseParentG.PrimitiveCanon n
                            } -> Expect.equal n.Key.Name "Attribute" "the canon"
                | ValueSome { Parent = other } -> failtestf "expected a canon base, got %A" other
                | ValueNone -> failtest "MarkAttribute records no base"
            }

            test "the heritable `exn` primitive is inherited by canon" {
                let ctx = analyse "type MyErr(m: string) =\n    inherit exn(m)"

                match (expectClass ctx "MyErr").Base with
                | ValueSome {
                                Parent = BaseParentG.PrimitiveCanon n
                            } -> Expect.equal n.Key.Name "exn" "the canon"
                | ValueSome { Parent = other } -> failtestf "expected a canon base, got %A" other
                | ValueNone -> failtest "MyErr records no base"
            }

            // --- `override` conforms to the slot it targets ---

            test "override of a base-declared virtual conforms to the base slot" {
                // `Equals : B -> bool` is B's own slot; conforming it to `obj -> bool`
                // instead would report a diagnostic against a correct override.
                let ctx =
                    analyse
                        "type B() =\n    abstract member Equals : B -> bool\ntype D() =\n    inherit B()\n    override this.Equals (that: B) = true"

                Expect.isEmpty ctx.Diagnostics (sprintf "no diagnostics — B declares the slot: %A" ctx.Diagnostics)
            }

            test "an unannotated override takes its parameter type from the base slot" {
                let ctx =
                    analyse
                        "type B() =\n    abstract member Store : int -> unit\ntype D() =\n    inherit B()\n    override this.Store x = ()"

                let d = expectClass ctx "D"

                match d.Members |> Array.tryFind (fun m -> m.Name = "Store") with
                | Some m ->
                    Expect.equal
                        (Unification.zonk ctx.Store m.Type)
                        (TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyUnit))
                        "D.Store : int -> unit"
                | None -> failtest "D.Store not registered"
            }

            test "override Equals under a base declaring none pins the Object slot" {
                let ctx =
                    analyse
                        "type B() =\n    member this.X = 1\ntype D() =\n    inherit B()\n    override this.Equals that = true"

                let d = expectClass ctx "D"

                match d.Members |> Array.tryFind (fun m -> m.Name = "Equals") with
                | Some m ->
                    Expect.equal
                        (Unification.zonk ctx.Store m.Type)
                        (TyFun(TyConst(RuntimeNames.objKey, EqArray.empty), BuiltinTypes.tyBool))
                        "D.Equals : obj -> bool"
                | None -> failtest "D.Equals not registered"
            }

            // --- `:>` / `:?` / `:?>` arms ---

            test "`:>` upcast to declared base types as the base" {
                let input =
                    "type B() =\n    member this.X = 1\ntype D() =\n    inherit B()\nlet s = (new D()) :> B"

                let ctx = analyse input
                let patKey = NodeKey.ofSource (input.IndexOf "s = ") NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyClass("B", EqArray.empty)) "s : B"
                Expect.isEmpty ctx.Diagnostics "no diagnostics — D <: B"
            }

            test "`:>` upcast resolves interfaces of an arity-overloaded local host" {
                // Upcasting a `Base<int, string>` to the interface it declares walks onto the
                // arity-2 host: its `interface … with` impls must be found by the object
                // argument's `SymbolKey`, since a bare-name read misses `Base`2` and diagnoses.
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
        ]
