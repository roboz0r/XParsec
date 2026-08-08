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
            // Types `inherit Base(args)` against the parent's primary
            // ctor and mints the `base` TyVar. Inherited member access / `base.M()`
            // resolution gate on the member-chain walk.

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

            // --- member-chain lookup + `subsumes` ---
            // `resolveFieldStep` / `dischargePendingDotAccess` recurse into the
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
        ]
