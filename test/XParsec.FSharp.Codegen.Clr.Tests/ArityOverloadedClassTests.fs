module XParsec.FSharp.Codegen.Clr.Tests.ArityOverloadedClassTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Step A of the arity-overloaded-classes epic
// (`src/.../docs/arity-overloaded-classes-design.md`). The project-local
// class/interface registry was keyed by bare short name, so two interfaces
// `Fun<'a,'b>` and `Fun<'a,'b,'c>` collided ("Duplicate type definition: Fun").
// Step A gives that registry an `(name, arity)` key (mirroring the union
// machinery) while keeping a bare-name alias for single-arity classes, so every
// existing single-arity reference keeps resolving. No `Fun2` rename here (Step B).

[<Tests>]
let tests =
    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    testList
        "ArityOverloadedClasses"
        [
            // M1 (the positive form of the M0-red collision): two same-named
            // interfaces of different generic arity coexist — the arity key
            // disambiguates them. Before Step A this fragment raised
            // "Duplicate type definition: Fun" (the M0 wall); `typeChecks`
            // asserting NO error is the green form of that red probe.
            test "two same-named interfaces of different arity coexist (no duplicate diagnostic)" {
                typeChecks (
                    String.concat
                        "\n"
                        [
                            "type Fun<'A, 'B> ="
                            "    abstract member Invoke: 'A -> 'B"
                            "type Fun<'A, 'B, 'C> ="
                            "    abstract member Invoke: 'A * 'B -> 'C"
                        ]
                )
            }

            // Use sites of each arity resolve to the right member shape: the
            // arity-3 `Invoke` takes two args, the arity-2 one arg. A wrong-arity
            // use would type-error, proving the registry distinguishes them.
            test "use sites of each arity resolve to the correct member shape" {
                typeChecks (
                    String.concat
                        "\n"
                        [
                            "type Fun<'A, 'B> ="
                            "    abstract member Invoke: 'A -> 'B"
                            "type Fun<'A, 'B, 'C> ="
                            "    abstract member Invoke: 'A * 'B -> 'C"
                            "let useTwo (f: Fun<int, int>) = f.Invoke 1"
                            "let useThree (g: Fun<int, int, int>) = g.Invoke(1, 2)"
                        ]
                )
            }

            // M2 acceptance witness, in-suite: a single-arity interface still
            // resolves by its bare written name (the bare alias survives).
            test "a single-arity interface still resolves by bare name (bare alias survives)" {
                typeChecks (
                    String.concat
                        "\n"
                        [
                            "type Fun<'A, 'B> ="
                            "    abstract member Invoke: 'A -> 'B"
                            "let useTwo (f: Fun<int, int>) = f.Invoke 1"
                        ]
                )
            }

            // M3: two CLASSES (not interfaces) of different arity, each with a
            // distinct instance method, both emit (`Box\`1` / `Box\`2` metadata
            // names) and dispatch to the right member — proving codegen needs no
            // new arity plumbing (the arity-suffixed SymbolKey already separates
            // them). Construction uses explicit `new Box<…>(…)` so the front-end
            // resolves each by its arity-qualified key; bare `Box(…)` application
            // across two arities is the deferred written-arity-resolution hazard
            // (design §6 risk 2), out of Step A's scope.
            test "two same-named classes of different arity emit as Box`1 / Box`2 and dispatch correctly" {
                let _, artifact =
                    compileSource
                        "ArityBox"
                        (String.concat
                            "\n"
                            [
                                "type Box<'A>(a: 'A) ="
                                "    member this.One () = 1"
                                "type Box<'A, 'B>(a: 'A, b: 'B) ="
                                "    member this.Two () = 2"
                                "let p = new Box<int>(0)"
                                "let q = new Box<int, int>(0, 0)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)

                let openTy1 = asm.GetType "Box`1"
                let openTy2 = asm.GetType "Box`2"
                Expect.isNotNull openTy1 "the arity-1 class emits as metadata name Box`1"
                Expect.isNotNull openTy2 "the arity-2 class emits as metadata name Box`2"

                // Reflect over the CONSTRUCTED generic types so the methods are
                // invocable (an open generic def has ContainsGenericParameters).
                let ty1 = openTy1.MakeGenericType(typeof<int>)
                let ty2 = openTy2.MakeGenericType(typeof<int>, typeof<int>)

                let m1 = ty1.GetMethod("One", declaredInstance, null, [||], null)
                let m2 = ty2.GetMethod("Two", declaredInstance, null, [||], null)
                Expect.isNotNull m1 "Box`1 declares One"
                Expect.isNotNull m2 "Box`2 declares Two"

                let inst1 = Activator.CreateInstance(ty1, [| box 0 |])
                let inst2 = Activator.CreateInstance(ty2, [| box 0; box 0 |])
                Expect.equal (m1.Invoke(inst1, [||]) :?> int) 1 "Box`1.One() dispatches to the arity-1 member"
                Expect.equal (m2.Invoke(inst2, [||]) :?> int) 2 "Box`2.Two() dispatches to the arity-2 member"
            }

            // M4: cross-kind name overlap — a class `Foo<'A, 'B>` alongside a
            // union `Foo<'A>` of different arity. Today the union duplicate guard
            // checks `containsUnion name arity || containsRecord name` (no class
            // cross-check), and the class guard checks the union bare alias.
            // After arity-keying, a benign cross-arity coexistence must NOT be
            // rejected and neither mis-resolves the other.
            test "a class and a union of the same name, different arity, coexist without mis-resolution" {
                typeChecks (
                    String.concat
                        "\n"
                        [
                            "type Foo<'A> = Bar of 'A"
                            "type Foo<'A, 'B>(a: 'A, b: 'B) ="
                            "    member this.Sum () = 0"
                            "let u = Bar 1"
                            "let c = Foo(1, 2)"
                        ]
                )
            }
        ]
