module XParsec.FSharp.Codegen.Clr.Tests.ArityOverloadedClassTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection
open XParsec.FSharp.Codegen.Clr.Tests.PackageHarness

// Two project-local type definitions sharing a name at different generic arity
// (`Fun<'a,'b>` and `Fun<'a,'b,'c>`) must coexist and resolve apart: the registry claims
// `(name, arity)`, while a single-arity name still resolves bare.

[<Tests>]
let tests =
    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    testList
        "ArityOverloadedClasses"
        [
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

            // The arity-3 `Invoke` takes two args and the arity-2 one, so a use site
            // that reached the other arity's member would type-error on arg count.
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

            // Constructed with explicit `new Box<…>(…)` so the front end resolves each by
            // its arity-qualified key rather than by written arity at the application.
            test "two same-named classes of different arity emit as Box`1 / Box`2 and dispatch correctly" {
                let artifact =
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

            // Cross-KIND overlap: a union and a class share a name at different arities,
            // so they hold different `(name, arity)` claims and neither is rejected.
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
