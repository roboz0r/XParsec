module XParsec.FSharp.Codegen.Clr.Tests.ClassTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// vesper-set-sprint-plan Phase 1 / B-1 backend tests. The plan's Step 1.5
// test gate calls for compile-and-load tests across three shapes:
//   * `type C() = member this.M() = 1` — instantiate, call M, assert returns 1
//   * `type Box<'a>(v: 'a) = member this.V = v` — generic class with one typar
//   * `type Point(x:int, y:int)` — multi-param ctor + ctor-param field access
// These tests reflect over the emitted PE so a runtime failure (`InvalidProgram`,
// missing field, wrong vtable) surfaces with a legible stack trace through the
// `TestHelpers.loadAssembly` / `Activator.CreateInstance` path.

[<Tests>]
let monoTests =
    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    testList
        "ClassMono"
        [
            test "an emitted class type has a public ctor + one public field per primary-ctor parameter" {
                let _, artifact =
                    compileSource
                        "ClsMeta"
                        (String.concat
                            "\n"
                            [
                                "type Point(x: int, y: int) ="
                                "    member this.Magnitude () = x * x + y * y"
                                "let p = Point(3, 4)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Point"
                Expect.isNotNull ty "the assembly contains the class type Point"

                let ctors = ty.GetConstructors(BindingFlags.Public ||| BindingFlags.Instance)
                Expect.equal ctors.Length 1 "Point declares one public ctor"
                Expect.equal (ctors.[0].GetParameters().Length) 2 "Point ctor takes the two ctor params"

                let fields = ty.GetFields(BindingFlags.Public ||| BindingFlags.Instance)
                let names = fields |> Array.map (fun f -> f.Name) |> Set.ofArray
                Expect.equal names (Set.ofList [ "x"; "y" ]) "Point exposes both ctor-param backing fields publicly"
            }

            test "a class with `member this.M () = 1` emits an instance method that returns 1" {
                let _, artifact =
                    compileSource
                        "ClsM1"
                        (String.concat "\n" [ "type C() ="; "    member this.M () = 1"; "let c = C()" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "C"
                Expect.isNotNull ty "the assembly contains the class type C"

                let m = ty.GetMethod("M", declaredInstance, null, [||], null)
                Expect.isNotNull m "M emitted as an instance method"

                let instance = Activator.CreateInstance(ty, [||])
                let result = m.Invoke(instance, [||]) :?> int
                Expect.equal result 1 "C().M() returns 1"
            }

            test "a class instance member reads a ctor-param backing field (Magnitude(3,4) returns 25)" {
                let _, artifact =
                    compileSource
                        "ClsMag"
                        (String.concat
                            "\n"
                            [
                                "type Point(x: int, y: int) ="
                                "    member this.Magnitude () = x * x + y * y"
                                "let p = Point(0, 0)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Point"

                let m = ty.GetMethod("Magnitude", declaredInstance, null, [||], null)
                Expect.isNotNull m "Magnitude emitted as an instance method"

                let instance = Activator.CreateInstance(ty, [| box 3; box 4 |])
                let result = m.Invoke(instance, [||]) :?> int
                Expect.equal result 25 "Magnitude reads ctor params x=3 / y=4 via backing fields and computes 25"
            }

            test "a class auto-property (member this.X = x) round-trips the ctor param" {
                let _, artifact =
                    compileSource
                        "ClsProp"
                        (String.concat
                            "\n"
                            [
                                "type Point(x: int, y: int) ="
                                "    member this.X = x"
                                "    member this.Y = y"
                                "let p = Point(0, 0)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Point"

                // Properties emit as `get_<Name>` methods (no PropertyDef row in
                // v1 — see P3d.3).
                let getX = ty.GetMethod("get_X", declaredInstance, null, [||], null)
                let getY = ty.GetMethod("get_Y", declaredInstance, null, [||], null)
                Expect.isNotNull getX "get_X emitted"
                Expect.isNotNull getY "get_Y emitted"

                let instance = Activator.CreateInstance(ty, [| box 7; box 9 |])
                Expect.equal (getX.Invoke(instance, [||]) :?> int) 7 "Point(7,9).X = 7"
                Expect.equal (getY.Invoke(instance, [||]) :?> int) 9 "Point(7,9).Y = 9"
            }

            test "a class with no ctor params + a method that references no captures emits cleanly" {
                let _, artifact =
                    compileSource
                        "ClsNullary"
                        (String.concat "\n" [ "type C() ="; "    member this.M () = 42"; "let c = C()" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "C"

                Expect.isNotNull ty "C emitted"
                let ctors = ty.GetConstructors(BindingFlags.Public ||| BindingFlags.Instance)
                Expect.equal ctors.Length 1 "single public ctor"
                Expect.equal (ctors.[0].GetParameters().Length) 0 "C ctor takes no parameters"

                let fields = ty.GetFields(BindingFlags.Public ||| BindingFlags.Instance)
                Expect.isEmpty fields "C has no backing fields (no ctor params)"
            }

            test "a class is reference-equal by default — no IEquatable<Self> + no synthesised Equals override" {
                let _, artifact =
                    compileSource
                        "ClsRefEq"
                        (String.concat "\n" [ "type C() ="; "    member this.M () = 1"; "let c = C()" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "C"

                Expect.isNull
                    (ty.GetMethod("Equals", declaredInstance, null, [| typeof<obj> |], null))
                    "no Equals(object) override on the class itself"

                Expect.isNull
                    (ty.GetMethod("GetHashCode", declaredInstance, null, [||], null))
                    "no GetHashCode override on the class itself"

                let iface = typedefof<IEquatable<_>>.MakeGenericType ty
                Expect.isFalse (iface.IsAssignableFrom ty) "C does not declare IEquatable<C>"
            }

            test "a class emission does not pin FSharp.Core" {
                let _, artifact =
                    compileSource
                        "ClsNoDep"
                        (String.concat "\n" [ "type C() ="; "    member this.M () = 1"; "let c = C()" ])

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf "class emission only references the BCL (%A)" artifact.FSharpCoreDependencies)
            }

            // vesper-set-sprint-plan §1.6 / B-8: a class without `[<Sealed>]`
            // emits an *open* `TypeDefinition` (no `TypeAttributes.Sealed`)
            // so Phase 2's inheritance can derive from it; `[<Sealed>]` flips
            // the flag.
            test "a class without [<Sealed>] is not sealed" {
                let _, artifact =
                    compileSource
                        "ClsOpen"
                        (String.concat "\n" [ "type C() ="; "    member this.M () = 1"; "let c = C()" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "C"

                Expect.isFalse ty.IsSealed "C is not sealed without [<Sealed>]"
            }

            test "a class with [<Sealed>] has TypeAttributes.Sealed set" {
                let _, artifact =
                    compileSource
                        "ClsSealed"
                        (String.concat "\n" [ "[<Sealed>]"; "type C() ="; "    member this.M () = 1"; "let c = C()" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "C"

                Expect.isTrue ty.IsSealed "[<Sealed>] sets the sealed IL flag"

                let m = ty.GetMethod("M", declaredInstance, null, [||], null)
                Expect.isNotNull m "M is still emitted on a sealed class"

                let instance = Activator.CreateInstance(ty, [||])
                let result = m.Invoke(instance, [||]) :?> int
                Expect.equal result 1 "sealed C().M() still returns 1"
            }
        ]

[<Tests>]
let staticTests =
    let declaredStatic =
        BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly

    testList
        "ClassStatic"
        [
            // vesper-set-sprint-plan §1.8 / B-10 test gate:
            //   `type C() = static member M () = 1` — C.M() = 1
            test "a class static method emits with the Static flag and returns 1" {
                let _, artifact =
                    compileSource
                        "ClsStaticM"
                        (String.concat "\n" [ "type C() ="; "    static member M () = 1"; "let c = C()" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "C"
                Expect.isNotNull ty "the assembly contains the class type C"

                let m = ty.GetMethod("M", declaredStatic, null, [||], null)
                Expect.isNotNull m "M emitted as a static method"
                Expect.isTrue m.IsStatic "M is static"

                let result = m.Invoke(null, [||]) :?> int
                Expect.equal result 1 "C.M() returns 1"
            }

            // static field via `static let`:
            //   `type C() = static let x = 42; static member Get() = x` — C.Get() = 42
            test "a class `static let` becomes a static field initialised by the cctor (Get() = 42)" {
                let _, artifact =
                    compileSource
                        "ClsStaticLet"
                        (String.concat
                            "\n"
                            [
                                "type C() ="
                                "    static let x = 42"
                                "    static member Get () = x"
                                "let c = C()"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "C"
                Expect.isNotNull ty "the assembly contains the class type C"

                let m = ty.GetMethod("Get", declaredStatic, null, [||], null)
                Expect.isNotNull m "Get emitted as a static method"

                let result = m.Invoke(null, [||]) :?> int
                Expect.equal result 42 "C.Get() reads the static-let field x = 42"
            }

            // A `static let` is in scope for instance members too (F# §8.7), so an
            // instance method reads the same static field via `ldsfld`.
            test "an instance member reads a `static let` field (this.Get() = 7)" {
                let _, artifact =
                    compileSource
                        "ClsStaticLetInst"
                        (String.concat
                            "\n"
                            [
                                "type C() ="
                                "    static let k = 7"
                                "    member this.Get () = k"
                                "let c = C()"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "C"

                let m =
                    ty.GetMethod(
                        "Get",
                        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly,
                        null,
                        [||],
                        null
                    )

                Expect.isNotNull m "Get emitted as an instance method"

                let instance = Activator.CreateInstance(ty, [||])
                Expect.equal (m.Invoke(instance, [||]) :?> int) 7 "instance Get() reads the static-let field k = 7"
            }
        ]

[<Tests>]
let genericTests =
    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    testList
        "ClassGeneric"
        [
            test "a generic class `Box<'T>` is emitted as a generic TypeDefinition" {
                let _, artifact =
                    compileSource
                        "ClsGenMeta"
                        (String.concat "\n" [ "type Box<'a>(v: 'a) ="; "    member this.V = v"; "let b = Box(0)" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let boxTy = asm.GetType "Box`1"
                Expect.isNotNull boxTy "Box`1 emitted"
                Expect.isTrue boxTy.IsGenericTypeDefinition "Box`1 is a generic type definition"

                let boxInt = boxTy.MakeGenericType typeof<int>

                let ctor = boxInt.GetConstructors(BindingFlags.Public ||| BindingFlags.Instance).[0]
                Expect.equal (ctor.GetParameters().Length) 1 "Box<int> ctor takes one param"

                let getV = boxInt.GetMethod("get_V", declaredInstance, null, [||], null)
                Expect.isNotNull getV "Box<int>::get_V reachable"
            }

            test "Box<int>(42).V returns 42" {
                let _, artifact =
                    compileSource
                        "ClsGenIntV"
                        (String.concat "\n" [ "type Box<'a>(v: 'a) ="; "    member this.V = v"; "let b = Box(0)" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let boxInt = (asm.GetType "Box`1").MakeGenericType typeof<int>

                let getV = boxInt.GetMethod("get_V", declaredInstance, null, [||], null)
                let instance = Activator.CreateInstance(boxInt, [| box 42 |])
                let result = getV.Invoke(instance, [||]) :?> int
                Expect.equal result 42 "Box(42).V = 42 — ctor-param field reads through `MemberRef` on TypeSpec"
            }

            test "Box<string>(\"hi\").V returns \"hi\" — same emitted body works at any instantiation" {
                let _, artifact =
                    compileSource
                        "ClsGenStrV"
                        (String.concat "\n" [ "type Box<'a>(v: 'a) ="; "    member this.V = v"; "let b = Box(0)" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let boxStr = (asm.GetType "Box`1").MakeGenericType typeof<string>

                let getV = boxStr.GetMethod("get_V", declaredInstance, null, [||], null)
                let instance = Activator.CreateInstance(boxStr, [| box "hi" |])
                let result = getV.Invoke(instance, [||]) :?> string
                Expect.equal result "hi" "Box(\"hi\").V = \"hi\""
            }
        ]
