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
let secondaryCtorTests =
    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    testList
        "ClassSecondaryCtor"
        [
            // vesper-set-sprint-plan §1.9 / B-11 test gate:
            //   `type C(x: int) = new() = C(0)` — C() returns C(0), C(5) returns C(5).
            // The secondary ctor emits as a `.ctor` overload that chains to the
            // primary `.ctor`; reading `this.X` afterwards proves the chain ran.
            test "a secondary ctor `new() = C(0)` chains to the primary ctor (C().X = 0, C(5).X = 5)" {
                let _, artifact =
                    compileSource
                        "ClsSecCtor"
                        (String.concat
                            "\n"
                            [
                                "type C(x: int) ="
                                "    new() = C(0)"
                                "    member this.X = x"
                                "let c = C()"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "C"
                Expect.isNotNull ty "the assembly contains the class type C"

                let ctors = ty.GetConstructors(BindingFlags.Public ||| BindingFlags.Instance)
                Expect.equal ctors.Length 2 "C declares the primary ctor + one secondary ctor"

                let arities = ctors |> Array.map (fun c -> c.GetParameters().Length) |> Set.ofArray
                Expect.equal arities (Set.ofList [ 0; 1 ]) "one nullary (secondary) and one 1-arg (primary) ctor"

                let getX = ty.GetMethod("get_X", declaredInstance, null, [||], null)
                Expect.isNotNull getX "get_X emitted"

                let viaSecondary = Activator.CreateInstance(ty, [||])
                Expect.equal (getX.Invoke(viaSecondary, [||]) :?> int) 0 "C() chains to C(0), so X = 0"

                let viaPrimary = Activator.CreateInstance(ty, [| box 5 |])
                Expect.equal (getX.Invoke(viaPrimary, [||]) :?> int) 5 "C(5).X = 5"
            }

            // A secondary ctor that forwards its own parameter to a *different-arity*
            // primary chain (`type C2(x:int, y:int) = new(x:int) = C2(x, 0)`): the
            // overload's param feeds the chain alongside a constant.
            test "a secondary ctor forwards its parameter to the primary chain (C2(9).Sum = 9)" {
                let _, artifact =
                    compileSource
                        "ClsSecCtorFwd"
                        (String.concat
                            "\n"
                            [
                                "type C2(x: int, y: int) ="
                                "    new(x: int) = C2(x, 0)"
                                "    member this.Sum = x + y"
                                "let c = C2(0)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "C2"

                let ctors = ty.GetConstructors(BindingFlags.Public ||| BindingFlags.Instance)
                Expect.equal ctors.Length 2 "primary (2-arg) + secondary (1-arg) ctor"

                let getSum = ty.GetMethod("get_Sum", declaredInstance, null, [||], null)
                let instance = Activator.CreateInstance(ty, [| box 9 |])
                Expect.equal (getSum.Invoke(instance, [||]) :?> int) 9 "C2(9) chains to C2(9, 0): Sum = 9"
            }

            // Generic class secondary ctor (the `set.fs:23` `new(k) = SetTree(k, 1)`
            // shape): the chain target is a `MemberRef` on the open self-`TypeSpec`.
            // The secondary forwards its `'a` param to the primary's first field and a
            // constant `1` to the second, so `Box(7).V = 7` and `Box(7).N = 1` prove
            // both the generic chain ran and that the non-first field round-trips
            // (vesper-set-sprint-plan §1.11 / B-1 fix — the ctor `stfld` now routes
            // through the open self-`TypeSpec` `MemberRef`).
            test "a generic class secondary ctor chains through the open self-TypeSpec (Box(7).V = 7, .N = 1)" {
                let _, artifact =
                    compileSource
                        "ClsSecCtorGen"
                        (String.concat
                            "\n"
                            [
                                "type Box<'a>(v: 'a, n: int) ="
                                "    new(v: 'a) = Box(v, 1)"
                                "    member this.V = v"
                                "    member this.N = n"
                                "let b = Box(0, 0)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let boxInt = (asm.GetType "Box`1").MakeGenericType typeof<int>

                let ctors = boxInt.GetConstructors(BindingFlags.Public ||| BindingFlags.Instance)
                Expect.equal ctors.Length 2 "Box<int> has the primary (2-arg) + secondary (1-arg) ctors"

                let arities = ctors |> Array.map (fun c -> c.GetParameters().Length) |> Set.ofArray
                Expect.equal arities (Set.ofList [ 1; 2 ]) "the secondary (1-arg) and primary (2-arg) ctors"

                let getV = boxInt.GetMethod("get_V", declaredInstance, null, [||], null)
                let getN = boxInt.GetMethod("get_N", declaredInstance, null, [||], null)
                let instance = Activator.CreateInstance(boxInt, [| box 7 |])
                Expect.equal (getV.Invoke(instance, [||]) :?> int) 7 "Box(7) chains to Box(7, 1): V = 7"

                Expect.equal
                    (getN.Invoke(instance, [||]) :?> int)
                    1
                    "Box(7) chains to Box(7, 1): N = 1 (non-first field)"
            }
        ]

[<Tests>]
let typeAppTests =
    let publicInstance = BindingFlags.Public ||| BindingFlags.Instance

    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    // Explicit type application at a construction site — `Box<int>(v)` /
    // `Holder<'T>(v)` — was an `infer: TODO TypeApp` front-end gap. `set.fs`'s
    // `Set<'T>(...)` calls (a Phase 9 prerequisite, not inheritance) need it.
    // The type args unify against the head's nominal result, and `tryClassRef`
    // peels the `Expr.TypeApp` so the call still lowers to `TExpr.New`.
    testList
        "ClassTypeApp"
        [
            test "module-level explicit type-app construction (Box<int>(5)) round-trips" {
                let _, artifact =
                    compileSource
                        "TyAppMod"
                        (String.concat "\n" [ "type Box<'a>(v: 'a) ="; "    member this.V = v"; "let b = Box<int>(5)" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let boxInt = (asm.GetType "Box`1").MakeGenericType typeof<int>
                let instance = Activator.CreateInstance(boxInt, [| box 5 |])
                let getV = boxInt.GetMethod("get_V", declaredInstance, null, [||], null)
                Expect.equal (getV.Invoke(instance, [||]) :?> int) 5 "Box<int>(5).V = 5"
            }

            // The `set.fs` shape: explicit `<'T>` at a construction site inside a
            // member where `'T` is the enclosing type's typar (in scope).
            test "explicit type-app construction at the declaring typar inside a member (Holder<'T>(v)) round-trips" {
                let _, artifact =
                    compileSource
                        "TyAppMember"
                        (String.concat
                            "\n"
                            [
                                "type Holder<'T>(v: 'T) ="
                                "    member this.V = v"
                                "    member this.Rebuild () = Holder<'T>(v)"
                                "let h = Holder(0)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let holderInt = (asm.GetType "Holder`1").MakeGenericType typeof<int>
                let instance = Activator.CreateInstance(holderInt, [| box 7 |])

                let rebuild = holderInt.GetMethod("Rebuild", declaredInstance, null, [||], null)
                Expect.isNotNull rebuild "Rebuild emitted"
                let rebuilt = rebuild.Invoke(instance, [||])

                Expect.equal
                    (rebuilt.GetType())
                    holderInt
                    "Rebuild () returns a Holder<int> (the explicit <'T> instantiation)"

                let getV = holderInt.GetMethod("get_V", publicInstance, null, [||], null)

                Expect.equal
                    (getV.Invoke(rebuilt, [||]) :?> int)
                    7
                    "Holder<'T>(v).V threads the field through the rebuilt instance"
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

            // vesper-set-sprint-plan §1.11 / B-1 bug fix: a *generic* class with
            // more than one instance field must round-trip *every* field, not just
            // the first. The single-field `Box<'a>(v: 'a)` tests above never
            // exercised a field at index >= 1, so the generic non-first-field gap
            // (ctor `stfld` / member-body `ldfld` resolving the wrong slot at
            // runtime for a generic type) stayed latent. `SetTree<'T>(k, h)` reads
            // its second ctor param `h` for AVL height, so this must work before
            // any `Vesper.Set` runtime test is trusted.
            test "Box<int>(7, 3).N returns 3 — a generic class round-trips its non-first field" {
                let _, artifact =
                    compileSource
                        "ClsGenTwoField"
                        (String.concat
                            "\n"
                            [
                                "type Box<'a>(v: 'a, n: int) ="
                                "    member this.V = v"
                                "    member this.N = n"
                                "let b = Box(0, 0)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let boxInt = (asm.GetType "Box`1").MakeGenericType typeof<int>

                let getV = boxInt.GetMethod("get_V", declaredInstance, null, [||], null)
                let getN = boxInt.GetMethod("get_N", declaredInstance, null, [||], null)
                let instance = Activator.CreateInstance(boxInt, [| box 7; box 3 |])

                Expect.equal (getV.Invoke(instance, [||]) :?> int) 7 "Box<int>(7, 3).V = 7 (first field)"
                Expect.equal (getN.Invoke(instance, [||]) :?> int) 3 "Box<int>(7, 3).N = 3 (non-first field)"
            }
        ]

[<Tests>]
let genericMethodTests =
    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    // vesper-set-sprint-plan §1.10 / B-12 test gate: a member that introduces
    // its *own* generic parameter (`member this.Map<'b> …`). The method's typar
    // emits as a `GenericParam` row owned by the `MethodDef` (encoded `!!i` in
    // its signature), distinct from the declaring type's typars (`!i`). The
    // round-trips reflect over the emitted PE, `MakeGenericMethod` the open
    // method, and invoke it — a wrong typar index or missing `GenericParam` row
    // surfaces as `BadImageFormatException` / `InvalidProgram` at load/invoke.
    testList
        "ClassGenericMethod"
        [
            test "a generic method on a monomorphic class round-trips at two instantiations" {
                let _, artifact =
                    compileSource
                        "ClsGenMethMono"
                        (String.concat "\n" [ "type C() ="; "    member this.Id<'b> (x: 'b) = x"; "let c = C()" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "C"

                let idMethod = ty.GetMethods() |> Array.find (fun m -> m.Name = "Id")
                Expect.isTrue idMethod.IsGenericMethodDefinition "Id is a generic method definition"
                Expect.equal (idMethod.GetGenericArguments().Length) 1 "Id owns exactly one generic parameter ('b)"

                let instance = Activator.CreateInstance(ty, [||])

                let idInt = idMethod.MakeGenericMethod typeof<int>
                Expect.equal (idInt.Invoke(instance, [| box 5 |]) :?> int) 5 "C().Id<int>(5) = 5"

                let idStr = idMethod.MakeGenericMethod typeof<string>
                Expect.equal (idStr.Invoke(instance, [| box "a" |]) :?> string) "a" "C().Id<string>(\"a\") = \"a\""
            }

            test "Box<int>(0).Echo<string>(\"hi\") = \"hi\" — method typar rides param + return" {
                let _, artifact =
                    compileSource
                        "ClsGenMethEcho"
                        (String.concat
                            "\n"
                            [
                                "type Box<'a>(v: 'a) ="
                                "    member this.Echo<'b> (x: 'b) = x"
                                "let b = Box(0)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let boxInt = (asm.GetType "Box`1").MakeGenericType typeof<int>

                let echo = boxInt.GetMethods() |> Array.find (fun m -> m.Name = "Echo")
                Expect.isTrue echo.IsGenericMethodDefinition "Echo is a generic method definition"
                Expect.equal (echo.GetGenericArguments().Length) 1 "Echo owns one method typar ('b)"

                let echoStr = echo.MakeGenericMethod typeof<string>
                let instance = Activator.CreateInstance(boxInt, [| box 0 |])

                Expect.equal
                    (echoStr.Invoke(instance, [| box "hi" |]) :?> string)
                    "hi"
                    "Box<int>(0).Echo<string>(\"hi\") = \"hi\""
            }

            // The decisive B-12 case: the same signature mixes the *method* typar
            // (the `'b` param, `!!0`) and the *type* typar (the `'a` return, `!0`).
            // A swapped index would either fail to load or return the wrong slot.
            test "a generic method's own typar (!!0) and its class's typar (!0) stay distinct" {
                let _, artifact =
                    compileSource
                        "ClsGenMethMixed"
                        (String.concat
                            "\n"
                            [
                                "type Box<'a>(v: 'a) ="
                                "    member this.First<'b> (x: 'b) = v"
                                "let b = Box(0)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let boxTy = asm.GetType "Box`1"
                Expect.equal (boxTy.GetGenericArguments().Length) 1 "Box`1 owns one type typar ('a)"

                let boxInt = boxTy.MakeGenericType typeof<int>
                let first = boxInt.GetMethods() |> Array.find (fun m -> m.Name = "First")
                Expect.isTrue first.IsGenericMethodDefinition "First is a generic method definition"
                Expect.equal (first.GetGenericArguments().Length) 1 "First owns one method typar ('b), separate from 'a"

                let firstStr = first.MakeGenericMethod typeof<string>
                let instance = Activator.CreateInstance(boxInt, [| box 7 |])

                Expect.equal
                    (firstStr.Invoke(instance, [| box "ignored" |]) :?> int)
                    7
                    "Box<int>(7).First<string>(_) returns the 'a-typed field v = 7"
            }
        ]

[<Tests>]
let castTests =
    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    // vesper-set-sprint Phase 2 / B-4 Step 2.4 backend gate: `:?` and `:?>`
    // emit `isinst` / `castclass` against a `TypeToken` for the target type.
    // `obj`-subsumption isn't wired in v1, so these exercise the cast IL on a
    // same-type cast on `this` (`this :? C` / `this :?> C`), which still emits
    // the real `isinst` / `castclass` and runs them against a live instance.
    // (Base→derived construction now round-trips — see `ClassInheritance`.)
    testList
        "ClassCast"
        [
            test "`this :? C` emits isinst and returns true for the instance itself" {
                let _, artifact =
                    compileSource
                        "ClsTypeTest"
                        (String.concat "\n" [ "type C() ="; "    member this.IsC () = this :? C"; "let c = C()" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "C"
                let isC = ty.GetMethod("IsC", declaredInstance, null, [||], null)
                Expect.isNotNull isC "IsC emitted"

                let instance = Activator.CreateInstance(ty, [||])
                Expect.isTrue (isC.Invoke(instance, [||]) :?> bool) "(c :? C) is true"
            }

            test "`(this :?> C).M ()` emits castclass then dispatches the member" {
                let _, artifact =
                    compileSource
                        "ClsDowncast"
                        (String.concat
                            "\n"
                            [
                                "type C() ="
                                "    member this.M () = 42"
                                "    member this.AsC () = (this :?> C).M ()"
                                "let c = C()"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "C"
                let asC = ty.GetMethod("AsC", declaredInstance, null, [||], null)
                Expect.isNotNull asC "AsC emitted"

                let instance = Activator.CreateInstance(ty, [||])
                Expect.equal (asC.Invoke(instance, [||]) :?> int) 42 "(c :?> C).M() = 42"
            }
        ]

[<Tests>]
let inheritanceTests =
    let publicInstance = BindingFlags.Public ||| BindingFlags.Instance

    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    // vesper-set-sprint Phase 2 / B-4 Step 2.5 backend gate: `inherit Base(args)`
    // wires the IL `TypeDefinition.BaseType` to the parent and the primary `.ctor`
    // to chain `ldarg.0; <args>; call Base::.ctor` before storing the derived
    // fields. Construction is itself the proof the chain is sound — a `.ctor` that
    // never calls a base / sibling ctor fails PE verification — and an inherited
    // member read confirms the base ctor stored its arg.
    testList
        "ClassInheritance"
        [
            test "a derived class's IL base type is its declared parent" {
                let _, artifact =
                    compileSource
                        "InhBaseType"
                        (String.concat
                            "\n"
                            [
                                "type Shape(x: int) ="
                                "    member this.Raw = x"
                                "type Circle(r: int, t: int) ="
                                "    inherit Shape(t)"
                                "    member this.Radius = r"
                                "let c = Circle(0, 0)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let circle = asm.GetType "Circle"
                let shape = asm.GetType "Shape"
                Expect.isNotNull circle "Circle emitted"
                Expect.equal circle.BaseType shape "Circle's IL base type is Shape, not Object"
            }

            test "constructing a derived instance chains to the base ctor (inherited member reads the base-ctor arg)" {
                let _, artifact =
                    compileSource
                        "InhBaseCtor"
                        (String.concat
                            "\n"
                            [
                                "type Shape(x: int) ="
                                "    member this.Raw = x"
                                "type Circle(r: int, t: int) ="
                                "    inherit Shape(t)"
                                "    member this.Radius = r"
                                "let c = Circle(0, 0)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let circle = asm.GetType "Circle"

                // Construction succeeds ⇒ the primary ctor chained to Shape::.ctor.
                let instance = Activator.CreateInstance(circle, [| box 5; box 9 |])

                // `Raw` is inherited from Shape; reading it proves `inherit Shape(t)`
                // passed `t = 9` to the base ctor (which stored it in Shape's field).
                let getRaw = circle.GetMethod("get_Raw", publicInstance, null, [||], null)
                Expect.isNotNull getRaw "get_Raw reachable through the inheritance chain"
                Expect.equal (getRaw.Invoke(instance, [||]) :?> int) 9 "inherited Raw returns the base-ctor arg t = 9"

                // `Radius` is declared on Circle and reads Circle's own field.
                let getRadius = circle.GetMethod("get_Radius", declaredInstance, null, [||], null)
                Expect.equal (getRadius.Invoke(instance, [||]) :?> int) 5 "Circle.Radius reads its own field r = 5"
            }

            test "a two-level chain (Loud : Shape : Object) constructs and the override is selected on the derived type" {
                let _, artifact =
                    compileSource
                        "InhOverride"
                        (String.concat
                            "\n"
                            [
                                "type Shape(x: int) ="
                                "    member this.Raw = x"
                                "    member this.Describe () = x"
                                "type Loud(n: int) ="
                                "    inherit Shape(n)"
                                "    override this.Describe () = 99"
                                "let l = Loud(0)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let loud = asm.GetType "Loud"
                let shape = asm.GetType "Shape"
                Expect.equal loud.BaseType shape "Loud derives from Shape"

                let instance = Activator.CreateInstance(loud, [| box 7 |])

                // The base ctor ran: inherited Raw reads the value `inherit Shape(n)`
                // forwarded.
                let getRaw = loud.GetMethod("get_Raw", publicInstance, null, [||], null)
                Expect.equal (getRaw.Invoke(instance, [||]) :?> int) 7 "inherited Raw = 7 (base ctor chained with n)"

                // Loud's own `Describe` override is the one declared on Loud.
                let describe = loud.GetMethod("Describe", declaredInstance, null, [||], null)
                Expect.isNotNull describe "Loud declares its own Describe override"
                Expect.equal (describe.Invoke(instance, [||]) :?> int) 99 "Loud.Describe () returns the override's 99"
            }

            // The Phase 2 exit condition's `set.fs` shape is a generic class deriving
            // from a generic-base *instantiation*: the base type is a `GENERICINST`
            // `TypeSpec` and the base `.ctor` a `MemberRef` on it. Here a monomorphic
            // class inherits an instantiated generic base (`IntBox : Box<int>`).
            test
                "a mono class inheriting an instantiated generic base (Box<int>) constructs and reads through the chain" {
                let _, artifact =
                    compileSource
                        "InhGenericBase"
                        (String.concat
                            "\n"
                            [
                                "type Box<'a>(v: 'a) ="
                                "    member this.V = v"
                                "type IntBox(n: int) ="
                                "    inherit Box<int>(n)"
                                "    member this.Twice = n + n"
                                "let b = IntBox(0)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let intBox = asm.GetType "IntBox"
                Expect.isNotNull intBox "IntBox emitted"

                Expect.equal
                    (intBox.BaseType.GetGenericTypeDefinition())
                    (asm.GetType "Box`1")
                    "IntBox derives from Box<_>"

                let instance = Activator.CreateInstance(intBox, [| box 21 |])
                let getV = intBox.GetMethod("get_V", publicInstance, null, [||], null)

                Expect.equal
                    (getV.Invoke(instance, [||]) :?> int)
                    21
                    "inherited Box<int>.V returns the base-ctor arg 21"
            }

            // The fully generic `SetTree<'T>` / `SetTreeNode<'T>` pair from `set.fs`:
            // a generic class inheriting a generic base instantiated at its *own*
            // typar (`inherit SetTree<'T>(h)`), so the base `TypeSpec` carries `!0`.
            test
                "a generic class inheriting a generic base at its own typar (SetTreeNode<'T> : SetTree<'T>) round-trips" {
                let _, artifact =
                    compileSource
                        "InhGenericChain"
                        (String.concat
                            "\n"
                            [
                                "type SetTree<'T>(h: int) ="
                                "    member this.Height = h"
                                "type SetTreeNode<'T>(v: 'T, h: int) ="
                                "    inherit SetTree<'T>(h)"
                                "    member this.Value = v"
                                "let n = SetTreeNode(0, 0)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let node = (asm.GetType "SetTreeNode`1").MakeGenericType(typeof<int>)
                let instance = Activator.CreateInstance(node, [| box 42; box 3 |])

                let getHeight = node.GetMethod("get_Height", publicInstance, null, [||], null)

                Expect.equal
                    (getHeight.Invoke(instance, [||]) :?> int)
                    3
                    "inherited SetTree<'T>.Height returns base-ctor arg 3"

                let getValue = node.GetMethod("get_Value", declaredInstance, null, [||], null)

                Expect.equal
                    (getValue.Invoke(instance, [||]) :?> int)
                    42
                    "SetTreeNode<int>.Value returns its own field 42"
            }

            // Step 2.6: `base.M(...)` must dispatch non-virtually (`call B::M`),
            // not virtually (`callvirt`) — otherwise an `override` body calling
            // `base.M()` re-enters itself and stack-overflows. Parent returns 1;
            // the override adds 1, so calling `M` on a derived instance returns 2
            // (and *returns* at all, proving it didn't recurse infinitely).
            test "`base.M()` in an override calls the parent's method, not itself (non-virtual dispatch)" {
                let _, artifact =
                    compileSource
                        "InhBaseCall"
                        (String.concat
                            "\n"
                            [
                                "type Base() ="
                                "    member this.M () = 1"
                                "type Derived() ="
                                "    inherit Base()"
                                "    override this.M () = base.M() + 1"
                                "let d = Derived()"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let derived = asm.GetType "Derived"
                let instance = Activator.CreateInstance(derived, [||])

                // `M` is the override declared on Derived; invoking it through the
                // virtual slot runs `base.M() + 1` = 1 + 1 = 2.
                let m = derived.GetMethod("M", declaredInstance, null, [||], null)
                Expect.isNotNull m "Derived declares its own M override"
                Expect.equal (m.Invoke(instance, [||]) :?> int) 2 "base.M() (= 1) + 1 = 2; no infinite recursion"
            }
        ]

[<Tests>]
let interfaceImplTests =
    // vesper-set-sprint-phase-5 §5.1 test gate: a user class implementing an
    // external BCL interface front-end-resolves the interface and its member
    // body without diagnostic. Front-end only — the interface type resolves
    // against the metadata provider in the default contract stack, the impl is
    // registered on the class's `ClassTypeInfo`, and the member body type-checks.
    // The TAST shape + codegen emit (`.override` rows, `TypeDefinition.Interfaces`)
    // are Step 5.3, deferred.
    testList
        "ClassInterfaceImpl"
        [
            test "a class implementing System.IComparable resolves the interface + CompareTo without diagnostic" {
                let provider, _ = SymbolProviders.buildContract defaultManifests

                let src =
                    String.concat
                        "\n"
                        [
                            "type C() ="
                            "    interface System.IComparable with"
                            "        member this.CompareTo(o: obj) = 0"
                        ]

                let lexed, file = parseFile src
                let ctx, tast = Pipeline.analyseWithContext provider src lexed file

                let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)
                Expect.isEmpty errors (sprintf "no front-end errors (%A)" errors)

                match ctx.Types.Class.TryGetValue "C" with
                | true, info ->
                    Expect.equal info.InterfaceImpls.Length 1 "one interface impl registered on C"
                    let impl = info.InterfaceImpls.[0]

                    match impl.Resolved with
                    | ValueSome(TyClass(name, _)) ->
                        Expect.stringContains name "IComparable" "the impl resolved to the IComparable interface"
                    | other -> failtestf "interface impl did not resolve to an interface TyClass: %A" other

                    Expect.equal impl.Members.Length 1 "the CompareTo member is registered on the impl"
                | false, _ -> failtest "class C was not registered"
            }

            test "implementing a non-interface type is rejected with a diagnostic" {
                let provider, _ = SymbolProviders.buildContract defaultManifests

                let src =
                    String.concat
                        "\n"
                        [
                            "type Base() ="
                            "    member this.M () = 1"
                            "type C() ="
                            "    interface Base with"
                            "        member this.M () = 2"
                        ]

                let lexed, file = parseFile src
                let _, tast = Pipeline.analyseWithContext provider src lexed file

                let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

                Expect.isNonEmpty errors "implementing a concrete class as an interface diagnoses"

                Expect.isTrue
                    (errors |> List.exists (fun d -> d.Message.Contains "not an interface"))
                    "the diagnostic explains the target is not an interface"
            }

            // vesper-set-sprint-phase-5 §5.2 test gate: a user class implementing
            // the generic `IEnumerable<int>` and the non-generic `IEnumerable`
            // type-checks both `GetEnumerator` methods independently. Each
            // `interface … with` block resolves *its own* declared `GetEnumerator`
            // overload (the metadata walk is `DeclaredOnly`), so the generic one
            // conforms to `unit -> IEnumerator<int>` and the non-generic one to
            // `unit -> IEnumerator` with no cross-talk. Bodies are `failwith` so the
            // front-end test needs no concrete enumerator — only the signatures matter.
            test "implementing IEnumerable<int> and IEnumerable conforms both GetEnumerator methods" {
                let provider, _ = SymbolProviders.buildContract defaultManifests

                let src =
                    String.concat
                        "\n"
                        [
                            "type C() ="
                            "    interface System.Collections.Generic.IEnumerable<int> with"
                            "        member this.GetEnumerator() : System.Collections.Generic.IEnumerator<int> = failwith \"x\""
                            "    interface System.Collections.IEnumerable with"
                            "        member this.GetEnumerator() : System.Collections.IEnumerator = failwith \"x\""
                        ]

                let lexed, file = parseFile src
                let ctx, tast = Pipeline.analyseWithContext provider src lexed file

                let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)
                Expect.isEmpty errors (sprintf "no front-end errors (%A)" errors)

                match ctx.Types.Class.TryGetValue "C" with
                | true, info ->
                    Expect.equal info.InterfaceImpls.Length 2 "two interface impls registered on C"

                    Expect.isTrue
                        (info.InterfaceImpls
                         |> Array.forall (fun impl ->
                             match impl.Resolved with
                             | ValueSome(TyClass _) -> true
                             | _ -> false
                         ))
                        "both interface impls resolved to an interface TyClass"
                | false, _ -> failtest "class C was not registered"
            }

            // §5.2 step 2: argument + return types must match the interface
            // signature. `CompareTo` returning a `string` where `IComparable`
            // promises an `int` is a conformance failure.
            test "a member whose signature does not match the interface is diagnosed" {
                let provider, _ = SymbolProviders.buildContract defaultManifests

                let src =
                    String.concat
                        "\n"
                        [
                            "type C() ="
                            "    interface System.IComparable with"
                            "        member this.CompareTo(o: obj) = \"wrong\""
                        ]

                let lexed, file = parseFile src
                let _, tast = Pipeline.analyseWithContext provider src lexed file

                let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

                Expect.isNonEmpty errors "a return-type mismatch against the interface diagnoses"

                Expect.isTrue
                    (errors |> List.exists (fun d -> d.Message.Contains "mismatch"))
                    "the diagnostic reports a type mismatch"
            }

            // §5.2 step 1 + 3: a member the interface does not declare is rejected,
            // and the required-but-unimplemented member is reported missing.
            test "a wrongly-named member is rejected and the required member reported missing" {
                let provider, _ = SymbolProviders.buildContract defaultManifests

                let src =
                    String.concat
                        "\n"
                        [
                            "type C() ="
                            "    interface System.IComparable with"
                            "        member this.Nope(o: obj) = 0"
                        ]

                let lexed, file = parseFile src
                let _, tast = Pipeline.analyseWithContext provider src lexed file

                let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

                Expect.isTrue
                    (errors |> List.exists (fun d -> d.Message.Contains "does not define a member"))
                    "the unknown member 'Nope' is rejected"

                Expect.isTrue
                    (errors
                     |> List.exists (fun d ->
                         d.Message.Contains "No implementation given" && d.Message.Contains "CompareTo"
                     ))
                    "the required 'CompareTo' is reported missing"
            }
        ]
