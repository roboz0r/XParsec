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

            // A tupled member `M(a, b)` is a single tuple argument pattern;
            // `Elaborate.memberParams` must flatten it to one parameter per component
            // (F# compiles it to a method with N scalar params, not a `Tuple<_,_>`).
            // A missing flatten dropped the bindings and codegen threw "no binding for variable".
            test "a two-parameter instance member binds both args (Add(3,4) returns 7)" {
                let _, artifact =
                    compileSource
                        "ClsAdd2"
                        (String.concat
                            "\n"
                            [
                                "type T() ="
                                "    member this.Add(a: int, b: int) : int = a + b"
                                "let t = T()"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "T"

                let m =
                    ty.GetMethod("Add", declaredInstance, null, [| typeof<int>; typeof<int> |], null)

                Expect.isNotNull m "Add emitted as an instance method taking two int params"
                Expect.equal (m.GetParameters().Length) 2 "Add has two scalar parameters (tuple flattened)"

                let instance = Activator.CreateInstance(ty, [||])
                Expect.equal (m.Invoke(instance, [| box 3; box 4 |]) :?> int) 7 "T().Add(3, 4) returns 7"
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

            // vesper-set-g-wall.md G14: F# lets each member name its own
            // self-identifier, independently of the type-level `as` alias.
            // `Set<'T>` (no `as` clause) spells its members `member s.Add`,
            // `member x.Choose`, … — before the fix only the default `this`
            // was bound, so every `s` / `x` receiver (and `s.Member` access)
            // went unresolved. Here `First` (self-id `a`) reads `Second`
            // (self-id `b`) through its own self-id; both must resolve and the
            // qualified `a.Second` must round-trip.
            test "two instance members with distinct self-ids resolve (First reads a.Second)" {
                let _, artifact =
                    compileSource
                        "ClsSelfIds"
                        (String.concat
                            "\n"
                            [
                                "type C() ="
                                "    member a.First = a.Second + 1"
                                "    member b.Second = 10"
                                "let c = C()"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "C"

                let getFirst = ty.GetMethod("get_First", declaredInstance, null, [||], null)
                let getSecond = ty.GetMethod("get_Second", declaredInstance, null, [||], null)
                Expect.isNotNull getFirst "get_First emitted (self-id `a` bound)"
                Expect.isNotNull getSecond "get_Second emitted (self-id `b` bound)"

                let instance = Activator.CreateInstance(ty, [||])
                Expect.equal (getSecond.Invoke(instance, [||]) :?> int) 10 "C().Second = 10"
                Expect.equal (getFirst.Invoke(instance, [||]) :?> int) 11 "C().First reads a.Second + 1 = 11"
            }
        ]

[<Tests>]
let staticTests =
    let declaredStatic =
        BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly

    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

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

            // Same tuple-flatten requirement on the static path:
            // `static member M(a, b)` parses as one tuple arg pattern.
            test "a two-parameter static member binds both args (M(3,4) returns 7)" {
                let _, artifact =
                    compileSource
                        "ClsStaticAdd2"
                        (String.concat "\n" [ "type T ="; "    static member M(a: int, b: int) : int = a + b" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "T"

                let m =
                    ty.GetMethod("M", declaredStatic, null, [| typeof<int>; typeof<int> |], null)

                Expect.isNotNull m "M emitted as a static method taking two int params"
                Expect.isTrue m.IsStatic "M is static"
                Expect.equal (m.GetParameters().Length) 2 "M has two scalar parameters (tuple flattened)"
                Expect.equal (m.Invoke(null, [| box 3; box 4 |]) :?> int) 7 "T.M(3, 4) returns 7"
            }

            // module-representation-plan: a module-level value (`let x = e` at
            // module scope) is a `public static` field on its module holder,
            // initialised by the holder's `.cctor`, read everywhere as `ldsfld` —
            // never a `Main` local or a closure capture. These rows pin the four
            // reference contexts that the `set.fs` Phase-9 wall hit (none of which
            // worked before: the module value had no storage, and a Library has no
            // `Main`). The field lands on the named-module holder (`Helper`).

            // (1) An instance member body reads a module value.
            test "an instance member reads a module-level value via a static field (Get() = 42)" {
                let _, artifact =
                    compileSource
                        "ModuleValMember"
                        (String.concat
                            "\n"
                            [
                                "module Helper ="
                                "    let seed : int = 42"
                                ""
                                "type Box(v: int) ="
                                "    member this.Get () = Helper.seed"
                                "let b = Box(0)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let helper = asm.GetType "Helper"
                Expect.isNotNull helper "the named-module holder Helper is emitted"

                let seedField = helper.GetField("seed", BindingFlags.Public ||| BindingFlags.Static)
                Expect.isNotNull seedField "the module value `seed` is a public static field on Helper"

                let boxTy = asm.GetType "Box"
                let inst = boxTy.GetConstructors().[0].Invoke [| box 0 |]
                let m = boxTy.GetMethod("Get", declaredInstance, null, [||], null)
                Expect.equal (m.Invoke(inst, [||]) :?> int) 42 "Box().Get() reads the module value seed = 42"
            }

            // (2) A *generic* class `static let` initialiser references a module
            //     value (the `set.fs` `static let empty = … SetTree.empty` shape:
            //     a module value read inside a generic class's `.cctor`).
            test "a generic class `static let` reads a module value through the cctor (SeedV() = 42)" {
                let _, artifact =
                    compileSource
                        "ModuleValGenericStaticLet"
                        (String.concat
                            "\n"
                            [
                                "module Helper ="
                                "    let seed : int = 42"
                                ""
                                "type Box<'T>(v: int) ="
                                "    static let s : int = Helper.seed"
                                "    member this.V = v"
                                "    static member SeedV () = s"
                                "let b = Box<int>(0)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let boxTy = (asm.GetType "Box`1").MakeGenericType typeof<int>
                let m = boxTy.GetMethod("SeedV", declaredStatic, null, [||], null)
                Expect.isNotNull m "SeedV emitted as a static method"

                Expect.equal
                    (m.Invoke(null, [||]) :?> int)
                    42
                    "Box<int>.SeedV() reads the static-let `s`, built in the cctor from the module value seed = 42"
            }

            // (3) A sibling module function reading a module value emits as a real
            //     static method (a direct `ldsfld`), not a closure capturing it.
            test "a module function reading a module value emits as a static method (get() = 42)" {
                let _, artifact =
                    compileSource
                        "ModuleValFn"
                        (String.concat
                            "\n"
                            [ "module Helper ="; "    let seed : int = 42"; "    let get () : int = seed" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let helper = asm.GetType "Helper"
                let m = helper.GetMethod("get", declaredStatic)
                Expect.isNotNull m "get emitted as a static method on Helper (not a closure capturing seed)"
                Expect.isTrue m.IsStatic "get is a static method"
                Expect.equal (m.Invoke(null, [| () |]) :?> int) 42 "Helper.get () reads seed = 42"
            }

            // (4) A module value initialised from an *earlier* module value — the
            //     cctor evaluates initialisers in declaration order.
            test "a module value initialised from an earlier module value (cctor order: b = 2)" {
                let _, artifact =
                    compileSource
                        "ModuleValChain"
                        (String.concat "\n" [ "module Helper ="; "    let a : int = 1"; "    let b : int = a + 1" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let helper = asm.GetType "Helper"
                let bField = helper.GetField("b", BindingFlags.Public ||| BindingFlags.Static)
                Expect.equal (bField.GetValue null :?> int) 2 "b = a + 1 = 2, the cctor ran a's init first"
            }

            // (5) A module value's initialiser must resolve entirely to other
            //     module values / static methods inside the holder `.cctor`. The
            //     bare `seed = f` reference makes `f` escape as a value, demoting
            //     it to a closure held in a `Main` local — which a `.cctor`
            //     cannot see — so the assembler fails with a targeted message
            //     rather than the generic `buildVarLoad` "no binding" crash.
            test "a module value whose init needs a Main local fails with a targeted error" {
                let msg =
                    try
                        compileSource
                            "ModuleValUnresolvable"
                            (String.concat
                                "\n"
                                [
                                    "module Helper ="
                                    "    let f = fun (x: int) -> x + 1"
                                    "    let seed : int -> int = f"
                                ])
                        |> ignore

                        ""
                    with e ->
                        e.Message

                Expect.stringContains msg "module value 'seed'" "the failure names the module value"

                Expect.stringContains
                    msg
                    "neither a module value nor a static method"
                    "the failure states the resolution rule"
            }

            // (6) A *generic* module value (`let empty: Node<'T> = null`) cannot be a
            //     static *field* — a non-generic module holder has no type parameter to
            //     type it — so it lowers to a zero-arg *generic static method* on its
            //     holder; every reference `call`s its `MethodSpec`, the instantiation
            //     recovered from the reference's own type (module-representation-plan
            //     §generic-values). This is the exact `set.fs` `SetTree.empty` shape:
            //     a generic class's `static let` cctor reads a generic module value.
            test "a generic module value lowers to a generic method read across contexts (null)" {
                let _, artifact =
                    compileSource
                        "GenericModuleVal"
                        (String.concat
                            "\n"
                            [
                                "[<AllowNullLiteral>]"
                                "type Node<'T>(v: 'T) ="
                                "    member _.V = v"
                                ""
                                "module Tree ="
                                "    let empty : Node<'T> = null"
                                "    let getEmpty () : Node<'T> = empty"
                                ""
                                "type Box<'T>() ="
                                "    static let s : Node<'T> = Tree.empty"
                                "    static member S () : Node<'T> = s"
                                "    member _.Direct () : Node<'T> = Tree.empty"
                                "let b = Box<int>()"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)

                // `empty` is a zero-arg generic method on the `Tree` holder, NOT a field.
                let tree = asm.GetType "Tree"
                Expect.isNotNull tree "the Tree holder is emitted"

                Expect.isNull
                    (tree.GetField("empty", BindingFlags.Public ||| BindingFlags.Static))
                    "a generic module value is a method, not a static field"

                let emptyM = tree.GetMethod("empty", declaredStatic)
                Expect.isNotNull emptyM "empty emitted as a static method"
                Expect.isTrue emptyM.IsGenericMethodDefinition "empty is a generic method (one method typar)"

                // Each reference instantiates it at the use-site type.
                let emptyInt = emptyM.MakeGenericMethod typeof<int>
                Expect.isNull (emptyInt.Invoke(null, [||])) "Tree.empty<int>() = null"

                // A sibling module fn reads it (a `call`, not a closure capture).
                let getEmpty = tree.GetMethod("getEmpty", declaredStatic)

                Expect.isNull
                    (getEmpty.MakeGenericMethod(typeof<int>).Invoke(null, [| () |]))
                    "Tree.getEmpty<int>() = null"

                // The generic-class `static let` cctor reads it (the `set.fs` shape).
                let boxInt = (asm.GetType "Box`1").MakeGenericType typeof<int>
                let s = boxInt.GetMethod("S", declaredStatic, null, [||], null)
                Expect.isNull (s.Invoke(null, [||])) "Box<int>.S() reads the static-let built from Tree.empty = null"

                // A direct instance-member read.
                let inst = boxInt.GetConstructors().[0].Invoke [||]
                let direct = boxInt.GetMethod("Direct", declaredInstance, null, [||], null)
                Expect.isNull (direct.Invoke(inst, [||])) "Box<int>().Direct() = Tree.empty<int> = null"
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

            // G13 (vesper-set-g-wall): `static let` on a *generic* class. The field
            // lives on the open generic `TypeDefinition` (one instance per closed
            // instantiation, `.cctor`-initialised); its `.cctor` store and the
            // member-body read both mint a `MemberRef` on the self-`TypeSpec`
            // (`Box\`1<!0>::tag`), not a raw `Def` token. Read it back at two
            // instantiations to prove the per-instantiation field resolves.
            // (`set.fs`'s `static let empty` cache shape.)
            test "a generic class `static let` reads back at two instantiations (Box<int>/Box<string>.Tag() = 99)" {
                let _, artifact =
                    compileSource
                        "ClsStaticLetGeneric"
                        (String.concat
                            "\n"
                            [
                                "type Box<'T>(v: 'T) ="
                                "    static let tag = 99"
                                "    member this.V = v"
                                "    member this.Tag () = tag"
                                "let b = Box(0)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let boxTy = asm.GetType "Box`1"
                Expect.isNotNull boxTy "the assembly contains the generic class Box`1"

                let readTag (argTy: Type) (ctorArg: obj) =
                    let inst = boxTy.MakeGenericType argTy
                    let ctor = inst.GetConstructors().[0]
                    let value = ctor.Invoke [| ctorArg |]

                    let m =
                        inst.GetMethod(
                            "Tag",
                            BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly,
                            null,
                            [||],
                            null
                        )

                    Expect.isNotNull m "Tag emitted as an instance method"
                    m.Invoke(value, [||]) :?> int

                Expect.equal (readTag typeof<int> (box 0)) 99 "Box<int>().Tag() reads the static-let field tag = 99"

                Expect.equal
                    (readTag typeof<string> (box "x"))
                    99
                    "Box<string>().Tag() reads its own per-instantiation static-let field tag = 99"
            }

            // G10 (vesper-set-phase-9-handoff): a static *operator* member's body
            // was never inferred. `MemberRegistration.memberNameOf` and
            // `Unification.fillTypeMembers` both only recognised `Pat.NamedSimple`
            // heads, so a `Pat.Op` member got no `TypeMemberInfo` and Unification
            // skipped its body — leaving every application in it a free TyVar that
            // crashed Freeze (`translateApp: expected function type … free TypeVar`).
            // The `(+)` body here contains applications (the `+` on the fields, the
            // `V(...)` ctor) that only freeze once the body is inferred; it now
            // emits as `op_Addition`. (`set.fs`'s `static member (-)`/`(+)` shape.)
            test "a static operator member's body is inferred (op_Addition(V 3, V 4).N = 7)" {
                let _, artifact =
                    compileSource
                        "ClsOpMember"
                        (String.concat
                            "\n"
                            [
                                "type V(n: int) ="
                                "    member this.N = n"
                                "    static member (+) (a: V, b: V) : V = V(a.N + b.N)"
                                "let z = V(0)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "V"
                Expect.isNotNull ty "the assembly contains the class type V"

                let op = ty.GetMethod("op_Addition", declaredStatic, null, [| ty; ty |], null)
                Expect.isNotNull op "the (+) member emitted as a static op_Addition"
                Expect.isTrue op.IsStatic "op_Addition is static"

                // `member this.N` emits a `get_N` method (no PropertyDefinition row
                // yet, P3d.3), so read it through the getter rather than GetProperty.
                let getN = ty.GetMethod("get_N", declaredInstance, null, [||], null)
                Expect.isNotNull getN "get_N emitted"

                let a = Activator.CreateInstance(ty, [| box 3 |])
                let b = Activator.CreateInstance(ty, [| box 4 |])
                let r = op.Invoke(null, [| a; b |])
                let n = getN.Invoke(r, [||]) :?> int
                Expect.equal n 7 "op_Addition(V 3, V 4).N = 7"
            }

            // vesper-set-phase-9-handoff (follow-on to G10): a static member read on
            // an *explicitly* instantiated generic class (`Box<'T>.Make x`,
            // `Box<'T>.Tag`) parses as `DotLookup(TypeApp(Box, <'T>), .Member)`, not
            // the folded `LongIdent[Box; Member]` the bare `Box.Member` form takes.
            // Freeze had no arm for the `TypeApp` receiver and threw at its `TODO
            // TypeApp` catch-all. `Set<'T>.Empty` (property) / `Set<'T>.Singleton x`
            // (method) in set.fs are this shape. Asserted at the TAST level — codegen
            // contract extraction of a generic type's static members is a separate,
            // still-open gap (`?ungrounded-operator`), out of scope for this Freeze
            // arm.
            test "`Box<'T>.Member` lowers to Static{Method,Property} (no Freeze TODO TypeApp)" {
                let provider = SymbolProviders.buildContract defaultManifests

                // `Tag`/`Origin` are `'T`-free so the receiver's `<'T>` is the only
                // explicit instantiation under test; the access sites are instance
                // members (no `'T`-annotated static params — that signature-typar
                // scope is a separate, still-open gap).
                let src =
                    String.concat
                        "\n"
                        [
                            "type Box<'T>(v: 'T) ="
                            "    member this.V = v"
                            "    static member Tag : int = 42"
                            "    static member Origin () : int = 7"
                            "    member this.ReadTag () : int = Box<'T>.Tag"
                            "    member this.ReadOrigin () : int = Box<'T>.Origin ()"
                        ]

                let lexed, file = parseFile src
                let _, tast = Pipeline.analyseSemWithContext provider src lexed file

                let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)
                Expect.isEmpty errors (sprintf "no front-end errors (%A)" errors)

                let mutable staticCalls = 0
                let mutable staticGets = 0

                let it =
                    { TastWalk.identityIter with
                        VisitExpr =
                            fun _ e ->
                                match e with
                                | TExpr.StaticMethodCall _ -> staticCalls <- staticCalls + 1
                                | TExpr.StaticPropertyGet _ -> staticGets <- staticGets + 1
                                | _ -> ()

                                true
                    }

                for d in EqArray.toList tast.Decls do
                    match d with
                    | TDecl.Type td ->
                        match td.Kind with
                        | TTypeKindG.Class(members = members) ->
                            for m in EqArray.toList members do
                                TastWalk.iterExpr it m.Body
                        | _ -> ()
                    | _ -> ()

                Expect.isTrue (staticCalls > 0) "`Box<'T>.Make x` (in Remake) lowered to a TExpr.StaticMethodCall"
                Expect.isTrue (staticGets > 0) "`Box<'T>.Tag` (in MakeTagged) lowered to a TExpr.StaticPropertyGet"
            }

            // Outstanding-2 gap A (vesper-set-phase-9-handoff): a static method on a
            // *generic* class whose RESULT type does not surface the declaring
            // instantiation, called from a concrete (non-declaring) context. The
            // result-type fast path in `resolveStaticMember.instantiationFor` can't
            // recover `'T=int` (the return is `int`), so it now structurally matches
            // the member's open signature against the call's argument types
            // (`RecoverOpenTypars`, declaring axis) to mint `Box\`1<int32>::Describe`.
            // Previously emitted an open `Box\`1<!0>` ref → `BadImageFormatException`.
            test "gapA: a generic class's static method recovers its instantiation from an arg" {
                runs
                    "9"
                    (String.concat
                        "\n"
                        [
                            "type Box<'T>(v: 'T) ="
                            "    member this.V = v"
                            "    static member Describe (x: 'T) : int = 9"
                            "let r = Box<int>.Describe 5"
                            "printfn \"%d\" r"
                        ])
            }

            // Outstanding-2 gap B (vesper-set-phase-9-handoff): a higher-order call
            // (`List.fold`) with a closure argument from inside a member body — the
            // faithful `Set.Union` shape (ClosureTests "a mono own-class
            // static-operator passed as a value" runs the *module-level* form). The
            // own-class `static member (+)` taken by value eta-reifies to
            // `fun a b -> V.op_Addition(a, b)`, a member-body closure; the gap report
            // was that the fold returned its seed `V 0`. Now closed (the member-body
            // closure discovery/capture work landed for Set.map/partition fixed it).
            test "gapB: List.fold over an own-op closure inside a member body" {
                runs
                    "6"
                    (String.concat
                        "\n"
                        [
                            "type V(n: int) ="
                            "    member x.N = n"
                            "    static member (+) (a: V, b: V) = V(a.N + b.N)"
                            "    member this.SumAll (xs: V list) : V = List.fold (+) (V 0) xs"
                            "let xs = [ V 1; V 2; V 3 ]"
                            "let v = V 0"
                            "printfn \"%d\" ((v.SumAll xs).N)"
                        ])
            }

            // The *generic* `Set.Union` shape end-to-end — the run ClosureTests
            // (`a generic own-class static-operator value froze to a Lambda calling
            // op_Addition`) could only pin at the TAST level, because it was blocked
            // by BOTH gap A (static-method call on a generic class from a concrete
            // context) and gap B (closure HOF in a member body). Both now closed, so
            // the generic fold runs green: a generic member-body closure `call`s the
            // class's own `op_Addition`, and the concrete `V<int>` ctor / fold seed
            // resolve their instantiation.
            test "gapA+B: generic own-op List.fold inside a generic member body runs end-to-end" {
                runs
                    "6"
                    (String.concat
                        "\n"
                        [
                            "type V<'T>(n: int) ="
                            "    member x.N = n"
                            "    static member (+) (a: V<'T>, b: V<'T>) = V<'T>(a.N + b.N)"
                            "    member this.SumAll (xs: V<'T> list) : V<'T> = List.fold (+) (V<'T>(0)) xs"
                            "let xs = [ V<int>(1); V<int>(2); V<int>(3) ]"
                            "let v = V<int>(0)"
                            "printfn \"%d\" ((v.SumAll xs).N)"
                        ])
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

            // vesper-set-phase-9 wall: reading an *inherited* member from
            // in-Vesper code (`node.Key` where `Key` is declared on the base
            // `SetTree`, accessed on a `SetTreeNode` receiver). The access is a
            // local-headed LongIdent chain, so Freeze's `fieldStep` fires; before
            // the fix its own-class-only member check fell through to a `FieldGet`,
            // which codegen's record-only `resolveRecordField` rejected with
            // `class 'Derived`1' has no field 'Key'`. The fix walks the `inherit`
            // chain and upcasts the receiver to the declaring ancestor (a ref-type
            // upcast is a codegen no-op), so the receiver-keyed
            // `resolveInstanceMember` resolves `get_Key` on the base. Generic to
            // mirror the `SetTreeNode<'T>` shape.
            test "reading an inherited member on a derived receiver resolves the base property (Key shape)" {
                runs
                    "42"
                    (String.concat
                        "\n"
                        [
                            "type Base<'T>(k: 'T) ="
                            "    member _.Key = k"
                            "type Derived<'T>(k: 'T, h: int) ="
                            "    inherit Base<'T>(k)"
                            "    member _.Height = h"
                            "let readKey (d: Derived<'T>) = d.Key"
                            "printfn \"%d\" (readKey (Derived(42, 1)))"
                        ])
            }

            // The same inherited read *inside a closure body* — the faithful
            // `set.fs` context, where the crash surfaced in `buildClosureInvoke`
            // (a `SetTree.*` closure capturing the node and reading `.Key`). A
            // lambda capturing the derived receiver and reading its inherited
            // member must lower the same way.
            test "an inherited member read captured in a closure resolves the base property" {
                runs
                    "7"
                    (String.concat
                        "\n"
                        [
                            "type Base<'T>(k: 'T) ="
                            "    member _.Key = k"
                            "type Derived<'T>(k: 'T, h: int) ="
                            "    inherit Base<'T>(k)"
                            "let firstKey (d: Derived<'T>) ="
                            "    let f = fun () -> d.Key"
                            "    f ()"
                            "printfn \"%d\" (firstKey (Derived(7, 1)))"
                        ])
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
                let provider = SymbolProviders.buildContract defaultManifests

                let src =
                    String.concat
                        "\n"
                        [
                            "type C() ="
                            "    interface System.IComparable with"
                            "        member this.CompareTo(o: obj) = 0"
                        ]

                let lexed, file = parseFile src
                let ctx, tast = Pipeline.analyseSemWithContext provider src lexed file

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
                let provider = SymbolProviders.buildContract defaultManifests

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
                let _, tast = Pipeline.analyseSemWithContext provider src lexed file

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
                let provider = SymbolProviders.buildContract defaultManifests

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
                let ctx, tast = Pipeline.analyseSemWithContext provider src lexed file

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
                let provider = SymbolProviders.buildContract defaultManifests

                let src =
                    String.concat
                        "\n"
                        [
                            "type C() ="
                            "    interface System.IComparable with"
                            "        member this.CompareTo(o: obj) = \"wrong\""
                        ]

                let lexed, file = parseFile src
                let _, tast = Pipeline.analyseSemWithContext provider src lexed file

                let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

                Expect.isNonEmpty errors "a return-type mismatch against the interface diagnoses"

                Expect.isTrue
                    (errors |> List.exists (fun d -> d.Message.Contains "mismatch"))
                    "the diagnostic reports a type mismatch"
            }

            // §5.2 step 1 + 3: a member the interface does not declare is rejected,
            // and the required-but-unimplemented member is reported missing.
            test "a wrongly-named member is rejected and the required member reported missing" {
                let provider = SymbolProviders.buildContract defaultManifests

                let src =
                    String.concat
                        "\n"
                        [
                            "type C() ="
                            "    interface System.IComparable with"
                            "        member this.Nope(o: obj) = 0"
                        ]

                let lexed, file = parseFile src
                let _, tast = Pipeline.analyseSemWithContext provider src lexed file

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

[<Tests>]
let interfaceImplCodegenTests =
    // vesper-set-sprint-phase-5 §5.3 test gate: a class implementing the generic
    // `IEnumerable<int>` and the non-generic `IEnumerable` loads with both
    // `InterfaceImpl` rows; reflecting `GetInterfaces()` shows both; and calling
    // through each interface succeeds at runtime, the generic one yielding an
    // `IEnumerator<int>`. The class stores an `IEnumerator<int>` ctor param and
    // each `GetEnumerator` returns it (the non-generic one `:>`-upcast to the
    // base `IEnumerator`) — no external enumerator construction (that's Phase 6),
    // so the test exercises interface-impl emission, not enumeration machinery.
    let src =
        String.concat
            "\n"
            [
                "type C(e: System.Collections.Generic.IEnumerator<int>) ="
                "    interface System.Collections.Generic.IEnumerable<int> with"
                "        member this.GetEnumerator() : System.Collections.Generic.IEnumerator<int> = e"
                "    interface System.Collections.IEnumerable with"
                "        member this.GetEnumerator() : System.Collections.IEnumerator = e :> System.Collections.IEnumerator"
            ]

    // A fresh `IEnumerator<int>` over [1;2;3] (the BCL list's own), reused per
    // construction below — each test path builds its own so enumeration state
    // doesn't bleed across assertions.
    let freshEnumerator () : System.Collections.Generic.IEnumerator<int> =
        (System.Collections.Generic.List<int>([ 1; 2; 3 ]) :> seq<int>).GetEnumerator()

    testList
        "ClassInterfaceImplCodegen"
        [
            test "a class implementing IEnumerable<int> + IEnumerable emits both InterfaceImpl rows and enumerates" {
                let _, artifact = compileSource "ClsIEnum" src
                let bytes = Codegen.toBytes artifact

                // Both `InterfaceImpl` rows land in the PE metadata.
                Expect.equal
                    (peInterfaceImplCount bytes)
                    2
                    "C carries two InterfaceImpl rows (IEnumerable<int> + IEnumerable)"

                let asm = loadAssembly bytes
                let ty = asm.GetType "C"
                Expect.isNotNull ty "the assembly contains the class type C"

                let ifaceNames = ty.GetInterfaces() |> Array.map (fun i -> i.Name) |> Set.ofArray
                Expect.isTrue (ifaceNames.Contains "IEnumerable`1") "C reflects as implementing IEnumerable<int>"

                Expect.isTrue
                    (ifaceNames.Contains "IEnumerable")
                    "C reflects as implementing the non-generic IEnumerable"

                // `(c :> IEnumerable<int>).GetEnumerator()` yields an IEnumerator<int>.
                let asGeneric =
                    Activator.CreateInstance(ty, [| box (freshEnumerator ()) |])
                    :?> System.Collections.Generic.IEnumerable<int>

                Expect.equal (asGeneric |> Seq.toList) [ 1; 2; 3 ] "enumerating through IEnumerable<int> yields 1,2,3"

                // `(c :> IEnumerable).GetEnumerator()` succeeds through the non-generic slot.
                let asNonGeneric =
                    Activator.CreateInstance(ty, [| box (freshEnumerator ()) |]) :?> System.Collections.IEnumerable

                let nonGenericEnum = asNonGeneric.GetEnumerator()
                Expect.isTrue (nonGenericEnum.MoveNext()) "the non-generic enumerator advances to the first element"
                Expect.equal (nonGenericEnum.Current :?> int) 1 "the first element through IEnumerable is 1"
            }

            // vesper-set-phase-9-handoff G8 #3: a generic class implementing
            // `IStructuralEquatable` (`Equals(obj, IEqualityComparer)` /
            // `GetHashCode(IEqualityComparer)`) alongside `override`s of Object's
            // `Equals(obj)` / `GetHashCode()`. Pre-fix `Set\`1` failed CLR type-load
            // ("Method 'Equals' … does not have an implementation"): the unannotated
            // override/interface params leaked a method typar, so each emitted as a
            // spurious *generic* `Equals\`1`/`GetHashCode\`1` whose generic arity (1)
            // no longer matched the (arity-0) interface slot, and the `override`s
            // emitted *non-virtual*. The fix (a) flows `IsOverride` Tast→codegen so an
            // Object override emits virtual reusing the base slot, (b) pins each
            // override's params to the Object slot, and (c) skips generalisation for
            // override + interface-impl members. This gate forces the type to load
            // (the failure mode) and asserts every method is non-generic + virtual
            // with the expected arity.
            test
                "a class implementing IStructuralEquatable + Object overrides type-loads with non-generic virtual members" {
                let src =
                    String.concat
                        "\n"
                        [
                            "open System.Collections"
                            "type C<'T>(x: 'T) ="
                            "    member this.X = x"
                            "    override this.GetHashCode() = 0"
                            "    override this.Equals(that) = true"
                            "    interface IStructuralEquatable with"
                            "        member this.Equals(that, comparer) = comparer.Equals(that, that)"
                            "        member this.GetHashCode(comparer) = 0"
                        ]

                let _, artifact = compileSource "ClsStructEq" src
                let bytes = Codegen.toBytes artifact

                // Forcing the load is the pre-fix failure mode (TypeLoadException).
                let asm = loadAssembly bytes
                let ty = asm.GetType("C`1", throwOnError = true)

                let methodOf name arity =
                    ty.GetMethods(
                        BindingFlags.Public
                        ||| BindingFlags.NonPublic
                        ||| BindingFlags.Instance
                        ||| BindingFlags.DeclaredOnly
                    )
                    |> Array.tryFind (fun m -> m.Name = name && m.GetParameters().Length = arity)

                // The Object overrides: virtual, non-generic, reusing the base slots.
                let equalsObj = methodOf "Equals" 1
                Expect.isSome equalsObj "Equals(obj) override emitted"
                Expect.isTrue equalsObj.Value.IsVirtual "Equals(obj) is virtual (reuses Object.Equals)"
                Expect.equal (equalsObj.Value.GetGenericArguments().Length) 0 "Equals(obj) is non-generic"

                let getHash0 = methodOf "GetHashCode" 0
                Expect.isSome getHash0 "GetHashCode() override emitted"
                Expect.isTrue getHash0.Value.IsVirtual "GetHashCode() is virtual"
                Expect.equal (getHash0.Value.GetGenericArguments().Length) 0 "GetHashCode() is non-generic"

                // The IStructuralEquatable slots: 2-arg Equals + 1-arg GetHashCode,
                // both non-generic so the implicit name+signature match satisfies the
                // interface (no MethodImpl emission).
                let equals2 = methodOf "Equals" 2
                Expect.isSome equals2 "IStructuralEquatable.Equals(obj, comparer) emitted with two params"
                Expect.equal (equals2.Value.GetGenericArguments().Length) 0 "the 2-arg Equals is non-generic"

                let getHash1 = methodOf "GetHashCode" 1
                Expect.isSome getHash1 "IStructuralEquatable.GetHashCode(comparer) emitted"
                Expect.equal (getHash1.Value.GetGenericArguments().Length) 0 "the 1-arg GetHashCode is non-generic"

                // The whole type satisfies the interface — reflect it as implemented.
                let ifaces = ty.GetInterfaces() |> Array.map (fun i -> i.Name) |> Set.ofArray
                Expect.isTrue (ifaces.Contains "IStructuralEquatable") "C`1 implements IStructuralEquatable"
            }

            // vesper-set-phase-9-handoff gap #4 (the producer-grounding wall): a
            // generic class whose member passes a `'T`-typed value into a BCL `obj`
            // parameter (`comparer.GetHashCode(x)`, the `Set<'T>`/`IStructuralEquatable`
            // shape). Pre-fix the unifier *ground* `'T := obj` at that call, so EVERY
            // member of the class emitted `obj` for `'T` (`get_Value() : obj`, the
            // whole-class typar grounding that made `Set\`1::Add(obj):Set<obj>`). The
            // fix makes `obj` the universal supertype at argument-coercion sites (no
            // grounding — `Engine.unifyAppliedSig` / the `obj` rule in
            // `tryCoerceUpcast`) and boxes the typar argument at the call (`EmitCall`).
            // This gate asserts `'T` survives (`get_Value` returns the generic
            // parameter, not `obj`) AND the boxed call runs (the box is materialised).
            test "a generic member passing 'T into an obj parameter keeps 'T generic and boxes the arg" {
                let src =
                    String.concat
                        "\n"
                        [
                            "open System.Collections"
                            "type Holder<'T>(x: 'T) ="
                            "    member _.Value = x"
                            "    member _.HashVia(c: IEqualityComparer) = c.GetHashCode(x)"
                        ]

                let _, artifact = compileSource "HolderObjArg" src
                let bytes = Codegen.toBytes artifact
                let asm = loadAssembly bytes
                let ty = asm.GetType("Holder`1", throwOnError = true)

                // `'T` is NOT grounded: `get_Value` returns the declaring generic
                // parameter, not `obj`. (Pre-fix this returned `System.Object` — the
                // whole-class typar grounding.)
                let getValue =
                    ty.GetMethods(BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly)
                    |> Array.find (fun m -> m.Name = "get_Value")

                Expect.isTrue
                    getValue.ReturnType.IsGenericParameter
                    "get_Value returns the class typar (not obj) — the class typar was not ground to obj"

                // The implied box is materialised: `HashVia`'s body boxes the `'T`
                // argument (`box !0`, opcode 0x8C) before the `obj`-parameter call.
                // (Asserted at the IL level rather than by *calling* the BCL method —
                // the runtime member-ref for `IEqualityComparer.GetHashCode(obj)` is a
                // separate pre-existing gap, not exercised by the shipping round-trip.)
                let il = peMethodIl bytes "Holder`1" "HashVia"

                Expect.isTrue
                    (il |> Array.contains 0x8Cuy)
                    "HashVia emits a box (0x8C) for the 'T argument flowing into the obj parameter"
            }

            // thermo-nuclear review finding #1: the implicit value→`obj` upcast
            // (the front end accepts `value`/`'T` → `obj` without grounding — Engine's
            // `obj` rule) must be materialised as a `box` at EVERY call site, not only
            // the external-member path. A *project-local* instance call into an `obj`
            // parameter (`this.M(5)`) pre-fix pushed the raw `int` where `object` was
            // expected — unverifiable IL (`InvalidProgramException` on invoke). The fix
            // routes every call / ctor / record-cons emit site through the one shared
            // `boxArgIntoObjParam` policy. This gate asserts both the box opcode and a
            // successful invoke (the IL actually verifies + runs).
            test "a project-local instance call into an obj parameter boxes a value-type arg and runs" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type C() ="
                            "    member _.M(x: obj) = x"
                            "    member this.Probe() = this.M(5)"
                        ]

                let _, artifact = compileSource "ObjParamInstanceCall" src
                let bytes = Codegen.toBytes artifact
                let il = peMethodIl bytes "C" "Probe"

                Expect.isTrue
                    (il |> Array.contains 0x8Cuy)
                    "Probe boxes (0x8C) the int arg flowing into the obj parameter"

                // The IL verifies and runs: `Probe()` returns the boxed `5`.
                let asm = loadAssembly bytes
                let ty = asm.GetType("C", throwOnError = true)
                let instance = System.Activator.CreateInstance ty
                let result = ty.GetMethod("Probe").Invoke(instance, [||])
                Expect.equal (result :?> int) 5 "Probe() returns the boxed 5"
            }

            // vesper-set-phase-9-handoff "unbalanced member-body IL": a `void`-
            // returning interface-impl member whose body *terminates* (ends in
            // `raise`) — the `ICollection<'T>.Add` / `IDisposable.Dispose` shape on a
            // read-only `Set<'T>`. `buildMember` used to emit the residual-`unit` pop
            // for the void slot unconditionally; after a `Throw` the fall-through is
            // unreachable, so the pop lowered to an unreachable `Pop` that
            // `IlIr.analyze` rejected as an unbalanced body. Guarding the pop on a live
            // operand closes it. Fails pre-fix with `IlIr.lower: unbalanced body:
            // unreachable instruction Pop`.
            test "a void interface member whose body ends in `raise` emits and throws at runtime" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type C() ="
                            "    interface System.IDisposable with"
                            "        member this.Dispose() = raise (System.NotSupportedException(\"nope\"))"
                        ]

                let _, artifact = compileSource "ClsVoidRaise" src
                let bytes = Codegen.toBytes artifact
                let asm = loadAssembly bytes
                let ty = asm.GetType "C"
                Expect.isNotNull ty "the assembly contains the class type C"

                let disposable = Activator.CreateInstance ty :?> System.IDisposable

                let raised =
                    try
                        disposable.Dispose()
                        None
                    with :? System.NotSupportedException as ex ->
                        Some ex.Message

                Expect.equal
                    raised
                    (Some "nope")
                    "Dispose raises NotSupportedException (carrying the source message) through the void interface slot"
            }
        ]

[<Tests>]
let coercionTests =
    // vesper-set-g-wall §G19/G20: implicit class→interface / class→base upcasts.
    // G19 = a class value flowing into an interface-typed parameter (the
    // `Comparer<'T>.Default` → `IComparer<'T>` shape pervasive in set.fs);
    // G20 = an explicit `:>` to a base or a declared interface. The fix is in
    // `subsumes` (interface walk) + `tryCoerceUpcast`/`unifyArg` (which unify the
    // witness's type args so a generic / wildcard target is pinned) + the
    // interface-impl resolution pre-pass (so a class knows its interfaces before
    // any member body is typed). Front-end gates assert no diagnostic; the base
    // upcast also round-trips through codegen (ref-type upcast erases).
    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    let analyseErrs src =
        let provider = SymbolProviders.buildContract defaultManifests
        let lexed, file = parseFile src
        let _, tast = Pipeline.analyseSemWithContext provider src lexed file
        tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

    testList
        "ClassCoercion"
        [
            test "G19: a class value coerces to an interface-typed ctor param (secondary-ctor chain call)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type C(comparer: System.Collections.Generic.IComparer<int>) ="
                            "    member this.Cmp = comparer"
                            "    new() = C(System.Collections.Generic.Comparer<int>.Default)"
                        ]

                let errs = analyseErrs src
                Expect.isEmpty errs (sprintf "Comparer<int> coerces to the IComparer<int> ctor param: %A" errs)
            }

            test "G19: a class value coerces to an interface-typed function argument" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let f (c: System.Collections.Generic.IComparer<int>) = 0"
                            "let g () = f (System.Collections.Generic.Comparer<int>.Default)"
                        ]

                let errs = analyseErrs src
                Expect.isEmpty errs (sprintf "Comparer<int> coerces to the IComparer<int> arg: %A" errs)
            }

            test "G19: a class member's forward call to a sibling-module fn coerces a subtype arg to an interface param" {
                // The G19 *forward-reference* residue: `walkElems` types class member
                // bodies before it walks module-level `let`s, so `M.useCmp` has no
                // scheme yet when `C.Run` is typed. Without the annotation-derived
                // forward scheme (`prebindModuleFunctionSchemes`), the subtype arg
                // `Comparer<'T>` monomorphically pins `useCmp`'s param TyVar, clashing
                // with its own `IComparer<'T>` annotation once its body is typed. This
                // mirrors `SetTree.add`'s shape in set.fs.
                let src =
                    String.concat
                        "\n"
                        [
                            "module M ="
                            "    let useCmp (c: System.Collections.Generic.IComparer<'T>) (x: 'T) = x"
                            "type C<'T>(comparer: System.Collections.Generic.Comparer<'T>) ="
                            "    member this.Run (k: 'T) = M.useCmp comparer k"
                        ]

                let errs = analyseErrs src

                Expect.isEmpty
                    errs
                    (sprintf "forward module call upcasts the Comparer<'T> arg to IComparer<'T>: %A" errs)
            }

            test "G20: `(this :> System.IComparable)` upcasts a class to a declared interface" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type C() ="
                            "    interface System.IComparable with"
                            "        member this.CompareTo(o: obj) = 0"
                            "    member this.AsCmp () = (this :> System.IComparable)"
                        ]

                let errs = analyseErrs src
                Expect.isEmpty errs (sprintf "C upcasts to its declared IComparable: %A" errs)
            }

            test "G22: an external interface member's unannotated param resolves for member access (IEqualityComparer)" {
                // `IStructuralEquatable.Equals(that, comparer)` has *unannotated*
                // params; `comparer` is pinned to the external
                // `System.Collections.IEqualityComparer` only by the conformance unify
                // that runs *after* the body — so `comparer.Equals(…)` was deferred as
                // a pending dot-access on a free TyVar, and the drain (Engine.fs) only
                // knew project-local classes, mis-reporting "Unknown class type
                // 'IEqualityComparer'". The drain now resolves an external receiver
                // through the provider, the deferred mirror of `resolveFieldStep`.
                let src =
                    String.concat
                        "\n"
                        [
                            "open System.Collections"
                            "type C() ="
                            "    interface IStructuralEquatable with"
                            "        member this.Equals(that, comparer) ="
                            "            let _ = comparer.GetHashCode(that)"
                            "            comparer.Equals(that, that)"
                            "        member this.GetHashCode(comparer) = 0"
                        ]

                let errs = analyseErrs src

                Expect.isEmpty
                    errs
                    (sprintf "comparer.Equals/GetHashCode resolve on the IEqualityComparer param: %A" errs)
            }

            test "G21: `that :?> C` downcasts an interface member's unannotated (obj) param" {
                // `that` is pinned to `obj` only by the conformance unify after the
                // body, so at the downcast site it is still a free TyVar. The check
                // used to fire "Cannot downcast type 'TyVar …'"; an unresolved source
                // is now admitted (runtime-checked, like `obj`).
                let src =
                    String.concat
                        "\n"
                        [
                            "open System.Collections"
                            "type C() ="
                            "    interface IStructuralEquatable with"
                            "        member this.Equals(that, comparer) ="
                            "            if that :? C then"
                            "                let _ = that :?> C"
                            "                true"
                            "            else"
                            "                false"
                            "        member this.GetHashCode(comparer) = 0"
                        ]

                let errs = analyseErrs src
                Expect.isEmpty errs (sprintf "that :?> C admitted for the obj-typed interface param: %A" errs)
            }

            test "G20: `(this :> Shape)` upcasts a derived class to its base and round-trips" {
                let _, artifact =
                    compileSource
                        "UpcastBase"
                        (String.concat
                            "\n"
                            [
                                "type Shape(x: int) ="
                                "    member this.Raw = x"
                                "type Circle(r: int) ="
                                "    inherit Shape(r)"
                                "    member this.AsShape () = (this :> Shape).Raw"
                                "let c = Circle(7)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let circle = asm.GetType "Circle"
                let asShape = circle.GetMethod("AsShape", declaredInstance, null, [||], null)
                Expect.isNotNull asShape "AsShape emitted"

                let instance = Activator.CreateInstance(circle, [| box 7 |])

                Expect.equal
                    (asShape.Invoke(instance, [||]) :?> int)
                    7
                    "(this :> Shape).Raw reads the inherited field = 7"
            }
        ]
