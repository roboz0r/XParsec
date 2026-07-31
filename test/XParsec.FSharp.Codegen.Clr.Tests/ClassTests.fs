module XParsec.FSharp.Codegen.Clr.Tests.ClassTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// These tests reflect over the emitted PE so a runtime failure (`InvalidProgram`,
// missing field, wrong vtable) surfaces with a legible stack trace through the
// `TestHelpers.loadAssembly` / `Activator.CreateInstance` path.

/// A top-level (Program-holder) value/function's emitted metadata name carries its
/// source offset (`x` → `x$<offset>`), so a shadowed `let x` stays a distinct row and
/// its `SymbolKey` handle key stays injective. Match a source name against that scheme.
let private topLevelNameMatches (source: string) (emitted: string) : bool =
    emitted = source || emitted.StartsWith(source + "$")

/// The Program-holder static field for a source top-level value `name` (its emitted
/// field is `name$<offset>`), or `null` when there is none.
let private programStaticField (program: Type) (name: string) : FieldInfo =
    program.GetFields(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)
    |> Array.tryFind (fun f -> topLevelNameMatches name f.Name)
    |> Option.toObj

[<Tests>]
let monoTests =
    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    testList
        "ClassMono"
        [
            // Ctor-param backing fields are compiler-generated storage, not declared
            // API: FSC emits them `assembly`, and so do we (see `Layout.classParts`).
            test "an emitted class type has a public ctor + one assembly-visible field per primary-ctor parameter" {
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

                Expect.isEmpty
                    (ty.GetFields(BindingFlags.Public ||| BindingFlags.Instance))
                    "no ctor-param backing field is public"

                let fields =
                    ty.GetFields(BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly)

                let names = fields |> Array.map (fun f -> f.Name) |> Set.ofArray
                Expect.equal names (Set.ofList [ "x"; "y" ]) "Point holds both ctor-param backing fields"
                Expect.isTrue (fields |> Array.forall (fun f -> f.IsAssembly)) "both are `assembly`, not `private`"
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

                // Properties emit as `get_<Name>` methods (no PropertyDef row).
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

            // A class without `[<Sealed>]` emits an *open* `TypeDefinition`
            // (no `TypeAttributes.Sealed`) so inheritance can derive from it;
            // `[<Sealed>]` flips the flag.
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

            // F# lets each member name its own self-identifier, independently of the
            // type-level `as` alias. Here `First` (self-id `a`) reads `Second`
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

            // `static member M(a, b)` parses as one tuple arg pattern;
            // `Elaborate.memberParams` must flatten it to scalar params.
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

            // A module-level value (`let x = e` at module scope) is a `public static`
            // field on its module holder, initialised by the holder's `.cctor`, read
            // everywhere as `ldsfld` — never a `Main` local or a closure capture.
            // The field lands on the named-module holder (`Helper`).

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

            // (2) A *generic* class `static let` initialiser references a module value
            //     (a module value read inside a generic class's `.cctor`).
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

            // (3) A sibling module function reading a module value emits as a static
            //     method (a direct `ldsfld`), not a closure capturing it.
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
                // `get ()` compiles to a PARAMETERLESS method — the lone `unit` param
                // is erased.
                Expect.equal (m.GetParameters().Length) 0 "get () erases its lone unit param (parameterless)"
                Expect.equal (m.Invoke(null, [||]) :?> int) 42 "Helper.get () reads seed = 42"
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
            //     module values / static methods inside the holder `.cctor`. A
            //     value that demotes to a closure held in a `Main` local (which a
            //     `.cctor` cannot see) fails with a targeted message.
            //
            //     Under the unified escape model an escaping function is NO LONGER
            //     demoted — it keeps its flat static method and the escape becomes a
            //     bridge, so a bare value-use can no
            //     longer strand a module value. The one demotion left is the *capture*
            //     axis: `f` captures the destructured top-level local `a` (a genuine
            //     `Main` local — a tuple-pattern `let` is not collected as a module
            //     value), so `f` stays a `Main`-local closure rather than a static
            //     method, `seed = f` is left un-bridged (a non-eligible reference), and
            //     `seed`'s `.cctor` init can't see the `Main`-local `f`.
            test "a module value whose init needs a Main local fails with a targeted error" {
                let msg =
                    try
                        compileSource
                            "ModuleValUnresolvable"
                            (String.concat
                                "\n"
                                [
                                    "let (a, b) = (1, 2)"
                                    "let f = fun (x: int) -> x + a"
                                    "module Helper ="
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
            //     recovered from the reference's own type.
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

                Expect.isNull (getEmpty.MakeGenericMethod(typeof<int>).Invoke(null, [||])) "Tree.getEmpty<int>() = null"

                // The generic-class `static let` cctor reads it (the `set.fs` shape).
                let boxInt = (asm.GetType "Box`1").MakeGenericType typeof<int>
                let s = boxInt.GetMethod("S", declaredStatic, null, [||], null)
                Expect.isNull (s.Invoke(null, [||])) "Box<int>.S() reads the static-let built from Tree.empty = null"

                // A direct instance-member read.
                let inst = boxInt.GetConstructors().[0].Invoke [||]
                let direct = boxInt.GetMethod("Direct", declaredInstance, null, [||], null)
                Expect.isNull (direct.Invoke(inst, [||])) "Box<int>().Direct() = Tree.empty<int> = null"
            }

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

            // A `static let mutable` binder IS the static field, so a write (`n <- e`)
            // stores to it — `TExpr.StaticFieldSet`, emitted `stsfld` (0x80), the store
            // analogue of the `ldsfld` (0x7E) a read emits. Assert both opcodes on the
            // emitted method IL directly (not just the reflected result), then confirm the
            // shared cell accumulates across two writes.
            test "a write to a `static let mutable` emits `stsfld` (accumulates to 7)" {
                let _, artifact =
                    compileSource
                        "ClsStaticLetMutable"
                        (String.concat
                            "\n"
                            [
                                "type C() ="
                                "    static let mutable n = 0"
                                "    static member Bump (k: int) = n <- n + k"
                                "    static member N = n"
                                "let c = C()"
                            ])

                let bytes = Codegen.toBytes artifact
                let bumpIl = peMethodIl bytes "C" "Bump"
                Expect.isTrue (Array.contains 0x80uy bumpIl) "the write to `static let mutable` emits `stsfld` (0x80)"
                Expect.isTrue (Array.contains 0x7Euy bumpIl) "the read of `n` in `n + k` emits `ldsfld` (0x7E)"

                let asm = loadAssembly bytes
                let ty = asm.GetType "C"
                let bump = ty.GetMethod("Bump", declaredStatic, null, [| typeof<int> |], null)
                let getN = ty.GetMethod("get_N", declaredStatic, null, [||], null)
                bump.Invoke(null, [| box 3 |]) |> ignore
                bump.Invoke(null, [| box 4 |]) |> ignore
                Expect.equal (getN.Invoke(null, [||]) :?> int) 7 "the shared static cell accumulates both writes"
            }

            // `static let` on a *generic* class. The field lives on the open generic
            // `TypeDefinition` (one instance per closed instantiation, `.cctor`-
            // initialised); its `.cctor` store and the member-body read both mint a
            // `MemberRef` on the self-`TypeSpec` (`Box\`1<!0>::tag`), not a raw `Def`
            // token. Read it back at two instantiations to prove the per-instantiation
            // field resolves.
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

            // A class's compiler-generated backing storage (`static let` fields,
            // ctor-param fields) is reached by lambdas lifted out of member bodies.
            // Those lambdas become closure classes nested in the enclosing *module*,
            // not in the class — so they are a DIFFERENT type reading the class's
            // fields, and only `assembly` accessibility lets them through. `private`
            // backing storage JIT-faults with `FieldAccessException`; these two cases
            // are the only ones that exercise the cross-type read, since every other
            // backing-field test reads from a member body of the declaring type.
            test "a `static let` read from inside a lambda in a member body (Assembly-visible field)" {
                runs
                    "43"
                    (String.concat
                        "\n"
                        [
                            "type C() ="
                            "    static let x = 42"
                            "    member _.F () = fun () -> x + 1"
                            "let c = C()"
                            "let g = c.F()"
                            "printfn \"%d\" (g ())"
                        ])
            }

            test "a ctor-param backing field read from inside a lambda in a member body (Assembly-visible field)" {
                runs
                    "43"
                    (String.concat
                        "\n"
                        [
                            "type C(k: int) ="
                            "    member _.F () = fun () -> k + 1"
                            "let c = C(42)"
                            "let g = c.F()"
                            "printfn \"%d\" (g ())"
                        ])
            }

            // A static *operator* member: the `(+)` body contains applications (the
            // `+` on the fields, the `V(...)` ctor) that only freeze once the body
            // is inferred; it emits as `op_Addition`.
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

                // `member this.N` emits a `get_N` method (no PropertyDefinition row),
                // so read it through the getter rather than GetProperty.
                let getN = ty.GetMethod("get_N", declaredInstance, null, [||], null)
                Expect.isNotNull getN "get_N emitted"

                let a = Activator.CreateInstance(ty, [| box 3 |])
                let b = Activator.CreateInstance(ty, [| box 4 |])
                let r = op.Invoke(null, [| a; b |])
                let n = getN.Invoke(r, [||]) :?> int
                Expect.equal n 7 "op_Addition(V 3, V 4).N = 7"
            }

            // A static member read on an *explicitly* instantiated generic class
            // (`Box<'T>.Tag`) parses as `DotLookup(TypeApp(Box, <'T>), .Member)`, not
            // the folded `LongIdent[Box; Member]` the bare `Box.Member` form takes.
            // Asserted at the TAST level.
            test "`Box<'T>.Member` lowers to Static{Method,Property} (no Elaborate TODO TypeApp)" {
                let provider = ClrSymbolProviders.buildContract defaultManifests

                // `Tag`/`Origin` are `'T`-free so the receiver's `<'T>` is the only
                // explicit instantiation under test.
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

                let _, tast =
                    Pipeline.analyseSemWithContext provider (Hashing.originSourceOfText src lexed) file

                let errors = tast.Diagnostics |> Diagnostic.errors
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
                        | TTypeKindG.Class c ->
                            for m in EqArray.toList c.Members do
                                TastWalk.iterExpr it m.Body
                        | _ -> ()
                    | _ -> ()

                Expect.isTrue (staticCalls > 0) "`Box<'T>.Make x` (in Remake) lowered to a TExpr.StaticMethodCall"
                Expect.isTrue (staticGets > 0) "`Box<'T>.Tag` (in MakeTagged) lowered to a TExpr.StaticPropertyGet"
            }

            // A static method on a *generic* class whose result type does not surface
            // the declaring instantiation, called from a concrete (non-declaring)
            // context. The result-type fast path can't recover `'T=int` (the return is
            // `int`), so it structurally matches the member's open signature against
            // the call's argument types (`RecoverOpenTypars`, declaring axis) to mint
            // `Box\`1<int32>::Describe`.
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

            // A higher-order call (`List.fold`) with a closure argument from inside a
            // member body. The own-class `static member (+)` taken by value
            // eta-reifies to `fun a b -> V.op_Addition(a, b)`, a member-body closure.
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

            // The generic shape end-to-end: a generic member-body closure `call`s the
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

            // Leading top-level values (declared before any top-level statement) are
            // `static initonly` fields on the Program holder, initialised by its
            // `.cctor` in declaration order — so a later leading value reads an earlier
            // one (`ldsfld`, already set). A member then reads them, and a trailing
            // `printfn` confirms the runtime values.
            test "leading top-level values are Program-holder initonly fields, cctor-initialised in order" {
                let _, artifact =
                    compileSource
                        "TopLevelLeading"
                        (String.concat
                            "\n"
                            [
                                "let a = 10" // leading → .cctor, initonly
                                "let b = a + 5" // leading → .cctor, initonly; reads a (ldsfld) in the cctor
                                "type Reader() ="
                                "    member this.Sum () : int = a + b"
                                "printfn \"%d\" ((Reader()).Sum())"
                            ])

                let bytes = Codegen.toBytes artifact

                // The member observes both values; the cctor set them in order (b = a + 5).
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "the program exits 0"
                Expect.stringContains output "25" "Reader().Sum() = a + b = 10 + 15 = 25"

                let asm = loadAssembly bytes
                let program = asm.GetType "Program"

                let field n = programStaticField program n

                Expect.isNotNull (field "a") "a is a public static field on Program"
                Expect.isTrue (field "a").IsInitOnly "a (leading) is initonly — set by the Program .cctor"
                Expect.isNotNull (field "b") "b is a public static field on Program"
                Expect.isTrue (field "b").IsInitOnly "b (leading) is initonly — set by the Program .cctor"
            }

            test "a member reads a top-level (leading) value via a Program-holder initonly field" {
                let _, artifact =
                    compileSource
                        "TopLevelValMember"
                        (String.concat
                            "\n"
                            [
                                "let provider = 42"
                                "type Reader() ="
                                "    member this.Get () : int = provider"
                                "let _z = Reader()"
                            ])

                let bytes = Codegen.toBytes artifact
                let asm = loadAssembly bytes
                let program = asm.GetType "Program"
                Expect.isNotNull program "the Program holder is emitted"

                let providerField = programStaticField program "provider"

                Expect.isNotNull providerField "the top-level value `provider` is a public static field on Program"
                Expect.isTrue providerField.IsInitOnly "provider (leading) is initonly — set by the Program .cctor"

                let readerTy = asm.GetType "Reader"
                let inst = readerTy.GetConstructors().[0].Invoke [||]
                let m = readerTy.GetMethod("Get", declaredInstance, null, [||], null)
                Expect.equal (m.Invoke(inst, [||]) :?> int) 42 "Reader().Get() reads the top-level value provider = 42"
            }

            // A *generic* top-level value (`let empty : 'T list = []`) cannot be a
            // static *field* (a non-generic Program holder has no type parameter to
            // type it), so it lowers to a **zero-arg generic static method** on the
            // Program holder, `call`ed at the use-site instantiation.
            test "a generic top-level value is a generic static method on Program (not a field)" {
                let _, artifact =
                    compileSource
                        "TopLevelGeneric"
                        (String.concat
                            "\n"
                            [
                                "let empty : 'T list = []" // generic → zero-arg generic static method
                                "let xs : int list = empty" // instantiates empty<int> (call MethodSpec)
                                "printfn \"%d\" (List.length xs)"
                            ])

                let bytes = Codegen.toBytes artifact

                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "the program exits 0"
                Expect.stringContains output "0" "List.length of the empty list = 0"

                let asm = loadAssembly bytes
                let program = asm.GetType "Program"
                Expect.isNotNull program "the Program holder is emitted"

                // `empty` is a generic static METHOD, never a static field.
                Expect.isNull
                    (program.GetField("empty", BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static))
                    "a generic value is not a static field"

                let emptyMethod =
                    program.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)
                    |> Array.tryFind (fun m -> topLevelNameMatches "empty" m.Name)

                match emptyMethod with
                | None -> failtest "the generic value `empty` is emitted as a static method on Program"
                | Some m ->
                    Expect.isTrue m.IsGenericMethodDefinition "`empty` is a generic method (one type parameter)"
                    Expect.equal (m.GetGenericArguments().Length) 1 "`empty<'T>` has one type parameter"
                    Expect.equal (m.GetParameters().Length) 0 "`empty` is a zero-arg method (a generic value)"
            }

            // A top-level value *after* a statement is a *trailing* value — a
            // `public static` (mutable, NOT initonly) field written by `Main` via
            // `stsfld` in source order, not hoisted to the pre-`Main` `.cctor`. The
            // parser folds the trailing `let r = …` into the preceding statement's
            // sequential; `EmitLower` flattens it back to a standalone decl. Proof of
            // the partition: `r`'s init side effect runs in `Main` *after* the leading
            // value is printed (not pre-`Main`); `p` (leading) is initonly, `r`
            // (trailing) is not.
            test "a top-level value after a statement is a Main-written mutable static field (trailing)" {
                let _, artifact =
                    compileSource
                        "TopLevelTrailing"
                        (String.concat
                            "\n"
                            [
                                "let p = 10" // leading → .cctor, initonly
                                "printfn \"%d\" p" // statement → Main
                                "let r = (printfn \"r-init\"; p + 5)" // trailing → Main, mutable static
                                "printfn \"%d\" r" // statement → Main, reads r (ldsfld)
                            ])

                let bytes = Codegen.toBytes artifact

                // Runtime order proves the partition: "10" (leading, set in .cctor and
                // printed first), THEN "r-init" (r's init side effect, run in Main —
                // not hoisted ahead of the printf), THEN "15".
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "the program exits 0"
                let iP = output.IndexOf "10"
                let iInit = output.IndexOf "r-init"
                let iR = output.IndexOf "15"
                Expect.isGreaterThan iP -1 "the leading value p (= 10) is printed"

                Expect.isGreaterThan
                    iInit
                    iP
                    "r's init side effect runs AFTER p is printed (Main, not the pre-Main .cctor)"

                Expect.isGreaterThan iR iInit "r (= 15) is printed after its init runs"

                let asm = loadAssembly bytes
                let program = asm.GetType "Program"
                Expect.isNotNull program "the Program holder is emitted"

                let statics = program.GetFields(BindingFlags.Public ||| BindingFlags.Static)

                let pField = programStaticField program "p"

                Expect.isNotNull pField "the leading value p is a public static field on Program"
                Expect.isTrue pField.IsInitOnly "p (leading) is initonly — set by the Program .cctor"

                // The trailing value `r` is nested (folded into a sequential), so it
                // carries no recorded source name and is field-named `value$<offset>`;
                // it is the (sole) mutable Program static — `Main` `stsfld`s it.
                let mutables = statics |> Array.filter (fun f -> not f.IsInitOnly) |> Array.toList

                Expect.equal mutables.Length 1 "exactly one trailing (mutable) Program static field — r"
                Expect.equal mutables.[0].FieldType typeof<int> "the trailing value r is an int field"
            }
        ]

[<Tests>]
let secondaryCtorTests =
    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    testList
        "ClassSecondaryCtor"
        [
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

            // Generic class secondary ctor: the chain target is a `MemberRef` on the
            // open self-`TypeSpec`. The secondary forwards its `'a` param to the
            // primary's first field and a constant `1` to the second, so `Box(7).V = 7`
            // and `Box(7).N = 1` prove both the generic chain ran and that the
            // non-first field round-trips.
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
    // `Holder<'T>(v)`. The type args unify against the head's nominal result,
    // and `tryClassRef` peels the `Expr.TypeApp` so the call still lowers to
    // `TExpr.New`.
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

            // Explicit `<'T>` at a construction site inside a member where `'T` is
            // the enclosing type's typar (in scope).
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

            // A *generic* class with more than one instance field must
            // round-trip *every* field, not just the first. The single-field
            // `Box<'a>(v: 'a)` tests above never exercised a field at index >= 1, so
            // the generic non-first-field gap (ctor `stfld` / member-body `ldfld`
            // resolving the wrong slot at runtime for a generic type) stayed latent.
            // `SetTree<'T>(k, h)` reads its second ctor param `h` for AVL height, so
            // this must work before any `Vesper.Set` runtime test is trusted.
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

    // A member that introduces its *own* generic parameter (`member this.Map<'b> …`).
    // The method's typar emits as a `GenericParam` row owned by the `MethodDef`
    // (encoded `!!i` in its signature), distinct from the declaring type's typars
    // (`!i`). A wrong typar index or missing `GenericParam` row surfaces as
    // `BadImageFormatException` / `InvalidProgram` at load/invoke.
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

            // The decisive case: the same signature mixes the *method* typar
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

    // `:?` and `:?>` emit `isinst` / `castclass` against a `TypeToken` for the
    // target type. These exercise the cast IL on a same-type cast on `this`
    // (`this :? C` / `this :?> C`), which still emits the real `isinst` /
    // `castclass` and runs them against a live instance.
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

    // `inherit Base(args)` wires the IL `TypeDefinition.BaseType` to the parent
    // and the primary `.ctor` to chain `ldarg.0; <args>; call Base::.ctor` before
    // storing the derived fields. Construction is itself the proof the chain is
    // sound — a `.ctor` that never calls a base / sibling ctor fails PE
    // verification — and an inherited member read confirms the base ctor stored its
    // arg.
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

            // The `inherit` args run BEFORE `this` exists, so no instance rewrite applies to
            // them — but the `.cctor` has already run, so a `static let` named there IS in scope
            // and is a static FIELD. Without the static field rewrite its binder key would reach
            // codegen as a bare `TExpr.Var` with no slot to load it from.
            test "a `static let` referenced in the `inherit` arguments loads from the static field" {
                let _, artifact =
                    compileSource
                        "InhStaticLetArg"
                        (String.concat
                            "\n"
                            [
                                "type Shape(x: int) ="
                                "    member this.Raw = x"
                                "type Circle() ="
                                "    inherit Shape(k + 1)"
                                "    static let k = 40"
                                "    member this.Two = 2"
                                "let c = Circle()"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let circle = asm.GetType "Circle"
                let instance = Activator.CreateInstance(circle, [||])

                let getRaw = circle.GetMethod("get_Raw", publicInstance, null, [||], null)
                Expect.equal (getRaw.Invoke(instance, [||]) :?> int) 41 "the base ctor got `k + 1` off the static field"
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

            // A monomorphic class inheriting an instantiated generic base
            // (`IntBox : Box<int>`): the base type is a `GENERICINST` `TypeSpec` and
            // the base `.ctor` a `MemberRef` on it.
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

            // A generic class inheriting a generic base instantiated at its *own*
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

            // Reading an *inherited* member from in-Vesper code (`node.Key` where
            // `Key` is declared on the base). The access walks the `inherit` chain
            // and upcasts the receiver to the declaring ancestor (a ref-type upcast is
            // a codegen no-op), so the receiver-keyed `resolveInstanceMember` resolves
            // `get_Key` on the base.
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

            // The same inherited read *inside a closure body*: a lambda capturing the
            // derived receiver and reading its inherited member must lower the same
            // way.
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

            // `base.M(...)` must dispatch non-virtually (`call B::M`), not virtually
            // (`callvirt`) — otherwise an `override` body calling `base.M()` re-enters
            // itself and stack-overflows. Parent returns 1; the override adds 1, so
            // calling `M` on a derived instance returns 2 (and *returns* at all,
            // proving it didn't recurse infinitely).
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
    // A user class implementing an external BCL interface front-end-resolves the
    // interface and its member body without diagnostic. Front-end only — the
    // interface type resolves against the metadata provider in the default contract
    // stack, the impl is registered on the class's `ClassTypeInfo`, and the member
    // body type-checks.
    testList
        "ClassInterfaceImpl"
        [
            test "a class implementing System.IComparable resolves the interface + CompareTo without diagnostic" {
                let provider = ClrSymbolProviders.buildContract defaultManifests

                let src =
                    String.concat
                        "\n"
                        [
                            "type C() ="
                            "    interface System.IComparable with"
                            "        member this.CompareTo(o: obj) = 0"
                        ]

                let lexed, file = parseFile src

                let ctx, tast =
                    Pipeline.analyseSemWithContext provider (Hashing.originSourceOfText src lexed) file

                let errors = tast.Diagnostics |> Diagnostic.errors
                Expect.isEmpty errors (sprintf "no front-end errors (%A)" errors)

                match TypeRegistry.tryClass ctx.Types UseSite.unbounded "C" with
                | ValueSome info ->
                    Expect.equal info.InterfaceImpls.Length 1 "one interface impl registered on C"
                    let impl = info.InterfaceImpls.[0]

                    match impl.Resolved with
                    | ValueSome(TyClass(name, _)) ->
                        Expect.stringContains name "IComparable" "the impl resolved to the IComparable interface"
                    | other -> failtestf "interface impl did not resolve to an interface TyClass: %A" other

                    Expect.equal impl.Members.Length 1 "the CompareTo member is registered on the impl"
                | ValueNone -> failtest "class C was not registered"
            }

            test "implementing a non-interface type is rejected with a diagnostic" {
                let provider = ClrSymbolProviders.buildContract defaultManifests

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

                let _, tast =
                    Pipeline.analyseSemWithContext provider (Hashing.originSourceOfText src lexed) file

                let errors = tast.Diagnostics |> Diagnostic.errors

                Expect.isNonEmpty errors "implementing a concrete class as an interface diagnoses"

                Expect.isTrue
                    (errors |> List.exists (fun d -> d.Message.Contains "not an interface"))
                    "the diagnostic explains the target is not an interface"
            }

            // A user class implementing the generic `IEnumerable<int>` and the
            // non-generic `IEnumerable` type-checks both `GetEnumerator` methods
            // independently. Each
            // `interface … with` block resolves *its own* declared `GetEnumerator`
            // overload (the metadata walk is `DeclaredOnly`), so the generic one
            // conforms to `unit -> IEnumerator<int>` and the non-generic one to
            // `unit -> IEnumerator` with no cross-talk. Bodies are `failwith` so the
            // front-end test needs no concrete enumerator — only the signatures matter.
            test "implementing IEnumerable<int> and IEnumerable conforms both GetEnumerator methods" {
                let provider = ClrSymbolProviders.buildContract defaultManifests

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

                let ctx, tast =
                    Pipeline.analyseSemWithContext provider (Hashing.originSourceOfText src lexed) file

                let errors = tast.Diagnostics |> Diagnostic.errors
                Expect.isEmpty errors (sprintf "no front-end errors (%A)" errors)

                match TypeRegistry.tryClass ctx.Types UseSite.unbounded "C" with
                | ValueSome info ->
                    Expect.equal info.InterfaceImpls.Length 2 "two interface impls registered on C"

                    Expect.isTrue
                        (info.InterfaceImpls
                         |> Array.forall (fun impl ->
                             match impl.Resolved with
                             | ValueSome(TyClass _) -> true
                             | _ -> false
                         ))
                        "both interface impls resolved to an interface TyClass"
                | ValueNone -> failtest "class C was not registered"
            }

            // A *generic* struct implementing a *generic*
            // local interface must thread the class typar `'T` into the impl
            // member's scope — the impl member's return type `'T` is the enclosing
            // type's type parameter, NOT a free typar, so it must not trip the
            // "Free type parameter" diagnostic.
            test
                "a generic struct implementing a generic local interface threads the class typar (no free-typar diagnostic)" {
                let provider = ClrSymbolProviders.buildContract defaultManifests

                let src =
                    String.concat
                        "\n"
                        [
                            "type IBox<'E> ="
                            "    abstract member Unwrap : unit -> 'E"
                            "[<Struct>]"
                            "type Box<'T>(value: 'T) ="
                            "    interface IBox<'T> with"
                            "        member this.Unwrap() : 'T = value"
                        ]

                let lexed, file = parseFile src

                let ctx, tast =
                    Pipeline.analyseSemWithContext provider (Hashing.originSourceOfText src lexed) file

                let errors = tast.Diagnostics |> Diagnostic.errors

                // The impl member's free `'T` is the enclosing struct's type
                // parameter, threaded into the impl scope — NOT a genuinely-free
                // typar, so it must not trip the diagnostic.
                Expect.isFalse
                    (errors |> List.exists (fun d -> d.Message.Contains "Free type parameter"))
                    "the impl member's return type 'T is the class typar, not a free typar"

                Expect.isEmpty errors (sprintf "no front-end errors (%A)" errors)

                match TypeRegistry.tryClass ctx.Types UseSite.unbounded "Box" with
                | ValueSome info ->
                    Expect.equal info.InterfaceImpls.Length 1 "one interface impl registered on Box"

                    match info.InterfaceImpls.[0].Resolved with
                    | ValueSome(TyClass(name, _)) -> Expect.stringContains name "IBox" "impl resolved to IBox"
                    | other -> failtestf "interface impl did not resolve to IBox: %A" other
                | ValueNone -> failtest "struct Box was not registered"
            }

            // Argument + return types must match the interface
            // signature. `CompareTo` returning a `string` where `IComparable`
            // promises an `int` is a conformance failure.
            test "a member whose signature does not match the interface is diagnosed" {
                let provider = ClrSymbolProviders.buildContract defaultManifests

                let src =
                    String.concat
                        "\n"
                        [
                            "type C() ="
                            "    interface System.IComparable with"
                            "        member this.CompareTo(o: obj) = \"wrong\""
                        ]

                let lexed, file = parseFile src

                let _, tast =
                    Pipeline.analyseSemWithContext provider (Hashing.originSourceOfText src lexed) file

                let errors = tast.Diagnostics |> Diagnostic.errors

                Expect.isNonEmpty errors "a return-type mismatch against the interface diagnoses"

                Expect.isTrue
                    (errors |> List.exists (fun d -> d.Message.Contains "mismatch"))
                    "the diagnostic reports a type mismatch"
            }

            // A member the interface does not declare is rejected,
            // and the required-but-unimplemented member is reported missing.
            test "a wrongly-named member is rejected and the required member reported missing" {
                let provider = ClrSymbolProviders.buildContract defaultManifests

                let src =
                    String.concat
                        "\n"
                        [
                            "type C() ="
                            "    interface System.IComparable with"
                            "        member this.Nope(o: obj) = 0"
                        ]

                let lexed, file = parseFile src

                let _, tast =
                    Pipeline.analyseSemWithContext provider (Hashing.originSourceOfText src lexed) file

                let errors = Diagnostic.errors tast.Diagnostics

                // The interface not declaring `Nope` IS "that type has no member of that
                // name"; asked of the verdict, so the two readings cannot drift apart.
                Expect.isTrue
                    (errors
                     |> List.exists (fun d ->
                         match d.Kind with
                         | Kind.NoMember(_, MemberNoun.Member, "Nope") -> true
                         | _ -> false
                     ))
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
    // A class implementing the generic `IEnumerable<int>` and the non-generic
    // `IEnumerable` loads with both `InterfaceImpl` rows; reflecting
    // `GetInterfaces()` shows both; and calling through each interface succeeds at
    // runtime. The class stores an `IEnumerator<int>` ctor param and each
    // `GetEnumerator` returns it (the non-generic one `:>`-upcast to the base
    // `IEnumerator`) — no external enumerator construction, so the test exercises
    // interface-impl emission, not enumeration machinery.
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

            // Generic struct-interface-impl escape-hatch shape: A *generic*
            // class `C<'T>` implementing `IEnumerable<'T>` + the non-generic `IEnumerable`
            // upcasts its OWN-TYPAR-instantiated `IEnumerator<'T>` to the non-generic base
            // `IEnumerator` (`e :> System.Collections.IEnumerator`). The upcast target's
            // generic arg is the class typar `'T`, so the subtype walk must reach the
            // non-generic base interface of `IEnumerator<'T>` — a relation distinct from
            // the concrete `IEnumerator<int>` case above.
            test "a generic class upcasts its own-typar IEnumerator<'T> to the non-generic IEnumerator and enumerates" {
                let gsrc =
                    String.concat
                        "\n"
                        [
                            "type C<'T>(e: System.Collections.Generic.IEnumerator<'T>) ="
                            "    interface System.Collections.Generic.IEnumerable<'T> with"
                            "        member this.GetEnumerator() : System.Collections.Generic.IEnumerator<'T> = e"
                            "    interface System.Collections.IEnumerable with"
                            "        member this.GetEnumerator() : System.Collections.IEnumerator = e :> System.Collections.IEnumerator"
                        ]

                let _, artifact = compileSource "ClsIEnumGen" gsrc
                let bytes = Codegen.toBytes artifact

                Expect.equal
                    (peInterfaceImplCount bytes)
                    2
                    "C<'T> carries two InterfaceImpl rows (IEnumerable<'T> + IEnumerable)"

                let asm = loadAssembly bytes
                let ty = asm.GetType("C`1", throwOnError = true)
                let tyInt = ty.MakeGenericType(typeof<int>)

                let ifaceNames = tyInt.GetInterfaces() |> Array.map (fun i -> i.Name) |> Set.ofArray
                Expect.isTrue (ifaceNames.Contains "IEnumerable`1") "C<int> reflects as implementing IEnumerable<'T>"

                Expect.isTrue
                    (ifaceNames.Contains "IEnumerable")
                    "C<int> reflects as implementing the non-generic IEnumerable"

                // `(c :> IEnumerable<int>).GetEnumerator()` yields an IEnumerator<int>.
                let asGeneric =
                    Activator.CreateInstance(tyInt, [| box (freshEnumerator ()) |])
                    :?> System.Collections.Generic.IEnumerable<int>

                Expect.equal
                    (asGeneric |> Seq.toList)
                    [ 1; 2; 3 ]
                    "enumerating C<int> through IEnumerable<int> yields 1,2,3"

                // `(c :> IEnumerable).GetEnumerator()` succeeds through the non-generic slot
                // — the own-typar `IEnumerator<'T> :> IEnumerator` upcast in the body.
                let asNonGeneric =
                    Activator.CreateInstance(tyInt, [| box (freshEnumerator ()) |]) :?> System.Collections.IEnumerable

                let nonGenericEnum = asNonGeneric.GetEnumerator()
                Expect.isTrue (nonGenericEnum.MoveNext()) "the non-generic enumerator advances to the first element"
                Expect.equal (nonGenericEnum.Current :?> int) 1 "the first element through IEnumerable is 1"
            }

            // A generic class implementing `IStructuralEquatable`
            // (`Equals(obj, IEqualityComparer)` / `GetHashCode(IEqualityComparer)`)
            // alongside `override`s of Object's `Equals(obj)` / `GetHashCode()`.
            // Unannotated override/interface params must not be generalised (that would
            // emit a spurious generic arity that mismatches the interface slot). The
            // `override`s must emit virtual, reusing the base Object slots. This gate
            // forces the type to load and asserts every method is non-generic + virtual
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

            // A generic class whose member passes a `'T`-typed value into a BCL `obj`
            // parameter (`comparer.GetHashCode(x)`). `obj` is the universal supertype
            // at argument-coercion sites (no grounding), and the typar argument is
            // boxed at the call. This gate asserts `'T` survives (`get_Value` returns
            // the generic parameter, not `obj`) AND the boxed call runs.
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
                let il = peMethodIl bytes "Holder`1" "HashVia"

                Expect.isTrue
                    (il |> Array.contains 0x8Cuy)
                    "HashVia emits a box (0x8C) for the 'T argument flowing into the obj parameter"
            }

            // The implicit value→`obj` upcast must be materialised as a `box` at
            // EVERY call site, not only the external-member path. A *project-local*
            // instance call into an `obj` parameter (`this.M(5)`) must push a boxed
            // value, not a raw `int` (unverifiable IL). Every call / ctor /
            // record-cons emit site routes through the shared `boxArgIntoObjParam`
            // policy. This gate asserts both the box opcode and a successful invoke.
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

            // A `void`-returning interface-impl member whose body *terminates* (ends
            // in `raise`). `buildMember` must not emit the residual-`unit` pop after
            // a `Throw`: the fall-through is unreachable, so an unconditional pop
            // produces an unbalanced body that `IlIr.analyze` rejects. The pop is
            // guarded on a live operand.
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
    // Implicit class→interface and explicit class→base upcasts. The mechanism is
    // `subsumes` (interface walk) + `tryCoerceUpcast`/`unifyArg` (which unify the
    // witness's type args so a generic / wildcard target is pinned) + the
    // interface-impl resolution pre-pass (so a class knows its interfaces before
    // any member body is typed). Front-end gates assert no diagnostic; the base
    // upcast also round-trips through codegen (ref-type upcast erases).
    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    let analyseErrs src =
        let provider = ClrSymbolProviders.buildContract defaultManifests
        let lexed, file = parseFile src

        let _, tast =
            Pipeline.analyseSemWithContext provider (Hashing.originSourceOfText src lexed) file

        tast.Diagnostics |> Diagnostic.errors

    testList
        "ClassCoercion"
        [
            test "a class value coerces to an interface-typed ctor param (secondary-ctor chain call)" {
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

            test "a class value coerces to an interface-typed function argument" {
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

            test "a class member's forward call to a sibling-module fn coerces a subtype arg to an interface param" {
                // `walkElems` types class member bodies before it walks module-level
                // `let`s, so `M.useCmp` has no scheme yet when `C.Run` is typed.
                // Without the annotation-derived forward scheme
                // (`prebindModuleFunctionSchemes`), the subtype arg `Comparer<'T>`
                // monomorphically pins `useCmp`'s param TyVar, clashing with its own
                // `IComparer<'T>` annotation once its body is typed.
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

            test "`(this :> System.IComparable)` upcasts a class to a declared interface" {
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

            test "an external interface member's unannotated param resolves for member access (IEqualityComparer)" {
                // `IStructuralEquatable.Equals(that, comparer)` has *unannotated*
                // params; `comparer` is pinned to `System.Collections.IEqualityComparer`
                // only by the conformance unify that runs *after* the body — so
                // `comparer.Equals(…)` defers as a pending dot-access on a free TyVar.
                // The discharge resolves an external receiver through the provider.
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

            test "`that :?> C` downcasts an interface member's unannotated (obj) param" {
                // `that` is pinned to `obj` only by the conformance unify after the
                // body, so at the downcast site it is still a free TyVar. An
                // unresolved source is admitted (runtime-checked, like `obj`).
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

            test "`(this :> Shape)` upcasts a derived class to its base and round-trips" {
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

[<Tests>]
let classPreambleTests =
    // The class preamble: instance `let` / `do` (the tail of the primary `.ctor`, after
    // the base-ctor call and the ctor-param stores) and `static do` (part of the
    // `.cctor`'s ordered static sequence).
    //
    // EVERY initialiser here contains an OPERATOR: a literal-only initialiser cannot
    // catch an operator reaching Elaborate in a preamble position with no desugared
    // form — a hard crash the `static let x = 42` shape hid for as long as it was the
    // only preamble under test.
    //
    // The RUNTIME semantics of the instance preamble (declaration order, `let mutable`
    // sharing one storage location, `let rec`, generics, a function-`let` used
    // first-class) are owned by the cross-backend conformance corpus
    // (`test/Codegen.Conformance/classes/preamble-*.fs`, run on CLR *and* JS). What
    // lives here is what the corpus cannot see: IL/metadata facts (field visibility,
    // `initonly`), and the CLR-only shapes JS rejects outright — `inherit` and the
    // static preamble.
    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    let declaredStatic =
        BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly

    let backingField (ty: Type) (name: string) =
        ty.GetField(name, BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly)

    testList
        "ClassPreamble"
        [
            test "an instance `let` reading a ctor param backs an assembly initonly field a member reads" {
                let _, artifact =
                    compileSource
                        "PreambleLet"
                        (String.concat
                            "\n"
                            [
                                "type C(n: int) ="
                                "    let m = n + 1"
                                "    member this.M () = m * 10"
                                "let c = C(1)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "C"

                let field = backingField ty "m"
                Expect.isNotNull field "the instance `let` m takes a backing field"
                Expect.isTrue field.IsAssembly "m is `assembly` — a closure class is a sibling type, not a nested one"
                Expect.isTrue field.IsInitOnly "a non-`mutable` let is written once, by the ctor"

                let instance = Activator.CreateInstance(ty, [| box 4 |])
                let m = ty.GetMethod("M", declaredInstance, null, [||], null)
                Expect.equal (m.Invoke(instance, [||]) :?> int) 50 "C(4).M() reads m = n + 1 = 5"
            }

            // A `let mutable` is an ordinary mutable FIELD, never a ref cell: the
            // function-`let` closure and every member body must see the same storage, so
            // a mutation through the closure has to persist across calls.
            test "a `let mutable` mutated through a function-`let` persists across calls" {
                let _, artifact =
                    compileSource
                        "PreambleMutable"
                        (String.concat
                            "\n"
                            [
                                "type Counter(step: int) ="
                                "    let mutable count = 0"
                                "    let bump (k: int) = count <- count + k * step"
                                "    member this.Bump (k: int) ="
                                "        let _ = bump k"
                                "        count"
                                "let c = Counter(1)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Counter"

                let count = backingField ty "count"
                Expect.isNotNull count "the `let mutable` count takes a backing field"
                Expect.isFalse count.IsInitOnly "a `let mutable` field stays writable"

                let bump = backingField ty "bump"
                Expect.isNotNull bump "the function-valued `let` bump takes a backing field (a closure over `this`)"

                let instance = Activator.CreateInstance(ty, [| box 2 |])

                let m = ty.GetMethod("Bump", declaredInstance, null, [| typeof<int> |], null)

                Expect.equal (m.Invoke(instance, [| box 3 |]) :?> int) 6 "first Bump(3) sets count = 3 * 2"
                Expect.equal (m.Invoke(instance, [| box 4 |]) :?> int) 14 "second Bump(4) accumulates onto the field"
            }

            // Probed: F# runs `inherit Base(…)` first, then the derived preamble
            // top-to-bottom — so the base's `do` must print before the derived one's.
            test "the base ctor runs before the derived class's preamble" {
                runsLines
                    [ "base 5"; "derived z=8" ]
                    (String.concat
                        "\n"
                        [
                            "type Base(x: int) ="
                            "    do printfn \"base %d\" x"
                            "    member this.X () = x"
                            "type Derived(y: int) ="
                            "    inherit Base(y + 2)"
                            "    let z = y + 5"
                            "    do printfn \"derived z=%d\" z"
                            "    member this.Z () = z"
                            "let d = Derived(3)"
                        ])
            }

            // The `.cctor` runs the WHOLE static sequence in declaration order, not just
            // the `static let` stores: `static do` sees the lets above it and runs before
            // the lets below it.
            test "`static do` runs in the cctor, interleaved with `static let` in declaration order" {
                runsLines
                    [ "static a=2"; "static b=6"; "B=6" ]
                    (String.concat
                        "\n"
                        [
                            "type S() ="
                            "    static let a = 1 + 1"
                            "    static do printfn \"static a=%d\" a"
                            "    static let b = a * 3"
                            "    static do printfn \"static b=%d\" b"
                            "    static member B () = b"
                            "printfn \"B=%d\" (S.B ())"
                        ])
            }

            test "a class whose static preamble is only `static do` still gets a cctor" {
                let _, artifact =
                    compileSource
                        "PreambleStaticDoOnly"
                        (String.concat
                            "\n"
                            [
                                "type S() ="
                                "    static do printfn \"%d\" (1 + 1)"
                                "    static member Id (x: int) = x"
                                "let s = S()"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "S"

                Expect.isNotNull (ty.TypeInitializer) "the `static do`-only class still emits a `.cctor`"

                let m = ty.GetMethod("Id", declaredStatic, null, [| typeof<int> |], null)

                Expect.equal (m.Invoke(null, [| box 3 |]) :?> int) 3 "the class is otherwise intact"
            }
        ]
