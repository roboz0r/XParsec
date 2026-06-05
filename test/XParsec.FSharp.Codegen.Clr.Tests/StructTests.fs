module XParsec.FSharp.Codegen.Clr.Tests.StructTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// vesper-set-sprint-phase-6 — `[<Struct>]` value-type emission. These tests
// reflect over the emitted PE so a runtime fault (bad IL, wrong base type,
// lost mutation) surfaces through `loadAssembly` / `Activator.CreateInstance`.
//
// The boxed-interface path is the one `Vesper.Set`'s struct enumerator needs:
// construct a struct, coerce it to an interface (box), dispatch through the
// boxed reference (callvirt) — value-type *method dispatch on an unboxed local*
// is deliberately out of scope (see structs-handoff.md).

[<Tests>]
let structTests =
    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    testList
        "Struct"
        [
            test "a `[<Struct>]` type emits as a System.ValueType-based value type" {
                let _, artifact =
                    compileSource
                        "StructShape"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type SPoint(x: int, y: int) ="
                                "    member this.X = x"
                                "let p = SPoint(3, 4)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "SPoint"
                Expect.isNotNull ty "the assembly contains the struct type SPoint"
                Expect.isTrue ty.IsValueType "SPoint emits as a value type"
                Expect.isTrue ty.IsSealed "a value type is sealed"

                let fields = ty.GetFields(BindingFlags.Public ||| BindingFlags.Instance)
                let names = fields |> Array.map (fun f -> f.Name) |> Set.ofArray
                Expect.equal names (Set.ofList [ "x"; "y" ]) "both ctor-param backing fields are present"
            }

            test "a struct ctor stores ctor params + a member reads one back (boxed dispatch)" {
                let _, artifact =
                    compileSource
                        "StructCtorRead"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type Holder(v: int) ="
                                "    interface System.IComparable with"
                                "        member this.CompareTo(o: obj) = v"
                                "let h = Holder(7)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Holder"
                Expect.isTrue ty.IsValueType "Holder is a value type"

                // Reflection boxes the constructed struct; the interface dispatch is
                // a normal callvirt on the boxed reference, and the method reads the
                // ctor-param backing field through the byref `this`.
                let boxed = Activator.CreateInstance(ty, [| box 7 |])
                let cmp = boxed :?> IComparable
                Expect.equal (cmp.CompareTo(null)) 7 "CompareTo returns the stored ctor-param field"
            }

            test "a struct `val mutable` field mutates through a boxed method and persists" {
                let _, artifact =
                    compileSource
                        "StructMutable"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type Counter ="
                                "    val mutable N: int"
                                "    member this.Bump() = this.N <- this.N + 1"
                                "    member this.Get() = this.N"
                                "let c = Counter()"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Counter"
                Expect.isTrue ty.IsValueType "Counter is a value type"

                let fld = ty.GetField("N", BindingFlags.Public ||| BindingFlags.Instance)
                Expect.isNotNull fld "the val field N is emitted"
                Expect.isFalse fld.IsInitOnly "a mutable val field is writable"

                // A boxed value type: `Invoke` mutates the box's interior through the
                // byref `this`, so the mutation survives across calls.
                let boxed = Activator.CreateInstance ty
                let bump = ty.GetMethod("Bump", declaredInstance, null, [||], null)
                let get = ty.GetMethod("Get", declaredInstance, null, [||], null)
                bump.Invoke(boxed, [||]) |> ignore
                bump.Invoke(boxed, [||]) |> ignore
                Expect.equal (get.Invoke(boxed, [||]) :?> int) 2 "two Bump()s leave N = 2"
            }

            test "a method call on an unboxed struct local dispatches by address" {
                // structs-handoff #1: `p.Sum()` on a `let`-bound struct value needs
                // the receiver *address* (`ldloca` + `constrained. callvirt`), not a
                // by-value `callvirt` (invalid IL on an unboxed value type).
                let _, artifact =
                    compileSource
                        "StructUnboxedCall"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type SPoint(x: int, y: int) ="
                                "    member this.Sum() = x + y"
                                "    static member SumOf() : int ="
                                "        let p = SPoint(3, 4)"
                                "        p.Sum()"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "SPoint"
                let sumOf = ty.GetMethod("SumOf", BindingFlags.Public ||| BindingFlags.Static)
                Expect.isNotNull sumOf "SumOf emitted as a static method"
                Expect.equal (sumOf.Invoke(null, [||]) :?> int) 7 "p.Sum() on an unboxed local returns 7"
            }

            test "a property get on an unboxed struct local dispatches by address" {
                let _, artifact =
                    compileSource
                        "StructUnboxedProp"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type SPoint(x: int, y: int) ="
                                "    member this.X = x"
                                "    static member XOf() : int ="
                                "        let p = SPoint(5, 6)"
                                "        p.X"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "SPoint"
                let xOf = ty.GetMethod("XOf", BindingFlags.Public ||| BindingFlags.Static)
                Expect.isNotNull xOf "XOf emitted as a static method"
                Expect.equal (xOf.Invoke(null, [||]) :?> int) 5 "p.X on an unboxed local returns 5"
            }

            test "a mutating method on an unboxed struct local persists (in-place addressing)" {
                // This only passes if the receiver is addressed in place (`ldloca`
                // the slot) — a spill-to-temp copy per call would mutate a throwaway
                // and `Get()` would read the un-mutated original.
                let _, artifact =
                    compileSource
                        "StructUnboxedMutate"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type Counter ="
                                "    val mutable N: int"
                                "    member this.Bump() = this.N <- this.N + 1"
                                "    member this.Get() = this.N"
                                "    static member Run() : int ="
                                "        let c = Counter()"
                                "        c.Bump()"
                                "        c.Bump()"
                                "        c.Get()"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Counter"
                let run = ty.GetMethod("Run", BindingFlags.Public ||| BindingFlags.Static)
                Expect.isNotNull run "Run emitted as a static method"
                Expect.equal (run.Invoke(null, [||]) :?> int) 2 "two Bump()s on the same local leave N = 2"
            }

            test "a struct upcast `:>` to an interface boxes (round-trips through the interface)" {
                let _, artifact =
                    compileSource
                        "StructUpcast"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type Holder(v: int) ="
                                "    interface System.IComparable with"
                                "        member this.CompareTo(o: obj) = v"
                                "    static member AsCmp(h: Holder) : System.IComparable = h :> System.IComparable"
                                "let h = Holder(9)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Holder"

                // `AsCmp` upcasts the struct to the interface (`box`); dispatching
                // through the returned reference proves the `:>` box round-trips.
                let asCmp = ty.GetMethod("AsCmp", BindingFlags.Public ||| BindingFlags.Static)
                Expect.isNotNull asCmp "AsCmp emitted as a static method"

                let h = Activator.CreateInstance(ty, [| box 9 |])
                let cmp = asCmp.Invoke(null, [| h |]) :?> IComparable
                Expect.equal (cmp.CompareTo(null)) 9 "the boxed struct keeps its field value through `:>`"
            }

            // static-members-gap.md: the natural shape `SumOf(a, b)` that the
            // unboxed-dispatch test above had to sidestep. A tupled static member
            // now flattens to two scalar params, so it can take its own args and
            // forward them to the struct ctor.
            test "a two-parameter static member on a struct binds both args (SumOf(3,4) returns 7)" {
                let _, artifact =
                    compileSource
                        "StructStaticAdd2"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type SPoint(x: int, y: int) ="
                                "    member this.Sum() = x + y"
                                "    static member SumOf(a: int, b: int) : int ="
                                "        let p = SPoint(a, b)"
                                "        p.Sum()"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "SPoint"
                let sumOf = ty.GetMethod("SumOf", BindingFlags.Public ||| BindingFlags.Static)
                Expect.isNotNull sumOf "SumOf emitted as a static method"
                Expect.equal (sumOf.GetParameters().Length) 2 "SumOf has two scalar parameters (tuple flattened)"
                Expect.equal (sumOf.Invoke(null, [| box 3; box 4 |]) :?> int) 7 "SPoint(3,4).Sum() returns 7"
            }
        ]
