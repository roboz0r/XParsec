module XParsec.FSharp.Codegen.Clr.Tests.RecordTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Records-plan §B5 backend tests. Mirrors `StructuralEqualityTests` for the
// equality triple, plus end-to-end runtime tests that exercise the value-level
// IL (`RecordCons` / `FieldGet` / `FieldSet` / `RecordClone` / `TPat.Record`).

[<Tests>]
let monoTests =
    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    testList
        "RecordMono"
        [
            test "a record literal constructs + a field-get reads its value (prints 7)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type Point = { X: int; Y: int }"
                            "let p = { X = 7; Y = 9 }"
                            "printfn \"%d\" p.X"
                        ]

                let _, artifact = compileSource "RecLitFieldGet" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "7" "p.X returns the field value the literal stored"
            }

            test "field-set on a mutable field updates in place (prints 42)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type Counter = { mutable Count: int }"
                            "let c = { Count = 0 }"
                            "c.Count <- 42"
                            "printfn \"%d\" c.Count"
                        ]

                let _, artifact = compileSource "RecFieldSet" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "42" "c.Count round-trips through FieldSet/FieldGet"
            }

            test "record-clone overrides one field + copies the rest (prints 99)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type Point = { X: int; Y: int }"
                            "let p = { X = 1; Y = 2 }"
                            "let p2 = { p with Y = 99 }"
                            "printfn \"%d\" p2.Y"
                            "printfn \"%d\" p2.X"
                        ]

                let _, artifact = compileSource "RecClone" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                let lines = output.Split([| '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)

                Expect.equal lines.[0] "99" "Y was overridden"
                Expect.equal lines.[1] "1" "X was copied from source"
            }

            test "record pattern destructures named fields (prints 30)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type Point = { X: int; Y: int }"
                            "let p = { X = 10; Y = 20 }"
                            "let s = match p with | { X = x; Y = y } -> x + y"
                            "printfn \"%d\" s"
                        ]

                let _, artifact = compileSource "RecPat" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "30" "TPat.Record bound both fields and summed them"
            }

            test "an emitted record type has a public ctor + one public field per record field" {
                let _, artifact =
                    compileSource
                        "RecMeta"
                        (String.concat "\n" [ "type Point = { X: int; Y: int }"; "let p = { X = 0; Y = 0 }" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Point"
                Expect.isNotNull ty "the assembly contains the record type Point"

                let ctors = ty.GetConstructors(BindingFlags.Public ||| BindingFlags.Instance)
                Expect.equal ctors.Length 1 "Point declares one public ctor"
                Expect.equal (ctors.[0].GetParameters().Length) 2 "Point ctor takes the two fields"

                let fields = ty.GetFields(BindingFlags.Public ||| BindingFlags.Instance)
                let names = fields |> Array.map (fun f -> f.Name) |> Set.ofArray
                Expect.equal names (Set.ofList [ "X"; "Y" ]) "Point exposes both fields publicly"
            }

            test "a record's bytes do not pin FSharp.Core (BCL-only equality + IL)" {
                let _, artifact =
                    compileSource
                        "RecNoDep"
                        (String.concat "\n" [ "type Point = { X: int; Y: int }"; "let p = { X = 0; Y = 0 }" ])

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf "record emission only references the BCL (%A)" artifact.FSharpCoreDependencies)
            }

            test "an all-immutable record emits the structural-equality triple + IEquatable<Self>" {
                let _, artifact =
                    compileSource
                        "RecEqMeta"
                        (String.concat "\n" [ "type Point = { X: int; Y: int }"; "let p = { X = 0; Y = 0 }" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Point"

                let equalsObj =
                    ty.GetMethod("Equals", declaredInstance, null, [| typeof<obj> |], null)

                let equalsTyped = ty.GetMethod("Equals", declaredInstance, null, [| ty |], null)
                let getHash = ty.GetMethod("GetHashCode", declaredInstance, null, [||], null)

                Expect.isNotNull equalsObj "Equals(object) override emitted"
                Expect.isNotNull equalsTyped "typed Equals(Point) emitted"
                Expect.isNotNull getHash "GetHashCode() override emitted"

                Expect.isTrue equalsObj.IsVirtual "Equals(object) is virtual"
                Expect.isTrue getHash.IsVirtual "GetHashCode() is virtual"

                let iface = typedefof<IEquatable<_>>.MakeGenericType ty
                Expect.isTrue (iface.IsAssignableFrom ty) "Point implements IEquatable<Point>"
            }

            test "two records with equal fields compare equal + hash equal" {
                let _, artifact =
                    compileSource
                        "RecEqValue"
                        (String.concat "\n" [ "type Point = { X: int; Y: int }"; "let p = { X = 0; Y = 0 }" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Point"

                let mk x y =
                    Activator.CreateInstance(ty, [| box (x: int); box (y: int) |])

                let p1 = mk 3 4
                let p2 = mk 3 4
                let p3 = mk 3 5

                let equalsObj =
                    ty.GetMethod("Equals", declaredInstance, null, [| typeof<obj> |], null)

                let typedEquals = ty.GetMethod("Equals", declaredInstance, null, [| ty |], null)
                let hash = ty.GetMethod("GetHashCode", declaredInstance, null, [||], null)

                Expect.isTrue (equalsObj.Invoke(p1, [| p2 |]) :?> bool) "equal fields ⇒ Equals(object) true"
                Expect.isTrue (typedEquals.Invoke(p1, [| p2 |]) :?> bool) "equal fields ⇒ typed Equals true"
                Expect.isFalse (equalsObj.Invoke(p1, [| p3 |]) :?> bool) "Y differs ⇒ Equals(object) false"
                Expect.isFalse (equalsObj.Invoke(p1, [| null |]) :?> bool) "Equals(object) rejects null"

                Expect.equal (hash.Invoke(p1, [||]) :?> int) (hash.Invoke(p2, [||]) :?> int) "equal records hash equal"
            }

            test "a record with a mutable field does NOT declare its own equality triple" {
                let _, artifact =
                    compileSource
                        "RecEqMutSkip"
                        (String.concat "\n" [ "type Counter = { mutable Count: int }"; "let c = { Count = 0 }" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Counter"

                Expect.isNull
                    (ty.GetMethod("Equals", declaredInstance, null, [| typeof<obj> |], null))
                    "no Equals(object) override on the record itself"

                Expect.isNull
                    (ty.GetMethod("GetHashCode", declaredInstance, null, [||], null))
                    "no GetHashCode override on the record itself"

                let iface = typedefof<IEquatable<_>>.MakeGenericType ty
                Expect.isFalse (iface.IsAssignableFrom ty) "Counter does not declare IEquatable<Counter>"
            }
        ]

[<Tests>]
let genericTests =
    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    testList
        "RecordGeneric"
        [
            test "a generic record `Box<'T>` is emitted as a generic TypeDefinition" {
                let _, artifact =
                    compileSource
                        "RecGenMeta"
                        (String.concat "\n" [ "type Box<'T> = { Value: 'T }"; "let b = { Value = 0 }" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let boxTy = asm.GetType "Box`1"
                Expect.isNotNull boxTy "Box`1 emitted"
                Expect.isTrue boxTy.IsGenericTypeDefinition "Box`1 is a generic type definition"

                let boxInt = boxTy.MakeGenericType typeof<int>

                Expect.isNotNull
                    (boxInt.GetMethod("Equals", declaredInstance, null, [| typeof<obj> |], null))
                    "Box<int>::Equals(object) reachable"

                Expect.isNotNull
                    (boxInt.GetMethod("Equals", declaredInstance, null, [| boxInt |], null))
                    "Box<int>::Equals(Box<int>) reachable"

                let iface = typedefof<IEquatable<_>>.MakeGenericType boxInt
                Expect.isTrue (iface.IsAssignableFrom boxInt) "Box<int> implements IEquatable<Box<int>>"
            }

            test "Box<int> compares structurally through `EqualityComparer<!0>` (the typar-typed field)" {
                let _, artifact =
                    compileSource
                        "RecGenIntEq"
                        (String.concat "\n" [ "type Box<'T> = { Value: 'T }"; "let b = { Value = 0 }" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let boxTy = asm.GetType "Box`1"
                let boxInt = boxTy.MakeGenericType typeof<int>

                let mkInt (v: int) =
                    Activator.CreateInstance(boxInt, [| box v |])

                let bi3a = mkInt 3
                let bi3b = mkInt 3
                let bi5 = mkInt 5

                let typedEquals =
                    boxInt.GetMethod("Equals", declaredInstance, null, [| boxInt |], null)

                let hash = boxInt.GetMethod("GetHashCode", declaredInstance, null, [||], null)

                Expect.isTrue
                    (typedEquals.Invoke(bi3a, [| bi3b |]) :?> bool)
                    "Box 3 = Box 3 (int field via EqualityComparer<!0>)"

                Expect.isFalse (typedEquals.Invoke(bi3a, [| bi5 |]) :?> bool) "Box 3 <> Box 5"

                Expect.equal
                    (hash.Invoke(bi3a, [||]) :?> int)
                    (hash.Invoke(bi3b, [||]) :?> int)
                    "equal Box<int> hash equal"
            }

            test "Box<string> uses the same emitted members (the `!0` encoding works for any element)" {
                let _, artifact =
                    compileSource
                        "RecGenStrEq"
                        (String.concat "\n" [ "type Box<'T> = { Value: 'T }"; "let b = { Value = 0 }" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let boxTy = asm.GetType "Box`1"
                let boxStr = boxTy.MakeGenericType typeof<string>

                let mkStr (v: string) =
                    Activator.CreateInstance(boxStr, [| box v |])

                let bs = mkStr "hi"
                let bs' = mkStr "hi"
                let bs2 = mkStr "yo"

                let typedEquals =
                    boxStr.GetMethod("Equals", declaredInstance, null, [| boxStr |], null)

                let hash = boxStr.GetMethod("GetHashCode", declaredInstance, null, [||], null)

                Expect.isTrue (typedEquals.Invoke(bs, [| bs' |]) :?> bool) "Box \"hi\" = Box \"hi\""
                Expect.isFalse (typedEquals.Invoke(bs, [| bs2 |]) :?> bool) "Box \"hi\" <> Box \"yo\""

                Expect.equal
                    (hash.Invoke(bs, [||]) :?> int)
                    (hash.Invoke(bs', [||]) :?> int)
                    "equal Box<string> hash equal"
            }

            test "Equals(object) on Box<int> rejects null and a different instantiation" {
                let _, artifact =
                    compileSource
                        "RecGenIsinst"
                        (String.concat "\n" [ "type Box<'T> = { Value: 'T }"; "let b = { Value = 0 }" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let boxTy = asm.GetType "Box`1"
                let boxInt = boxTy.MakeGenericType typeof<int>
                let boxStr = boxTy.MakeGenericType typeof<string>

                let bi3 = Activator.CreateInstance(boxInt, [| box 3 |])
                let bs3 = Activator.CreateInstance(boxStr, [| box "3" |])

                let equalsObj =
                    boxInt.GetMethod("Equals", declaredInstance, null, [| typeof<obj> |], null)

                Expect.isFalse (equalsObj.Invoke(bi3, [| null |]) :?> bool) "Box<int> 3 <> null"

                Expect.isFalse
                    (equalsObj.Invoke(bi3, [| bs3 |]) :?> bool)
                    "Box<int> 3 <> Box<string> \"3\" (isinst Box<int> fails)"
            }

            // B-1 ctor-store fix, record mirror: a *multi-field* generic record must
            // round-trip *every* field, not just the first. The existing single-field
            // `Box<'T> = { Value: 'T }` tests never exercised a field at index >= 1,
            // so the raw-`FieldDef` `stfld` miscompilation on non-first generic fields
            // stayed latent here too.
            test "a multi-field generic record round-trips its non-first field (Pair<int>.Second = 3)" {
                let _, artifact =
                    compileSource
                        "RecGenTwoField"
                        (String.concat
                            "\n"
                            [
                                "type Pair<'a> = { First: 'a; Second: int }"
                                "let p = { First = 0; Second = 0 }"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let pairInt = (asm.GetType "Pair`1").MakeGenericType typeof<int>

                let instance = Activator.CreateInstance(pairInt, [| box 7; box 3 |])

                Expect.equal (pairInt.GetField("First").GetValue instance :?> int) 7 "Pair.First = 7 (first field)"

                Expect.equal
                    (pairInt.GetField("Second").GetValue instance :?> int)
                    3
                    "Pair.Second = 3 (non-first field)"
            }
        ]

[<Tests>]
let interfaceImplTests =
    testList
        "RecordInterfaceImpl"
        [
            // §14.6 slice 5: a record implementing a LOCAL interface whose impl reads
            // a field of `this`. The CLR backend emits the record's user
            // `interface … with` impl (the `InterfaceImpl` row + the impl body + the
            // interface-slot override) exactly like the union slice. `({ N = 7 } :>
            // IRank).Rank()` is a real interface dispatch through the record's vtable
            // slot — broken IL that type-checks would fault here.
            test "a record implementing a local interface dispatches through the interface slot (prints 7)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IRank ="
                            "    abstract member Rank : unit -> int"
                            "type R ="
                            "    { N: int }"
                            "    interface IRank with"
                            "        member this.Rank() = this.N"
                            "let r = { N = 7 }"
                            "printfn \"%d\" ((r :> IRank).Rank())"
                        ]

                let tast, artifact = compileSource "RecIfaceRank" src
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let bytes = Codegen.toBytes artifact

                // The user `InterfaceImpl` row lands on the record type (alongside any
                // synthesised structural eq interface).
                let asm = loadAssembly bytes
                let ty = asm.GetType "R"
                Expect.isNotNull ty "the assembly contains the record type R"
                let ifaceNames = ty.GetInterfaces() |> Array.map (fun i -> i.Name) |> Set.ofArray
                Expect.isTrue (ifaceNames.Contains "IRank") "R reflects as implementing the user IRank"

                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "7" "(r :> IRank).Rank() reads this.N (=7) via interface dispatch"
            }

            // The record ALSO synthesises an `IEquatable<R>` (structural equality), so
            // the user `IRank` impl and the synthesised eq interface must coexist on
            // the same record type with no slot collision: reflection sees BOTH
            // interfaces, and structural `=` and `(r :> IRank).Rank()` both work.
            test "a record's user interface coexists with its synthesised IEquatable (no slot collision)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IRank ="
                            "    abstract member Rank : unit -> int"
                            "type R ="
                            "    { N: int }"
                            "    interface IRank with"
                            "        member this.Rank() = this.N"
                            "let eq = ({ N = 7 } = { N = 7 })"
                            "let ne = ({ N = 7 } = { N = 3 })"
                            "printfn \"%b\" eq"
                            "printfn \"%b\" ne"
                            "printfn \"%d\" (({ N = 5 } :> IRank).Rank())"
                        ]

                let tast, artifact = compileSource "RecIfaceAndEq" src
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let bytes = Codegen.toBytes artifact
                let asm = loadAssembly bytes
                let ty = asm.GetType "R"
                Expect.isNotNull ty "the assembly contains the record type R"

                let ifaceNames = ty.GetInterfaces() |> Array.map (fun i -> i.Name) |> Set.ofArray
                Expect.isTrue (ifaceNames.Contains "IRank") "R reflects as implementing the user IRank"

                Expect.isTrue
                    (ifaceNames.Contains "IEquatable`1")
                    "R reflects as implementing the synthesised IEquatable<R>"

                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"

                let outLines =
                    output.Replace("\r", "").Split('\n') |> Array.filter (fun s -> s.Length > 0)

                Expect.equal
                    outLines
                    [| "true"; "false"; "5" |]
                    "structural `=` (synthesised IEquatable) and `Rank()` (user IRank) both dispatch correctly"
            }
        ]
