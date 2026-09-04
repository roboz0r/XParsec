module XParsec.FSharp.Codegen.Clr.Tests.RecordTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection

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

                let artifact = compileSource "RecLitFieldGet" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "7" "p.X returns the field value the literal stored"
            }

            test "a value boxes into an obj record field + unboxes back (prints 7)" {
                // `{ V = 7 }` into `V: obj` must emit a `box`, and `b.V :?> int` an
                // `unbox.any`. A missing box is invalid IL that fails to load.
                let src =
                    String.concat
                        "\n"
                        [
                            "type Box = { V: obj }"
                            "let b = { V = 7 }"
                            "let n = b.V :?> int"
                            "printfn \"%d\" n"
                        ]

                let artifact = compileSource "RecObjFieldBox" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "7" "the boxed int reads back as 7"
            }

            // A record is claimed by `(name, arity)`, so `Point`2` and `Point`3` are two
            // types under one source name; the field set picks which one a literal builds.
            test "two arity-overloaded records coexist; each constructs + field-reads (prints 20 / 39)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type Point<'X, 'Y> = { X: 'X; Y: 'Y }"
                            "type Point<'X, 'Y, 'Z> = { X: 'X; Y: 'Y; Z: 'Z }"
                            "let a = { X = 10; Y = 20 }"
                            "let b = { X = 1; Y = 2; Z = 39 }"
                            "printfn \"%d\" a.Y"
                            "printfn \"%d\" b.Z"
                        ]

                let artifact = compileSource "RecArityOverload" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "20\n39"
                    "a : Point`2 reads Y=20; b : Point`3 reads Z=39, so each resolved to the right arity"
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

                let artifact = compileSource "RecFieldSet" src
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

                let artifact = compileSource "RecClone" src
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

                let artifact = compileSource "RecPat" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "30" "TPat.Record bound both fields and summed them"
            }

            test "an emitted record type has a public ctor + one public property per record field" {
                let artifact =
                    compileSource
                        "RecMeta"
                        (String.concat "\n" [ "type Point = { X: int; Y: int }"; "let p = { X = 0; Y = 0 }" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Point"
                Expect.isNotNull ty "the assembly contains the record type Point"

                let ctors = ty.GetConstructors(BindingFlags.Public ||| BindingFlags.Instance)
                Expect.equal ctors.Length 1 "Point declares one public ctor"
                Expect.equal (ctors.[0].GetParameters().Length) 2 "Point ctor takes the two fields"

                let props = ty.GetProperties(BindingFlags.Public ||| BindingFlags.Instance)
                let names = props |> Array.map (fun p -> p.Name) |> Set.ofArray
                Expect.equal names (Set.ofList [ "X"; "Y" ]) "Point exposes both fields as properties"

                Expect.isEmpty
                    (ty.GetFields(BindingFlags.Public ||| BindingFlags.Instance))
                    "the storage behind them is private"
            }

            test "a record's bytes do not pin FSharp.Core (BCL-only equality + IL)" {
                let artifact =
                    compileSource
                        "RecNoDep"
                        (String.concat "\n" [ "type Point = { X: int; Y: int }"; "let p = { X = 0; Y = 0 }" ])

                expectNoFSharpCore artifact "record emission only references the BCL"
            }

            test "an all-immutable record emits the structural-equality triple + IEquatable<Self>" {
                let artifact =
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
                let artifact =
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

            test "a record with a mutable field keeps the structural equality triple, as fsc's does" {
                let artifact =
                    compileSource
                        "RecEqMut"
                        (String.concat "\n" [ "type Counter = { mutable Count: int }"; "let c = { Count = 0 }" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Counter"

                let equalsObj =
                    ty.GetMethod("Equals", declaredInstance, null, [| typeof<obj> |], null)

                Expect.isNotNull equalsObj "Equals(object) override on the record itself"

                Expect.isNotNull
                    (ty.GetMethod("GetHashCode", declaredInstance, null, [||], null))
                    "GetHashCode override on the record itself"

                let iface = typedefof<IEquatable<_>>.MakeGenericType ty
                Expect.isTrue (iface.IsAssignableFrom ty) "Counter declares IEquatable<Counter>"

                let mk (n: int) =
                    ty.GetConstructor([| typeof<int> |]).Invoke [| n |]

                Expect.isTrue (equalsObj.Invoke(mk 3, [| mk 3 |]) :?> bool) "equal fields ⇒ Equals(object) true"
                Expect.isFalse (equalsObj.Invoke(mk 3, [| mk 4 |]) :?> bool) "Count differs ⇒ Equals(object) false"
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
                let artifact =
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
                let artifact =
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
                let artifact =
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
                let artifact =
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

            // The `Box<'T>` tests above are all single-field, so nothing there reaches a
            // generic field at index >= 1, where the ctor's `stfld` needs its own ref.
            test "a multi-field generic record round-trips its non-first field (Pair<int>.Second = 3)" {
                let artifact =
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

                Expect.equal (pairInt.GetProperty("First").GetValue instance :?> int) 7 "Pair.First = 7 (first field)"

                Expect.equal
                    (pairInt.GetProperty("Second").GetValue instance :?> int)
                    3
                    "Pair.Second = 3 (non-first field)"
            }
        ]

[<Tests>]
let interfaceImplTests =
    testList
        "RecordInterfaceImpl"
        [
            // `(r :> IRank).Rank()` is a real vtable dispatch, so IL that type-checks but
            // wires the slot wrongly faults here rather than compiling clean.
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

                let artifact = compileSource "RecIfaceRank" src

                let bytes = Codegen.toBytes artifact

                let asm = loadAssembly bytes
                let ty = asm.GetType "R"
                Expect.isNotNull ty "the assembly contains the record type R"
                let ifaceNames = ty.GetInterfaces() |> Array.map (fun i -> i.Name) |> Set.ofArray
                Expect.isTrue (ifaceNames.Contains "IRank") "R reflects as implementing the user IRank"

                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "7" "(r :> IRank).Rank() reads this.N (=7) via interface dispatch"
            }

            // The record also synthesises `IEquatable<R>`, so the authored impl and the
            // synthesised one must land on the same type without colliding slots.
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

                let artifact = compileSource "RecIfaceAndEq" src

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

            // `interface seq<'T>` declares only `GetEnumerator() : enumerator<'T>`, so the
            // backend synthesises the slots `IEnumerable<'T>` inherits but the capability
            // does not declare: `IEnumerable.GetEnumerator`, `IEnumerator.Current`, `Reset`.
            test
                "a generic record implementing the seq capability iterates through the synthesised IEnumerable co-slots" {
                let src =
                    String.concat
                        "\n"
                        [
                            "open Vesper.Collections"
                            "[<Struct>]"
                            "type BagEnumerator<'T> ="
                            "    val Items : 'T[]"
                            "    val mutable Idx : int"
                            "    new(items: 'T[]) = { Items = items; Idx = -1 }"
                            "    interface enumerator<'T> with"
                            "        member this.Current : 'T = this.Items.[this.Idx]"
                            "        member this.MoveNext() : bool ="
                            "            this.Idx <- this.Idx + 1"
                            "            this.Idx < this.Items.Length"
                            "    interface Vesper.disposable with"
                            "        member this.Dispose() : unit = ()"
                            "type Bag<'T> ="
                            "    { Items: 'T[] }"
                            "    interface seq<'T> with"
                            "        member this.GetEnumerator() = (new BagEnumerator<'T>(this.Items) :> enumerator<'T>)"
                            "let sum (b: Bag<int>) : int ="
                            "    let e = (b :> seq<int>).GetEnumerator()"
                            "    let mutable total = 0"
                            "    while e.MoveNext() do"
                            "        total <- total + e.Current"
                            "    total"
                            "printfn \"%d\" (sum { Items = [| 1; 2; 3 |] })"
                        ]

                let artifact = compileSource "RecordSeqCapability" src

                let bytes = Codegen.toBytes artifact

                // The hand-driven pull, which is what a capability consumer writes; the
                // `for x in b` sugar over a record source is covered elsewhere.
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "6"
                    "the manual enumerator walk sums the record's elements"

                // The non-generic `IEnumerable` is made ENTIRELY of synthesised co-slots,
                // so walking it is a BCL consumer reaching a type it knows nothing about.
                let asm = loadAssembly bytes
                let bagTy = (asm.GetType "Bag`1").MakeGenericType typeof<int>
                let bag = Activator.CreateInstance(bagTy, [| box [| 1; 2; 3 |] |])

                let generic = bag :?> Collections.Generic.IEnumerable<int>
                Expect.sequenceEqual generic [ 1; 2; 3 ] "IEnumerable<int> (the authored member) yields the elements"

                let e = (bag :?> Collections.IEnumerable).GetEnumerator()

                let walked =
                    [
                        while e.MoveNext() do
                            yield e.Current
                    ]

                Expect.equal
                    walked
                    [ box 1; box 2; box 3 ]
                    "the non-generic IEnumerator co-slots yield the boxed elements"

                // The pull protocol has no rewind, so `Reset` exists only to fill the slot.
                Expect.throwsT<NotSupportedException> (fun () -> e.Reset()) "the synthesised Reset co-slot throws"
            }

            // `interface seq<'T>` already publishes `IEnumerable<'T>`, so authoring that
            // interface too would emit one slot twice and fail to load at the consumer.
            let collisionSrc (iface: string) (getEnumerator: string) =
                String.concat
                    "\n"
                    [
                        "open Vesper.Collections"
                        "type Bag<'T> ="
                        "    { Items: 'T[] }"
                        "    interface seq<'T> with"
                        "        member this.GetEnumerator() : enumerator<'T> = failwith \"x\""
                        sprintf "    interface %s with" iface
                        sprintf "        member this.GetEnumerator() : %s = failwith \"x\"" getEnumerator
                    ]

            let capabilityCollisionErrors (src: string) =
                let provider = ClrSymbolProviders.buildContract defaultPackages
                let lexed, file = parseFile src

                let _, tast =
                    Pipeline.analyseSemWithContextFor testCompiling provider (LexedFile.ofText lexed) file

                tast.Diagnostics |> Diagnostic.errors

            test "implementing the seq capability and its generic BCL interface is a diagnostic" {
                let errors =
                    capabilityCollisionErrors (
                        collisionSrc
                            "System.Collections.Generic.IEnumerable<'T>"
                            "System.Collections.Generic.IEnumerator<'T>"
                    )

                Expect.isNonEmpty errors "the capability's own platform interface collides"

                Expect.isTrue
                    (errors
                     |> List.exists (fun d -> d.Message.Contains "platform interface of capability"))
                    (sprintf "the diagnostic names the capability collision (%A)" errors)
            }

            test "implementing the seq capability and the non-generic IEnumerable is a diagnostic" {
                let errors =
                    capabilityCollisionErrors (
                        collisionSrc "System.Collections.IEnumerable" "System.Collections.IEnumerator"
                    )

                Expect.isNonEmpty errors "an interface the capability only INHERITS collides too"

                Expect.isTrue
                    (errors
                     |> List.exists (fun d -> d.Message.Contains "platform interface of capability"))
                    (sprintf "the diagnostic names the capability collision (%A)" errors)
            }

            // `comparable<'T>` publishes `System.IComparable<'T>`, which inherits nothing:
            // the NON-generic `System.IComparable` is a separate interface with its own
            // slot, and no co-slot synthesises it. The two names differ only by arity.
            test "implementing the comparable capability and the non-generic IComparable is allowed" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type Money ="
                            "    { Cents: int }"
                            "    interface Vesper.comparable<Money> with"
                            "        member this.CompareTo(that: Money) : int = this.Cents - that.Cents"
                            "    interface System.IComparable with"
                            "        member this.CompareTo(that: obj) : int = 0"
                        ]

                let errors = capabilityCollisionErrors src

                Expect.isEmpty errors (sprintf "the two arities are distinct interfaces (%A)" errors)
            }
        ]

// `v.Method()` / `v.Property` on a record object argument, as opposed to the
// `(r :> IFace).M()` coercion above.
[<Tests>]
let instanceMemberTests =
    testList
        "RecordInstanceMember"
        [
            test "a record instance method call resolves to the member (prints 7)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type Vec ="
                            "    { X: int; Y: int }"
                            "    member this.Sum () = this.X + this.Y"
                            "let v = { X = 3; Y = 4 }"
                            "printfn \"%d\" (v.Sum())"
                        ]

                let artifact = compileSource "RecInstMethod" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "7" "v.Sum() computes X+Y (=7) via the member body, not a field read"
            }

            test "a record instance method with an argument resolves + passes the arg (prints 17)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type Vec ="
                            "    { X: int; Y: int }"
                            "    member this.AddN (n: int) = this.X + this.Y + n"
                            "let v = { X = 3; Y = 4 }"
                            "printfn \"%d\" (v.AddN 10)"
                        ]

                let artifact = compileSource "RecInstMethodArg" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "17" "v.AddN 10 computes X+Y+n (=17)"
            }

            // `Doubled` is not a field name, and 6 is not a field value (3, 4), so a stray
            // field read cannot produce the expected output by accident.
            test "a record instance property resolves to the member, not a field (prints 6)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type Vec ="
                            "    { X: int; Y: int }"
                            "    member this.Doubled = this.X * 2"
                            "let v = { X = 3; Y = 4 }"
                            "printfn \"%d\" v.Doubled"
                        ]

                let artifact = compileSource "RecInstProperty" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "6" "v.Doubled reads the member (X*2=6), not a field"
            }

            // The field-vs-member decision is made per access, not per type.
            test "a field read and a member read coexist on one record (prints 3 then 6)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type Vec ="
                            "    { X: int; Y: int }"
                            "    member this.Doubled = this.X * 2"
                            "let v = { X = 3; Y = 4 }"
                            "printfn \"%d\" v.X"
                            "printfn \"%d\" v.Doubled"
                        ]

                let artifact = compileSource "RecFieldAndMember" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Trim().Replace("\r", ""))
                    "3\n6"
                    "v.X is a field read (3); v.Doubled is a member (6)"
            }
        ]
