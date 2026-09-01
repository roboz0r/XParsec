module XParsec.FSharp.Codegen.Clr.Tests.StructTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection

// `[<Struct>]` value-type emission, asserted by reflecting over the emitted PE.
// Each test's program is a standalone file under `data/`; a `//#include _x.fs`
// line in one splices in a shared fragment (`_layout-core.fs`, `_sink-*.fs`).

[<Tests>]
let structTests =
    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    testList
        "Struct"
        [
            test "a `[<Struct>]` record emits as a System.ValueType-based value type" {
                let artifact = compileSourceData "StructRecordShape"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "P"
                Expect.isNotNull ty "the assembly contains the struct record P"
                Expect.isTrue ty.IsValueType "P emits as a value type"
                Expect.isTrue ty.IsSealed "a value type is sealed"
                Expect.equal ty.BaseType typeof<System.ValueType> "P extends System.ValueType"

                let fields =
                    ty.GetFields(BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly)

                let names = fields |> Array.map (fun f -> f.Name) |> Set.ofArray
                Expect.equal names (Set.ofList [ "X"; "Y" ]) "both record fields are present"
            }

            // The seven lines are `a.X`, `a.Y`, `a = b`, `a = c`, `hash a = hash b`,
            // then `{ a with X = 10 }`'s two fields.
            test "a `[<Struct>]` record constructs, field-reads, and compares structurally" {
                runsDataLines [ "3"; "4"; "true"; "false"; "true"; "10"; "4" ] "StructRecordShape"
            }

            test "a `[<Struct>]` type emits as a System.ValueType-based value type" {
                let artifact = compileSourceData "StructShape"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "SPoint"
                Expect.isNotNull ty "the assembly contains the struct type SPoint"
                Expect.isTrue ty.IsValueType "SPoint emits as a value type"
                Expect.isTrue ty.IsSealed "a value type is sealed"

                // FSC emits ctor-param backing fields `assembly` on a struct too.
                let fields =
                    ty.GetFields(BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly)

                let names = fields |> Array.map (fun f -> f.Name) |> Set.ofArray
                Expect.equal names (Set.ofList [ "x"; "y" ]) "both ctor-param backing fields are present"
                Expect.isTrue (fields |> Array.forall (fun f -> f.IsAssembly)) "both are `assembly`-visible"
            }

            test "a struct ctor stores ctor params + a member reads one back (boxed dispatch)" {
                let artifact = compileSourceData "StructCtorRead"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Holder"
                Expect.isTrue ty.IsValueType "Holder is a value type"

                // Reflection boxes the struct, so this is a plain `callvirt` on the
                // box; the method reads its ctor-param field through the byref `this`.
                let boxed = Activator.CreateInstance(ty, [| box 7 |])
                let cmp = boxed :?> IComparable
                Expect.equal (cmp.CompareTo(null)) 7 "CompareTo returns the stored ctor-param field"
            }

            test "a struct `val mutable` field mutates through a boxed method and persists" {
                let artifact = compileSourceData "StructMutable"

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
                // `p.Sum()` on a `let`-bound struct needs the `this` *pointer*
                // (`ldloca` + `constrained. callvirt`), because a by-value `callvirt` on an
                // unboxed value type is invalid IL.
                let artifact = compileSourceData "StructUnboxedCall"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "SPoint"
                let sumOf = ty.GetMethod("SumOf", BindingFlags.Public ||| BindingFlags.Static)
                Expect.isNotNull sumOf "SumOf emitted as a static method"
                Expect.equal (sumOf.Invoke(null, [||]) :?> int) 7 "p.Sum() on an unboxed local returns 7"
            }

            test "a property get on an unboxed struct local dispatches by address" {
                let artifact = compileSourceData "StructUnboxedProp"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "SPoint"
                let xOf = ty.GetMethod("XOf", BindingFlags.Public ||| BindingFlags.Static)
                Expect.isNotNull xOf "XOf emitted as a static method"
                Expect.equal (xOf.Invoke(null, [||]) :?> int) 5 "p.X on an unboxed local returns 5"
            }

            test "a mutating method on an unboxed struct local persists (in-place addressing)" {
                // `this` must be `ldloca` of the slot itself: a spill-to-temp copy per
                // call would mutate a throwaway and `Get()` would read the original.
                let artifact = compileSourceData "StructUnboxedMutate"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Counter"
                let run = ty.GetMethod("Run", BindingFlags.Public ||| BindingFlags.Static)
                Expect.isNotNull run "Run emitted as a static method"
                Expect.equal (run.Invoke(null, [||]) :?> int) 2 "two Bump()s on the same local leave N = 2"
            }

            // `Counter()` lowers to `ldloca; initobj; ldloc` on a scratch local, not a
            // `newobj` against the synthesised parameterless `.ctor`.
            test "a parameterless struct construction zero-inits its fields via initobj" {
                let artifact = compileSourceData "StructInitObj"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Counter"
                let fresh = ty.GetMethod("Fresh", BindingFlags.Public ||| BindingFlags.Static)
                Expect.isNotNull fresh "Fresh emitted as a static method"
                Expect.equal (fresh.Invoke(null, [||]) :?> int) 0 "an initobj-constructed Counter has N = 0"
            }

            test "a struct upcast `:>` to an interface boxes (round-trips through the interface)" {
                let artifact = compileSourceData "StructUpcast"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Holder"

                let asCmp = ty.GetMethod("AsCmp", BindingFlags.Public ||| BindingFlags.Static)
                Expect.isNotNull asCmp "AsCmp emitted as a static method"

                let h = Activator.CreateInstance(ty, [| box 9 |])
                let cmp = asCmp.Invoke(null, [| h |]) :?> IComparable
                Expect.equal (cmp.CompareTo(null)) 9 "the boxed struct keeps its field value through `:>`"
            }

            test "`:> obj` boxes every value-type shape — unit, tuple and enum" {
                let artifact = compileSourceData "ValueUpcast"
                let bytes = Codegen.toBytes artifact

                for fn in [ "unitAsObj"; "tupleAsObj"; "enumAsObj" ] do
                    Expect.isTrue
                        (peMethodIl bytes "Program" fn |> Array.contains 0x8Cuy)
                        (sprintf "%s IL contains a `box` (0x8C)" fn)

                let program = (loadAssembly bytes).GetType "Program"

                // Each `describe` runs its boxed value through `ToString`, so a missing box is
                // invalid IL the JIT refuses here rather than a wrong result. Whether a `unit`
                // parameter survives into the ABI is not this test's claim, so the call is
                // built from the signature it finds.
                let describes (name: string) : string =
                    let m = program.GetMethod(name, BindingFlags.Public ||| BindingFlags.Static)
                    Expect.isNotNull m (sprintf "%s emitted as a static method" name)

                    let args =
                        m.GetParameters()
                        |> Array.map (fun p -> Activator.CreateInstance p.ParameterType)

                    m.Invoke(null, args) :?> string

                Expect.equal (describes "describeUnit") (ValueTuple().ToString()) "boxed `unit` is a ValueTuple"

                Expect.equal
                    (describes "describeTuple")
                    (ValueTuple<int, int>(3, 4).ToString())
                    "boxed tuple keeps 3, 4"

                Expect.equal (describes "describeEnum") "Red" "boxed enum keeps its case name"
            }

            test "`:?>` and `:? _ as _` on an open typar unbox at a value-type instantiation" {
                let artifact = compileSourceData "TyparDowncast"
                let bytes = Codegen.toBytes artifact

                // 0xA5 `unbox.any`, 0x74 `castclass`. A typar target takes `unbox.any`, which
                // IS `castclass` at a reference instantiation, so `castTo` needs no second arm.
                let il = peMethodIl bytes "Program" "castTo"
                Expect.isTrue (Array.contains 0xA5uy il) "castTo IL contains `unbox.any`"
                Expect.isFalse (Array.contains 0x74uy il) "castTo IL contains no `castclass`"

                let program = (loadAssembly bytes).GetType "Program"

                let invoke (name: string) (tyArg: Type) (args: obj[]) : obj =
                    let m = program.GetMethod(name, BindingFlags.Public ||| BindingFlags.Static)
                    Expect.isNotNull m (sprintf "%s emitted as a static method" name)
                    m.MakeGenericMethod([| tyArg |]).Invoke(null, args)

                // Under `castclass !!T` these return the boxed object's address read as an
                // `int`: no exception at emit, at JIT or at run time, just a wrong value.
                Expect.equal (invoke "castTo" typeof<int> [| box 5 |]) (box 5) "castTo<int> unboxes to 5"

                Expect.equal
                    (invoke "castTo" typeof<string> [| box "s" |])
                    (box "s")
                    "castTo<string> is a reference cast"

                Expect.equal
                    (invoke "orDefault" typeof<int> [| box 7; box 0 |])
                    (box 7)
                    "`:? 'T as v` binds the unboxed 7"

                Expect.equal
                    (invoke "orDefault" typeof<int> [| box "s"; box 0 |])
                    (box 0)
                    "a failed `:? 'T` test takes the fallback"
            }

            test "a two-parameter static member on a struct binds both args (SumOf(3,4) returns 7)" {
                let artifact = compileSourceData "StructStaticAdd2"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "SPoint"
                let sumOf = ty.GetMethod("SumOf", BindingFlags.Public ||| BindingFlags.Static)
                Expect.isNotNull sumOf "SumOf emitted as a static method"
                Expect.equal (sumOf.GetParameters().Length) 2 "SumOf has two scalar parameters (tuple flattened)"
                Expect.equal (sumOf.Invoke(null, [| box 3; box 4 |]) :?> int) 7 "SPoint(3,4).Sum() returns 7"
            }

            // A secondary ctor of the explicit field-init form `new(args) = { f = e; … }`.
            // Unlike a chain-form `new`, it stores directly into `val` fields (no primary-`.ctor` chain).
            test "a struct secondary ctor with an explicit field-init block initialises val fields" {
                let artifact = compileSourceData "StructFieldInit"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Pair"
                Expect.isTrue ty.IsValueType "Pair is a value type"

                let ctor = ty.GetConstructor [| typeof<int>; typeof<int> |]
                Expect.isNotNull ctor "the two-arg secondary ctor is emitted"

                let boxed = Activator.CreateInstance(ty, [| box 3; box 4 |])
                let fieldA = ty.GetField("A", BindingFlags.Public ||| BindingFlags.Instance)
                let fieldB = ty.GetField("B", BindingFlags.Public ||| BindingFlags.Instance)
                Expect.equal (fieldA.GetValue boxed :?> int) 3 "field A initialised from the first ctor param"
                Expect.equal (fieldB.GetValue boxed :?> int) 4 "field B initialised from the second ctor param"

                let sumOf = ty.GetMethod("SumOf", BindingFlags.Public ||| BindingFlags.Static)
                Expect.equal (sumOf.Invoke(null, [| box 3; box 4 |]) :?> int) 7 "Pair(3,4).Sum() returns 7"
            }

            test "a field-init ctor runs its let-preamble before storing fields" {
                // In `new(a) = let d = a + a in { A = a; B = d }` the let runs ahead of
                // the `stfld` stores, so `B` sees `d`.
                let artifact = compileSourceData "StructFieldInitLet"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "LetPair"

                let boxed = Activator.CreateInstance(ty, [| box 5 |])
                let fieldA = ty.GetField("A", BindingFlags.Public ||| BindingFlags.Instance)
                let fieldB = ty.GetField("B", BindingFlags.Public ||| BindingFlags.Instance)
                Expect.equal (fieldA.GetValue boxed :?> int) 5 "A = a"
                Expect.equal (fieldB.GetValue boxed :?> int) 10 "B = the let-bound a + a"
            }

            // An immutable `val x: T` (no `mutable`) emits as `InitOnly`.
            // `stfld` in a ctor is legal on InitOnly; writes elsewhere are forbidden.
            test "an immutable struct val field emits as InitOnly and is set by a field-init ctor" {
                let artifact = compileSourceData "StructInitOnly"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Ro"

                let fieldA = ty.GetField("A", BindingFlags.Public ||| BindingFlags.Instance)
                let fieldB = ty.GetField("B", BindingFlags.Public ||| BindingFlags.Instance)
                Expect.isTrue fieldA.IsInitOnly "an immutable val field is InitOnly"
                Expect.isFalse fieldB.IsInitOnly "a mutable val field stays writable"

                let boxed = Activator.CreateInstance(ty, [| box 3; box 4 |])
                Expect.equal (fieldA.GetValue boxed :?> int) 3 "InitOnly field initialised from the ctor"
                Expect.equal (fieldB.GetValue boxed :?> int) 4 "mutable field initialised from the ctor"
            }

            test "a field-init ctor mixes a param-sourced field and a bool-literal field" {
                let artifact = compileSourceData "StructFieldInitMixed"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Iter"

                let boxed = Activator.CreateInstance(ty, [| box 11 |])
                let fieldCur = ty.GetField("Cur", BindingFlags.Public ||| BindingFlags.Instance)

                let fieldStarted =
                    ty.GetField("Started", BindingFlags.Public ||| BindingFlags.Instance)

                Expect.equal (fieldCur.GetValue boxed :?> int) 11 "Cur initialised from the ctor param"
                Expect.equal (fieldStarted.GetValue boxed :?> bool) false "Started initialised from the bool literal"
            }

            // A generic value type's self-`TypeSpec` (base type, ctor field `MemberRef`s,
            // signature encoding) must carry the VALUETYPE tag throughout.
            test "a generic `[<Struct>]` type emits as a generic value type" {
                let artifact = compileSourceData "GenericStructShape"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Box`1"
                Expect.isNotNull ty "the assembly contains the generic struct type Box`1"
                Expect.isTrue ty.IsValueType "Box`1 emits as a value type"
                Expect.isTrue ty.IsSealed "a value type is sealed"
                Expect.isTrue ty.IsGenericTypeDefinition "Box`1 is a generic type definition"

                let inst = ty.MakeGenericType [| typeof<int> |]
                let boxed = Activator.CreateInstance(inst, [| box 42 |])

                let valField =
                    inst.GetField("value", BindingFlags.NonPublic ||| BindingFlags.Instance)

                Expect.equal (valField.GetValue boxed :?> int) 42 "the ctor-param field stores the generic value"
            }

            test "a generic struct dispatches a member that reads a generic ctor-param field (boxed)" {
                // The ctor stores into the open `Box\`1<!0>::value` field and `Get()`
                // reads it back, so the self-`TypeSpec` must be VALUETYPE-tagged.
                let artifact = compileSourceData "GenericStructMember"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let inst = (asm.GetType "Box`1").MakeGenericType [| typeof<int> |]

                let boxed = Activator.CreateInstance(inst, [| box 99 |])
                let get = inst.GetMethod("Get", declaredInstance, null, [||], null)
                Expect.equal (get.Invoke(boxed, [||]) :?> int) 99 "Get() reads the generic ctor-param field"
            }

            test "a generic struct with a val field + field-init ctor round-trips boxed to an interface" {
                let artifact = compileSourceData "GenericStructIter"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let openTy = asm.GetType "Cell`1"
                Expect.isTrue openTy.IsValueType "Cell`1 is a value type"
                let ty = openTy.MakeGenericType [| typeof<int> |]

                let ctor =
                    openTy.GetConstructors() |> Array.find (fun c -> c.GetParameters().Length = 1)

                Expect.isNotNull ctor "the one-arg field-init secondary ctor is emitted"

                let boxed = Activator.CreateInstance(ty, [| box 7 |])
                let itemFld = ty.GetField("Item", BindingFlags.Public ||| BindingFlags.Instance)

                let startedFld =
                    ty.GetField("Started", BindingFlags.Public ||| BindingFlags.Instance)

                Expect.equal (itemFld.GetValue boxed :?> int) 7 "Item initialised from the generic ctor param"
                Expect.equal (startedFld.GetValue boxed :?> bool) false "Started initialised from the bool literal"

                let get = ty.GetMethod("Get", declaredInstance, null, [||], null)
                Expect.equal (get.Invoke(boxed, [||]) :?> int) 7 "Get() reads the val field back"
            }

            test "a generic struct enumerator implements the three IEnumerator interfaces and advances boxed" {
                let artifact = compileSourceData "StructEnumerator"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let openTy = asm.GetType "OnceEnum`1"
                Expect.isTrue openTy.IsValueType "OnceEnum`1 is a value type"
                let ty = openTy.MakeGenericType [| typeof<int> |]

                // `MoveNext`'s mutation goes through the byref `this` into the box, so
                // it survives across calls and the second `MoveNext` sees the end.
                let boxed = Activator.CreateInstance(ty, [| box 42 |])
                let e = boxed :?> System.Collections.Generic.IEnumerator<int>
                Expect.isTrue (e.MoveNext()) "first MoveNext starts the single-element enumeration"
                Expect.equal e.Current 42 "Current yields the ctor-stored item through IEnumerator<int>"
                Expect.isFalse (e.MoveNext()) "second MoveNext reports the end (Started mutation persisted)"

                // The non-generic `IEnumerator.Current` boxes the item.
                let boxed2 = Activator.CreateInstance(ty, [| box 7 |])
                let ng = boxed2 :?> System.Collections.IEnumerator
                Expect.isTrue (ng.MoveNext()) "non-generic MoveNext advances"
                Expect.equal (ng.Current :?> int) 7 "non-generic Current boxes the item"
            }

            test "a class GetEnumerator constructs a struct enumerator and returns it boxed (yields the element)" {
                let artifact = compileSourceData "StructEnumeratorSeq"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let seqTy = (asm.GetType "OnceSeq`1").MakeGenericType [| typeof<int> |]

                let s =
                    Activator.CreateInstance(seqTy, [| box 99 |]) :?> System.Collections.Generic.IEnumerable<int>

                Expect.equal
                    (s |> Seq.toList)
                    [ 99 ]
                    "enumerating the seq via its struct enumerator yields the single element"
            }

            // At `OnceEnum<'T>(x)` the secondary ctor's type args ground from both the
            // explicit `<'T>` and the value arg, rather than leaking as a free `TyVar`.
            test "a class GetEnumerator constructs the struct enumerator with explicit type args (Set<'T> shape)" {
                let artifact = compileSourceData "StructEnumeratorTypeApp"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let seqTy = (asm.GetType "OnceSeq`1").MakeGenericType [| typeof<int> |]

                let s =
                    Activator.CreateInstance(seqTy, [| box 5 |]) :?> System.Collections.Generic.IEnumerable<int>

                Expect.equal (s |> Seq.toList) [ 5 ] "explicit-type-app construction enumerates to the single element"
            }

            test "a struct declared in a referenced package encodes as VALUETYPE in a consumer signature" {
                // The package directory name IS the package identity that `depends-on`
                // resolves against, so it must equal the manifest `name` below.
                let outDir = tmpDir "Vesper.PointPkg"
                let manifestPath = System.IO.Path.Combine(outDir, "manifest.clr.toml")

                System.IO.File.WriteAllText(
                    manifestPath,
                    "[core]\nname = \"Vesper.PointPkg\"\ndepends-on = []\nfiles = [\"point.fsi\", \"point.fs\"]\n"
                )

                // The `.fsi` and its companion spell the struct identically, so the pair
                // conforms; a listed `.fsi` owes a companion either way.
                let point =
                    "namespace Vesper\n\ntype Point =\n    struct\n        val X: int\n        val Y: int\n    end\n"

                System.IO.File.WriteAllText(System.IO.Path.Combine(outDir, "point.fsi"), point)
                System.IO.File.WriteAllText(System.IO.Path.Combine(outDir, "point.fs"), point)

                let provider = ClrSymbolProviders.buildContract [ vesperCorePackage; outDir ]

                // Identity function forces `Point` into the emitted signature (return + param).
                let src =
                    "namespace App\n\nopen Vesper\n\nmodule Consumer =\n    let echo (p: Point) : Point = p\n"

                let project = ProjectInfo.library "StructXPkgConsumer"
                let artifact = compileAgainst provider project src

                let bytes = Codegen.toBytes artifact

                let declType =
                    match peMethodNames bytes |> List.filter (fun (_, m) -> m = "echo") with
                    | (t, _) :: _ -> t
                    | [] -> failwithf "no `echo` method emitted; methods: %A" (peMethodNames bytes)

                let elem = peMethodReturnElementType bytes declType "echo"

                // ELEMENT_TYPE_VALUETYPE = 0x11, ELEMENT_TYPE_CLASS = 0x12.
                Expect.notEqual elem 0x12uy "the referenced struct must NOT encode as ELEMENT_TYPE_CLASS"
                Expect.equal elem 0x11uy "the referenced struct encodes as ELEMENT_TYPE_VALUETYPE"
            }

            // The `=` here sits inside an `interface … with member …` body, so desugar
            // has to walk interface member bodies, not only the type's own members.
            test "an infix operator inside a struct interface member resolves (SetIterator.MoveNext shape)" {
                let artifact = compileSourceData "StructIfaceInfix"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Iter"
                Expect.isTrue ty.IsValueType "Iter is a value type"

                // `Iter(Node 1)` seeds a one-element stack, so the first `MoveNext`
                // takes the `t.Height = 1` branch and pops it; the second sees `[]`.
                let nodeTy = asm.GetType "Node"
                let node = Activator.CreateInstance(nodeTy, [| box 1 |])
                let boxed = Activator.CreateInstance(ty, [| node |])
                let e = boxed :?> System.Collections.IEnumerator
                Expect.isTrue (e.MoveNext()) "first MoveNext takes the `t.Height = 1` true branch"
                Expect.isFalse (e.MoveNext()) "second MoveNext hits the empty-stack arm"
            }

            // In `this.Stack.IsEmpty` the intermediate step `Stack` is a `val` field,
            // not a member, so the object argument for `IsEmpty` types as `int list`.
            test "a chained property on a struct val field types the object argument as the field, not the property" {
                let artifact = compileSourceData "StructFieldChainProp"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Wrap"
                Expect.isTrue ty.IsValueType "Wrap is a value type"

                let notEmpty = ty.GetMethod("NotEmpty", declaredInstance, null, [||], null)

                // `Wrap(0)` seeds `[]` ⇒ `this.Stack.IsEmpty` is true ⇒ `NotEmpty()` false.
                let emptyWrap = Activator.CreateInstance(ty, [| box 0 |])
                Expect.isFalse (notEmpty.Invoke(emptyWrap, [||]) :?> bool) "empty stack ⇒ NotEmpty() is false"

                // `Wrap(5)` seeds `5 :: []` ⇒ `IsEmpty` false ⇒ `NotEmpty()` true.
                let fullWrap = Activator.CreateInstance(ty, [| box 5 |])
                Expect.isTrue (notEmpty.Invoke(fullWrap, [||]) :?> bool) "non-empty stack ⇒ NotEmpty() is true"
            }

            // `[<Struct; IsByRefLike>]` emits the `IsByRefLikeAttribute` marker so the
            // CLR confines the type to the stack.
            test "a `[<Struct; IsByRefLike>]` type emits a byref-like value type" {
                let artifact = compileSourceData "RefStructShape"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "RPoint"
                Expect.isNotNull ty "the assembly contains the type RPoint"
                Expect.isTrue ty.IsValueType "RPoint emits as a value type"
                Expect.isTrue ty.IsByRefLike "RPoint is byref-like (ref struct)"
            }

            // The marker is opt-in: `IsByRefLikeAttribute` must not leak onto plain `[<Struct>]` types.
            test "a plain `[<Struct>]` type is not byref-like" {
                let artifact = compileSourceData "PlainStructNotRefLike"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "NPoint"
                Expect.isTrue ty.IsValueType "NPoint is a value type"
                Expect.isFalse ty.IsByRefLike "a plain [<Struct>] is not byref-like"
            }

            // `Span<char>` is an external generic value type: its ctor/member refs tag
            // `VALUETYPE`, and dispatch is `ldloca` + non-virtual `call` (a by-value
            // `callvirt` on a ref struct is verifier-illegal).
            test "ref struct with a Span<char> field: ctor, Length, Slice round-trip" {
                runsDataLines [ "5"; "3" ] "ref-struct-span-field"
            }

            // `Span<T>`'s only element accessor is `get_Item(i) : T&`, so `chars.[i]`
            // must encode `ELEMENT_TYPE_BYREF` in the member-ref and `ldobj` the result.
            test "`chars.[i]` — a byref indexer read on a ref struct's Span<char>" {
                runsDataLines [ "e"; "o" ] "ref-struct-span-byref-indexer"
            }

            // A byref-like value cannot box to `IDisposable`, so `use` accepts a pattern
            // `Dispose()` on it (C#8 pattern-`using` parity) and calls it directly. Analysis
            // accepts the form; `EmitBindings.buildUse` has no value-type arm yet.
            ptest
                "`use` over a `[<IsByRefLike>]` ref struct calls its pattern `Dispose` — emit gap: `use` over a value type" {
                runsDataLines [ "body"; "disposed" ] "ref-struct-use-pattern-dispose"
            }

            // The call omits `Return`'s optional trailing `clearArray = false`; that
            // constant is synthesised, so codegen still sees a full two-arg call.
            test "ArrayPool<char>.Shared Rent + Return (omitted optional arg)" {
                runsDataLines [ "ok" ] "arraypool-rent-return-optional-arg"
            }

            // The `null` literal pattern binds nothing and lowers to `ldloc; brtrue`
            // past the arm, so only a null scrutinee reaches the body.
            test "a `null` literal pattern matches a null reference, binds nothing" {
                runsDataLines [ "null"; "value" ] "null-literal-pattern"
            }

            // `Span<char>` passed as a by-value argument: the member-ref parent for a
            // Span parameter must encode `VALUETYPE`, not just the object argument/field/return.
            test "Span<char> by-value args: string.CopyTo, span CopyTo, Fill, ToString, TryCopyTo" {
                runsDataLines [ "ab---cd"; "ab---cdab"; "true"; "false" ] "span-byval-args"
            }

            test "a struct field block declares + initialises (static let, nullable refs, Span)" {
                runsDataLines [ "true"; "5"; "5"; "false"; "-1"; "true" ] "struct-field-block-init"
            }

            test "hex uint32 literal reads back; radix literals" {
                runsDataLines [ "true"; "true"; "true"; "true" ] "hex-radix-literals"
            }

            test "uint cast feeding Math.Max/Min/Clamp" { runsDataLines [ "true" ] "uint-cast-math-clamp" }

            // One project-local generic member, called at int / float / ref in the same
            // assembly: its method typars freshen per call site rather than grounding
            // to the first call's type.
            test "generic member IFormattable dispatch (int/float/ref), same-assembly multi-instantiation" {
                runsDataLines [ "42"; "3.14"; "hi" ] "generic-member-multi-instantiation"
            }

            // The same freshening at module scope: a capture-free generic function
            // called at two types stays generic.
            test "generic free function, same-assembly multi-instantiation" {
                runsDataLines [ "42"; "3.14" ] "generic-free-fn-multi-instantiation"
            }

            // `&r` lowers to `ldloca` of the slot, with `ELEMENT_TYPE_BYREF` on the
            // member-ref parameter. `r` has to be a function-local mutable, because a
            // module-level mutable is a static field, not a slot-addressable local.
            test "Int32.TryParse(s, &r) writes the out local through a byref arg" {
                runsDataLines [ "123"; "-1" ] "int-tryparse-byref-out"
            }

            // `TryFormat` writes chars straight into a `Span<char>` and reports the
            // count through `&cw`. `int`/`float` take it; `string` has no
            // `ISpanFormattable`, so `"hi"` comes from the `o.ToString()` arm.
            test "AppendFormatted span fast-path via ISpanFormattable.TryFormat" {
                runsDataLines [ "42"; "3.14"; "hi" ] "spanformattable-tryformat"
            }

            // A five-deep self-call chain (`AppendFormatted` → `AppendLiteral` →
            // `GrowThenCopyString` → `Grow` → `GrowCore`), each body mutating `this`:
            // the mutations persist only if self-calls address `this` in place.
            test "struct formatter core: literal, generic hole, grow, string sink" {
                runsDataLines [ "x=42, pi=3.14"; "400" ] "formatter-core-selfcall-grow"
            }

            // Three same-name `G` overloads, each with its own method typar `'T`.
            // Matching by name alone would share the first overload's typars and
            // freeze the others' `'T` as `?unresolved-typar`.
            test "same-name overloaded generic members each generalise their own 'T" {
                runsDataLines [ "42" ] "overloaded-generic-members-own-typar"
            }

            // The parser folds the dot, so `w.Write("hi")` arrives as
            // `fn = LongIdent [w; Write]`, but it still has to pick the best overload by
            // argument type, not take the first/widest one the field walk finds.
            test "external instance overload pick on a folded-LongIdent object argument" {
                runsDataLines [ "hi" ] "external-instance-overload-folded-longident"
            }

            // Binary `+` on `string` must lower to `String.Concat`, not a numeric
            // `add` on two string references (a garbage pointer → AccessViolation).
            test "binary + on string concatenates (String.Concat), not numeric add" {
                runsDataLines [ "xy" ] "string-plus-concat"
            }

            // An external value type with 0 ctor args lowers to `initobj`, not `newobj`.
            test "parameterless Span<char>() (external value-type default ctor)" {
                runsDataLines [ "0" ] "external-valuetype-default-ctor"
            }

            test "int conversion of a uint (Math.Clamp narrowing)" { runsDataLines [ "50" ] "int-of-uint-clamp" }

            // `for i in a..b do` over an integer range lowers to a counted `ForTo`
            // loop, not a range enumerable walk.
            test "for i in 1..n range loop (lowers to counted ForTo)" { runsDataLines [ "15" ] "for-in-range-counted" }

            // `a + b + c` is `(a + b) + c`, and the outer `+`'s `^T` grounds from a
            // sibling ground operand (`c : string`, or the `string` return position),
            // not from the inner App's still-abstract result type.
            test "Chained string concat a + b + c lowers to String.Concat (not numeric add)" {
                runsDataLines [ "abc"; "(x)" ] "chained-string-concat"
            }

            // `"\t"` reaches the emitted value as one tab char, not the raw 2-char span.
            test "string literal escape sequences decode in the emitted value" {
                runsDataLines [ "a"; "b"; "x\ty"; "q\"r" ] "string-escape-sequences"
            }

            // A group is all-flat iff `col + flatWidth inner <= width` (`width = 0` ⇒
            // always flat), never half-broken. The fragment's `L`-prefixed cases
            // (`LDoc`/`LText`/…) dodge the `Vesper.Doc`/`DocGroup` names in scope.
            test "Doc layout core: flatWidth + Render (flat / never-break / broken)" {
                runsDataLines
                    [
                        "[1; 2; 3]" // width 80: fits ⇒ all-flat
                        "[1; 2; 3]" // width 0: never-break ⇒ all-flat
                        "[" // width 5: group broken ⇒ all soft breaks become newlines
                        "  1;"
                        "  2;"
                        "  3"
                        "]"
                        "(Some 1)" // a parens group (DU application) renders its `(`/`)`
                    ]
                    "doc-layout-core"
            }

            // In `LCat [ a; b; c ]` only round-paren / begin-end grouping is peeled off
            // the case argument; unwrapping the `[ … ]` block would drop the list.
            test "list literal as a direct union-case argument round-trips" {
                runsDataLines [ "3"; "6" ] "list-literal-union-case-arg"
            }

            // `S` has three `G` overloads adding 100 / 10 / 1; `U` self-calls
            // `this.G(y, 0)`, so picking by argument type (not by name) is what makes
            // two `U` calls total 2 rather than 200 or 20.
            test "overloaded struct self-call resolves the right overload by arg types" {
                runsDataLines [ "2" ] "overloaded-struct-selfcall"
            }

            test "%u / aligned / zero-padded-float holes match the structural spec" {
                runsEq "42" "printfn \"%u\" 42"
                runsEq "   42" "printfn \"%5u\" 42"
                runsEq "00003.14" "printfn \"%08.2f\" 3.14"
                runsEq "42   ,7" "printfn \"%-5u,%d\" 42 7"
            }

            // A union case drives the `BeginCase`/`Child`/`EndCase` sink protocol; the
            // sink parenthesises a single payload only when the child is itself
            // application-shaped: `Some (Some 1)` gets parens, a bare `Some 1` does not.
            test "a class IFormatSink sink + frame stack renders lists and nested cases" {
                runsDataLines
                    [
                        "[1; 2; 3]" // width 80: the list group fits ⇒ flat
                        "[" // width 5: the group breaks ⇒ soft breaks become newlines
                        "  1;"
                        "  2;"
                        "  3"
                        "]"
                        "Some 1" // top-level application: not parenthesised
                        "Some (Some 1)" // the inner Some is in arg position ⇒ parens
                    ]
                    "format-sink-frame-stack"
            }

            test "structural-format probe: numeric type-tests + suffix render" {
                runsDataLines [ "5y"; "5uy"; "5s"; "5us"; "5u"; "5L"; "5UL"; "5M" ] "structural-format-numeric-suffixes"
            }

            test "structural-format probe: float fixup + string/char quoting" {
                runsDataLines
                    [
                        "3.0"
                        "3.5"
                        "nan"
                        "3.0f"
                        "nanf"
                        "infinityf"
                        "\"a\\\"b\\nc\""
                        "'c'"
                        "'\\n'"
                    ]
                    "structural-format-float-quote"
            }

            test "structural-format probe: ITuple + IEnumerable walk" {
                runsDataLines [ "(1, 2)"; "[1; 2; 3]" ] "structural-format-tuple-enum"
            }

            // One value per arm of the reflection-free `:?` dispatch chain; the final
            // `[1; 2; ...]` is the size budget cutting the list short.
            test "structural %A engine: Dispatch + atom rendering (depth+size budget)" {
                runsDataLines
                    [
                        "42"
                        "true"
                        "3.0"
                        "\"hi\""
                        "'c'"
                        "5L"
                        "1.5M"
                        "(1, \"a\")"
                        "[1; 2; 3]"
                        "Some (Some 1)"
                        "[1; 2; ...]"
                    ]
                    "structural-format-dispatch-engine"
            }

            // `new() = { X = 42; Y = 7 }` is the only ctor, so no empty primary
            // `.ctor()` is synthesised beside it; otherwise a second identical row would leave
            // construction binding the empty one and both fields at 0.
            test "a val-field class with only a parameterless new() initialises its fields" {
                runsDataLines [ "42"; "7" ] "valfield-parameterless-ctor"
            }

            test "a `[<Struct>]` union emits as a readonly System.ValueType-based value type" {
                let artifact = compileSourceData "StructUnionShape"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Shape"
                Expect.isNotNull ty "the assembly contains the struct union Shape"
                Expect.isTrue ty.IsValueType "Shape emits as a value type"
                Expect.isTrue ty.IsSealed "a value type is sealed"
                Expect.equal ty.BaseType typeof<System.ValueType> "Shape extends System.ValueType"

                let isReadOnly =
                    ty.GetCustomAttributesData()
                    |> Seq.exists (fun a ->
                        a.AttributeType.FullName = "System.Runtime.CompilerServices.IsReadOnlyAttribute"
                    )

                Expect.isTrue isReadOnly "Shape carries IsReadOnlyAttribute"

                let ctors =
                    ty.GetConstructors(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance)

                Expect.equal ctors.Length 1 "a struct union declares one flat .ctor"
                Expect.equal (ctors.[0].GetParameters().Length) 4 "tag + Point_0 + Pair_0 + Pair_1"

                // `_tag` is private, so the payload and the discriminant only come out
                // together under `NonPublic`.
                let fields =
                    ty.GetFields(
                        BindingFlags.Public
                        ||| BindingFlags.NonPublic
                        ||| BindingFlags.Instance
                        ||| BindingFlags.DeclaredOnly
                    )

                for f in fields do
                    Expect.isTrue f.IsInitOnly (f.Name + " is initonly")

                let names = fields |> Array.map (fun f -> f.Name) |> Set.ofArray

                Expect.equal
                    names
                    (Set.ofList [ "_tag"; "Point_0"; "Pair_0"; "Pair_1" ])
                    "the flat per-(case, index) field set plus the tag"
            }

            // One public instance `Get_<Case>_<i>` reader per logical case field is the
            // union's cross-assembly payload ABI; a nullary case declares none.
            test "a `[<Struct>]` union declares one public Get_<Case>_<i> reader per case field" {
                let artifact = compileSourceData "StructUnionShape"
                let bytes = Codegen.toBytes artifact

                MetadataStructure.assertWellFormed "StructUnionGetters" bytes

                let methods = MetadataStructure.methodAttrsOf bytes "Shape"
                let getters = methods |> List.filter (fun (n, _) -> n.StartsWith "Get_")

                Expect.equal
                    (List.map fst getters)
                    [ "Get_Point_0"; "Get_Pair_0"; "Get_Pair_1" ]
                    "one getter per (case, field), in case then field order"

                for (name, attrs) in getters do
                    Expect.equal
                        (attrs &&& MethodAttributes.MemberAccessMask)
                        MethodAttributes.Public
                        (name + " is public")

                    Expect.isFalse (attrs.HasFlag MethodAttributes.Static) (name + " is an instance method")
                    Expect.isFalse (attrs.HasFlag MethodAttributes.Virtual) (name + " binds by call")

                let asm = loadAssembly bytes
                let ty = asm.GetType "Shape"
                let pair = ty.GetMethod("Pair").Invoke(null, [| box 4; box 5 |])

                Expect.equal (ty.GetMethod("Get_Pair_0").Invoke(pair, [||])) (box 4) "Get_Pair_0 reads a"
                Expect.equal (ty.GetMethod("Get_Pair_1").Invoke(pair, [||])) (box 5) "Get_Pair_1 reads b"
            }

            // The last two lines are the `default` semantics: the zero value is the
            // tag-0 case (`Empty`), reachable by pattern matching and equal to it.
            test "a `[<Struct>]` union constructs, matches, equates; its zero value is the tag-0 case" {
                runsDataLines [ "0"; "3"; "9"; "true"; "false"; "0"; "true" ] "StructUnionShape"
            }

            test "a generic `[<Struct>]` union emits as a generic value type" {
                let artifact = compileSourceData "StructUnionGenericShape"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "GBox`1"
                Expect.isNotNull ty "the assembly contains the generic struct union GBox`1"
                Expect.isTrue ty.IsValueType "GBox`1 emits as a value type"
                Expect.isTrue ty.IsSealed "a value type is sealed"
                Expect.isTrue ty.IsGenericTypeDefinition "GBox`1 is a generic type definition"

                let isReadOnly =
                    ty.GetCustomAttributesData()
                    |> Seq.exists (fun a ->
                        a.AttributeType.FullName = "System.Runtime.CompilerServices.IsReadOnlyAttribute"
                    )

                Expect.isTrue isReadOnly "GBox`1 carries IsReadOnlyAttribute"
            }

            test "a generic `[<Struct>]` union constructs, matches and equates at int" {
                runsDataLines [ "3"; "40"; "true"; "false" ] "StructUnionGenericShape"
            }

            test "same-name different-type case fields are representable (FS3585 relaxed)" {
                runsDataLines [ "5"; "hello"; "true"; "false" ] "StructUnionSameNameFields"
            }
        ]
