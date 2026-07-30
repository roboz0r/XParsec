module XParsec.FSharp.Codegen.Clr.Tests.StructTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `[<Struct>]` value-type emission. These tests reflect over the emitted PE so
// a runtime fault (bad IL, wrong base type, lost mutation) surfaces through
// `loadAssembly` / `Activator.CreateInstance`.
//
// Each test's to-be-compiled program lives as a standalone file under `data/`,
// read + compiled at test time via `compileSourceData` / `runsDataLines` /
// `runsSelfHostDataLines` (see `TestHelpers`). The self-hosted `%A` layout /
// structural-format probes single-source their layout / frame / protocol fragments
// through `//#include` directives in those data files, so a sink-protocol change
// edits one fragment (`data/_sink-*.fs`) rather than several hand-copied program blobs.

[<Tests>]
let structTests =
    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    testList
        "Struct"
        [
            // A `[<Struct>]` RECORD routes through the same value-type machinery as a
            // struct class: `System.ValueType` base, sealed, `RegisterUserValueType`.
            // Before the fix a struct record emitted as an ordinary reference type
            // (record base was `Object`, no `FTRecord` value-type recognition).
            test "a `[<Struct>]` record emits as a System.ValueType-based value type" {
                let _, artifact = compileSourceData "StructRecordShape"

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

            // End-to-end: the struct record constructs, reads its fields, and its
            // synthesised value-type equality triple (`Equals(object)` via unbox,
            // typed `Equals(Self)` by value, `GetHashCode`) + `{ r with … }` all run.
            test "a `[<Struct>]` record constructs, field-reads, and compares structurally" {
                runsDataLines [ "3"; "4"; "true"; "false"; "true"; "10"; "4" ] "StructRecordShape"
            }

            test "a `[<Struct>]` type emits as a System.ValueType-based value type" {
                let _, artifact = compileSourceData "StructShape"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "SPoint"
                Expect.isNotNull ty "the assembly contains the struct type SPoint"
                Expect.isTrue ty.IsValueType "SPoint emits as a value type"
                Expect.isTrue ty.IsSealed "a value type is sealed"

                // Ctor-param backing fields are compiler-generated storage: `assembly`,
                // as FSC emits them for a struct too.
                let fields =
                    ty.GetFields(BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly)

                let names = fields |> Array.map (fun f -> f.Name) |> Set.ofArray
                Expect.equal names (Set.ofList [ "x"; "y" ]) "both ctor-param backing fields are present"
                Expect.isTrue (fields |> Array.forall (fun f -> f.IsAssembly)) "both are `assembly`-visible"
            }

            test "a struct ctor stores ctor params + a member reads one back (boxed dispatch)" {
                let _, artifact = compileSourceData "StructCtorRead"

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
                let _, artifact = compileSourceData "StructMutable"

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
                // `p.Sum()` on a `let`-bound struct value needs
                // the receiver *address* (`ldloca` + `constrained. callvirt`), not a
                // by-value `callvirt` (invalid IL on an unboxed value type).
                let _, artifact = compileSourceData "StructUnboxedCall"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "SPoint"
                let sumOf = ty.GetMethod("SumOf", BindingFlags.Public ||| BindingFlags.Static)
                Expect.isNotNull sumOf "SumOf emitted as a static method"
                Expect.equal (sumOf.Invoke(null, [||]) :?> int) 7 "p.Sum() on an unboxed local returns 7"
            }

            test "a property get on an unboxed struct local dispatches by address" {
                let _, artifact = compileSourceData "StructUnboxedProp"

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
                let _, artifact = compileSourceData "StructUnboxedMutate"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Counter"
                let run = ty.GetMethod("Run", BindingFlags.Public ||| BindingFlags.Static)
                Expect.isNotNull run "Run emitted as a static method"
                Expect.equal (run.Invoke(null, [||]) :?> int) 2 "two Bump()s on the same local leave N = 2"
            }

            // A parameterless struct construction lowers to `ldloca; initobj; ldloc`
            // on a scratch local, not a `newobj` against the synthesised parameterless
            // `.ctor`. The field reads back as its zero-init default.
            test "a parameterless struct construction zero-inits its fields via initobj" {
                let _, artifact = compileSourceData "StructInitObj"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Counter"
                let fresh = ty.GetMethod("Fresh", BindingFlags.Public ||| BindingFlags.Static)
                Expect.isNotNull fresh "Fresh emitted as a static method"
                Expect.equal (fresh.Invoke(null, [||]) :?> int) 0 "an initobj-constructed Counter has N = 0"
            }

            test "a struct upcast `:>` to an interface boxes (round-trips through the interface)" {
                let _, artifact = compileSourceData "StructUpcast"

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

            test "a two-parameter static member on a struct binds both args (SumOf(3,4) returns 7)" {
                let _, artifact = compileSourceData "StructStaticAdd2"

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
                let _, artifact = compileSourceData "StructFieldInit"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Pair"
                Expect.isTrue ty.IsValueType "Pair is a value type"

                // The two-arg secondary ctor must exist alongside the (empty) primary.
                let ctor = ty.GetConstructor [| typeof<int>; typeof<int> |]
                Expect.isNotNull ctor "the two-arg secondary ctor is emitted"

                // Construct directly: the field-init block stored both args.
                let boxed = Activator.CreateInstance(ty, [| box 3; box 4 |])
                let fieldA = ty.GetField("A", BindingFlags.Public ||| BindingFlags.Instance)
                let fieldB = ty.GetField("B", BindingFlags.Public ||| BindingFlags.Instance)
                Expect.equal (fieldA.GetValue boxed :?> int) 3 "field A initialised from the first ctor param"
                Expect.equal (fieldB.GetValue boxed :?> int) 4 "field B initialised from the second ctor param"

                // And through a member that constructs + reads back.
                let sumOf = ty.GetMethod("SumOf", BindingFlags.Public ||| BindingFlags.Static)
                Expect.equal (sumOf.Invoke(null, [| box 3; box 4 |]) :?> int) 7 "Pair(3,4).Sum() returns 7"
            }

            test "a field-init ctor runs its let-preamble before storing fields" {
                // The `let d = a + a` preamble binds a local the field-init block
                // then reads — proving lets execute ahead of the `stfld` stores.
                let _, artifact = compileSourceData "StructFieldInitLet"

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
                let _, artifact = compileSourceData "StructInitOnly"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Ro"

                let fieldA = ty.GetField("A", BindingFlags.Public ||| BindingFlags.Instance)
                let fieldB = ty.GetField("B", BindingFlags.Public ||| BindingFlags.Instance)
                Expect.isTrue fieldA.IsInitOnly "an immutable val field is InitOnly"
                Expect.isFalse fieldB.IsInitOnly "a mutable val field stays writable"

                // The InitOnly field is still written by the field-init ctor.
                let boxed = Activator.CreateInstance(ty, [| box 3; box 4 |])
                Expect.equal (fieldA.GetValue boxed :?> int) 3 "InitOnly field initialised from the ctor"
                Expect.equal (fieldB.GetValue boxed :?> int) 4 "mutable field initialised from the ctor"
            }

            test "a field-init ctor mixes a param-sourced field and a bool-literal field" {
                let _, artifact = compileSourceData "StructFieldInitMixed"

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
                let _, artifact = compileSourceData "GenericStructShape"

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
                // Boxed dispatch through the generic self-`TypeSpec`: the ctor stores
                // `value` into the open `Box\`1<!0>::value` field and `Get()` reads it
                // back. A `CLASS`-tagged self-`TypeSpec` would fault "value type
                // mismatch" before `Get()` ever runs.
                let _, artifact = compileSourceData "GenericStructMember"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let inst = (asm.GetType "Box`1").MakeGenericType [| typeof<int> |]

                let boxed = Activator.CreateInstance(inst, [| box 99 |])
                let get = inst.GetMethod("Get", declaredInstance, null, [||], null)
                Expect.equal (get.Invoke(boxed, [||]) :?> int) 99 "Get() reads the generic ctor-param field"
            }

            test "a generic struct with a val field + field-init ctor round-trips boxed to an interface" {
                let _, artifact = compileSourceData "GenericStructIter"

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
                let _, artifact = compileSourceData "StructEnumerator"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let openTy = asm.GetType "OnceEnum`1"
                Expect.isTrue openTy.IsValueType "OnceEnum`1 is a value type"
                let ty = openTy.MakeGenericType [| typeof<int> |]

                // Box the struct and drive it through the generic `IEnumerator<int>`.
                // The mutation in `MoveNext` must survive across calls (byref `this`
                // into the box), so the second `MoveNext` reports the end.
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

            // A class `GetEnumerator()` constructs the struct enumerator in-method
            // and returns it `:>`-upcast (boxed) to the interface.
            test "a class GetEnumerator constructs a struct enumerator and returns it boxed (yields the element)" {
                let _, artifact = compileSourceData "StructEnumeratorSeq"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let seqTy = (asm.GetType "OnceSeq`1").MakeGenericType [| typeof<int> |]

                let s =
                    Activator.CreateInstance(seqTy, [| box 99 |]) :?> System.Collections.Generic.IEnumerable<int>

                Expect.equal
                    (s |> Seq.toList)
                    [ 99 ]
                    "enumerating the seq via its struct enumerator yields the single element"
            }

            // An explicit type application at the construction site (`OnceEnum<'T>(x)`):
            // the secondary-ctor type args must ground from both the explicit `<'T>` and
            // the value arg, not leak as a free `TyVar`.
            test "a class GetEnumerator constructs the struct enumerator with explicit type args (Set<'T> shape)" {
                let _, artifact = compileSourceData "StructEnumeratorTypeApp"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let seqTy = (asm.GetType "OnceSeq`1").MakeGenericType [| typeof<int> |]

                let s =
                    Activator.CreateInstance(seqTy, [| box 5 |]) :?> System.Collections.Generic.IEnumerable<int>

                Expect.equal (s |> Seq.toList) [ 5 ] "explicit-type-app construction enumerates to the single element"
            }

            // A struct declared in a referenced package: the consumer must encode
            // `ELEMENT_TYPE_VALUETYPE` (0x11) not `CLASS` (0x12) — a wrong tag faults
            // the loader. The package directory name IS the package identity
            // (`buildClosure` resolves `depends-on` against it), so it must equal
            // the manifest `name` — hence `Vesper.PointPkg`, not a descriptive slug.
            test "a struct declared in a referenced package encodes as VALUETYPE in a consumer signature" {
                let outDir = tmpDir "Vesper.PointPkg"
                let manifestPath = System.IO.Path.Combine(outDir, "manifest.toml")
                let fsiPath = System.IO.Path.Combine(outDir, "point.fsi")

                System.IO.File.WriteAllText(
                    manifestPath,
                    "[core]\nname = \"Vesper.PointPkg\"\nnamespace = \"Vesper\"\ndepends-on = []\nfiles = [\"point.fsi\"]\n"
                )

                System.IO.File.WriteAllText(
                    fsiPath,
                    "namespace Vesper\n\ntype Point =\n    struct\n        val X: int\n        val Y: int\n    end\n"
                )

                let provider = ClrSymbolProviders.buildContract [ vesperCoreManifest; manifestPath ]

                // Identity function forces `Point` into the emitted signature (return + param).
                let src =
                    "namespace App\n\nopen Vesper\n\nmodule Consumer =\n    let echo (p: Point) : Point = p\n"

                let project = ProjectInfo.library "StructXPkgConsumer"
                let lexed, file = parseFile src

                let tast =
                    Pipeline.analyseFor project.AssemblyName provider (Hashing.originSourceOfText src lexed) file

                let errors = tast.Residue.Diagnostics |> Diagnostic.errors

                if not (List.isEmpty errors) then
                    failwithf "consumer failed to analyse: %A" (errors |> List.map (fun d -> d.Message))

                let artifact = Codegen.compile provider project tast
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

            // An infix operator inside an `interface … with member …` body: desugar
            // must walk interface member bodies, not only the type's own members,
            // or the operator gets no `DesugaredForm.OpName` entry and Elaborate throws.
            test "an infix operator inside a struct interface member resolves (SetIterator.MoveNext shape)" {
                let _, artifact = compileSourceData "StructIfaceInfix"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Iter"
                Expect.isTrue ty.IsValueType "Iter is a value type"

                // Box the struct and drive `MoveNext` through `IEnumerator`. The
                // ctor seeds a single-element stack whose `Node.Height = 1`, so the
                // first `MoveNext` takes the `=`-true branch (pops the stack, sets
                // `Hit`) and returns true — proving the interface-body `=` both
                // froze and ran. A second call hits the `[]` arm and returns false.
                let nodeTy = asm.GetType "Node"
                let node = Activator.CreateInstance(nodeTy, [| box 1 |])
                let boxed = Activator.CreateInstance(ty, [| node |])
                let e = boxed :?> System.Collections.IEnumerator
                Expect.isTrue (e.MoveNext()) "first MoveNext takes the `t.Height = 1` true branch"
                Expect.isFalse (e.MoveNext()) "second MoveNext hits the empty-stack arm"
            }

            // A chained property access `this.field.Prop` where `field` is a
            // `val`/ctor-param instance field: `recoverFieldStepTy` must scan val
            // fields (not just members) when resolving intermediate chain types,
            // otherwise the receiver gets the final property's type instead of the
            // field's type.
            test "a chained property on a struct val field types the receiver as the field, not the property" {
                let _, artifact = compileSourceData "StructFieldChainProp"

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

            // `[<Struct; IsByRefLike>]` emits the `IsByRefLikeAttribute` marker so
            // the CLR confines the type to the stack. The runtime surfaces this as
            // `Type.IsByRefLike`.
            test "a `[<Struct; IsByRefLike>]` type emits a byref-like value type" {
                let _, artifact = compileSourceData "RefStructShape"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "RPoint"
                Expect.isNotNull ty "the assembly contains the type RPoint"
                Expect.isTrue ty.IsValueType "RPoint emits as a value type"
                Expect.isTrue ty.IsByRefLike "RPoint is byref-like (ref struct)"
            }

            // The marker is opt-in: `IsByRefLikeAttribute` must not leak onto plain `[<Struct>]` types.
            test "a plain `[<Struct>]` type is not byref-like" {
                let _, artifact = compileSourceData "PlainStructNotRefLike"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "NPoint"
                Expect.isTrue ty.IsValueType "NPoint is a value type"
                Expect.isFalse ty.IsByRefLike "a plain [<Struct>] is not byref-like"
            }

            // An external generic value type (`Span<char>`): ctor/member refs must be
            // tagged `VALUETYPE` not `CLASS`, and dispatch must be address-based
            // (`ldloca` + non-virtual `call`) — a by-value `callvirt` is verifier-illegal
            // on a ref struct.
            test "ref struct with a Span<char> field — ctor, Length, Slice round-trip" {
                runsDataLines [ "5"; "3" ] "ref-struct-span-field"
            }

            // `Span<T>`'s element accessor is `get_Item(i) : T&` (byref return, no
            // by-value form). This exercises the full byref stack: resolving
            // `get_Item` as carrying `FTConst("byref",[elem])`, encoding
            // `ELEMENT_TYPE_BYREF` in the member-ref, and dereferencing via `ldobj`.
            test "ref struct Span<char> byref indexer read — chars.[i]" {
                runsDataLines [ "e"; "o" ] "ref-struct-span-byref-indexer"
            }

            // `ArrayPool<char>.Shared.Return` omits its optional `clearArray = false`
            // trailing parameter. The provider surfaces `OptionalDefaults`; Elaborate
            // synthesises the omitted constant so codegen sees the full call.
            test "ArrayPool<char>.Shared Rent + Return (omitted optional arg)" {
                runsDataLines [ "ok" ] "arraypool-rent-return-optional-arg"
            }

            // The `null` literal pattern binds nothing and lowers to a non-null test
            // (`ldloc; brtrue` skips the arm), so a null scrutinee falls to the body
            // and any other value to the next arm.
            test "a `null` literal pattern matches a null reference, binds nothing" {
                runsDataLines [ "null"; "value" ] "null-literal-pattern"
            }

            // `Span<char>` passed as a by-value argument: the member-ref parent for a
            // Span parameter must encode `VALUETYPE`, not just the receiver/field/return.
            test "Span<char> by-value args — string.CopyTo, span CopyTo, Fill, ToString, TryCopyTo" {
                runsDataLines [ "ab---cd"; "ab---cdab"; "true"; "false" ] "span-byval-args"
            }

            test "a struct field block declares + initialises (static let, nullable refs, Span)" {
                runsDataLines [ "true"; "5"; "5"; "false"; "-1"; "true" ] "struct-field-block-init"
            }

            test "hex uint32 literal reads back; radix literals" {
                runsDataLines [ "true"; "true"; "true"; "true" ] "hex-radix-literals"
            }

            test "uint cast feeding Math.Max/Min/Clamp" { runsDataLines [ "true" ] "uint-cast-math-clamp" }

            // A project-local generic member called at multiple distinct types within
            // the same assembly: method typars must be freshened per call site
            // (`Engine.instantiateMemberCall`), not grounded to the first call's type.
            // Without freshening the member emits as a mono method and the second
            // call passes a wrong-typed arg (`InvalidProgramException` at JIT).
            test "generic member IFormattable dispatch (int/float/ref), same-assembly multi-instantiation" {
                runsDataLines [ "42"; "3.14"; "hi" ] "generic-member-multi-instantiation"
            }

            // Same typar-freshening invariant at module scope: a generic free function
            // (capturing nothing) called at two distinct types must stay generic,
            // not ground to the first call.
            test "generic free function, same-assembly multi-instantiation" {
                runsDataLines [ "42"; "3.14" ] "generic-free-fn-multi-instantiation"
            }

            // `&local` (managed address-of) lowers to `ldloca` of the operand's slot;
            // the member-ref encodes the parameter with `ELEMENT_TYPE_BYREF`. The local
            // must be a function-local mutable — a module-level mutable is a static
            // field, not a slot-addressable local.
            test "Int32.TryParse(s, &r) writes the out local through a byref arg" {
                runsDataLines [ "123"; "-1" ] "int-tryparse-byref-out"
            }

            // `ISpanFormattable.TryFormat` writes formatted chars straight into a
            // `Span<char>` buffer (no intermediate string), reading the count back
            // through the `&cw` out arg. `int`/`float` implement `ISpanFormattable`;
            // `string` falls to the `o.ToString()` arm.
            test "AppendFormatted span fast-path via ISpanFormattable.TryFormat" {
                runsDataLines [ "42"; "3.14"; "hi" ] "spanformattable-tryformat"
            }

            // Every body mutates `this` through self-calls (`AppendFormatted` →
            // `AppendLiteral` → `GrowThenCopyString` → `Grow` → `GrowCore`). These
            // only persist because struct self-calls address `this` in place
            // (`EmitMember.loadStructReceiverAddr`); a defensive copy per call would
            // lose each mutation.
            test "struct formatter core — literal, generic hole, grow, string sink" {
                runsDataLines [ "x=42, pi=3.14"; "400" ] "formatter-core-selfcall-grow"
            }

            // Same-name overloaded generic members: each overload carries its own
            // method typar `'T`. Elaboration must match the exact overload by `DeclKey`,
            // not by name — otherwise all overloads share the first one's typars and
            // the others' `'T` freezes as `?free-typar`.
            test "same-name overloaded generic members each generalise their own 'T" {
                runsDataLines [ "42" ] "overloaded-generic-members-own-typar"
            }

            // External instance method overload resolution on a variable/property
            // receiver: `w.Write("hi")` parses with `fn = LongIdent [w; Write]`
            // (the parser folds the dot). This must go through `pickBestOverload`,
            // not the single-pick field walk (which would grab the widest overload).
            test "external instance overload pick on a folded-LongIdent receiver" {
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

            // `a + b + c` = `(a + b) + c`: the outer `+`'s `^T` must be grounded by
            // a sibling ground operand (`c : string` or the `string` return position),
            // not pinned to the inner App's still-abstract result type. Without this,
            // the operator falls to its numeric `add` base — `add` on string refs is
            // an AccessViolation.
            test "Chained string concat a + b + c lowers to String.Concat (not numeric add)" {
                runsDataLines [ "abc"; "(x)" ] "chained-string-concat"
            }

            // String escape sequences must decode to the char they denote: `ElaborateLiterals`
            // must not append the raw 2-char span verbatim.
            test "string literal escape sequences decode in the emitted value" {
                runsDataLines [ "a"; "b"; "x\ty"; "q\"r" ] "string-escape-sequences"
            }

            // The `%A` layout core: a group is all-flat iff
            // `col + inner.flatWidth <= width` (`width = 0` ⇒ always flat), never
            // half-broken. `render` returns a record `{ Txt; Col }` rather than
            // threading a `StringBuilder` (avoids mixed ref/value-field path).
            //
            // The DU cases are named `LDoc`/`LText`/… to avoid clashing with the
            // external C# `Vesper.Doc`/`Vesper.DocGroup`/… already in scope from the
            // default test stack; a local `DocGroup(…)` would otherwise bind the
            // external class (no ctor recipe) instead of the local union case. The
            // layout core is the single-sourced `data/_layout-core.fs` fragment.
            test "Doc layout core — flatWidth + Render (flat / never-break / broken)" {
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

            // A list literal passed directly as a union-case argument `LCat [ a; b; c ]`:
            // `peelCtorArgs` must only collapse round-paren/begin-end grouping, never
            // unwrap a `[ … ]` literal's `EnclosedBlock` (which would drop the list lowering).
            test "list literal as a direct union-case argument round-trips" {
                runsDataLines [ "3"; "6" ] "list-literal-union-case-arg"
            }

            // Overloaded instance members on a struct, where one overload self-calls
            // another: `Members` must key to an overload list and pick by argument
            // types at the call site (ECMA-335 §I.10.2). Keying by name only makes
            // every call resolve to the first overload's handle, causing
            // `InvalidProgramException` when the arity or types mismatch.
            test "overloaded struct self-call resolves the right overload by arg types" {
                runsSelfHostDataLines [ "2" ] "overloaded-struct-selfcall"
            }

            test "%u / aligned / zero-padded-float holes match the structural spec" {
                runsEq "42" "printfn \"%u\" 42"
                runsEq "   42" "printfn \"%5u\" 42"
                runsEq "00003.14" "printfn \"%08.2f\" 3.14"
                runsEq "42   ,7" "printfn \"%-5u,%d\" 42 7"
            }

            // A class-hosted `IFormatSink` + frame stack: a union case drives the
            // semantic `BeginCase`/`Child`/`EndCase` protocol; the sink decides
            // single-payload parenthesisation from the child's application-shapedness
            // (`Some (Some 1)` but not `Some 1`). Drivers are classes (`MyList`/`MyOpt`)
            // because union interface impls are unsupported front-to-back.
            test "a class IFormatSink sink + frame stack renders lists and nested cases" {
                runsSelfHostDataLines
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
                runsSelfHostDataLines
                    [ "5y"; "5uy"; "5s"; "5us"; "5u"; "5L"; "5UL"; "5M" ]
                    "structural-format-numeric-suffixes"
            }

            test "structural-format probe: float fixup + string/char quoting" {
                runsSelfHostDataLines
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
                runsSelfHostDataLines [ "(1, 2)"; "[1; 2; 3]" ] "structural-format-tuple-enum"
            }

            // The full `%A` dispatch + atom rendering: the reflection-free `:?` chain,
            // atom helpers (`formatPrimitive`/`fixFloat`/`quoteString`/`quoteChar`),
            // and the depth+size budget. Drives one value per dispatch arm.
            test "structural %A engine: Dispatch + atom rendering (depth+size budget)" {
                runsSelfHostDataLines
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

            // A val-field class whose only ctor is a parameterless `new() = { … }`:
            // no synthesised empty primary `.ctor()` must be emitted alongside it
            // (two identical `.ctor()` rows would leave construction binding the empty
            // one and every field uninitialised).
            test "a val-field class with only a parameterless new() initialises its fields" {
                runsDataLines [ "42"; "7" ] "valfield-parameterless-ctor"
            }
        ]
