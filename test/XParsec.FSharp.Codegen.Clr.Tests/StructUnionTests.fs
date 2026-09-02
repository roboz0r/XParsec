module XParsec.FSharp.Codegen.Clr.Tests.StructUnionTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection

// `[<Struct>]` union emission: the nested `Payload` and overlay types, the `.ctor`, the
// factories and the `Get_<Case>_<i>` readers, asserted on the metadata and through the
// loader. Each test's program is a standalone file under `data/`.

[<Tests>]
let structUnionTests =
    testList
        "StructUnion"
        [
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
                Expect.equal (ctors.[0].GetParameters().Length) 2 "tag + payload"

                // `_tag` is private and `_payload` assembly-visible, so both only come out
                // under `NonPublic`.
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
                Expect.equal names (Set.ofList [ "_tag"; "_payload" ]) "the tag and the one payload field"

                let payloadTy = asm.GetType "Shape+Payload"
                Expect.isNotNull payloadTy "the union nests its Payload struct"
                Expect.isTrue payloadTy.IsValueType "Payload is a value type"
                Expect.isTrue payloadTy.IsNestedAssembly "Payload is assembly-visible"

                Expect.equal
                    (fields |> Array.find (fun f -> f.Name = "_payload")).FieldType
                    payloadTy
                    "_payload is typed as the nested Payload"
            }

            // `Point.x`, `Pair.a` and `Pair.b` are all unmanaged `int`s, so every case's
            // fields sit on its own data struct and the two structs share offset 0 of the
            // explicit-layout overlay `Data`, which is `Payload`'s only slot.
            test "a `[<Struct>]` union's unmanaged fields overlap at offset 0 of an explicit-layout overlay" {
                let bytes = Codegen.toBytes (compileSourceData "StructUnionShape")
                MetadataStructure.assertWellFormed "StructUnionOverlay" bytes

                Expect.equal
                    (MetadataStructure.fieldsOf bytes "Shape+Payload")
                    [ "_data" ]
                    "Payload holds the overlay alone"

                Expect.equal
                    (MetadataStructure.fieldsOf bytes "Shape+Data")
                    [ "Point"; "Pair" ]
                    "one overlay field per case"

                Expect.equal (MetadataStructure.fieldsOf bytes "Shape+Data_Point") [ "_x" ] "Point's data struct"
                Expect.equal (MetadataStructure.fieldsOf bytes "Shape+Data_Pair") [ "_a"; "_b" ] "Pair's data struct"

                let decl name =
                    match MetadataStructure.typeDecl bytes name with
                    | ValueSome d -> d
                    | ValueNone -> failwithf "no type %s" name

                let overlay = decl "Shape+Data"
                Expect.equal overlay.Layout TypeAttributes.ExplicitLayout "the overlay is ExplicitLayout"
                Expect.equal overlay.Visibility TypeAttributes.NestedAssembly "the overlay is assembly-visible"
                Expect.isTrue overlay.IsSealed "the overlay is sealed"
                Expect.equal overlay.Extends "System.ValueType" "the overlay is a value type"
                Expect.isEmpty overlay.Typars "the overlay declares no typar"

                Expect.equal
                    (MetadataStructure.fieldLayoutsOf bytes "Shape+Data")
                    [ "Point", 0; "Pair", 0 ]
                    "every case data struct sits at offset 0"

                Expect.equal (MetadataStructure.classLayoutRowCount bytes) 0 "the loader computes every size"

                for name in [ "Shape+Payload"; "Shape+Data_Point"; "Shape+Data_Pair" ] do
                    let d = decl name
                    Expect.equal d.Layout TypeAttributes.SequentialLayout (name + " is sequential")
                    Expect.equal d.Extends "System.ValueType" (name + " is a value type")
                    Expect.isEmpty (MetadataStructure.fieldLayoutsOf bytes name) (name + " carries no FieldLayout row")

                // The factory writes a case's fields through `ldflda` into a zeroed
                // `Payload` local, which `initonly` would forbid.
                for name in [ "Shape+Payload"; "Shape+Data"; "Shape+Data_Pair" ] do
                    for (field, attrs) in MetadataStructure.fieldAttrsOf bytes name do
                        Expect.isFalse (attrs.HasFlag FieldAttributes.InitOnly) (sprintf "%s.%s is writable" name field)

                        Expect.equal
                            (attrs &&& FieldAttributes.FieldAccessMask)
                            FieldAttributes.Assembly
                            (sprintf "%s.%s is assembly-visible" name field)
            }

            // The loader is the arbiter of the overlay's legality: it computes the sizes
            // and rejects an overlap of GC references, or explicit layout on a generic type.
            test "every struct-union data program loads and constructs through the runtime loader" {
                for program in
                    [
                        "StructUnionShape"
                        "StructUnionGenericShape"
                        "StructUnionSameNameFields"
                        "StructUnionExternalPayload"
                        "StructUnionLocalRefPayload"
                        "StructUnionMixedStorage"
                        "StructUnionGenericOverlay"
                    ] do
                    let asm = loadAssembly (Codegen.toBytes (compileSourceData program))

                    // Every type, nested storage types included, through the loader.
                    for ty in asm.GetTypes() do
                        ty.GetFields(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance)
                        |> ignore

                let shape =
                    (loadAssembly (Codegen.toBytes (compileSourceData "StructUnionShape"))).GetType "Shape"

                let pair = shape.GetMethod("Pair").Invoke(null, [| box 4; box 5 |])

                Expect.equal
                    (shape.GetMethod("Get_Pair_1").Invoke(pair, [||]))
                    (box 5)
                    "Pair.b reads through the overlay"
            }

            // The ten lines are `score` over each constructed value, then the equalities:
            // same case same payload, same case different payload, a mixed case, two
            // different cases.
            test "a mixed-storage struct union constructs, matches and equates across every storage kind" {
                runsDataLines
                    [ "5"; "-5"; "5"; "5"; "12"; "10"; "true"; "false"; "true"; "false" ]
                    "StructUnionMixedStorage"
            }

            test "a mixed-storage struct union reads every storage kind back through its getters" {
                let bytes = Codegen.toBytes (compileSourceData "StructUnionMixedStorage")

                Expect.equal
                    (MetadataStructure.fieldsOf bytes "Storage+Payload")
                    [ "_data"; "_ref0"; "_val0"; "_val1" ]
                    "the overlay, the object slot, then the two exact slots"

                Expect.equal
                    (MetadataStructure.fieldsOf bytes "Storage+Data")
                    [ "Scalars"; "Nested"; "Both" ]
                    "the cases with an unmanaged field"

                let asm = loadAssembly bytes
                let ty = asm.GetType "Storage"
                let read (name: string) (value: obj) = ty.GetMethod(name).Invoke(value, [||])

                let id = Guid.NewGuid()
                let idValue = ty.GetMethod("Id").Invoke(null, [| box id |])
                Expect.equal (read "Get_Id_0" idValue) (box id) "an undetermined BCL struct round-trips its exact slot"

                let both = ty.GetMethod("Both").Invoke(null, [| box 7; box "xyz" |])
                Expect.equal (read "Get_Both_0" both) (box 7) "the overlaid int of a mixed case"
                Expect.equal (read "Get_Both_1" both) (box "xyz") "the erased string of a mixed case"

                let scalars = ty.GetMethod("Scalars").Invoke(null, [| box 5; box true |])
                Expect.equal (read "Get_Scalars_0" scalars) (box 5) "an overlaid int"
                Expect.equal (read "Get_Scalars_1" scalars) (box true) "an overlaid bool"

                // The zero value is the tag-0 case with every field zero.
                let zero = Activator.CreateInstance ty
                Expect.equal (read "Get_Scalars_0" zero) (box 0) "the zero value's overlaid int"
                Expect.equal (read "Get_Id_0" zero) (box Guid.Empty) "the zero value's exact slot"
            }

            // The overlay and its case data struct are non-generic types nested in the
            // generic union, so explicit layout stays off the generic type.
            test "a generic struct union with an unmanaged case loads and runs" {
                let bytes = Codegen.toBytes (compileSourceData "StructUnionGenericOverlay")
                MetadataStructure.assertWellFormed "StructUnionGenericOverlay" bytes

                let decl name =
                    match MetadataStructure.typeDecl bytes name with
                    | ValueSome d -> d
                    | ValueNone -> failwithf "no type %s" name

                Expect.equal (decl "GShape`1+Payload").Typars [ "T" ] "Payload redeclares the union's typar"
                Expect.isEmpty (decl "GShape`1+Data").Typars "the overlay is non-generic"
                Expect.equal (decl "GShape`1+Data").Layout TypeAttributes.ExplicitLayout "the overlay is ExplicitLayout"
                Expect.isEmpty (decl "GShape`1+Data_Pt").Typars "the case data struct is non-generic"

                let asm = loadAssembly bytes
                let ty = (asm.GetType "GShape`1").MakeGenericType [| typeof<string> |]
                let pt = ty.GetMethod("Pt").Invoke(null, [| box 4; box 5 |])

                Expect.equal
                    (ty.GetMethod("Get_Pt_1").Invoke(pt, [||]))
                    (box 5)
                    "an overlaid field at a string instantiation"

                let v = ty.GetMethod("Val").Invoke(null, [| box "abc" |])

                Expect.equal
                    (ty.GetMethod("Get_Val_0").Invoke(v, [||]))
                    (box "abc")
                    "the typar slot at a string instantiation"

                runsDataLines [ "3"; "9"; "true"; "false" ] "StructUnionGenericOverlay"
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

            // A generic struct union's factory and match arm reach the erased `object` slot
            // through `MemberRef`s on the instantiated `TypeSpec`, at the declared types.
            test "a generic `[<Struct>]` union erases a string and a local record into one slot" {
                runsDataLines [ "three"; "hi"; "n"; "true"; "false" ] "StructUnionLocalRefPayload"
            }

            // The reference field is erased into a shared `object` slot; the unmanaged `int`
            // one lands in the overlay. A same-name pair still lands in two places, so the
            // FS3585 relaxation survives the sharing.
            test "a reference-typed case field is stored in an `object` slot" {
                let artifact = compileSourceData "StructUnionSameNameFields"
                let bytes = Codegen.toBytes artifact

                MetadataStructure.assertWellFormed "StructUnionSlots" bytes

                Expect.equal
                    (MetadataStructure.fieldAttrsOf bytes "Mixed" |> List.map fst)
                    [ "_tag"; "_payload" ]
                    "the tag and the payload"

                Expect.equal
                    (MetadataStructure.fieldsOf bytes "Mixed+Payload")
                    [ "_data"; "_ref0" ]
                    "the overlay and one object slot"

                let asm = loadAssembly bytes
                let payloadTy = asm.GetType "Mixed+Payload"

                let fieldTy name =
                    payloadTy
                        .GetField(name, BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance)
                        .FieldType

                Expect.equal (fieldTy "_data") (asm.GetType "Mixed+Data") "the overlay slot is the nested Data"
                Expect.equal (fieldTy "_ref0") typeof<obj> "the reference slot is `object`"

                let ty = asm.GetType "Mixed"

                // `Get_S_0` returns `string`, so the erased read `castclass`es on the way
                // out.
                let s = ty.GetMethod("S").Invoke(null, [| box "hi" |])
                Expect.equal (ty.GetMethod("Get_S_0").Invoke(s, [||])) (box "hi") "Get_S_0 reads through the cast"
            }
        ]
