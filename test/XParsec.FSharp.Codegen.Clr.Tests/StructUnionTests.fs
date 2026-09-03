module XParsec.FSharp.Codegen.Clr.Tests.StructUnionTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Common.Tests
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
            // explicit-layout overlay `Shape$Data`, which is `Payload`'s only slot. The
            // overlay is the union's sibling, with the case data structs nested in it.
            test "a `[<Struct>]` union's unmanaged fields overlap at offset 0 of an explicit-layout overlay" {
                let bytes = Codegen.toBytes (compileSourceData "StructUnionShape")
                MetadataStructure.assertWellFormed "StructUnionOverlay" bytes

                Expect.equal
                    (MetadataStructure.fieldsOf bytes "Shape+Payload")
                    [ "_data" ]
                    "Payload holds the overlay alone"

                Expect.equal
                    (MetadataStructure.fieldsOf bytes "Shape$Data")
                    [ "Point"; "Pair" ]
                    "one overlay field per case"

                Expect.equal (MetadataStructure.fieldsOf bytes "Shape$Data+Data_Point") [ "_x" ] "Point's data struct"

                Expect.equal
                    (MetadataStructure.fieldsOf bytes "Shape$Data+Data_Pair")
                    [ "_a"; "_b" ]
                    "Pair's data struct"

                let decl name =
                    match MetadataStructure.typeDecl bytes name with
                    | ValueSome d -> d
                    | ValueNone -> failwithf "no type %s" name

                let overlay = decl "Shape$Data"
                Expect.equal overlay.Layout TypeAttributes.ExplicitLayout "the overlay is ExplicitLayout"
                Expect.equal overlay.Visibility TypeAttributes.NotPublic "the overlay is assembly-visible"
                Expect.isTrue overlay.IsSealed "the overlay is sealed"
                Expect.equal overlay.Extends "System.ValueType" "the overlay is a value type"
                Expect.isEmpty overlay.Typars "the overlay declares no typar"

                Expect.equal
                    (MetadataStructure.fieldLayoutsOf bytes "Shape$Data")
                    [ "Point", 0; "Pair", 0 ]
                    "every case data struct sits at offset 0"

                Expect.equal (MetadataStructure.classLayoutRowCount bytes) 0 "the loader computes every size"

                for name in [ "Shape+Payload"; "Shape$Data+Data_Point"; "Shape$Data+Data_Pair" ] do
                    let d = decl name
                    Expect.equal d.Layout TypeAttributes.SequentialLayout (name + " is sequential")
                    Expect.equal d.Extends "System.ValueType" (name + " is a value type")
                    Expect.isEmpty (MetadataStructure.fieldLayoutsOf bytes name) (name + " carries no FieldLayout row")

                // The factory writes a case's fields through `ldflda` into a zeroed
                // `Payload` local, which `initonly` would forbid.
                for name in [ "Shape+Payload"; "Shape$Data"; "Shape$Data+Data_Pair" ] do
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

                // The readers are ABI, withheld from IDE completion; the views are surface.
                let editorBrowsableNever (m: MethodInfo) =
                    m.GetCustomAttributesData()
                    |> Seq.exists (fun a ->
                        a.AttributeType.FullName = "System.ComponentModel.EditorBrowsableAttribute"
                        && a.ConstructorArguments.[0].Value = box 1
                    )

                for name in [ "Get_Point_0"; "Get_Pair_0"; "Get_Pair_1" ] do
                    Expect.isTrue (editorBrowsableNever (shape.GetMethod name)) (name + " is EditorBrowsable(Never)")

                for name in [ "Get_Point"; "Get_Pair"; "Point"; "Pair" ] do
                    Expect.isFalse (editorBrowsableNever (shape.GetMethod name)) (name + " stays browsable")
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
                    (MetadataStructure.fieldsOf bytes "Storage$Data")
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

            // The overlay and its case data struct are non-generic siblings of the generic
            // union, so explicit layout stays off the generic type and every type nested in
            // the generic union redeclares its typars.
            test "a generic struct union with an unmanaged case loads and runs" {
                let bytes = Codegen.toBytes (compileSourceData "StructUnionGenericOverlay")
                MetadataStructure.assertWellFormed "StructUnionGenericOverlay" bytes

                let decl name =
                    match MetadataStructure.typeDecl bytes name with
                    | ValueSome d -> d
                    | ValueNone -> failwithf "no type %s" name

                Expect.equal (decl "GShape`1+Payload").Typars [ "T" ] "Payload redeclares the union's typar"
                Expect.isEmpty (decl "GShape$Data$1").Typars "the overlay is non-generic"
                Expect.equal (decl "GShape$Data$1").Layout TypeAttributes.ExplicitLayout "the overlay is ExplicitLayout"
                Expect.isEmpty (decl "GShape$Data$1+Data_Pt").Typars "the case data struct is non-generic"

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

                // The `Get_<Case>` view accessors share the prefix, so the whole `Get_`
                // family is pinned by name: the field readers first, then the accessors.
                let getterNames = [ "Get_Point_0"; "Get_Pair_0"; "Get_Pair_1" ]

                Expect.equal
                    (methods |> List.map fst |> List.filter (fun n -> n.StartsWith "Get_"))
                    [ yield! getterNames; yield "Get_Point"; yield "Get_Pair" ]
                    "one getter per (case, field) in case then field order, then one view accessor per payload-bearing case"

                let getters = methods |> List.filter (fun (n, _) -> List.contains n getterNames)

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

            // The consumer surface: one public readonly `Payload_<Case>` per payload-bearing
            // case, wrapping a `Payload` and exposing the case's fields as properties in
            // F#'s own spelling. The compiled match reads the placement directly instead.
            test "a `[<Struct>]` union nests one public Payload_<Case> view per payload-bearing case" {
                let bytes = Codegen.toBytes (compileSourceData "StructUnionShape")

                MetadataStructure.assertWellFormed "StructUnionViews" bytes

                let decl name =
                    match MetadataStructure.typeDecl bytes name with
                    | ValueSome d -> d
                    | ValueNone -> failwithf "no type %s" name

                for name in [ "Shape+Payload_Point"; "Shape+Payload_Pair" ] do
                    let d = decl name
                    Expect.equal d.Visibility TypeAttributes.NestedPublic (name + " is public")
                    Expect.equal d.Layout TypeAttributes.SequentialLayout (name + " is sequential")
                    Expect.isTrue d.IsSealed (name + " is sealed")
                    Expect.equal d.Extends "System.ValueType" (name + " is a value type")

                    Expect.equal
                        (MetadataStructure.fieldsOf bytes name)
                        [ "_payload" ]
                        (name + " wraps one Payload and nothing else")

                Expect.equal
                    (MetadataStructure.propertiesOf bytes "Shape+Payload_Pair")
                    [ "a", (ValueSome "get_a", ValueNone); "b", (ValueSome "get_b", ValueNone) ]
                    "one get-only property per logical field, in declaration order"

                Expect.equal
                    (MetadataStructure.methodAttrsOf bytes "Shape+Payload_Pair" |> List.map fst)
                    [ ".ctor"; "get_a"; "get_b" ]
                    "the .ctor then the property getters"

                // `Payload` is assembly-visible, so the `.ctor` taking one is too: a
                // consumer outside the assembly reaches a view through `Get_<Case>`.
                for (name, attrs) in MetadataStructure.methodAttrsOf bytes "Shape+Payload_Pair" do
                    let access = attrs &&& MethodAttributes.MemberAccessMask

                    if name = ".ctor" then
                        Expect.equal access MethodAttributes.Assembly ".ctor is assembly-visible"
                    else
                        Expect.equal access MethodAttributes.Public (name + " is public")
                        Expect.isTrue (attrs.HasFlag MethodAttributes.SpecialName) (name + " is an accessor")

                // `Get_<Case>` is a plain method beside the `Get_<Case>_<i>` field readers,
                // spelled outside the `get_` accessor convention, since it backs no property.
                Expect.equal
                    (MetadataStructure.methodAttrsOf bytes "Shape"
                     |> List.map fst
                     |> List.filter (fun n -> n.StartsWith "get_" || n.StartsWith "Get_"))
                    [
                        "get_Tag"
                        "Get_Point_0"
                        "Get_Pair_0"
                        "Get_Pair_1"
                        "Get_Point"
                        "Get_Pair"
                    ]
                    "the union hands out each case's view through Get_<Case>, after the field readers"

                let asm = loadAssembly bytes
                let ty = asm.GetType "Shape"

                Expect.equal
                    (asm.GetType "Shape+Payload_Pair")
                    (ty.GetMethod("Get_Pair").ReturnType)
                    "Get_Pair returns the case's view"

                let pair = ty.GetMethod("Pair").Invoke(null, [| box 4; box 5 |])
                let view = ty.GetMethod("Get_Pair").Invoke(pair, [||])

                Expect.equal (view.GetType().GetProperty("a").GetValue view) (box 4) "the view reads a"
                Expect.equal (view.GetType().GetProperty("b").GetValue view) (box 5) "the view reads b"
            }

            // One view per storage kind, over the same placement table the `Get_<Case>_<i>`
            // readers use: the overlay, the `object` slot with its cast, and both exact
            // slots.
            test "a mixed-storage struct union reads every storage kind back through its case views" {
                let bytes = Codegen.toBytes (compileSourceData "StructUnionMixedStorage")
                let asm = loadAssembly bytes
                let ty = asm.GetType "Storage"
                let payloadTy = asm.GetType "Storage+Payload"

                // A view's whole state is the payload it reads through, so a view copy
                // costs one `Payload`.
                for case in [ "Scalars"; "Nested"; "Text"; "Labelled"; "Id"; "Both" ] do
                    let viewTy = asm.GetType("Storage+Payload_" + case)
                    Expect.isNotNull viewTy (case + " has a view")

                    let fields =
                        viewTy.GetFields(
                            BindingFlags.Public
                            ||| BindingFlags.NonPublic
                            ||| BindingFlags.Instance
                            ||| BindingFlags.DeclaredOnly
                        )

                    Expect.equal
                        (fields |> Array.map (fun f -> f.FieldType))
                        [| payloadTy |]
                        (case + "'s view holds one Payload and nothing else")

                let build (case: string) (args: obj[]) : obj = ty.GetMethod(case).Invoke(null, args)

                let viewOf (case: string) (value: obj) : obj =
                    ty.GetMethod("Get_" + case).Invoke(value, [||])

                let read (view: obj) (prop: string) : obj =
                    view.GetType().GetProperty(prop).GetValue view

                let scalars = viewOf "Scalars" (build "Scalars" [| box 5; box true |])
                Expect.equal (read scalars "x") (box 5) "an overlaid int"
                Expect.equal (read scalars "y") (box true) "an overlaid bool"

                let inner = Activator.CreateInstance(asm.GetType "Inner", [| box 2; box 3.5 |])

                Expect.equal
                    (read (viewOf "Nested" (build "Nested" [| inner |])) "inner")
                    inner
                    "an overlaid struct record"

                Expect.equal
                    (read (viewOf "Text" (build "Text" [| box "hello" |])) "s")
                    (box "hello")
                    "an erased string"

                let tagged = Activator.CreateInstance(asm.GetType "Tagged", [| box "ab"; box 10 |])

                Expect.equal
                    (read (viewOf "Labelled" (build "Labelled" [| tagged |])) "t")
                    tagged
                    "a managed struct in an exact slot"

                let id = Guid.NewGuid()
                Expect.equal (read (viewOf "Id" (build "Id" [| box id |])) "id") (box id) "an undetermined BCL struct"

                let both = viewOf "Both" (build "Both" [| box 7; box "xyz" |])
                Expect.equal (read both "k") (box 7) "the overlaid int of a mixed case"
                Expect.equal (read both "name") (box "xyz") "the erased string of a mixed case"
            }

            // The views share `Payload`'s typars, so a view's `.ctor` and its wrapped field
            // are minted on the view's own `TypeSpec`.
            test "a generic struct union's case views are generic with it" {
                let bytes = Codegen.toBytes (compileSourceData "StructUnionGenericOverlay")

                let decl name =
                    match MetadataStructure.typeDecl bytes name with
                    | ValueSome d -> d
                    | ValueNone -> failwithf "no type %s" name

                Expect.equal (decl "GShape`1+Payload_Val").Typars [ "T" ] "the view redeclares the union's typar"
                Expect.equal (decl "GShape`1+Payload_Pt").Typars [ "T" ] "every view does, whatever the case stores"

                Expect.equal
                    (MetadataStructure.propertiesOf bytes "GShape`1+Payload_Pt")
                    [ "x", (ValueSome "get_x", ValueNone); "y", (ValueSome "get_y", ValueNone) ]
                    "the overlaid case's properties"

                let asm = loadAssembly bytes
                let ty = (asm.GetType "GShape`1").MakeGenericType [| typeof<string> |]

                let read (case: string) (value: obj) (prop: string) : obj =
                    let view = ty.GetMethod("Get_" + case).Invoke(value, [||])
                    view.GetType().GetProperty(prop).GetValue view

                let pt = ty.GetMethod("Pt").Invoke(null, [| box 4; box 5 |])
                Expect.equal (read "Pt" pt "y") (box 5) "an overlaid field at a string instantiation"

                let v = ty.GetMethod("Val").Invoke(null, [| box "abc" |])
                Expect.equal (read "Val" v "v") (box "abc") "the typar slot at a string instantiation"
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

                Expect.equal (fieldTy "_data") (asm.GetType "Mixed$Data") "the overlay slot is the sibling overlay"
                Expect.equal (fieldTy "_ref0") typeof<obj> "the reference slot is `object`"

                let ty = asm.GetType "Mixed"

                // `Get_S_0` returns `string`, so the erased read `castclass`es on the way
                // out.
                let s = ty.GetMethod("S").Invoke(null, [| box "hi" |])
                Expect.equal (ty.GetMethod("Get_S_0").Invoke(s, [||])) (box "hi") "Get_S_0 reads through the cast"
            }
        ]

// ---- Decompiled-C# goldens --------------------------------------------------
// One golden per union in the struct-union data corpus, `goldens/<Program>.<Union>.cs`: the
// union's F# declaration in a leading block comment, then the union and its sibling overlay
// rendered as C#. The rendering will not compile: the overlay's `$` name has no C# spelling.
// Regenerate with `-UpdateSnapshots`.

let private goldensDir = IO.Path.Combine(__SOURCE_DIRECTORY__, "goldens")

let private goldenFile (entry: StructUnionProgram) : string =
    sprintf "%s.%s.cs" entry.Program entry.Union

/// The union's declaration in its data program: the `type` line with the attribute lines
/// directly above it, through every following blank or indented line.
let private unionDeclSource (entry: StructUnionProgram) : string =
    let lines =
        (dataSource entry.Program).Split '\n' |> Array.map (fun l -> l.TrimEnd '\r')

    let declares (line: string) =
        let prefix = "type " + entry.Union

        line.StartsWith prefix
        && (line.Length = prefix.Length || " <=".IndexOf line.[prefix.Length] >= 0)

    let typeLine =
        match Array.tryFindIndex declares lines with
        | Some i -> i
        | None -> failwithf "%s declares no `type %s`" entry.Program entry.Union

    let rec firstAttr i =
        if i > 0 && lines.[i - 1].StartsWith "[<" then
            firstAttr (i - 1)
        else
            i

    let continues (line: string) =
        line.Length = 0 || Char.IsWhiteSpace line.[0]

    let rec pastEnd i =
        if i < lines.Length && continues lines.[i] then
            pastEnd (i + 1)
        else
            i

    lines.[firstAttr typeLine .. pastEnd (typeLine + 1) - 1]
    |> Array.rev
    |> Array.skipWhile (fun l -> l.Length = 0)
    |> Array.rev
    |> String.concat "\n"

[<Tests>]
let structUnionLayoutGoldens =
    testList
        "StructUnion layout goldens"
        [
            for entry in structUnionCorpus do
                let file = goldenFile entry

                test file {
                    let artifact = compileSourceData entry.Program
                    let symbols, u = analysedStructUnion entry

                    let names =
                        [
                            yield entry.MetaName

                            match (flatPlacementsOf symbols u).OverlaidCases with
                            | [] -> ()
                            | _ -> yield UnionPayloadType.overlayName u.Key
                        ]

                    let actual =
                        sprintf "/*\n%s\n*/\n\n%s" (unionDeclSource entry) (Decompile.typesAsCSharp artifact names)

                    Goldens.check (IO.Path.Combine(goldensDir, file)) file actual
                }

            test "no golden outlives the union it pins" {
                Goldens.checkNoOrphans goldensDir "StructUnion*.cs" (List.map goldenFile structUnionCorpus)
            }
        ]
