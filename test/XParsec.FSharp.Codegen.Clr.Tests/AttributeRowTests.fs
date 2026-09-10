module XParsec.FSharp.Codegen.Clr.Tests.AttributeRowTests

open System
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection

// The generalised `CustomAttribute` rows: a frozen attribute on a type declaration lands as
// a metadata row carrying the real ctor reference and an ECMA-335 II.23.3 blob.

let inline private toEntity (h: ^T) : EntityHandle =
    (^T: (static member op_Implicit: ^T -> EntityHandle) h)

/// The `CustomAttribute` rows parented on the TypeDef named `typeName` (simple name), as
/// `(ctor's declaring-type simple name, raw blob bytes)`.
let private customAttributeRowsOn (bytes: byte[]) (typeName: string) : (string * byte[]) list =
    use pe = openPe bytes
    let md = pe.GetMetadataReader()

    let tdh =
        md.TypeDefinitions
        |> Seq.find (fun h -> md.GetString (md.GetTypeDefinition h).Name = typeName)

    [
        for h in md.CustomAttributes do
            let ca = md.GetCustomAttribute h

            if ca.Parent = toEntity tdh then
                let ctorDeclName =
                    match ca.Constructor.Kind with
                    | HandleKind.MemberReference ->
                        let mr = md.GetMemberReference(MemberReferenceHandle.op_Explicit ca.Constructor)

                        match mr.Parent.Kind with
                        | HandleKind.TypeReference ->
                            md.GetString (md.GetTypeReference(TypeReferenceHandle.op_Explicit mr.Parent)).Name
                        | other -> failwithf "unexpected MemberRef parent kind %A" other
                    | HandleKind.MethodDefinition ->
                        let m = md.GetMethodDefinition(MethodDefinitionHandle.op_Explicit ca.Constructor)

                        md.GetString (md.GetTypeDefinition(m.GetDeclaringType())).Name
                    | other -> failwithf "unexpected CustomAttribute ctor kind %A" other

                yield ctorDeclName, md.GetBlobBytes ca.Value
    ]

[<Tests>]
let tests =
    testList
        "CustomAttribute rows"
        [
            // (a) The full round trip: `[<AllowNullLiteral>]` emits its row, and the
            // metadata reader's `hasAllowNullLiteral` sees the Vesper spelling on a
            // reflection-only load of the emitted DLL.
            test "[<AllowNullLiteral>] round-trips through emit and MetadataSymbols" {
                let outDir = tmpDir "attr-allownull"
                let outPath = Path.Combine(outDir, "AttrAllowNull.dll")

                let project =
                    { ProjectInfo.library "AttrAllowNull" with
                        OutputPath = Some outPath
                    }

                let source =
                    String.concat
                        "\n"
                        [
                            "namespace AttrProbe"
                            ""
                            "[<AllowNullLiteral>]"
                            "type NullableClass() ="
                            "    member _.Value = 1"
                        ]

                let artifact = compileSourceTo project source
                Codegen.materialise artifact

                let rows = customAttributeRowsOn (Codegen.toBytes artifact) "NullableClass"

                Expect.exists
                    rows
                    (fun (ctorDecl, _) -> ctorDecl = "AllowNullLiteralAttribute")
                    "the type carries an AllowNullLiteralAttribute row"

                let provider =
                    MetadataSymbols.create (outPath :: vesperCoreDll.Value :: MetadataSymbols.runtimeAssemblyPaths ())

                match ExternalSymbols.tryMetaType provider "AttrProbe.NullableClass" with
                | ValueSome(ExternalTypeShape.Class info) ->
                    Expect.isTrue
                        info.Flags.Declared.AllowNullLiteral
                        "the reader reports AllowNullLiteral off the emitted row alone"
                | other -> failtestf "expected AttrProbe.NullableClass as a Class shape, got %A" other
            }

            // (b) A parameterless marker: `[<ReferenceEquality>]` produces its row, and a
            // runtime reflection load resolves the Vesper.Core attribute type.
            test "[<ReferenceEquality>] emits its CustomAttribute row" {
                // Force the Default-ALC Vesper.Core so the fresh-ALC load below resolves
                // the attribute type by fall-through.
                vesperCoreDll.Value |> ignore

                let source =
                    String.concat
                        "\n"
                        [
                            "[<ReferenceEquality>]"
                            "type Pair = { A: int; B: int }"
                            ""
                            "let p = { A = 1; B = 2 }"
                        ]

                let artifact = compileSource "AttrRefEq" source
                let bytes = Codegen.toBytes artifact

                let rows = customAttributeRowsOn bytes "Pair"

                Expect.exists
                    rows
                    (fun (ctorDecl, _) -> ctorDecl = "ReferenceEqualityAttribute")
                    "the record carries a ReferenceEqualityAttribute row"

                let asm = loadAssembly bytes
                let ty = asm.GetType "Pair"
                Expect.isNotNull ty "the assembly contains Pair"

                let attr =
                    ty.GetCustomAttributesData()
                    |> Seq.tryFind (fun a -> a.AttributeType.FullName = "Vesper.ReferenceEqualityAttribute")

                Expect.isSome attr "reflection resolves the Vesper.ReferenceEqualityAttribute row"
            }

            // (c) Argument-bearing rows: a project-local attribute class round-trips its
            // string ctor argument, and its own `[<AttributeUsage>]` lands as the BCL
            // `System.AttributeUsageAttribute` with the enum fixed arg and the named
            // `AllowMultiple` property.
            test "an argument-bearing attribute round-trips its blob args" {
                let source =
                    String.concat
                        "\n"
                        [
                            "[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Method, AllowMultiple = false)>]"
                            "type MarkAttribute(name: string) ="
                            "    member _.Name = name"
                            ""
                            "[<Mark(\"hello\")>]"
                            "type Tagged() ="
                            "    [<Mark(\"on-member\")>]"
                            "    member _.M() = 2"
                            ""
                            "    member _.X = 1"
                        ]

                let artifact = compileSource "AttrArgs" source
                let bytes = Codegen.toBytes artifact
                let asm = loadAssembly bytes

                let tagged = asm.GetType "Tagged"
                Expect.isNotNull tagged "the assembly contains Tagged"

                let mark =
                    tagged.GetCustomAttributesData()
                    |> Seq.find (fun a -> a.AttributeType.Name = "MarkAttribute")

                Expect.equal mark.ConstructorArguments.Count 1 "Mark has one fixed arg"
                Expect.equal (mark.ConstructorArguments.[0].Value :?> string) "hello" "the string fixed arg round-trips"

                // A member-position row: the attribute lands on the member's MethodDef.
                let onMember =
                    tagged.GetMethod("M").GetCustomAttributesData()
                    |> Seq.find (fun a -> a.AttributeType.Name = "MarkAttribute")

                Expect.equal
                    (onMember.ConstructorArguments.[0].Value :?> string)
                    "on-member"
                    "the member's Mark row round-trips its arg"

                let markTy = asm.GetType "MarkAttribute"
                Expect.isNotNull markTy "the assembly contains MarkAttribute"

                let usage =
                    markTy.GetCustomAttributesData()
                    |> Seq.find (fun a -> a.AttributeType.FullName = "System.AttributeUsageAttribute")

                Expect.equal usage.ConstructorArguments.Count 1 "AttributeUsage has one fixed arg"

                Expect.equal
                    (Convert.ToInt32 usage.ConstructorArguments.[0].Value)
                    (int (AttributeTargets.Class ||| AttributeTargets.Method))
                    "the AttributeTargets fixed arg encodes as its underlying integral"

                let allowMultiple =
                    usage.NamedArguments |> Seq.find (fun n -> n.MemberName = "AllowMultiple")

                Expect.equal (allowMultiple.TypedValue.Value :?> bool) false "the named AllowMultiple round-trips"
            }

            // (d) The keyword-derived `IsByRefLike` row stays single: the `[<IsByRefLike>]`
            // spelling also lands in the frozen attribute list, and the generalised path
            // must not add a second row for it.
            test "a [<Struct; IsByRefLike>] type carries exactly one IsByRefLike row" {
                let artifact = compileSourceData "RefStructShape"

                let byRefLikeRows =
                    customAttributeRowsOn (Codegen.toBytes artifact) "RPoint"
                    |> List.filter (fun (ctorDecl, _) -> ctorDecl = "IsByRefLikeAttribute")

                Expect.equal (List.length byRefLikeRows) 1 "one IsByRefLike row, no duplicate"
            }

            // (e) A named enum-typed argument encodes II.23.3's enum form: `FieldOrPropType`
            // `0x55` + the enum type's SerString, then the value at the underlying width.
            test "a named enum-typed argument encodes as 0x55 + enum SerString" {
                let source =
                    String.concat
                        "\n"
                        [
                            "type Targets2 ="
                            "    | A = 1"
                            "    | B = 2"
                            ""
                            "type MarkAttribute() ="
                            "    member _.N = 1"
                            ""
                            "[<Mark(T = Targets2.B)>]"
                            "type Tagged2() ="
                            "    member _.X = 1"
                        ]

                let artifact = compileSource "AttrEnumNamed" source

                let blob =
                    customAttributeRowsOn (Codegen.toBytes artifact) "Tagged2"
                    |> List.pick (fun (ctorDecl, blob) -> if ctorDecl = "MarkAttribute" then Some blob else None)

                // A packed length below 128 is one byte.
                let serString (s: string) =
                    byte s.Length :: (Text.Encoding.UTF8.GetBytes s |> List.ofArray)

                Expect.equal
                    (List.ofArray blob)
                    [
                        yield! [ 1uy; 0uy ] // prolog
                        yield! [ 1uy; 0uy ] // named-argument count
                        yield 0x54uy // PROPERTY
                        yield 0x55uy // FieldOrPropType: enum
                        yield! serString "Targets2"
                        yield! serString "T"
                        yield! [ 2uy; 0uy; 0uy; 0uy ] // Targets2.B at the underlying int32 width
                    ]
                    "the named enum argument carries the enum's SerString"
            }

            // (f) A `typeof<T>` / `typedefof<T>` argument encodes as II.23.3's `Type` element
            // (`0x50`) plus the type's name: assembly-qualified for a referenced type, the
            // full name alone for a type of the assembly under emission.
            test "a reified-type argument encodes as the 0x50 Type element" {
                let source =
                    String.concat
                        "\n"
                        [
                            "type MarkAttribute(t: Type) ="
                            "    member _.T = t"
                            ""
                            "type Box<'T> = { Item: 'T }"
                            ""
                            "[<Mark(typeof<int>)>]"
                            "type TaggedPrim() ="
                            "    member _.X = 1"
                            ""
                            "[<Mark(typedefof<Box<int>>)>]"
                            "type TaggedLocal() ="
                            "    member _.X = 1"
                        ]

                let artifact = compileSource "AttrTypeElem" source
                let bytes = Codegen.toBytes artifact

                let markBlob (typeName: string) =
                    customAttributeRowsOn bytes typeName
                    |> List.pick (fun (ctorDecl, blob) -> if ctorDecl = "MarkAttribute" then Some blob else None)

                // A packed length below 128 is one byte.
                let serString (s: string) =
                    byte s.Length :: (Text.Encoding.UTF8.GetBytes s |> List.ofArray)

                Expect.equal
                    (List.ofArray (markBlob "TaggedLocal"))
                    [
                        yield! [ 1uy; 0uy ] // prolog
                        yield! serString "Box`1" // the fixed Type argument's name
                        yield! [ 0uy; 0uy ] // named-argument count
                    ]
                    "a locally emitted type is spelled by full name alone"

                // The positional `Type` element writes its SerString bare: the fixed-argument
                // encoding follows the ctor's parameter type, as an enum does.
                Expect.stringContains
                    (Text.Encoding.UTF8.GetString(markBlob "TaggedPrim"))
                    "System.Int32"
                    "a BCL primitive operand is spelled by its platform type name"

                let asm = loadAssembly bytes

                let markArg (typeName: string) =
                    let attr =
                        (asm.GetType typeName).GetCustomAttributesData()
                        |> Seq.find (fun a -> a.AttributeType.Name = "MarkAttribute")

                    attr.ConstructorArguments.[0].Value :?> Type

                Expect.equal (markArg "TaggedPrim").FullName "System.Int32" "reflection resolves the reified primitive"

                Expect.equal
                    (markArg "TaggedLocal").FullName
                    "Box`1"
                    "reflection resolves the reified local generic definition"
            }

            // (g) A module-held enum emits nested in the module's class, so its `0x55`
            // SerString is the layout's reflection name, which reflection resolves. The
            // module shares its name with a type, so its class is `KindsModule` rather than
            // the key's own `Kinds+Targets3` spelling.
            test "a named argument typed by a module-held enum carries the nested reflection name" {
                let source =
                    String.concat
                        "\n"
                        [
                            "type Kinds() ="
                            "    member _.X = 1"
                            ""
                            "module Kinds ="
                            "    type Targets3 ="
                            "        | A = 1"
                            "        | B = 2"
                            ""
                            "type MarkAttribute(t0: Kinds.Targets3) ="
                            "    let mutable t = t0"
                            "    member _.T with get () = t and set (v: Kinds.Targets3) = t <- v"
                            ""
                            "[<Mark(Kinds.Targets3.A, T = Kinds.Targets3.B)>]"
                            "type Tagged3() ="
                            "    member _.X = 1"
                        ]

                let artifact = compileSource "AttrEnumNested" source
                let bytes = Codegen.toBytes artifact

                let blob =
                    customAttributeRowsOn bytes "Tagged3"
                    |> List.pick (fun (ctorDecl, blob) -> if ctorDecl = "MarkAttribute" then Some blob else None)

                Expect.stringContains
                    (Text.Encoding.UTF8.GetString blob)
                    "KindsModule+Targets3"
                    "the enum SerString is the nested reflection name"

                let asm = loadAssembly bytes

                let named =
                    (asm.GetType "Tagged3").GetCustomAttributesData()
                    |> Seq.find (fun a -> a.AttributeType.Name = "MarkAttribute")
                    |> fun a -> a.NamedArguments |> Seq.find (fun n -> n.MemberName = "T")

                Expect.equal
                    named.TypedValue.ArgumentType.FullName
                    "KindsModule+Targets3"
                    "reflection resolves the enum through the module class"
            }

            // (i) An array argument encodes II.23.3's `SZARRAY` form: a `uint32` count then
            // each item's `Elem`. A positional array follows the ctor's parameter type; a
            // named array writes `0x1D` + the item's `FieldOrPropType` first.
            test "array arguments encode as count-prefixed elements" {
                let source =
                    String.concat
                        "\n"
                        [
                            "type Targets4 ="
                            "    | A = 1"
                            "    | B = 2"
                            "    | C = 4"
                            ""
                            "type MarkAttribute(xs: int[], es: Targets4[]) ="
                            "    let mutable named: int[] = [||]"
                            "    member _.Xs = xs"
                            "    member _.Es = es"
                            "    member _.Named with get () = named and set (v: int[]) = named <- v"
                            ""
                            "[<Mark([| 1; 2 |], [| Targets4.A; Targets4.B ||| Targets4.C |], Named = [| 3; 4 |])>]"
                            "type Tagged4() ="
                            "    member _.X = 1"
                        ]

                let artifact = compileSource "AttrArrayArgs" source
                let bytes = Codegen.toBytes artifact

                let blob =
                    customAttributeRowsOn bytes "Tagged4"
                    |> List.pick (fun (ctorDecl, blob) -> if ctorDecl = "MarkAttribute" then Some blob else None)

                let serString (s: string) =
                    byte s.Length :: (Text.Encoding.UTF8.GetBytes s |> List.ofArray)

                let int32Bytes (n: int) = List.ofArray (BitConverter.GetBytes n)

                Expect.equal
                    (List.ofArray blob)
                    [
                        yield! [ 1uy; 0uy ] // prolog
                        yield! int32Bytes 2 // xs: count
                        yield! int32Bytes 1
                        yield! int32Bytes 2
                        yield! int32Bytes 2 // es: count
                        yield! int32Bytes 1 // Targets4.A at the underlying int32 width
                        yield! int32Bytes 6 // Targets4.B ||| Targets4.C
                        yield! [ 1uy; 0uy ] // named-argument count
                        yield 0x54uy // PROPERTY
                        yield! [ 0x1Duy; 0x08uy ] // FieldOrPropType: SZARRAY of I4
                        yield! serString "Named"
                        yield! int32Bytes 2 // count
                        yield! int32Bytes 3
                        yield! int32Bytes 4
                    ]
                    "each array is its count then its items; only the named one spells its type"

                let asm = loadAssembly bytes

                let mark =
                    (asm.GetType "Tagged4").GetCustomAttributesData()
                    |> Seq.find (fun a -> a.AttributeType.Name = "MarkAttribute")

                let items (arg: CustomAttributeTypedArgument) : int list =
                    let elements =
                        arg.Value :?> Collections.ObjectModel.ReadOnlyCollection<CustomAttributeTypedArgument>

                    [ for item in elements -> Convert.ToInt32 item.Value ]

                Expect.equal (items mark.ConstructorArguments.[0]) [ 1; 2 ] "reflection reads the int[] fixed arg"

                Expect.equal
                    (items mark.ConstructorArguments.[1])
                    [ 1; 6 ]
                    "reflection reads the enum[] fixed arg at the underlying width"

                Expect.equal
                    (mark.ConstructorArguments.[1].ArgumentType.GetElementType().Name)
                    "Targets4"
                    "the enum[] fixed arg is typed by the ctor's parameter"

                let named = mark.NamedArguments |> Seq.find (fun n -> n.MemberName = "Named")
                Expect.equal (items named.TypedValue) [ 3; 4 ] "reflection reads the named int[] arg"
            }

            // (h) A defective row does not vanish silently: the reason lands on the artifact
            // and the compile still succeeds.
            test "an ambiguous attribute ctor skips the row with a reason on the artifact" {
                let source =
                    String.concat
                        "\n"
                        [
                            "type AmbAttribute(n: int) ="
                            "    member _.N = n"
                            "    new(s: string) = AmbAttribute(1)"
                            ""
                            "[<Amb(3)>]"
                            "type Tagged3() ="
                            "    member _.X = 1"
                        ]

                let artifact = compileSource "AttrAmbiguous" source

                let ambRows =
                    customAttributeRowsOn (Codegen.toBytes artifact) "Tagged3"
                    |> List.filter (fun (ctorDecl, _) -> ctorDecl = "AmbAttribute")

                Expect.isEmpty ambRows "no row for the unresolvable ctor"

                match artifact.SkippedAttributeRows with
                | [ skip ] ->
                    Expect.equal
                        (SymbolKeyOps.typeMetaName skip.AttributeKey)
                        "AmbAttribute"
                        "the skip names the attribute"

                    Expect.stringContains skip.Parent "Tagged3" "the skip names the parent element"

                    Expect.equal
                        skip.Reason
                        (SkippedAttributeRowReason.AmbiguousCtor 1)
                        "the skip states the ambiguity and the arity"
                | other -> failtestf "expected exactly one skip, got %A" other
            }

            ptest "GAP: union-case and enum-case attribute rows are unemitted" {
                // The frozen tree carries these attributes (AttributeFoldTests pins it); no
                // metadata parent row exists for them, so no row is emitted.
                let source =
                    String.concat
                        "\n"
                        [
                            "type MarkAttribute(n: int) ="
                            "    member _.N = n"
                            ""
                            "type Shape = | [<Mark(1)>] Circle of int"
                            ""
                            "let s = Circle 3"
                        ]

                let artifact = compileSource "AttrCaseRows" source
                use pe = openPe (Codegen.toBytes artifact)
                let md = pe.GetMetadataReader()

                let markRows =
                    [
                        for h in md.CustomAttributes do
                            let ca = md.GetCustomAttribute h

                            if ca.Constructor.Kind = HandleKind.MethodDefinition then
                                let m = md.GetMethodDefinition(MethodDefinitionHandle.op_Explicit ca.Constructor)

                                if md.GetString (md.GetTypeDefinition(m.GetDeclaringType())).Name = "MarkAttribute" then
                                    yield ca
                    ]

                Expect.isNonEmpty markRows "the union case's Mark lands on a metadata row"
            }

            ptest "GAP: a property member's attribute row lands on its get_ method, not a Property row" {
                // The emitter parents a property attribute on the `get_` MethodDef, the only
                // row the property emits; .NET tooling reads e.g. `[<Obsolete>]` off the
                // Property row.
                let source =
                    String.concat
                        "\n"
                        [
                            "type MarkAttribute(n: int) ="
                            "    member _.N = n"
                            ""
                            "type Holder() ="
                            "    [<Mark(7)>]"
                            "    member _.X = 1"
                        ]

                let artifact = compileSource "AttrPropRows" source
                let holder = (loadAssembly (Codegen.toBytes artifact)).GetType "Holder"

                let onProperty =
                    holder.GetProperty("X").GetCustomAttributesData()
                    |> Seq.filter (fun a -> a.AttributeType.Name = "MarkAttribute")

                Expect.isNonEmpty (List.ofSeq onProperty) "the property's Mark sits on the Property row"
            }
        ]
