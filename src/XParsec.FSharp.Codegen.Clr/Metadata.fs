namespace XParsec.FSharp.Codegen.Clr

open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable

// Ordered construction of the metadata graph over `MetadataBuilder` +
// `BlobBuilder`.

[<AutoOpen>]
module internal Handles =

    /// Widen a metadata handle to `EntityHandle`. Explicit, because F#'s
    /// implicit-conversion warning is an error here; the SRTP call picks the
    /// `-> EntityHandle` overload over `-> Handle`.
    let inline toEntity (h: ^T) : EntityHandle =
        (^T: (static member op_Implicit: ^T -> EntityHandle) h)

type MetadataContext() =
    let mb = MetadataBuilder()
    let ilBuilder = BlobBuilder()
    let asmRefs = Dictionary<string, AssemblyReferenceHandle>()
    let asmRefNames = HashSet<string>()
    let typeRefs = Dictionary<struct (int * string * string), TypeReferenceHandle>()

    member _.Builder = mb
    member _.IlBuilder = ilBuilder

    /// Simple names of every assembly an `AssemblyRef` row was minted for, which is the
    /// emitted PE's actual reference set.
    member _.ReferencedAssemblyNames: string list = List.ofSeq asmRefNames

    /// Cheap to recreate per body because the struct just wraps `ilBuilder`.
    member _.BodyStream = MethodBodyStreamEncoder(ilBuilder)

    // Handle = row number = add order, so the next row's handle derives from the
    // builder's own row count; a caller-side parallel counter drifts from the adds.
    member _.FieldRowCount: int = mb.GetRowCount(TableIndex.Field)

    member _.PropertyRowCount: int = mb.GetRowCount(TableIndex.Property)

    /// The handle the *next* `AddParameter` will return (a method's `ParamList`
    /// start; past-the-end for a zero-parameter method).
    member _.NextParamHandle: ParameterHandle =
        MetadataTokens.ParameterHandle(mb.GetRowCount(TableIndex.Param) + 1)

    member _.UserString(s: string) : UserStringHandle = mb.GetOrAddUserString(s)
    member _.String(s: string) : StringHandle = mb.GetOrAddString(s)
    member _.Blob(b: BlobBuilder) : BlobHandle = mb.GetOrAddBlob(b)

    /// Module + assembly rows. Must be added before the table rows that
    /// reference them; call once, first.
    member _.AddModuleAndAssembly(name: string) : unit =
        mb.AddModule(
            0,
            mb.GetOrAddString(name + ".dll"),
            mb.GetOrAddGuid(Guid.NewGuid()),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore

        mb.AddAssembly(
            mb.GetOrAddString(name),
            Version(1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            enum<AssemblyFlags> 0,
            AssemblyHashAlgorithm.Sha1
        )
        |> ignore

    /// Identified by the loaded assembly's name / version / public-key token, so
    /// the emitted reference matches whatever is loaded in the host process.
    member _.AssemblyRef(name: System.Reflection.AssemblyName) : AssemblyReferenceHandle =
        let key = name.FullName
        asmRefNames.Add name.Name |> ignore

        match asmRefs.TryGetValue key with
        | true, h -> h
        | _ ->
            let pkt = name.GetPublicKeyToken()

            let pktBlob =
                if isNull pkt || pkt.Length = 0 then
                    Unchecked.defaultof<BlobHandle>
                else
                    mb.GetOrAddBlob(pkt)

            let h =
                mb.AddAssemblyReference(
                    mb.GetOrAddString(name.Name),
                    (if isNull name.Version then
                         Version(0, 0, 0, 0)
                     else
                         name.Version),
                    Unchecked.defaultof<StringHandle>,
                    pktBlob,
                    enum<AssemblyFlags> 0,
                    Unchecked.defaultof<BlobHandle>
                )

            asmRefs[key] <- h
            h

    member _.TypeRef(scope: EntityHandle, ns: string, name: string) : TypeReferenceHandle =
        let key = struct (MetadataTokens.GetToken scope, ns, name)

        match typeRefs.TryGetValue key with
        | true, h -> h
        | _ ->
            let h = mb.AddTypeReference(scope, mb.GetOrAddString(ns), mb.GetOrAddString(name))
            typeRefs[key] <- h
            h

    member _.MemberRef(parent: EntityHandle, name: string, signature: BlobBuilder) : MemberReferenceHandle =
        mb.AddMemberReference(parent, mb.GetOrAddString(name), mb.GetOrAddBlob(signature))

    member _.TypeSpec(signature: BlobBuilder) : TypeSpecificationHandle =
        mb.AddTypeSpecification(mb.GetOrAddBlob(signature))

    member _.AddStandaloneSignature(signature: BlobBuilder) : StandaloneSignatureHandle =
        mb.AddStandaloneSignature(mb.GetOrAddBlob(signature))

    member _.MethodSpec(meth: EntityHandle, instantiation: BlobBuilder) : MethodSpecificationHandle =
        mb.AddMethodSpecification(meth, mb.GetOrAddBlob(instantiation))

    /// `ctor` is a `.ctor` `MemberRef`/`MethodDef`; `value` is the serialised argument
    /// blob; a parameterless attribute is `01 00 00 00` (prolog `0x0001`, zero named
    /// args). SRM sorts the table by parent, so add order is free.
    member _.AddCustomAttribute(owner: EntityHandle, ctor: EntityHandle, value: BlobBuilder) : CustomAttributeHandle =
        mb.AddCustomAttribute(owner, ctor, mb.GetOrAddBlob(value))

    /// Fields must be added in the order the owning types claim them (each
    /// `TypeDefinition`'s field range runs from its `firstField` to the next type's).
    member _.AddField(attrs: FieldAttributes, name: string, signature: BlobBuilder) : FieldDefinitionHandle =
        mb.AddFieldDefinition(attrs, mb.GetOrAddString(name), mb.GetOrAddBlob(signature))

    /// A `Constant` row for a `[<Literal>]` static field, namely an enum case carrying its
    /// underlying integer. `value` is that primitive, boxed (`int`/`byte`/`uint32`/`int64`);
    /// its runtime type is what SRM maps to the `ConstantTypeCode`. Add order is free.
    member _.AddConstant(parent: EntityHandle, value: obj) : ConstantHandle = mb.AddConstant(parent, value)

    /// Methods must be added in the order types will claim them.
    member _.AddMethod
        (attrs: MethodAttributes, name: string, signature: BlobBuilder, bodyOffset: int)
        : MethodDefinitionHandle =
        mb.AddMethodDefinition(
            attrs,
            MethodImplAttributes.IL,
            mb.GetOrAddString(name),
            mb.GetOrAddBlob(signature),
            bodyOffset,
            Unchecked.defaultof<ParameterHandle>
        )

    /// Add a `Param` row (`sequenceNumber` 1-based; 0 is the return parameter).
    /// A method's `Param` rows must be added in sequence order, before the
    /// `MethodDefinition` that claims them via its `ParamList`.
    member _.AddParameter(sequenceNumber: int, name: string) : ParameterHandle =
        mb.AddParameter(
            ParameterAttributes.None,
            (if String.IsNullOrEmpty name then
                 Unchecked.defaultof<StringHandle>
             else
                 mb.GetOrAddString(name)),
            sequenceNumber
        )

    /// Records a valid `ParamList` range (the first `Param` row, or past-the-end
    /// when none). `AddMethod`'s nil `ParamList` still *executes* a body, but reflection's
    /// `GetParameters` cannot see the parameters; this round-trips through reflection.
    member _.AddMethodWithParamList
        (attrs: MethodAttributes, name: string, signature: BlobBuilder, bodyOffset: int, firstParam: ParameterHandle)
        : MethodDefinitionHandle =
        mb.AddMethodDefinition(
            attrs,
            MethodImplAttributes.IL,
            mb.GetOrAddString(name),
            mb.GetOrAddBlob(signature),
            bodyOffset,
            firstParam
        )

    /// Properties must be added in the order the owning types claim them, because a
    /// `PropertyMap` row's `PropertyList` runs to the next map row's.
    member _.AddProperty(name: string, signature: BlobBuilder) : PropertyDefinitionHandle =
        mb.AddProperty(PropertyAttributes.None, mb.GetOrAddString(name), mb.GetOrAddBlob(signature))

    /// Claims `firstProperty` onward for `declaringType`. SRM neither sorts nor validates
    /// this table, so callers must add rows in ascending `declaringType` order; a type
    /// declaring no property gets NO row, rather than an empty range.
    member _.AddPropertyMap(declaringType: TypeDefinitionHandle, firstProperty: PropertyDefinitionHandle) : unit =
        mb.AddPropertyMap(declaringType, firstProperty)

    /// Binds an accessor to the property it half-implements. SRM sorts the table by
    /// `Association`, so add order is free.
    member _.AddMethodSemantics
        (association: EntityHandle, semantics: MethodSemanticsAttributes, accessor: MethodDefinitionHandle)
        : unit =
        mb.AddMethodSemantics(association, semantics, accessor)

    /// The mandatory `<Module>` pseudo-type (table row 1). `firstMethod`
    /// points at the first real method so its own method range stays empty.
    member _.AddModuleType(firstMethod: MethodDefinitionHandle) : unit =
        mb.AddTypeDefinition(
            enum<TypeAttributes> 0,
            Unchecked.defaultof<StringHandle>,
            mb.GetOrAddString("<Module>"),
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle(1),
            firstMethod
        )
        |> ignore

    /// A concrete `TypeDefinition` with an arbitrary base; `ns` empty ⇒ global.
    /// Callers must add this type's fields and methods (in type order) BEFORE the
    /// `TypeDefinition` row, because `firstField` / `firstMethod` start its contiguous ranges.
    member _.AddClass
        (
            attrs: TypeAttributes,
            ns: string,
            name: string,
            baseType: EntityHandle,
            firstField: FieldDefinitionHandle,
            firstMethod: MethodDefinitionHandle
        ) : TypeDefinitionHandle =
        mb.AddTypeDefinition(
            attrs,
            (if String.IsNullOrEmpty ns then
                 Unchecked.defaultof<StringHandle>
             else
                 mb.GetOrAddString(ns)),
            mb.GetOrAddString(name),
            baseType,
            firstField,
            firstMethod
        )

    /// An interface `TypeDefinition` with a **nil base** (interfaces have none); `ns` empty ⇒
    /// global. An interface has no fields, so `firstField` points past any preceding rows.
    member _.AddInterfaceType
        (
            attrs: TypeAttributes,
            ns: string,
            name: string,
            firstField: FieldDefinitionHandle,
            firstMethod: MethodDefinitionHandle
        ) : TypeDefinitionHandle =
        mb.AddTypeDefinition(
            attrs,
            (if String.IsNullOrEmpty ns then
                 Unchecked.defaultof<StringHandle>
             else
                 mb.GetOrAddString(ns)),
            mb.GetOrAddString(name),
            Unchecked.defaultof<EntityHandle>,
            firstField,
            firstMethod
        )

    /// The `NestedClass` row is where nesting lives: a nested `TypeDef` spells one bare
    /// name segment and an empty namespace column (`Ns.Outer+Inner` is a reflection DISPLAY
    /// convention). SRM validates the table is sorted by the NESTED handle.
    member _.AddNestedType(nested: TypeDefinitionHandle, enclosing: TypeDefinitionHandle) : unit =
        mb.AddNestedType(nested, enclosing)

    /// `interfaceType` is a `TypeDef` / `TypeRef` / `TypeSpec` (a closure's instantiated
    /// `Vesper.Fun\`2<a,b>`). SRM validates the `InterfaceImpl` table is sorted by the
    /// `Class` column, so callers must add rows in ascending `typeDef` order.
    member _.AddInterfaceImplementation(typeDef: TypeDefinitionHandle, interfaceType: EntityHandle) : unit =
        mb.AddInterfaceImplementation(typeDef, interfaceType) |> ignore

    /// `owner` is a `TypeDefinition` or `MethodDefinition`. SRM requires `GenericParam`
    /// rows globally sorted by `CodedIndex.TypeOrMethodDef(owner)` then `index`, and the
    /// two owner kinds interleave, so collect every row and sort before adding.
    member _.AddGenericParameter(owner: EntityHandle, index: int, name: string) : GenericParameterHandle =
        mb.AddGenericParameter(owner, GenericParameterAttributes.None, mb.GetOrAddString(name), index)

    member _.Serialize(entryPoint: MethodDefinitionHandle) : BlobBuilder =
        let header = PEHeaderBuilder(imageCharacteristics = Characteristics.ExecutableImage)
        let root = MetadataRootBuilder(mb)
        let peBuilder = ManagedPEBuilder(header, root, ilBuilder, entryPoint = entryPoint)
        let peBlob = BlobBuilder()
        peBuilder.Serialize(peBlob) |> ignore
        peBlob

    /// Serialise as a *library* PE: DLL characteristics, **no entry point**.
    member _.SerializeLibrary() : BlobBuilder =
        let header =
            PEHeaderBuilder(imageCharacteristics = (Characteristics.ExecutableImage ||| Characteristics.Dll))

        let root = MetadataRootBuilder(mb)
        // No `entryPoint` ⇒ the PE has no managed entry point (a library).
        let peBuilder = ManagedPEBuilder(header, root, ilBuilder)
        let peBlob = BlobBuilder()
        peBuilder.Serialize(peBlob) |> ignore
        peBlob
