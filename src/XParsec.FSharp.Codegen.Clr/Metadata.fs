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

    /// Widen a specific metadata handle to `EntityHandle`. Done explicitly to
    /// stay off F#'s implicit-conversion warning, which `TreatWarningsAsErrors`
    /// would fail; SRTP picks the `-> EntityHandle` overload over `-> Handle`.
    let inline toEntity (h: ^T) : EntityHandle =
        (^T: (static member op_Implicit: ^T -> EntityHandle) h)

/// Deeply stateful (the SRM writers are), but the mutation never leaks past
/// `compile`.
type MetadataContext() =
    let mb = MetadataBuilder()
    let ilBuilder = BlobBuilder()
    let asmRefs = Dictionary<string, AssemblyReferenceHandle>()
    let typeRefs = Dictionary<struct (int * string * string), TypeReferenceHandle>()

    member _.Builder = mb
    member _.IlBuilder = ilBuilder

    /// Cheap to recreate per body — the struct just wraps `ilBuilder`.
    member _.BodyStream = MethodBodyStreamEncoder(ilBuilder)

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
    member _.AssemblyRef(name: AssemblyName) : AssemblyReferenceHandle =
        let key = name.FullName

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

    /// Fields must be added in the order the owning types claim them (each
    /// `TypeDefinition`'s field range runs from its `firstField` to the next type's).
    member _.AddField(attrs: FieldAttributes, name: string, signature: BlobBuilder) : FieldDefinitionHandle =
        mb.AddFieldDefinition(attrs, mb.GetOrAddString(name), mb.GetOrAddBlob(signature))

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
    /// when none). Unlike `AddMethod`'s nil `ParamList` — fine for *executing* a
    /// body but not for reflection's `GetParameters` — this round-trips through reflection.
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

    /// A concrete `TypeDefinition` with an arbitrary base (a closure's
    /// instantiated `FSharpFunc\`2` `TypeSpec`, or `Object` for a union). `ns`
    /// empty ⇒ global. Callers must add this type's fields and methods (in type
    /// order) before the `TypeDefinition` row, since `firstField` / `firstMethod`
    /// start its contiguous ranges.
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

    /// The `abstract sealed` (static) holder for top-level members. `firstField`
    /// points past any preceding closure fields (the holder owns none), so its
    /// field range stays empty.
    member _.AddProgramType
        (name: string, baseType: EntityHandle, firstField: FieldDefinitionHandle, firstMethod: MethodDefinitionHandle)
        : TypeDefinitionHandle =
        mb.AddTypeDefinition(
            TypeAttributes.Class
            ||| TypeAttributes.Public
            ||| TypeAttributes.Abstract
            ||| TypeAttributes.Sealed
            ||| TypeAttributes.AutoLayout
            ||| TypeAttributes.BeforeFieldInit,
            Unchecked.defaultof<StringHandle>,
            mb.GetOrAddString(name),
            baseType,
            firstField,
            firstMethod
        )

    /// An interface `TypeDefinition` — **nil base** (interfaces have none). `ns`
    /// empty ⇒ global. An interface has no fields, so `firstField` points past
    /// any preceding rows. Generic-parameter rows are added separately via
    /// `AddGenericParameter`.
    member _.AddInterfaceType
        (ns: string, name: string, firstField: FieldDefinitionHandle, firstMethod: MethodDefinitionHandle)
        : TypeDefinitionHandle =
        mb.AddTypeDefinition(
            TypeAttributes.Interface ||| TypeAttributes.Abstract ||| TypeAttributes.Public,
            (if String.IsNullOrEmpty ns then
                 Unchecked.defaultof<StringHandle>
             else
                 mb.GetOrAddString(ns)),
            mb.GetOrAddString(name),
            Unchecked.defaultof<EntityHandle>,
            firstField,
            firstMethod
        )

    /// `owner` is a `TypeDefinition` or `MethodDefinition`. SRM requires all
    /// `GenericParam` rows globally sorted by `CodedIndex.TypeOrMethodDef(owner)`
    /// then `index` (validated on serialize) — type and method owners interleave,
    /// so callers must collect every row and sort before adding.
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
