namespace XParsec.FSharp.Codegen.Clr

open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable

// The metadata-construction layer (no LicenseToCIL analog, no stack typing):
// ordered construction of the metadata graph over `MetadataBuilder` +
// `BlobBuilder`. See [codegen-clr-plan](../XParsec.FSharp.SemanticAnalysis/docs/codegen-clr-plan.md) §B.

[<AutoOpen>]
module internal Handles =

    /// Widen any specific metadata handle to `EntityHandle` via its
    /// `op_Implicit`. SRTP picks the `-> EntityHandle` overload (each handle
    /// struct also has one to `Handle`); doing it explicitly keeps us off F#'s
    /// implicit-conversion warning, which `TreatWarningsAsErrors` would fail.
    let inline toEntity (h: ^T) : EntityHandle =
        (^T: (static member op_Implicit: ^T -> EntityHandle) h)

/// In-memory metadata + IL builders plus reference caches. Deeply stateful
/// (the SRM writers are), but the mutation never leaks past `compile`.
type MetadataContext() =
    let mb = MetadataBuilder()
    let ilBuilder = BlobBuilder()
    let asmRefs = Dictionary<string, AssemblyReferenceHandle>()
    let typeRefs = Dictionary<struct (int * string * string), TypeReferenceHandle>()

    member _.Builder = mb
    member _.IlBuilder = ilBuilder

    /// A `MethodBodyStreamEncoder` over the shared IL builder. Cheap to
    /// recreate per body — the struct just wraps `ilBuilder`.
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

    /// Cached `AssemblyReference`, identified by the loaded assembly's name /
    /// version / public-key token so the emitted reference matches whatever is
    /// loaded in the host process (robust for in-process execution).
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

    /// Cached `TypeReference` within `scope` (an assembly ref, normally).
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

    /// A standalone signature row — used for a method body's local-variable
    /// signature (built with `BlobEncoder.LocalVariableSignature`).
    member _.AddStandaloneSignature(signature: BlobBuilder) : StandaloneSignatureHandle =
        mb.AddStandaloneSignature(mb.GetOrAddBlob(signature))

    member _.MethodSpec(meth: EntityHandle, instantiation: BlobBuilder) : MethodSpecificationHandle =
        mb.AddMethodSpecification(meth, mb.GetOrAddBlob(instantiation))

    /// Add a `FieldDefinition` row. Like methods, fields must be added in the
    /// order the owning types claim them (each `TypeDefinition`'s field range
    /// runs from its `firstField` to the next type's). Used for closure
    /// capture fields.
    member _.AddField(attrs: FieldAttributes, name: string, signature: BlobBuilder) : FieldDefinitionHandle =
        mb.AddFieldDefinition(attrs, mb.GetOrAddString(name), mb.GetOrAddBlob(signature))

    /// Add a `MethodDefinition` row. Methods must be added in the order types
    /// will claim them (see `AddProgramType`).
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

    /// Add a `MethodDefinition` whose `ParamList` points at `firstParam` (the
    /// first of its `Param` rows, or the past-the-end handle when it has none).
    /// Unlike `AddMethod` — which passes a nil `ParamList`, fine for *executing*
    /// a body but not for reflection's `GetParameters` — this records a valid
    /// parameter range, so the emitted method round-trips through reflection.
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

    /// A concrete (instance) `TypeDefinition` whose base is an arbitrary
    /// `EntityHandle` — a `TypeSpec` for a synthesised closure deriving from
    /// the instantiated `FSharpFunc\`2<a,b>`, or `Object` for an emitted union.
    /// Generalises `AddProgramType`, which hard-codes `abstract sealed` +
    /// `Object`. `ns` is the namespace (empty ⇒ global — a synthesised closure
    /// has none; a declared union carries its `namespace X`). `firstField` /
    /// `firstMethod` start this type's contiguous field / method ranges, so
    /// callers must add this type's fields and methods (in type order) before
    /// the `TypeDefinition` rows.
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

    /// The `abstract sealed` (static) holder class for top-level members,
    /// owning every method from `firstMethod` onward. `firstField` points past
    /// any closure fields that precede it in the table (the holder owns none),
    /// so its field range stays empty.
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

    /// A generic interface `TypeDefinition` — `Interface ||| Abstract ||| Public`,
    /// **nil base** (interfaces have no base type). `ns` is the namespace (empty ⇒
    /// global). `firstField` / `firstMethod` start its contiguous ranges (an
    /// interface has no fields, so `firstField` points past any preceding rows).
    /// Generic-parameter rows are added separately afterwards via
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

    /// A `GenericParam` row for `owner`'s type parameter at `index` (0-based).
    /// `owner` is a `TypeDefinition` (a type's typars) or a `MethodDefinition` (a
    /// generic method's own typars). SRM requires all `GenericParam` rows added
    /// globally sorted by `CodedIndex.TypeOrMethodDef(owner)` then by `index`
    /// (and validates this on serialize) — type and method owners therefore
    /// interleave, so callers must collect every row and sort before adding.
    member _.AddGenericParameter(owner: EntityHandle, index: int, name: string) : GenericParameterHandle =
        mb.AddGenericParameter(owner, GenericParameterAttributes.None, mb.GetOrAddString(name), index)

    /// Serialise the assembled metadata + IL into a PE image.
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
