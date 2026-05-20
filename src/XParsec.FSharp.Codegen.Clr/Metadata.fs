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

    /// The `abstract sealed` (static) holder class for top-level members,
    /// owning every method from `firstMethod` onward.
    member _.AddProgramType
        (name: string, baseType: EntityHandle, firstMethod: MethodDefinitionHandle)
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
            MetadataTokens.FieldDefinitionHandle(1),
            firstMethod
        )

    /// Serialise the assembled metadata + IL into a PE image.
    member _.Serialize(entryPoint: MethodDefinitionHandle) : BlobBuilder =
        let header = PEHeaderBuilder(imageCharacteristics = Characteristics.ExecutableImage)
        let root = MetadataRootBuilder(mb)
        let peBuilder = ManagedPEBuilder(header, root, ilBuilder, entryPoint = entryPoint)
        let peBlob = BlobBuilder()
        peBuilder.Serialize(peBlob) |> ignore
        peBlob
