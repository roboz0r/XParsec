namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Generic

// The tables a compilation unit's published surface is ACCUMULATED in, addressed by identity
// rather than by a rendering of one. Both halves of a unit fill one: a `.fs` projected from
// its frozen pools, a `.fsi` from its resolved signatures.

/// The form in which a declaration states that the target supplies a type's representation.
/// It fixes what the paired implementation's `(# … #)` binding must be, which the published
/// shape alone does not: a capability on a target binding no repr publishes as a plain class.
[<RequireQualifiedAccess>]
type DeclaredRepr =
    /// `type X = extern` / `type X = (# "…" #)` — an opaque value repr.
    | Opaque
    /// `type X = extern class` / `type X = (# class "…" #)` — a heritable external base a
    /// later `inherit` may name. `IsHeritable` is what the implementation's `(# class … #)`
    /// tag must agree with.
    | Heritable
    /// `type X = extern interface with …` — a capability anchor. Its repr names a platform
    /// interface, which is an opaque value repr.
    | Capability

type PublishedSurfaceBuilder =
    {
        ShapesByKey: Dictionary<TypeKey, ExternalTypeShape>
        /// Canonical intrinsic identity -> the representation form declared for it.
        DeclaredReprs: Dictionary<TypeKey, DeclaredRepr>
        /// A type's FULL member list, in DECLARATION order: the overload scan depends on it.
        MembersByKey: Dictionary<TypeKey, ResizeArray<ExternalMember>>
        /// Canonical compiled name -> the registered identity. First declaration wins.
        TypesByName: Dictionary<string, TypeKey>
        /// Dotted source path of a declared module -> the container a type it holds sits in.
        /// What makes a written `A.M.T` reach the type compiled as `A.M+T`.
        ModuleContainers: Dictionary<string, TypeContainer>
        /// Bare case name -> the union declaring it. First declaration wins.
        UnionCases: Dictionary<string, ExternalUnionCase>
        /// Field name -> every record declaring it, a MULTIMAP rather than first-wins: a field
        /// name is deliberately shared across records, so each one ADDS a candidate.
        RecordFields: Dictionary<string, ResizeArray<ExternalRecordCandidate>>
        /// Values, keyed as a binding key renders: `.`-joined.
        Symbols: Dictionary<string, ExternalSymbol>
        /// Prefixes a consumer resolves through with no `open` of its own.
        mutable AmbientOpenPrefixes: string list
    }

[<RequireQualifiedAccess>]
module PublishedSurfaceBuilder =

    let create () : PublishedSurfaceBuilder =
        {
            ShapesByKey = Dictionary()
            DeclaredReprs = Dictionary()
            MembersByKey = Dictionary()
            TypesByName = Dictionary(StringComparer.Ordinal)
            ModuleContainers = Dictionary(StringComparer.Ordinal)
            UnionCases = Dictionary(StringComparer.Ordinal)
            RecordFields = Dictionary(StringComparer.Ordinal)
            Symbols = Dictionary(StringComparer.Ordinal)
            AmbientOpenPrefixes = []
        }

    /// Index `m` and every module enclosing it, so a type held anywhere down the chain is
    /// reachable by the dotted path its source writes.
    let rec addModuleContainer (surface: PublishedSurfaceBuilder) (m: ModuleKey) : unit =
        let path = SymbolKeyOps.moduleFullName m

        if not (surface.ModuleContainers.ContainsKey path) then
            surface.ModuleContainers.[path] <- TypeContainer.InModule m

        match m.Container with
        | ModuleContainer.InModule parent -> addModuleContainer surface parent
        | ModuleContainer.InNamespace _ -> ()

    /// Index a type's identity by the name a consumer writes: its compiled name, and the
    /// enclosing module chain that makes a written `A.M.T` reach it. An `InType`-nested or
    /// namespace-direct type contributes no module container.
    let addTypeName (surface: PublishedSurfaceBuilder) (key: TypeKey) : unit =
        let name = SymbolKeyOps.typeMetaName key

        // First declaration wins on a compiled-name collision.
        if not (surface.TypesByName.ContainsKey name) then
            surface.TypesByName.[name] <- key

        match key.Container with
        | TypeContainer.InModule m -> addModuleContainer surface m
        | TypeContainer.InNamespace _
        | TypeContainer.InType _ -> ()

    let addShape (surface: PublishedSurfaceBuilder) (key: TypeKey) (shape: ExternalTypeShape) : unit =
        surface.ShapesByKey.[key] <- shape

    /// Record that `canon`'s representation is the target's to supply, in the form `repr`
    /// states. `canon` is the intrinsic identity a use site resolves the name to, which is
    /// what an implementation files its `(# … #)` binding under.
    let addDeclaredRepr (surface: PublishedSurfaceBuilder) (canon: TypeKey) (repr: DeclaredRepr) : unit =
        surface.DeclaredReprs.[canon] <- repr

    /// Append to a type's member list, which stays in DECLARATION order. An empty batch
    /// creates no entry: a type with no published member has no member table.
    let addMembers (surface: PublishedSurfaceBuilder) (key: TypeKey) (members: seq<ExternalMember>) : unit =
        let batch = ResizeArray<ExternalMember> members

        if batch.Count > 0 then
            match surface.MembersByKey.TryGetValue key with
            | true, existing -> existing.AddRange batch
            | _ -> surface.MembersByKey.[key] <- batch

    /// One candidate per record, appended to EVERY field's bucket, so a shared field name
    /// keeps both records live.
    let addRecordCandidate (surface: PublishedSurfaceBuilder) (candidate: ExternalRecordCandidate) : unit =
        for f in candidate.FieldNames do
            match surface.RecordFields.TryGetValue f with
            | true, buf -> buf.Add candidate
            | _ ->
                let buf = ResizeArray<ExternalRecordCandidate>()
                buf.Add candidate
                surface.RecordFields.[f] <- buf

    /// Index a union case by its BARE name. First declaration wins on a collision; an RQA
    /// union's cases carry the flag so a consumer's bare `Red` is rejected.
    let addUnionCase (surface: PublishedSurfaceBuilder) (case: ExternalUnionCase) : unit =
        if not (surface.UnionCases.ContainsKey case.Case.Name) then
            surface.UnionCases.[case.Case.Name] <- case

/// One entry of a published table. A key-ordered array of these rather than a dictionary,
/// because the surface is a VALUE: fixing the order is what lets two of them compare, and
/// later hash, by contents.
type SurfaceEntry<'K, 'V> = { Key: 'K; Value: 'V }

/// The same tables once ACCUMULATION IS OVER: an immutable, key-ordered value, equal to
/// another exactly when it publishes the same thing. `Symbols` is the exception, a `ValRepr`
/// holding pool-relative handles that do not compare by contents.
type PublishedSurface =
    {
        ShapesByKey: EqArray<SurfaceEntry<TypeKey, ExternalTypeShape>>
        /// Canonical intrinsic identity -> the representation form declared for it.
        DeclaredReprs: EqArray<SurfaceEntry<TypeKey, DeclaredRepr>>
        /// A type's FULL member list, in DECLARATION order: the overload scan depends on it.
        MembersByKey: EqArray<SurfaceEntry<TypeKey, EqArray<ExternalMember>>>
        /// Canonical compiled name -> the registered identity.
        TypesByName: EqArray<SurfaceEntry<string, TypeKey>>
        /// Dotted source path of a declared module -> the container a type it holds sits in.
        ModuleContainers: EqArray<SurfaceEntry<string, TypeContainer>>
        /// Bare case name -> the union declaring it.
        UnionCases: EqArray<SurfaceEntry<string, ExternalUnionCase>>
        /// Field name -> every record declaring it.
        RecordFields: EqArray<SurfaceEntry<string, EqArray<ExternalRecordCandidate>>>
        /// Values, keyed as a binding key renders: `.`-joined.
        Symbols: EqArray<SurfaceEntry<string, ExternalSymbol>>
        /// Derived from the `Intrinsic` shapes above. The BUILDER has no such field, so a
        /// producer cannot put a CAPABILITY interface here: it carries its platform name on
        /// its own identity and must stay OFF this axis.
        Intrinsics: IntrinsicTypeMap
        /// Prefixes a consumer resolves through with no `open` of its own, in SEARCH order:
        /// the one table that is not key-ordered.
        AmbientOpenPrefixes: EqArray<string>
    }

[<RequireQualifiedAccess>]
module PublishedSurface =

    /// Key-ordered by an ORDINAL rendering of the key, so the order is the same on every
    /// machine and in every process.
    let private ordered (render: 'K -> string) (pairs: seq<'K * 'V>) : EqArray<SurfaceEntry<'K, 'V>> =
        pairs
        |> Seq.map (fun (k, v) -> struct (render k, k, v))
        |> Seq.sortWith (fun struct (a, _, _) struct (b, _, _) -> String.CompareOrdinal(a, b))
        |> Seq.map (fun struct (_, k, v) -> { Key = k; Value = v })
        |> EqArray.ofSeq

    let private byName (d: Dictionary<string, 'V>) : EqArray<SurfaceEntry<string, 'V>> =
        ordered id (seq { for KeyValue(k, v) in d -> k, v })

    let private byTypeKey (d: Dictionary<TypeKey, 'V>) : EqArray<SurfaceEntry<TypeKey, 'V>> =
        ordered SymbolKeyOps.typeMetaName (seq { for KeyValue(k, v) in d -> k, v })

    /// Copy the builder's tables into the value. A producer that keeps writing to the builder
    /// afterwards no longer changes what it published.
    let ofBuilder (b: PublishedSurfaceBuilder) : PublishedSurface =
        let shapes = byTypeKey b.ShapesByKey

        {
            ShapesByKey = shapes
            DeclaredReprs = byTypeKey b.DeclaredReprs
            MembersByKey =
                b.MembersByKey
                |> Seq.map (fun (KeyValue(k, ms)) -> k, EqArray.ofResizeArray ms)
                |> ordered SymbolKeyOps.typeMetaName
            TypesByName = byName b.TypesByName
            ModuleContainers = byName b.ModuleContainers
            UnionCases = byName b.UnionCases
            RecordFields =
                b.RecordFields
                |> Seq.map (fun (KeyValue(k, cs)) -> k, EqArray.ofResizeArray cs)
                |> ordered id
            Symbols = byName b.Symbols
            Intrinsics =
                IntrinsicTypeMap.ofSeq (
                    seq {
                        for entry in shapes do
                            match entry.Value with
                            | ExternalTypeShape.Intrinsic { Id = id } ->
                                {
                                    Canon = id.Canon
                                    Platform = id.Platform
                                }
                            | _ -> ()
                    }
                )
            AmbientOpenPrefixes = EqArray.ofList b.AmbientOpenPrefixes
        }

    /// The lookup index over one published table. Derived on demand, never part of the value,
    /// because a `Dictionary` compares by reference.
    let private index (entries: EqArray<SurfaceEntry<'K, 'V>>) (comparer: IEqualityComparer<'K>) =
        let d = Dictionary<'K, 'V>(entries.Length, comparer)

        for e in entries do
            d.[e.Key] <- e.Value

        d

    let private nameIndex (entries: EqArray<SurfaceEntry<string, 'V>>) =
        index entries (StringComparer.Ordinal :> IEqualityComparer<string>)

    let private keyIndex (entries: EqArray<SurfaceEntry<TypeKey, 'V>>) = index entries HashIdentity.Structural

    let toProvider (surface: PublishedSurface) : IExternalSymbolProvider =
        let typesByName = nameIndex surface.TypesByName
        let moduleContainers = nameIndex surface.ModuleContainers
        let unionCases = nameIndex surface.UnionCases
        let recordFields = nameIndex surface.RecordFields
        let symbols = nameIndex surface.Symbols

        // Two spellings arrive: the canonical metadata name, a direct hit; and the dotted
        // spelling source writes for a module-held type (`M.T`), resolved through the
        // declared modules.
        let tryTypeKey (probe: string) : TypeKey voption =
            let exact (name: string) =
                match typesByName.TryGetValue name with
                | true, key -> ValueSome key
                | _ -> ValueNone

            let moduleContainer (path: string) =
                match moduleContainers.TryGetValue path with
                | true, container -> ValueSome container
                | _ -> ValueNone

            SymbolKeyOps.tryDottedInModule exact moduleContainer probe

        ExternalSymbolProviders.ofKeyedChannels (
            ExternalSymbolProviders.KeyedChannels.ofKeyIndexes
                { ExternalSymbolProviders.KeyIndexedChannels.empty with
                    ShapesByKey = keyIndex surface.ShapesByKey
                    MembersByKey = keyIndex surface.MembersByKey
                    ResolveTypeName = tryTypeKey
                    TryLookup =
                        fun name ->
                            match symbols.TryGetValue name with
                            | true, sym -> ValueSome sym
                            | _ -> ValueNone
                    TryLookupUnionCase =
                        fun caseName ->
                            match unionCases.TryGetValue caseName with
                            | true, hit -> ValueSome hit
                            | _ -> ValueNone
                    TryRecordsWithField =
                        fun fieldName ->
                            match recordFields.TryGetValue fieldName with
                            | true, cs -> cs
                            | _ -> EqArray.empty
                    AmbientOpenPrefixes = List.ofSeq surface.AmbientOpenPrefixes
                    IntrinsicTypeMap = surface.Intrinsics
                }
        )
