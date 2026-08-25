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

/// How the source spells a published value: the declaring scope's dotted path and the
/// binding's short name, each as WRITTEN (`Vesper.List` + `fold` for the value compiled as
/// `Vesper.ListModule.fold`).
type SourceSpelling = { Path: string; Name: string }

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
        /// Declaring union's compiled name + `.` + case name -> the case; every case published.
        UnionCases: Dictionary<string, ExternalUnionCase>
        /// Field name -> every record declaring it, a MULTIMAP rather than first-wins: a field
        /// name is deliberately shared across records, so each one ADDS a candidate.
        RecordFields: Dictionary<string, ResizeArray<ExternalRecordCandidate>>
        /// Every published value, one entry per identity.
        Symbols: Dictionary<BindingKey, ExternalSymbol>
        /// The source spelling of a value whose compiled name differs, and the binding it
        /// names. First spelling wins.
        SourceSpellings: Dictionary<SourceSpelling, BindingKey>
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
            Symbols = Dictionary(HashIdentity.Structural)
            SourceSpellings = Dictionary(HashIdentity.Structural)
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

    /// Publishes the value under its own identity, and records `source` as a second spelling
    /// reaching it where the two differ: `Vesper.List.fold` beside `Vesper.ListModule.fold`.
    /// Only a module `source.Path` is added to `ModuleContainers`.
    let addValue (surface: PublishedSurfaceBuilder) (source: SourceSpelling voption) (sym: ExternalSymbol) : unit =
        surface.Symbols.[sym.Key] <- sym

        match source with
        | ValueSome s ->
            if s.Path <> SymbolKeyOps.containerFullName sym.Key.Decl || s.Name <> sym.Key.Name then
                surface.SourceSpellings.TryAdd(s, sym.Key) |> ignore

                match sym.Key.Decl with
                | ModuleContainer.InModule m ->
                    surface.ModuleContainers.TryAdd(s.Path, TypeContainer.InModule m) |> ignore
                | ModuleContainer.InNamespace _ -> ()
        | ValueNone -> ()

    /// Index a union case under its declaring union's compiled name plus its own
    /// (`` Test.A.M+Color.Red ``), so every published case is retained. An RQA union's cases
    /// carry the flag so a consumer's bare `Red` is rejected.
    let addUnionCase (surface: PublishedSurfaceBuilder) (case: ExternalUnionCase) : unit =
        surface.UnionCases.[SymbolKeyOps.typeMetaName case.UnionKey + "." + case.Case.Name] <- case

    /// The one entry point for publishing a type declaration: compiled name, module chain,
    /// shape, member table, and the case or field index the shape implies. `members` is the
    /// type's FULL member list in declaration order, duplicating a shape's own. The cons-list
    /// is the one shape whose cases are left out of the case index.
    let addTypeWith
        (surface: PublishedSurfaceBuilder)
        (key: TypeKey)
        (shape: ExternalTypeShape)
        (members: seq<ExternalMember>)
        : unit =
        addTypeName surface key
        addShape surface key shape
        addMembers surface key members

        match shape with
        | ExternalTypeShape.Union _ when RuntimeNames.isVesperListName (SymbolKeyOps.typeMetaName key) -> ()
        | ExternalTypeShape.Union(cases = cases; requiresQualifiedAccess = rqa) ->
            for case in cases do
                addUnionCase
                    surface
                    {
                        UnionKey = key
                        Case = case
                        IsRequireQualifiedAccess = rqa
                    }
        | ExternalTypeShape.Record(arity = arity; fields = fields; requiresQualifiedAccess = rqa) ->
            addRecordCandidate
                surface
                {
                    TypeKey = key
                    TyparArity = arity
                    FieldNames = fields |> EqArray.map (fun f -> f.Name)
                    IsRequireQualifiedAccess = rqa
                }
        | ExternalTypeShape.Class _
        | ExternalTypeShape.IntrinsicInterface _
        | ExternalTypeShape.Enum _
        | ExternalTypeShape.Intrinsic _
        | ExternalTypeShape.Abbrev _
        | ExternalTypeShape.Unmodelled _ -> ()

    /// `addTypeWith` for a type publishing no member.
    let addType (surface: PublishedSurfaceBuilder) (key: TypeKey) (shape: ExternalTypeShape) : unit =
        addTypeWith surface key shape EqArray.empty

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
        /// Declaring union's compiled name + `.` + case name -> the case.
        UnionCases: EqArray<SurfaceEntry<string, ExternalUnionCase>>
        /// Field name -> every record declaring it.
        RecordFields: EqArray<SurfaceEntry<string, EqArray<ExternalRecordCandidate>>>
        /// Every published value, one entry per identity.
        Symbols: EqArray<SurfaceEntry<BindingKey, ExternalSymbol>>
        /// The source spelling of a value whose compiled name differs, and the binding it names.
        SourceSpellings: EqArray<SurfaceEntry<SourceSpelling, BindingKey>>
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

    /// The whole name a source spelling writes: `Vesper.List.fold`.
    let private writtenName (s: SourceSpelling) : string = SymbolKeyOps.qualify s.Path s.Name

    let private bindingName (k: BindingKey) : string =
        SymbolKeyOps.qualifiedName (SymbolKey.Binding k)

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
            Symbols = ordered bindingName (seq { for KeyValue(k, v) in b.Symbols -> k, v })
            SourceSpellings = ordered writtenName (seq { for KeyValue(k, v) in b.SourceSpellings -> k, v })
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

    /// The surface `fill` accumulates. `ofBuilder` is for a producer threading one builder
    /// through a pass; this is for a surface assembled in one place.
    let build (fill: PublishedSurfaceBuilder -> unit) : PublishedSurface =
        let b = PublishedSurfaceBuilder.create ()
        fill b
        ofBuilder b

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

    /// A scope's contents derived from the published tables: every symbol, case and type is
    /// filed under the container its key declares, so a segment-by-segment read of `A.M.x`
    /// asks the module `A.M` for `x` rather than a name index for `A.M.x`.
    let private scopeOf (surface: PublishedSurface) : IScopeContents =
        let symbols = index surface.Symbols HashIdentity.Structural

        let valuesIn =
            Dictionary<struct (ModuleContainer * string), ExternalSymbol>(HashIdentity.Structural)

        for e in surface.Symbols do
            valuesIn.[struct (e.Key.Decl, e.Key.Name)] <- e.Value

        // A source spelling answers in the container of the binding it names: `Vesper.Set.empty`
        // reaches the binding compiled as `SetModule.Empty`. A compiled short name already
        // filed wins.
        for e in surface.SourceSpellings do
            match symbols.TryGetValue e.Value with
            | true, sym -> valuesIn.TryAdd(struct (e.Value.Decl, e.Key.Name), sym) |> ignore
            | _ -> ()

        let casesIn =
            Dictionary<struct (ModuleContainer * string), ResizeArray<ExternalUnionCase>>(HashIdentity.Structural)

        for e in surface.UnionCases do
            match SymbolKeyOps.tryModuleContainerOf e.Value.UnionKey.Container with
            | ValueSome c ->
                let slot = struct (c, e.Value.Case.Name)

                match casesIn.TryGetValue slot with
                | true, claims -> claims.Add e.Value
                | _ ->
                    let claims = ResizeArray 1
                    claims.Add e.Value
                    casesIn.[slot] <- claims
            | ValueNone -> ()

        let typesIn =
            Dictionary<struct (ModuleContainer * string), ResizeArray<struct (TypeKey * ExternalTypeShape)>>(
                HashIdentity.Structural
            )

        for e in surface.ShapesByKey do
            match SymbolKeyOps.tryModuleContainerOf e.Key.Container with
            | ValueSome c ->
                let slot = struct (c, e.Key.Name)

                match typesIn.TryGetValue slot with
                | true, arities -> arities.Add(struct (e.Key, e.Value))
                | _ ->
                    let arities = ResizeArray 1
                    arities.Add(struct (e.Key, e.Value))
                    typesIn.[slot] <- arities
            | ValueNone -> ()

        // Every module and namespace a published key sits in, each module's enclosing chain
        // and every prefix of each namespace: `System` is a namespace wherever
        // `System.Collections` is, and a module holding only values is still a module.
        let containers = Dictionary<string, ModuleContainer>(StringComparer.Ordinal)

        let rec noteNamespace (dotted: string) =
            if
                dotted.Length > 0
                && containers.TryAdd(dotted, ModuleContainer.InNamespace(SymbolKeyOps.namespaceKey dotted))
            then
                match dotted.LastIndexOf '.' with
                | i when i > 0 -> noteNamespace (dotted.Substring(0, i))
                | _ -> ()

        let rec noteContainer (c: ModuleContainer) =
            match c with
            | ModuleContainer.InNamespace ns -> noteNamespace ns.Dotted
            | ModuleContainer.InModule m ->
                if containers.TryAdd(SymbolKeyOps.moduleFullName m, c) then
                    noteContainer m.Container

        for e in surface.ShapesByKey do
            match SymbolKeyOps.tryModuleContainerOf e.Key.Container with
            | ValueSome c -> noteContainer c
            | ValueNone -> noteNamespace e.Key.Namespace.Dotted

        for e in surface.Symbols do
            noteContainer e.Key.Decl

        // Last, so a compiled path already registered wins. `e.Key` is the source path:
        // `Vesper.List` beside the compiled `Vesper.ListModule`.
        for e in surface.ModuleContainers do
            match e.Value with
            | TypeContainer.InModule m ->
                let c = ModuleContainer.InModule m
                noteContainer c
                containers.TryAdd(e.Key, c) |> ignore
            | TypeContainer.InNamespace ns -> noteNamespace ns.Dotted
            | TypeContainer.InType _ -> ()

        { new IScopeContents with
            member _.TryContainer path =
                match containers.TryGetValue path with
                | true, c -> ValueSome c
                | _ -> ValueNone

            member _.TryValue(container, name) =
                match valuesIn.TryGetValue(struct (container, name)) with
                | true, sym -> ValueSome sym
                | _ -> ValueNone

            member _.UnionCasesNamed(container, name) =
                match casesIn.TryGetValue(struct (container, name)) with
                | true, claims -> EqArray.ofResizeArray claims
                | _ -> EqArray.empty

            member _.TypesNamed(container, name) =
                match typesIn.TryGetValue(struct (container, name)) with
                | true, arities -> EqArray.ofResizeArray arities
                | _ -> EqArray.empty
        }

    let toProvider (surface: PublishedSurface) : IExternalSymbolProvider =
        let typesByName = nameIndex surface.TypesByName
        let recordFields = nameIndex surface.RecordFields

        // The canonical metadata name only. The dotted spelling source writes for a
        // module-held type (`M.T`) is reached through the module, on the scope.
        let tryTypeKey (probe: string) : TypeKey voption =
            match typesByName.TryGetValue probe with
            | true, key -> ValueSome key
            | _ -> ValueNone

        ExternalSymbolProviders.ofKeyedChannels (
            ExternalSymbolProviders.KeyedChannels.ofKeyIndexes
                { ExternalSymbolProviders.KeyIndexedChannels.empty with
                    Scope = scopeOf surface
                    ShapesByKey = keyIndex surface.ShapesByKey
                    MembersByKey = keyIndex surface.MembersByKey
                    ResolveTypeName = tryTypeKey
                    SymbolsByKey = index surface.Symbols HashIdentity.Structural
                    TryRecordsWithField =
                        fun fieldName ->
                            match recordFields.TryGetValue fieldName with
                            | true, cs -> cs
                            | _ -> EqArray.empty
                    AmbientOpenPrefixes = List.ofSeq surface.AmbientOpenPrefixes
                    IntrinsicTypeMap = surface.Intrinsics
                }
        )
