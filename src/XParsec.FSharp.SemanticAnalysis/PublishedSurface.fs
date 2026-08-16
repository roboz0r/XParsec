namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic

// The tables a compilation unit's published surface is ACCUMULATED in, addressed by identity
// rather than by a rendering of one. Both halves of a unit fill one — a `.fs` projected from
// its frozen pools, a `.fsi` from its resolved signatures — so what a later file resolves
// through does not depend on which half published it.

type PublishedSurfaceBuilder =
    {
        ShapesByKey: Dictionary<TypeKey, ExternalTypeShape>
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
        mutable Intrinsics: IntrinsicTypeMap
        /// Prefixes a consumer resolves through with no `open` of its own.
        mutable AmbientOpenPrefixes: string list
    }

[<RequireQualifiedAccess>]
module PublishedSurfaceBuilder =

    let create () : PublishedSurfaceBuilder =
        {
            ShapesByKey = Dictionary()
            MembersByKey = Dictionary()
            TypesByName = Dictionary(System.StringComparer.Ordinal)
            ModuleContainers = Dictionary(System.StringComparer.Ordinal)
            UnionCases = Dictionary(System.StringComparer.Ordinal)
            RecordFields = Dictionary(System.StringComparer.Ordinal)
            Symbols = Dictionary(System.StringComparer.Ordinal)
            Intrinsics = IntrinsicTypeMap.empty
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

    /// The identity a written type name denotes here. Two spellings arrive: the canonical
    /// metadata name, a direct hit; and the dotted spelling source writes for a module-held
    /// type (`M.T`), resolved through the declared modules.
    let tryTypeKey (surface: PublishedSurfaceBuilder) (probe: string) : TypeKey voption =
        let exact (name: string) =
            match surface.TypesByName.TryGetValue name with
            | true, key -> ValueSome key
            | _ -> ValueNone

        let moduleContainer (path: string) =
            match surface.ModuleContainers.TryGetValue path with
            | true, container -> ValueSome container
            | _ -> ValueNone

        SymbolKeyOps.tryDottedInModule exact moduleContainer probe

    let toProvider (surface: PublishedSurfaceBuilder) : IExternalSymbolProvider =
        ExternalSymbolProviders.ofKeyedChannels (
            ExternalSymbolProviders.KeyedChannels.ofKeyIndexes
                { ExternalSymbolProviders.KeyIndexedChannels.empty with
                    ShapesByKey = surface.ShapesByKey
                    MembersByKey = surface.MembersByKey
                    ResolveTypeName = tryTypeKey surface
                    TryLookup =
                        fun name ->
                            match surface.Symbols.TryGetValue name with
                            | true, sym -> ValueSome sym
                            | _ -> ValueNone
                    TryLookupUnionCase =
                        fun caseName ->
                            match surface.UnionCases.TryGetValue caseName with
                            | true, hit -> ValueSome hit
                            | _ -> ValueNone
                    TryRecordsWithField =
                        fun fieldName ->
                            match surface.RecordFields.TryGetValue fieldName with
                            | true, buf -> EqArray.ofResizeArray buf
                            | _ -> EqArray.empty
                    AmbientOpenPrefixes = surface.AmbientOpenPrefixes
                    IntrinsicTypeMap = surface.Intrinsics
                }
        )
