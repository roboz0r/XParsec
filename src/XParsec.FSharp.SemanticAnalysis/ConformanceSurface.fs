namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic

// `.fsi` ↔ `.fs` conformance over the two ANALYSED halves: the surface a signature publishes
// against the declarations its implementation froze, compared by resolved identity. A
// `[<CompiledName>]`, a `ModuleSuffix` module and a shadowed attribute are therefore all
// already settled before a comparison here.

module ConformanceSurface =

    /// The type identities an implementation DECLARES: its frozen type declarations, plus the
    /// canonical identities claimed by its `(# … #)` bindings, which are stored on the residue.
    let private declaredTypes (frozen: FrozenPools) : HashSet<TypeKey> =
        let declared = HashSet<TypeKey>(HashIdentity.Structural)
        let pool = TastPoolBuilder.openOver frozen

        for decl in TastAccessor.roots pool do
            match TastAccessor.declKind decl with
            | DeclShape.Type -> declared.Add (TastAccessor.declType decl).TypeKey |> ignore
            | _ -> ()

        for KeyValue(canon, _) in frozen.Residue.IntrinsicReprKeys do
            declared.Add canon |> ignore

        declared

    /// The binding identities an implementation DEFINES, under the compiled name each
    /// publishes. A pattern binding no single variable defines no identity.
    let private definedValues (frozen: FrozenPools) : HashSet<BindingKey> =
        let defined = HashSet<BindingKey>(HashIdentity.Structural)
        let pool = TastPoolBuilder.openOver frozen
        let moduleMembers = DenseTable.index frozen.ModuleMembers

        for decl in TastAccessor.roots pool do
            match decl with
            | TastAccessor.DLet {
                                    Pattern = TastAccessor.PNamed boundVar
                                } ->
                match moduleMembers.TryGetValue boundVar with
                | true, info ->
                    match info.Key with
                    | SymbolKey.Binding bindingKey -> defined.Add bindingKey |> ignore
                    | _ -> ()
                | _ -> ()
            | _ -> ()

        defined

    /// Does the signature's shape for this key oblige the implementation to declare a type of
    /// the same identity?
    let private demandsDeclaration (declaredReprs: HashSet<TypeKey>) (entry: SurfaceEntry<TypeKey, ExternalTypeShape>) =
        match entry.Value with
        // The `extern` family is checked by the repr pairing below instead. A capability on a
        // target binding no repr publishes as a plain `Class`, so the key is what identifies it.
        | ExternalTypeShape.Intrinsic _
        | ExternalTypeShape.IntrinsicInterface _ -> false
        // A GAP the signature published in place of a type (a delegate, a type extension): it
        // claims no identity for an implementation to match. The first USE of one reports.
        | ExternalTypeShape.Unmodelled _ -> false
        | ExternalTypeShape.Abbrev _
        | ExternalTypeShape.Record _
        | ExternalTypeShape.Union _
        | ExternalTypeShape.Enum _
        | ExternalTypeShape.Class _ -> not (declaredReprs.Contains entry.Key)

    /// Type PRESENCE, and the `extern` ↔ `(# … #)` pairing with its heritability. Findings come
    /// in key order: presence first, then the reprs the signature declares, then the ones only
    /// the implementation binds.
    let checkTypes (published: PublishedSurface) (frozen: FrozenPools) : Conformance.ConformanceError list =
        let declared = declaredTypes frozen
        let implReprs = frozen.Residue.IntrinsicReprKeys

        let declaredReprKeys =
            HashSet<TypeKey>(seq { for e in published.DeclaredReprs -> e.Key }, HashIdentity.Structural)

        let named (key: TypeKey) = SymbolKeyOps.typeMetaName key

        [
            for entry in published.ShapesByKey do
                if demandsDeclaration declaredReprKeys entry && not (declared.Contains entry.Key) then
                    yield Conformance.ConformanceError.MissingInImpl(named entry.Key)

            for entry in published.DeclaredReprs do
                match EqDict.tryFind entry.Key implReprs with
                | ValueNone -> yield Conformance.ConformanceError.ExternWithoutIntrinsic(named entry.Key)
                | ValueSome repr ->
                    if repr.Heritable <> entry.Value.IsHeritable then
                        yield Conformance.ConformanceError.HeritabilityMismatch(named entry.Key)

            // A repr the contract never declares. A plain implementation type absent from the
            // signature is hidden by F#, so only the repr case is reported.
            for KeyValue(canon, _) in implReprs do
                if not (declaredReprKeys.Contains canon) then
                    yield Conformance.ConformanceError.IntrinsicWithoutExtern(named canon)
        ]

    /// Value PRESENCE: every symbol the signature publishes is met by an implementation
    /// binding of the same identity. The converse is not reported, because F# hides an
    /// implementation value the signature omits.
    let checkValues (published: PublishedSurface) (frozen: FrozenPools) : Conformance.ConformanceError list =
        let defined = definedValues frozen

        [
            for entry in published.Symbols do
                if not (defined.Contains entry.Key) then
                    yield
                        Conformance.ConformanceError.ValueMissingInImpl(
                            SymbolKeyOps.qualifiedName (SymbolKey.Binding entry.Key)
                        )
        ]
