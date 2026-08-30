namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic

// `.fsi` ↔ `.fs` conformance over the two ANALYSED halves: the surface a signature publishes
// against the declarations its implementation froze, compared by resolved identity. A
// `[<CompiledName>]`, a `ModuleSuffix` module and a shadowed attribute are therefore all
// already settled before a comparison here.

module ConformanceSurface =

    /// The type identities an implementation DECLARES: its frozen type declarations, plus the
    /// canonical identities its `(# … #)` bindings claim. The value is the nominal family a
    /// concrete declaration defines; an abbreviation and an intrinsic claim define none.
    let private declaredTypes (frozen: FrozenPools) : Dictionary<TypeKey, Conformance.TypeKindFamily voption> =
        let declared =
            Dictionary<TypeKey, Conformance.TypeKindFamily voption>(HashIdentity.Structural)

        let pool = TastPoolBuilder.openOver frozen

        for decl in TastAccessor.roots pool do
            match TastAccessor.declKind decl with
            | DeclShape.Type ->
                let td = TastAccessor.declType decl

                declared.[td.TypeKey] <-
                    match td.Kind with
                    | TTypeKindG.Record _ -> ValueSome Conformance.TypeKindFamily.Record
                    | TTypeKindG.Union _ -> ValueSome Conformance.TypeKindFamily.Union
                    | TTypeKindG.Enum _ -> ValueSome Conformance.TypeKindFamily.Enum
                    | TTypeKindG.Interface _ -> ValueSome Conformance.TypeKindFamily.Interface
                    | TTypeKindG.Class _ -> ValueSome Conformance.TypeKindFamily.Class
                    | TTypeKindG.Abbrev _ -> ValueNone
            | _ -> ()

        for KeyValue(canon, _) in frozen.Residue.IntrinsicBindings do
            if not (declared.ContainsKey canon) then
                declared.[canon] <- ValueNone

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
    let private demandsDeclaration
        (declaredExterns: HashSet<TypeKey>)
        (entry: SurfaceEntry<TypeKey, ExternalTypeShape>)
        =
        match entry.Value with
        // The `extern` family is checked by the repr pairing below instead. A capability on a
        // target binding no repr publishes as a plain `Class`, so the key is what identifies it.
        | ExternalTypeShape.Intrinsic _
        | ExternalTypeShape.IntrinsicInterface _ -> false
        // A GAP the signature published in place of a type (a delegate, a type extension). It
        // claims no identity to match, so the verdict comes from elsewhere: a delegate is
        // refused at its declaration (`NotYetSupported`), and either form reports at first USE.
        | ExternalTypeShape.Unmodelled _ -> false
        | ExternalTypeShape.Abbrev _
        | ExternalTypeShape.Record _
        | ExternalTypeShape.Union _
        | ExternalTypeShape.Enum _
        | ExternalTypeShape.Class _ -> not (declaredExterns.Contains entry.Key)

    /// Type PRESENCE, the nominal-family agreement, and the `extern` ↔ `(# … #)` pairing with
    /// its heritability. Findings come in that order, each group in key order.
    let checkTypes (published: PublishedSurface) (frozen: FrozenPools) : Conformance.ConformanceError list =
        let declared = declaredTypes frozen
        let implBindings = frozen.Residue.IntrinsicBindings

        let declaredExternKeys =
            HashSet<TypeKey>(seq { for e in published.ExternForms -> e.Key }, HashIdentity.Structural)

        let named (key: TypeKey) = SymbolKeyOps.typeMetaName key

        [
            for entry in published.ShapesByKey do
                if
                    demandsDeclaration declaredExternKeys entry
                    && not (declared.ContainsKey entry.Key)
                then
                    yield Conformance.ConformanceError.MissingInImpl(named entry.Key)

            // Only a key the implementation defines a family for takes a verdict: an absent
            // one is reported above, and an abbreviation is transparent.
            for entry in published.DeclaredKinds do
                match declared.TryGetValue entry.Key with
                | true, ValueSome defined when defined <> entry.Value ->
                    yield Conformance.ConformanceError.TypeKindMismatch(named entry.Key, entry.Value, defined)
                | _ -> ()

            for entry in published.ExternForms do
                match EqDict.tryFind entry.Key implBindings with
                | ValueNone -> yield Conformance.ConformanceError.ExternWithoutIntrinsic(named entry.Key)
                | ValueSome binding ->
                    if binding.Heritable <> entry.Value.IsHeritable then
                        yield Conformance.ConformanceError.HeritabilityMismatch(named entry.Key)

            // A binding the contract never declares. A plain implementation type absent from the
            // signature is hidden by F#, so only the intrinsic-binding case is reported.
            for KeyValue(canon, _) in implBindings do
                if not (declaredExternKeys.Contains canon) then
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
