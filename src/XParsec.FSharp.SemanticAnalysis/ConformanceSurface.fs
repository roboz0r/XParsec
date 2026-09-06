namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic

// `.fsi` ↔ `.fs` conformance over the two ANALYSED halves: the surface a signature publishes
// against the declarations its implementation froze, compared by resolved identity. A
// `ModuleSuffix` module and a shadowed attribute are settled by that resolution before any
// comparison here. A `[<CompiledName>]` is off the identity axis, because fsc pairs the
// halves by the name each writes (FS0193); it is checked as its own agreement between the
// paired declarations.

/// The findings about one group of a signature's declarations. `Errors` is at error
/// severity, `Divergent` at warning severity.
[<NoComparison>]
type ConformanceFindings =
    {
        Errors: Conformance.ConformanceError list
        Divergent: Conformance.AttributeDivergence list
    }

module ConformanceSurface =

    let private declaredTypes (frozen: FrozenPools) : Dictionary<TypeKey, TastAccessor.TypeDecl> =
        let declared = Dictionary<TypeKey, TastAccessor.TypeDecl>(HashIdentity.Structural)

        let pool = TastPoolBuilder.openOver frozen

        for decl in TastAccessor.roots pool do
            match TastAccessor.declKind decl with
            | DeclShape.Type ->
                let td = TastAccessor.declType decl
                declared.[td.TypeKey] <- td
            | _ -> ()

        declared

    let private definedFamily (td: TastAccessor.TypeDecl) : Conformance.TypeKindFamily voption =
        match td.Kind with
        | TTypeKindG.Record _ -> ValueSome Conformance.TypeKindFamily.Record
        | TTypeKindG.Union _ -> ValueSome Conformance.TypeKindFamily.Union
        | TTypeKindG.Enum _ -> ValueSome Conformance.TypeKindFamily.Enum
        | TTypeKindG.Interface _ -> ValueSome Conformance.TypeKindFamily.Interface
        | TTypeKindG.Class _ -> ValueSome Conformance.TypeKindFamily.Class
        | TTypeKindG.Abbrev _
        | TTypeKindG.Measure _ -> ValueNone

    /// The binding identities an implementation DEFINES, each with the declaration it was
    /// filed from. Only a pattern binding exactly one variable carries an identity.
    let private definedValues (frozen: FrozenPools) : Dictionary<BindingKey, ModuleBindingInfo> =
        let defined = Dictionary<BindingKey, ModuleBindingInfo>(HashIdentity.Structural)
        let pool = TastPoolBuilder.openOver frozen
        let moduleMembers = DenseTable.index frozen.ModuleMembers

        for decl in TastAccessor.roots pool do
            match decl with
            | TastAccessor.DLet {
                                    Pattern = TastAccessor.PNamed boundVar
                                } ->
                match moduleMembers.TryGetValue boundVar with
                | true, info -> defined.[info.BindingKey] <- info
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
        | ExternalTypeShape.Measure _
        | ExternalTypeShape.Record _
        | ExternalTypeShape.Union _
        | ExternalTypeShape.Enum _
        | ExternalTypeShape.Class _ -> not (declaredExterns.Contains entry.Key)

    /// One occurrence's arguments in comparison form: the positional arguments in written
    /// order, then the named arguments by name. `[<Foo(1, Y = 2, X = 3)>]` and
    /// `[<Foo(1, X = 3, Y = 2)>]` carry one attribute value, so they compare equal.
    let private comparableArgs (args: EqArray<TAttributeArg>) : TAttributeArg list * TAttributeArg list =
        let positional =
            [
                for a in args do
                    if a.Name.IsNone then
                        yield a
            ]

        let named =
            [
                for a in args do
                    if a.Name.IsSome then
                        yield a
            ]
            |> List.sortBy (fun a -> a.Name)

        positional, named

    /// Each occurrence of attribute `key`, argument-comparable, in written order. An
    /// `AllowMultiple` attribute contributes one entry per occurrence.
    let private occurrencesOf (key: TypeKey) (attrs: TAttributes) =
        [
            for a in attrs do
                if a.Key = key then
                    yield comparableArgs a.Args
        ]

    /// The attribute types BOTH halves write whose arguments differ, in the order `declared`
    /// writes them. Matched by resolved attribute identity and compared as folded values, so
    /// `0x1` and `1` are one argument. An attribute written on one half alone is absent.
    /// `[<CompiledName>]` is excluded: `CompiledNameDiffers` judges the emitted name across
    /// the pair, including the one-sided case this cannot see.
    let private divergentAttributes (declared: TAttributes) (defined: TAttributes) : TypeKey list =
        let judged = HashSet<TypeKey>(HashIdentity.Structural)

        [
            for a in declared do
                if a.Key <> RuntimeNames.compiledNameAttributeKey && judged.Add a.Key then
                    let onImpl = occurrencesOf a.Key defined

                    if not (List.isEmpty onImpl) && onImpl <> occurrencesOf a.Key declared then
                        yield a.Key
        ]

    /// Each error group in key order; divergences in key order, attributes in the signature's
    /// order. An attribute-argument divergence is fsc's FS1200.
    let private checkTypes (published: PublishedSurface) (frozen: FrozenPools) : ConformanceFindings =
        let declared = declaredTypes frozen
        let implBindings = frozen.Residue.IntrinsicBindings

        let declaredExternKeys =
            HashSet<TypeKey>(seq { for e in published.ExternForms -> e.Key }, HashIdentity.Structural)

        let named (key: TypeKey) = SymbolKeyOps.typeMetaName key

        /// A type declaration or a `(# … #)` binding of the same canonical identity.
        let implemented (key: TypeKey) =
            declared.ContainsKey key || implBindings.ContainsKey key

        let errors =
            [
                for entry in published.ShapesByKey do
                    if demandsDeclaration declaredExternKeys entry && not (implemented entry.Key) then
                        yield Conformance.ConformanceError.MissingInImpl(named entry.Key)

                // Only a key BOTH halves commit a family for takes a verdict.
                for entry in published.ShapesByKey do
                    match entry.Value.DeclaredFamily with
                    | ValueNone -> ()
                    | ValueSome family ->
                        match declared.TryGetValue entry.Key with
                        | true, td ->
                            match definedFamily td with
                            | ValueSome defined when defined <> family ->
                                yield Conformance.ConformanceError.TypeKindMismatch(named entry.Key, family, defined)
                            | _ -> ()
                        | _ -> ()

                for entry in published.ExternForms do
                    match EqDict.tryFind entry.Key implBindings with
                    | ValueNone -> yield Conformance.ConformanceError.ExternWithoutIntrinsic(named entry.Key)
                    | ValueSome binding ->
                        if binding.Heritable <> entry.Value.IsHeritable then
                            yield Conformance.ConformanceError.HeritabilityMismatch(named entry.Key)

                // A plain implementation type absent from the signature is hidden by F#, so only
                // an intrinsic binding is reported.
                for KeyValue(canon, _) in implBindings do
                    if not (declaredExternKeys.Contains canon) then
                        yield Conformance.ConformanceError.IntrinsicWithoutExtern(named canon)
            ]

        let divergent: Conformance.AttributeDivergence list =
            [
                for entry in published.AttributesByKey do
                    match declared.TryGetValue entry.Key with
                    | true, td ->
                        for key in divergentAttributes entry.Value td.Attributes do
                            yield
                                {
                                    Declaration = named entry.Key
                                    Attribute = named key
                                }
                    | _ -> ()
            ]

        {
            Errors = errors
            Divergent = divergent
        }

    /// Findings in key order, attributes in the signature's order. An attribute-argument
    /// divergence is fsc's FS1200.
    let private checkValues (published: PublishedSurface) (frozen: FrozenPools) : ConformanceFindings =
        let defined = definedValues frozen

        let named (key: BindingKey) = SymbolKeyOps.qualifiedBindingName key

        let errors =
            [
                for entry in published.Symbols do
                    match defined.TryGetValue entry.Key with
                    | false, _ -> yield Conformance.ConformanceError.ValueMissingInImpl(named entry.Key)
                    | true, impl ->
                        // The signature's `[<CompiledName>]` is what a reference resolves through;
                        // the implementation's is what emits.
                        let declaredEmission = entry.Value.EmittedName

                        if impl.EmittedName <> declaredEmission then
                            yield
                                Conformance.ConformanceError.CompiledNameDiffers(
                                    named entry.Key,
                                    declaredEmission,
                                    impl.EmittedName
                                )
            ]

        let divergent: Conformance.AttributeDivergence list =
            [
                for entry in published.Symbols do
                    match defined.TryGetValue entry.Key with
                    | true, impl ->
                        for key in divergentAttributes entry.Value.Attributes impl.Attributes do
                            yield
                                {
                                    Declaration = named entry.Key
                                    Attribute = SymbolKeyOps.typeMetaName key
                                }
                    | _ -> ()
            ]

        {
            Errors = errors
            Divergent = divergent
        }

    /// Every finding over a signature's declarations, the type findings before the value
    /// findings.
    let check (published: PublishedSurface) (frozen: FrozenPools) : ConformanceFindings =
        let types = checkTypes published frozen
        let values = checkValues published frozen

        {
            Errors = [ yield! types.Errors; yield! values.Errors ]
            Divergent = [ yield! types.Divergent; yield! values.Divergent ]
        }
