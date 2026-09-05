namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic

// `.fsi` ↔ `.fs` conformance over the two ANALYSED halves: the surface a signature publishes
// against the declarations its implementation froze, compared by resolved identity. A
// `ModuleSuffix` module and a shadowed attribute are settled by that resolution before any
// comparison here. A `[<CompiledName>]` is off the identity axis, because fsc pairs the
// halves by the name each writes (FS0193); it is checked as its own agreement between the
// paired declarations.

/// The findings about a signature's VALUES. `Errors` is at error severity, `Divergent` at
/// warning severity.
[<NoComparison>]
type ValueConformance =
    {
        Errors: Conformance.ConformanceError list
        Divergent: Conformance.AttributeDivergence list
    }

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
                    | TTypeKindG.Abbrev _
                    | TTypeKindG.Measure _ -> ValueNone
            | _ -> ()

        for KeyValue(canon, _) in frozen.Residue.IntrinsicBindings do
            if not (declared.ContainsKey canon) then
                declared.[canon] <- ValueNone

        declared

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

            // Only a key BOTH halves commit a family for takes a verdict: an absent
            // declaration is reported above, and an abbreviation is transparent.
            for entry in published.ShapesByKey do
                match entry.Value.DeclaredFamily with
                | ValueNone -> ()
                | ValueSome family ->
                    match declared.TryGetValue entry.Key with
                    | true, ValueSome defined when defined <> family ->
                        yield Conformance.ConformanceError.TypeKindMismatch(named entry.Key, family, defined)
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

    /// Value PRESENCE (every symbol the signature publishes is met by an implementation binding
    /// of the same identity), the EMITTED name both halves settled on, and the attribute
    /// ARGUMENTS both halves wrote (fsc's FS1200). Findings come in key order, attributes in
    /// the signature's order.
    let checkValues (published: PublishedSurface) (frozen: FrozenPools) : ValueConformance =
        let defined = definedValues frozen
        let errors = ResizeArray<Conformance.ConformanceError>()
        let divergent = ResizeArray<Conformance.AttributeDivergence>()

        for entry in published.Symbols do
            let declaration () =
                SymbolKeyOps.qualifiedBindingName entry.Key

            match defined.TryGetValue entry.Key with
            | false, _ -> errors.Add(Conformance.ConformanceError.ValueMissingInImpl(declaration ()))
            | true, impl ->
                // A reference resolves through the SIGNATURE's surface while the
                // implementation emits under its own declaration, so the two `[<CompiledName>]`
                // readings are a pair that has to agree.
                let declaredEmission = entry.Value.EmittedName

                if impl.EmittedName <> declaredEmission then
                    errors.Add(
                        Conformance.ConformanceError.CompiledNameDiffers(
                            declaration (),
                            declaredEmission,
                            impl.EmittedName
                        )
                    )

                for key in divergentAttributes entry.Value.Attributes impl.Attributes do
                    divergent.Add
                        {
                            Declaration = declaration ()
                            Attribute = SymbolKeyOps.typeMetaName key
                        }

        {
            Errors = List.ofSeq errors
            Divergent = List.ofSeq divergent
        }
