namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open Vesper

// `.fsi` ↔ `.fs` conformance over the two ANALYSED halves, compared by resolved identity: the
// signature's surface against the one the implementation would publish signatureless. A
// `[<CompiledName>]` is checked as its own agreement between the paired declarations (FS0193).

/// Every finding from one signature/implementation comparison. `Errors` is at error
/// severity, `Divergent` at warning severity.
[<NoComparison>]
type ConformanceFindings =
    {
        Errors: Conformance.ConformanceError list
        Divergent: Conformance.AttributeDivergence list
    }

module ConformanceSurface =

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

    let private comparable (a: TAttributeArg) : string voption * TConstDenotation = a.Name, TConstExpr.denotation a.Expr

    /// One occurrence's arguments in comparison form: the positional arguments in written
    /// order, then the named arguments by name. `[<Foo(1, Y = 2, X = 3)>]` and
    /// `[<Foo(1, X = 3, Y = 2)>]` carry one attribute value, so they compare equal.
    let private comparableArgs
        (args: Block<TAttributeArg>)
        : (string voption * TConstDenotation) list * (string voption * TConstDenotation) list =
        let positional =
            [
                for a in args do
                    if a.Name.IsNone then
                        yield comparable a
            ]

        let named =
            [
                for a in args do
                    if a.Name.IsSome then
                        yield comparable a
            ]
            |> List.sortBy fst

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

    /// Each error group in key order. `implBindings` is the implementation's `(# … #)`
    /// bindings, which publish under the `extern` family alone.
    let private checkTypes
        (published: PublishedSurface)
        (implemented: PublishedSurface)
        (implBindings: EqDict<TypeKey, IntrinsicBindingInfo>)
        : Conformance.ConformanceError list =
        let implShapes = PublishedSurface.keyIndex implemented.ShapesByKey

        let declaredExternKeys =
            HashSet<TypeKey>(seq { for e in published.ExternForms -> e.Key }, HashIdentity.Structural)

        let named (key: TypeKey) = SymbolKeyOps.typeMetaName key

        /// A type declaration or a `(# … #)` binding of the same canonical identity.
        let isImplemented (key: TypeKey) =
            implShapes.ContainsKey key || implBindings.ContainsKey key

        [
            for entry in published.ShapesByKey do
                if demandsDeclaration declaredExternKeys entry && not (isImplemented entry.Key) then
                    yield Conformance.ConformanceError.MissingInImpl(named entry.Key)

            // A mismatch is reported only where BOTH halves declare a family.
            for entry in published.ShapesByKey do
                match entry.Value.DeclaredFamily with
                | ValueNone -> ()
                | ValueSome family ->
                    match implShapes.TryGetValue entry.Key with
                    | true, shape ->
                        match shape.DeclaredFamily with
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

    /// Errors in key order.
    let private checkValues
        (published: PublishedSurface)
        (implemented: PublishedSurface)
        : Conformance.ConformanceError list =
        let defined = PublishedSurface.index implemented.Symbols HashIdentity.Structural

        let named (key: BindingKey) = SymbolKeyOps.qualifiedBindingName key

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

    /// Divergences over every type and value declaration, in key order, attributes in the
    /// signature's order. An attribute-argument divergence is fsc's FS1200.
    let private checkAttributes
        (published: PublishedSurface)
        (implemented: PublishedSurface)
        : Conformance.AttributeDivergence list =
        let implAttributes =
            PublishedSurface.index implemented.AttributesByKey HashIdentity.Structural

        [
            for entry in published.AttributesByKey do
                match implAttributes.TryGetValue entry.Key with
                | true, defined ->
                    for key in divergentAttributes entry.Value defined do
                        yield
                            {
                                Declaration = SymbolKeyOps.qualifiedName entry.Key
                                Attribute = SymbolKeyOps.typeMetaName key
                            }
                | _ -> ()
        ]

    /// Every finding over a signature's declarations. `implemented` is the surface the
    /// implementation would publish signatureless; `implBindings` is its `(# … #)` bindings.
    let check
        (published: PublishedSurface)
        (implemented: PublishedSurface)
        (implBindings: EqDict<TypeKey, IntrinsicBindingInfo>)
        : ConformanceFindings =
        {
            Errors =
                [
                    yield! checkTypes published implemented implBindings
                    yield! ConformanceBodies.check published implemented
                    yield! checkValues published implemented
                ]
            Divergent = checkAttributes published implemented
        }
