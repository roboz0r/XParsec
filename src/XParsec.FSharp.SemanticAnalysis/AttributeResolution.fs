namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open NameResolutionContainers

[<AutoOpen>]
module AttributeResolution =

    /// Each attribute written in `sets`, paired with the `[<` opening its set.
    let private writtenAttributes
        (sets: Attributes<SyntaxToken>)
        : struct (SyntaxToken * ObjectConstruction<SyntaxToken>) seq =
        seq {
            for AttributeSet(lBracket = lb; attributes = entries) in sets do
                for Attribute(construction = construction), _sep in entries do
                    yield struct (lb, construction)
        }

    let private constructedType (construction: ObjectConstruction<SyntaxToken>) : Type<SyntaxToken> =
        match construction with
        | ObjectConstruction(typ = t) -> t
        | InterfaceConstruction(typ = t) -> t

    type PassContext with

        /// F#'s suffix rule: `Attribute`-suffixed FIRST, then as written, so `[<Foo>]` binds
        /// `FooAttribute` even where a non-attribute `Foo` is in scope. Deliberately unstamped
        /// as a type-ref verdict; the verdict lives in `Resolution.AttributeVerdicts`.
        member private this.TryResolveAttributeTypeKey(typeRef: CstKeys.TypeRef) : TypeKey voption =
            let useSite = this.UseSiteAt typeRef.Site.Key
            let written = this.WrittenTypeNameOf typeRef.LongIdent

            let tryName (name: string) : TypeKey voption =
                let w = { written with Name = name }

                match TypeRegistry.tryWrittenTypeClaim this.Types useSite w typeRef.TyparArity with
                | ValueSome claim -> ValueSome claim.Key
                | ValueNone ->
                    tryPickExternalWritten
                        this
                        useSite
                        (WrittenArity.Exact typeRef.TyparArity)
                        (fun key _ -> ValueSome key)
                        (Qualifier.ofPath w.Path)
                        w.Name

            match tryName (written.Name + RuntimeNames.AttributeSuffix) with
            | ValueSome k -> ValueSome k
            | ValueNone -> tryName written.Name

        /// The key `typeRef` denotes, memoised per site in `Resolution.AttributeVerdicts`. An
        /// unresolved attribute is an ERROR, reported at the first read of its site.
        member private this.AttributeKeyAt(typeRef: CstKeys.TypeRef) : TypeKey voption =
            match this.Resolution.AttributeVerdicts.TryGetValue typeRef.Site.Key with
            | ValueSome verdict -> verdict.Key
            | ValueNone ->
                let verdict =
                    match this.TryResolveAttributeTypeKey typeRef with
                    | ValueSome k -> AttributeVerdict.Resolved k
                    | ValueNone ->
                        this.Report(
                            typeRef.Site.Tok,
                            Kind.Message(
                                sprintf
                                    "The attribute '%s' does not resolve to a type here, so it would have no effect. Reference the contract that declares it, or qualify the path to the type meant."
                                    (this.WrittenTypeNameOf typeRef.LongIdent).Written
                            )
                        )

                        AttributeVerdict.Reported

                this.Resolution.AttributeVerdicts.Set(typeRef.Site.Key, verdict)
                verdict.Key

        /// `AttributeKeyAt` without the memo: the recorded verdict, else a fresh resolution
        /// that records and reports nothing.
        member private this.PeekAttributeKey(typeRef: CstKeys.TypeRef) : TypeKey voption =
            match this.Resolution.AttributeVerdicts.TryGetValue typeRef.Site.Key with
            | ValueSome verdict -> verdict.Key
            | ValueNone -> this.TryResolveAttributeTypeKey typeRef

        /// Each written attribute resolved to the declaration it denotes; an unresolved one
        /// is reported once per site and omitted.
        member this.ResolveAttributes(attrs: Attributes<SyntaxToken> voption) : ResolvedAttributes =
            match attrs with
            | ValueNone -> ResolvedAttributes.None
            | ValueSome sets ->
                let resolved = ResizeArray<ResolvedAttribute>()

                for struct (lb, construction) in writtenAttributes sets do
                    match CstKeys.ofTypeRef (constructedType construction) with
                    | ValueNone -> this.ReportOnce(lb, Kind.Message "An attribute must be a named type.")
                    | ValueSome typeRef ->
                        match this.AttributeKeyAt typeRef with
                        | ValueSome k ->
                            resolved.Add
                                {
                                    Key = k
                                    TypeRef = typeRef
                                    Construction = construction
                                }
                        | ValueNone -> ()

                { Entries = List.ofSeq resolved }

        /// Whether an attribute written in `attrs` denotes `key`. Records and reports nothing,
        /// so it is safe at CLAIM time; `ResolveAttributes` at registration reports what stays
        /// unresolved.
        member this.HasAttribute(attrs: Attributes<SyntaxToken> voption, key: TypeKey) : bool =
            match attrs with
            | ValueNone -> false
            | ValueSome sets ->
                writtenAttributes sets
                |> Seq.exists (fun (struct (_, construction)) ->
                    match CstKeys.ofTypeRef (constructedType construction) with
                    | ValueNone -> false
                    | ValueSome typeRef -> this.PeekAttributeKey typeRef = ValueSome key
                )
