namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open NameResolutionContainers

[<AutoOpen>]
module AttributeResolution =

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

        /// Each written attribute resolved to the declaration it denotes. An unresolved
        /// attribute is an ERROR — silence would ship a declaration without the meaning it
        /// asked for — reported once per site, memoised in `Resolution.AttributeVerdicts`.
        member this.ResolveAttributes(attrs: Attributes<SyntaxToken> voption) : ResolvedAttributes =
            match attrs with
            | ValueNone -> ResolvedAttributes.None
            | ValueSome sets ->
                let resolved = ResizeArray<ResolvedAttribute>()

                for AttributeSet(lBracket = lb; attributes = entries) in sets do
                    for Attribute(construction = construction), _sep in entries do
                        let attrTy =
                            match construction with
                            | ObjectConstruction(typ = t) -> t
                            | InterfaceConstruction(typ = t) -> t

                        match CstKeys.ofTypeRef attrTy with
                        | ValueNone -> this.ReportOnce(lb, Kind.Message "An attribute must be a named type.")
                        | ValueSome typeRef ->
                            let verdict =
                                match this.Resolution.AttributeVerdicts.TryGetValue typeRef.Site.Key with
                                | ValueSome v -> v
                                | ValueNone ->
                                    let v =
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

                                    this.Resolution.AttributeVerdicts.Set(typeRef.Site.Key, v)
                                    v

                            match verdict with
                            | AttributeVerdict.Resolved k -> resolved.Add { Key = k; Construction = construction }
                            | AttributeVerdict.Reported -> ()

                { Entries = List.ofSeq resolved }
