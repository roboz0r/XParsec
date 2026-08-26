namespace XParsec.FSharp.SemanticAnalysis

/// The opens-aware probe over a NAME-KEYED referenced contract: a written spelling → the
/// external identity it denotes at the use site. A source that declares module structure
/// answers through `IScopeContents` instead, which is where `NameResolutionLongIdent` reads
/// a written spelling first.
module ExternalTypeProbe =

    /// Per qualified candidate, probe the compiled name each of `arities` keys it under
    /// (`` Name`2 ``, and `Name` at arity 0) and take the first hit `pick` admits. A shape
    /// whose own typar count differs from the arity probed for is not a hit, so the identity
    /// and the shape `pick` receives agree on arity.
    let tryPickExternalType
        (ctx: PassContext)
        (arities: int list)
        (pick: TypeKey -> ExternalTypeShape -> 'T voption)
        (name: string)
        : 'T voption =
        let lookup (candidate: string) : 'T voption =
            let rec go (remaining: int list) =
                match remaining with
                | [] -> ValueNone
                | arity :: rest ->
                    match ctx.Resolver.TryLookupType(SymbolKeyOps.arityName candidate arity) with
                    | ValueSome(struct (key, shape)) when shape.TyparArity = arity ->
                        match pick key shape with
                        | ValueSome _ as hit -> hit
                        | ValueNone -> go rest
                    | _ -> go rest

            go arities

        OpenScope.tryResolve ctx.Resolution.OpenScope lookup name

    let tryResolveExternalTypeKey (ctx: PassContext) (name: string) (arity: int) : TypeKey voption =
        tryPickExternalType ctx [ arity ] (fun key _ -> ValueSome key) name
