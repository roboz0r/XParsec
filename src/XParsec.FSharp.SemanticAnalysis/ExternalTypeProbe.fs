namespace XParsec.FSharp.SemanticAnalysis

/// The opens-aware probes over the referenced contracts: a written spelling → the external
/// identity it denotes at the use site.
module ExternalTypeProbe =

    [<Struct>]
    type ExternalTypeHit =
        {
            /// The identity a reference at the use site denotes. A NOMINAL shape takes the
            /// producer's registered key, which preserves an `InModule` containment chain; an
            /// abbrev, an intrinsic or an unmodelled shape takes the probed compiled name,
            /// dealiasing on read.
            UseSiteKey: TypeKey
            /// The arity the PROBE asked for, which a shape declining it still answers under:
            /// the caller compares the two.
            ProbedTyparArity: int
            Shape: ExternalTypeShape
        }

    /// The registered key for a nominal shape, the probe's own cut for the rest. Private, so
    /// `UseSiteKey` is the single identity a caller can reach and a re-cut from a rendering
    /// has no site to happen at.
    let private useSiteKeyOf (registered: TypeKey) (probe: string) (arity: int) (shape: ExternalTypeShape) : TypeKey =
        match shape with
        | ExternalTypeShape.Class _
        | ExternalTypeShape.IntrinsicInterface _
        | ExternalTypeShape.Record _
        | ExternalTypeShape.Union _
        | ExternalTypeShape.Enum _ -> registered
        | ExternalTypeShape.Abbrev _
        | ExternalTypeShape.Intrinsic _
        | ExternalTypeShape.Unmodelled _ -> SymbolKeyOps.qualifiedTypeKeyOf probe arity

    /// Per qualified candidate, probe the compiled name each of `arities` keys it under
    /// (`` Name`2 ``, and `Name` at arity 0) and take the first hit `pick` admits.
    let tryPickExternalType
        (ctx: PassContext)
        (arities: int list)
        (pick: ExternalTypeHit -> 'T voption)
        (name: string)
        : 'T voption =
        let lookup (candidate: string) : 'T voption =
            let rec go (remaining: int list) =
                match remaining with
                | [] -> ValueNone
                | arity :: rest ->
                    let probe = SymbolKeyOps.arityName candidate arity

                    match ctx.Resolver.TryLookupType probe with
                    | ValueSome(struct (key, shape)) ->
                        let hit =
                            {
                                UseSiteKey = useSiteKeyOf key probe arity shape
                                ProbedTyparArity = arity
                                Shape = shape
                            }

                        match pick hit with
                        | ValueSome v -> ValueSome v
                        | ValueNone -> go rest
                    | ValueNone -> go rest

            go arities

        OpenScope.tryResolve ctx.Resolution.OpenScope lookup name

    /// At exactly `arity`: a shape whose own typar count differs is not a hit.
    let tryResolveExternalTypeKey (ctx: PassContext) (name: string) (arity: int) : TypeKey voption =
        tryPickExternalType
            ctx
            [ arity ]
            (fun hit ->
                if hit.Shape.TyparArity = hit.ProbedTyparArity then
                    ValueSome hit.UseSiteKey
                else
                    ValueNone
            )
            name
