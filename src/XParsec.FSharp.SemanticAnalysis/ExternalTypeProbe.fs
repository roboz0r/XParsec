namespace XParsec.FSharp.SemanticAnalysis

/// The opens-aware probes over the referenced contracts: a written spelling → the external
/// identity it denotes at the use site.
module ExternalTypeProbe =

    [<Struct>]
    type ExternalTypeHit =
        {
            /// The identity the ANSWERING provider registered, never re-cut from `Compiled`.
            Key: TypeKey
            /// The probed name that hit: open-prefix qualified, arity-suffixed if the probe was.
            Compiled: string
            /// The arity the PROBE asked for, not necessarily the shape's own: a bare-keyed
            /// generic (`Vesper.Option`, arity 1) hits the bare probe.
            ProbedTyparArity: int
            Shape: ExternalTypeShape
        }

    /// Per qualified candidate, probe every pair `probes` yields and take the first hit
    /// `pick` admits.
    let tryPickExternalType
        (ctx: PassContext)
        (probes: string -> struct (string * int) list)
        (pick: ExternalTypeHit -> 'T voption)
        (name: string)
        : 'T voption =
        let lookup (candidate: string) : 'T voption =
            let rec go (remaining: struct (string * int) list) =
                match remaining with
                | [] -> ValueNone
                | struct (probe, arity) :: rest ->
                    match ctx.Resolver.TryLookupType probe with
                    | ValueSome(struct (key, shape)) ->
                        let hit =
                            {
                                Key = key
                                Compiled = probe
                                ProbedTyparArity = arity
                                Shape = shape
                            }

                        match pick hit with
                        | ValueSome v -> ValueSome v
                        | ValueNone -> go rest
                    | ValueNone -> go rest

            go (probes candidate)

        OpenScope.tryResolve ctx.Resolution.OpenScope lookup name

    /// Metadata keys a generic type `` Name`arity `` while the contract layer keys it bare,
    /// so the arity-suffixed name is probed first and wins when both could match.
    let arityProbes (arity: int) (candidate: string) : struct (string * int) list =
        if arity = 0 then
            [ struct (candidate, 0) ]
        else
            [
                struct (SymbolKeyOps.arityName candidate arity, arity)
                struct (candidate, arity)
            ]

    /// A NOMINAL type constructor takes the producer's REGISTERED key: only that preserves an `InModule`
    /// containment chain, a re-cut from the dotted spelling flattening the module segment into the
    /// namespace. The rest key off the compiled name, an abbrev dealiasing on read.
    let useSiteTypeKey (hit: ExternalTypeHit) : TypeKey =
        match hit.Shape with
        | ExternalTypeShape.Class _
        | ExternalTypeShape.IntrinsicInterface _
        | ExternalTypeShape.Record _
        | ExternalTypeShape.Union _
        | ExternalTypeShape.Enum _ -> hit.Key
        | ExternalTypeShape.Abbrev _
        | ExternalTypeShape.Intrinsic _
        | ExternalTypeShape.Unmodelled _ -> SymbolKeyOps.qualifiedTypeKeyOf hit.Compiled hit.ProbedTyparArity

    /// At exactly `arity`: a shape whose own typar count differs is not a hit.
    let tryResolveExternalTypeKey (ctx: PassContext) (name: string) (arity: int) : TypeKey voption =
        tryPickExternalType
            ctx
            (arityProbes arity)
            (fun hit ->
                if hit.Shape.TyparArity = hit.ProbedTyparArity then
                    ValueSome(useSiteTypeKey hit)
                else
                    ValueNone
            )
            name
