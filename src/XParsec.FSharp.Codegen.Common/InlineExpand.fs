namespace XParsec.FSharp.Codegen.Common

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis

/// Splice each `TExprG.InlineCall` edge with the body of the specialization it identifies, and
/// freshen that body's bound variables. Anchors stay readable: a copied node records the file it was
/// WRITTEN in (`NodeOrigin`) while its own anchor moves to the call site.
module InlineExpand =

    /// The producer file a node was written in, and its index into THAT file's tokens —
    /// neither half means anything without the other. Held beside the node because the node's
    /// own anchor was moved onto the call site.
    [<Struct>]
    type NodeOrigin = { File: AssemblyFilePath; At: Anchor }

    /// Every node a rewrite AUTHORED → the node it was authored from. Splicing a body inside a
    /// lambda re-authors that lambda, so a table keyed by node (`FrozenPools.FunVerdicts`,
    /// `Expansion.Origins`) then misses it silently; `tryFind` is what reads it back.
    type Derivation =
        private
            {
                Links: Dictionary<TastAccessor.ExprId, TastAccessor.ExprId>
            }

    module Derivation =

        let create () : Derivation = { Links = Dictionary() }

        /// Record that `result` was authored from `source`; nothing, when the rewrite returned
        /// the very node it was given.
        let authored (d: Derivation) (source: TastAccessor.ExprId) (result: TastAccessor.ExprId) : unit =
            if result <> source then
                d.Links.[result] <- source

        /// Continue `source`'s links in `into`, so a chain that starts in one rewrite and is
        /// extended by the next is followed as one.
        let absorb (into: Derivation) (source: Derivation) : unit =
            for KeyValue(result, from) in source.Links do
                into.Links.[result] <- from

        /// The fact `table` holds for the NEAREST node on `e`'s authorship chain — `e`'s own,
        /// where no rewrite touched it. Chains run several links deep; each link points at the
        /// OLDER node it was authored from, so the walk strictly descends and terminates.
        let rec tryFind
            (d: Derivation)
            (table: IReadOnlyDictionary<TastAccessor.ExprId, 'a>)
            (e: TastAccessor.ExprId)
            : 'a voption =
            match table.TryGetValue e with
            | true, v -> ValueSome v
            | _ ->
                match d.Links.TryGetValue e with
                | true, from -> tryFind d table from
                | _ -> ValueNone

        /// Every derived node paired with the fact its chain lands on — the EAGER `tryFind`,
        /// for a consumer that must hand on a node-keyed table. No fact on the chain ⇒ absent.
        let resolveAll
            (d: Derivation)
            (table: IReadOnlyDictionary<TastAccessor.ExprId, 'a>)
            : (TastAccessor.ExprId * 'a) seq =
            seq {
                for KeyValue(node, _) in d.Links do
                    match tryFind d table node with
                    | ValueSome v -> node, v
                    | ValueNone -> ()
            }

    /// What one expansion produced: the trees with no edges left, and where the nodes that
    /// came out of the table were written.
    type Expansion =
        {
            /// The declarations, edges spliced. A declaration whose expressions did not move
            /// comes back as itself, so a file with no inline call appends no rows.
            Decls: TastAccessor.DeclId list
            /// Every node anchored in a file OTHER than the one being compiled → that file and
            /// its position in it; absence means the compiling file's own. Keyed by the copied
            /// node, so read it through `Derived`. Only a source-map-emitting backend reads it.
            Origins: IReadOnlyDictionary<TastAccessor.ExprId, NodeOrigin>
            /// Every node this expansion authored → the node it was authored from.
            Derived: Derivation
        }

    /// WHICH FILE the material being walked is anchored in: a producer's, or the consuming
    /// file's own. Both are `AssemblyFilePath`s; the walk is TOLD which by the node it descends
    /// through, so the comparison happens once per domain entered, not once per node filed.
    [<Struct>]
    type private Domain =
        | Consuming
        | Producer of AssemblyFilePath

    /// The state of ONE entry-body copy. Per copy, not per expansion: an entry reached from
    /// inside another entry's body takes its own, so the two copies' variables cannot collide;
    /// that inner edge's arguments, written in the OUTER body, are copied under the outer one.
    type private Copy =
        {
            /// The call-site anchor every node of this copy is moved onto: an `Anchor` indexes
            /// one file's tokens, so a producer's node sitting in the consuming file's tree
            /// must be readable against that file. Where it came from is `NodeOrigin`.
            At: Anchor
            /// This copy's own bound variables, old → new. One not in it is FREE in the body —
            /// a reference to the consuming scope — and passes through: a use is always
            /// lexically inside the scope binding it, so a pre-order copy has bound it first.
            BoundVars: Dictionary<BoundVarId, BoundVarId>
        }

    /// Whether the node being read is rewritten IN PLACE or COPIED — a question about SHARING,
    /// not about which file it was written in (`Domain`): two call sites of one entry must not
    /// share a bound variable, so they must not share a node. Everything else keeps its id.
    type private Site =
        | InPlace
        | Copied of Copy

    /// One entry the walk is currently INSIDE, and the site the material that called it was
    /// walked at. They pop together: a `CallerExpr`'s subtree is the CALLER's, so keeping the
    /// site would leave its `Var` references resolving to the uncopied body's bound variables.
    [<NoEquality; NoComparison>]
    type private Entered =
        {
            Spec: SpecializationId
            CallerSite: Site
        }

    /// Splice every `TExprG.InlineCall` in `decls` — member bodies included, via
    /// `mapDeclBodies` — with the entry it identifies, applied to the edge's own arguments. A
    /// `TExprG.CallerExpr` is unwrapped and its subtree left where it stands, being the caller's.
    let expand (pool: PoolBuilder) (decls: TastAccessor.DeclId list) : Expansion =
        let origins = Dictionary<TastAccessor.ExprId, NodeOrigin>()
        let derived = Derivation.create ()

        // The one file whose anchors need no provenance is the file being compiled. It has to
        // be the identity the front end stamped onto the nodes — one rebuilt here from a path
        // would compare unequal and file this file's own code as if it were foreign.
        let compiling = TastPoolBuilder.path pool

        let domainOf (stamp: AssemblyFilePath) : Domain =
            if stamp = compiling then Consuming else Producer stamp

        let authored (source: TastAccessor.ExprId) (result: TastAccessor.ExprId) : TastAccessor.ExprId =
            Derivation.authored derived source result
            result

        // The copy's own bound variable for `b`, minting one on first sight. A `Var` reference
        // resolves through the same table, so a variable and its uses cannot resolve to different slots.
        let bind (copy: Copy) (b: BoundVarId) : BoundVarId =
            let fresh = TastPoolBuilder.mintBoundVar pool
            copy.BoundVars.[b] <- fresh
            fresh

        let useBoundVar (copy: Copy) (b: BoundVarId) : BoundVarId =
            match copy.BoundVars.TryGetValue b with
            | true, fresh -> fresh
            | _ -> b

        // Where `source` was written, filed against the node that landed. Nothing is filed for
        // the consuming file's own material — including a COPY of it, so an inlined body of this
        // file's own template reads at the call site that asked for it, as a debugger wants.
        let record (domain: Domain) (source: TastAccessor.ExprId) (landed: TastAccessor.ExprId) =
            match domain with
            | Consuming -> ()
            | Producer file ->
                origins.[landed] <-
                    {
                        File = file
                        At = TastAccessor.exprTok source
                    }

        let rec copyPat (copy: Copy) (p: TastAccessor.PatId) : TastAccessor.PatId =
            let kids = TastAccessor.patChildren p |> Array.map (fun k -> (copyPat copy k).Id)

            {
                Pool = p.Pool
                Id =
                    TastPoolBuilder.copyPatFresh
                        p.Pool
                        p.Id
                        (fun row ->
                            { row with
                                Tok = copy.At
                                Children = kids
                                Payload =
                                    match row.Payload with
                                    | PatPayload.NamedSimple b -> PatPayload.NamedSimple(bind copy b)
                                    | other -> other
                            }
                        )
            }

        let rec go
            (domain: Domain)
            (entered: Entered list)
            (site: Site)
            (e: TastAccessor.ExprId)
            : TastAccessor.ExprId =
            match TastAccessor.exprKind e with
            | ExprShape.InlineCall -> expandEdge domain entered site e
            | ExprShape.CallerExpr ->
                // The marked subtree is the CALLER's, moved into the entry, so it is walked at
                // the caller's site and in the caller's domain — a producer's if that site was
                // itself inside an entry. Sound only because a fusing entry has ONE call edge.
                let caller = domainOf (TastAccessor.exprCallerExprSource e)

                // The entry stack pops with the material: `a && (c && d)` puts a second edge to
                // one specialization inside the first one's marked subtree, so without the pop
                // the self-reference check below convicts a legal program.
                match entered with
                | frame :: outer -> go caller outer frame.CallerSite (TastAccessor.exprChild e 0)
                | [] ->
                    failwith
                        "InlineExpand: a CallerExpr outside every entry, though the node marks material moved INTO a body, so one must have been entered"
            | _ ->
                match site with
                | InPlace ->
                    record domain e e
                    authored e (TastAccessor.mapChildren (go domain entered site) e)
                | Copied copy -> copyNode domain entered copy e

        and copyNode
            (domain: Domain)
            (entered: Entered list)
            (copy: Copy)
            (e: TastAccessor.ExprId)
            : TastAccessor.ExprId =
            // EVERY variable this node introduces is bound before any child is copied: a use is
            // lexically inside the scope binding it, so a child copied first would rewire its
            // reference through a slot that does not exist yet. `ForTo` carries its var on the payload.
            let pats =
                TastAccessor.exprPatChildren e |> Array.map (fun p -> (copyPat copy p).Id)

            let loopVar =
                match TastAccessor.exprKind e with
                | ExprShape.ForTo -> ValueSome(bind copy (TastAccessor.exprForTo e).Var)
                | _ -> ValueNone

            let kids =
                TastAccessor.exprChildren e
                |> Array.map (fun c -> (go domain entered (Copied copy) c).Id)

            // The payload's own positions move with the node; its loop variable is the one bound above.
            let payload (p: ExprPayload) : ExprPayload =
                match ExprPayload.mapToks (fun _ -> copy.At) p, loopVar with
                | ExprPayload.ForTo ft, ValueSome v -> ExprPayload.ForTo {| ft with Var = v |}
                | moved, _ -> moved

            let copied: TastAccessor.ExprId =
                {
                    Pool = e.Pool
                    Id =
                        TastPoolBuilder.copyExprFresh
                            e.Pool
                            e.Id
                            (fun row ->
                                { row with
                                    Tok = copy.At
                                    Children = kids
                                    PatChildren = pats
                                    VarBoundVar = row.VarBoundVar |> ValueOption.map (useBoundVar copy)
                                    Payload = payload row.Payload
                                }
                            )
                }

            record domain e copied
            authored e copied

        and expandEdge
            (domain: Domain)
            (entered: Entered list)
            (site: Site)
            (e: TastAccessor.ExprId)
            : TastAccessor.ExprId =
            let spec = TastAccessor.exprInlineCallSpec e
            let entry = TastAccessor.specialization pool spec

            // The node states the domain of its own anchor; the descent derives the same thing
            // from the edges it came through. A disagreement is the only warning: an anchor read
            // in the wrong file's index space resolves in range and points to an unrelated token.
            let stated = domainOf (TastAccessor.exprInlineCallSource e)

            if stated <> domain then
                failwithf
                    "InlineExpand: an edge to specialization %A says its material is anchored in %A, but the descent that reached it was walking %A"
                    spec
                    stated
                    domain

            if entered |> List.exists (fun f -> f.Spec = spec) then
                failwithf
                    "InlineExpand: specialization %A (%A) reaches itself, but the table is checked acyclic before anything walks it"
                    spec
                    entry.Key.Template

            // Where the edge SITS is where the body it identifies is moved to. A copied edge already
            // sits on the enclosing call site, so an inner body collapses onto the outermost one.
            let at =
                match site with
                | InPlace -> TastAccessor.exprTok e
                | Copied copy -> copy.At

            // The arguments are the CALLER's material: read in the site and domain the edge
            // itself sits in, never in the entry's.
            let args = TastAccessor.exprChildren e |> Array.map (go domain entered site)

            // The body is the ENTRY's, and the entry states where it was written.
            let body =
                go
                    (domainOf entry.Source)
                    ({ Spec = spec; CallerSite = site } :: entered)
                    (Copied
                        {
                            At = at
                            BoundVars = Dictionary<BoundVarId, BoundVarId>()
                        })
                    entry.Value

            betaReduce at body (List.ofArray args)

        // Lower an application of a curried lambda to its arguments into a `Let` chain. A
        // leftover lambda is a partial application and stands. The `Let` sits at the call site
        // and keeps the already-freshened parameter pattern, so its slot is that site's own.
        and betaReduce (at: Anchor) (fn: TastAccessor.ExprId) (args: TastAccessor.ExprId list) : TastAccessor.ExprId =
            match args with
            | [] -> fn
            | arg :: rest ->
                match TastAccessor.exprKind fn with
                | ExprShape.Lambda ->
                    let lam = TastAccessor.exprLambda fn

                    match TastAccessor.patKind lam.Param with
                    | PatShape.NamedSimple ->
                        let reduced = betaReduce at lam.Body rest
                        TastAccessor.mintLet lam.Param arg reduced (TastAccessor.exprTy reduced) at
                    | other -> failwithf "InlineExpand: inline parameter destructuring is out of scope: %A" other
                | other -> failwithf "InlineExpand: over-application of an inline body, at a %A" other

        {
            Decls = decls |> List.map (TastAccessor.mapDeclBodies (go Consuming [] InPlace))
            Origins = origins
            Derived = derived
        }
