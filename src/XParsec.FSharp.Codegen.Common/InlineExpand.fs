namespace XParsec.FSharp.Codegen.Common

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis

/// The resolved-specialization GRAPH, spliced back into the trees at EMIT.
///
/// The pre-freeze pass resolves an inline call — type arguments ground, static-opt
/// clauses selected, trait calls dispatched — and leaves a `TExprG.InlineCall` EDGE into the
/// file's specialization table rather than the body itself. That is what lets an entry's
/// nodes keep the anchors they were WRITTEN at, beside the `OriginFile` saying which file
/// those indices belong to. Placing the body is what is left, and it is this.
///
/// Shared, and necessarily so: both backends need the same beta reduction, the same binder
/// freshening and the same reading of which file each node's anchors index, and two copies of
/// that is two chances for a call site's locals to alias. Nothing here is target-shaped —
/// `TastAccessor` handles, `FrozenType` and `Anchor` only, the family `TastLower` shares.
module InlineExpand =

    /// WHERE a node of an expanded tree was written: the producer file, and the node's own
    /// index into THAT file's tokens — exactly the pair needed to read a token back out of a
    /// producer's retained `Lexed`, and neither half means anything without the other.
    ///
    /// It is recorded ALONGSIDE the node rather than on it because a copy is moved onto the
    /// call site (see `expand`), which leaves the anchor COLUMN in the consuming file's domain
    /// for every existing reader of it while the position the node was written at survives
    /// here.
    [<Struct>]
    type NodeOrigin = { File: OriginFile; At: Anchor }

    /// Every node a REWRITE authored → the node it was authored from. A node is re-authored
    /// whenever a descendant of it moved, so splicing a body deep inside a lambda gives that
    /// lambda a new id — and any table keyed by NODE (`FrozenPools.FunVerdicts`, the producer
    /// file a copied node was written in) then misses it, silently dropping a fact the emitter
    /// needs rather than faulting.
    ///
    /// ONE relation for every rewrite that re-authors a node, so a consumer carrying node-keyed
    /// facts across a splice reads one table however many rewrites ran: the expansion below
    /// fills it, and an emitter that derives further nodes of its own (the JS backend's
    /// pure-`let` substitution) keeps filling the same one.
    ///
    /// Read through `tryFind` — never by a direct lookup, which would see only the nodes the
    /// LAST rewrite authored.
    type Derivation =
        private
            {
                Links: Dictionary<TastAccessor.ExprId, TastAccessor.ExprId>
            }

    module Derivation =

        let create () : Derivation = { Links = Dictionary() }

        /// Record that `result` was authored from `source`, and nothing when the rewrite
        /// returned the very node it was given (a row copy preserves the id when nothing moved,
        /// and a node that IS the one a table was keyed against needs no link back to itself).
        let authored (d: Derivation) (source: TastAccessor.ExprId) (result: TastAccessor.ExprId) : unit =
            if result <> source then
                d.Links.[result] <- source

        /// Continue `source`'s relation in `into` — what an emitter takes over from the
        /// expansion whose output it walks, so the chain a node's facts hang off spans both.
        let absorb (into: Derivation) (source: Derivation) : unit =
            for KeyValue(result, from) in source.Links do
                into.Links.[result] <- from

        /// The fact `table` holds for the NEAREST node on `e`'s authorship chain — `e`'s own
        /// when the rewrites left it alone. THE reading of a derivation, and the only one: the
        /// chain can be several links long (a lambda inside a lambda, both re-authored; a
        /// substitution over material an expansion already copied), so a consumer that followed
        /// one link would still miss, and one that followed the chain to its END would walk past
        /// the copy the fact was filed against.
        ///
        /// Terminates because a link always points from a node MINTED by a rewrite to the older
        /// node it was authored from, so the chain strictly descends.
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

        /// Every derived node paired with the fact its chain lands on — the EAGER reading, for
        /// a consumer that must hand on a node-keyed table rather than resolve one node at a
        /// time. A node whose chain reaches no fact is absent, exactly as `tryFind` says.
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
            /// is the very declaration that came in — the row copies preserve an id when
            /// nothing changed, so a file that reaches no inline body appends nothing.
            Decls: TastAccessor.DeclId list
            /// Every node whose anchor indexes a file OTHER than the one being compiled → that
            /// file and its position in it. A node absent from this is the compiling unit's
            /// own, so absence is the answer rather than a missing entry.
            ///
            /// Mostly nodes COPIED out of a specialization entry, which a consumer must
            /// therefore read through `Derived` rather than by a bare lookup — but not only:
            /// material a `CallerExpr` marks is left in place and keeps the anchors of the file
            /// it was written in, which is a producer's whenever the call site it came from was
            /// itself inside an entry.
            Origins: IReadOnlyDictionary<TastAccessor.ExprId, NodeOrigin>
            /// Every node this expansion authored → the node it was authored from
            /// (`Derivation`).
            Derived: Derivation
        }

    /// WHICH FILE the material being walked is anchored in, as the walk needs to know it: a
    /// producer's, or the consuming unit's own. Both are `OriginFile`s and the walk is TOLD
    /// which by the node it descends through — `Consuming` is not a missing answer but the
    /// answer compared for once, at the point a domain is entered, rather than at every node
    /// a copy is filed from.
    ///
    /// The distinction is what `Expansion.Origins` records: a node already in the consuming
    /// unit's index space is read against the tree it sits in, which is what every consumer of
    /// an `Anchor` does by default, so filing it would say nothing.
    [<Struct>]
    type private Domain =
        | Consuming
        | Producer of OriginFile

    /// The state of ONE entry-body copy. Per copy and not per expansion: an entry reached
    /// from inside another entry's body takes its own, so the two copies' binders cannot
    /// collide, while the arguments of that inner edge — written in the OUTER body — are
    /// copied under the outer one.
    type private Copy =
        {
            /// The call-site anchor every node of this copy is moved onto. Forced, not
            /// chosen: an `Anchor` is an index into one file's tokens, so a producer's node
            /// sitting in the consuming file's tree must be readable against that file.
            /// What the copy does NOT lose is where it came from — that is `NodeOrigin`.
            At: Anchor
            /// This copy's own binders, old → new. A binder not in it is FREE in the body
            /// (a reference to the consuming scope) and passes through: a use is always
            /// lexically inside its binder, so a pre-order copy has bound it first.
            Binders: Dictionary<BinderId, BinderId>
        }

    /// Whether the node being read is rewritten IN PLACE or COPIED — which is a question about
    /// SHARING, not about which file the node was written in (`Domain`): an entry's body is
    /// copied because two call sites of one entry must not share a binder and so must not
    /// share a node, where everything else keeps the very id the emitter's node-keyed tables
    /// were built from.
    type private Site =
        | InPlace
        | Copied of Copy

    /// One entry the walk is currently INSIDE, and the site the material that called it was
    /// being walked at.
    ///
    /// The two travel together because a `CallerExpr` pops both at once: the marked subtree is
    /// the CALLER's, so it belongs to whatever the caller belonged to. That is `InPlace` only at
    /// the outermost — where the caller is the file's own declarations — and is the enclosing
    /// COPY wherever the calling body is itself an entry being copied. Popping the entry while
    /// keeping the site would leave such a subtree's binder references naming the uncopied
    /// body's binders, which nothing in the emitted tree declares.
    [<NoEquality; NoComparison>]
    type private Entered =
        {
            Spec: SpecializationId
            CallerSite: Site
        }

    /// Splice every `TExprG.InlineCall` in `decls` — including the ones inside MEMBER bodies,
    /// which no expression-level entry point of either backend reaches — replacing it with
    /// the entry it names, applied to the edge's own arguments, and descending into the
    /// entry's own edges as it goes. Every `TExprG.CallerExpr` is unwrapped — it is transparent
    /// to semantics, so dropping it changes evaluation order not at all — and the subtree under
    /// it is left exactly where it stands, being the caller's own.
    ///
    /// Total on the graph and idempotent on its output — decls with no edges come back as the
    /// very decls that went in.
    ///
    /// PRECONDITION: the table is ACYCLIC. `InlineSpecTable.findCycle` checks that on the
    /// finished table and the pass reports `Kind.CyclicInline` rather than emitting, so
    /// reaching here with a cycle is a compiler bug; the stack of entered entries convicts it
    /// at the entry that closes the loop instead of exhausting the call stack.
    let expand (pool: PoolBuilder) (decls: TastAccessor.DeclId list) : Expansion =
        let origins = Dictionary<TastAccessor.ExprId, NodeOrigin>()
        let derived = Derivation.create ()

        // A stated domain, read: the one file whose anchors need no provenance is the file
        // being compiled, and it is the pool that says which that is. It has to be the very
        // identity the front end stamped onto the nodes — one rebuilt here from a path would
        // compare unequal and file the unit's own code as if it were foreign.
        let compiling = TastPoolBuilder.origin pool

        let domainOf (origin: OriginFile) : Domain =
            if origin = compiling then Consuming else Producer origin

        let authored (source: TastAccessor.ExprId) (result: TastAccessor.ExprId) : TastAccessor.ExprId =
            Derivation.authored derived source result
            result

        // The copy's own binder for `b`, minting one on first sight. A `Var` reference resolves
        // through the same table, so a binder and the references to it cannot come to name
        // different slots.
        let bind (copy: Copy) (b: BinderId) : BinderId =
            let fresh = TastPoolBuilder.mintBinder pool
            copy.Binders.[b] <- fresh
            fresh

        let useBinder (copy: Copy) (b: BinderId) : BinderId =
            match copy.Binders.TryGetValue b with
            | true, fresh -> fresh
            | _ -> b

        // Where `source` was written, filed against the node that lands in the tree — the copy
        // where one was taken, the node itself where it stays put. Nothing is filed for the
        // consuming unit's own material, and absence carries that meaning.
        //
        // That includes a COPY of the consuming unit's own material — a template of this file,
        // whose entry keeps the positions it was written at. Deliberate: absence means "read the
        // node's own anchor", which for such a copy is the call site it was moved onto, and
        // attributing an inlined body to the call that asked for it is what a stack trace and a
        // debugger want. Filing it would instead point every node at the template's definition,
        // which is a different (and defensible) answer, so the choice is made here rather than
        // inherited from the domain test.
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
                // The marked subtree was written by the caller and MOVED into the entry, so it
                // is not the ENTRY's to copy: it belongs to whatever the caller belonged to, and
                // is walked at the caller's own site — which keeps its node identity where that
                // caller is the file's own declarations, and lets a node-keyed emit table (a
                // lambda's value-struct verdict) still recognise it.
                //
                // Sound only because an entry that marks anything has exactly ONE call edge
                // (`InlineSpecTable.miscountedFusedEntries`): the material appears once however
                // the graph is walked, so there is no second expansion for it to alias.
                //
                // The node states the caller's domain, which is NOT the consuming unit's
                // whenever the call site was itself inside an entry. So the anchors it keeps
                // may be a producer's, and the material is filed as it is walked.
                let caller = domainOf (TastAccessor.exprCallerExprOrigin e)

                // The entry stack pops with the material: the caller may legitimately reach
                // the very entry this node sits in — `a && (c && d)` puts a second edge to
                // one specialization inside the first one's marked subtree — and without the
                // pop the acyclicity assertion below convicts a legal program.
                match entered with
                | frame :: outer -> go caller outer frame.CallerSite (TastAccessor.exprChild e 0)
                | [] ->
                    failwith
                        "InlineExpand: a CallerExpr outside every entry — the node marks material moved INTO a body, so one must have been entered"
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
            // EVERY binder this node introduces is bound before any child is copied: a use is
            // always lexically inside its binder, so a child copied first would rewire a
            // reference through a binding that does not exist yet and keep the ORIGINAL id —
            // which resolves to nothing in the copy. Patterns are one such binder; a `ForTo`
            // carries its loop variable on the PAYLOAD instead, and it binds the body just the
            // same.
            let pats =
                TastAccessor.exprPatChildren e |> Array.map (fun p -> (copyPat copy p).Id)

            let loopVar =
                match TastAccessor.exprKind e with
                | ExprShape.ForTo -> ValueSome(bind copy (TastAccessor.exprForTo e).Var)
                | _ -> ValueNone

            let kids =
                TastAccessor.exprChildren e
                |> Array.map (fun c -> (go domain entered (Copied copy) c).Id)

            // The payload's own positions move with the node; its loop variable is the binder
            // taken above.
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
                                    VarBinder = row.VarBinder |> ValueOption.map (useBinder copy)
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

            // The node states the domain of its own anchor and its arguments'; the descent
            // derives the same thing from the edges it came through. They agree, or one of
            // them is wrong and the disagreement is the only warning: an anchor read in the
            // wrong file's index space resolves in range and names an unrelated token.
            let stated = domainOf (TastAccessor.exprInlineCallOrigin e)

            if stated <> domain then
                failwithf
                    "InlineExpand: an edge to specialization %A says its material is anchored in %A, but the descent that reached it was walking %A"
                    spec
                    stated
                    domain

            if entered |> List.exists (fun f -> f.Spec = spec) then
                failwithf
                    "InlineExpand: specialization %A (%A) reaches itself — the table is acyclic by `InlineSpecTable.findCycle`, checked before anything walks it"
                    spec
                    entry.Key.Template

            // Where the edge SITS, which is where the body it names is moved to. A copied edge
            // has already been moved onto the enclosing call site, so an inner body collapses
            // onto the outermost one, exactly as a chain of physical splices did.
            let at =
                match site with
                | InPlace -> TastAccessor.exprTok e
                | Copied copy -> copy.At

            // The arguments are the CALLER's material: read in the site the edge itself sits
            // in, and in the domain it sits in, never in the entry's.
            let args = TastAccessor.exprChildren e |> Array.map (go domain entered site)

            // The body is the ENTRY's, and the entry states where it was written — the pop
            // this replaces could only say "one file out", which names a file at all only
            // where the entry has exactly one call edge.
            let body =
                go
                    (domainOf entry.Origin)
                    ({ Spec = spec; CallerSite = site } :: entered)
                    (Copied
                        {
                            At = at
                            Binders = Dictionary<BinderId, BinderId>()
                        })
                    entry.Value

            betaReduce at body (List.ofArray args)

        // Lower an application of a curried lambda to its arguments into a `Let` chain. The
        // lambda count is at least the argument count; a leftover lambda is a partial
        // application and stands. The synthesised `Let` sits at the call site and keeps the
        // (already copied, already freshened) parameter pattern, so the binder a call site's
        // local slot comes from is that call site's own.
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
