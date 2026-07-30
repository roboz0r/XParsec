namespace XParsec.FSharp.Codegen.Common

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis

/// The resolved-specialization GRAPH, spliced back into the trees at EMIT.
///
/// `Passes.InlineExpansion` resolves an inline call — type arguments ground, static-opt
/// clauses selected, trait calls dispatched — and leaves a `TExprG.InlineCall` EDGE into the
/// file's specialization table rather than the body itself. That is what lets an entry's
/// nodes keep the anchors they were WRITTEN at, beside the `OriginFile` saying which file
/// those indices belong to. Placing the body is what is left, and it is this.
///
/// Shared, and necessarily so: both backends need the same beta reduction, the same binder
/// freshening and the same frame chain, and two copies of that is two chances for a call
/// site's locals to alias. Nothing here is target-shaped — `TastAccessor` handles,
/// `FrozenType` and `Anchor` only, the same family `TastLower` already shares.
module InlineExpand =

    /// WHERE a node of an expanded tree was written: the producer file, and the node's own
    /// index into THAT file's tokens. Exactly the pair `OriginSources.tokenAt` consumes, and
    /// a `ForeignAnchor` rather than an `Anchor` because that is what the integer is — an
    /// index read against another file, which resolves in range against the consuming file
    /// and lands on an unrelated token.
    ///
    /// It is recorded ALONGSIDE the node rather than on it: the copy is moved onto the call
    /// site (see `expand`), so the node's own anchor column stays in the consuming file's
    /// domain and every existing reader of it keeps working.
    [<Struct>]
    type NodeOrigin = { File: OriginFile; At: ForeignAnchor }

    /// What one expansion produced: the trees with no edges left, and where the nodes that
    /// came out of the table were written.
    type Expansion =
        {
            /// The declarations, edges spliced. A declaration whose expressions did not move
            /// is the very declaration that came in — the row copies preserve an id when
            /// nothing changed, so a file that reaches no inline body appends nothing.
            Decls: TastAccessor.DeclId list
            /// Every node COPIED out of a specialization entry → the file it was written in
            /// and its position there. A node absent from this is the compiling unit's own:
            /// the frame chain's bottom names no producer, so absence is the answer rather
            /// than a missing entry.
            Origins: IReadOnlyDictionary<TastAccessor.ExprId, NodeOrigin>
            /// Every node this expansion AUTHORED → the node it was authored FROM. A node is
            /// re-authored whenever a descendant of it moved, so splicing a body deep inside a
            /// lambda gives that lambda a new id — and a table of the FROZEN pools keyed by
            /// node (`FrozenPools.FunVerdicts`) then misses it, silently dropping a fact the
            /// emitter needs rather than faulting. Read through `frozenNode`, which follows the
            /// chain back to the node the pools hold.
            Derived: IReadOnlyDictionary<TastAccessor.ExprId, TastAccessor.ExprId>
        }

    /// The node of the FROZEN pools an expanded node was authored from — itself, when the
    /// expansion left it alone. THE reading of `Derived`: the chain can be several links long
    /// (a lambda inside a lambda, both re-authored), and a consumer that followed one link
    /// would find a node the pools do not hold either.
    let rec frozenNode (expansion: Expansion) (e: TastAccessor.ExprId) : TastAccessor.ExprId =
        match expansion.Derived.TryGetValue e with
        | true, from -> frozenNode expansion from
        | _ -> e

    /// One frame of the chain a descent through the graph maintains: which entry was entered,
    /// and the file its nodes are anchored in. Descending an `InlineCall` PUSHES one;
    /// descending a `CallerExpr` POPS back to the caller's, that node being the marker for
    /// material a fusion spliced in from one frame out.
    ///
    /// `Spec` is not decoration: an entry already on the stack is a cycle, and this is where
    /// the acyclicity `Inline.findCycle` checks on the finished table is re-asserted at the
    /// point where a violation would otherwise substitute bodies into bodies forever.
    [<Struct>]
    type private Frame =
        {
            Spec: SpecializationId
            Origin: OriginFile
        }

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

    /// Whether the node being read is the consuming unit's own or an entry's.
    ///
    /// The distinction is the whole of the walk: the unit's own nodes are rewritten IN PLACE
    /// (an untouched subtree keeps the very id the emitter's node-keyed tables were built
    /// from), where an entry's are COPIED, because two call sites of one entry must not share
    /// a binder — and so must not share a node.
    type private Site =
        | InPlace
        | Copied of Copy

    /// The abstraction an entry's edge applies. An entry is ALWAYS a `TDecl.Let` of lambdas
    /// (`TSpecializationG.Decl`), asserted here so nothing below has to re-state it.
    let private entryValue (spec: SpecializationId) (entry: TastAccessor.Specialization) : TastAccessor.ExprId =
        match TastAccessor.declKind entry.Decl with
        | DeclShape.Let -> (TastAccessor.declLet entry.Decl).Value
        | other ->
            let (SpecializationId i) = spec
            failwithf "InlineExpand: specialization %d is not a `Let` declaration, but %A" i other

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
    /// PRECONDITION: the table is ACYCLIC. `Inline.findCycle` checks that on the finished
    /// table and the pass reports `Kind.CyclicInline` rather than emitting, so reaching here
    /// with a cycle is a compiler bug; the frame stack convicts it at the entry that closes
    /// the loop instead of exhausting the stack.
    let expand (pool: PoolBuilder) (decls: TastAccessor.DeclId list) : Expansion =
        let origins = Dictionary<TastAccessor.ExprId, NodeOrigin>()
        let derived = Dictionary<TastAccessor.ExprId, TastAccessor.ExprId>()

        // Record the authorship of a node this walk produced, and nothing when the walk
        // returned the very node it was given (a row copy preserves the id when nothing moved,
        // and a node that IS the frozen one needs no link back to itself).
        let authored (source: TastAccessor.ExprId) (result: TastAccessor.ExprId) : TastAccessor.ExprId =
            if result <> source then
                derived.[result] <- source

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

        // Where the copy of `source` was written, filed against the frame the node sits in.
        // The bottom of the stack is the compiling unit, which names no producer file — so
        // nothing is filed there and absence carries that meaning.
        let record (frames: Frame list) (source: TastAccessor.ExprId) (copied: TastAccessor.ExprId) =
            match frames with
            | frame :: _ ->
                origins.[copied] <-
                    {
                        File = frame.Origin
                        At = ForeignAnchor.ofAnchor (TastAccessor.exprTok source)
                    }
            | [] -> ()

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

        let rec go (frames: Frame list) (site: Site) (e: TastAccessor.ExprId) : TastAccessor.ExprId =
            match TastAccessor.exprKind e with
            | ExprShape.InlineCall -> expandEdge frames site e
            | ExprShape.CallerExpr ->
                // The POP, and the only reading of it. The marked subtree was written by the
                // caller and MOVED into the entry, so it is not the entry's to copy: it stays
                // where it is, keeping its own anchors, its own binders and its own node
                // identity — which is what lets a node-keyed emit table (a lambda's
                // value-struct verdict) still recognise it.
                //
                // Sound only because an entry that marks anything has exactly ONE call edge
                // (`Inline.miscountedFusedEntries`): the material appears once however the
                // graph is walked, so there is no second expansion for it to alias.
                match frames with
                | _ :: outer -> go outer InPlace (TastAccessor.exprChild e 0)
                | [] ->
                    failwith
                        "InlineExpand: a CallerExpr outside every entry — the node pops one frame, so one must have been pushed"
            | _ ->
                match site with
                | InPlace -> authored e (TastAccessor.mapChildren (go frames site) e)
                | Copied copy -> copyNode frames copy e

        and copyNode (frames: Frame list) (copy: Copy) (e: TastAccessor.ExprId) : TastAccessor.ExprId =
            // Patterns first: a binder is bound before any reference to it can be rewritten,
            // and a use is always lexically inside its binder.
            let pats =
                TastAccessor.exprPatChildren e |> Array.map (fun p -> (copyPat copy p).Id)

            let kids =
                TastAccessor.exprChildren e
                |> Array.map (fun c -> (go frames (Copied copy) c).Id)

            // The payload's own positions move with the node; the loop variable is a binder the
            // payload carries rather than a pattern, so it freshens here alongside them.
            let payload (p: ExprPayload) : ExprPayload =
                match ExprPayload.mapToks (fun _ -> copy.At) p with
                | ExprPayload.ForTo ft -> ExprPayload.ForTo {| ft with Var = bind copy ft.Var |}
                | moved -> moved

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

            record frames e copied
            authored e copied

        and expandEdge (frames: Frame list) (site: Site) (e: TastAccessor.ExprId) : TastAccessor.ExprId =
            let spec = TastAccessor.exprInlineCallSpec e
            let entry = TastAccessor.specialization pool spec

            match frames |> List.tryFind (fun f -> f.Spec = spec) with
            | Some _ ->
                let (SpecializationId i) = spec

                failwithf
                    "InlineExpand: specialization %d (%A) reaches itself — the table is acyclic by `Inline.findCycle`, checked before anything walks it"
                    i
                    entry.Key.Template
            | None -> ()

            // Where the edge SITS, which is where the body it names is moved to. A copied edge
            // has already been moved onto the enclosing call site, so an inner body collapses
            // onto the outermost one, exactly as a chain of physical splices did.
            let at =
                match site with
                | InPlace -> TastAccessor.exprTok e
                | Copied copy -> copy.At

            // The arguments are the CALLER's material: read in the site the edge itself sits
            // in, never in the entry's.
            let args = TastAccessor.exprChildren e |> Array.map (go frames site)

            let body =
                go
                    ({ Spec = spec; Origin = entry.Origin } :: frames)
                    (Copied
                        {
                            At = at
                            Binders = Dictionary<BinderId, BinderId>()
                        })
                    (entryValue spec entry)

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
            Decls = decls |> List.map (TastAccessor.mapDeclBodies (go [] InPlace))
            Origins = origins
            Derived = derived
        }
