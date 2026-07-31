namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// WHERE a node's projections LAND: the sink a pooling walk pours rows into, the walk
// itself, and `toPools` — the whole-file fill that is the last step of the freeze
// (`Freeze.run`) and its only production caller. What a node projects TO is
// `TastPoolShapes.fs`, which this file is the sole consumer of; the drain — `substitute*`
// and `ofPools`, rebuilding the DU from the columns alone — is `TastUnpool.fs`; the
// wire-shape types are split by scope, one NODE (`ExprPoolId`/`ExprPayload`/`ExprRow`/…,
// `TastPoolNodes.fs`) versus the whole FILE (`FrozenPools` and its side-table containers,
// `TastPoolTypes.fs`).
//
// The corpus-wide `TastUnpool.ofPools ∘ toPools = id` is the proof that the columns carry
// the whole tree.

[<RequireQualifiedAccess>]
module TastPools =

    open TastPoolShapes

    /// The id-keyed record a row cannot carry, reported as the walk appends the node it
    /// belongs to. One case per such record, so the datum a case carries is the one that
    /// case needs and there is no argument that is meaningful only under some other
    /// field's value. The DU node is deliberately NOT passed: a sink must be expressible
    /// without one, since the columns are Node-sufficient.
    [<RequireQualifiedAccess>]
    type PooledEvent<'id> =
        /// A `Var`'s reference edge. The walk cannot fill it — binder ids are the sink's
        /// to assign, and a `Var` may name a binder the walk has not reached — so it
        /// reports the reference as the tree writes it and the sink resolves it onto
        /// `ExprVarBinder`.
        | VarRef of binder: 'id * at: ExprPoolId
        /// A source lambda's slot in the lambda id space, with the anchor its `LambdaKey`
        /// is the anchor OF. The anchor and not the key: this walk is shared with the
        /// overlay sink, which mints nodes spelled by no token at all — and drops the event
        /// unread, so nothing may depend on the anchor being a real one before the sink
        /// asks for it.
        | LambdaPooled of anchor: Anchor * at: ExprPoolId

    /// Where a pooling walk PUTS the rows it produces. The walk itself — which nodes
    /// exist, in what order, and which child edges they carry — is `poolExpr`/`poolPat`/
    /// `poolDecl` below and exists exactly once; a sink decides only where a row lands and
    /// how a binder id is assigned. `toPools` fills a fresh pool with one; an overlay
    /// builder appends to a stacked one with another.
    ///
    /// Generic in BOTH axes the walked tree differs from the columns in. The identity it
    /// names binders by: a source-shaped file names them by `NodeKey` and an already-pooled
    /// tree by `BinderId`. And the way it spells a POSITION: a source-shaped file carries
    /// `SyntaxToken`s, a pooled tree the stored index already. Both are poured into the
    /// same dense columns.
    type PoolSink<'tok, 'id> =
        {
            /// Called for every binder a walked node INTRODUCES (a `NamedSimple` pattern's
            /// binding, a `ForTo` loop variable, a declaration shape's pattern-less key
            /// slot), before the node's row is added, and answering with the dense id that
            /// binder took — which is what the node's own payload then names it by.
            /// Idempotent in the binder.
            ///
            /// The binder and NOTHING ELSE: how the source writes it is not a fact of the
            /// walk, which sees only where a node ended up, and the two differ once a body
            /// has been copied onto a call site. `fill` joins the two by asking the
            /// binder's own spelling record.
            InternBinder: BinderKeyG<'id> -> BinderId
            /// How the walked tree's spelling of a position becomes the stored anchor. It
            /// is the sink's and not the walk's because it is a property of the DESTINATION:
            /// a node of a frozen FILE must anchor on a real lexed token of that file
            /// (`Anchor.ofToken` faults otherwise), while an overlay's rows belong to no
            /// file and owe that nothing.
            Anchor: 'tok -> Anchor
            AddExpr: ExprRow -> ExprPoolId
            AddPat: PatRow -> PatPoolId
            AddDecl: DeclRow -> DeclPoolId
            /// Called once per pooled expr node that carries an id-keyed record, after
            /// its row is added. Not called at all for a node that carries none.
            OnExprPooled: PooledEvent<'id> -> unit
        }

    /// Pool a pattern subtree post-order: a node's children are pooled before the node
    /// itself, so every child id its row names already resolves.
    let rec poolPat (sink: PoolSink<'tok, 'id>) (p: TPatG<FrozenType, 'tok, 'id>) : PatPoolId =
        // Interned BEFORE the payload is built: the payload names this binder by the id
        // the intern hands back, so there is one identity rather than a key and an id.
        let binder = BinderKey.ofPat p |> ValueOption.map sink.InternBinder
        let kids = patChildren p |> Array.map (poolPat sink)

        sink.AddPat
            {
                Ty = TastWalk.patTy p
                Tok = sink.Anchor(TastWalk.patTok p)
                Children = kids
                Payload = patPayload binder p
            }

    /// Pool an expression subtree post-order (see `poolPat`), its owned sub-patterns
    /// included.
    let rec poolExpr (sink: PoolSink<'tok, 'id>) (e: TExprG<FrozenType, 'tok, 'id>) : ExprPoolId =
        // A `ForTo` binds its loop variable with no pattern node behind it, so the
        // intern cannot ride `poolPat`.
        let binder = BinderKey.ofExpr e |> ValueOption.map sink.InternBinder
        let exprKids = exprChildren e |> Array.map (poolExpr sink)
        let patKids = exprPatChildren e |> Array.map (poolPat sink)

        let row =
            {
                Ty = TastWalk.exprTy e
                Tok = sink.Anchor(TastWalk.exprTok e)
                Children = exprKids
                PatChildren = patKids
                VarBinder = ValueNone
                Payload = exprPayload sink.Anchor binder e
            }

        let id = sink.AddExpr row

        match e with
        | TExprG.Var(binding = binding) -> sink.OnExprPooled(PooledEvent.VarRef(binding, id))
        | TExprG.Lambda _ -> sink.OnExprPooled(PooledEvent.LambdaPooled(row.Tok, id))
        | _ -> ()

        id

    /// The residual payload of a frozen declaration node — its fields MINUS the child
    /// expr/pat roots. The exact inverse of `substituteDecl`. Exhaustive with no catch-all,
    /// so a new `TDecl` case fails to compile here.
    ///
    /// A `Type` decl is where the payload does real work: its member/preamble/ctor bodies
    /// are pooled through the sink and the declaration keeps their IDS in the slots that
    /// held the trees, and its pattern-less binder slots (`this`, member/ctor parameters,
    /// ctor locals) likewise keep the dense id the intern hands back (`PooledTypeDecl`).
    /// `TastConvert.typeDecl` supplies the traversal — the same one the `'ty` freeze runs —
    /// so the seven body slots and the seven key slots are enumerated in one place.
    ///
    /// The re-filing IS the interning: a slot is offered to the sink exactly where its id
    /// replaces it, so no slot can be rewritten without having been interned and none can
    /// be interned without being rewritten.
    let private declPayload (sink: PoolSink<'tok, 'id>) (d: TDeclG<FrozenType, 'tok, 'id>) : DeclPayload =
        match d with
        | TDeclG.Let(isInline = isInline; ty = ty) -> DeclPayload.Let {| IsInline = isInline; Ty = ty |}
        | TDeclG.Expression(ty = ty) -> DeclPayload.Expression ty
        | TDeclG.Type td ->
            DeclPayload.Type(
                TastConvert.typeDecl
                    {
                        Ty = id
                        Tok = sink.Anchor
                        Id = sink.InternBinder
                        Body = poolExpr sink
                    }
                    td
            )

    /// Pool a declaration, its expr/pat roots (see `poolPat`) and — for a `Type` decl —
    /// its member bodies, which the payload names by id rather than surfacing as children.
    let poolDecl (sink: PoolSink<'tok, 'id>) (d: TDeclG<FrozenType, 'tok, 'id>) : DeclPoolId =
        let struct (exprKids, patKids) =
            match d with
            | TDeclG.Let(binding = binding; value = value) ->
                struct ([| poolExpr sink value |], [| poolPat sink binding |])
            | TDeclG.Expression(expr = expr) -> struct ([| poolExpr sink expr |], [||])
            | TDeclG.Type _ -> struct ([||], [||])

        sink.AddDecl
            {
                ExprChildren = exprKids
                PatChildren = patKids
                Payload = declPayload sink d
            }

    /// The SOURCE arity of every module binding, READ OFF THE POOLED SPINE — the columns
    /// `toPools` has just filled, so a tuple group's pattern IS the lambda-spine node it
    /// was peeled from and not a re-pooled copy of it. Derived rather than carried: it is a
    /// projection of the tree, and the only way for a projection to disagree with what it
    /// projects is for the two to be computed apart.
    ///
    /// `ArgGroups.peel` is the walk; the readers below are the only column-domain part.
    ///
    /// Nothing here resolves a key: the binder a head pattern introduces is already the
    /// dense id its payload carries, so the table is keyed by the very id the pooled node
    /// bears rather than by a lookup that could miss.
    let private bindingValReprs (pools: FrozenPools) : DenseTable<BinderId, PooledValRepr> =
        let unLambda (ExprPoolId i) =
            match pools.ExprPayloads.[i] with
            | ExprPayload.Lambda ->
                ValueSome(struct (ChildColumn.item pools.ExprPatChildren i 0, ChildColumn.item pools.ExprChildren i 0))
            | _ -> ValueNone

        let facts (PatPoolId i) : ArgGroups.ParamPatFacts<BinderId> =
            {
                Shape = PatPayload.shape pools.PatPayloads.[i]
                Ty = pools.Types.[pools.PatTys.[i]]
                Binder =
                    match pools.PatPayloads.[i] with
                    | PatPayload.NamedSimple binder -> ValueSome binder
                    | _ -> ValueNone
                ConstValue =
                    match pools.PatPayloads.[i] with
                    | PatPayload.Const value -> ValueSome value
                    | _ -> ValueNone
            }

        [|
            for DeclPoolId d in pools.Roots do
                match pools.DeclPayloads.[d] with
                | DeclPayload.Let _ ->
                    let (PatPoolId head) = ChildColumn.item pools.DeclPatChildren d 0

                    match pools.PatPayloads.[head] with
                    // Only a simple binder has a side-table identity; a destructuring or
                    // wildcard head introduces none and needs none (see `BinderKey.ofPat`).
                    | PatPayload.NamedSimple binder ->
                        let groups, body =
                            ArgGroups.peel unLambda facts (ChildColumn.item pools.DeclExprChildren d 0)

                        let (ExprPoolId b) = body

                        yield
                            binder,
                            {
                                // A plain value has no lambda groups and records an
                                // empty-`Groups` entry, which the file→file signature
                                // projection reads as "not a function".
                                //
                                // `pools` already carries the filled column, so the width
                                // comes from the very one it ships with rather than from a
                                // second NodeKey-keyed copy of it.
                                Typars = FrozenPools.typarArity pools binder
                                Groups = groups
                                ResultTy = pools.Types.[pools.ExprTys.[b]]
                            }
                    | _ -> ()
                | DeclPayload.Expression _
                | DeclPayload.Type _ -> ()
        |]

    /// Pool a tree, assigning each reachable node a dense id and recording its child edges
    /// as ids. Generic in the identity the tree names binders by, because the fill runs in
    /// BOTH directions of the round-trip: a source-shaped file arrives naming them by
    /// `NodeKey`, a file drained back out of the columns by `BinderId`, and the columns are
    /// the same columns either way.
    ///
    /// Three things differ between the directions, and all are arguments. `spellingOf` is how
    /// the two binder columns are filled — the one fact neither the identity nor the walk
    /// carries: the producer's record for a source-shaped tree, the pool it came out of for
    /// a drained one. `anchor` is how the tree's spelling of a position becomes the stored
    /// one — a checked narrowing from a `SyntaxToken`, or nothing at all for a tree already
    /// in the stored form. `origin` is the file the stored anchors index, which likewise comes
    /// from the compilation on one side and from the drained pool on the other. See `toPools`
    /// and `rePool`.
    let private fill
        (origin: OriginFile)
        (spellingOf: BinderKeyG<'id> -> BinderSpelling)
        (anchor: 'tok -> Anchor)
        (file: TastFileG<FrozenType, 'tok, 'id>)
        : FrozenPools =
        // The unit's type tables, hash-consed AS THE COLUMNS ARE FILLED — there is no
        // separate interning pass, so what a node's `ty` column holds is a row of the very
        // table the pools ship with. A type is interned exactly where its column entry is
        // appended (the sink below), which is what keeps the two from being built apart.
        let typeTable = FrozenTypeTableBuilder()

        // The expression pool as parallel column builders (struct-of-arrays); all are
        // appended together per node so they stay index-aligned by `ExprPoolId`.
        let exprTys = ResizeArray<TypeId>()
        let exprToks = ResizeArray<Anchor>()
        let exprChildrenCol = ChildColumnBuilder<ExprPoolId>()
        let exprPatChildrenCol = ChildColumnBuilder<PatPoolId>()
        let exprPayloads = ResizeArray<ExprPayload>()

        // Each `Var`'s expr id + the binder it references, captured in pass 1 and resolved
        // to a `BinderId` in pass 2 — a `Var` may name a binder pooled after it (a forward /
        // mutually-recursive reference), so the enumeration must complete first.
        let varBindings = ResizeArray<struct (int * 'id)>()

        // The pattern pool as parallel column builders (struct-of-arrays), index-aligned by
        // `PatPoolId`.
        let patTys = ResizeArray<TypeId>()
        let patToks = ResizeArray<Anchor>()
        let patChildrenCol = ChildColumnBuilder<PatPoolId>()
        let patPayloads = ResizeArray<PatPayload>()

        // The declaration pool as parallel column builders, index-aligned by `DeclPoolId`.
        let declExprChildrenCol = ChildColumnBuilder<ExprPoolId>()
        let declPatChildrenCol = ChildColumnBuilder<PatPoolId>()
        let declPayloads = ResizeArray<DeclPayload>()

        // The binder pool: each distinct definition site the walk reaches takes a dense
        // `BinderId` on first encounter, and its two columns record what the slot alone
        // cannot say — how the source spells it, and where. The introducing sites are
        // enumerated by the `BinderKey` projections (`ofPat` / `siteOfExpr`, plus a
        // declaration shape's key slots, which are typed by one) as the trees are walked,
        // so nothing re-derives which nodes bind — and the side tables remapped below were
        // filed through those same projections.
        //
        // The WALKED tree's identity is the interning KEY and stops here: nothing of it
        // reaches the columns, so a source-shaped file's `NodeKey`s — a grammar-versioned
        // node kind included — cannot be read back off a frozen file. It is what makes the
        // intern idempotent while the walk still speaks whatever identity it arrived in.
        //
        // The enumeration spans the whole FILE, because a side table may key on a binder in
        // any of its trees — and every tree the file bears is now pooled, so ONE walk covers
        // them all. Correspondingly a `BinderId` need not have a pooled pattern node behind
        // it (a `this` slot has none anywhere) — the binder space is dense and independent,
        // inverted by position.
        let binderNames = ResizeArray<string>()
        let binderToks = ResizeArray<Anchor>()
        let binderIds = System.Collections.Generic.Dictionary<'id, BinderId>()

        // The lambda id space: a source lambda's dense id IS its `ExprPoolId` (positional —
        // every `Lambda` expr is already in the `Expr*` columns), paired with the key its
        // `FunVerdicts` entry is filed under — the node's own anchor.
        //
        // A LIST, not a key→id map, because the key is one-to-MANY over this space and a map
        // could only keep one of the nodes. Two lambdas share a key whenever they share a
        // source token, which is routine in two ways: the published TEMPLATE is a second tree
        // over the same source as the emitted function it was stashed from, and a resolved
        // specialization entry is a third — an entry keeps the positions its template was
        // written at, so its lambdas land on the template's own tokens. The verdict belongs to
        // ALL of them; a map would have silently given it to whichever was pooled last, and
        // left every other copy to emit as an ordinary heap closure.
        let lambdaSlots = ResizeArray<struct (ExprPoolId * LambdaKey)>()

        let internBinder (binder: BinderKeyG<'id>) : BinderId =
            let k = BinderKey.identity binder

            match binderIds.TryGetValue k with
            | true, id -> id
            | false, _ ->
                let id = BinderId binderNames.Count
                binderIds.Add(k, id)
                // Both columns out of the ONE record, so the name and the place it is
                // written at cannot come from different tokens.
                let sp = spellingOf binder
                binderNames.Add sp.Name
                binderToks.Add sp.At
                id

        // The sink: rows land at the end of the column builders, so a node's id is the
        // count at the moment it is added. `ExprRow.VarBinder` is dropped here — the `Var`
        // reference edge cannot resolve until the binder enumeration is complete, so it is
        // recorded as pending and filled by the second pass below.
        //
        // `Anchor` is where a node of THIS FILE is held to anchoring on a real lexed token:
        // `toPools` supplies the checked narrowing, `rePool` a tree already in the stored
        // form. It is a property of the DESTINATION and so of the sink, not of the shared
        // walk — `TastPoolBuilder`'s overlay keeps its own rows, never appends to these
        // columns, and so owes it nothing.
        let sink: PoolSink<'tok, 'id> =
            {
                InternBinder = internBinder
                Anchor = anchor
                AddExpr =
                    fun row ->
                        let id = exprPayloads.Count
                        exprTys.Add(typeTable.Intern row.Ty)
                        exprToks.Add row.Tok
                        exprChildrenCol.Add row.Children
                        exprPatChildrenCol.Add row.PatChildren
                        exprPayloads.Add row.Payload
                        ExprPoolId id
                AddPat =
                    fun row ->
                        let id = patPayloads.Count
                        patTys.Add(typeTable.Intern row.Ty)
                        patToks.Add row.Tok
                        patChildrenCol.Add row.Children
                        patPayloads.Add row.Payload
                        PatPoolId id
                AddDecl =
                    fun row ->
                        let id = declPayloads.Count
                        declExprChildrenCol.Add row.ExprChildren
                        declPatChildrenCol.Add row.PatChildren
                        declPayloads.Add row.Payload
                        DeclPoolId id
                // A `Var`'s binder reference resolves in pass 2 (see `varBindings`); a
                // lambda's positional identity is its slot, stamped so `FunVerdicts`
                // (lambda-expression-keyed) resolves onto it.
                OnExprPooled =
                    fun ev ->
                        match ev with
                        | PooledEvent.VarRef(binder, ExprPoolId id) -> varBindings.Add(struct (id, binder))
                        | PooledEvent.LambdaPooled(anchor, id) -> lambdaSlots.Add(struct (id, LambdaKey anchor))
            }

        let roots = file.Decls |> EqArray.toArray |> Array.map (poolDecl sink)

        // The inline vocabulary, pooled as its OWN roots. A template is a different tree
        // from the emitted function of the same name — the unwalked snapshot `Freeze`
        // published — so the two are pooled independently and neither is derived from the
        // other. Both are ordinary pooled decls; the binder enumeration therefore reaches a
        // template's binders as part of the normal walk, with no intern-only side pass.
        let inlineTemplates =
            file.InlineBodies
            |> EqArray.toArray
            |> Array.map (fun iv ->
                {
                    Key = iv.Key
                    Decl = poolDecl sink iv.Body.Decl
                    ParamAttrs = iv.Body.ParamAttrs
                }
            )

        // The specialization table, pooled as its own roots for the same reason. Pooled in
        // SLOT ORDER, which is what keeps the `SpecializationId`s the tree already carries
        // valid against the array — the ids name positions in this array, not in the pool
        // the decls land in.
        let specializations =
            file.Specializations
            |> EqArray.toArray
            |> Array.map (fun s ->
                {
                    Key = s.Key
                    Origin = s.Origin
                    Decl = poolDecl sink s.Decl
                }
            )

        // THE lookup: the dense id the enumeration above interned a key under. Written
        // once because both faults below ARE this lookup missing; what differs is only
        // what a miss means, and that is what each of them says.
        let internedBinderId (k: 'id) : BinderId voption =
            match binderIds.TryGetValue k with
            | true, id -> ValueSome id
            | false, _ -> ValueNone

        // Resolve a REFERENCE to the binder it names — a `Var`'s binding edge, or a row
        // derived off the columns. A miss means the referent was introduced by no
        // definition site the walk covers: an incomplete binder enumeration, which is
        // exactly the failure the id-resolution gate exists to surface. A reference is
        // written by whoever resolved the name, so it arrives as a bare `NodeKey`.
        let binderIdOfRef (referent: string) (k: 'id) : BinderId =
            match internedBinderId k with
            | ValueSome id -> id
            | ValueNone ->
                failwithf "TastPools.toPools: %s key %O references a binder no definition site introduced" referent k

        // SIDE-TABLE REACHABILITY — and THE POLICY every binder-keyed table is held to.
        //
        // `BinderKey`'s private constructor already makes a key naming a NON-binder
        // unwritable. What is left is the other half: a key naming a real binder whose
        // declaration was dropped before the freeze. The entry then addresses nothing, and
        // its reader gets a silent miss where a fact was recorded.
        //
        // The policy is FAULT BY DEFAULT. A new binder-keyed table inherits this check by
        // being remapped at all, and the fix for a fault is at the PRODUCER: prune the
        // entry where the declaration is pruned, the way `Elaborate.translateModuleElem`
        // records no binder for a binding it elides. Dropping the surplus key here instead
        // is permitted only where the producer can argue, AT ITSELF, that its surplus is
        // inert — `Regions.closureReprSnapshot` is the one table that does, and it filters
        // in its own body, which is where such an argument has to live to stay checkable
        // against the code it is about.
        let binderIdOf (referent: string) (b: BinderKeyG<'id>) : BinderId =
            let k = BinderKey.identity b

            match internedBinderId k with
            | ValueSome id -> id
            | ValueNone ->
                failwithf
                    "TastPools.toPools: %s entry %O names a binder no declaration in the frozen file introduces — prune the entry where its declaration is pruned"
                    referent
                    k

        // `FunVerdicts` onto the lambda id space, driven from the ID side: every pooled
        // lambda is offered the key it was stamped with, and a lambda whose key carries a
        // verdict takes a row. Driving it from the KEY side instead is what a `Map` remap
        // would do, and it can only pick ONE lambda per key — see `lambdaSlots` for why
        // that is the wrong arity.
        //
        // The reachability check the key side did own still holds, and is made explicitly:
        // a verdict key that stamped no pooled lambda is a table entry addressing nothing,
        // the same defect `binderIdOf` faults on for the binder-keyed tables.
        let funVerdicts =
            let matched = System.Collections.Generic.HashSet<LambdaKey>()

            let rows =
                [|
                    for struct (id, k) in lambdaSlots do
                        match Map.tryFind k file.FunVerdicts with
                        | Some v ->
                            matched.Add k |> ignore
                            yield id, v
                        | None -> ()
                |]

            for KeyValue(k, _) in file.FunVerdicts do
                if not (matched.Contains k) then
                    failwithf "TastPools.toPools: FunVerdicts key %O names no pooled lambda" k

            rows

        // Second pass: now the enumeration is complete, route each `Var`'s reference edge
        // to its binder's dense id — the sparse `ExprVarBinder` column (`ValueNone` at
        // every non-`Var` slot).
        let exprVarBinder: BinderId voption[] = Array.create exprPayloads.Count ValueNone

        for (struct (id, key)) in varBindings do
            exprVarBinder.[id] <- ValueSome(binderIdOfRef "Var" key)

        // One generic remap over the BINDER-keyed side tables, parameterized by the key
        // resolver so an unresolvable key says which table holds it (each call site applies
        // the resolver to its table's name first). `FunVerdicts` does not come through here:
        // its key space is one-to-many over the ids it maps to, so it is built from the id
        // side above.
        let remapSideTable (resolve: 'k -> 'dense) (m: Map<'k, 'v>) : ('dense * 'v)[] =
            m |> Map.toArray |> Array.map (fun (k, v) -> resolve k, v)

        // A per-binder SCALAR goes into a COLUMN instead (`BinderColumn`): the producer's
        // key is resolved here — through the same `binderIdOf`, so the reachability policy
        // above applies to it unchanged — and then DROPPED, the fact landing at the binder's
        // own slot. What that removes is the stored key, and with it the possibility of a
        // stale entry surviving the freeze at all.
        let binderColumn (referent: string) (m: Map<BinderKeyG<'id>, 'v>) : BinderColumn<'v> =
            let col = Array.create binderNames.Count ValueNone

            for KeyValue(k, v) in m do
                let (BinderId i) = binderIdOf referent k
                col.[i] <- ValueSome v

            col

        // Every column is snapshotted here and NOTHING below appends: the derived table
        // that follows only READS the pools, so the record's field order carries no
        // correctness weight.
        let pools =
            {
                Origin = origin
                // Snapshotted AFTER every walk above, so the tables hold every type the
                // columns name. Nothing below interns.
                Types = FrozenTypeTable.OfRows typeTable.Rows
                ExprTys = exprTys.ToArray()
                ExprToks = exprToks.ToArray()
                ExprChildren = exprChildrenCol.ToColumn()
                ExprPatChildren = exprPatChildrenCol.ToColumn()
                ExprVarBinder = exprVarBinder
                ExprPayloads = exprPayloads.ToArray()
                PatTys = patTys.ToArray()
                PatToks = patToks.ToArray()
                PatChildren = patChildrenCol.ToColumn()
                PatPayloads = patPayloads.ToArray()
                DeclExprChildren = declExprChildrenCol.ToColumn()
                DeclPatChildren = declPatChildrenCol.ToColumn()
                DeclPayloads = declPayloads.ToArray()
                Roots = roots
                InlineTemplates = inlineTemplates
                Specializations = specializations
                BinderNames = binderNames.ToArray()
                BinderToks = binderToks.ToArray()
                Residue =
                    {
                        Diagnostics = file.Diagnostics
                        IntrinsicReprKeys = file.IntrinsicReprKeys
                        Accessibility = file.Accessibility
                    }
                ModuleMembers = remapSideTable (binderIdOf "ModuleMembers") file.ModuleMembers
                ClosureReprs = remapSideTable (binderIdOf "ClosureReprs") file.ClosureReprs
                FunVerdicts = funVerdicts
                GenericFnSchemes = remapSideTable (binderIdOf "GenericFnSchemes") file.GenericFnSchemes
                // Derived below, off the pools themselves.
                BindingValReprs = [||]
                BindingTyparArities = binderColumn "BindingTyparArities" file.BindingTyparArities
            }

        { pools with
            BindingValReprs = bindingValReprs pools
        }

    /// Pool a source-shaped frozen file — the freeze's last step. `spellings` is the
    /// producers' record of how the source writes each binder (`PassContext.BinderSpellings`),
    /// which is the only thing here that knows: the walk sees where a node ENDED UP, and a
    /// spliced body ends up on its call site.
    ///
    /// This is where a binder's name is committed, so the text a backend emits is a column
    /// value and not a re-scan of a source file the backend may no longer hold (a unit
    /// emitted from its cache blob has none). The text is verbatim; mangling it for a target
    /// dialect's reserved words belongs to the backend.
    ///
    /// It is also where the file's tokens become indices, and `Anchor.ofToken` is what makes
    /// "a frozen node anchors on a real token of its own file" a fact of the CONVERSION
    /// rather than an assertion some later reader might not make. `origin` is the file those
    /// indices are taken against — the same identity the anchored tokens came out of.
    let toPools (origin: OriginFile) (spellings: BinderKey -> BinderSpelling) (file: Frozen.TastFile) : FrozenPools =
        fill origin spellings Anchor.ofToken file

    /// Pool a tree DRAINED from `pools` (`TastUnpool.ofPools`) — the fill direction of the
    /// round-trip, which is what makes `ofPools` checkable at all: the columns are
    /// tree-sufficient exactly when re-pooling their own drain reproduces them.
    ///
    /// No producer is in reach on this side, so the spelling — and the file the drained
    /// anchors index — come back off the pool the tree was drained out of, where both were
    /// written down. The node anchors need no narrowing at all: a drained tree already
    /// carries the stored one.
    let rePool (pools: FrozenPools) (file: Pooled.TastFile) : FrozenPools =
        let spellingOf (b: BinderKeyG<BinderId>) =
            let (BinderId i) = BinderKey.identity b

            {
                Name = pools.BinderNames.[i]
                At = pools.BinderToks.[i]
            }

        fill pools.Origin spellingOf id file
