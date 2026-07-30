namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore

// The pre-freeze inline-expansion pass. Runs
// between `Elaborate.elaborate` and `Elaborate.freezeTypars`, on the still
// `TyVar`-carrying `TExpr` tree, where `zonk` / union-find are native. It
// relocates module-level `let inline` expansion out of codegen
// (`EmitLower.lowerWith`'s inline branches): a saturated use of a local
// `let inline` — or of a cross-unit `val inline` whose body the provider serves on the
// resolved entry (`ExternalSymbol.InlineBody` / `ExternalMember.InlineBody`) — is
// expanded + beta-reduced + static-opt resolved here, so the frozen module decls
// reaching codegen carry no inline call heads and no `StaticOptimization` nodes.
//
// A cross-unit body arrives FROZEN and is THAWED here, minting this
// unit's own inference cells. That thaw is the single immutable→mutable transition on
// the provider seam: nothing below it can `UnionFind.union` into a producer's cells,
// because it never holds one.
//
// WHERE a cross-unit body ends up is the pass's other decision, and it is made once, at that
// same thaw. A body whose provider RETAINED the producer file becomes an entry of the file's
// resolved-specialization table, keeping the positions it was written at
// (`Inline.thawBodyAtOrigin`), and the call site gets a `TExpr.InlineCall` edge naming it —
// so a body called from N sites is one entry and N edges, each with its own provenance. A
// body served without a retained file has no anchor domain an entry could name, so it is
// MOVED onto the call site (`Inline.thawBody`) and physically spliced, as are same-unit
// templates (whose positions are already this file's). The table is a DAG: an entry's own
// body carries edges.
//
// Nothing downstream of this pass expands the table yet, so `run` flattens it back into the
// decls before returning (`Inline.flatten`) and hands the table out beside them.
//
// Scope: EVERY module-level decl (`TDecl.Let` — `inline` or not — and
// `TDecl.Expression`) AND every expression a `TDecl.Type` carries (member
// bodies, `static let` inits, secondary-ctor `let`s + chain args, base-ctor
// args). An `inline` binding is walked because it is also EMITTED as an ordinary
// module function, and codegen's input invariant (no inline call heads, no
// `StaticOptimization`, no `External` used as a value) has to hold of that function
// like any other. The walked form is therefore the EMITTED one and NOT the published
// template: `Elaborate.run` snapshots the unwalked body into `ctx.InlineTemplates`
// first, because a template's static-opt clauses and trait calls must resolve against
// a CALL SITE's operand types, not against the nothing that is ground at its
// definition.
//
// It also owns the compiler's ONE eta-reification (`etaReify`): an `External` of
// function type used as a VALUE becomes a closure here, whether or not it has an
// inline body. Codegen has no eta of its own. The pre-freeze position is forced —
// an inline-bodied external (`List.fold (+) 0 xs`) reified post-freeze mints its
// call `App` past the last point the body can be spliced into it, and codegen's
// lowering never walks member bodies at all.
//
// The inline-first soundness condition (beta-reduction half): a lambda
// argument bound to an inline parameter and fully applied inside the body is
// eliminated — its closure never exists (`classifyApplication` +
// `nonInlinableLambdaParams` + the `lambdaEnv` splice). A lambda that is stored
// or partially applied survives as a real closure, exactly as before. The
// byref-like-capture half (reject / ref-struct closures for a SURVIVING closure
// that holds a `Span`/`ref struct`) is deferred — see the TODO in
// `classifyApplication`; it needs a byref-like predicate that does not exist yet.

module InlineExpansion =

    /// Replace every `Var k` in `body` with `replacement`. Used for a parameter
    /// the declaration marked `[<CallAtMostOnce>]` — `Elaborate` already validated
    /// that `k` occurs at most once and not under a lambda or loop, so this
    /// substitutes 0-or-1 times (the argument is then evaluated at most once, on
    /// demand) and cannot capture (`replacement` is the call-site argument, whose
    /// free vars are disjoint from the freshly-minted inline-body binders). This is
    /// what makes the library `&&`/`||` (`if e1 then e2 else false`, `e2` marked)
    /// short-circuit without the operator being known to the compiler.
    let private substituteVar (k: NodeKey) (replacement: TExpr) (body: TExpr) : TExpr =
        let m =
            { TastWalk.identityMapper with
                OverrideExpr =
                    fun _ e ->
                        match e with
                        | TExpr.Var(vk, _, _) when vk = k -> ValueSome replacement
                        | _ -> ValueNone
            }

        TastWalk.mapExpr m body

    /// The number of leading `fun x -> …` abstractions with a simple-named
    /// parameter — the arity at which a lambda argument is *fully applied*. Only
    /// `NamedSimple` binders count: a destructuring lambda parameter (`fun (a, b)
    /// -> …`) is left at the abstraction below it, so a use that tries to
    /// saturate past it never matches the arity and the closure is kept (it would
    /// otherwise trip `betaReduce`'s destructuring guard).
    let rec private lambdaArity (e: TExpr) : int =
        match e with
        | TExpr.Lambda(TPat.NamedSimple _, body, _, _) -> 1 + lambdaArity body
        | _ -> 0

    /// Rewrite what a curried lambda COMPUTES, leaving its abstractions in place. The lambda
    /// spine has to survive the rewrite because `Inline.betaReduce` matches on it, and its
    /// binders are consumed against arguments belonging to whatever body the lambda is spliced
    /// into — so a rewrite about the lambda's own origin applies below them, not around them.
    let rec private underLambdas (f: TExpr -> TExpr) (e: TExpr) : TExpr =
        match e with
        | TExpr.Lambda(param, body, ty, tok) -> TExpr.Lambda(param, underLambdas f body, ty, tok)
        | _ -> f e

    /// Of the lambda-valued inline parameters in `candidates` (key → its bound
    /// lambda), the ones that are NOT eligible for inline-first elimination —
    /// i.e. a parameter with at least one use that is not a *fully saturated*
    /// application head. A saturated head (`f a b` where `f`'s lambda has arity 2)
    /// beta-reduces away and the closure vanishes; any other use — a bare `Var`
    /// (the lambda is stored or passed onward), a partial application, or an
    /// over-application — forces the parameter to survive as a real closure.
    /// The inline-first soundness condition: only a
    /// fully-applied [<InlineIfLambda>]-style parameter is guaranteed to vanish.
    ///
    /// The walk mirrors the expansion walker's `App` rule exactly: collect the
    /// WHOLE spine at each `App` and never let `TastWalk`'s default recursion
    /// descend into a sub-`App` (which would mis-measure a partial spine as the
    /// arity), recursing only into the spine's head (when not a candidate) and
    /// its arguments.
    let private nonInlinableLambdaParams (candidates: Dictionary<NodeKey, TExpr>) (core: TExpr) : HashSet<NodeKey> =
        let bad = HashSet<NodeKey>()

        let it =
            { TastWalk.identityIter with
                VisitExpr =
                    fun iter e ->
                        match e with
                        | TExpr.App _ ->
                            let head, args = TastWalk.collectSpine [] e

                            (match head with
                             | TExpr.Var(k, _, _) when candidates.ContainsKey k ->
                                 if List.length args <> lambdaArity candidates.[k] then
                                     bad.Add k |> ignore
                             | _ -> TastWalk.iterExpr iter head)

                            for (a, _, _) in args do
                                TastWalk.iterExpr iter a

                            false
                        | TExpr.Var(k, _, _) when candidates.ContainsKey k ->
                            // A bare reference: the lambda is stored / passed on,
                            // so it cannot be inlined away.
                            bad.Add k |> ignore
                            false
                        | _ -> true
            }

        TastWalk.iterExpr it core
        bad

    /// Arrow-spine views of a `SemType`, the two things this pass asks of a curried
    /// function type: which domains it has, and what it returns after `n` of them are
    /// applied. Each step zonks — pre-freeze a `TyFun` is often reachable only through
    /// a union-find Link, so a raw match would see a `TyVar` and report arity 0. A
    /// spine shorter than `n` is not an error here: both callers cap `n` at a count
    /// this very module measured, and `deriveInlineTypeArgs` is deliberately tolerant
    /// of a declared type it cannot fully peel.
    ///
    /// The `FrozenType` twin is `TastLower.peelArrows` — deliberately separate: that
    /// side has no union-find to chase.
    [<RequireQualifiedAccess>]
    module private Arrows =

        /// The number of `->` in the spine.
        let rec count (store: TypeStore) (t: SemType) : int =
            match Unification.zonk store t with
            | TyFun(_, r) -> 1 + count store r
            | _ -> 0

        /// The first `n` domain types, left to right.
        let rec domains (store: TypeStore) (n: int) (t: SemType) : SemType list =
            if n <= 0 then
                []
            else
                match Unification.zonk store t with
                | TyFun(a, b) -> a :: domains store (n - 1) b
                | _ -> []

        /// What the spine returns once `n` arguments have been applied.
        let rec resultAfter (store: TypeStore) (n: int) (t: SemType) : SemType =
            let t = Unification.zonk store t

            if n <= 0 then
                t
            else
                match t with
                | TyFun(_, b) -> resultAfter store (n - 1) b
                | _ -> t

    /// A (zonked) `SemType` with no free `TyVar` anywhere — fully monomorphic. The
    /// `SemType` sibling of `FrozenTypeBridge.ftIsGround` (this one zonks; the frozen
    /// one has no vars to zonk). Used to rank competing candidates for one typar in
    /// `deriveInlineTypeArgs` — a ground candidate beats an abstract one.
    let rec private isGroundType (store: TypeStore) (t: SemType) : bool =
        match Unification.zonk store t with
        | TyVar _
        | TyUnknown _
        | TyTypar _ -> false
        | t -> SemType.forallChildren (isGroundType store) t

    /// Recover an inline binding's type arguments at a call site by matching its
    /// declared parameter (and return) types — carrying the quantified typars —
    /// against the actual spine-arg types. Tolerant: a typar the params don't pin is
    /// left as its own `TyVar`, which selects no `when ^T : Type` clause and so falls
    /// to the body's base. Returned in `Inline.quantifiedTypars` order. A verbatim port of
    /// `EmitLower.deriveInlineTypeArgs` (`zonk` → `Unification.zonk`,
    /// `typeOfExpr` → `TastWalk.exprTy`).
    let private deriveInlineTypeArgs
        (store: TypeStore)
        (declTy: SemType)
        (spineArgs: (TExpr * SemType * SyntaxToken) list)
        : SemType[] =
        let typars = Inline.quantifiedTypars store declTy

        if typars.Length = 0 then
            [||]
        else
            let roots = typars |> Array.map (UnionFind.find store)
            let result = Array.create roots.Length ValueNone

            let rec go (defT: SemType) (actT: SemType) =
                match Unification.zonk store defT, Unification.zonk store actT with
                | TyVar tv, act ->
                    let r = UnionFind.find store tv

                    match roots |> Array.tryFindIndex (fun x -> x = r) with
                    | Some i ->
                        match result.[i] with
                        | ValueNone -> result.[i] <- ValueSome act
                        // A later position mapping to the SAME typar can upgrade a
                        // non-ground candidate to a ground one — so a still-abstract
                        // operand cannot starve a static-opt clause that a concrete
                        // SIBLING position would have selected. This only arises where
                        // one typar spans several positions: the HOMOGENEOUS operators
                        // (`(=) : ^T -> ^T -> bool`, `(<)`, `hash`) map both operands to
                        // one slot. The arithmetic family does not — its three typars get
                        // three independent slots, which is exactly what lets a
                        // heterogeneous operand pair keep its distinct types.
                        // Keeping the first ground match is intentional: a genuinely
                        // generic `let f a b = a = b` never sees a ground candidate, so
                        // the typar stays abstract and the body falls to its base.
                        | ValueSome prev when not (isGroundType store prev) && isGroundType store act ->
                            result.[i] <- ValueSome act
                        | ValueSome _ -> ()
                    | None -> ()
                | TyFun(a1, r1), TyFun(a2, r2) ->
                    go a1 a2
                    go r1 r2
                // A generic intrinsic carries its args structurally — notably the
                // array `'T[]` = `TyConst("[]", ['T])`, whose element typar is only
                // reachable by descending here (the `GetArray`/`GetArrayLength`
                // inline bodies pin `'T` solely through their `'T[]` parameter). The
                // codegen twin `EmitLower.matchInstantiation` has the same arm.
                | TyConst(_, xs), TyConst(_, ys) when xs.Length = ys.Length ->
                    for i in 0 .. xs.Length - 1 do
                        go xs.[i] ys.[i]
                | TyTuple xs, TyTuple ys when xs.Length = ys.Length ->
                    for i in 0 .. xs.Length - 1 do
                        go xs.[i] ys.[i]
                | TyRecord(_, xs), TyRecord(_, ys) when xs.Length = ys.Length ->
                    for i in 0 .. xs.Length - 1 do
                        go xs.[i] ys.[i]
                | TyUnion(_, xs), TyUnion(_, ys) when xs.Length = ys.Length ->
                    for i in 0 .. xs.Length - 1 do
                        go xs.[i] ys.[i]
                | TyClass(_, xs), TyClass(_, ys) when xs.Length = ys.Length ->
                    for i in 0 .. xs.Length - 1 do
                        go xs.[i] ys.[i]
                | _ -> ()

            let rec pairGo ps acts =
                match ps, acts with
                | p :: ps', a :: acts' ->
                    go p a
                    pairGo ps' acts'
                | _ -> ()

            let nArgs = List.length spineArgs

            pairGo (Arrows.domains store nArgs declTy) [ for (a, _, _) in spineArgs -> TastWalk.exprTy a ]

            // Pair the result position too: `failwith`'s only typar `'T` sits in
            // the *return* (`string -> 'T`), so the param walk leaves it unbound.
            // The last spine arg's recorded type is the whole application's result
            // (`collectSpine` pairs each arg with its `App` node's result), so
            // unifying it against `declTy`'s return position grounds the result
            // typars.
            if nArgs > 0 then
                let declRetTy = Arrows.resultAfter store nArgs declTy
                let _, actualRetTy, _ = spineArgs |> List.last
                go declRetTy actualRetTy

            Array.mapi
                (fun i v ->
                    match v with
                    | ValueSome t -> t
                    | ValueNone -> TyVar roots.[i].Id
                )
                result

    /// The verdict for a trait call the expansion could not dispatch. An operator is named
    /// as the user WROTE it (`+`), never by the member it compiled to (`op_Addition`) —
    /// `OperatorNames.sourceSymbol` inverts the lexer's own table, so the spelling cannot
    /// drift from the name. A member outside that table is not an operator at all (a
    /// user-written `(^T: (member GetAwaiter: …) x)`), and the verdict says so.
    let private unsupportedTrait (store: TypeStore) (u: Inline.UnresolvedTrait) : Kind =
        let receiver = shown store u.Receiver

        match OperatorNames.sourceSymbol u.MemberName with
        | ValueSome symbol -> Kind.TraitNotSupported(receiver, MemberNoun.Operator, symbol)
        | ValueNone -> Kind.TraitNotSupported(receiver, MemberNoun.Member, u.MemberName)

    /// What becomes of ONE curried parameter of an inline body at ONE call site. The three
    /// FUSED dispositions all put call-site material INSIDE the body, which is what makes a
    /// reduction site-specific; a `Survive` parameter leaves the body closed over it and is
    /// the only kind that can be a parameter of a shared specialization entry.
    ///
    /// Decided BEFORE the body is walked (every input is the peel and the declared attributes,
    /// none of them the walk's result), which is what lets a caller reserve a table slot ahead
    /// of the recursion that fills it.
    [<RequireQualifiedAccess>]
    type private Disposition =
        /// Bound to a bare `External` function VALUE — pure and capture-free, so it is
        /// substituted into the body and its `let` disappears.
        | FuseExternalValue
        /// A lambda argument every use of which is a fully saturated application head: spliced
        /// at each use, so its closure never exists.
        | FuseLambda
        /// Declared `[<CallAtMostOnce>]`: substituted at its single validated use rather than
        /// bound eagerly, so the argument is evaluated at most once and on demand.
        | FuseAtMostOnce
        /// Bound by an ordinary `let` (a physical splice) or abstracted by the entry (an edge).
        | Survive

    /// Where a reduction's body will LIVE — which is the whole of what decides whether the
    /// call-site material it FUSES in changes anchor domain, and so whether that material
    /// needs a `TExpr.CallerExpr` around it.
    ///
    /// Decided from the served body alone (does the provider retain a producer file?), before
    /// any parameter is classified, so one reduction cannot mark half its fusions.
    [<RequireQualifiedAccess>]
    type private Placement =
        /// Abstracted into a specialization entry, whose nodes stay anchored in the producer
        /// file its `OriginFile` names. Descending the edge PUSHES that file, so fused
        /// call-site material is one frame out and pops back.
        | Outlined
        /// Moved onto the call site, so the body and the material fused into it are already
        /// one domain. Nothing pushed a frame, so nothing may pop one.
        | Spliced

    [<RequireQualifiedAccess>]
    module private Placement =

        /// A fused call-site argument as it must appear INSIDE the body it is fused into.
        /// Applied UNCONDITIONALLY at an outlined site, a trivial argument included: a uniform
        /// invariant is checkable where one that skips `Var`s and constants is not.
        let fuse (placement: Placement) (arg: TExpr) : TExpr =
            match placement with
            | Placement.Outlined -> Inline.callerExpr arg
            | Placement.Spliced -> arg

    /// One curried parameter of an inline body paired with the argument the call site supplies
    /// for it. A record because `AppTok` and `PatTok` are two DIFFERENT positions that are
    /// equal in the common case — the application node the `let` lowers, and the template's own
    /// parameter binder — and a tuple would let them be swapped silently.
    type private InlineParam =
        {
            Key: NodeKey
            Ty: SemType
            /// The call-site argument. Unwalked in a `Peeled`; walked in a `Reduced`.
            Arg: TExpr
            /// The application node that supplied `Arg`.
            AppTok: SyntaxToken
            /// The template's own binder for this parameter.
            PatTok: SyntaxToken
            Disposition: Disposition
        }

    /// The peel of a resolved inline body against one call site's spine, with each parameter's
    /// disposition already decided. Everything about the reduction that is knowable before the
    /// body is walked.
    type private Peeled =
        {
            /// Outermost curried parameter first, so an index into this list IS the curried
            /// position the declared `ParamAttrs` are aligned to.
            Params: InlineParam list
            /// The body under the peeled lambdas, with the `FuseExternalValue` substitutions
            /// already applied (they are what the lambda-parameter classification reads).
            Core: TExpr
        }

    /// The reduction of one call site once its body has been walked: the fused body, and the
    /// parameters that survived it with their walked arguments.
    type private Reduced =
        {
            Body: TExpr
            Survivors: InlineParam list
        }

    /// A cross-unit inline body as this pass consumes it: the template realised in this unit's
    /// `SemType` domain, the identity it was reached by (which is the specialization table's
    /// template identity), its declared parameter attributes, and — when the provider retained
    /// the producer file — the anchor domain its nodes keep.
    ///
    /// `Origin = ValueNone` is not a missing field but the OTHER reading of a served body: no
    /// retained producer file means no domain an entry could name, so such a body has already
    /// been moved onto the call site and can only be spliced.
    type private ServedBody =
        {
            Key: SymbolKey
            Decl: TDecl
            ParamAttrs: ParamAttrs[]
            Origin: OriginFile voption
        }

    /// The identity two call sites must agree on to name ONE specialization entry.
    ///
    /// `Key` is the entry's own stored key (template identity + the type arguments it was
    /// resolved at). `Arity` is the number of parameters the site actually APPLIED, which the
    /// grounding does not pin: a partial application of the same template at the same types
    /// leaves a different body behind (the unapplied lambdas stay in it), and sharing the
    /// saturated site's entry with it would hand one body two arities.
    type private Grounding = { Key: SpecializationKey; Arity: int }

    [<RequireQualifiedAccess>]
    module private Peeled =

        /// No call-site material was fused into the body, so the body is CLOSED over its
        /// parameters and nothing in it belongs to this site. That is exactly the condition
        /// under which two sites may share one specialization entry — and the condition under
        /// which an entry's nodes all come from the file its `OriginFile` names.
        let isClosed (p: Peeled) : bool =
            p.Params |> List.forall (fun x -> x.Disposition = Disposition.Survive)

    /// What one run of the pass produced.
    ///
    /// `Decls` are FLATTENED — every edge the walk minted has been spliced back — because
    /// nothing downstream of this pass expands the table yet. `Specializations` is the table
    /// those edges named, kept because it is the pass's real product: the flattening is a
    /// consumer of it, not the thing that replaces it.
    type Expanded =
        {
            Decls: (TDecl * (TyVarId * SemType) list) list
            /// Slot order — a `SpecializationId` an entry (or a decl before flattening) carries
            /// indexes THIS array.
            Specializations: TSpecialization[]
        }

    /// Expand the module-level inlines in one decl-list (the elaborated,
    /// `TyVar`-carrying decls paired with their freeze envs). The cross-unit inline-body
    /// channel is `provider` itself — the body rides the resolved entry, reached by the
    /// key the use-site node carries; a front-end-only provider serves none and every
    /// lookup returns `ValueNone`, so the walk is an identity rebuild — which
    /// `Elaborate.freezeTypars` does to every decl immediately after regardless, so there
    /// is no node-identity to preserve by skipping it.
    let run (ctx: PassContext) (decls: (TDecl * (TyVarId * SemType) list) list) : Expanded =

        let provider = ctx.Provider

        // Parameter attributes for a *local* module-level inline, by binder key
        // (the cross-package twin rides `InlineBody.ParamAttrs`). Empty when the
        // inline declared no recognised parameter attribute.
        let localParamAttrs (k: NodeKey) : ParamAttrs[] =
            match ctx.InlineParamAttrs.TryGetValue k with
            | true, a -> a
            | _ -> [||]

        // Local module-level `let inline` bindings, keyed by binder NodeKey — the
        // same map codegen's `lowerWith` used to build (now retired). A `Var(k)`
        // use of one of these is a local inline call site.
        //
        // Off the INPUT decls, so a splice always takes the TEMPLATE — the body as
        // elaborated — never this pass's own walked rewrite of the same binding (which is
        // the ordinary function that binding also emits, already resolved against its
        // definition site and so wrong to splice anywhere else).
        let localInlines = Dictionary<NodeKey, TDecl>()

        for (d, _) in decls do
            match d with
            | TDecl.Let(TPat.NamedSimple(b, _, _), _, true, _) -> localInlines.[b] <- d
            | _ -> ()

        // The cast-based "provider carries no inlines" fast-path is retired: every
        // provider now implements the channel, and the cross-package path (no local
        // inlines, bodies served by the contract stack) must still walk, so the
        // signal that gated the skip is gone. Only the degenerate empty-file case
        // short-circuits; freezeTypars rebuilds every tree next anyway.
        if List.isEmpty decls then
            {
                Decls = decls
                Specializations = [||]
            }
        else
            // Build-wide monotone counter for freshened inline binders: one counter for the
            // whole run, so two expansions of the same template never mint the same key.
            let mutable counter = 0

            let mint () =
                let k = NodeKey.ofSyntheticCounter counter NodeKind.SynthPreFreezeInline
                counter <- counter + 1
                k

            // The resolved-specialization table this run builds, in SLOT ORDER — a slot is
            // RESERVED before the body that fills it is built (see `mintEntry`), so a slot is
            // briefly empty and the array is materialised only once every build has finished.
            let entries = ResizeArray<TSpecialization voption>()

            // Producer files a served body arrived with, retained for the whole run so that an
            // entry's foreign anchors stay readable. Keyed by path (`OriginSources`), so a file
            // serving many templates is retained once — and a path served at two DIFFERENT
            // contents faults at the read rather than silently re-attributing a body.
            let mutable origins = OriginSources.empty

            // The entries a later call site may REUSE — see `tryReuse` for what qualifies.
            let interned = Dictionary<Grounding, SpecializationId>()

            // Reach a cross-unit body by its resolved `SymbolKey` — the sole channel
            // (`tryInlineBody`), which routes a value key and a member key to the entry
            // that CARRIES it, so the body and the identity cannot disagree (and a member
            // is selected by EXACT key, never by a name lookup whose best-by-arity collapse
            // could serve a sibling overload's body).
            //
            // Every splice-eligible head is key-stamped upstream: value refs by
            // NameResolution (`ExternalValue`), operator / synthesised-intrinsic heads by
            // `Elaborate` (`Resolution.IntrinsicKey`), intra-body sibling refs by `Freeze`'s
            // publish rewrite, and a member call by its resolved `MemberKey`. Operators are
            // NOT an exception — a primitive `1 + 2` head is keyed and DOES reach
            // `ops-platform.fs`'s `(+)`. A `key = ValueNone` head carries no inline body by
            // construction (`Array.ofList` / ctor-as-value, handled by codegen recipes /
            // eta-expansion), so `ValueNone` is a genuine "no body", never a missed keyless
            // lookup. A provider with no inline bodies returns `ValueNone`.
            //
            // The KEY rides out with the body because it is the specialization table's template
            // identity: an entry says which template it resolved, and the only place that is
            // known is the lookup that found it.
            //
            // The THAW happens here and picks WHERE the body's nodes sit, which is the whole of
            // the difference between the two readings: a body with a retained producer file
            // keeps that file's own positions (it stays behind an edge and its indices never
            // have to mean anything against this unit's tokens), and one without is MOVED onto
            // the call site (its nodes land in this file's tree, where a producer's index would
            // name an unrelated token). Thawed per lookup, so two call sites of one template
            // never share an inference cell.
            let lookupExternal (at: SyntaxToken) (keyOpt: SymbolKey voption) : ServedBody voption =
                match keyOpt with
                | ValueSome key ->
                    ExternalSymbolProviders.tryInlineBody provider key
                    |> ValueOption.map (fun ib ->
                        let decl, origin =
                            match ib.Origin with
                            | ValueSome src ->
                                origins <- OriginSources.add src origins
                                Inline.thawBodyAtOrigin ctx.Store origins src.File ib.Decl, ValueSome src.File
                            | ValueNone -> Inline.thawBody ctx.Store at ib.Decl, ValueNone

                        {
                            Key = key
                            Decl = decl
                            ParamAttrs = ib.ParamAttrs
                            Origin = origin
                        }
                    )
                | ValueNone -> ValueNone

            // Eta-reify an `External` function used as a VALUE — `(+)` in
            // `List.fold (+) 0 xs`, `List.fold` itself in `let g = List.fold` — into
            // `fun p0 p1 -> f p0 p1`, turning a function NAME into a closure. This is
            // the sole eta in the compiler: codegen has none, so an `External` of
            // function type never reaches a backend in value position.
            //
            // Doing it here rather than post-freeze is what lets an inline-bodied one
            // finish: the saturated `App` the eta mints is claimed by THIS pass's own
            // `App` arm, which splices the body and resolves its `StaticOptimization`
            // against the context-pinned operand type. Reified after the freeze, that
            // `App` would be minted past the last point its body can be reached. And
            // only a pre-freeze eta reaches MEMBER bodies at all (codegen's lowering
            // never walks them).
            //
            // `body` is the reference's inline body when it has one. Arity is the
            // reference's arrow count, capped by the body's lambda arity:
            // `classifyApplication` rejects an over-applied inline body, and a partial eta
            // (`fun x -> f x` for a 2-arrow `f` whose body abstracts once) is still
            // type-correct. `ValueNone` when there is nothing to eta (arity 0) — a
            // non-function reference. This never recurses: the eta'd `App` re-presents
            // the SAME `External` in call-HEAD position, where the `App` arm claims it
            // before this value-position arm can see it.
            //
            // The eta mints an `App` and NOT an `InlineCall` even when the reference has a
            // body, because that `App` is claimed by the arm above, which is the ONE place
            // that decides between an edge and a splice. Minting the edge here would state
            // that fork a second time and let the two drift.
            let etaReify
                (body: ServedBody voption)
                (name: string)
                (keyOpt: SymbolKey voption)
                (refTy: SemType)
                (tok: SyntaxToken)
                : TExpr voption =
                let arity =
                    match body with
                    | ValueSome ib ->
                        let bodyArity =
                            match ib.Decl with
                            | TDecl.Let(_, value, _, _) -> lambdaArity value
                            | _ -> 0

                        min (Arrows.count ctx.Store refTy) bodyArity
                    | ValueNone -> Arrows.count ctx.Store refTy

                if arity = 0 then
                    ValueNone
                else
                    // Fresh binders come from the pass's own `mint`, so an eta site
                    // can never alias the binders of the body about to be spliced
                    // into it.
                    let binders =
                        Arrows.domains ctx.Store arity refTy |> List.mapi (fun i pty -> mint (), pty, i)

                    let appBody =
                        binders
                        |> List.fold
                            (fun acc (k, pty, i) ->
                                let resTy = Arrows.resultAfter ctx.Store (i + 1) refTy
                                TExpr.App(acc, TExpr.Var(k, pty, tok), resTy, tok)
                            )
                            (TExpr.External(name, keyOpt, refTy, tok))

                    binders
                    |> List.foldBack (fun (k, pty, _) (innerBody, innerTy) ->
                        let lamTy = TyFun(pty, innerTy)
                        TExpr.Lambda(TPat.NamedSimple(k, pty, tok), innerBody, lamTy, tok), lamTy
                    )
                    <| (appBody, Arrows.resultAfter ctx.Store arity refTy)
                    |> fst
                    |> ValueSome

            // Expand ONE inline binding for one use site: derive the site's type
            // arguments from the spine, substitute them through the body (which also
            // selects its `StaticOptimization` clause and dispatches its `TraitCall`s),
            // freshen the binders, and report every trait call the substitution could
            // NOT dispatch.
            //
            // The SINGLE resolution entry — local and external, applied and bare, spliced and
            // outlined — so no path can carry a body forward while quietly leaving an
            // unresolvable `TraitCall` in it. That matters because neither backend has a
            // `TraitCall` arm: an unreported one is an emitter crash, where a reported one is
            // "the type 'decimal' does not support the operator '+'" and stops the
            // compile before codegen. Reporting belongs here, not in `Inline`: the
            // expander is `PassContext`-free, and it sees the body before it acquires a
            // position — so the call site is the only anchor available to it. It reports at
            // EVERY site, including one that goes on to reuse an entry another site interned:
            // an unsupported operator is a fact about the site that wrote it.
            //
            // Deriving the type arguments is not only for static-opt selection: for any
            // generic inline it GROUNDS the body's typars to the caller's types. Without
            // it, a typar reachable only through the body (`asNode`'s
            // `value :?> SetTreeNode<'T>` result) stays a free `TyVar` root of the
            // CALLEE's scheme — beta-reduction binds the value params but never unifies
            // that typar — and pollutes the caller's frozen TAST as a `ResolvedTypes`
            // "unresolved TyVar".
            //
            // `Inline.freshen` is applied but `Inline.relocate` is NOT: renaming binders is
            // required of every expansion (two of one template must not share a codegen local
            // slot), where MOVING the body is a decision about placement that the two callers
            // below make differently — a physical splice must relocate onto the call site to
            // satisfy the `Anchor` invariant, an entry must not, because keeping the positions
            // the body was written at is the entry's whole purpose.
            let resolveAt
                (siteTok: SyntaxToken)
                (decl: TDecl)
                (spineArgs: (TExpr * SemType * SyntaxToken) list)
                : {| Body: TExpr; TypeArgs: SemType[] |} =
                match decl with
                | TDecl.Let(_, _, _, declTy) ->
                    let typeArgs = deriveInlineTypeArgs ctx.Store declTy spineArgs
                    let expanded, unresolved = Inline.inlineExpand ctx decl typeArgs

                    for u in unresolved do
                        ctx.Report(siteTok, unsupportedTrait ctx.Store u)

                    {|
                        Body = Inline.freshen mint expanded
                        // The grounding the specialization table keys on — recovered here and
                        // nowhere else, so an entry's stored key and the substitution its body
                        // actually underwent are the same array.
                        TypeArgs = typeArgs
                    |}
                | _ -> failwith "InlineExpansion: an inline body must be a TDecl.Let"

            // A SAME-UNIT template resolved for one call site and moved onto it. The template's
            // positions are its definition site's, shared by every expansion of it, so a copy
            // landing in a consuming tree takes the call site's — which is also the only file
            // identity available here: a local template is written in the file being compiled,
            // which has no `OriginFile` to name (it is not a producer any provider retained),
            // so it has no entry to sit behind and is spliced exactly as before.
            let expandLocalAt
                (siteTok: SyntaxToken)
                (decl: TDecl)
                (spineArgs: (TExpr * SemType * SyntaxToken) list)
                : TExpr =
                Inline.relocate siteTok (resolveAt siteTok decl spineArgs).Body

            // Inline-first lambda elimination. A lambda
            // argument bound to an inline function's parameter and FULLY APPLIED
            // inside the body is inlined at each use so its closure never exists —
            // F#'s `[<InlineIfLambda>]` guarantee, taken unconditionally for any
            // such parameter (the plan's "always beta-reduce fully-applied lambda
            // params" alternative; we do not yet read the attribute). The binder
            // key → its bound lambda; populated by `reduceClassified`, consumed by
            // the walker's `App` rule. Keys are `mint`-fresh per expansion, so the
            // map never needs structural scoping beyond the stack-disciplined
            // add/remove `reduceClassified` does.
            let lambdaEnv = Dictionary<NodeKey, TExpr>()

            // Peel a resolved inline body against one call site's spine and DECIDE each
            // parameter's fate — the half of the reduction that needs no recursion, and so the
            // half that can run before a specialization slot is reserved for the body the
            // recursion will build.
            //
            //   1. peel the inline's lambdas, pairing each parameter with its arg;
            //   2. a parameter bound to a bare `External` function VALUE is substituted into
            //      the body right here — the classification in (3) reads the substituted body;
            //   3. a lambda-valued parameter every use of which is a saturated application
            //      head is marked `FuseLambda` (the walker splices it away) — its closure
            //      vanishes;
            //   4. a declared `[<CallAtMostOnce>]` parameter is marked `FuseAtMostOnce`;
            //   5. everything else survives.
            //
            // `placement` reaches step (2) because that fusion happens HERE rather than in the
            // reduction: an argument substituted into an outlined body has left its own file
            // and is marked accordingly.
            let classifyApplication
                (placement: Placement)
                (paramAttrs: ParamAttrs[])
                (expanded: TExpr)
                (args: (TExpr * SemType * SyntaxToken) list)
                : Peeled =
                // Carry each application node's `tok` alongside the binder so the
                // surviving `Let` is anchored at the call site it lowers, and the template's
                // own binder token so an entry's parameter keeps the position it was written
                // at.
                let rec peel
                    (fn: TExpr)
                    (args: (TExpr * SemType * SyntaxToken) list)
                    (acc: InlineParam list)
                    : InlineParam list * TExpr =
                    match fn, args with
                    | _, [] -> List.rev acc, fn
                    | TExpr.Lambda(TPat.NamedSimple(k, paramTy, patTok), body, _, _), (arg, _, appTok) :: rest ->
                        peel
                            body
                            rest
                            ({
                                Key = k
                                Ty = paramTy
                                Arg = arg
                                AppTok = appTok
                                PatTok = patTok
                                // Provisional: the classification below is what settles it, and
                                // it needs the whole peel first.
                                Disposition = Disposition.Survive
                             }
                             :: acc)
                    | TExpr.Lambda(param, _, _, _), _ ->
                        failwithf "InlineExpansion: inline parameter destructuring is out of scope: %A" param
                    | _, _ :: _ -> failwith "InlineExpansion: over-application of an inline function"

                let bindings, core = peel expanded args []

                // A parameter bound to a bare `External` function value — a
                // library/top-level symbol reference, pure and capture-free (e.g.
                // `ignore` in `x |> ignore`, where `(|>) arg func = func arg` binds
                // `func = ignore`) — is substituted directly into the body BEFORE the
                // recursive walk. A saturated `func arg` use then re-forms the
                // `ignore arg` head and the walker expands its inline body (from
                // `ops-platform.fs`). Duplicating a value reference is always sound
                // (no side effect, no capture). Without this the binding survives as
                // `let func = ignore in func arg`, leaving `ignore` a bare external
                // value codegen cannot eta-expand ("no call recipe for external …").
                //
                // Recognised through `Inline.unmarked`: an argument an OUTER fusion already
                // marked is still the bare external value this rule is about, and re-marking
                // it here is right rather than redundant — two frames out is two pops.
                let externalValParams =
                    bindings
                    |> List.choose (fun p ->
                        match Inline.unmarked p.Arg with
                        | TExpr.External _ -> Some(p.Key, Placement.fuse placement p.Arg)
                        | _ -> None
                    )

                let externalKeys = HashSet<NodeKey>(externalValParams |> List.map fst)

                let core =
                    externalValParams |> List.fold (fun body (k, v) -> substituteVar k v body) core

                let candidates = Dictionary<NodeKey, TExpr>()

                for p in bindings do
                    match p.Arg with
                    | TExpr.Lambda _ -> candidates.[p.Key] <- p.Arg
                    | _ -> ()

                let bad = nonInlinableLambdaParams candidates core

                // Positionally aligned to the inline's curried parameters (freshen /
                // typar-substitution preserve order), so the index into `bindings` IS the
                // curried position `ParamAttrs` is indexed by.
                //
                // TODO(byref-capture half): a lambda arg that SURVIVES here (not fused —
                // stored or partially applied) and captures a byref-like value (`Span`,
                // `ReadOnlySpan`, any `ref struct`) is a real heap closure that cannot legally
                // hold it. Today it compiles to a heap closure regardless (we have no
                // byref-like detection — `SemType` has no ref-struct case and metadata drops
                // byref params, see Inline.isStructType). The full path forks here: (1) emit it
                // as a ref-struct closure (`Fun`-as-`ref struct`, the designed-for escape
                // hatch) so the capture is legal, or (3) reject it like F# when it genuinely
                // escapes (`HeapShared` per Regions). Either makes a currently (would-be)
                // rejected program compile or fail cleanly; both need the byref-like predicate
                // that does not exist yet.
                let classified =
                    bindings
                    |> List.mapi (fun i p ->
                        let disposition =
                            if externalKeys.Contains p.Key then
                                Disposition.FuseExternalValue
                            elif candidates.ContainsKey p.Key && not (bad.Contains p.Key) then
                                Disposition.FuseLambda
                            // A `[<CallAtMostOnce>]` parameter is substituted at its single
                            // (declaration-validated linear) use instead of bound eagerly, so
                            // the argument is evaluated at most once and on demand — the
                            // mechanism behind `&&`/`||` short-circuiting, driven by the
                            // declared attribute rather than a body-shape guess. Every other
                            // parameter keeps the eager binding (F#-strict evaluation order,
                            // single-evaluation, and closure capture undisturbed).
                            elif i < paramAttrs.Length && paramAttrs.[i].CallAtMostOnce then
                                Disposition.FuseAtMostOnce
                            else
                                Disposition.Survive

                        { p with Disposition = disposition }
                    )

                { Params = classified; Core = core }

            // Finish a classified application: walk the body (which is where every nested
            // inline head inside it resolves) and fuse in the call-site material the
            // classification marked, leaving the surviving parameters and their walked
            // arguments.
            //
            // The three fusions are all substitutions INTO the body, so their order relative to
            // one another does not matter: a fused parameter's key is `mint`-fresh and occurs
            // only in the body, never in another parameter's argument.
            let reduceClassified (placement: Placement) (walk: TExpr -> TExpr) (peeled: Peeled) : Reduced =
                let fusedLambdas =
                    peeled.Params |> List.filter (fun p -> p.Disposition = Disposition.FuseLambda)

                // Marked UNDER its own binders, not around the whole lambda. `Inline.betaReduce`
                // consumes those binders against arguments taken from the BODY the lambda is
                // spliced into, so the `Let`s that replace them belong to that body's file;
                // only what the lambda computes was written at the call site. (The binder
                // PATTERN's own token stays with it and no expression marker can cover it —
                // the one position a fused lambda still attributes to the body's file.)
                for p in fusedLambdas do
                    lambdaEnv.[p.Key] <- underLambdas (Placement.fuse placement) p.Arg

                let core = walk peeled.Core

                for p in fusedLambdas do
                    lambdaEnv.Remove p.Key |> ignore

                // Innermost parameter first, so the arguments are walked in the order the
                // `let` nesting binds them from the inside out.
                let mutable body = core
                let survivors = ResizeArray<InlineParam>()

                for p in List.rev peeled.Params do
                    match p.Disposition with
                    // Already substituted into `Core` / spliced at each use by the walk above:
                    // neither carries a surviving binding.
                    | Disposition.FuseExternalValue
                    | Disposition.FuseLambda -> ()
                    | Disposition.FuseAtMostOnce ->
                        body <- substituteVar p.Key (Placement.fuse placement (walk p.Arg)) body
                    | Disposition.Survive -> survivors.Add { p with Arg = walk p.Arg }

                survivors.Reverse()

                {
                    Body = body
                    Survivors = List.ofSeq survivors
                }

            // The PHYSICAL form of a reduction: surviving parameters re-bound with ordinary
            // `let`s, innermost last so the nesting matches left-to-right application order.
            let letBound (r: Reduced) : TExpr =
                List.foldBack
                    (fun (p: InlineParam) acc ->
                        TExpr.Let(TPat.NamedSimple(p.Key, p.Ty, p.AppTok), p.Arg, acc, TastWalk.exprTy acc, p.AppTok)
                    )
                    r.Survivors
                    r.Body

            // The entry `grounding` already names, if a previous site interned one.
            //
            // Two conditions decide `shareable`, and both are about whether an entry belongs to
            // the TEMPLATE rather than to one site. A fused reduction holds the site's own
            // material; a non-ground grounding leaves this thaw's inference cells inside the
            // body, which a second site linking to the entry would then share. Either way the
            // next site resolves its own.
            let tryReuse (grounding: Grounding) (shareable: bool) : SpecializationId voption =
                if shareable then
                    match interned.TryGetValue grounding with
                    | true, spec -> ValueSome spec
                    | _ -> ValueNone
                else
                    ValueNone

            // The TABLE form of a reduction: a new entry, its surviving parameters abstracted
            // back into the lambda spine an `InlineCall`'s arguments are positional against.
            // Arity is therefore the surviving-parameter count and nothing stores it — a
            // parameter the reduction fused is simply not a parameter of the entry.
            //
            // The slot is reserved, and (when shareable) INTERNED, BEFORE `build` runs. That
            // ordering is what a template whose resolution reaches itself meets: it finds its
            // own id already present and terminates into a self-edge, leaving a CYCLIC table —
            // a finite thing a checker can reject — where building first and interning after
            // recurses until the stack goes.
            let mintEntry
                (grounding: Grounding)
                (shareable: bool)
                (origin: OriginFile)
                (build: unit -> Reduced)
                : SpecializationId * InlineParam list =
                let slot = entries.Count
                let spec = SpecializationId slot
                entries.Add ValueNone

                if shareable then
                    interned.[grounding] <- spec

                let reduced = build ()

                let value, declTy =
                    List.foldBack
                        (fun (p: InlineParam) (inner, innerTy) ->
                            let lamTy = TyFun(p.Ty, innerTy)
                            TExpr.Lambda(TPat.NamedSimple(p.Key, p.Ty, p.PatTok), inner, lamTy, p.PatTok), lamTy
                        )
                        reduced.Survivors
                        (reduced.Body, TastWalk.exprTy reduced.Body)

                // What LICENSES `TExpr.CallerExpr`: the node pops one frame, and "the frame
                // out" names a single file only while the entry has a single call edge. A
                // shareable entry is exactly the one that gives that up, so a mark inside it
                // would be undefined rather than merely unhelpful.
                //
                // It holds by construction — `shareable` implies `Peeled.isClosed`, which is
                // "no parameter fused", and a nested reduction puts its own fusions in its own
                // entry — so this is the check that a fusion bug shows up as a fault here
                // instead of as a body silently shared across sites with one site's material
                // baked into it.
                if shareable && Inline.containsCallerExpr value then
                    failwithf
                        "InlineExpansion: specialization %d is shareable but marks caller material — a closed reduction fused nothing, so this entry's parameters were mis-classified"
                        slot

                entries.[slot] <-
                    ValueSome
                        {
                            Key = grounding.Key
                            Origin = origin
                            // The binder is unread — `Inline.inlineExpand` and the flattener both
                            // match `TDecl.Let(_, value, _, _)` — so it is minted rather than
                            // taken from anything, exactly as a harvested member body's is.
                            Decl =
                                TDecl.Let(
                                    TPat.NamedSimple(mint (), declTy, TastWalk.exprTok value),
                                    value,
                                    true,
                                    declTy
                                )
                        }

                spec, reduced.Survivors

            // Expand ONE cross-unit call site into an EDGE: intern the resolved specialization
            // and emit an `InlineCall` naming it, with this site's own arguments.
            //
            // A body the provider retained no file for cannot be an entry — there is no anchor
            // domain to record — so it takes the physical form its thaw already committed to.
            let expandExternalCall
                (walk: TExpr -> TExpr)
                (headTok: SyntaxToken)
                (served: ServedBody)
                (spineArgs: (TExpr * SemType * SyntaxToken) list)
                : TExpr =
                let resolved = resolveAt headTok served.Decl spineArgs

                // Read off the served body, ahead of the classification, so every fusion of one
                // reduction agrees about which file its material ends up in.
                let placement =
                    match served.Origin with
                    | ValueSome _ -> Placement.Outlined
                    | ValueNone -> Placement.Spliced

                let peeled = classifyApplication placement served.ParamAttrs resolved.Body spineArgs

                match served.Origin with
                | ValueNone -> letBound (reduceClassified placement walk peeled)
                | ValueSome origin ->
                    let grounding =
                        {
                            Key =
                                {
                                    Template = served.Key
                                    TypeArgs = EqArray.ofArray resolved.TypeArgs
                                }
                            Arity = List.length peeled.Params
                        }

                    let shareable =
                        Peeled.isClosed peeled
                        && resolved.TypeArgs |> Array.forall (isGroundType ctx.Store)

                    // The application's own result type, which the outermost `App` node already
                    // records (`collectSpine` pairs each argument with its node's result). Read
                    // off the SITE rather than off the entry: a reused entry's types are the
                    // thaw that built it, and this node belongs to this file.
                    let resultTy =
                        match List.tryLast spineArgs with
                        | Some(_, ty, _) -> ty
                        | None -> TastWalk.exprTy resolved.Body

                    match tryReuse grounding shareable with
                    // A shared entry is CLOSED, so every peeled parameter survived it and this
                    // site's whole spine is the edge's argument list.
                    | ValueSome spec ->
                        let args = peeled.Params |> List.map (fun p -> walk p.Arg)
                        TExpr.InlineCall(spec, EqArray.ofList args, resultTy, headTok)
                    | ValueNone ->
                        let spec, survivors =
                            mintEntry grounding shareable origin (fun () -> reduceClassified placement walk peeled)

                        TExpr.InlineCall(spec, EqArray.ofList [ for p in survivors -> p.Arg ], resultTy, headTok)

            // The expansion walker. This is now the sole inline expander —
            // it took over `EmitLower.lowerExpr`'s (retired) inline branches
            // verbatim, minus eta-reification (an `External` function VALUE is
            // still left as a leaf for codegen). Crucially the `App` arm is
            // ALWAYS handled explicitly (never falls through to `TastWalk`'s
            // default child recursion): collect the whole spine, keep an
            // `External` call head verbatim, and recurse only into the ARGS
            // (`rebuildApp head' (args |> walk)`). Relying on default recursion
            // would instead let the walker descend into a saturated op's
            // partial-application sub-`App` and expand it with a single arg —
            // leaving a dangling `fun y -> …` closure with a free `TyVar`.
            let mapper =
                { TastWalk.identityMapper with
                    OverrideExpr =
                        fun m e ->
                            let walk x = TastWalk.mapExpr m x

                            match e with
                            | TExpr.App _ ->
                                let markedHead, spineArgs = TastWalk.collectSpine [] e

                                // Dispatch reads THROUGH any caller mark: a fused external value
                                // in head position is still the head it was before the fusion
                                // marked it, and a head that stopped being recognised would fall
                                // to the catch-all as a bare external no backend can call. The
                                // mark is consumed with the node — the rewrite replaces the head
                                // itself, so there is no subtree left for it to cover.
                                let head = Inline.unmarked markedHead

                                match head with
                                | TExpr.Var(k, _, headTok) when localInlines.ContainsKey k ->
                                    ValueSome(
                                        letBound (
                                            reduceClassified
                                                // A same-unit template is MOVED onto the call
                                                // site, so its body and the arguments fused into
                                                // it are one anchor domain already.
                                                Placement.Spliced
                                                walk
                                                (classifyApplication
                                                    Placement.Spliced
                                                    (localParamAttrs k)
                                                    (expandLocalAt headTok localInlines.[k] spineArgs)
                                                    spineArgs)
                                        )
                                    )
                                // A saturated use of an inline-first lambda
                                // parameter: splice a fresh
                                // copy of its bound lambda, beta-reduced against the
                                // call args, and walk it (nested inline heads /
                                // further lambda params resolve in the recursion).
                                // `nonInlinableLambdaParams` guaranteed every use is
                                // saturated, so `betaReduce` consumes exactly the
                                // lambda's arity — no surviving closure.
                                //
                                // `freshen`, not `spliceAt`: what is copied is the CALL
                                // SITE's own argument, written in THIS file, and the use it
                                // is copied to is inside a body already moved onto that same
                                // call site — so its tokens are already local, and its own
                                // are the finer ones. They are also the anchor a
                                // `FunVerdicts` entry for it is filed under, and
                                // these copies are the only ones pooled: an inlined-away
                                // parameter keeps no surviving `let`.
                                | TExpr.Var(k, _, _) when lambdaEnv.ContainsKey k ->
                                    ValueSome(walk (Inline.betaReduce (Inline.freshen mint lambdaEnv.[k]) spineArgs))
                                | TExpr.External(_, keyOpt, _, headTok) ->
                                    match lookupExternal headTok keyOpt with
                                    // An external WITH an inline body ALWAYS expands —
                                    // no operand-groundness gate. An un-ground `^T`
                                    // simply selects no per-primitive
                                    // `StaticOptimization` clause and falls to the
                                    // body's BASE, which is where the safe generic
                                    // default lives (`EqualityComparer<^T>.Default.Equals`
                                    // for `=`). Declining instead routed the
                                    // head to a name-keyed raw-IL fallback, turning a
                                    // structural `=` into a reference `ceq`.
                                    | ValueSome served -> ValueSome(expandExternalCall walk headTok served spineArgs)
                                    // An external with no inline body (a real
                                    // cross-package call): keep the head, lower the
                                    // args — exactly codegen's `head'` rule, left for
                                    // its recipe path.
                                    | _ ->
                                        ValueSome(
                                            TastWalk.rebuildApp
                                                markedHead
                                                [ for (a, t, tok) in spineArgs -> walk a, t, tok ]
                                        )
                                // A dotted member call on an external type — the same
                                // head `x.get_Item(2)` / `w.Poke 41` lowers to. The
                                // member-keyed inline store forks call-vs-splice here:
                                //   * `ValueSome served` — a concrete `(# … #)`-bodied member
                                //     (harvested `this`-first). EXPAND it. The receiver is
                                //     a FIELD of the head, not a spine arg, so PREPEND it
                                //     onto the spine (`this`→receiver); a STATIC member
                                //     (`receiver = ValueNone`) prepends nothing. Then
                                //     expand via the SAME path the `External` arm uses —
                                //     `expandExternalCall` consumes the
                                //     spine POSITIONALLY, so with `this` at curried
                                //     position 0 each `pi` aligns to `argi`.
                                //   * `ValueNone` — a real CLR/JS method with no inline
                                //     body: keep the call, walking the receiver (inside the
                                //     head) and the args, exactly the `_` catch-all rule.
                                | TExpr.ExternalMember(receiver, key, _, _, _, memberTok) ->
                                    match lookupExternal memberTok (ValueSome key) with
                                    | ValueSome served ->
                                        let fullSpine =
                                            match receiver with
                                            | ValueSome r -> (r, TastWalk.exprTy r, memberTok) :: spineArgs
                                            | ValueNone -> spineArgs

                                        ValueSome(expandExternalCall walk memberTok served fullSpine)
                                    | ValueNone ->
                                        ValueSome(
                                            TastWalk.rebuildApp
                                                (walk markedHead)
                                                [ for (a, t, tok) in spineArgs -> walk a, t, tok ]
                                        )
                                // A non-external, non-local-inline head (e.g. a
                                // higher-order parameter): lower the head and args,
                                // keeping the spine intact. The head SURVIVES here, so it is
                                // rebuilt marked — only a rewrite that consumes the node
                                // consumes its mark.
                                | _ ->
                                    ValueSome(
                                        TastWalk.rebuildApp
                                            (walk markedHead)
                                            [ for (a, t, tok) in spineArgs -> walk a, t, tok ]
                                    )
                            // A BARE (non-applied) reference to a LOCAL inline — the
                            // template used as a value. No spine, so no type argument is
                            // derivable and the body's typars stay abstract; it still
                            // goes through `expandLocalAt` so its static-opt clauses resolve
                            // and any trait call it cannot dispatch is REPORTED rather
                            // than handed to a backend that has no arm for it.
                            | TExpr.Var(k, _, tok) when localInlines.ContainsKey k ->
                                ValueSome(walk (expandLocalAt tok localInlines.[k] []))
                            // A BARE (non-applied) reference to a cross-package `let`
                            // value whose body is a single zero-operand intrinsic
                            // (`undefined`, `defaultof`): the intrinsic body stands in place of
                            // the `External` reference, so codegen emits the bare intrinsic with
                            // no import and never a `const undefined = undefined` definition. A
                            // nullary intrinsic value cannot be applied, so this never collides
                            // with the `App`-head inline paths above.
                            //
                            // A GENERIC nullary intrinsic (`defaultof<'T>`) carries its own
                            // scheme typar in the harvested body; the bare splice has no spine
                            // to derive it from, so ground the intrinsic's operand/result to the
                            // reference's already-resolved type (`refTy` — `defaultof`'s 'T
                            // unified with the use site). Without this the callee typar survives
                            // as an unbound `TyVar` ("unresolved TyVar" at freeze). A NON-generic
                            // one (`undefined`) is unchanged: `refTy` equals its concrete result
                            // type and it carries no operand.
                            //
                            // ANY OTHER external of function type in value position is a
                            // function name used as a value (`List.fold (+) 0 xs`, or
                            // `List.fold` itself): eta-reify it into a closure and walk
                            // the result, so the `App` arm above splices the body (when
                            // there is one) at the freshly-minted call head. This is the
                            // compiler's only eta — an inline-bodied external and a plain
                            // one take the SAME path, differing only in whether the `App`
                            // finds a body to splice. An external of non-function type
                            // (`System.Int32.MaxValue`) etas to nothing and stays a leaf.
                            | TExpr.External(name, keyOpt, refTy, tok) ->
                                let body = lookupExternal tok keyOpt

                                match body with
                                | ValueSome served ->
                                    match Inline.nullaryIntrinsicValueBody served.Decl with
                                    // A nullary intrinsic is a single node with no binders, so
                                    // it needs no expansion — but it crosses the same file
                                    // boundary every other served body does, so it becomes an
                                    // ordinary (zero-parameter) entry rather than a second,
                                    // parallel notion of "a body from elsewhere". The node keeps
                                    // the token the intrinsic was WRITTEN at, which is the whole
                                    // difference an entry buys.
                                    | ValueSome(TExpr.ILIntrinsic(op, operand, args, _, intrinsicTok)) ->
                                        let groundedOperand =
                                            match operand with
                                            | ValueSome _ -> ValueSome refTy
                                            | ValueNone -> ValueNone

                                        match served.Origin with
                                        | ValueSome origin ->
                                            let grounding =
                                                {
                                                    Key =
                                                        {
                                                            Template = served.Key
                                                            // The reference's own resolved type IS
                                                            // the grounding: it is what a generic
                                                            // `defaultof<'T>` is instantiated at,
                                                            // and the only thing a bare reference
                                                            // supplies.
                                                            TypeArgs = EqArray.ofArray [| refTy |]
                                                        }
                                                    Arity = 0
                                                }

                                            let shareable = isGroundType ctx.Store refTy

                                            let spec =
                                                match tryReuse grounding shareable with
                                                | ValueSome spec -> spec
                                                | ValueNone ->
                                                    mintEntry
                                                        grounding
                                                        shareable
                                                        origin
                                                        (fun () ->
                                                            {
                                                                Body =
                                                                    TExpr.ILIntrinsic(
                                                                        op,
                                                                        groundedOperand,
                                                                        args,
                                                                        refTy,
                                                                        intrinsicTok
                                                                    )
                                                                Survivors = []
                                                            }
                                                        )
                                                    |> fst

                                            ValueSome(TExpr.InlineCall(spec, EqArray.empty, refTy, tok))
                                        // No retained producer file: the thaw already moved the
                                        // node onto this reference, so it stands where it lands.
                                        | ValueNone ->
                                            ValueSome(TExpr.ILIntrinsic(op, groundedOperand, args, refTy, tok))
                                    | _ -> etaReify body name keyOpt refTy tok |> ValueOption.map walk
                                | ValueNone -> etaReify body name keyOpt refTy tok |> ValueOption.map walk
                            | _ -> ValueNone
                }

            let walkExpr (e: TExpr) : TExpr = TastWalk.mapExpr mapper e

            // Apply `f` to every expression a declaration carries: a module binding's value,
            // a `do` expression, and everything a type declaration holds — member bodies, the
            // class preambles (`[static] let` initialisers and `[static] do` bodies),
            // secondary-ctor `let`s + chain args, and the `inherit Base(args)` arguments.
            // Mirrors `Elaborate.freezeKind`'s expr-bearing coverage, relocating codegen's
            // `EmitLower.spliceExternalInlinesInExpr` splice (`NominalEmit`'s three
            // sites) out of emission. The expansion walk also covers local inlines a member
            // body might call — a superset of the external-only codegen splice —
            // but a local-inline reference in a member body would otherwise dangle
            // (codegen drops local inline templates), so this only ever turns a
            // would-be error into a correct expansion; existing green corpora carry
            // none, so output is byte-identical.
            //
            // Parameterised over `f` because the pass makes TWO passes over this same
            // coverage — the expansion walk, then the flattening of the graph it built — and a
            // second hand-written traversal is exactly how one of them comes to miss an
            // expression slot the other reaches.
            let mapDeclExprs (f: TExpr -> TExpr) (d: TDecl) : TDecl =
                let mapMember (m: TTypeMember) : TTypeMember = { m with Body = f m.Body }

                let mapPreambleEntry (entry: TPreambleEntry) : TPreambleEntry =
                    match entry with
                    | TPreambleEntry.Let l -> TPreambleEntry.Let { l with Init = f l.Init }
                    | TPreambleEntry.Do e -> TPreambleEntry.Do(f e)

                let mapKind (k: TTypeKind) : TTypeKind =
                    match k with
                    | TTypeKind.Interface _ -> k
                    // An enum has no member bodies to walk.
                    | TTypeKind.Enum _ -> k
                    | TTypeKind.Union(cases, members, interfaces) ->
                        TTypeKind.Union(
                            cases,
                            members |> EqArray.map mapMember,
                            interfaces |> EqArray.map (fun (ity, ms) -> ity, ms |> EqArray.map mapMember)
                        )
                    | TTypeKind.Record(fields, members, interfaces, valueKind) ->
                        TTypeKind.Record(
                            fields,
                            members |> EqArray.map mapMember,
                            interfaces |> EqArray.map (fun (ity, ms) -> ity, ms |> EqArray.map mapMember),
                            valueKind
                        )
                    | TTypeKind.Class c ->
                        TTypeKind.Class
                            { c with
                                Members = c.Members |> EqArray.map mapMember
                                Interfaces =
                                    c.Interfaces |> EqArray.map (fun (ity, ms) -> ity, ms |> EqArray.map mapMember)
                                StaticPreamble = c.StaticPreamble |> EqArray.map mapPreambleEntry
                                InstancePreamble = c.InstancePreamble |> EqArray.map mapPreambleEntry
                                SecondaryCtors =
                                    c.SecondaryCtors
                                    |> EqArray.map (fun sc ->
                                        { sc with
                                            Lets = sc.Lets |> EqArray.map (fun cl -> { cl with Init = f cl.Init })
                                            PrimaryArgs = sc.PrimaryArgs |> EqArray.map f
                                            FieldInits =
                                                sc.FieldInits |> EqArray.map (fun fi -> { fi with Init = f fi.Init })
                                        }
                                    )
                                BaseCtorCall =
                                    c.BaseCtorCall
                                    |> ValueOption.map (fun bc ->
                                        { bc with
                                            Args = bc.Args |> EqArray.map f
                                        }
                                    )
                            }

                match d with
                | TDecl.Let(p, value, isInline, ty) -> TDecl.Let(p, f value, isInline, ty)
                | TDecl.Expression(e, ty) -> TDecl.Expression(f e, ty)
                | TDecl.Type td -> TDecl.Type { td with Kind = mapKind td.Kind }

            let expanded = decls |> List.map (fun (d, env) -> mapDeclExprs walkExpr d, env)

            let table =
                entries
                |> Seq.mapi (fun i e ->
                    match e with
                    | ValueSome entry -> entry
                    | ValueNone ->
                        failwithf
                            "InlineExpansion: specialization %d was reserved but never built — an expansion abandoned its slot"
                            i
                )
                |> Array.ofSeq

            // Flatten the graph the walk just built, so what leaves this pass is what left it
            // before the table existed. The table is the pass's product; nothing downstream of
            // here expands it yet, and both emit routers fault on an edge that reaches them.
            //
            // Only the DECLS are flattened — an entry keeps its own edges, which is what makes
            // the table a DAG rather than N copies of one body.
            {
                Decls =
                    expanded
                    |> List.map (fun (d, env) -> mapDeclExprs (Inline.flatten mint table) d, env)
                Specializations = table
            }
