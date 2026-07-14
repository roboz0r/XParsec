namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

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
// A cross-unit body arrives FROZEN and is THAWED here (`Inline.thawBody`), minting this
// unit's own inference cells. That thaw is the single immutable→mutable transition on
// the provider seam: nothing below it can `UnionFind.union` into a producer's cells,
// because it never holds one.
//
// Scope: module-level decls (`TDecl.Let` non-inline values and
// `TDecl.Expression`) AND every expression a `TDecl.Type` carries (member
// bodies, `static let` inits, secondary-ctor `let`s + chain args, base-ctor
// args). It deliberately does NOT touch inline TEMPLATES
// (`TDecl.Let(isInline = true)`) — codegen drops them, and `Freeze` publishes them as
// vocabulary, so the template-publish path sees them exactly as elaborated.
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
// eliminated — its closure never exists (`reduceApplication` +
// `nonInlinableLambdaParams` + the `lambdaEnv` splice). A lambda that is stored
// or partially applied survives as a real closure, exactly as before. The
// byref-like-capture half (reject / ref-struct closures for a SURVIVING closure
// that holds a `Span`/`ref struct`) is deferred — see the TODO in
// `reduceApplication`; it needs a byref-like predicate that does not exist yet.

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

    /// Beta-reduce a curried lambda against its spine args, lowering each
    /// application to a `TExpr.Let` — mirrors `EmitLower.betaReduce`. Lambda count
    /// must match the spine-arg count for a fully applied call. Used to splice an
    /// inline-first lambda parameter at a saturated use site;
    /// the inline FUNCTION itself is reduced by `reduceApplication`, which
    /// peels the same way but classifies lambda params for elimination first.
    let rec private betaReduce (fn: TExpr) (args: (TExpr * SemType * SyntaxToken) list) : TExpr =
        match fn, args with
        | _, [] -> fn
        | TExpr.Lambda(TPat.NamedSimple(k, paramTy, patTok), lamBody, _, _), (arg, _, appTok) :: rest ->
            let reduced = betaReduce lamBody rest
            // Anchor the synthesised `Let` at the application node it lowers; the
            // binder keeps the lambda parameter's own token.
            TExpr.Let(TPat.NamedSimple(k, paramTy, patTok), arg, reduced, TastWalk.exprTy reduced, appTok)
        | TExpr.Lambda(param, _, _, _), _ ->
            failwithf "InlineExpansion: inline parameter destructuring is out of scope: %A" param
        | _, _ :: _ -> failwith "InlineExpansion: over-application of an inline function"

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
        let rec count (t: SemType) : int =
            match Unification.zonk t with
            | TyFun(_, r) -> 1 + count r
            | _ -> 0

        /// The first `n` domain types, left to right.
        let rec domains (n: int) (t: SemType) : SemType list =
            if n <= 0 then
                []
            else
                match Unification.zonk t with
                | TyFun(a, b) -> a :: domains (n - 1) b
                | _ -> []

        /// What the spine returns once `n` arguments have been applied.
        let rec resultAfter (n: int) (t: SemType) : SemType =
            let t = Unification.zonk t

            if n <= 0 then
                t
            else
                match t with
                | TyFun(_, b) -> resultAfter (n - 1) b
                | _ -> t

    /// A (zonked) `SemType` with no free `TyVar` anywhere — fully monomorphic. The
    /// `SemType` sibling of `FrozenTypeBridge.ftIsGround` (this one zonks; the frozen
    /// one has no vars to zonk). Used to rank competing candidates for one typar in
    /// `deriveInlineTypeArgs` — a ground candidate beats an abstract one.
    let rec private isGroundType (t: SemType) : bool =
        match Unification.zonk t with
        | TyVar _
        | TyUnknown _
        | TyTypar _ -> false
        | t -> SemType.forallChildren isGroundType t

    /// Recover an inline binding's type arguments at a call site by matching its
    /// declared parameter (and return) types — carrying the quantified typars —
    /// against the actual spine-arg types. Tolerant: a typar the params don't pin is
    /// left as its own `TyVar`, which selects no `when ^T : Type` clause and so falls
    /// to the body's base. Returned in `Inline.quantifiedTypars` order. A verbatim port of
    /// `EmitLower.deriveInlineTypeArgs` (`zonk` → `Unification.zonk`,
    /// `typeOfExpr` → `TastWalk.exprTy`).
    let private deriveInlineTypeArgs (declTy: SemType) (spineArgs: (TExpr * SemType * SyntaxToken) list) : SemType[] =
        let typars = Inline.quantifiedTypars declTy

        if typars.Length = 0 then
            [||]
        else
            let roots = typars |> Array.map UnionFind.find
            let result = Array.create roots.Length ValueNone

            let rec go (defT: SemType) (actT: SemType) =
                match Unification.zonk defT, Unification.zonk actT with
                | TyVar tv, act ->
                    let r = UnionFind.find tv

                    match roots |> Array.tryFindIndex (fun x -> System.Object.ReferenceEquals(x, r)) with
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
                        | ValueSome prev when not (isGroundType prev) && isGroundType act -> result.[i] <- ValueSome act
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

            pairGo (Arrows.domains nArgs declTy) [ for (a, _, _) in spineArgs -> TastWalk.exprTy a ]

            // Pair the result position too: `failwith`'s only typar `'T` sits in
            // the *return* (`string -> 'T`), so the param walk leaves it unbound.
            // The last spine arg's recorded type is the whole application's result
            // (`collectSpine` pairs each arg with its `App` node's result), so
            // unifying it against `declTy`'s return position grounds the result
            // typars.
            if nArgs > 0 then
                let declRetTy = Arrows.resultAfter nArgs declTy
                let _, actualRetTy, _ = spineArgs |> List.last
                go declRetTy actualRetTy

            Array.mapi
                (fun i v ->
                    match v with
                    | ValueSome t -> t
                    | ValueNone -> TyVar roots.[i]
                )
                result

    /// A short display name for the receiver of an unresolved trait call. An unpinned
    /// typar prints as F#'s anonymous `'a` — the honest rendering of "a type parameter
    /// nothing pinned". Total by construction: this text reaches the USER, so no case
    /// may fall through to a `%A` dump of the internal `SemType` DU.
    let rec private receiverName (t: SemType) : string =
        match UnionFind.headZonk t with
        | TyConst(key, _) ->
            let (DisplayName shown) = SymbolKeyOps.simpleName key
            shown
        | TyEnum key -> SymbolKeyOps.typeMetaName key
        | TyClass(k, _)
        | TyUnion(k, _)
        | TyRecord(k, _) -> SymbolKeyOps.typeMetaName k
        | TyVar _
        | TyTypar _ -> "'a"
        | TyFun _ -> "function"
        | TyTuple _ -> "tuple"
        | TyOr ms -> ms.Members |> EqSet.toList |> List.map receiverName |> String.concat " | "
        | TyLiteral v -> sprintf "%A" v
        | TyUnknown name -> name
        | TyKeyOf _
        | TyIndexedAccess _
        | TyConditional _ -> "type expression"

    /// The user-facing wording for a trait call the expansion could not dispatch.
    /// An operator is named as the user WROTE it (`+`), never by the member it compiled
    /// to (`op_Addition`) — `OperatorNames.sourceSymbol` inverts the lexer's own table,
    /// so the spelling cannot drift from the name. A member outside that table is not an
    /// operator at all (a user-written `(^T: (member GetAwaiter: …) x)`), and says so.
    let private unsupportedTraitMessage (u: Inline.UnresolvedTrait) : string =
        match OperatorNames.sourceSymbol u.MemberName with
        | ValueSome symbol ->
            sprintf "The type '%s' does not support the operator '%s'" (receiverName u.Receiver) symbol
        | ValueNone -> sprintf "The type '%s' does not support the member '%s'" (receiverName u.Receiver) u.MemberName

    /// Expand the module-level inlines in one decl-list (the elaborated,
    /// `TyVar`-carrying decls paired with their freeze envs). The cross-unit inline-body
    /// channel is `provider` itself — the body rides the resolved entry, reached by the
    /// key the use-site node carries; a front-end-only provider serves none and every
    /// lookup returns `ValueNone`, so the walk is an identity rebuild — which
    /// `Elaborate.freezeTypars` does to every decl immediately after regardless, so there
    /// is no node-identity to preserve by skipping it.
    let run
        (ctx: PassContext)
        (decls: (TDecl * (TypeVar * SemType) list) list)
        : (TDecl * (TypeVar * SemType) list) list =

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
            decls
        else
            // Build-wide monotone counter for freshened inline binders: one counter for the
            // whole run, so two expansions of the same template never mint the same key.
            let mutable counter = 0

            let mint () =
                let k = NodeKey.ofSyntheticCounter counter NodeKind.SynthPreFreezeInline
                counter <- counter + 1
                k

            // A cross-unit body, THAWED into this unit's `SemType` domain — fresh cells
            // by construction, so nothing below can unify into the producer's inference
            // state. Thawed per lookup, so two call sites of one template never share a
            // cell either.
            let thaw (ib: InlineBody) : TInlineBody =
                {
                    Decl = Inline.thawBody ib.Decl
                    ParamAttrs = ib.ParamAttrs
                }

            // Splice a cross-unit body by its resolved `SymbolKey` — the sole channel
            // (`tryInlineBody`), which routes a value key and a member key to the entry
            // that CARRIES it, so the body and the identity cannot disagree (and a member
            // is selected by EXACT key, never by a name lookup whose best-by-arity collapse
            // could serve a sibling overload's body).
            //
            // Every splice-eligible head is key-stamped upstream: value refs by
            // NameResolution (`ExternalValue`), operator / synthesised-intrinsic heads by
            // `Elaborate` (`Resolution.IntrinsicKey`), intra-body sibling refs by `Freeze`'s
            // publish rewrite, and a member call by its resolved `MemberKey`. Operators are
            // NOT an exception — a primitive `1 + 2` head is keyed and DOES splice
            // `ops-platform.fs`'s `(+)`. A `key = ValueNone` head carries no inline body by
            // construction (`Array.ofList` / ctor-as-value, handled by codegen recipes /
            // eta-expansion), so `ValueNone` is a genuine "no body", never a missed keyless
            // splice. A provider with no inline bodies returns `ValueNone`.
            let lookupExternal (keyOpt: SymbolKey voption) : TInlineBody voption =
                match keyOpt with
                | ValueSome key -> ExternalSymbolProviders.tryInlineBody provider key |> ValueOption.map thaw
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
            // `reduceApplication` rejects an over-applied inline body, and a partial eta
            // (`fun x -> f x` for a 2-arrow `f` whose body abstracts once) is still
            // type-correct. `ValueNone` when there is nothing to eta (arity 0) — a
            // non-function reference. This never recurses: the eta'd `App` re-presents
            // the SAME `External` in call-HEAD position, where the `App` arm claims it
            // before this value-position arm can see it.
            let etaReify
                (body: TInlineBody voption)
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

                        min (Arrows.count refTy) bodyArity
                    | ValueNone -> Arrows.count refTy

                if arity = 0 then
                    ValueNone
                else
                    // Fresh binders come from the pass's own `mint`, so an eta site
                    // can never alias the binders of the body about to be spliced
                    // into it.
                    let binders = Arrows.domains arity refTy |> List.mapi (fun i pty -> mint (), pty, i)

                    let appBody =
                        binders
                        |> List.fold
                            (fun acc (k, pty, i) ->
                                let resTy = Arrows.resultAfter (i + 1) refTy
                                TExpr.App(acc, TExpr.Var(k, pty, tok), resTy, tok)
                            )
                            (TExpr.External(name, keyOpt, refTy, tok))

                    binders
                    |> List.foldBack (fun (k, pty, _) (innerBody, innerTy) ->
                        let lamTy = TyFun(pty, innerTy)
                        TExpr.Lambda(TPat.NamedSimple(k, pty, tok), innerBody, lamTy, tok), lamTy
                    )
                    <| (appBody, Arrows.resultAfter arity refTy)
                    |> fst
                    |> ValueSome

            // Expand ONE inline binding for one use site: derive the site's type
            // arguments from the spine, substitute them through the body (which also
            // selects its `StaticOptimization` clause and dispatches its `TraitCall`s),
            // freshen the binders, and report every trait call the substitution could
            // NOT dispatch.
            //
            // The SINGLE expansion entry — local and external, applied and bare — so no
            // path can splice a body while quietly leaving an unresolvable `TraitCall`
            // in it. That matters because neither backend has a `TraitCall` arm: an
            // unreported one is an emitter crash, where a reported one is
            // "the type 'decimal' does not support the operator '+'" and stops the
            // compile before codegen. Reporting belongs here, not in `Inline`: the
            // expander is `PassContext`-free, and the spliced body's tokens address the
            // LIBRARY file it came from — the call site is the only honest anchor.
            //
            // Deriving the type arguments is not only for static-opt selection: for any
            // generic inline it GROUNDS the body's typars to the caller's types. Without
            // it, a typar reachable only through the body (`asNode`'s
            // `value :?> SetTreeNode<'T>` result) stays a free `TyVar` root of the
            // CALLEE's scheme — beta-reduction binds the value params but never unifies
            // that typar — and pollutes the caller's frozen TAST as a `ResolvedTypes`
            // "unresolved TyVar".
            let expandAt
                (siteTok: SyntaxToken)
                (decl: TDecl)
                (spineArgs: (TExpr * SemType * SyntaxToken) list)
                : TExpr =
                match decl with
                | TDecl.Let(_, _, _, declTy) ->
                    let expanded, unresolved =
                        Inline.inlineExpand decl (deriveInlineTypeArgs declTy spineArgs)

                    for u in unresolved do
                        ctx.Error(NodeKey.ofToken siteTok NodeKind.ExprApp, unsupportedTraitMessage u)

                    Inline.freshen mint expanded
                | _ -> failwith "InlineExpansion: an inline body must be a TDecl.Let"

            // Inline-first lambda elimination. A lambda
            // argument bound to an inline function's parameter and FULLY APPLIED
            // inside the body is inlined at each use so its closure never exists —
            // F#'s `[<InlineIfLambda>]` guarantee, taken unconditionally for any
            // such parameter (the plan's "always beta-reduce fully-applied lambda
            // params" alternative; we do not yet read the attribute). The binder
            // key → its bound lambda; populated by `reduceApplication`, consumed by
            // the walker's `App` rule. Keys are `mint`-fresh per expansion, so the
            // map never needs structural scoping beyond the stack-disciplined
            // add/remove `reduceApplication` does.
            let lambdaEnv = Dictionary<NodeKey, TExpr>()

            // Beta-reduce an inline expansion `expanded` against its call `args`,
            // eliminating fully-applied lambda parameters. Replaces the bare
            // `walk (betaReduce …)` the local/external inline call sites used:
            //   1. peel the inline's lambdas, pairing each parameter with its arg;
            //   2. a lambda-valued parameter every use of which is a saturated
            //      application head is registered in `lambdaEnv` (the walker
            //      splices it away) — its closure vanishes;
            //   3. every other parameter (a value arg, or a lambda that is stored /
            //      partially applied) is re-bound with an ordinary `Let`, exactly
            //      as before — a surviving closure.
            let reduceApplication
                (walk: TExpr -> TExpr)
                (paramAttrs: ParamAttrs[])
                (expanded: TExpr)
                (args: (TExpr * SemType * SyntaxToken) list)
                : TExpr =
                // Carry each application node's `tok` alongside the binder so the
                // surviving `Let` is anchored at the call site it lowers.
                let rec peel
                    (fn: TExpr)
                    (args: (TExpr * SemType * SyntaxToken) list)
                    (acc: (NodeKey * SemType * TExpr * SyntaxToken) list)
                    =
                    match fn, args with
                    | _, [] -> List.rev acc, fn
                    | TExpr.Lambda(TPat.NamedSimple(k, paramTy, _), body, _, _), (arg, _, appTok) :: rest ->
                        peel body rest ((k, paramTy, arg, appTok) :: acc)
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
                let externalValParams =
                    bindings
                    |> List.choose (fun (k, _, arg, _) ->
                        match arg with
                        | TExpr.External _ -> Some(k, arg)
                        | _ -> None
                    )

                let externalKeys = HashSet<NodeKey>(externalValParams |> List.map fst)

                let core =
                    externalValParams |> List.fold (fun body (k, v) -> substituteVar k v body) core

                let candidates = Dictionary<NodeKey, TExpr>()

                for (k, _, arg, _) in bindings do
                    match arg with
                    | TExpr.Lambda _ -> candidates.[k] <- arg
                    | _ -> ()

                let bad = nonInlinableLambdaParams candidates core

                let inlinable = HashSet<NodeKey>()

                for kv in candidates do
                    if not (bad.Contains kv.Key) then
                        inlinable.Add kv.Key |> ignore
                        lambdaEnv.[kv.Key] <- kv.Value

                let core' = walk core

                for k in inlinable do
                    lambdaEnv.Remove k |> ignore

                // Re-bind the parameters not inlined away, innermost last so the
                // `Let` nesting matches `betaReduce`'s left-to-right order. The arg
                // is walked here (its own inline heads expand); an inlined arg is
                // dropped — the walker spliced + walked a fresh copy at each use.
                //
                // TODO(byref-capture half): a lambda arg
                // that lands here (NOT inlined — stored or partially applied) and
                // captures a byref-like value (`Span`, `ReadOnlySpan`, any `ref
                // struct`) is a real heap closure that cannot legally hold it.
                // Today it compiles to a heap closure regardless (we have no
                // byref-like detection — `SemType` has no ref-struct case and
                // metadata drops byref params, see Inline.isStructType). The full
                // path forks here: (1) emit it as a ref-struct closure
                // (`Fun`-as-`ref struct`, the designed-for escape hatch) so the
                // capture is legal, or (3) reject it like F# when it genuinely
                // escapes (`HeapShared` per Regions). Either makes a currently
                // (would-be) rejected program compile or fail cleanly; both need
                // the byref-like predicate that does not exist yet.
                // Pair each binding with its declaration position so the
                // `[<CallAtMostOnce>]` flag (positionally aligned to the inline's
                // curried parameters; freshen / typar-substitution preserve order)
                // can gate it. `peel` returns parameters outermost-first, so `i` is
                // the curried position.
                let indexed =
                    bindings |> List.mapi (fun i (k, ty, a, appTok) -> (i, k, ty, a, appTok))

                List.foldBack
                    (fun (i, k, paramTy, arg, appTok) acc ->
                        // `inlinable` lambda params and the bare-`External`-value
                        // params (substituted into `core` above) carry no surviving
                        // `let` binding.
                        if inlinable.Contains k || externalKeys.Contains k then
                            acc
                        else
                            let warg = walk arg

                            // A `[<CallAtMostOnce>]` parameter is substituted at its
                            // single (declaration-validated linear) use instead of an
                            // eager `let`, so the argument is evaluated at most once
                            // and on demand — the mechanism behind `&&`/`||`
                            // short-circuiting, now driven by the declared attribute
                            // rather than a body-shape guess. Every other parameter
                            // keeps the eager `let` (F#-strict evaluation order,
                            // single-evaluation, and closure capture undisturbed).
                            let callAtMostOnce = i < paramAttrs.Length && paramAttrs.[i].CallAtMostOnce

                            if callAtMostOnce then
                                substituteVar k warg acc
                            else
                                TExpr.Let(TPat.NamedSimple(k, paramTy, appTok), warg, acc, TastWalk.exprTy acc, appTok)
                    )
                    indexed
                    core'

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
                                let head, spineArgs = TastWalk.collectSpine [] e

                                match head with
                                | TExpr.Var(k, _, headTok) when localInlines.ContainsKey k ->
                                    ValueSome(
                                        reduceApplication
                                            walk
                                            (localParamAttrs k)
                                            (expandAt headTok localInlines.[k] spineArgs)
                                            spineArgs
                                    )
                                // A saturated use of an inline-first lambda
                                // parameter: splice a fresh
                                // copy of its bound lambda, beta-reduced against the
                                // call args, and walk it (nested inline heads /
                                // further lambda params resolve in the recursion).
                                // `nonInlinableLambdaParams` guaranteed every use is
                                // saturated, so `betaReduce` consumes exactly the
                                // lambda's arity — no surviving closure.
                                | TExpr.Var(k, _, _) when lambdaEnv.ContainsKey k ->
                                    ValueSome(walk (betaReduce (Inline.freshen mint lambdaEnv.[k]) spineArgs))
                                | TExpr.External(_, keyOpt, _, headTok) ->
                                    match lookupExternal keyOpt with
                                    // An external WITH an inline body ALWAYS splices —
                                    // no operand-groundness gate. An un-ground `^T`
                                    // simply selects no per-primitive
                                    // `StaticOptimization` clause and falls to the
                                    // body's BASE, which is where the safe generic
                                    // default lives (`EqualityComparer<^T>.Default.Equals`
                                    // for `=`). Declining to splice instead routed the
                                    // head to a name-keyed raw-IL fallback, turning a
                                    // structural `=` into a reference `ceq`.
                                    | ValueSome ib ->
                                        ValueSome(
                                            reduceApplication
                                                walk
                                                ib.ParamAttrs
                                                (expandAt headTok ib.Decl spineArgs)
                                                spineArgs
                                        )
                                    // An external with no inline body (a real
                                    // cross-package call): keep the head, lower the
                                    // args — exactly codegen's `head'` rule, left for
                                    // its recipe path.
                                    | _ ->
                                        ValueSome(
                                            TastWalk.rebuildApp head [ for (a, t, tok) in spineArgs -> walk a, t, tok ]
                                        )
                                // A dotted member call on an external type — the same
                                // head `x.get_Item(2)` / `w.Poke 41` lowers to. The
                                // member-keyed inline store forks call-vs-splice here:
                                //   * `ValueSome ib` — a concrete `(# … #)`-bodied member
                                //     (harvested `this`-first). SPLICE it. The receiver is
                                //     a FIELD of the head, not a spine arg, so PREPEND it
                                //     onto the spine (`this`→receiver); a STATIC member
                                //     (`receiver = ValueNone`) prepends nothing. Then
                                //     splice via the SAME path the `External` arm uses —
                                //     `expandExternalAt` / `reduceApplication` consume the
                                //     spine POSITIONALLY, so with `this` at curried
                                //     position 0 each `pi` aligns to `argi`.
                                //   * `ValueNone` — a real CLR/JS method with no inline
                                //     body: keep the call, walking the receiver (inside the
                                //     head) and the args, exactly the `_` catch-all rule.
                                | TExpr.ExternalMember(receiver, key, _, _, _, memberTok) ->
                                    match lookupExternal (ValueSome key) with
                                    | ValueSome ib ->
                                        let fullSpine =
                                            match receiver with
                                            | ValueSome r -> (r, TastWalk.exprTy r, memberTok) :: spineArgs
                                            | ValueNone -> spineArgs

                                        ValueSome(
                                            reduceApplication
                                                walk
                                                ib.ParamAttrs
                                                (expandAt memberTok ib.Decl fullSpine)
                                                fullSpine
                                        )
                                    | ValueNone ->
                                        ValueSome(
                                            TastWalk.rebuildApp
                                                (walk head)
                                                [ for (a, t, tok) in spineArgs -> walk a, t, tok ]
                                        )
                                // A non-external, non-local-inline head (e.g. a
                                // higher-order parameter): lower the head and args,
                                // keeping the spine intact.
                                | _ ->
                                    ValueSome(
                                        TastWalk.rebuildApp
                                            (walk head)
                                            [ for (a, t, tok) in spineArgs -> walk a, t, tok ]
                                    )
                            // A BARE (non-applied) reference to a LOCAL inline — the
                            // template used as a value. No spine, so no type argument is
                            // derivable and the body's typars stay abstract; it still
                            // goes through `expandAt` so its static-opt clauses resolve
                            // and any trait call it cannot dispatch is REPORTED rather
                            // than handed to a backend that has no arm for it.
                            | TExpr.Var(k, _, tok) when localInlines.ContainsKey k ->
                                ValueSome(walk (expandAt tok localInlines.[k] []))
                            // A BARE (non-applied) reference to a cross-package `let`
                            // value whose body is a single zero-operand intrinsic
                            // (`undefined`, `defaultof`): splice the intrinsic body in place of
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
                                let body = lookupExternal keyOpt

                                match body with
                                | ValueSome ib ->
                                    match Inline.nullaryIntrinsicValueBody ib.Decl with
                                    | ValueSome(TExpr.ILIntrinsic(op, operand, args, _, intrinsicTok)) ->
                                        let groundedOperand =
                                            match operand with
                                            | ValueSome _ -> ValueSome refTy
                                            | ValueNone -> ValueNone

                                        ValueSome(TExpr.ILIntrinsic(op, groundedOperand, args, refTy, intrinsicTok))
                                    | _ -> etaReify body name keyOpt refTy tok |> ValueOption.map walk
                                | ValueNone -> etaReify body name keyOpt refTy tok |> ValueOption.map walk
                            | _ -> ValueNone
                }

            let walkExpr (e: TExpr) : TExpr = TastWalk.mapExpr mapper e

            // Expand the inlines embedded in every expression a type declaration
            // carries: member bodies, the class preambles (`[static] let` initialisers and
            // `[static] do` bodies), secondary-ctor `let`s + chain args, and the
            // `inherit Base(args)` arguments. Mirrors `Elaborate.freezeKind`'s
            // expr-bearing coverage, relocating codegen's
            // `EmitLower.spliceExternalInlinesInExpr` splice (`NominalEmit`'s three
            // sites) out of emission. `walkExpr` also covers local inlines a member
            // body might call — a superset of the external-only codegen splice —
            // but a local-inline reference in a member body would otherwise dangle
            // (codegen drops local inline templates), so this only ever turns a
            // would-be error into a correct expansion; existing green corpora carry
            // none, so output is byte-identical.
            let walkMember (m: TTypeMember) : TTypeMember = { m with Body = walkExpr m.Body }

            let walkPreambleEntry (entry: TPreambleEntry) : TPreambleEntry =
                match entry with
                | TPreambleEntry.Let l -> TPreambleEntry.Let { l with Init = walkExpr l.Init }
                | TPreambleEntry.Do e -> TPreambleEntry.Do(walkExpr e)

            let walkKind (k: TTypeKind) : TTypeKind =
                match k with
                | TTypeKind.Interface _ -> k
                // An enum has no member bodies to walk.
                | TTypeKind.Enum _ -> k
                | TTypeKind.Union(cases, members, interfaces) ->
                    TTypeKind.Union(
                        cases,
                        members |> EqArray.map walkMember,
                        interfaces |> EqArray.map (fun (ity, ms) -> ity, ms |> EqArray.map walkMember)
                    )
                | TTypeKind.Record(fields, members, interfaces) ->
                    TTypeKind.Record(
                        fields,
                        members |> EqArray.map walkMember,
                        interfaces |> EqArray.map (fun (ity, ms) -> ity, ms |> EqArray.map walkMember)
                    )
                | TTypeKind.Class c ->
                    TTypeKind.Class
                        { c with
                            Members = c.Members |> EqArray.map walkMember
                            Interfaces =
                                c.Interfaces |> EqArray.map (fun (ity, ms) -> ity, ms |> EqArray.map walkMember)
                            StaticPreamble = c.StaticPreamble |> EqArray.map walkPreambleEntry
                            InstancePreamble = c.InstancePreamble |> EqArray.map walkPreambleEntry
                            SecondaryCtors =
                                c.SecondaryCtors
                                |> EqArray.map (fun sc ->
                                    { sc with
                                        Lets = sc.Lets |> EqArray.map (fun cl -> { cl with Init = walkExpr cl.Init })
                                        PrimaryArgs = sc.PrimaryArgs |> EqArray.map walkExpr
                                        FieldInits =
                                            sc.FieldInits
                                            |> EqArray.map (fun fi -> { fi with Init = walkExpr fi.Init })
                                    }
                                )
                            BaseCtorCall =
                                c.BaseCtorCall
                                |> ValueOption.map (fun bc ->
                                    { bc with
                                        Args = bc.Args |> EqArray.map walkExpr
                                    }
                                )
                        }

            decls
            |> List.map (fun (d, env) ->
                let d' =
                    match d with
                    // Inline templates are left untouched (codegen drops them;
                    // `collectInlineBodies` extracts them raw).
                    | TDecl.Let(_, _, true, _) -> d
                    | TDecl.Let(p, value, false, ty) -> TDecl.Let(p, walkExpr value, false, ty)
                    | TDecl.Expression(e, ty) -> TDecl.Expression(walkExpr e, ty)
                    | TDecl.Type td -> TDecl.Type { td with Kind = walkKind td.Kind }

                d', env
            )
