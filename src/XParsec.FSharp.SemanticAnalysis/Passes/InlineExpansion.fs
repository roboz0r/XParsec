namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// The pre-freeze inline-expansion pass. Runs
// between `Freeze.elaborate` and `Freeze.freezeTypars`, on the still
// `TyVar`-carrying `TExpr` tree, where `zonk` / union-find are native. It
// relocates module-level `let inline` expansion out of codegen
// (`EmitLower.lowerWith`'s inline branches): a saturated use of a local
// `let inline` — or of a cross-package `val inline` whose body the provider
// serves (`IExternalSymbolProvider.TryLookupInlineBody`) — is expanded + beta-reduced + static-opt
// resolved here, so the frozen module decls reaching codegen carry no inline
// call heads and no `StaticOptimization` nodes.
//
// Scope (beat (b)): module-level decls (`TDecl.Let` non-inline values and
// `TDecl.Expression`) AND every expression a `TDecl.Type` carries (member
// bodies, `static let` inits, secondary-ctor `let`s + chain args, base-ctor
// args). It deliberately does NOT touch:
//   * inline TEMPLATES (`TDecl.Let(isInline = true)`) — codegen still drops
//     them and `SymbolProviders.collectInlineBodies` extracts them raw, so the
//     cross-package template-extraction path stays byte-identical;
//   * eta-reification of `External` function VALUES — left to codegen;
//   * `expandBuiltinOps` (operator → inline IL) — left to codegen, where it
//     must run after eta anyway.
// Beat (a) left type-member bodies to codegen's `NominalEmit` splice; beat (b)
// relocates them here and deletes the codegen inline-expansion machinery
// (`EmitLower.lowerWith`'s inline branches + `spliceExternalInlinesInExpr`),
// keeping only eta + `expandBuiltinOps`.
//
// 3A-3 (the inline-first soundness condition, beta-reduction half): a lambda
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

    /// A (zonked) `SemType` with no free `TyVar` anywhere — fully monomorphic, so
    /// codegen can encode it. The `SemType` sibling of `FrozenTypeBridge.ftIsGround`
    /// (this one zonks; the frozen one has no vars to zonk): the cross-package
    /// equality / `hash` inline bodies reach `EqualityComparer<^T>`, emittable
    /// only when `^T` is ground; an unpinned operand leaves it free and must fall
    /// back to codegen's `BuiltinOps`.
    let rec private isGroundType (t: SemType) : bool =
        match Unification.zonk t with
        | TyVar _
        | TyUnknown _
        | TyTypar _ -> false
        | t -> SemType.forallChildren isGroundType t

    /// Whether a derived inline type argument is concrete enough to splice a saturated
    /// builtin operator. A ground type qualifies; so does a *nominal-headed* type
    /// (`Set<'T>`) even with abstract element typars — its head constructor pins the
    /// `when ^T : ^T` static-opt clause to the type's own static operator member, which
    /// codegen emits generic in the residual typars. A bare typar / TyVar (a truly
    /// unpinned `let f a b = a + b`) does NOT qualify and falls to `expandBuiltinOps`.
    let private isSpliceableOperatorArg (t: SemType) : bool =
        // `Inline.isNominalType` is the one nominal-head predicate (class / union /
        // record) — shared with the static-opt clause gate / `TraitCall` resolution
        // so they never disagree on what counts as a nominal operand.
        isGroundType t || Inline.isNominalType t

    /// Recover an inline binding's type arguments at a call site by matching its
    /// declared parameter (and return) types — carrying the quantified typars —
    /// against the actual spine-arg types. Tolerant: a typar the params don't pin
    /// is left as its own `TyVar` so the catch-all `when ^T : ^T` clause still
    /// selects. Returned in `Inline.quantifiedTypars` order. A verbatim port of
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
                        // non-ground candidate to a ground one. A chained `a + b + c`
                        // matches `^T` first against the inner `(+) a b`-App, whose
                        // recorded result type is still an abstract `TyVar` at this
                        // pre-freeze pass; the concrete sibling `c : string` (and the
                        // application's `string` return position) must be allowed to
                        // win, or `^T` stays abstract, the operator fails the splice
                        // gate (`isSpliceableOperatorArg`), and falls to its numeric
                        // `add` base — emitting `add` on two string references (an
                        // AccessViolation at runtime). Keeping the first ground match
                        // is intentional: a genuinely generic `let f a b = a + b`
                        // never sees a ground candidate, so `^T` stays abstract.
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

            let rec peelParams n t =
                if n <= 0 then
                    []
                else
                    match Unification.zonk t with
                    | TyFun(a, b) -> a :: peelParams (n - 1) b
                    | _ -> []

            let rec pairGo ps acts =
                match ps, acts with
                | p :: ps', a :: acts' ->
                    go p a
                    pairGo ps' acts'
                | _ -> ()

            let nArgs = List.length spineArgs

            pairGo (peelParams nArgs declTy) [ for (a, _, _) in spineArgs -> TastWalk.exprTy a ]

            // Pair the result position too: `failwith`'s only typar `'T` sits in
            // the *return* (`string -> 'T`), so the param walk leaves it unbound.
            // The last spine arg's recorded type is the whole application's result
            // (`collectSpine` pairs each arg with its `App` node's result), so
            // unifying it against `declTy`'s return position grounds the result
            // typars.
            let rec returnAfter n t =
                if n <= 0 then
                    t
                else
                    match Unification.zonk t with
                    | TyFun(_, b) -> returnAfter (n - 1) b
                    | _ -> t

            if nArgs > 0 then
                let declRetTy = returnAfter nArgs declTy
                let _, actualRetTy, _ = spineArgs |> List.last
                go declRetTy actualRetTy

            Array.mapi
                (fun i v ->
                    match v with
                    | ValueSome t -> t
                    | ValueNone -> TyVar roots.[i]
                )
                result

    /// Built-in operator compiled name → arity — the saturation gate codegen
    /// applies (`EmitLower.BuiltinOps.isSaturated`). A saturated built-in
    /// operator (`op_Equality`, …) whose inline body the provider serves is
    /// expanded ONLY when its operands are ground; otherwise the head is left for
    /// codegen's `BuiltinOps` `ceq`/`add`/… fallback. Mirror the codegen table
    /// exactly; keep in sync.
    let private builtinOpArity: Map<string, int> =
        Map
            [
                "op_Equality", 2
                "op_Inequality", 2
                "op_LessThan", 2
                "op_GreaterThan", 2
                "op_LessThanOrEqual", 2
                "op_GreaterThanOrEqual", 2
                "op_Addition", 2
                "op_Subtraction", 2
                "op_Multiply", 2
                "op_Division", 2
                "op_Modulus", 2
                "op_UnaryNegation", 1
                "op_BitwiseAnd", 2
                "op_BitwiseOr", 2
                "op_ExclusiveOr", 2
                "op_LeftShift", 2
                "op_RightShift", 2
                "op_LogicalNot", 1
            ]

    let private isSaturatedBuiltin (name: string) (spineLen: int) : bool =
        match Map.tryFind name builtinOpArity with
        | Some arity -> spineLen = arity
        | None -> false

    /// Expand the module-level inlines in one decl-list (the elaborated,
    /// `TyVar`-carrying decls paired with their freeze envs). The cross-package
    /// inline-body channel is `provider` itself (`TryLookupInlineBody`, keyed by the
    /// resolved `SymbolKey`, a member of `IExternalSymbolProvider`); a front-end-only provider serves
    /// none and every lookup returns `ValueNone`, so the walk is an identity
    /// rebuild — which `Freeze.freezeTypars` does to every decl immediately after
    /// regardless, so there is no node-identity to preserve by skipping it.
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
            // Build-wide monotone counter for freshened inline binders, in the
            // dedicated `SynthPreFreezeInline` space so a baked key can never
            // collide with the `SynthInlineExpansion` keys codegen's still-live
            // eta-expansion mints.
            let mutable counter = 0

            let mint () =
                let k = NodeKey.ofSynthetic counter NodeKind.SynthPreFreezeInline
                counter <- counter + 1
                k

            let lookupExternal (keyOpt: SymbolKey voption) : InlineBody voption =
                // Splice a cross-package `let inline` body by its resolved
                // `SymbolKey` — the sole channel. Every splice-eligible `External`
                // head is key-stamped upstream: value refs by NameResolution
                // (`ExternalValue`), operator / synthesised-intrinsic heads by
                // `Freeze` (`Resolution.IntrinsicKey`), and intra-body sibling refs by
                // `collectInlineBodies`' rewrite. A `key = ValueNone` head therefore
                // carries no inline body by construction — a saturated builtin
                // operator codegen emits directly, `Array.ofList` / ctor-as-value
                // handled by codegen recipes / eta-expansion — so ValueNone here is a
                // genuine "no body", never a missed keyless splice. A provider with no
                // inline bodies returns `ValueNone`.
                match keyOpt with
                | ValueSome key -> provider.TryLookupInlineBody key
                | ValueNone -> ValueNone

            let expandLocalAt (k: NodeKey) (spineArgs: (TExpr * SemType * SyntaxToken) list) : TExpr =
                let decl = localInlines.[k]

                match decl with
                // Derive the call-site type arguments and substitute the body's
                // quantified typars — needed both to resolve a `StaticOptimization`
                // clause AND, for any generic local inline, to GROUND the body's
                // typars to the caller's types. Without it, a typar reachable only
                // through the body (e.g. `asNode`'s `value :?> SetTreeNode<'T>`
                // result, or `isEmpty`'s `isNull` typar) stays a free `TyVar` root of
                // the *callee's* scheme: beta-reduction binds the value params but
                // never unifies that typar, so it pollutes the caller's frozen TAST
                // as a `ResolvedTypes` "unresolved TyVar". The external path
                // (`expandExternalAt`) already always derives — this is the local
                // twin of that.
                | TDecl.Let(_, _, _, declTy) ->
                    Inline.inlineExpand decl (deriveInlineTypeArgs declTy spineArgs)
                    |> Inline.freshen mint
                | _ -> Inline.inlineExpand decl [||] |> Inline.freshen mint

            let expandLocal (k: NodeKey) : TExpr =
                Inline.inlineExpand localInlines.[k] [||] |> Inline.freshen mint

            let expandExternalAt (decl: TDecl) (spineArgs: (TExpr * SemType * SyntaxToken) list) : TExpr =
                match decl with
                | TDecl.Let(_, _, _, declTy) ->
                    Inline.inlineExpand decl (deriveInlineTypeArgs declTy spineArgs)
                    |> Inline.freshen mint
                | _ -> failwith "InlineExpansion: external inline body must be a TDecl.Let"

            let externalArgsGround (decl: TDecl) (spineArgs: (TExpr * SemType * SyntaxToken) list) : bool =
                match decl with
                | TDecl.Let(_, _, _, declTy) ->
                    deriveInlineTypeArgs declTy spineArgs |> Array.forall isSpliceableOperatorArg
                | _ -> false

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
                                | TExpr.Var(k, _, _) when localInlines.ContainsKey k ->
                                    ValueSome(
                                        reduceApplication walk (localParamAttrs k) (expandLocalAt k spineArgs) spineArgs
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
                                | TExpr.External(name, keyOpt, _, _) ->
                                    match lookupExternal keyOpt with
                                    | ValueSome ib when
                                        externalArgsGround ib.Decl spineArgs
                                        || not (isSaturatedBuiltin name (List.length spineArgs))
                                        ->
                                        ValueSome(
                                            reduceApplication
                                                walk
                                                ib.ParamAttrs
                                                (expandExternalAt ib.Decl spineArgs)
                                                spineArgs
                                        )
                                    // An external head we don't expand (a saturated
                                    // builtin op with un-ground operands, or a
                                    // non-inline external call): keep the head,
                                    // lower the args — exactly codegen's `head'`
                                    // rule. Left for codegen's `BuiltinOps` /
                                    // recipe path.
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
                                    match provider.TryLookupInlineBody key with
                                    | ValueSome ib ->
                                        let fullSpine =
                                            match receiver with
                                            | ValueSome r -> (r, TastWalk.exprTy r, memberTok) :: spineArgs
                                            | ValueNone -> spineArgs

                                        ValueSome(
                                            reduceApplication
                                                walk
                                                ib.ParamAttrs
                                                (expandExternalAt ib.Decl fullSpine)
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
                            | TExpr.Var(k, _, _) when localInlines.ContainsKey k -> ValueSome(walk (expandLocal k))
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
                            | TExpr.External(_, keyOpt, refTy, _) ->
                                match lookupExternal keyOpt with
                                | ValueSome ib ->
                                    match Inline.nullaryIntrinsicValueBody ib.Decl with
                                    | ValueSome(TExpr.ILIntrinsic(op, operand, args, _, tok)) ->
                                        let groundedOperand =
                                            match operand with
                                            | ValueSome _ -> ValueSome refTy
                                            | ValueNone -> ValueNone

                                        ValueSome(TExpr.ILIntrinsic(op, groundedOperand, args, refTy, tok))
                                    | other -> other
                                | ValueNone -> ValueNone
                            | _ -> ValueNone
                }

            let walkExpr (e: TExpr) : TExpr = TastWalk.mapExpr mapper e

            // Expand the inlines embedded in every expression a type declaration
            // carries: member bodies, `static let`
            // initialisers, secondary-ctor `let`s + chain args, and the
            // `inherit Base(args)` arguments. Mirrors `Freeze.freezeKind`'s
            // expr-bearing coverage, relocating codegen's
            // `EmitLower.spliceExternalInlinesInExpr` splice (`NominalEmit`'s three
            // sites) out of emission. `walkExpr` also covers local inlines a member
            // body might call — a superset of the external-only codegen splice —
            // but a local-inline reference in a member body would otherwise dangle
            // (codegen drops local inline templates), so this only ever turns a
            // would-be error into a correct expansion; existing green corpora carry
            // none, so output is byte-identical.
            let walkMember (m: TTypeMember) : TTypeMember = { m with Body = walkExpr m.Body }

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
                            StaticLets = c.StaticLets |> EqArray.map (fun sl -> { sl with Init = walkExpr sl.Init })
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
