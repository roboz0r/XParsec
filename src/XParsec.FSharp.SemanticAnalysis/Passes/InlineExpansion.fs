namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
// The table this pass builds, and the shape of one reduction as the table consumes it.
open InlineSpecTable

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
// The edges SURVIVE the pass, and the freeze: placing the bodies is the backends' shared
// `Codegen.Common.InlineExpand.expand`, which splices them as it emits and keeps a frame
// chain naming the file each spliced node was written in. That is what deferral is for — a
// body flattened here has one anchor domain (the call site's) and nothing to say where it
// came from. What this pass owns is RESOLUTION, and what it hands out is the table.
//
// The table itself — its slots, its reuse pool and its assertions — is `InlineSpecTable`; the
// `SemType`/`TExpr` questions a resolution asks are `Inline`'s. What is left here is the WALK:
// which node is a call site, which chain it was written under, and where its body goes.
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
// `Inline.nonInlinableLambdaParams` + the `lambdaEnv` splice). A lambda that is stored
// or partially applied survives as a real closure, exactly as before. The
// byref-like-capture half (reject / ref-struct closures for a SURVIVING closure
// that holds a `Span`/`ref struct`) is deferred — see the TODO in
// `classifyApplication`; it needs a byref-like predicate that does not exist yet.

module InlineExpansion =

    /// Where a reduction's body will LIVE — which is the whole of what decides whether the
    /// call-site material it FUSES in changes anchor domain, and so whether that material
    /// needs a `TExpr.CallerExpr` around it.
    ///
    /// Decided from the served body alone (does the provider retain a producer file?), before
    /// any parameter is classified, so one reduction cannot mark half its fusions.
    ///
    /// The retained file rides ON the outlined case rather than travelling beside it: it is
    /// the entry's anchor domain and the sole reason there is an entry at all, so there is no
    /// spliced reduction an origin could be paired with and no second place to read the fork.
    [<RequireQualifiedAccess>]
    type private Placement =
        /// Abstracted into a specialization entry, whose nodes stay anchored in the producer
        /// file its `OriginFile` names. Descending the edge PUSHES that file, so fused
        /// call-site material is one frame out and pops back.
        | Outlined of origin: OriginFile
        /// Moved onto the call site, so the body and the material fused into it are already
        /// one domain. Nothing pushed a frame, so nothing may pop one.
        | Spliced

    [<RequireQualifiedAccess>]
    module private Placement =

        /// THE reading of a served body's retained producer file, taken once so that every
        /// later decision branches on the placement itself. `ValueNone` is not a missing field
        /// but the other reading of a served body: no retained file means no domain an entry
        /// could name, and the thaw has already moved such a body onto the call site.
        let ofOrigin (origin: OriginFile voption) : Placement =
            match origin with
            | ValueSome file -> Placement.Outlined file
            | ValueNone -> Placement.Spliced

        /// A fused call-site argument as it must appear INSIDE the body it is fused into.
        /// Applied UNCONDITIONALLY at an outlined site, a trivial argument included: a uniform
        /// invariant is checkable where one that skips `Var`s and constants is not.
        let fuse (placement: Placement) (arg: TExpr) : TExpr =
            match placement with
            | Placement.Outlined _ -> Inline.callerExpr arg
            | Placement.Spliced -> arg

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

    /// WHICH inline binding a reduction is expanding — the identity a RECURSION is detected on.
    ///
    /// The TEMPLATE alone and not its `Grounding`: a binding that reaches itself at a different
    /// grounding is no less non-terminating (a polymorphically recursive one mints a fresh
    /// grounding every time round), so keying on the grounding would let exactly the divergence
    /// this exists to stop straight through.
    ///
    /// Two cases because a SAME-UNIT template has no `SymbolKey` at all — `Freeze.publishable`
    /// refuses to publish a top-level `let inline` — so its binder is the only identity it has.
    [<RequireQualifiedAccess>]
    type private TemplateId =
        | Local of binder: NodeKey
        | Foreign of key: SymbolKey

    /// One inline reduction IN FLIGHT: a binding whose body is currently being expanded.
    ///
    /// The CHAIN of these is what makes a recursive inline TERMINATE. A call that reaches a
    /// binding already on the chain has no expansion — substituting the body in would present
    /// the same call again — so it is answered from the frame rather than expanded a second time.
    ///
    /// The chain is a list, INNERMOST FIRST, threaded through the walk as a parameter rather
    /// than held anywhere: pushing a frame is a cons and popping one is a tail, so a piece of
    /// material is walked under the chain it was WRITTEN under by construction and no reader has
    /// to state a depth. The one thing the walk mutates is `Slot`, and only on a frame it
    /// already holds.
    [<NoEquality; NoComparison>]
    type private ExpansionFrame =
        {
            Template: TemplateId
            /// How a diagnostic spells the binding.
            Name: string
            /// The call site as written in the file being compiled. On the OUTERMOST frame it is
            /// the only position a verdict about this expansion can carry: every deeper site is a
            /// node of a producer's body, whose anchor indexes a file this compilation is not
            /// reporting against.
            Site: SyntaxToken
            /// The table slot this expansion reserved, once it has one. A re-entrant call becomes
            /// a back EDGE to it, which is what leaves the table finite and CYCLIC — a thing
            /// `Inline.findCycle` can reject — instead of minting entries until the stack goes.
            ///
            /// `ValueNone` for a SPLICED reduction: it has no entry, so there is nothing for an
            /// edge to name and the recursive call is left unexpanded instead.
            mutable Slot: SpecializationId voption
        }

    /// A FRESH reduction and the two chains its material belongs to: `Own` is the frame this
    /// reduction pushes, `Caller` the chain — innermost first — the CALL SITE's material was
    /// written under.
    ///
    /// Two fields and not one list because a reduction walks two kinds of material against two
    /// different chains: the body walks under `Own :: Caller`, an argument the site supplied
    /// walks under `Caller` alone. An argument is the caller's, not the callee's, so a call it
    /// makes to the binding it is an argument OF is an ordinary nested application and not a
    /// recursion: `1 - 2 - 3` applies `(-)` inside `(-)`'s own left operand without either
    /// expansion containing the other, and walking it under the callee's chain would convict
    /// every such program.
    [<NoEquality; NoComparison>]
    type private InFlight =
        {
            Own: ExpansionFrame
            Caller: ExpansionFrame list
        }

    [<RequireQualifiedAccess>]
    module private InFlight =

        /// The chain this reduction's own BODY is walked under, innermost first.
        let frames (f: InFlight) : ExpansionFrame list = f.Own :: f.Caller

        /// The outermost frame — see `Recursion.Site` for why that is the one a verdict reads.
        let outermost (f: InFlight) : ExpansionFrame =
            match f.Caller with
            | [] -> f.Own
            | caller -> List.last caller

    /// The application an expansion is being entered FOR, in the two forms the RECURSIVE answer
    /// needs it — which are NOT the same form, and conflating them is how a back edge comes to
    /// disagree with the entry it names.
    ///
    /// An answered call is either an EDGE into the entry the re-entered expansion reserved, or
    /// the call left exactly as written. The edge is positional against that entry's parameters,
    /// so it takes the spine the FRESH reduction would have peeled — for an `ExternalMember`
    /// that includes the receiver, at curried position 0. The un-expanded rebuild is the
    /// original application, where the receiver belongs inside the head it was written in and no
    /// spine ever mentioned it.
    [<NoEquality; NoComparison>]
    type private PendingCall =
        {
            /// The position the expansion stands in for. A rewrite inherits the position of the
            /// node it REPLACES, so for an application that is the APPLICATION node's own token
            /// and never the head's — the two differ exactly when an outer fusion substituted
            /// the head in, which makes the head call-site material sitting inside a producer's
            /// own application. It is also the site the fresh frame records.
            Tok: SyntaxToken
            /// The node's own result type, which the outermost `App` already records
            /// (`collectSpine` pairs each argument with its node's result) and a bare reference
            /// carries directly.
            Ty: SemType
            /// The spine AS APPLIED — what the entry's parameters were peeled against, so an
            /// edge's arguments are positional against them by construction. Unwalked: only the
            /// edge answer walks them, and only the arguments it actually carries.
            Spine: (TExpr * SemType * SyntaxToken) list
            /// Walks one of `Spine`. Those are the CALLER's own material and are expanded on
            /// their own merits however the call is answered.
            Walk: TExpr -> TExpr
            /// What the call becomes when there is NO entry to name — the arm's own "this head
            /// carries no inline body" rebuild, which walks whatever the head holds and leaves
            /// the application as written. A thunk, forced only on that answer, so no material
            /// is walked for an answer that discards it.
            Rebuild: unit -> TExpr
        }

    /// A lambda argument eligible for inline-first elimination, with the chain it was WRITTEN
    /// under.
    ///
    /// The chain travels with it because the lambda is spliced at a use INSIDE the body it was
    /// passed to: what it computes is still the caller's material, so a call it makes to the very
    /// binding it was passed to is an ordinary nested application and not a recursion. It is the
    /// chain itself and not a position in one — the splice site is an arbitrary number of bodies
    /// deeper than the capture, so any index taken here would be read against a chain the lambda
    /// never saw.
    [<NoEquality; NoComparison>]
    type private FusedLambda =
        {
            Body: TExpr
            CallerFrames: ExpansionFrame list
        }

    [<RequireQualifiedAccess>]
    module private Peeled =

        /// No call-site material was fused into the body, so the body is CLOSED over its
        /// parameters and nothing in it belongs to this site. That is exactly the condition
        /// under which two sites may share one specialization entry — and the condition under
        /// which an entry's nodes all come from the file its `OriginFile` names.
        let isClosed (p: Peeled) : bool =
            p.Params |> List.forall (fun x -> x.Disposition = Disposition.Survive)

    /// Apply `f` to every expression a declaration carries: a module binding's value, a `do`
    /// expression, and — through `TastWalk.mapTypeDecl`, at the type axis `id` — everything a
    /// type declaration holds. That covers member bodies, the class preambles (`[static] let`
    /// initialisers and `[static] do` bodies), secondary-ctor `let`s + chain args, and the
    /// `inherit Base(args)` arguments, and it covers them because the DECLARATION SHAPE says
    /// so, not because a traversal here remembered to. This relocates codegen's
    /// `EmitLower.spliceExternalInlinesInExpr` splice (`NominalEmit`'s three sites) out of
    /// emission. The expansion walk also covers local inlines a member body might call — a
    /// superset of the external-only codegen splice — but a local-inline reference in a member
    /// body would otherwise dangle (codegen drops local inline templates), so this only ever
    /// turns a would-be error into a correct expansion; existing green corpora carry none, so
    /// output is byte-identical.
    ///
    /// Parameterised over `f` because the pass makes TWO passes over this same coverage — the
    /// expansion walk, then the collection of the roots the finished table is checked against —
    /// and they must not be able to disagree about which expressions exist.
    let private mapDeclExprs (f: TExpr -> TExpr) (d: TDecl) : TDecl =
        match d with
        | TDecl.Let(p, value, isInline, ty) -> TDecl.Let(p, f value, isInline, ty)
        | TDecl.Expression(e, ty) -> TDecl.Expression(f e, ty)
        | TDecl.Type td -> TDecl.Type(TastWalk.mapTypeDecl id f td)

    /// Expand ONE inline binding for one use site: derive the site's type arguments from the
    /// spine, substitute them through the body (which also selects its `StaticOptimization`
    /// clause and dispatches its `TraitCall`s), freshen the binders, and report every trait
    /// call the substitution could NOT dispatch.
    ///
    /// The SINGLE resolution entry — local and external, applied and bare, spliced and
    /// outlined — so no path can carry a body forward while quietly leaving an unresolvable
    /// `TraitCall` in it. That matters because neither backend has a `TraitCall` arm: an
    /// unreported one is an emitter crash, where a reported one is "the type 'decimal' does not
    /// support the operator '+'" and stops the compile before codegen. Reporting belongs here,
    /// not in `Inline`: the expander is `PassContext`-free, and it sees the body before it
    /// acquires a position — so the call site is the only anchor available to it. It reports at
    /// EVERY site, including one that goes on to reuse an entry another site interned: an
    /// unsupported operator is a fact about the site that wrote it.
    ///
    /// Deriving the type arguments is not only for static-opt selection: for any generic inline
    /// it GROUNDS the body's typars to the caller's types. Without it, a typar reachable only
    /// through the body (`asNode`'s `value :?> SetTreeNode<'T>` result) stays a free `TyVar`
    /// root of the CALLEE's scheme — beta-reduction binds the value params but never unifies
    /// that typar — and pollutes the caller's frozen TAST as a `ResolvedTypes` "unresolved
    /// TyVar".
    ///
    /// `Inline.freshen` is applied but `Inline.relocate` is NOT: renaming binders is required
    /// of every expansion (two of one template must not share a codegen local slot), where
    /// MOVING the body is a decision about placement that the two callers make differently — a
    /// physical splice must relocate onto the call site to satisfy the `Anchor` invariant, an
    /// entry must not, because keeping the positions the body was written at is the entry's
    /// whole purpose.
    let private resolveAt
        (ctx: PassContext)
        (mint: unit -> NodeKey)
        (siteTok: SyntaxToken)
        (decl: TDecl)
        (spineArgs: (TExpr * SemType * SyntaxToken) list)
        : {| Body: TExpr; TypeArgs: SemType[] |} =
        match decl with
        | TDecl.Let(_, _, _, declTy) ->
            let typeArgs = Inline.deriveInlineTypeArgs ctx.Store declTy spineArgs
            let expanded, unresolved = Inline.inlineExpand ctx decl typeArgs

            for u in unresolved do
                ctx.Report(siteTok, Inline.unsupportedTrait ctx.Store u)

            {|
                Body = Inline.freshen mint expanded
                // The grounding the specialization table keys on — recovered here and nowhere
                // else, so an entry's stored key and the substitution its body actually
                // underwent are the same array.
                TypeArgs = typeArgs
            |}
        | _ -> failwith "InlineExpansion: an inline body must be a TDecl.Let"

    /// A SAME-UNIT template resolved for one call site and moved onto it. The template's
    /// positions are its definition site's, shared by every expansion of it, so a copy landing
    /// in a consuming tree takes the call site's — which is also the only file identity
    /// available here: a local template is written in the file being compiled, which has no
    /// `OriginFile` to name (it is not a producer any provider retained), so it has no entry to
    /// sit behind and is spliced exactly as before.
    let private expandLocalAt
        (ctx: PassContext)
        (mint: unit -> NodeKey)
        (siteTok: SyntaxToken)
        (decl: TDecl)
        (spineArgs: (TExpr * SemType * SyntaxToken) list)
        : TExpr =
        Inline.relocate siteTok (resolveAt ctx mint siteTok decl spineArgs).Body

    /// Eta-reify an `External` function used as a VALUE — `(+)` in `List.fold (+) 0 xs`,
    /// `List.fold` itself in `let g = List.fold` — into `fun p0 p1 -> f p0 p1`, turning a
    /// function NAME into a closure. This is the sole eta in the compiler: codegen has none, so
    /// an `External` of function type never reaches a backend in value position.
    ///
    /// Doing it here rather than post-freeze is what lets an inline-bodied one finish: the
    /// saturated `App` the eta mints is claimed by THIS pass's own `App` arm, which splices the
    /// body and resolves its `StaticOptimization` against the context-pinned operand type.
    /// Reified after the freeze, that `App` would be minted past the last point its body can be
    /// reached. And only a pre-freeze eta reaches MEMBER bodies at all (codegen's lowering
    /// never walks them).
    ///
    /// `body` is the reference's inline body when it has one. Arity is the reference's arrow
    /// count, capped by the body's lambda arity: `classifyApplication` rejects an over-applied
    /// inline body, and a partial eta (`fun x -> f x` for a 2-arrow `f` whose body abstracts
    /// once) is still type-correct. `ValueNone` when there is nothing to eta (arity 0) — a
    /// non-function reference. This never recurses: the eta'd `App` re-presents the SAME
    /// `External` in call-HEAD position, where the `App` arm claims it before the
    /// value-position arm can see it.
    ///
    /// The eta mints an `App` and NOT an `InlineCall` even when the reference has a body,
    /// because that `App` is claimed by the `App` arm, which is the ONE place that decides
    /// between an edge and a splice. Minting the edge here would state that fork a second time
    /// and let the two drift.
    let private etaReify
        (ctx: PassContext)
        (mint: unit -> NodeKey)
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
                    | TDecl.Let(_, value, _, _) -> Inline.lambdaArity value
                    | _ -> 0

                min (Inline.Arrows.count ctx.Store refTy) bodyArity
            | ValueNone -> Inline.Arrows.count ctx.Store refTy

        match arity with
        | 0 -> ValueNone
        | arity ->
            // Fresh binders come from the pass's own `mint`, so an eta site can never alias the
            // binders of the body about to be spliced into it.
            let binders =
                Inline.Arrows.domains ctx.Store arity refTy
                |> List.mapi (fun i pty -> mint (), pty, i)

            let appBody =
                binders
                |> List.fold
                    (fun acc (k, pty, i) ->
                        let resTy = Inline.Arrows.resultAfter ctx.Store (i + 1) refTy
                        TExpr.App(acc, TExpr.Var(k, pty, tok), resTy, tok)
                    )
                    (TExpr.External(name, keyOpt, refTy, tok))

            binders
            |> List.foldBack (fun (k, pty, _) (innerBody, innerTy) ->
                let lamTy = TyFun(pty, innerTy)
                TExpr.Lambda(TPat.NamedSimple(k, pty, tok), innerBody, lamTy, tok), lamTy
            )
            <| (appBody, Inline.Arrows.resultAfter ctx.Store arity refTy)
            |> fst
            |> ValueSome

    /// Peel a resolved inline body against one call site's spine and DECIDE each parameter's
    /// fate — the half of the reduction that needs no recursion, and so the half that can run
    /// before a specialization slot is reserved for the body the recursion will build.
    ///
    ///   1. peel the inline's lambdas, pairing each parameter with its arg;
    ///   2. a parameter bound to a bare `External` function VALUE is substituted into
    ///      the body right here — the classification in (3) reads the substituted body;
    ///   3. a lambda-valued parameter every use of which is a saturated application
    ///      head is marked `FuseLambda` (the walker splices it away) — its closure
    ///      vanishes;
    ///   4. a declared `[<CallAtMostOnce>]` parameter is marked `FuseAtMostOnce`;
    ///   5. everything else survives.
    ///
    /// `placement` reaches step (2) because that fusion happens HERE rather than in the
    /// reduction: an argument substituted into an outlined body has left its own file and is
    /// marked accordingly.
    let private classifyApplication
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
            externalValParams
            |> List.fold (fun body (k, v) -> Inline.substituteVar k v body) core

        let candidates = Dictionary<NodeKey, TExpr>()

        for p in bindings do
            match p.Arg with
            | TExpr.Lambda _ -> candidates.[p.Key] <- p.Arg
            | _ -> ()

        let bad = Inline.nonInlinableLambdaParams candidates core

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

    /// The PHYSICAL form of a reduction: surviving parameters re-bound with ordinary `let`s,
    /// innermost last so the nesting matches left-to-right application order.
    let private letBound (r: Reduced) : TExpr =
        List.foldBack
            (fun (p: InlineParam) acc ->
                TExpr.Let(TPat.NamedSimple(p.Key, p.Ty, p.AppTok), p.Arg, acc, TastWalk.exprTy acc, p.AppTok)
            )
            r.Survivors
            r.Body

    /// The reservation a FRESH reduction makes when it outlines its body. The one place the
    /// walk's frame representation meets the table's, and it goes one way: the frame supplies a
    /// position and a sink for the id, and learns nothing back.
    let private reserving (inFlight: InFlight) : Reservation =
        {
            Site = (InFlight.outermost inFlight).Site
            Announce = fun spec -> inFlight.Own.Slot <- ValueSome spec
        }

    /// Reach a cross-unit body by its resolved `SymbolKey` — the sole channel
    /// (`tryInlineBody`), which routes a value key and a member key to the entry that CARRIES
    /// it, so the body and the identity cannot disagree (and a member is selected by EXACT key,
    /// never by a name lookup whose best-by-arity collapse could serve a sibling overload's
    /// body).
    ///
    /// Every splice-eligible head is key-stamped upstream: value refs by NameResolution
    /// (`ExternalValue`), operator / synthesised-intrinsic heads by `Elaborate`
    /// (`Resolution.IntrinsicKey`), intra-body sibling refs by `Freeze`'s publish rewrite, and a
    /// member call by its resolved `MemberKey`. Operators are NOT an exception — a primitive
    /// `1 + 2` head is keyed and DOES reach `ops-platform.fs`'s `(+)`. A `key = ValueNone` head
    /// carries no inline body by construction (`Array.ofList` / ctor-as-value, handled by
    /// codegen recipes / eta-expansion), so `ValueNone` is a genuine "no body", never a missed
    /// keyless lookup. A provider with no inline bodies returns `ValueNone`.
    ///
    /// The KEY rides out with the body because it is the specialization table's template
    /// identity: an entry says which template it resolved, and the only place that is known is
    /// the lookup that found it.
    ///
    /// The THAW happens here and picks WHERE the body's nodes sit, which is the whole of the
    /// difference between the two readings `Placement` names: a body with a retained producer
    /// file keeps that file's own positions (it stays behind an edge and its indices never have
    /// to mean anything against this unit's tokens), and one without is MOVED onto the call site
    /// (its nodes land in this file's tree, where a producer's index would name an unrelated
    /// token). Thawed per lookup, so two call sites of one template never share an inference
    /// cell — and the producer file is retained on the table as it is read, since it is an
    /// entry's anchors that will have to be resolved against it.
    let private lookupExternal
        (ctx: PassContext)
        (specs: SpecTable)
        (at: SyntaxToken)
        (keyOpt: SymbolKey voption)
        : ServedBody voption =
        match keyOpt with
        | ValueSome key ->
            ExternalSymbolProviders.tryInlineBody ctx.Provider key
            |> ValueOption.map (fun ib ->
                let decl, origin =
                    match ib.Origin with
                    | ValueSome src ->
                        let sources = SpecTable.retainOrigin src specs
                        Inline.thawBodyAtOrigin ctx.Store sources src.File ib.Decl, ValueSome src.File
                    | ValueNone -> Inline.thawBody ctx.Store at ib.Decl, ValueNone

                {
                    Key = key
                    Decl = decl
                    ParamAttrs = ib.ParamAttrs
                    Origin = origin
                }
            )
        | ValueNone -> ValueNone

    /// Expand one template's body with the binding marked IN FLIGHT, giving `fresh` the frame
    /// to walk it under. A call that reaches a binding already being expanded is RECURSIVE —
    /// its expansion has no fixed point, since splicing the body in presents the same call
    /// again — so it is ANSWERED HERE rather than expanded, and that is what makes this pass
    /// terminate on a recursive `let inline` instead of exhausting the stack.
    ///
    /// THE gate: every reduction of every kind (local or served, spliced or outlined, applied
    /// or bare) passes through it, so no path can recurse by having been forgotten — and the
    /// recursive answer is INSIDE it, so no call site can give a different one.
    ///
    /// `frames` is the chain the CALL SITE was written under. A fresh reduction merely NAMES
    /// the frame the callee's body is to be walked with — nothing is pushed anywhere, so there
    /// is nothing to unwind however `fresh` returns, and a caller that walks its own material
    /// walks it under the very list it was handed.
    ///
    /// What a recursive call BECOMES turns on whether the re-entered expansion reserved a table
    /// slot. One that did can represent its own recursion: the call is a back EDGE into the
    /// entry being built, carrying `Spine` — the same list that expansion peeled its parameters
    /// from, so the edge and the entry agree on arity by construction — so the table comes out
    /// finite and cyclic and `Inline.findCycle` rejects it ONCE, naming every binding on the
    /// cycle, where a report from here would name only the arc this particular call closed. One
    /// that did not (a SPLICED reduction has no entry) has nothing for an edge to name, so the
    /// call is left as written by its own `Rebuild` and the verdict is reported here instead.
    let private expandingTemplate
        (t: SpecTable)
        (frames: ExpansionFrame list)
        (template: TemplateId)
        (name: string)
        (call: PendingCall)
        (fresh: InFlight -> TExpr)
        : TExpr =
        match frames |> List.tryFindIndex (fun f -> f.Template = template) with
        | Some i ->
            // At most one frame can match: a template found on the chain is answered rather
            // than entered, so it is never on it twice. `frames` is innermost first, so the
            // re-entered frame CLOSES the prefix through `i`, and that prefix reversed is the
            // loop in call order — the binding that was re-entered, then each binding it
            // called, out to the call closing it.
            let chain = frames |> List.truncate (i + 1) |> List.rev
            let reentered = List.head chain

            match reentered.Slot with
            | ValueSome spec ->
                TExpr.InlineCall(spec, EqArray.ofList [ for (a, _, _) in call.Spine -> call.Walk a ], call.Ty, call.Tok)
            | ValueNone ->
                // Positioned at the OUTERMOST in-flight frame's site: a nested call's own token
                // is a node of a producer's body (or of a copy moved onto that outer site), so
                // the outermost frame's is the only one that names a place in the file being
                // compiled. The chain is what `Kind.CyclicInline` names.
                SpecTable.reportCycle (List.last frames).Site [ for f in chain -> f.Name ] t
                call.Rebuild()
        | None ->
            fresh
                {
                    Own =
                        {
                            Template = template
                            Name = name
                            Site = call.Tok
                            Slot = ValueNone
                        }
                    Caller = frames
                }

    /// What one run of the pass produced.
    ///
    /// `Decls` CARRY EDGES — a `TExpr.InlineCall` per outlined call site — and
    /// `Specializations` is the table those edges name. Placement is deferred to emission
    /// (`Codegen.Common.InlineExpand.expand`), which is what lets an entry's nodes keep the
    /// anchors they were written at instead of collapsing onto their call sites.
    ///
    /// A CYCLIC table is rejected with a diagnostic and nothing walks it: substituting bodies
    /// into bodies is exactly what a cycle makes non-terminating. That is safe because the
    /// verdict is an error and no emitter runs behind one.
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

            // The resolved-specialization table this run builds, holding its own reuse pool,
            // its own retained producer files and its own cycle bookkeeping.
            let specs = SpecTable.create (fun tok kind -> ctx.Report(tok, kind)) mint

            // How a diagnostic spells a SAME-UNIT template, off the binder token that spells
            // it: a local `let inline` has no key for `servedName` to take one from.
            let localName (k: NodeKey) : string =
                match localInlines.[k] with
                | TDecl.Let(TPat.NamedSimple(_, _, tok), _, _, _) -> ctx.NameOf tok
                | other -> failwithf "InlineExpansion: a local inline is a named `TDecl.Let`; got %A" other

            // The provider seam and the three resolutions, bound to THIS run's context, table
            // and binder counter once, so that the call sites below read as what they are.
            // Each shadows the module-level function of the same name, which takes those first.
            let lookupExternal = lookupExternal ctx specs
            let resolveAt = resolveAt ctx mint
            let expandLocalAt = expandLocalAt ctx mint
            let etaReify = etaReify ctx mint

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
            let lambdaEnv = Dictionary<NodeKey, FusedLambda>()

            // Finish a classified application: walk the body (which is where every nested
            // inline head inside it resolves) and fuse in the call-site material the
            // classification marked, leaving the surviving parameters and their walked
            // arguments.
            //
            // The three fusions are all substitutions INTO the body, so their order relative to
            // one another does not matter: a fused parameter's key is `mint`-fresh and occurs
            // only in the body, never in another parameter's argument.
            let reduceClassified
                (placement: Placement)
                (walkAt: ExpansionFrame list -> TExpr -> TExpr)
                (inFlight: InFlight)
                (peeled: Peeled)
                : Reduced =
                let fusedLambdas =
                    peeled.Params |> List.filter (fun p -> p.Disposition = Disposition.FuseLambda)

                // Marked UNDER its own binders, not around the whole lambda. `Inline.betaReduce`
                // consumes those binders against arguments taken from the BODY the lambda is
                // spliced into, so the `Let`s that replace them belong to that body's file;
                // only what the lambda computes was written at the call site. (The binder
                // PATTERN's own token stays with it and no expression marker can cover it —
                // the one position a fused lambda still attributes to the body's file.)
                //
                // The caller's chain is captured HERE, where the lambda is still the caller's
                // argument, and travels to the use site the walk splices it at (which is inside
                // however many bodies the walk has descended by then).
                for p in fusedLambdas do
                    lambdaEnv.[p.Key] <-
                        {
                            Body = Inline.underLambdas (Placement.fuse placement) p.Arg
                            CallerFrames = inFlight.Caller
                        }

                let core = walkAt (InFlight.frames inFlight) peeled.Core

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
                        body <-
                            Inline.substituteVar p.Key (Placement.fuse placement (walkAt inFlight.Caller p.Arg)) body
                    | Disposition.Survive ->
                        survivors.Add
                            { p with
                                Arg = walkAt inFlight.Caller p.Arg
                            }

                survivors.Reverse()

                {
                    Body = body
                    Survivors = List.ofSeq survivors
                }

            // Expand ONE cross-unit APPLIED call site: an EDGE naming the resolved
            // specialization when the served body has a producer file to be anchored in, and
            // otherwise the physical form its thaw already committed to — a body the provider
            // retained no file for cannot be an entry, there being no anchor domain to record.
            let expandExternalCall
                (walkAt: ExpansionFrame list -> TExpr -> TExpr)
                (inFlight: InFlight)
                (siteTok: SyntaxToken)
                (served: ServedBody)
                (spineArgs: (TExpr * SemType * SyntaxToken) list)
                : TExpr =
                let resolved = resolveAt siteTok served.Decl spineArgs

                // Read off the served body, ahead of the classification, so every fusion of one
                // reduction agrees about which file its material ends up in — and matched ONCE
                // below, so the origin an outlined reduction records is the very one that made
                // it outlined.
                let placement = Placement.ofOrigin served.Origin

                let peeled = classifyApplication placement served.ParamAttrs resolved.Body spineArgs

                match placement with
                | Placement.Spliced -> letBound (reduceClassified placement walkAt inFlight peeled)
                | Placement.Outlined origin ->
                    SpecTable.outline
                        {
                            Reservation = reserving inFlight
                            Grounding =
                                {
                                    Key =
                                        {
                                            Template = served.Key
                                            TypeArgs = EqArray.ofArray resolved.TypeArgs
                                        }
                                    Arity = List.length peeled.Params
                                }
                            Shareable =
                                Peeled.isClosed peeled
                                && resolved.TypeArgs |> Array.forall (Inline.isGroundType ctx.Store)
                            Origin = origin
                            EdgeTok = siteTok
                            // The application's own result type, which the outermost `App` node
                            // already records (`collectSpine` pairs each argument with its
                            // node's result).
                            EdgeTy =
                                match List.tryLast spineArgs with
                                | Some(_, ty, _) -> ty
                                | None -> TastWalk.exprTy resolved.Body
                            ReuseArgs = fun () -> peeled.Params |> List.map (fun p -> walkAt inFlight.Caller p.Arg)
                            Build = fun () -> reduceClassified placement walkAt inFlight peeled
                        }
                        specs

            // The same entry-and-edge for a cross-unit NULLARY INTRINSIC used as a VALUE: the
            // degenerate reduction, at arity 0, with a body that is one node and no survivors.
            // It goes through the ordinary outlining path precisely because those are the only
            // differences — the token the intrinsic was WRITTEN at is exactly what an entry
            // keeps, so a parallel notion of "a body from elsewhere" would have to re-derive it.
            let outlineNullaryIntrinsic
                (inFlight: InFlight)
                (origin: OriginFile)
                (template: SymbolKey)
                (refTy: SemType)
                (tok: SyntaxToken)
                (body: TExpr)
                : TExpr =
                SpecTable.outline
                    {
                        Reservation = reserving inFlight
                        Grounding =
                            {
                                Key =
                                    {
                                        Template = template
                                        // The reference's own resolved type IS the grounding: it
                                        // is what a generic `defaultof<'T>` is instantiated at,
                                        // and the only thing a bare reference supplies.
                                        TypeArgs = EqArray.ofArray [| refTy |]
                                    }
                                // A value reference applies nothing, so the entry abstracts
                                // nothing and its edge carries no arguments.
                                Arity = 0
                            }
                        Shareable = Inline.isGroundType ctx.Store refTy
                        Origin = origin
                        EdgeTok = tok
                        EdgeTy = refTy
                        ReuseArgs = fun () -> []
                        Build = fun () -> { Body = body; Survivors = [] }
                    }
                    specs

            // The expansion walker, as a FUNCTION of the chain the material it is handed was
            // WRITTEN under — the only way the chain can be threaded, `TastWalk.Mapper` having no
            // room for a parameter of its own. Descending into a callee's body builds a mapper at
            // the chain with that callee's frame consed on; walking material the CALL SITE
            // supplied builds one at the shorter chain that material belongs to. The default
            // child recursion stays in the same material and so on the same chain, which is
            // exactly what re-using `m` says.
            //
            // This is the sole inline expander —
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
            let rec mapperAt (frames: ExpansionFrame list) : TastWalk.Mapper =
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

                                // A rewrite inherits the position of the node it REPLACES, never
                                // one of that node's children. What an expansion stands in for is
                                // the whole APPLICATION, so the application node's own token is
                                // its position — and reading the HEAD's instead is wrong wherever
                                // the two differ. They differ exactly when an outer fusion
                                // substituted the head in: the head was then written at the CALL
                                // SITE while the application around it is the producer's own
                                // material, so an edge anchored at the head sits inside an entry
                                // claiming a position in a file that entry does not name. Taking
                                // the application's token needs no marker to say so, because the
                                // node is producer material by construction.
                                let appTok = TastWalk.exprTok e

                                // The spine walked as the CALLER's own material, which every
                                // rebuild below needs and no expansion does.
                                let walkedArgs () =
                                    [ for (a, ty, tok) in spineArgs -> walk a, ty, tok ]

                                // The fixed half of a gate answer for an applied call. `spine` is
                                // what the callee's parameters are peeled against and `rebuild`
                                // is the arm's own no-inline-body fallthrough — see `PendingCall`
                                // for why those are two different shapes.
                                let pendingApp
                                    (spine: (TExpr * SemType * SyntaxToken) list)
                                    (rebuild: unit -> TExpr)
                                    : PendingCall =
                                    {
                                        Tok = appTok
                                        Ty = TastWalk.exprTy e
                                        Spine = spine
                                        Walk = walk
                                        Rebuild = rebuild
                                    }

                                match head with
                                | TExpr.Var(k, _, _) when localInlines.ContainsKey k ->
                                    ValueSome(
                                        expandingTemplate
                                            specs
                                            frames
                                            (TemplateId.Local k)
                                            (localName k)
                                            (pendingApp
                                                spineArgs
                                                (fun () -> TastWalk.rebuildApp markedHead (walkedArgs ())))
                                            (fun inFlight ->
                                                letBound (
                                                    reduceClassified
                                                        // A same-unit template is MOVED onto the call
                                                        // site, so its body and the arguments fused
                                                        // into it are one anchor domain already.
                                                        Placement.Spliced
                                                        walkAt
                                                        inFlight
                                                        (classifyApplication
                                                            Placement.Spliced
                                                            (localParamAttrs k)
                                                            (expandLocalAt appTok localInlines.[k] spineArgs)
                                                            spineArgs)
                                                )
                                            )
                                    )
                                // A saturated use of an inline-first lambda
                                // parameter: splice a fresh
                                // copy of its bound lambda, beta-reduced against the
                                // call args, and walk it (nested inline heads /
                                // further lambda params resolve in the recursion).
                                // `Inline.nonInlinableLambdaParams` guaranteed every use is
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
                                    let fused = lambdaEnv.[k]

                                    ValueSome(
                                        walkAt
                                            fused.CallerFrames
                                            (Inline.betaReduce (Inline.freshen mint fused.Body) spineArgs)
                                    )
                                | TExpr.External(_, keyOpt, _, _) ->
                                    match lookupExternal appTok keyOpt with
                                    // An external WITH an inline body ALWAYS expands —
                                    // no operand-groundness gate. An un-ground `^T`
                                    // simply selects no per-primitive
                                    // `StaticOptimization` clause and falls to the
                                    // body's BASE, which is where the safe generic
                                    // default lives (`EqualityComparer<^T>.Default.Equals`
                                    // for `=`). Declining instead routed the
                                    // head to a name-keyed raw-IL fallback, turning a
                                    // structural `=` into a reference `ceq`.
                                    | ValueSome served ->
                                        ValueSome(
                                            expandingTemplate
                                                specs
                                                frames
                                                (TemplateId.Foreign served.Key)
                                                (Inline.servedName served.Key)
                                                (pendingApp
                                                    spineArgs
                                                    (fun () -> TastWalk.rebuildApp markedHead (walkedArgs ())))
                                                (fun inFlight ->
                                                    expandExternalCall walkAt inFlight appTok served spineArgs
                                                )
                                        )
                                    // An external with no inline body (a real
                                    // cross-package call): keep the head, lower the
                                    // args — exactly codegen's `head'` rule, left for
                                    // its recipe path.
                                    | _ -> ValueSome(TastWalk.rebuildApp markedHead (walkedArgs ()))
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
                                    match lookupExternal appTok (ValueSome key) with
                                    | ValueSome served ->
                                        let fullSpine =
                                            match receiver with
                                            | ValueSome r -> (r, TastWalk.exprTy r, memberTok) :: spineArgs
                                            | ValueNone -> spineArgs

                                        ValueSome(
                                            expandingTemplate
                                                specs
                                                frames
                                                (TemplateId.Foreign served.Key)
                                                (Inline.servedName served.Key)
                                                // The receiver is a FIELD of the head, which is
                                                // where the two answers part company: an EDGE is
                                                // positional against parameters peeled from
                                                // `fullSpine`, so it must carry the receiver as
                                                // argument 0, while a rebuild leaves it inside the
                                                // head — where it is the one piece of material
                                                // nothing else walks.
                                                (pendingApp
                                                    fullSpine
                                                    (fun () -> TastWalk.rebuildApp (walk markedHead) (walkedArgs ())))
                                                (fun inFlight ->
                                                    expandExternalCall walkAt inFlight appTok served fullSpine
                                                )
                                        )
                                    | ValueNone -> ValueSome(TastWalk.rebuildApp (walk markedHead) (walkedArgs ()))
                                // A non-external, non-local-inline head (e.g. a
                                // higher-order parameter): lower the head and args,
                                // keeping the spine intact. The head SURVIVES here, so it is
                                // rebuilt marked — only a rewrite that consumes the node
                                // consumes its mark.
                                | _ -> ValueSome(TastWalk.rebuildApp (walk markedHead) (walkedArgs ()))
                            // A BARE (non-applied) reference to a LOCAL inline — the
                            // template used as a value. No spine, so no type argument is
                            // derivable and the body's typars stay abstract; it still
                            // goes through `expandLocalAt` so its static-opt clauses resolve
                            // and any trait call it cannot dispatch is REPORTED rather
                            // than handed to a backend that has no arm for it.
                            | TExpr.Var(k, _, tok) when localInlines.ContainsKey k ->
                                ValueSome(
                                    expandingTemplate
                                        specs
                                        frames
                                        (TemplateId.Local k)
                                        (localName k)
                                        {
                                            Tok = tok
                                            Ty = TastWalk.exprTy e
                                            // No spine at all: a bare reference applies nothing,
                                            // so an edge carries no arguments and there is nothing
                                            // to leave unexpanded but the reference itself.
                                            Spine = []
                                            Walk = walk
                                            Rebuild = fun () -> e
                                        }
                                        (fun inFlight ->
                                            walkAt (InFlight.frames inFlight) (expandLocalAt tok localInlines.[k] [])
                                        )
                                )
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
                                    // A nullary intrinsic is a single node with no binders, so it
                                    // needs no expansion — but it crosses the same file boundary
                                    // every other served body does, and so takes the same
                                    // entry-and-edge (`outlineNullaryIntrinsic`).
                                    | ValueSome(TExpr.ILIntrinsic(op, operand, args, _, intrinsicTok)) ->
                                        // A GENERIC nullary intrinsic carries its own scheme
                                        // typar, which only the reference's type can ground.
                                        let grounded (at: SyntaxToken) =
                                            let groundedOperand =
                                                match operand with
                                                | ValueSome _ -> ValueSome refTy
                                                | ValueNone -> ValueNone

                                            TExpr.ILIntrinsic(op, groundedOperand, args, refTy, at)

                                        match Placement.ofOrigin served.Origin with
                                        // No retained producer file: the thaw already moved the
                                        // node onto this reference, so it stands where it lands.
                                        | Placement.Spliced -> ValueSome(grounded tok)
                                        | Placement.Outlined origin ->
                                            // In flight like every other served body, so the
                                            // reservation has the one frame it publishes its slot
                                            // on. The recursive answer is unreachable rather than
                                            // absent — an intrinsic with no operands names nothing,
                                            // so nothing inside it can lead back here — and going
                                            // through the gate costs a line where a special case
                                            // would cost the uniformity that makes the gate total.
                                            ValueSome(
                                                expandingTemplate
                                                    specs
                                                    frames
                                                    (TemplateId.Foreign served.Key)
                                                    (Inline.servedName served.Key)
                                                    {
                                                        Tok = tok
                                                        Ty = refTy
                                                        Spine = []
                                                        Walk = walk
                                                        Rebuild = fun () -> e
                                                    }
                                                    (fun inFlight ->
                                                        outlineNullaryIntrinsic
                                                            inFlight
                                                            origin
                                                            served.Key
                                                            refTy
                                                            tok
                                                            (grounded intrinsicTok)
                                                    )
                                            )
                                    | _ -> etaReify body name keyOpt refTy tok |> ValueOption.map walk
                                | ValueNone -> etaReify body name keyOpt refTy tok |> ValueOption.map walk
                            | _ -> ValueNone
                }

            // Walk `e` as material written under `frames`.
            and walkAt (frames: ExpansionFrame list) (e: TExpr) : TExpr = TastWalk.mapExpr (mapperAt frames) e

            // The file's OWN declarations are inside no expansion, so they are walked under the
            // empty chain — which is also why the outermost frame of any chain reached from here
            // is the one whose `Site` names a position in this file.
            let walkExpr (e: TExpr) : TExpr = walkAt [] e

            let expanded = decls |> List.map (fun (d, env) -> mapDeclExprs walkExpr d, env)

            // The roots `SpecTable.finish` counts edges from — collected through `mapDeclExprs`
            // so "which expressions does a declaration carry?" is answered ONCE for the whole
            // pass; a hand-written second traversal is how one of them comes to miss a slot.
            let declExprs = ResizeArray<TExpr>()

            for (d, _) in expanded do
                mapDeclExprs
                    (fun e ->
                        declExprs.Add e
                        e
                    )
                    d
                |> ignore

            {
                Decls = expanded
                Specializations = SpecTable.finish declExprs specs
            }
