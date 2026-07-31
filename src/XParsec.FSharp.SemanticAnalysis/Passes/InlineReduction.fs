namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
// The table an outlined reduction reserves a slot in, and the parameter vocabulary a peel
// classifies into.
open InlineSpecTable

// The vocabulary of ONE inline reduction: what a cross-unit body IS once it has been served,
// WHERE its nodes will live, WHICH template it expands, and how one call site's spine is
// resolved against it and its parameters classified.
//
// A file of its own because none of it depends on the WALK. Everything here is a function of a
// served body, a spine and a placement — no traversal, no pass-wide mutable state, no descent
// into a callee — so each piece reads against the single reduction it describes rather than
// against the thousand-line closure that drives them all. What is deliberately ABSENT is the
// walk: which node is a call site, which chain it was written under, and where the result goes
// belong to the walk, and nothing here can observe them. The `ExpansionFrame` /
// `InFlight` chain is here even so, because what a frame IS — a template identity plus the
// table slot its expansion reserved — is a fact about one reduction; only the THREADING of the
// chain belongs to the walk.
//
// A cross-unit body arrives FROZEN and is THAWED here (`lookupExternal`), minting this unit's
// own inference cells. That thaw is the single immutable→mutable transition on the provider
// seam: nothing below it can `UnionFind.union` into a producer's cells, because it never holds
// one.
//
// WHERE that body ends up is decided once, at that same thaw, and `Placement` is the decision.
// A body whose provider RETAINED the producer file becomes an entry of the file's
// resolved-specialization table, keeping the positions it was written at, and the call site
// gets a `TExpr.InlineCall` edge naming it — so
// a body called from N sites is one entry and N edges, each with its own provenance. A body
// served without a retained file has no anchor domain an entry could name, so the thaw MOVES it
// onto the call site and it is physically spliced, as are same-unit templates (whose
// positions are already this file's).
//
// It also owns the compiler's ONE eta-reification (`etaReify`): an `External` of function type
// used as a VALUE becomes a closure here, whether or not it has an inline body. Codegen has no
// eta of its own. The pre-freeze position is forced — an inline-bodied external
// (`List.fold (+) 0 xs`) reified post-freeze mints its call `App` past the last point the body
// can be spliced into it, and codegen's lowering never walks member bodies at all.
//
// The inline-first soundness condition (beta-reduction half) is `classifyApplication`'s: a
// lambda argument bound to an inline parameter and fully applied inside the body is eliminated
// — its closure never exists, the classification marking it and the walk splicing it away at
// each use. A lambda that is stored or partially
// applied survives as a real closure. The byref-like-capture half (reject / ref-struct closures
// for a SURVIVING closure that holds a `Span`/`ref struct`) is deferred — see the TODO in
// `classifyApplication`; it needs a byref-like predicate that does not exist yet.
//
// `internal` and not `private`: this is the pass's own vocabulary and no consumer outside the
// assembly has business with it, but `InlineExpansion` names it from another module.
module InlineReduction =

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
    type internal Placement =
        /// Abstracted into a specialization entry, whose nodes stay anchored in the producer
        /// file its `OriginFile` names. Descending the edge PUSHES that file, so fused
        /// call-site material is one frame out and pops back.
        | Outlined of origin: OriginFile
        /// Moved onto the call site, so the body and the material fused into it are already
        /// one domain. Nothing pushed a frame, so nothing may pop one.
        | Spliced

    [<RequireQualifiedAccess>]
    module internal Placement =

        /// THE reading of a served body's retained producer file, taken once so that every
        /// later decision branches on the placement itself. `ValueNone` is not a missing field
        /// but the other reading of a served body: no retained file means no domain an entry
        /// could name, and the thaw has already moved such a body onto the call site.
        let ofOrigin (origin: OriginFile voption) : Placement =
            match origin with
            | ValueSome file -> Placement.Outlined file
            | ValueNone -> Placement.Spliced

        /// The anchor domain of the body a reduction is about to walk. An outlined body keeps
        /// the producer's positions; a spliced one was relocated onto the call site and so is
        /// in `caller`'s domain like everything else there.
        let domain (caller: OriginFile) (placement: Placement) : OriginFile =
            match placement with
            | Placement.Outlined origin -> origin
            | Placement.Spliced -> caller

        /// A fused call-site argument as it must appear INSIDE the body it is fused into,
        /// `caller` being the file it was written in. Applied UNCONDITIONALLY at an outlined
        /// site, a trivial argument included: a uniform invariant is checkable where one that
        /// skips `Var`s and constants is not.
        let fuse (caller: OriginFile) (placement: Placement) (arg: TExpr) : TExpr =
            match placement with
            | Placement.Outlined _ -> TastWalk.callerExpr caller arg
            | Placement.Spliced -> arg

    /// The peel of a resolved inline body against one call site's spine, with each parameter's
    /// disposition already decided. Everything about the reduction that is knowable before the
    /// body is walked.
    type internal Peeled =
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
    type internal ServedBody =
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
    /// Two cases because a SAME-UNIT template has no `SymbolKey` at all — the freeze refuses to
    /// publish a top-level `let inline` — so its binder is the only identity it has.
    [<RequireQualifiedAccess>]
    type internal TemplateId =
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
    /// to state a depth.
    ///
    /// An inline binding that reaches ITSELF has no expansion: splicing the body in presents
    /// the same call again, forever. This is what stops that — the compiler reports a cycle
    /// instead of substituting until its stack goes.
    ///
    /// Both cases are the SAME compile error (`Kind.CyclicInline`); they differ only in where
    /// it is raised, and so in how much of the loop it can name.
    [<RequireQualifiedAccess>]
    type internal CycleReport =
        /// From the finished table. The call is left as a reference to this binding's entry,
        /// which keeps the graph finite, and the cycle check then names EVERY binding on the
        /// loop rather than just the call that closed it.
        | FromTable of SpecializationId
        /// From the call itself. A spliced reduction has no entry to reference, so there is
        /// nothing to leave behind and nothing for a later check to find.
        | AtThisCall

    [<NoEquality; NoComparison>]
    type internal ExpansionFrame =
        {
            Template: TemplateId
            /// How a diagnostic spells the binding.
            Name: string
            /// The call site as written in the file being compiled. On the OUTERMOST frame it is
            /// the only position a verdict about this expansion can carry: every deeper site is a
            /// node of a producer's body, whose anchor indexes a file this compilation is not
            /// reporting against.
            Site: SyntaxToken
            /// The anchor domain of the body this frame is expanding, recorded when the frame is
            /// pushed. It rides the chain because material and chain always travel together, so
            /// a walk cannot be handed one under a domain the other disagrees with.
            Origin: OriginFile
            /// How a call reaching this binding again is reported.
            CycleReport: CycleReport
        }

    [<RequireQualifiedAccess>]
    module internal ExpansionFrame =

        /// The domain material walked under `frames` is anchored in. An empty chain is the
        /// compiling unit's own material, which `compiling` supplies.
        let originOf (compiling: OriginFile) (frames: ExpansionFrame list) : OriginFile =
            match frames with
            | [] -> compiling
            | f :: _ -> f.Origin

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
    type internal InFlight =
        {
            Own: ExpansionFrame
            Caller: ExpansionFrame list
        }

    [<RequireQualifiedAccess>]
    module internal InFlight =

        /// The chain this reduction's own BODY is walked under, innermost first.
        let frames (f: InFlight) : ExpansionFrame list = f.Own :: f.Caller

    /// What a FRESH reduction is handed once its template is known not to be on the chain: how
    /// to push its frame, and where a verdict about it is positioned.
    ///
    /// The frame is a FUNCTION of the placement and the cycle report because a reduction knows
    /// neither where its body will live nor how its own recursion would be reported until it
    /// has classified its parameters and either taken a table slot or settled on a splice.
    /// Nothing walks the body before then, so no call can reach the binding while the answer
    /// is still unsettled.
    ///
    /// Taking the PLACEMENT and not the domain it implies is what keeps a frame's domain a
    /// function of the chain it is consed onto: the builder closes over that chain, so a
    /// spliced frame cannot be handed some file its caller was not walking in.
    [<NoEquality; NoComparison>]
    type internal Entering =
        {
            Frame: Placement -> CycleReport -> InFlight
            /// The chain the CALL SITE's material was written under — what an argument this
            /// site supplied is walked against, independent of the re-entry answer.
            Caller: ExpansionFrame list
            /// The outermost in-flight site — a position in the file being compiled, which
            /// every deeper site fails to be.
            Site: SyntaxToken
        }

    /// The call an expansion is being entered FOR: which binding it calls, where it stands, and
    /// the two forms an ANSWER needs it in — which are NOT the same form, and conflating them is
    /// how an edge comes to disagree with the entry it names.
    ///
    /// An answered call is either an EDGE into the entry the re-entered expansion reserved, or
    /// the call left exactly as written. The edge is positional against that entry's parameters,
    /// so it takes the spine the FRESH reduction would have peeled — for an `ExternalMember`
    /// that includes the receiver, at curried position 0. The un-expanded rebuild is the
    /// original application, where the receiver belongs inside the head it was written in and no
    /// spine ever mentioned it.
    ///
    /// The FRESH reduction reads this SAME record, so the position and the result type an entry
    /// mints its edge with are the very ones the recursive answer would have used — one
    /// derivation of each, rather than a second that has to be argued equal to the first.
    [<NoEquality; NoComparison>]
    type internal PendingCall =
        {
            /// WHICH binding is being called: the identity a recursion is detected on, and the
            /// template a fresh reduction enters. Also all a frame needs to be NAMED, the
            /// diagnostic spelling of a binding being a function of its identity alone.
            Template: TemplateId
            /// The position the expansion stands in for. A rewrite inherits the position of the
            /// node it REPLACES, so for an application that is the APPLICATION node's own token
            /// and never the head's — the two differ exactly when an outer fusion substituted
            /// the head in, which makes the head call-site material sitting inside a producer's
            /// own application. It is also the site the fresh frame records, and the anchor a
            /// fresh reduction's own edge is minted at.
            Tok: SyntaxToken
            /// The node's own result type, which the outermost `App` already records
            /// (`collectSpine` pairs each argument with its node's result) and a bare reference
            /// carries directly. It is the type of EVERY edge minted for this call, recursive or
            /// fresh.
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

    /// A call HEAD that names a cross-unit symbol, reduced to the three things an answer needs
    /// of it: the key that resolves it, the spine the callee's parameters are peeled against,
    /// and the head an un-expanded rebuild leaves standing.
    ///
    /// The two head shapes — a plain `External` and the dotted `ExternalMember` that
    /// `x.get_Item(2)` / `w.Poke 41` lowers to — differ in exactly this much, and the whole of
    /// the difference is that the receiver is a FIELD of the member head rather than a spine
    /// argument. An EDGE is positional against parameters peeled from the spine, so a member
    /// call must carry its receiver at curried position 0 (`this`→receiver; a STATIC member has
    /// none and prepends nothing) for each `pi` to align to `argi`. A REBUILD leaves the
    /// receiver inside the head — where it is the one piece of material nothing else walks, so
    /// that head must be walked where a plain `External` head must not.
    ///
    /// Taken before the expansion so the expansion is written ONCE: the two shapes reach the
    /// same reduction by the same path, and cannot drift into two.
    [<NoEquality; NoComparison>]
    type internal ExternalHead =
        {
            /// `ValueNone` is a genuine "carries no inline body", never a missed lookup — see
            /// `lookupExternal`. A member head is always keyed.
            Key: SymbolKey voption
            Spine: (TExpr * SemType * SyntaxToken) list
            /// A thunk: the walk a member head needs is wasted on any answer that expands the
            /// call, which is every answer but the rebuild.
            RebuiltHead: unit -> TExpr
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
    type internal FusedLambda =
        {
            Body: TExpr
            CallerFrames: ExpansionFrame list
        }

    [<RequireQualifiedAccess>]
    module internal Peeled =

        /// No call-site material was fused into the body, so the body is CLOSED over its
        /// parameters and nothing in it belongs to this site. THE condition under which two
        /// sites may share one entry, and a question about VALUES and not positions: fused
        /// material is one site's OPERAND, so a shared entry holding it would evaluate the
        /// first site's argument at the second site's call. A fused node names the file it was
        /// written in, so what sharing costs is not an anchor domain but the right answer.
        let isClosed (p: Peeled) : bool =
            p.Params |> List.forall (fun x -> x.Disposition = Disposition.Survive)

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
    /// Binders are freshened but the body is NOT relocated: renaming binders is required
    /// of every expansion (two of one template must not share a codegen local slot), where
    /// MOVING the body is a decision about placement that the two callers make differently — a
    /// physical splice must relocate onto the call site to satisfy the `Anchor` invariant, an
    /// entry must not, because keeping the positions the body was written at is the entry's
    /// whole purpose.
    let internal resolveAt
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
    /// sit behind and can only be spliced.
    let internal expandLocalAt
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
    let internal etaReify
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

                min (SemTypeQuery.Arrows.count ctx.Store refTy) bodyArity
            | ValueNone -> SemTypeQuery.Arrows.count ctx.Store refTy

        match arity with
        | 0 -> ValueNone
        | arity ->
            // Fresh binders come from the pass's own `mint`, so an eta site can never alias the
            // binders of the body about to be spliced into it.
            let binders =
                SemTypeQuery.Arrows.domains ctx.Store arity refTy
                |> List.mapi (fun i pty -> mint (), pty, i)

            let appBody =
                binders
                |> List.fold
                    (fun acc (k, pty, i) ->
                        let resTy = SemTypeQuery.Arrows.resultAfter ctx.Store (i + 1) refTy
                        TExpr.App(acc, TExpr.Var(k, pty, tok), resTy, tok)
                    )
                    (TExpr.External(name, keyOpt, refTy, tok))

            binders
            |> List.foldBack (fun (k, pty, _) (innerBody, innerTy) ->
                let lamTy = TyFun(pty, innerTy)
                TExpr.Lambda(TPat.NamedSimple(k, pty, tok), innerBody, lamTy, tok), lamTy
            )
            <| (appBody, SemTypeQuery.Arrows.resultAfter ctx.Store arity refTy)
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
    /// `placement` and `caller` reach step (2) because that fusion happens HERE rather than in
    /// the reduction: an argument substituted into an outlined body has left the file it was
    /// written in and is marked with it.
    let internal classifyApplication
        (caller: OriginFile)
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
        // Recognised THROUGH any caller mark: an argument an OUTER fusion already
        // marked is still the bare external value this rule is about, and re-marking
        // it here is right rather than redundant — two frames out is two pops.
        let externalValParams =
            bindings
            |> List.choose (fun p ->
                match TastWalk.unmarked p.Arg with
                | TExpr.External _ -> Some(p.Key, Placement.fuse caller placement p.Arg)
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
    let internal letBound (r: Reduced) : TExpr =
        List.foldBack
            (fun (p: InlineParam) acc ->
                TExpr.Let(TPat.NamedSimple(p.Key, p.Ty, p.AppTok), p.Arg, acc, TastWalk.exprTy acc, p.AppTok)
            )
            r.Survivors
            r.Body

    /// Reach a cross-unit body by its resolved `SymbolKey` — the sole channel
    /// (`tryInlineBody`), which routes a value key and a member key to the entry that CARRIES
    /// it, so the body and the identity cannot disagree (and a member is selected by EXACT key,
    /// never by a name lookup whose best-by-arity collapse could serve a sibling overload's
    /// body).
    ///
    /// Every splice-eligible head is key-stamped upstream: value refs by name resolution,
    /// operator / synthesised-intrinsic heads by elaboration, intra-body sibling refs by the
    /// freeze's publish rewrite, and a member call by its resolved `MemberKey`. Operators are NOT an exception — a primitive
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
    let internal lookupExternal
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
                        InlineThaw.bodyAtOrigin ctx.Store sources src.File ib.Decl, ValueSome src.File
                    | ValueNone -> InlineThaw.body ctx.Store at ib.Decl, ValueNone

                {
                    Key = key
                    Decl = decl
                    ParamAttrs = ib.ParamAttrs
                    Origin = origin
                }
            )
        | ValueNone -> ValueNone
