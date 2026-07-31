namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open InlineSpecTable

// One inline reduction: what the body it expands IS, WHICH template that
// body belongs to, and how one call site's arguments are resolved against it and its parameters
// classified.
//
// A cross-file body arrives FROZEN and is THAWED here (`lookupExternal`), minting this file's
// own inference cells. That thaw is the single immutable→mutable transition on the provider
// seam: nothing below it can `UnionFind.union` into a producer's cells, because it never holds
// one.
//
// EVERY reduction is outlined, whether its template is this file's own or another file's: the
// body becomes a table entry keeping the positions it was written at, and the call site gets a
// `TExpr.InlineCall` edge naming it. Placing a body is the backends' emit-time expansion.
//
// It also owns the compiler's ONE eta-reification (`etaReify`), and the pre-freeze position is
// forced: an inline-bodied external reified post-freeze mints its call `App` past the last
// point the body can be spliced into it, and codegen's lowering never walks member bodies.
module InlineReduction =

    /// The peel of a resolved inline body against one call site's arguments — everything about the
    /// reduction that is knowable before the body is walked.
    type internal Peeled =
        {
            /// Outermost curried parameter first, so an index into this list IS the curried
            /// position the declared `ParamAttrs` are aligned to.
            Params: InlineParam list
            /// The body under the peeled lambdas, with the `FuseExternalValue` substitutions
            /// already applied (they are what the lambda-parameter classification reads).
            Core: TExpr
        }

    /// An inline body as this pass consumes it, in this file's `SemType` domain.
    ///
    /// The SAME record for a template of this file and for one another file served, because a
    /// reduction of the two differs in nothing: both are outlined, both keep the positions they
    /// were written at, and only `Origin` tells them apart.
    type internal TemplateBody =
        {
            Key: SymbolKey
            Decl: TDecl
            ParamAttrs: ParamAttrs[]
            /// The file every anchor in `Decl` indexes — this file's own for a local template,
            /// the producer's for a served one.
            Origin: OriginFile
        }

    /// WHICH inline binding a reduction is expanding — the identity a RECURSION is detected on,
    /// and NOT the `SymbolKey` its table entry is keyed by. A local binding's key and a
    /// package's can collide, and the collision must not answer "is this call recursive?".
    ///
    /// The TEMPLATE alone and not its `Grounding`: a binding that reaches itself at a different
    /// grounding is no less non-terminating (a polymorphically recursive one mints a fresh
    /// grounding every time round), so keying on the grounding would let exactly the divergence
    /// this exists to stop straight through.
    [<RequireQualifiedAccess>]
    type internal TemplateId =
        | Local of binder: NodeKey
        | Foreign of key: SymbolKey

    /// One inline reduction IN FLIGHT: a binding whose body is currently being expanded.
    ///
    /// The CHAIN of these is what makes a recursive inline TERMINATE. Substituting the body of a
    /// binding already on the chain would present the same call again, forever; so such a call
    /// is answered with an edge into the entry that expansion already reserved, which keeps the
    /// graph finite for the acyclicity check on the finished table to convict.
    [<NoEquality; NoComparison>]
    type internal ExpansionFrame =
        {
            Template: TemplateId
            /// The anchor domain of the body this frame is expanding, recorded when the frame is
            /// pushed. It rides the chain because material and chain always travel together, so
            /// a walk cannot be handed one under a domain the other disagrees with.
            Origin: OriginFile
            /// The table slot this expansion reserved — what a call reaching this binding again
            /// is answered with.
            Spec: SpecializationId
        }

    /// The inline bindings whose bodies this walk is currently INSIDE, innermost first, plus the
    /// token of the call in the user's file that it went inside them for.
    ///
    /// Expanding `sq 3` in `app.fs` means walking the body of `let inline sq x = x * x`, whose
    /// expressions were written somewhere else — possibly in another package's `math.fs` — and
    /// may call further inlines from there. At every node of such a body two questions come up
    /// that the node cannot answer about itself, and this is what answers them:
    ///
    ///   - the FRAMES say which binding's body this expression came out of: which FILE its
    ///     token indices are indices into (`math.fs`, not the `app.fs` being compiled), and
    ///     whether a call it makes names a binding already being expanded further up. The
    ///     second is what terminates `let rec inline f x = f x`: the call is answered from the
    ///     frame with an edge to the table slot that expansion already reserved, leaving a
    ///     finite table for the cycle check to convict with a diagnostic.
    ///   - the SITE is where that diagnostic — or any other verdict reached while expanding —
    ///     is reported: at `sq 3` in `app.fs`. Reporting it at the `x * x` the walk is standing
    ///     on would take a token index into `math.fs` and read it against `app.fs`'s tokens,
    ///     where it is in range and underlines unrelated source.
    ///
    /// The site is settled by the OUTERMOST call (`sq 3`) and every frame pushed beneath it
    /// inherits that same one, so it belongs to the descent and not to a frame: written once,
    /// where it is decided, instead of onto each frame and read back off the end of the chain.
    ///
    /// PRIVATE, so `top` and `enter` are the only ways to build one and no walk can push a frame
    /// while leaving the site behind.
    [<NoEquality; NoComparison>]
    type internal Descent =
        private
            {
                Frames: ExpansionFrame list
                /// `ValueNone` only while walking the compiling file's own declarations: nothing
                /// has been entered, so the call about to be answered is itself written there
                /// and its own token is the position to report at.
                Site: SyntaxToken voption
            }

    /// A FRESH reduction and the two descents its material belongs to.
    ///
    /// Two fields and not one because the body walks INSIDE this reduction while an argument the
    /// site supplied walks outside it. An argument is the caller's, so a call it makes to the
    /// binding it is an argument OF is an ordinary nested application and not a recursion:
    /// `1 - 2 - 3` applies `(-)` inside `(-)`'s own left operand without either expansion
    /// containing the other, and the callee's chain would convict every such program.
    [<NoEquality; NoComparison>]
    type internal InFlight =
        {
            /// The descent this reduction's own BODY is walked under.
            Own: Descent
            /// The descent the CALL SITE's own material is walked under.
            Caller: Descent
        }

    /// The call an expansion is being entered FOR: which binding it calls, where it stands, and
    /// the arguments an ANSWER needs it in.
    ///
    /// An answered call is an EDGE into the entry the re-entered expansion reserved. The edge is
    /// positional against that entry's parameters, so it takes the arguments the FRESH reduction
    /// would have peeled — for an `ExternalMember` that includes the receiver, at curried
    /// position 0, which the application it was reached through never held.
    ///
    /// The FRESH reduction reads this SAME record, so the position and the result type an entry
    /// mints its edge with are the very ones the recursive answer would have used — one
    /// derivation of each, rather than a second that has to be argued equal to the first.
    [<NoEquality; NoComparison>]
    type internal PendingCall =
        {
            /// WHICH binding is being called: the identity a recursion is detected on, and the
            /// template a fresh reduction enters.
            Template: TemplateId
            /// The position the expansion stands in for: the APPLICATION node's own token and
            /// never the head's. The two differ exactly when an outer fusion substituted the
            /// head in, which makes the head call-site material sitting inside a producer's own
            /// application.
            Tok: SyntaxToken
            /// The node's own result type — the type of EVERY edge minted for this call,
            /// recursive or fresh.
            Ty: SemType
            /// The arguments AS APPLIED — what the entry's parameters were peeled against, so an
            /// edge's arguments are positional against them by construction. Unwalked: only the
            /// edge answer walks them, and only the arguments it actually carries.
            Args: (TExpr * SemType * SyntaxToken) list
            /// Walks one of `Args`. Those are the CALLER's own material and are expanded on
            /// their own merits however the call is answered.
            Walk: TExpr -> TExpr
        }

    [<RequireQualifiedAccess>]
    module internal Descent =

        /// The compiling file's own declarations, inside no inline body — where the walk starts
        /// and what it returns to for every call-site argument it walks.
        let top: Descent = { Frames = []; Site = ValueNone }

        /// The file the expressions being walked here were WRITTEN in, whose token array their
        /// anchors index — the producer's once the walk is inside a body served from another
        /// file, and `compiling` while it is in the file's own declarations.
        let originOf (compiling: OriginFile) (d: Descent) : OriginFile =
            match d.Frames with
            | [] -> compiling
            | f :: _ -> f.Origin

        /// The frame already expanding `template`, if the walk is inside one: this call has
        /// reached a binding that reaches itself, and is answered from that frame instead of
        /// expanding the same body a second time. At most one frame can match — a template on
        /// the chain is answered rather than entered, so it is never on it twice.
        let reentered (template: TemplateId) (d: Descent) : ExpansionFrame voption =
            match d.Frames |> List.tryFind (fun f -> f.Template = template) with
            | Some f -> ValueSome f
            | None -> ValueNone

        /// The token a diagnostic about `call` is reported at: the user-written call this whole
        /// descent went inside a body for, or `call`'s own token where nothing has been entered
        /// and `call` IS that user-written one.
        let siteOf (d: Descent) (call: PendingCall) : SyntaxToken =
            match d.Site with
            | ValueSome site -> site
            | ValueNone -> call.Tok

        /// Go INSIDE the body this call names: the descent its own expressions are walked at
        /// (this binding pushed onto the caller's), beside the one the call site's arguments
        /// stay at — they are the caller's expressions and never enter anything.
        ///
        /// `origin` (the file the body was written in) and `spec` (its table slot) arrive last
        /// because neither is known until the body has been resolved and a slot taken; nothing
        /// walks the body before then, so no call can reach this binding while the answer a
        /// recursive reach would get is still unsettled.
        let enter (d: Descent) (call: PendingCall) (origin: OriginFile) (spec: SpecializationId) : InFlight =
            {
                Own =
                    {
                        Frames =
                            {
                                Template = call.Template
                                Origin = origin
                                Spec = spec
                            }
                            :: d.Frames
                        // The site the descent already carries, or this call's own where it is
                        // the outermost — the ONE write, inherited unchanged from here down.
                        Site = ValueSome(siteOf d call)
                    }
                Caller = d
            }

    /// A call HEAD that names a cross-file symbol, reduced to the three things an answer needs
    /// of it — so the expansion behind a plain `External` and behind the dotted
    /// `ExternalMember` that `x.get_Item(2)` lowers to is written ONCE.
    ///
    /// The whole of the difference between the two is that the receiver is a FIELD of the member
    /// head rather than an applied argument. An EDGE is positional against parameters peeled from
    /// the arguments, so a member call must carry its receiver at curried position 0 (a STATIC
    /// member has none and prepends nothing) for each `pi` to align to `argi`. A REBUILD instead
    /// leaves the receiver inside the head — where it is the one piece of material nothing else
    /// walks, so that head must be walked where a plain `External` head must not.
    [<NoEquality; NoComparison>]
    type internal ExternalHead =
        {
            /// `ValueNone` is a genuine "carries no inline body", never a missed lookup — see
            /// `lookupExternal`. A member head is always keyed.
            Key: SymbolKey voption
            Args: (TExpr * SemType * SyntaxToken) list
            /// A thunk: the walk a member head needs is wasted on any answer that expands the
            /// call, which is every answer but the rebuild.
            RebuiltHead: unit -> TExpr
        }

    /// A lambda argument eligible for inline-first elimination, with the descent it was WRITTEN
    /// under. That travels with it because the lambda is spliced at a use INSIDE the body it was
    /// passed to, where what it computes is still the caller's material — so a call it makes to
    /// the very binding it was passed to is a nested application and not a recursion.
    ///
    /// The descent itself and not a position in one: the splice site is an arbitrary number of
    /// bodies deeper than the capture, so any index taken here would be read against a chain the
    /// lambda never saw.
    [<NoEquality; NoComparison>]
    type internal FusedLambda = { Body: TExpr; Caller: Descent }

    /// What a call HEAD resolves to: the whole dispatch of the walker's application rule as one
    /// total answer. Three cases and not more — a template of this file and one another file
    /// served are the same reduction, and a cross-package call, a CLR/JS method and a
    /// higher-order parameter are all heads nothing here expands.
    [<RequireQualifiedAccess; NoEquality; NoComparison>]
    type internal CallHead =
        /// A head with an inline body, and the arguments that body's parameters are peeled against —
        /// which for a member head carries the receiver at curried position 0 and so is not the
        /// argument list the application was written with.
        | Template of id: TemplateId * body: TemplateBody * args: (TExpr * SemType * SyntaxToken) list
        /// A saturated use of an inline-first lambda parameter: the bound lambda is spliced at
        /// this use, so its closure never exists.
        | Fused of FusedLambda
        /// Nothing to expand; the head survives its application. A THUNK because the head a
        /// rebuild leaves standing is walked for a member (whose receiver nothing else walks) and
        /// must NOT be walked for a plain `External` — walking one etas it into the very closure
        /// this application is the saturated call of.
        | Opaque of rebuiltHead: (unit -> TExpr)

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
    /// arguments, substitute them through the body (which also selects its `StaticOptimization`
    /// clause and dispatches its `TraitCall`s), freshen the binders, and report every trait
    /// call the substitution could NOT dispatch.
    ///
    /// The SINGLE resolution entry — local and external, applied and bare — so no path can carry
    /// a body forward while quietly leaving an unresolvable `TraitCall` in it. Neither backend
    /// has a `TraitCall` arm, so an unreported one is an emitter crash. Reported at EVERY site,
    /// including one that goes on to reuse an entry another site interned: an unsupported
    /// operator is a fact about the site that wrote it, and the site is also the only anchor
    /// available (the expander is `PassContext`-free and sees the body before it has a position).
    ///
    /// Deriving the type arguments is not only for static-opt selection: for any generic inline
    /// it GROUNDS the body's typars to the caller's types. Without it, a typar reachable only
    /// through the body stays a free `TyVar` root of the CALLEE's scheme — beta-reduction binds
    /// the value params but never unifies that typar — and pollutes the caller's frozen TAST.
    ///
    /// Binders are freshened and the body is NOT moved: two expansions of one template must not
    /// share a codegen local slot, where keeping the positions the body was written at is the
    /// entry's whole purpose.
    let internal resolveAt
        (ctx: PassContext)
        (mint: unit -> NodeKey)
        (siteTok: SyntaxToken)
        (decl: TDecl)
        (args: (TExpr * SemType * SyntaxToken) list)
        : {| Body: TExpr; TypeArgs: SemType[] |} =
        match decl with
        | TDecl.Let(_, _, _, declTy) ->
            let typeArgs = Inline.deriveInlineTypeArgs ctx.Store declTy args
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

    /// Eta-reify an `External` function used as a VALUE — `(+)` in `List.fold (+) 0 xs`,
    /// `List.fold` itself in `let g = List.fold` — into `fun p0 p1 -> f p0 p1`, turning a
    /// function NAME into a closure. This is the sole eta in the compiler: codegen has none, so
    /// an `External` of function type never reaches a backend in value position.
    ///
    /// Doing it here rather than post-freeze is what lets an inline-bodied one finish: the
    /// saturated `App` the eta mints is claimed by THIS pass's own `App` arm, which resolves the
    /// body and its `StaticOptimization` against the context-pinned operand type. Reified after
    /// the freeze, that `App` would be minted past the last point its body can be reached. And
    /// only a pre-freeze eta reaches MEMBER bodies at all (codegen's lowering never walks them).
    ///
    /// It mints an `App` and NOT an `InlineCall` even when the reference has a body, because the
    /// `App` arm is the ONE place a call is resolved against a template; minting the edge here
    /// would resolve it a second time and let the two drift. This never recurses — the eta'd
    /// `App` re-presents the SAME `External` in call-HEAD position, where the `App` arm claims
    /// it before the value-position arm can see it.
    ///
    /// Arity is the reference's parameter count capped by the body's lambda arity, a partial eta
    /// (`fun x -> f x` for a 2-parameter `f` whose body abstracts once) still being type-correct.
    /// `ValueNone` at arity 0 — a non-function reference, which etas to nothing.
    let internal etaReify
        (ctx: PassContext)
        (mint: unit -> NodeKey)
        (body: TemplateBody voption)
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

                min (SemTypeQuery.Funs.count ctx.Store refTy) bodyArity
            | ValueNone -> SemTypeQuery.Funs.count ctx.Store refTy

        match arity with
        | 0 -> ValueNone
        | arity ->
            // Fresh binders come from the pass's own `mint`, so an eta site can never alias the
            // binders of the body about to be resolved at it.
            let binders =
                SemTypeQuery.Funs.domains ctx.Store arity refTy
                |> List.mapi (fun i pty -> mint (), pty, i)

            let appBody =
                binders
                |> List.fold
                    (fun acc (k, pty, i) ->
                        let resTy = SemTypeQuery.Funs.resultAfter ctx.Store (i + 1) refTy
                        TExpr.App(acc, TExpr.Var(k, pty, tok), resTy, tok)
                    )
                    (TExpr.External(name, keyOpt, refTy, tok))

            binders
            |> List.foldBack (fun (k, pty, _) (innerBody, innerTy) ->
                let lamTy = TyFun(pty, innerTy)
                TExpr.Lambda(TPat.NamedSimple(k, pty, tok), innerBody, lamTy, tok), lamTy
            )
            <| (appBody, SemTypeQuery.Funs.resultAfter ctx.Store arity refTy)
            |> fst
            |> ValueSome

    /// Peel a resolved inline body against one call site's arguments and DECIDE each parameter's
    /// fate — the half of the reduction that needs no recursion, and so the half that can run
    /// before a specialization slot is reserved for the body the recursion will build.
    ///
    /// The `FuseExternalValue` substitution happens HERE and not in the reduction, because the
    /// lambda classification below reads the substituted body. That is also why `caller` is
    /// needed: an argument substituted into a body has left the file it was written in, and is
    /// marked with it.
    let internal classifyApplication
        (caller: OriginFile)
        (paramAttrs: ParamAttrs[])
        (expanded: TExpr)
        (args: (TExpr * SemType * SyntaxToken) list)
        : Peeled =
        // Carry the template's own binder token so an entry's parameter keeps the position it
        // was written at. The application node's is not carried: the call site's position
        // belongs to the EDGE, and the `let` an argument is finally bound by is minted at
        // emission, where the site is the anchor the whole copy takes.
        let rec peel
            (fn: TExpr)
            (args: (TExpr * SemType * SyntaxToken) list)
            (acc: InlineParam list)
            : InlineParam list * TExpr =
            match fn, args with
            | _, [] -> List.rev acc, fn
            | TExpr.Lambda(TPat.NamedSimple(k, paramTy, patTok), body, _, _), (arg, _, _) :: rest ->
                peel
                    body
                    rest
                    ({
                        Key = k
                        Ty = paramTy
                        Arg = arg
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

        // A parameter bound to a bare `External` function value is substituted into the body
        // BEFORE the walk, which is sound because a value reference has no side effect and no
        // capture. A saturated `func arg` use then re-forms the head and the walk expands its
        // body; without this the binding survives as `let func = ignore in func arg`, leaving a
        // bare external value codegen cannot eta-expand.
        //
        // Recognised THROUGH any caller mark, and re-marked: an argument an OUTER fusion already
        // marked is still the bare external value this rule is about, two domains deep.
        let externalValParams =
            bindings
            |> List.choose (fun p ->
                match TastWalk.unmarked p.Arg with
                | TExpr.External _ -> Some(p.Key, TastWalk.callerExpr caller p.Arg)
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

        // Positionally aligned to the inline's curried parameters (freshen / typar-substitution
        // preserve order), so the index into `bindings` IS the curried position `ParamAttrs` is
        // indexed by.
        //
        // TODO(byref-capture): a SURVIVING lambda arg that captures a byref-like value (`Span`,
        // any `ref struct`) is a heap closure that cannot legally hold it, and today compiles to
        // one regardless. The fix is either a ref-struct closure or an F#-style rejection when
        // it genuinely escapes; both need a byref-like predicate that does not exist yet
        // (`SemType` has no ref-struct case and metadata drops byref params).
        let classified =
            bindings
            |> List.mapi (fun i p ->
                let disposition =
                    if externalKeys.Contains p.Key then
                        Disposition.FuseExternalValue
                    elif candidates.ContainsKey p.Key && not (bad.Contains p.Key) then
                        Disposition.FuseLambda
                    // Substituted at its single (declaration-validated linear) use instead of
                    // bound eagerly, so the argument is evaluated at most once and on demand —
                    // the mechanism behind `&&`/`||` short-circuiting, driven by the declared
                    // attribute rather than a body-shape guess. Everything else stays eager.
                    elif i < paramAttrs.Length && paramAttrs.[i].CallAtMostOnce then
                        Disposition.FuseAtMostOnce
                    else
                        Disposition.Survive

                { p with Disposition = disposition }
            )

        { Params = classified; Core = core }

    /// Reach a cross-file body by its resolved `SymbolKey` — the sole channel, routing a value
    /// key and a member key to the entry that CARRIES it, so the body and the identity cannot
    /// disagree. A member is selected by EXACT key, never by a name lookup whose best-by-arity
    /// collapse could serve a sibling overload's body.
    ///
    /// Every expandable head is key-stamped upstream (operators included — a primitive `1 + 2`
    /// head is keyed and DOES reach `ops-platform.fs`'s `(+)`), so a `key = ValueNone` head
    /// carries no inline body by construction and `ValueNone` is a genuine "no body", never a
    /// missed keyless lookup.
    ///
    /// The KEY rides out with the body because it is the table's template identity, and the
    /// lookup that found it is the only place that is known.
    ///
    /// The THAW happens here, per lookup, so two call sites of one template never share an
    /// inference cell. The body keeps the producer's own positions — it stays behind an edge, so
    /// its indices never have to mean anything against this file's tokens — and the producer
    /// file is retained on the table as it is read, being what those indices resolve against.
    let internal lookupExternal
        (ctx: PassContext)
        (specs: SpecTable)
        (keyOpt: SymbolKey voption)
        : TemplateBody voption =
        match keyOpt with
        | ValueSome key ->
            ExternalSymbolProviders.tryInlineBody ctx.Provider key
            |> ValueOption.map (fun ib ->
                let sources = SpecTable.retainOrigin ib.Origin specs

                {
                    Key = key
                    Decl = InlineThaw.bodyAtOrigin ctx.Store sources ib.Origin.File ib.Decl
                    ParamAttrs = ib.ParamAttrs
                    Origin = ib.Origin.File
                }
            )
        | ValueNone -> ValueNone
