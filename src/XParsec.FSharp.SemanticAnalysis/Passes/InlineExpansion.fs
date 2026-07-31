namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
// The table this pass builds, and the parameter vocabulary its reductions classify into.
open InlineSpecTable
// One reduction: what a served body is, where it will live, and how a spine resolves against it.
open InlineReduction

// The pre-freeze inline-expansion pass. Runs after elaboration and before the typar freeze,
// on the still `TyVar`-carrying `TExpr` tree, where `zonk` /
// union-find are native. A saturated use of a local `let inline` — or of a cross-unit
// `val inline` whose body the provider serves on the resolved entry
// (`ExternalSymbol.InlineBody` / `ExternalMember.InlineBody`) — is expanded + beta-reduced +
// static-opt resolved here, so the frozen module decls reaching codegen carry no inline call
// heads and no `StaticOptimization` nodes.
//
// What the walk LEAVES is a tree carrying edges — a `TExpr.InlineCall` per outlined call site
// — and the table those edges name, which is a DAG: an entry's own body carries edges. The
// edges SURVIVE the pass, and the freeze: placing the bodies is the backends' shared
// emit-time expansion, which splices them as it emits and keeps a frame
// chain naming the file each spliced node was written in. That is what deferral is for — a
// body flattened here has one anchor domain (the call site's) and nothing to say where it
// came from. What this pass owns is RESOLUTION, and what it hands out is the table.
//
// The table itself — its slots, its reuse pool and its assertions — is `InlineSpecTable`; ONE
// reduction — what a served body is, WHERE its nodes will live, how a call site's spine is
// resolved against it and its parameters classified — is `InlineReduction`; the
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
// template: the unwalked body is snapshotted into `ctx.InlineTemplates`
// first, because a template's static-opt clauses and trait calls must resolve against
// a CALL SITE's operand types, not against the nothing that is ground at its
// definition.

module InlineExpansion =

    /// Apply `f` to every expression a declaration carries: a module binding's value, a `do`
    /// expression, and — through the type-declaration mapper, at the type axis `id` — everything a
    /// type declaration holds. That covers member bodies, the class preambles (`[static] let`
    /// initialisers and `[static] do` bodies), secondary-ctor `let`s + chain args, and the
    /// `inherit Base(args)` arguments, and it covers them because the DECLARATION SHAPE says
    /// so, not because a traversal here remembered to.
    ///
    /// Parameterised over `f` because the pass makes TWO passes over this same coverage — the
    /// expansion walk, then the collection of the roots the finished table is checked against —
    /// and they must not be able to disagree about which expressions exist.
    let private mapDeclExprs (f: TExpr -> TExpr) (d: TDecl) : TDecl =
        match d with
        | TDecl.Let(p, value, isInline, ty) -> TDecl.Let(p, f value, isInline, ty)
        | TDecl.Expression(e, ty) -> TDecl.Expression(f e, ty)
        | TDecl.Type td -> TDecl.Type(TastWalk.mapTypeDecl id f td)

    /// What one run of the pass produced.
    ///
    /// `Decls` CARRY EDGES — a `TExpr.InlineCall` per outlined call site — and
    /// `Specializations` is the table those edges name. Placement is deferred to emission,
    /// which is what lets an entry's nodes keep the
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
    /// lookup returns `ValueNone`, so the walk is an identity rebuild — which the typar
    /// freeze does to every decl immediately after regardless, so there
    /// is no node-identity to preserve by skipping it.
    let run (ctx: PassContext) (decls: (TDecl * (TyVarId * SemType) list) list) : Expanded =

        // Parameter attributes for a *local* module-level inline, by binder key
        // (the cross-package twin rides `InlineBody.ParamAttrs`). Empty when the
        // inline declared no recognised parameter attribute.
        let localParamAttrs (k: NodeKey) : ParamAttrs[] =
            match ctx.InlineParamAttrs.TryGetValue k with
            | true, a -> a
            | _ -> [||]

        // Local module-level `let inline` bindings, keyed by binder NodeKey. A `Var(k)`
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

        // Only the degenerate empty-file case short-circuits. There is no "this provider
        // carries no inlines" fast path to take: every provider implements the channel, and a
        // file with no local inlines still reaches bodies served by the contract stack.
        // Skipping would save nothing anyway — `freezeTypars` rebuilds every tree next.
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

            // The anchor domain of the material walked under a frame chain. The empty chain is
            // this unit's own decls, which is where every walk starts.
            let originOf = ExpansionFrame.originOf ctx.Origin

            // THE protection against an inline binding that reaches itself. Such a binding cannot be
            // expanded, and the compiler reports instead of substituting
            // until stack overflow.
            //
            // Every reduction enters through here, so no path can recurse by having been overlooked, and
            // the answer is INSIDE it, so no call site can give a different one.
            //
            // A re-entered frame answers from its own `CycleReport`. A table reference carries
            // `Spine` — the same list that expansion peeled its parameters from — so it and the
            // entry agree on arity by construction.
            let expandingTemplate (frames: ExpansionFrame list) (call: PendingCall) (fresh: Entering -> TExpr) : TExpr =
                match frames |> List.tryFindIndex (fun f -> f.Template = call.Template) with
                | Some i ->
                    // At most one frame can match: a template found on the chain is answered
                    // rather than entered, so it is never on it twice. `frames` is innermost
                    // first, so the re-entered frame CLOSES the prefix through `i`, and that
                    // prefix reversed is the loop in call order — the binding that was re-entered,
                    // then each binding it called, out to the call closing it.
                    let chain = frames |> List.truncate (i + 1) |> List.rev
                    let reentered = List.head chain

                    match reentered.CycleReport with
                    | CycleReport.FromTable spec ->
                        TExpr.InlineCall(
                            spec,
                            EqArray.ofList [ for (a, _, _) in call.Spine -> call.Walk a ],
                            originOf frames,
                            call.Ty,
                            call.Tok
                        )
                    | CycleReport.AtThisCall ->
                        // Positioned at the OUTERMOST in-flight frame's site: a nested call's own
                        // token is a node of a producer's body (or of a copy moved onto that outer
                        // site), so the outermost frame's is the only one that names a place in
                        // the file being compiled. The chain is what `Kind.CyclicInline` names.
                        SpecTable.reportCycle (List.last frames).Site [ for f in chain -> f.Name ] specs
                        call.Rebuild()
                | None ->
                    // How a diagnostic spells the binding is a function of its IDENTITY alone, so
                    // it is taken here rather than at each call site: no frame can come to be
                    // named after a binding other than the one it expands.
                    let name =
                        match call.Template with
                        | TemplateId.Local k -> localName k
                        | TemplateId.Foreign key -> Inline.servedName key

                    fresh
                        {
                            Frame =
                                fun placement cycleReport ->
                                    {
                                        Own =
                                            {
                                                Template = call.Template
                                                Name = name
                                                Site = call.Tok
                                                Origin = Placement.domain (originOf frames) placement
                                                CycleReport = cycleReport
                                            }
                                        Caller = frames
                                    }
                            Caller = frames
                            Site =
                                match frames with
                                | [] -> call.Tok
                                | caller -> (List.last caller).Site
                        }

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
            //
            // In the walker's own `let rec` chain because it walks: every reduction resolves the
            // nested inline heads inside the body it is reducing.
            let rec reduceClassified (placement: Placement) (inFlight: InFlight) (peeled: Peeled) : Reduced =
                // Every fusion below is the CALLER's material, so it is marked with the caller's
                // domain and not this reduction's.
                let caller = originOf inFlight.Caller

                let fusedLambdas =
                    peeled.Params |> List.filter (fun p -> p.Disposition = Disposition.FuseLambda)

                // Marked UNDER its own binders, not around the whole lambda. Beta-reduction
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
                            Body = Inline.underLambdas (Placement.fuse caller placement) p.Arg
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
                            Inline.substituteVar
                                p.Key
                                (Placement.fuse caller placement (walkAt inFlight.Caller p.Arg))
                                body
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
            //
            // WHERE the call stands, WHAT it evaluates to and the spine its parameters are peeled
            // against are all read off `call` — the same record a re-entry reads — so the two
            // cannot be positioned or typed differently.
            //
            // Resolution and classification run BEFORE the frame is pushed. Neither walks the
            // body, so nothing can re-enter in the meantime, and the frame is therefore built
            // with its re-entry answer already settled.
            and expandExternalCall (entering: Entering) (served: ServedBody) (call: PendingCall) : TExpr =
                let resolved = resolveAt call.Tok served.Decl call.Spine

                // Read off the served body, ahead of the classification, so every fusion of one
                // reduction agrees about which file its material ends up in — and matched ONCE
                // below, so the origin an outlined reduction records is the very one that made
                // it outlined.
                let placement = Placement.ofOrigin served.Origin
                let caller = originOf entering.Caller

                let peeled =
                    classifyApplication caller placement served.ParamAttrs resolved.Body call.Spine

                match placement with
                | Placement.Spliced ->
                    letBound (reduceClassified placement (entering.Frame placement CycleReport.AtThisCall) peeled)
                | Placement.Outlined origin ->
                    SpecTable.outline
                        {
                            Site = entering.Site
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
                                && resolved.TypeArgs |> Array.forall (SemTypeQuery.isGround ctx.Store)
                            Origin = origin
                            EdgeTok = call.Tok
                            EdgeOrigin = caller
                            EdgeTy = call.Ty
                            ReuseArgs = fun () -> peeled.Params |> List.map (fun p -> walkAt entering.Caller p.Arg)
                            Build =
                                fun spec ->
                                    reduceClassified
                                        placement
                                        (entering.Frame placement (CycleReport.FromTable spec))
                                        peeled
                        }
                        specs

            // The same entry-and-edge for a cross-unit NULLARY INTRINSIC used as a VALUE: the
            // degenerate reduction, at arity 0, with a body that is one node and no survivors.
            // It goes through the ordinary outlining path precisely because those are the only
            // differences — the token the intrinsic was WRITTEN at is exactly what an entry
            // keeps, so a parallel notion of "a body from elsewhere" would have to re-derive it.
            and outlineNullaryIntrinsic
                (entering: Entering)
                (origin: OriginFile)
                (template: SymbolKey)
                (refTy: SemType)
                (tok: SyntaxToken)
                (body: TExpr)
                : TExpr =
                SpecTable.outline
                    {
                        Site = entering.Site
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
                        Shareable = SemTypeQuery.isGround ctx.Store refTy
                        Origin = origin
                        EdgeTok = tok
                        EdgeOrigin = originOf entering.Caller
                        EdgeTy = refTy
                        ReuseArgs = fun () -> []
                        Build = fun _ -> { Body = body; Survivors = [] }
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
            // This is the sole inline expander. Crucially the `App` arm is
            // ALWAYS handled explicitly (never falls through to `TastWalk`'s
            // default child recursion): collect the whole spine, keep an
            // `External` call head verbatim, and recurse only into the ARGS
            // (`rebuildApp head' (args |> walk)`). Relying on default recursion
            // would instead let the walker descend into a saturated op's
            // partial-application sub-`App` and expand it with a single arg —
            // leaving a dangling `fun y -> …` closure with a free `TyVar`.
            and mapperAt (frames: ExpansionFrame list) : TastWalk.Mapper =
                // The gate, bound to the chain THIS mapper's material was written under, so a
                // call site below names only the call it is answering.
                let expandingTemplate = expandingTemplate frames

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
                                let head = TastWalk.unmarked markedHead

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
                                    (template: TemplateId)
                                    (spine: (TExpr * SemType * SyntaxToken) list)
                                    (rebuild: unit -> TExpr)
                                    : PendingCall =
                                    {
                                        Template = template
                                        Tok = appTok
                                        Ty = TastWalk.exprTy e
                                        Spine = spine
                                        Walk = walk
                                        Rebuild = rebuild
                                    }

                                // How a cross-unit head presents itself, taken ONCE so the
                                // expansion behind it is written once — see `ExternalHead`.
                                // `ValueNone` is any other head.
                                let externalHead: ExternalHead voption =
                                    match head with
                                    | TExpr.External(_, keyOpt, _, _) ->
                                        ValueSome
                                            {
                                                Key = keyOpt
                                                Spine = spineArgs
                                                RebuiltHead = fun () -> markedHead
                                            }
                                    | TExpr.ExternalMember(receiver, key, _, _, _, memberTok) ->
                                        ValueSome
                                            {
                                                Key = ValueSome key
                                                Spine =
                                                    match receiver with
                                                    | ValueSome r -> (r, TastWalk.exprTy r, memberTok) :: spineArgs
                                                    | ValueNone -> spineArgs
                                                RebuiltHead = fun () -> walk markedHead
                                            }
                                    | _ -> ValueNone

                                match head with
                                | TExpr.Var(k, _, _) when localInlines.ContainsKey k ->
                                    ValueSome(
                                        expandingTemplate
                                            (pendingApp
                                                (TemplateId.Local k)
                                                spineArgs
                                                (fun () -> TastWalk.rebuildApp markedHead (walkedArgs ())))
                                            (fun entering ->
                                                letBound (
                                                    reduceClassified
                                                        // A same-unit template is MOVED onto the call
                                                        // site, so its body and the arguments fused
                                                        // into it are one anchor domain already.
                                                        Placement.Spliced
                                                        (entering.Frame Placement.Spliced CycleReport.AtThisCall)
                                                        (classifyApplication
                                                            (originOf entering.Caller)
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
                                // Classification only marks a parameter fusable when every
                                // use is saturated, so beta-reduction consumes exactly the
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
                                | _ ->
                                    match externalHead with
                                    | ValueSome ext ->
                                        match lookupExternal appTok ext.Key with
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
                                            // The one record BOTH answers read, so a re-entry and
                                            // a fresh reduction cannot mint differing edges.
                                            let call =
                                                pendingApp
                                                    (TemplateId.Foreign served.Key)
                                                    ext.Spine
                                                    (fun () -> TastWalk.rebuildApp (ext.RebuiltHead()) (walkedArgs ()))

                                            ValueSome(
                                                expandingTemplate
                                                    call
                                                    (fun entering -> expandExternalCall entering served call)
                                            )
                                        // No inline body — a real cross-package call, or a real
                                        // CLR/JS method: keep the head, lower the args, exactly
                                        // codegen's `head'` rule, left for its recipe path.
                                        | ValueNone ->
                                            ValueSome(TastWalk.rebuildApp (ext.RebuiltHead()) (walkedArgs ()))
                                    // A non-external, non-local-inline head (e.g. a
                                    // higher-order parameter): lower the head and args,
                                    // keeping the spine intact. The head SURVIVES here, so it is
                                    // rebuilt marked — only a rewrite that consumes the node
                                    // consumes its mark.
                                    | ValueNone -> ValueSome(TastWalk.rebuildApp (walk markedHead) (walkedArgs ()))
                            // A dispatched SRTP trait call — a static operator member whose
                            // body the provider serves. The INTRINSIC
                            // operator surface arrives here: `1 &&& 2` dispatches to
                            // `Vesper.int`'s declared `(&&&)`, and a primitive has no type to
                            // hang a method on, so its witness can only ever be spliced. A
                            // nominal's operator (`Vesper.Set`'s `op_Addition`) serves no body
                            // and falls through to the real call it is.
                            //
                            // Its arguments ARE the spine — a `StaticMethodCall` carries them
                            // itself rather than through an `App` chain — and they line up with
                            // the curried parameters `harvestMemberBody` wrapped the body in.
                            | TExpr.StaticMethodCall(key, args, ty, tok) ->
                                match lookupExternal tok (ValueSome key) with
                                | ValueSome served ->
                                    let call =
                                        {
                                            Template = TemplateId.Foreign served.Key
                                            Tok = tok
                                            Ty = ty
                                            Spine =
                                                [
                                                    for a in EqArray.toList args ->
                                                        a, TastWalk.exprTy a, TastWalk.exprTok a
                                                ]
                                            Walk = walk
                                            Rebuild =
                                                fun () -> TExpr.StaticMethodCall(key, EqArray.map walk args, ty, tok)
                                        }

                                    ValueSome(
                                        expandingTemplate call (fun inFlight -> expandExternalCall inFlight served call)
                                    )
                                // No served body: a real static call. Default child recursion
                                // walks the arguments.
                                | ValueNone -> ValueNone
                            // A BARE (non-applied) reference to a LOCAL inline — the
                            // template used as a value. No spine, so no type argument is
                            // derivable and the body's typars stay abstract; it still
                            // goes through `expandLocalAt` so its static-opt clauses resolve
                            // and any trait call it cannot dispatch is REPORTED rather
                            // than handed to a backend that has no arm for it.
                            | TExpr.Var(k, _, tok) when localInlines.ContainsKey k ->
                                ValueSome(
                                    expandingTemplate
                                        {
                                            Template = TemplateId.Local k
                                            Tok = tok
                                            Ty = TastWalk.exprTy e
                                            // No spine at all: a bare reference applies nothing,
                                            // so an edge carries no arguments and there is nothing
                                            // to leave unexpanded but the reference itself.
                                            Spine = []
                                            Walk = walk
                                            Rebuild = fun () -> e
                                        }
                                        (fun entering ->
                                            walkAt
                                                (InFlight.frames (
                                                    entering.Frame Placement.Spliced CycleReport.AtThisCall
                                                ))
                                                (expandLocalAt tok localInlines.[k] [])
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
                                                    {
                                                        Template = TemplateId.Foreign served.Key
                                                        Tok = tok
                                                        Ty = refTy
                                                        Spine = []
                                                        Walk = walk
                                                        Rebuild = fun () -> e
                                                    }
                                                    (fun entering ->
                                                        outlineNullaryIntrinsic
                                                            entering
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

            // The roots the finished table counts edges from — collected through `mapDeclExprs`
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
