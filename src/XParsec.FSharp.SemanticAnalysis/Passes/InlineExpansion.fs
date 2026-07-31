namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
// The table this pass builds, and the parameter vocabulary its reductions classify into.
open InlineSpecTable
// One reduction: what a template body is, and how a call site's spine resolves against it.
open InlineReduction

// The pre-freeze inline-expansion pass. Runs after elaboration and before the typar freeze, on
// the still `TyVar`-carrying `TExpr` tree, where `zonk` / union-find are native. A saturated use
// of a local `let inline` — or of a cross-file `val inline` whose body the provider serves on the
// resolved entry — is resolved and static-opt selected here, so the frozen module decls reaching
// codegen carry no inline call heads and no `StaticOptimization` nodes.
//
// What the walk LEAVES is a tree carrying edges — a `TExpr.InlineCall` per call site — and the
// table those edges name, which is a DAG: an entry's own body carries edges. EVERY expandable
// call becomes an edge, a same-file template's included: its body is anchored in the file being
// compiled and so has a domain to name like any other. The edges SURVIVE the pass, and the
// freeze: placing the bodies is the backends' shared emit-time expansion, which copies them as it
// emits and reads the file each copied node was written in off the node itself.
//
// What is left HERE is the WALK — which node is a call site, and which chain it was written
// under. The table is `InlineSpecTable`; ONE reduction is `InlineReduction`; the `SemType` /
// `TExpr` questions a resolution asks are `Inline`'s.
//
// Scope: EVERY module-level decl (`TDecl.Let` — `inline` or not — and `TDecl.Expression`) AND
// every expression a `TDecl.Type` carries (member bodies, `static let` inits, secondary-ctor
// `let`s + chain args, base-ctor args). An `inline` binding is walked because it is also EMITTED
// as an ordinary module function, and codegen's input invariant (no inline call heads, no
// `StaticOptimization`, no `External` used as a value) has to hold of that function like any
// other. The walked form is therefore the EMITTED one and NOT the published template: the
// unwalked body is snapshotted into `ctx.InlineTemplates` first, because a template's static-opt
// clauses and trait calls must resolve against a CALL SITE's operand types, not against the
// nothing that is ground at its definition.

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
    /// `Decls` CARRY EDGES — a `TExpr.InlineCall` per call site — and `Specializations` is the
    /// table those edges name. Placing a body is deferred to emission, which is what lets an
    /// entry's nodes keep the anchors they were written at instead of collapsing onto their
    /// call sites.
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

    /// Everything one run of the walk carries that is not a function of the node it stands at.
    ///
    /// Threaded rather than captured, because a capture is a dependency no signature states: the
    /// walk is a mutually recursive chain, and each link of it can now be read against what it is
    /// actually allowed to touch.
    [<NoEquality; NoComparison>]
    type private Expander =
        {
            Ctx: PassContext
            /// The resolved-specialization table this run builds, holding its own reuse pool, its
            /// own retained producer files and its own cycle bookkeeping.
            Specs: SpecTable
            /// Build-wide monotone counter for freshened inline binders: one counter for the whole
            /// run, so two expansions of the same template never mint the same key.
            Mint: unit -> NodeKey
            /// This file's own module-level `let inline` bindings, by binder key. A `Var` use of
            /// one is a local inline call site.
            LocalInlines: Dictionary<NodeKey, TemplateBody>
            /// Lambda arguments currently eligible for inline-first elimination, by the parameter
            /// binder they are bound to. Filled around the walk of the body that uses them; keys
            /// are `Mint`-fresh per expansion, so the map needs no structural scoping beyond that
            /// stack-disciplined add and remove.
            LambdaEnv: Dictionary<NodeKey, FusedLambda>
        }

    /// The anchor domain of the material walked at a descent. The empty chain is this file's
    /// own decls, which is where every walk starts.
    let private originOf (x: Expander) (at: Descent) : OriginFile = Descent.originOf x.Ctx.Origin at

    /// This file's module-level `let inline` bindings, in the form ONE reduction consumes.
    ///
    /// Off the INPUT decls, so a reduction always takes the TEMPLATE — the body as elaborated —
    /// never this pass's own walked rewrite of the same binding (which is the ordinary function
    /// that binding also emits, already resolved against its definition site and so wrong to
    /// expand anywhere else).
    let private collectLocalInlines
        (ctx: PassContext)
        (decls: (TDecl * (TyVarId * SemType) list) list)
        : Dictionary<NodeKey, TemplateBody> =
        // The identity a SAME-UNIT template's entry is keyed by: the very `SymbolKey` the freeze
        // publishes this same binding under, so one template has one identity whether the call
        // that resolved it was written in this file or in a consumer of it. Every module-level
        // binding that names something is registered — the freeze's own publish path reads the
        // same table — so an absence is a binding this pass should not have collected.
        let templateKey (head: TPat) : SymbolKey =
            match BinderKey.ofPat head with
            | ValueSome bk ->
                match ctx.Bindings.ModuleMembers.TryGetValue bk with
                | true, info -> info.Key
                | _ ->
                    failwithf
                        "InlineExpansion: the local inline bound at %A has no module-binding identity, so its specialization entry could name no template"
                        (TastWalk.patTok head)
            | ValueNone -> failwithf "InlineExpansion: a local inline's head is a named binder; got %A" head

        let locals = Dictionary<NodeKey, TemplateBody>()

        for (d, _) in decls do
            match d with
            | TDecl.Let(TPat.NamedSimple(b, _, _) as head, _, true, _) ->
                locals.[b] <-
                    {
                        Key = templateKey head
                        Decl = d
                        // The cross-package twin rides `InlineBody.ParamAttrs`. Empty when the
                        // inline declared no recognised parameter attribute.
                        ParamAttrs =
                            match ctx.InlineParamAttrs.TryGetValue b with
                            | true, a -> a
                            | _ -> [||]
                        Origin = ctx.Origin
                    }
            | _ -> ()

        locals

    /// THE protection against an inline binding that reaches itself. Such a binding cannot be
    /// expanded, and what stops the expansion here is that the call is answered from the frame —
    /// leaving a finite table for the acyclicity check to convict.
    ///
    /// Every reduction enters through here, so no path can recurse by having been overlooked, and
    /// the answer is INSIDE it, so no call site can give a different one.
    ///
    /// A re-entered frame is answered with an edge into the entry it reserved. The edge carries
    /// `Spine` — the same list that expansion peeled its parameters from — so it and the entry
    /// agree on arity by construction.
    let private expandingTemplate (x: Expander) (at: Descent) (call: PendingCall) (fresh: unit -> TExpr) : TExpr =
        match Descent.reentered call.Template at with
        | ValueSome reentered ->
            TExpr.InlineCall(
                reentered.Spec,
                EqArray.ofList [ for (a, _, _) in call.Spine -> call.Walk a ],
                originOf x at,
                call.Ty,
                call.Tok
            )
        | ValueNone -> fresh ()

    /// What THIS call head resolves to — the one dispatch of the application rule, so a head
    /// shape cannot be claimed by two answers or fall between them.
    let private callHead
        (x: Expander)
        (walk: TExpr -> TExpr)
        (markedHead: TExpr)
        (spineArgs: (TExpr * SemType * SyntaxToken) list)
        : CallHead =
        // Read THROUGH any caller mark: a fused external value in head position is still the head
        // it was before the fusion marked it, and a head that stopped being recognised would fall
        // to `Opaque` as a bare external no backend can call. The mark is consumed with the node —
        // an expansion replaces the head itself, so there is no subtree left for it to cover.
        match TastWalk.unmarked markedHead with
        | TExpr.Var(k, _, _) when x.LocalInlines.ContainsKey k ->
            CallHead.Template(TemplateId.Local k, x.LocalInlines.[k], spineArgs)
        | TExpr.Var(k, _, _) when x.LambdaEnv.ContainsKey k -> CallHead.Fused x.LambdaEnv.[k]
        | head ->
            // How a cross-file head presents itself, taken ONCE so the expansion behind it is
            // written once — see `ExternalHead`. `ValueNone` is any other head.
            let external: ExternalHead voption =
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

            match external with
            | ValueSome ext ->
                match lookupExternal x.Ctx x.Specs ext.Key with
                // An external WITH an inline body ALWAYS expands — no operand-groundness gate. An
                // un-ground `^T` simply selects no per-primitive `StaticOptimization` clause and
                // falls to the body's BASE, which is where the safe generic default lives
                // (`EqualityComparer<^T>.Default.Equals` for `=`). Declining instead routed the
                // head to a name-keyed raw-IL fallback, turning a structural `=` into a reference
                // `ceq`.
                | ValueSome served -> CallHead.Template(TemplateId.Foreign served.Key, served, ext.Spine)
                | ValueNone -> CallHead.Opaque ext.RebuiltHead
            | ValueNone -> CallHead.Opaque(fun () -> walk markedHead)

    /// The same entry-and-edge as an applied call for a cross-file NULLARY INTRINSIC used as a
    /// VALUE: the degenerate reduction, at arity 0, with a body that is one node and no survivors.
    /// It goes through the ordinary outlining path precisely because those are the only
    /// differences — the token the intrinsic was WRITTEN at is exactly what an entry keeps, so a
    /// parallel notion of "a body from elsewhere" would have to re-derive it.
    let private outlineNullaryIntrinsic
        (x: Expander)
        (at: Descent)
        (call: PendingCall)
        (template: TemplateBody)
        (body: TExpr)
        : TExpr =
        SpecTable.outline
            {
                Site = Descent.siteOf at call
                Grounding =
                    {
                        Key =
                            {
                                Template = template.Key
                                // The reference's own resolved type IS the grounding: it is what a
                                // generic `defaultof<'T>` is instantiated at, and the only thing a
                                // bare reference supplies.
                                TypeArgs = EqArray.ofArray [| call.Ty |]
                            }
                        // A value reference applies nothing, so the entry abstracts nothing and
                        // its edge carries no arguments.
                        Arity = 0
                    }
                Shareable = SemTypeQuery.isGround x.Ctx.Store call.Ty
                Origin = template.Origin
                EdgeTok = call.Tok
                EdgeOrigin = originOf x at
                EdgeTy = call.Ty
                ReuseArgs = fun () -> []
                Build = fun _ -> { Body = body; Survivors = [] }
            }
            x.Specs

    /// Finish a classified application: walk the body (which is where every nested inline head
    /// inside it resolves) and fuse in the call-site material the classification marked, leaving
    /// the surviving parameters and their walked arguments.
    ///
    /// The three fusions are all substitutions INTO the body, so their order relative to one
    /// another does not matter: a fused parameter's key is `Mint`-fresh and occurs only in the
    /// body, never in another parameter's argument.
    let rec private reduceClassified (x: Expander) (inFlight: InFlight) (peeled: Peeled) : Reduced =
        // Every fusion below is the CALLER's material, so it is marked with the caller's domain
        // and not this reduction's. Marked UNCONDITIONALLY, a trivial argument included: a uniform
        // invariant is checkable where one that skips `Var`s and constants is not.
        let caller = originOf x inFlight.Caller

        let fusedLambdas =
            peeled.Params |> List.filter (fun p -> p.Disposition = Disposition.FuseLambda)

        // Marked UNDER its own binders, not around the whole lambda. Beta-reduction consumes those
        // binders against arguments taken from the BODY the lambda is spliced into, so the `Let`s
        // that replace them belong to that body's file; only what the lambda computes was written
        // at the call site. (The binder PATTERN's own token stays with it and no expression marker
        // can cover it — the one position a fused lambda still attributes to the body's file.)
        //
        // The caller's chain is captured HERE, where the lambda is still the caller's argument,
        // and travels to the use site the walk splices it at (which is inside however many bodies
        // the walk has descended by then).
        for p in fusedLambdas do
            x.LambdaEnv.[p.Key] <-
                {
                    Body = Inline.underLambdas (TastWalk.callerExpr caller) p.Arg
                    Caller = inFlight.Caller
                }

        let core = walkAt x inFlight.Own peeled.Core

        for p in fusedLambdas do
            x.LambdaEnv.Remove p.Key |> ignore

        // Innermost parameter first, so the arguments are walked in the order the `let` nesting
        // binds them from the inside out.
        let mutable body = core
        let survivors = ResizeArray<InlineParam>()

        for p in List.rev peeled.Params do
            match p.Disposition with
            // Already substituted into `Core` / spliced at each use by the walk above: neither
            // carries a surviving binding.
            | Disposition.FuseExternalValue
            | Disposition.FuseLambda -> ()
            | Disposition.FuseAtMostOnce ->
                body <- Inline.substituteVar p.Key (TastWalk.callerExpr caller (walkAt x inFlight.Caller p.Arg)) body
            | Disposition.Survive ->
                survivors.Add
                    { p with
                        Arg = walkAt x inFlight.Caller p.Arg
                    }

        survivors.Reverse()

        {
            Body = body
            Survivors = List.ofSeq survivors
        }

    /// Answer ONE call site against ONE template — the gate FIRST, so a template already on the
    /// chain yields an edge into the entry it reserved instead of a second expansion of itself,
    /// and then an EDGE naming the resolved specialization. THE reduction: there is no path to
    /// one that does not come through here, so none can skip the gate.
    ///
    /// A template of this file and one another file served reach this by different lookups and
    /// are the same reduction thereafter — the whole reason `TemplateBody` says nothing about
    /// where its body came from.
    ///
    /// WHERE the call stands, WHAT it evaluates to and the spine its parameters are peeled against
    /// are all read off `call` — the same record a re-entry reads — so the two cannot be
    /// positioned or typed differently.
    ///
    /// Resolution and classification run BEFORE the frame is pushed. Neither walks the body, so
    /// nothing can re-enter in the meantime, and the frame is therefore built with its re-entry
    /// answer already settled.
    and private expandAt (x: Expander) (at: Descent) (template: TemplateBody) (call: PendingCall) : TExpr =
        expandingTemplate
            x
            at
            call
            (fun () ->
                let resolved = resolveAt x.Ctx x.Mint call.Tok template.Decl call.Spine
                let caller = originOf x at
                let peeled = classifyApplication caller template.ParamAttrs resolved.Body call.Spine

                SpecTable.outline
                    {
                        Site = Descent.siteOf at call
                        Grounding =
                            {
                                Key =
                                    {
                                        Template = template.Key
                                        TypeArgs = EqArray.ofArray resolved.TypeArgs
                                    }
                                Arity = List.length peeled.Params
                            }
                        Shareable =
                            Peeled.isClosed peeled
                            && resolved.TypeArgs |> Array.forall (SemTypeQuery.isGround x.Ctx.Store)
                        Origin = template.Origin
                        EdgeTok = call.Tok
                        EdgeOrigin = caller
                        EdgeTy = call.Ty
                        ReuseArgs = fun () -> peeled.Params |> List.map (fun p -> walkAt x at p.Arg)
                        Build = fun spec -> reduceClassified x (Descent.enter at call template.Origin spec) peeled
                    }
                    x.Specs
            )

    /// The expansion walker, as a FUNCTION of the descent the material it is handed was WRITTEN
    /// under — the only way the descent can be threaded, `TastWalk.Mapper` having no room for a
    /// parameter of its own. Descending into a callee's body builds a mapper at the descent with
    /// that callee's frame pushed; walking material the CALL SITE supplied builds one at the
    /// shorter descent that material belongs to. The default child recursion stays in the same
    /// material and so at the same descent, which is exactly what re-using `m` says.
    ///
    /// The `App` arm is ALWAYS handled explicitly (never falls through to `TastWalk`'s default
    /// child recursion): collect the whole spine and recurse only into the ARGS. Relying on
    /// default recursion would instead let the walker descend into a saturated op's
    /// partial-application sub-`App` and expand it with a single arg — leaving a dangling
    /// `fun y -> …` closure with a free `TyVar`.
    and private mapperAt (x: Expander) (at: Descent) : TastWalk.Mapper =
        { TastWalk.identityMapper with
            OverrideExpr =
                fun m e ->
                    let walk n = TastWalk.mapExpr m n

                    match e with
                    | TExpr.App _ ->
                        let markedHead, spineArgs = TastWalk.collectSpine [] e

                        match callHead x walk markedHead spineArgs with
                        | CallHead.Template(id, body, spine) ->
                            let call =
                                {
                                    Template = id
                                    // A rewrite inherits the position of the node it REPLACES,
                                    // never one of that node's children, and what an expansion
                                    // stands in for is the whole APPLICATION. Reading the HEAD's
                                    // token instead is wrong wherever the two differ: they differ
                                    // exactly when an outer fusion substituted the head in, so the
                                    // head was written at the CALL SITE while the application
                                    // around it is the producer's own material, and an edge
                                    // anchored at the head would sit inside an entry claiming a
                                    // position in a file that entry does not name.
                                    Tok = TastWalk.exprTok e
                                    Ty = TastWalk.exprTy e
                                    Spine = spine
                                    Walk = walk
                                }

                            ValueSome(expandAt x at body call)
                        // Beta-reduced against the call args and walked, so nested inline heads and
                        // further lambda parameters resolve in the recursion.
                        //
                        // The copy keeps its OWN positions: what is copied is the CALL SITE's own
                        // argument, and its tokens are strictly finer than the call's. They are
                        // also the anchor a `FunVerdicts` entry for it is filed under, and these
                        // copies are the only ones pooled: an inlined-away parameter keeps no
                        // surviving `let`.
                        | CallHead.Fused fused ->
                            ValueSome(
                                walkAt x fused.Caller (Inline.betaReduce (Inline.freshen x.Mint fused.Body) spineArgs)
                            )
                        // The spine walked as the CALLER's own material, which every rebuild needs
                        // and no expansion does.
                        | CallHead.Opaque rebuiltHead ->
                            ValueSome(
                                TastWalk.rebuildApp
                                    (rebuiltHead ())
                                    [ for (a, ty, tok) in spineArgs -> walk a, ty, tok ]
                            )
                    // A dispatched SRTP trait call — a static operator member whose body the
                    // provider serves. The INTRINSIC operator surface arrives here: `1 &&& 2`
                    // dispatches to `Vesper.int`'s declared `(&&&)`, and a primitive has no type to
                    // hang a method on, so its witness can only ever be spliced. A nominal's
                    // operator (`Vesper.Set`'s `op_Addition`) serves no body and falls through to
                    // the real call it is.
                    //
                    // Its arguments ARE the spine — a `StaticMethodCall` carries them itself rather
                    // than through an `App` chain — and they line up with the curried parameters
                    // the lifted member body was wrapped in.
                    | TExpr.StaticMethodCall(key, args, ty, tok) ->
                        match lookupExternal x.Ctx x.Specs (ValueSome key) with
                        | ValueSome served ->
                            let call =
                                {
                                    Template = TemplateId.Foreign served.Key
                                    Tok = tok
                                    Ty = ty
                                    Spine = [ for a in EqArray.toList args -> a, TastWalk.exprTy a, TastWalk.exprTok a ]
                                    Walk = walk
                                }

                            ValueSome(expandAt x at served call)
                        // No served body: a real static call. Default child recursion walks the
                        // arguments.
                        | ValueNone -> ValueNone
                    // A BARE (non-applied) reference to a LOCAL inline — the template used as a
                    // value. The degenerate reduction, at arity 0: no spine, so no type argument is
                    // derivable and the body's typars stay abstract, but it is the SAME expansion
                    // as an applied site (which is what a peel against no arguments comes to) and
                    // so resolves its static-opt clauses and reports any trait call it cannot
                    // dispatch rather than handing one to a backend with no arm.
                    | TExpr.Var(k, _, tok) when x.LocalInlines.ContainsKey k ->
                        let call =
                            {
                                Template = TemplateId.Local k
                                Tok = tok
                                Ty = TastWalk.exprTy e
                                Spine = []
                                Walk = walk
                            }

                        ValueSome(expandAt x at x.LocalInlines.[k] call)
                    // A BARE (non-applied) reference to a cross-package `let` value whose body is a
                    // single zero-operand intrinsic (`undefined`, `defaultof`): the intrinsic body
                    // stands in place of the `External` reference, so codegen emits the bare
                    // intrinsic with no import and never a `const undefined = undefined`
                    // definition. A nullary intrinsic value cannot be applied, so this never
                    // collides with the `App`-head inline paths above.
                    //
                    // ANY OTHER external of function type in value position is a function name used
                    // as a value (`List.fold (+) 0 xs`, or `List.fold` itself): eta-reify it into a
                    // closure and walk the result, so the `App` arm above expands the body (when
                    // there is one) at the freshly-minted call head. This is the compiler's only
                    // eta — an inline-bodied external and a plain one take the SAME path, differing
                    // only in whether the `App` finds a body to expand. An external of non-function
                    // type (`System.Int32.MaxValue`) etas to nothing and stays a leaf.
                    | TExpr.External(name, keyOpt, refTy, tok) ->
                        let body = lookupExternal x.Ctx x.Specs keyOpt

                        // A nullary intrinsic is a single node with no binders, so it needs no
                        // expansion — but it crosses the same file boundary every other served body
                        // does, and so takes the same entry-and-edge. Anything else — no served
                        // body, or one that is not a bare intrinsic — takes the eta below, so the
                        // two are ONE answer and not a served/unserved split.
                        let intrinsic =
                            body
                            |> ValueOption.bind (fun served ->
                                match Inline.nullaryIntrinsicValueBody served.Decl with
                                | ValueSome(TExpr.ILIntrinsic(op, operand, args, _, at)) ->
                                    // A GENERIC nullary intrinsic carries its own scheme typar,
                                    // which only the reference's type can ground. Without this the
                                    // callee typar survives as an unbound `TyVar` at the freeze. A
                                    // NON-generic one (`undefined`) is unchanged: `refTy` equals
                                    // its concrete result type and it carries no operand.
                                    let groundedOperand =
                                        match operand with
                                        | ValueSome _ -> ValueSome refTy
                                        | ValueNone -> ValueNone

                                    ValueSome
                                        {|
                                            Template = served
                                            Body = TExpr.ILIntrinsic(op, groundedOperand, args, refTy, at)
                                        |}
                                | _ -> ValueNone
                            )

                        match intrinsic with
                        | ValueSome hit ->
                            // In flight like every other served body, so the reservation has the
                            // one frame it publishes its slot on. The recursive answer is
                            // unreachable rather than absent — an intrinsic with no operands names
                            // nothing, so nothing inside it can lead back here — and going through
                            // the gate costs a line where a special case would cost the uniformity
                            // that makes the gate total.
                            let call =
                                {
                                    Template = TemplateId.Foreign hit.Template.Key
                                    Tok = tok
                                    Ty = refTy
                                    Spine = []
                                    Walk = walk
                                }

                            ValueSome(
                                expandingTemplate
                                    x
                                    at
                                    call
                                    (fun () -> outlineNullaryIntrinsic x at call hit.Template hit.Body)
                            )
                        | ValueNone -> etaReify x.Ctx x.Mint body name keyOpt refTy tok |> ValueOption.map walk
                    | _ -> ValueNone
        }

    /// Walk `e` as material written at `at`.
    and private walkAt (x: Expander) (at: Descent) (e: TExpr) : TExpr = TastWalk.mapExpr (mapperAt x at) e

    /// Expand the module-level inlines in one decl-list (the elaborated, `TyVar`-carrying decls
    /// paired with their freeze envs). The cross-file inline-body channel is `ctx.Provider`
    /// itself — the body rides the resolved entry, reached by the key the use-site node carries;
    /// a front-end-only provider serves none and every lookup returns `ValueNone`, so the walk is
    /// an identity rebuild — which the typar freeze does to every decl immediately after
    /// regardless, so there is no node-identity to preserve by skipping it.
    let run (ctx: PassContext) (decls: (TDecl * (TyVarId * SemType) list) list) : Expanded =
        // Only the degenerate empty-file case short-circuits. There is no "this provider carries
        // no inlines" fast path to take: every provider implements the channel, and a file with no
        // local inlines still reaches bodies served by the contract stack. Skipping would save
        // nothing anyway — `freezeTypars` rebuilds every tree next.
        if List.isEmpty decls then
            {
                Decls = decls
                Specializations = [||]
            }
        else
            let mutable counter = 0

            let mint () =
                let k = NodeKey.ofSyntheticCounter counter NodeKind.SynthPreFreezeInline
                counter <- counter + 1
                k

            let x =
                {
                    Ctx = ctx
                    Specs = SpecTable.create (fun tok kind -> ctx.Report(tok, kind)) mint
                    Mint = mint
                    LocalInlines = collectLocalInlines ctx decls
                    LambdaEnv = Dictionary()
                }

            // The file's OWN declarations are inside no expansion, so they are walked at the top
            // descent — which is also why every site a descent reached from here carries is a
            // position in this file.
            let expanded =
                decls |> List.map (fun (d, env) -> mapDeclExprs (walkAt x Descent.top) d, env)

            // The roots the finished table counts edges from — collected through `mapDeclExprs` so
            // "which expressions does a declaration carry?" is answered ONCE for the whole pass; a
            // hand-written second traversal is how one of them comes to miss a slot.
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
                Specializations = SpecTable.finish declExprs x.Specs
            }
