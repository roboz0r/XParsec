namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open InlineSpecTable
open InlineReduction

// The pre-freeze inline-expansion pass, on the still `TyVar`-carrying `TExpr` tree. A saturated
// use of a `let inline` — local, or cross-file with a served body — is resolved and static-opt
// selected here, leaving a `TExpr.InlineCall` edge the backends place the body for at emit time.

module InlineExpansion =

    let private mapDeclExprs (f: TExpr -> TExpr) (d: TDecl) : TDecl =
        match d with
        | TDecl.Let(p, value, isInline, ty) -> TDecl.Let(p, f value, isInline, ty)
        | TDecl.Expression(e, ty) -> TDecl.Expression(f e, ty)
        | TDecl.Type td -> TDecl.Type(TastWalk.mapTypeDecl id f td)

    /// `Decls` CARRY EDGES — a `TExpr.InlineCall` per call site — and `Specializations` is the
    /// table those edges name.
    type Expanded =
        {
            Decls: (TDecl * (TyVarId * SemType) list) list
            /// Slot order — a `SpecializationId` indexes THIS array.
            Specializations: TSpecialization[]
        }

    /// Everything one run of the walk carries that is not a function of the node it stands at.
    [<NoEquality; NoComparison>]
    type private Expander =
        {
            Ctx: PassContext
            Specs: SpecTable
            /// Fresh keys for inline binders: ONE counter for the whole run, so two expansions of
            /// the same template never mint the same key.
            Mint: unit -> NodeKey
            /// This file's own module-level `let inline` bindings, by binder key. A `Var` use of
            /// one is a local inline call site.
            LocalInlines: Dictionary<NodeKey, TemplateBody>
            /// Lambda arguments currently eligible for inline-first elimination, by the parameter
            /// binder they are bound to. Added and removed around the walk of the body using them.
            LambdaEnv: Dictionary<NodeKey, FusedLambda>
        }

    let private originOf (x: Expander) (at: Descent) : OriginFile = Descent.originOf x.Ctx.Origin at

    /// This file's module-level `let inline` bindings. Off the INPUT decls, so a reduction takes
    /// the TEMPLATE as elaborated, never this pass's own walked rewrite of it.
    let private collectLocalInlines
        (ctx: PassContext)
        (decls: (TDecl * (TyVarId * SemType) list) list)
        : Dictionary<NodeKey, TemplateBody> =
        // The very `SymbolKey` the freeze publishes this binding under, so one template has one
        // identity whether the call that resolved it is in this file or in a consumer of it.
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
                        // Empty when the inline declared no recognised parameter attribute.
                        ParamAttrs =
                            match ctx.InlineParamAttrs.TryGetValue b with
                            | true, a -> a
                            | _ -> [||]
                        Origin = ctx.Origin
                    }
            | _ -> ()

        locals

    /// A re-entered frame is answered with an edge into the entry it reserved rather than a
    /// second expansion — how an inline binding that reaches itself leaves a finite table.
    let private expandingTemplate (x: Expander) (at: Descent) (call: PendingCall) (fresh: unit -> TExpr) : TExpr =
        match Descent.reentered call.Template at with
        | ValueSome reentered ->
            TExpr.InlineCall(
                reentered.Spec,
                EqArray.ofList [ for (a, _, _) in call.Args -> call.Walk a ],
                originOf x at,
                call.Ty,
                call.Tok
            )
        | ValueNone -> fresh ()

    /// A METHOD call's applied arguments OPENED to the parameters the lifted body curried: the
    /// call applies ONE tuple whatever the parameter count, while the lift wraps one lambda per
    /// parameter. `ValueNone` is an argument that is not the literal tuple its arity needs.
    let private untupleMemberArgs
        (key: SymbolKey)
        (memberName: string)
        (storage: MemberStorage)
        (args: (TExpr * SemType * SyntaxToken) list)
        : (TExpr * SemType * SyntaxToken) list voption =
        let asTuple (arg: TExpr, _, _) =
            match arg with
            | TExpr.Tuple(items, _, _) ->
                ValueSome
                    [
                        for it in EqArray.toList items -> it, TastWalk.exprTy it, TastWalk.exprTok it
                    ]
            | _ -> ValueNone

        match storage, args with
        | MemberStorage.Method, first :: rest ->
            let arity =
                SymbolKeyOps.memberArity (sprintf "InlineExpansion: member '%s'" memberName) key

            SymbolKeyOps.openTupledArg asTuple arity first
            |> ValueOption.map (fun opened -> opened @ rest)
        | _ -> ValueSome args

    let private callHead
        (x: Expander)
        (walk: TExpr -> TExpr)
        (markedHead: TExpr)
        (args: (TExpr * SemType * SyntaxToken) list)
        : CallHead =
        // Read THROUGH any caller mark: a fused external value in head position is still the head
        // it was before the fusion marked it.
        match TastWalk.unmarked markedHead with
        | TExpr.Var(k, _, _) when x.LocalInlines.ContainsKey k ->
            CallHead.Template(TemplateId.Local k, x.LocalInlines.[k], args)
        | TExpr.Var(k, _, _) when x.LambdaEnv.ContainsKey k -> CallHead.Fused x.LambdaEnv.[k]
        | head ->
            // How a cross-file head presents itself; `ValueNone` is any other head.
            let external: ExternalHead voption =
                match head with
                | TExpr.External(_, keyOpt, _, _) ->
                    ValueSome
                        {
                            Key = keyOpt
                            Args = ValueSome args
                            RebuiltHead = fun () -> markedHead
                        }
                | TExpr.ExternalMember(receiver, key, memberName, storage, _, memberTok) ->
                    ValueSome
                        {
                            Key = ValueSome key
                            Args =
                                untupleMemberArgs key memberName storage args
                                |> ValueOption.map (fun opened ->
                                    match receiver with
                                    | ValueSome r -> (r, TastWalk.exprTy r, memberTok) :: opened
                                    | ValueNone -> opened
                                )
                            RebuiltHead = fun () -> walk markedHead
                        }
                | _ -> ValueNone

            match external with
            | ValueSome ext ->
                match lookupExternal x.Ctx x.Specs ext.Key, ext.Args with
                // An external WITH an inline body ALWAYS expands — no operand-groundness gate. An
                // un-ground `^T` selects no per-primitive `StaticOptimization` clause and falls to
                // the body's BASE, where the safe generic default lives (`=` → `Equals`).
                | ValueSome served, ValueSome opened -> CallHead.Template(TemplateId.Foreign served.Key, served, opened)
                // A body to splice and no parameters to splice it against: Elaborate opens every
                // member argument to the width its key declares, so this node is malformed.
                | ValueSome served, ValueNone ->
                    failwithf
                        "InlineExpansion: the spliced member %A was applied to an argument its declared parameters cannot be bound to"
                        served.Key
                | ValueNone, _ -> CallHead.Opaque ext.RebuiltHead
            | ValueNone -> CallHead.Opaque(fun () -> walk markedHead)

    /// The same entry-and-edge as an applied call, for a cross-file NULLARY INTRINSIC used as a
    /// VALUE: the degenerate reduction, at arity 0, with a body that is one node and no survivors.
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
                                // The reference's own resolved type IS the grounding: what a
                                // generic `defaultof<'T>` is instantiated at.
                                TypeArgs = EqArray.ofArray [| call.Ty |]
                            }
                        // A value reference applies nothing, so the entry abstracts nothing.
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

    /// Finish a classified application: walk the body — where every nested inline head inside it
    /// resolves — and fuse in the call-site material the classification marked.
    let rec private reduceClassified (x: Expander) (inFlight: InFlight) (peeled: Peeled) : Reduced =
        // Every fusion below is the CALLER's material, so it carries the caller's domain.
        let caller = originOf x inFlight.Caller

        let fusedLambdas =
            peeled.Params |> List.filter (fun p -> p.Disposition = Disposition.FuseLambda)

        // Marked UNDER its own binders: beta-reduction consumes those binders against arguments
        // from the BODY the lambda is spliced into, so only what it computes came from the call.
        for p in fusedLambdas do
            x.LambdaEnv.[p.Key] <-
                {
                    Body = Inline.underLambdas (TastWalk.callerExpr caller) p.Arg
                    Caller = inFlight.Caller
                }

        let core = walkAt x inFlight.Own peeled.Core

        for p in fusedLambdas do
            x.LambdaEnv.Remove p.Key |> ignore

        // Innermost parameter first, matching the order the `let` nesting binds them.
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

    /// Answer ONE call site against ONE template — of this file or served by another, which differ
    /// only in the lookup that found them. Resolution and classification run BEFORE the frame is
    /// pushed.
    and private expandAt (x: Expander) (at: Descent) (template: TemplateBody) (call: PendingCall) : TExpr =
        expandingTemplate
            x
            at
            call
            (fun () ->
                let resolved = resolveAt x.Ctx x.Mint call.Tok template.Decl call.Args
                let caller = originOf x at
                let peeled = classifyApplication caller template.ParamAttrs resolved.Body call.Args

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
    /// under — a `Mapper` has no room for a parameter of its own. Descending into a callee's body
    /// builds a mapper with that callee's frame pushed; call-site material keeps its own descent.
    and private mapperAt (x: Expander) (at: Descent) : TastWalk.Mapper =
        { TastWalk.identityMapper with
            OverrideExpr =
                fun m e ->
                    let walk n = TastWalk.mapExpr m n

                    match e with
                    // Collect the whole application and recurse only into the ARGS. Default
                    // child recursion would instead descend into a saturated op's
                    // partial-application sub-`App` and expand it with a single arg.
                    | TExpr.App _ ->
                        let markedHead, appArgs = TastWalk.collectAppChain [] e

                        match callHead x walk markedHead appArgs with
                        | CallHead.Template(id, body, templateArgs) ->
                            let call =
                                {
                                    Template = id
                                    // A rewrite inherits the position of the node it REPLACES: the
                                    // whole APPLICATION, not its head. The two differ when an
                                    // outer fusion substituted a CALL SITE head into this body.
                                    Tok = TastWalk.exprTok e
                                    Ty = TastWalk.exprTy e
                                    Args = templateArgs
                                    Walk = walk
                                }

                            ValueSome(expandAt x at body call)
                        // Beta-reduced against the call args and walked, so nested inline heads
                        // resolve in the recursion. The copy keeps its OWN positions: it is the
                        // CALL SITE's argument, whose tokens anchor its `FunVerdicts` entry.
                        | CallHead.Fused fused ->
                            ValueSome(
                                walkAt x fused.Caller (Inline.betaReduce (Inline.freshen x.Mint fused.Body) appArgs)
                            )
                        // A rebuild walks the arguments as the CALLER's own material.
                        | CallHead.Opaque rebuiltHead ->
                            ValueSome(
                                TastWalk.rebuildApp (rebuiltHead ()) [ for (a, ty, tok) in appArgs -> walk a, ty, tok ]
                            )
                    // A dispatched SRTP trait call, whose body the provider serves. The INTRINSIC
                    // operator surface arrives here: `1 &&& 2` dispatches to `Vesper.int`'s
                    // `(&&&)`, and a primitive has no type to hang a method on.
                    | TExpr.StaticMethodCall(key, args, ty, tok) ->
                        match lookupExternal x.Ctx x.Specs (ValueSome key) with
                        | ValueSome served ->
                            let call =
                                {
                                    Template = TemplateId.Foreign served.Key
                                    Tok = tok
                                    Ty = ty
                                    Args = [ for a in EqArray.toList args -> a, TastWalk.exprTy a, TastWalk.exprTok a ]
                                    Walk = walk
                                }

                            ValueSome(expandAt x at served call)
                        // No served body: a real static call, arguments walked by default.
                        | ValueNone -> ValueNone
                    // A BARE reference to a LOCAL inline — the template used as a value. At arity
                    // 0 no type argument is derivable and the body's typars stay abstract, but it
                    // is the SAME expansion, so static-opt clauses still resolve.
                    | TExpr.Var(k, _, tok) when x.LocalInlines.ContainsKey k ->
                        let call =
                            {
                                Template = TemplateId.Local k
                                Tok = tok
                                Ty = TastWalk.exprTy e
                                Args = []
                                Walk = walk
                            }

                        ValueSome(expandAt x at x.LocalInlines.[k] call)
                    // A BARE cross-package `let` whose body is one zero-operand intrinsic
                    // (`undefined`, `defaultof`) is replaced by it, so codegen emits no import.
                    // Any OTHER function-typed external etas into a closure the `App` arm expands.
                    | TExpr.External(name, keyOpt, refTy, tok) ->
                        let body = lookupExternal x.Ctx x.Specs keyOpt

                        // A nullary intrinsic needs no expansion, but crosses the same file
                        // boundary every other served body does, so it takes the same edge.
                        let intrinsic =
                            body
                            |> ValueOption.bind (fun served ->
                                match Inline.nullaryIntrinsicValueBody served.Decl with
                                | ValueSome(TExpr.ILIntrinsic(op, operand, args, _, at)) ->
                                    // A GENERIC nullary intrinsic's own scheme typar can only be
                                    // grounded by the reference's type; without this it survives
                                    // as an unbound `TyVar` at the freeze. `undefined` has none.
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
                            // one frame it publishes its slot on.
                            let call =
                                {
                                    Template = TemplateId.Foreign hit.Template.Key
                                    Tok = tok
                                    Ty = refTy
                                    Args = []
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

    /// Expand the module-level inlines in one decl-list (elaborated, `TyVar`-carrying decls paired
    /// with their freeze envs). A cross-file body rides the resolved provider entry, reached by
    /// the key the use-site node carries; a provider serving none makes this an identity rebuild.
    let run (ctx: PassContext) (decls: (TDecl * (TyVarId * SemType) list) list) : Expanded =
        // Only the degenerate empty-file case short-circuits: a file with no local inlines still
        // reaches bodies served by the contract stack.
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
            // descent.
            let expanded =
                decls |> List.map (fun (d, env) -> mapDeclExprs (walkAt x Descent.top) d, env)

            // The roots the finished table counts edges from.
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
