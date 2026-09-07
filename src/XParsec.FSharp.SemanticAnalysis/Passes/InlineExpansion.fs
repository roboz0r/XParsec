namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open InlineSpecTable
open InlineReduction

// The pre-freeze inline-expansion pass, on the still `TyVar`-carrying `TExpr` tree. A saturated
// use of a `let inline` (local, or cross-file with a served body) is resolved and static-opt
// selected here, leaving a `TExpr.InlineCall` edge the backends place the body for at emit time.

module InlineExpansion =

    /// `Decls` CARRY EDGES, one `TExpr.InlineCall` per call site, and `Specializations` is the
    /// table those edges name.
    type Expanded =
        {
            Decls: (TDecl * DeclEnv) list
            /// A `SpecializationId` indexes THIS array.
            Specializations: TSpecialization[]
        }

    /// Everything one run of the walk carries that is not a function of the node it stands at.
    [<NoEquality; NoComparison>]
    type private Expander =
        {
            Ctx: PassContext
            Specs: SpecTable
            /// Fresh keys for inline bound variables: ONE counter for the whole run, so two expansions of
            /// the same template never mint the same key.
            Mint: unit -> NodeKey
            /// This file's own module-level `let inline` bindings, by bound variable key. A `Var` use of
            /// one is a local inline call site.
            LocalInlines: Dictionary<NodeKey, TemplateBody>
        }

    let private pathOf (x: Expander) (at: Descent) : AssemblyFilePath = Descent.pathOf x.Ctx.File.Path at

    /// This file's module-level `let inline` bindings. Off the INPUT decls, so a reduction takes
    /// the TEMPLATE as elaborated, never this pass's own walked rewrite of it.
    let private collectLocalInlines
        (ctx: PassContext)
        (decls: (TDecl * DeclEnv) list)
        : Dictionary<NodeKey, TemplateBody> =
        // The very `SymbolKey` the freeze publishes this binding under, so one template has one
        // identity whether the call that resolved it is in this file or in a consumer of it.
        let templateKey (pattern: TPat) : SymbolKey =
            match BoundVarKey.ofPat pattern with
            | ValueSome bk ->
                match ctx.Bindings.ModuleMembers.TryGetValue bk with
                | true, info -> info.Key
                | _ ->
                    failwithf
                        "InlineExpansion: the local inline bound at %A has no module-binding identity, so its specialization entry could not name a template"
                        (TastWalk.patTok pattern)
            | ValueNone -> failwithf "InlineExpansion: a local inline's pattern binds no single name; got %A" pattern

        let locals = Dictionary<NodeKey, TemplateBody>()

        for (d, env) in decls do
            match d with
            | TDecl.Let(TPat.NamedSimple(b, _, _, _) as pattern, _, true, _, _) ->
                locals.[b] <-
                    {
                        Key = templateKey pattern
                        Decl = d
                        Typars = ElaborateTypars.quantifiedRoots env.All
                        // Empty when the inline declared no recognised parameter attribute.
                        ParamAttrs =
                            match ctx.InlineParamAttrs.TryGetValue b with
                            | true, a -> a
                            | _ -> EqArray.empty
                        Path = ctx.File.Path
                    }
            | _ -> ()

        locals

    /// A re-entered frame yields an edge into the entry it reserved rather than a second
    /// expansion, keeping the table finite for an inline binding that reaches itself.
    let private expandingTemplate (x: Expander) (at: Descent) (call: PendingCall) (fresh: unit -> TExpr) : TExpr =
        match Descent.reentered call.Template at with
        | ValueSome reentered ->
            TExpr.InlineCall(
                reentered.Spec,
                EqArray.ofList [ for a in call.Args -> call.Walk a.Arg ],
                pathOf x at,
                call.Ty,
                call.Tok
            )
        | ValueNone -> fresh ()

    /// An argument level for a call that was not written as an `App` chain (a member's object
    /// argument, a tuple element, a static call's argument): its result type is `a`'s own.
    let private levelOf (a: TExpr) (tok: SyntaxToken) : TastWalk.AppArg =
        {
            Arg = a
            AppResultTy = TastWalk.exprTy a
            AppTok = tok
        }

    /// A METHOD call's applied arguments opened to the parameters the lifted body curried: the
    /// lift wraps one lambda per PARAMETER, so `M(a, b)`'s one tuple and the curried `M a b`'s
    /// two arguments both arrive as `[a; b]`, with residual arguments passed through unopened.
    let private untupleMemberArgs
        (storage: MemberStorage)
        (widths: EqArray<int>)
        (args: TastWalk.AppArg list)
        : TastWalk.AppArg list voption =
        let asTuple (a: TastWalk.AppArg) : TastWalk.AppArg list voption =
            match a.Arg with
            | TExpr.Tuple(items, _, _) ->
                ValueSome [ for it in EqArray.toList items -> levelOf it (TastWalk.exprTok it) ]
            | _ -> ValueNone

        match storage with
        | MemberStorage.Method ->
            SymbolKeyOps.openArgGroups asTuple widths args
            |> ValueOption.map (fun opened -> opened.Flat @ opened.Residual)
        | _ -> ValueSome args

    let private appliedFunction
        (x: Expander)
        (at: Descent)
        (walk: TExpr -> TExpr)
        (markedFn: TExpr)
        (args: TastWalk.AppArg list)
        : AppliedFunction =
        // Read THROUGH any caller mark: a fused external value in function position is still the
        // function it was before the fusion marked it.
        match TastWalk.unmarked markedFn with
        | TExpr.Var(k, _, _) when x.LocalInlines.ContainsKey k ->
            AppliedFunction.Template(TemplateId.Local k, x.LocalInlines.[k], args)
        | TExpr.Var(Descent.Lambda at fused, _, _) -> AppliedFunction.Fused fused
        | fn ->
            // How a cross-file function presents itself; `ValueNone` is any other function.
            let external: ExternalFunction voption =
                match fn with
                | TExpr.External(key, _, _) ->
                    ValueSome
                        {
                            Key = SymbolKey.Binding key
                            Args = ValueSome args
                            RebuiltFn = fun () -> markedFn
                        }
                | TExpr.ExternalMember(objArg, key, _, storage, widths, _, memberTok) ->
                    ValueSome
                        {
                            Key = key
                            Args =
                                untupleMemberArgs storage widths args
                                |> ValueOption.map (fun opened ->
                                    match objArg with
                                    | ValueSome r -> levelOf r memberTok :: opened
                                    | ValueNone -> opened
                                )
                            RebuiltFn = fun () -> walk markedFn
                        }
                | _ -> ValueNone

            match external with
            | ValueSome ext ->
                match lookupExternal x.Ctx x.Specs ext.Key, ext.Args with
                // An external WITH an inline body ALWAYS expands: no operand-groundness gate. An
                // un-ground `^T` selects no per-primitive `StaticOptimization` clause and falls to
                // the body's BASE, where the safe generic default lives (`=` → `Equals`).
                | ValueSome served, ValueSome opened ->
                    AppliedFunction.Template(TemplateId.Foreign served.Key, served, opened)
                // A body to splice and no parameters to splice it against: Elaborate opens every
                // member argument to the width its key declares, so this node is malformed.
                | ValueSome served, ValueNone ->
                    failwithf
                        "InlineExpansion: the spliced member %A was applied to an argument its declared parameters cannot be bound to"
                        served.Key
                | ValueNone, _ -> AppliedFunction.Opaque ext.RebuiltFn
            | ValueNone -> AppliedFunction.Opaque(fun () -> walk markedFn)

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
                        AppliedArity = 0
                    }
                Shareable = SemTypeQuery.isGround x.Ctx.Store call.Ty
                Path = template.Path
                Edge =
                    {
                        Tok = call.Tok
                        Path = pathOf x at
                        Ty = call.Ty
                    }
                ReuseArgs = fun () -> []
                Build = fun _ -> { Body = body; Survivors = [] }
            }
            x.Specs

    /// Finish a classified application: walk the body, which is where every nested inline call
    /// inside it resolves, and fuse in the call-site material the classification marked.
    let rec private reduceClassified (x: Expander) (inFlight: InFlight) (peeled: Peeled) : Reduced =
        // Every fusion below is the CALLER's material, so it carries the caller's domain.
        let caller = pathOf x inFlight.Caller

        let fusedLambdas =
            peeled.Params |> List.filter (fun p -> p.Disposition = Disposition.FuseLambda)

        // Marked UNDER its own bound variables: beta-reduction consumes those bound variables against arguments
        // from the BODY the lambda is spliced into, so only what it computes came from the call.
        let own =
            inFlight.Own
            |> Descent.withLambdas
                [
                    for p in fusedLambdas ->
                        p.Key,
                        {
                            Body = Inline.underLambdas (TastWalk.callerExpr caller) p.Arg
                            Caller = inFlight.Caller
                        }
                ]

        let core = walkAt x own peeled.Core

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

    /// Expand ONE call site against ONE template, of this file or served by another; the two
    /// differ only in the lookup that found them. Resolution and classification run BEFORE the
    /// frame is pushed.
    and private expandAt (x: Expander) (at: Descent) (template: TemplateBody) (call: PendingCall) : TExpr =
        expandingTemplate
            x
            at
            call
            (fun () ->
                let resolved = resolveAt x.Ctx x.Mint call.Tok template call.Args
                let caller = pathOf x at
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
                                AppliedArity = List.length peeled.Params
                            }
                        Shareable =
                            Peeled.isClosed peeled
                            && resolved.TypeArgs |> Array.forall (SemTypeQuery.isGround x.Ctx.Store)
                        Path = template.Path
                        Edge =
                            {
                                Tok = call.Tok
                                Path = caller
                                Ty = call.Ty
                            }
                        ReuseArgs = fun () -> peeled.Params |> List.map (fun p -> walkAt x at p.Arg)
                        Build = fun spec -> reduceClassified x (Descent.enter at call template.Path spec) peeled
                    }
                    x.Specs
            )

    /// A node that APPLIES nothing of its own, spliced against the body its key is served
    /// under: the arguments the lifted body takes are all that vary. `ValueNone` = no served
    /// body, so it is a real call whose arguments the walk handles by default.
    and private tryExpandServed
        (x: Expander)
        (at: Descent)
        (walk: TExpr -> TExpr)
        (key: SymbolKey)
        (ty: SemType)
        (tok: SyntaxToken)
        (args: TastWalk.AppArg list)
        : TExpr voption =
        match lookupExternal x.Ctx x.Specs key with
        | ValueSome served ->
            ValueSome(
                expandAt
                    x
                    at
                    served
                    {
                        Template = TemplateId.Foreign served.Key
                        Tok = tok
                        Ty = ty
                        Args = args
                        Walk = walk
                    }
            )
        | ValueNone -> ValueNone

    /// The expansion walker, as a FUNCTION of the descent the material it is handed was WRITTEN
    /// under, because a `Mapper` has no room for a parameter of its own. Descending into a callee's
    /// body builds a mapper with that callee's frame pushed; call-site material keeps its own.
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
                        let markedFn, appArgs = TastWalk.collectAppChain [] e

                        match appliedFunction x at walk markedFn appArgs with
                        | AppliedFunction.Template(id, body, templateArgs) ->
                            let call =
                                {
                                    Template = id
                                    // A rewrite inherits the position of the node it REPLACES: the
                                    // whole APPLICATION, not the function it applies. The two
                                    // differ when an outer fusion substituted a CALL SITE
                                    // function into this body.
                                    Tok = TastWalk.exprTok e
                                    Ty = TastWalk.exprTy e
                                    Args = templateArgs
                                    Walk = walk
                                }

                            ValueSome(expandAt x at body call)
                        // Beta-reduced against the call args and walked, so nested inline calls
                        // resolve in the recursion. The copy keeps its OWN positions: it is the
                        // CALL SITE's argument, whose tokens anchor its `FunVerdicts` entry.
                        | AppliedFunction.Fused fused ->
                            ValueSome(
                                walkAt x fused.Caller (Inline.betaReduce (Inline.freshen x.Mint fused.Body) appArgs)
                            )
                        // A rebuild walks the arguments as the CALLER's own material.
                        | AppliedFunction.Opaque rebuiltFn ->
                            ValueSome(
                                TastWalk.rebuildApp (rebuiltFn ()) [ for a in appArgs -> { a with Arg = walk a.Arg } ]
                            )
                    // A dispatched SRTP trait call, whose body the provider serves. The INTRINSIC
                    // operator surface arrives here: `1 &&& 2` dispatches to `Vesper.int`'s
                    // `(&&&)`, and a primitive has no type to hang a method on.
                    | TExpr.StaticMethodCall(key, _, args, ty, tok) ->
                        let callArgs = [ for a in EqArray.toList args -> levelOf a (TastWalk.exprTok a) ]

                        tryExpandServed x at walk key ty tok callArgs
                    // A PROPERTY read applies nothing, so the `App` arm never classifies it: its
                    // object argument IS the one argument the lifted `this`-first body takes
                    // (`arr.Length` → the `ldlen` body).
                    | TExpr.ExternalMember(objArg, key, _, MemberStorage.Property, _, ty, tok) ->
                        let callArgs =
                            match objArg with
                            | ValueSome r -> [ levelOf r tok ]
                            | ValueNone -> []

                        tryExpandServed x at walk key ty tok callArgs
                    // A BARE reference to a LOCAL inline: the template used as a value. At arity
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
                    | TExpr.External(key, refTy, tok) ->
                        let body = lookupExternal x.Ctx x.Specs (SymbolKey.Binding key)

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
                        | ValueNone -> etaReify x.Ctx x.Mint body key refTy tok |> ValueOption.map walk
                    | _ -> ValueNone
        }

    /// Walk `e` as material written at `at`.
    and private walkAt (x: Expander) (at: Descent) (e: TExpr) : TExpr = TastWalk.mapExpr (mapperAt x at) e

    /// Expand the module-level inlines in one decl-list (elaborated, `TyVar`-carrying decls paired
    /// with their freeze envs). A cross-file body is carried on the provider entry the use-site
    /// key resolves to; a provider serving none makes this an identity rebuild.
    let run (ctx: PassContext) (decls: (TDecl * DeclEnv) list) : Expanded =
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
                }

            // The roots the finished table counts edges from.
            let declExprs = ResizeArray<TExpr>()

            // The file's OWN declarations are inside no expansion, so they are walked at the top
            // descent, and each walked expression is retained as it is produced.
            let walkTop (e: TExpr) : TExpr =
                let walked = walkAt x Descent.top e
                declExprs.Add walked
                walked

            let expanded =
                decls |> List.map (fun (d, env) -> TastWalk.mapDeclExprs walkTop d, env)

            {
                Decls = expanded
                Specializations = SpecTable.finish declExprs x.Specs
            }
