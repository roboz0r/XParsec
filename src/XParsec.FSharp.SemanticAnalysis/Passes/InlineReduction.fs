namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open InlineSpecTable

// One inline reduction: a cross-file body arrives FROZEN and is thawed here into this file's own
// inference cells; EVERY body is then outlined into a table entry keeping the positions it was
// written at, the call site getting a `TExpr.InlineCall` edge the backends expand at emit time.
module InlineReduction =

    /// The peel of a resolved inline body against one call site's arguments: everything about the
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

    /// An inline body as this pass consumes it, in this file's `SemType` domain. A template of
    /// this file and one another file served are the same record; only `Origin` tells them apart.
    type internal TemplateBody =
        {
            Key: SymbolKey
            Decl: TDecl
            ParamAttrs: EqArray<ParamAttrs>
            /// The file every anchor in `Decl` indexes: this file's own for a local template,
            /// the producer's for a served one.
            Origin: OriginFile
        }

    /// WHICH inline binding a reduction is expanding: the identity a RECURSION is detected on,
    /// and NOT the `SymbolKey` its table entry is keyed by. A local binding's key and a
    /// package's can collide, and the collision must not answer "is this call recursive?".
    [<RequireQualifiedAccess>]
    type internal TemplateId =
        | Local of boundVar: NodeKey
        | Foreign of key: SymbolKey

    /// One inline reduction IN FLIGHT. The CHAIN of these is what makes a recursive inline
    /// TERMINATE: a call reaching a binding already on the chain is answered with an edge into
    /// the entry that expansion reserved, leaving a finite graph for the acyclicity check.
    [<NoEquality; NoComparison>]
    type internal ExpansionFrame =
        {
            Template: TemplateId
            /// The anchor domain of the body this frame expands. It rides the chain so a walk
            /// cannot be handed material under a domain the chain disagrees with.
            Origin: OriginFile
            /// The table slot this expansion reserved: what a re-entering call is answered with.
            Spec: SpecializationId
        }

    /// The inline bindings whose bodies this walk is currently INSIDE, innermost first, plus the
    /// token of the call in the user's file it went inside them for. Expanding `sq 3` in `app.fs`
    /// walks `x * x` written in `math.fs`, but reports its diagnostics at `sq 3`.
    [<NoEquality; NoComparison>]
    type internal Descent =
        private
            {
                Frames: ExpansionFrame list
                /// `ValueNone` only while walking the compiling file's own declarations: the call
                /// about to be answered is written there, so its own token is the position.
                Site: SyntaxToken voption
            }

    /// A FRESH reduction and the two descents its material belongs to: the body walks INSIDE this
    /// reduction while a supplied argument walks outside it. `1 - 2 - 3` applies `(-)` inside
    /// `(-)`'s own left operand, and that nested application is not a recursion.
    [<NoEquality; NoComparison>]
    type internal InFlight =
        {
            /// The descent this reduction's own BODY is walked under.
            Own: Descent
            /// The descent the CALL SITE's own material is walked under.
            Caller: Descent
        }

    /// The call an expansion is being entered FOR: which binding it calls, where it stands, and
    /// the arguments an ANSWER needs it in. For an `ExternalMember` that includes the object
    /// argument, at curried position 0, which the application it was reached through never held.
    [<NoEquality; NoComparison>]
    type internal PendingCall =
        {
            Template: TemplateId
            /// The position the expansion stands in for: the APPLICATION node's own token, never
            /// the applied function's, because the two differ when an outer fusion substituted it in.
            Tok: SyntaxToken
            /// The node's own result type: the type of every edge minted for this call.
            Ty: SemType
            /// The arguments AS APPLIED: what the entry's parameters were peeled against, so an
            /// edge's arguments align to them positionally. Unwalked: only the edge answer walks
            /// them, and only the ones it carries.
            Args: (TExpr * SemType * SyntaxToken) list
            /// Walks one of `Args`, the CALLER's own material, expanded however the call is answered.
            Walk: TExpr -> TExpr
        }

    [<RequireQualifiedAccess>]
    module internal Descent =

        /// The compiling file's own declarations, inside no inline body: where the walk starts.
        let top: Descent = { Frames = []; Site = ValueNone }

        /// The file the expressions being walked here were WRITTEN in, whose token array their
        /// anchors index: the producer's inside a served body, `compiling` outside one.
        let originOf (compiling: OriginFile) (d: Descent) : OriginFile =
            match d.Frames with
            | [] -> compiling
            | f :: _ -> f.Origin

        /// The frame already expanding `template`, if the walk is inside one: this call has
        /// reached a binding that reaches itself, and is answered from that frame instead of
        /// expanding the same body a second time.
        let reentered (template: TemplateId) (d: Descent) : ExpansionFrame voption =
            match d.Frames |> List.tryFind (fun f -> f.Template = template) with
            | Some f -> ValueSome f
            | None -> ValueNone

        /// The token a diagnostic about `call` is reported at: the user-written call this descent
        /// went inside a body for, or `call`'s own where nothing has been entered.
        let siteOf (d: Descent) (call: PendingCall) : SyntaxToken =
            match d.Site with
            | ValueSome site -> site
            | ValueNone -> call.Tok

        /// Go INSIDE the body this call targets: the descent its own expressions are walked at
        /// (this binding pushed onto the caller's), beside the one the call site's arguments
        /// stay at, because they are the caller's expressions and never enter anything.
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
                        Site = ValueSome(siteOf d call)
                    }
                Caller = d
            }

    /// An applied FUNCTION that resolves to a cross-file symbol: a plain `External`, or the dotted
    /// `ExternalMember` that `x.get_Item(2)` lowers to, whose object argument is a FIELD of the
    /// function rather than an applied argument and so must be prepended at curried position 0.
    [<NoEquality; NoComparison>]
    type internal ExternalFunction =
        {
            /// `ValueNone` is a genuine "carries no inline body", never a missed lookup: an
            /// expandable function is key-stamped upstream, and a member is always keyed.
            Key: SymbolKey voption
            /// A member's arguments are the call's ONE tupled argument OPENED to the declared
            /// parameters; `ValueNone` is one that does not open.
            Args: (TExpr * SemType * SyntaxToken) list voption
            /// A thunk: the walk a member needs is wasted on any answer but the rebuild.
            RebuiltFn: unit -> TExpr
        }

    /// A lambda argument eligible for inline-first elimination, with the descent it was WRITTEN
    /// under: it is spliced at a use INSIDE the body it was passed to, where it is still the
    /// caller's material, so a call it makes to that binding is nested and not a recursion.
    [<NoEquality; NoComparison>]
    type internal FusedLambda = { Body: TExpr; Caller: Descent }

    /// What the function at a call resolves to: the whole dispatch of the walker's
    /// application rule.
    [<RequireQualifiedAccess; NoEquality; NoComparison>]
    type internal AppliedFunction =
        /// A function with an inline body, and the arguments its parameters are peeled against.
        /// For a member the object argument leads, so this is not the list the application was
        /// written with.
        | Template of id: TemplateId * body: TemplateBody * args: (TExpr * SemType * SyntaxToken) list
        /// A saturated use of an inline-first lambda parameter: the bound lambda is spliced at
        /// this use, so its closure never exists.
        | Fused of FusedLambda
        /// Nothing to expand; the function survives its application. A THUNK: a member's surviving
        /// function must be walked (nothing else walks its object argument), while walking a plain
        /// `External` etas it into the closure this call is the saturation of.
        | Opaque of rebuiltFn: (unit -> TExpr)

    [<RequireQualifiedAccess>]
    module internal Peeled =

        /// No call-site material was fused in, so the body is CLOSED over its parameters, which is
        /// the condition under which two sites may share one entry. Fused material is one site's
        /// OPERAND: a shared entry holding it would evaluate site 1's argument at site 2's call.
        let isClosed (p: Peeled) : bool =
            p.Params |> List.forall (fun x -> x.Disposition = Disposition.Survive)

    /// Expand ONE inline binding for one use site. Substituting the derived type arguments selects
    /// the body's `StaticOptimization` clause, dispatches its `TraitCall`s, and GROUNDS a typar
    /// reachable only through the body, which beta-reduction binds no value parameter to.
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
                // BoundVars are freshened so two expansions of one template cannot share a
                // codegen local slot; the body is NOT moved off the positions it was written at.
                Body = Inline.freshen mint expanded
                // The grounding the specialization table keys on.
                TypeArgs = typeArgs
            |}
        | _ -> failwith "InlineExpansion: an inline body must be a TDecl.Let"

    /// Eta-reify an `External` used as a VALUE (`(+)` in `List.fold (+) 0 xs`) into the `App`
    /// `fun p0 p1 -> (+) p0 p1`. Arity is the reference's parameter count capped by the body's
    /// lambda arity (a partial eta is type-correct); at arity 0 a value etas to `ValueNone`.
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
            // Fresh bound variables, so an eta site cannot alias the bound variables of the body resolved at it.
            let boundVars =
                SemTypeQuery.Funs.domains ctx.Store arity refTy
                |> List.mapi (fun i pty -> mint (), pty, i)

            let appBody =
                boundVars
                |> List.fold
                    (fun acc (k, pty, i) ->
                        let resTy = SemTypeQuery.Funs.resultAfter ctx.Store (i + 1) refTy
                        TExpr.App(acc, TExpr.Var(k, pty, tok), resTy, tok)
                    )
                    (TExpr.External(name, keyOpt, refTy, tok))

            boundVars
            |> List.foldBack (fun (k, pty, _) (innerBody, innerTy) ->
                let lamTy = TyFun(pty, innerTy)
                TExpr.Lambda(TPat.NamedSimple(k, pty, tok), innerBody, lamTy, tok), lamTy
            )
            <| (appBody, SemTypeQuery.Funs.resultAfter ctx.Store arity refTy)
            |> fst
            |> ValueSome

    /// Peel a resolved inline body against one call site's arguments and DECIDE each parameter's
    /// fate: the half of the reduction needing no recursion, so it runs before a specialization
    /// slot is reserved. `caller` marks a substituted argument, which has left the file it was in.
    let internal classifyApplication
        (caller: OriginFile)
        (paramAttrs: EqArray<ParamAttrs>)
        (expanded: TExpr)
        (args: (TExpr * SemType * SyntaxToken) list)
        : Peeled =
        // Carry the template's own bound variable token so an entry's parameter keeps the position it was
        // written at. The application node's is not carried: that position belongs to the EDGE.
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
                        // Provisional: the classification below settles it, once the whole peel is in.
                        Disposition = Disposition.Survive
                     }
                     :: acc)
            | TExpr.Lambda(param, _, _, _), _ ->
                failwithf "InlineExpansion: inline parameter destructuring is out of scope: %A" param
            | _, _ :: _ -> failwith "InlineExpansion: over-application of an inline function"

        let bindings, core = peel expanded args []

        // A parameter bound to a bare `External` function value is substituted into the body BEFORE
        // the walk (a value reference has no side effect and no capture), so a saturated `func arg`
        // use re-forms the application; else `let func = ignore in func arg` survives, uneta-expandable.
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

        // TODO(byref-capture): a SURVIVING lambda arg capturing a byref-like value (`Span`, any
        // `ref struct`) compiles to a heap closure that cannot legally hold it. Needs a byref-like
        // predicate that does not exist (`SemType` has no ref-struct case; metadata drops byrefs).
        let classified =
            bindings
            |> List.mapi (fun i p ->
                let disposition =
                    if externalKeys.Contains p.Key then
                        Disposition.FuseExternalValue
                    elif candidates.ContainsKey p.Key && not (bad.Contains p.Key) then
                        Disposition.FuseLambda
                    // Substituted at its single (declaration-validated linear) use instead of bound
                    // eagerly, so the argument is evaluated at most once and on demand, which is
                    // what makes `&&`/`||` short-circuit. Everything else stays eager.
                    elif i < paramAttrs.Length && paramAttrs.[i].CallAtMostOnce then
                        Disposition.FuseAtMostOnce
                    else
                        Disposition.Survive

                { p with Disposition = disposition }
            )

        { Params = classified; Core = core }

    /// Reach a cross-file body by EXACT `SymbolKey`, never by a name lookup whose best-by-arity
    /// collapse could serve a sibling overload's body. The THAW happens per lookup, so two call
    /// sites of one template never share an inference cell; the body keeps the producer's positions.
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
