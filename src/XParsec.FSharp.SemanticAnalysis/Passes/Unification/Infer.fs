namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationEngine
open UnificationTranslate
open UnificationInferGeneralize
open UnificationInferLiterals
open UnificationInferResolve
open UnificationInferPat
open UnificationInferOverload
open UnificationInferForwardSchemes
open UnificationInferDispatch
open UnificationInferLiteralExpr
open UnificationInferRecordAccess
open UnificationInferExternalCall
open UnificationInferCtor
open UnificationInferTypeOps
open UnificationInferAssign
open UnificationInferControlFlow
open UnificationInferIdentExpr
open UnificationInferApp

module UnificationInfer =

    /// Peel paren / ascription wrappers to a binding RHS's underlying format-string
    /// literal (`let fmt = ("%d" : Fmt)` → the inner `"%d"`). `ValueNone` unless the
    /// peeled expr is an `Expr.String` whose specifiers parse.
    let rec private peelToFormatString (ctx: PassContext) (e: Expr<SyntaxToken>) : Expr<SyntaxToken> voption =
        match e with
        | Expr.EnclosedBlock(expr = inner)
        | Expr.TypeAnnotation(expr = inner) -> peelToFormatString ctx inner
        | Expr.String _ ->
            if (formatSpecifiers ctx e).IsSome then
                ValueSome e
            else
                ValueNone
        | _ -> ValueNone

    let rec infer (ctx: PassContext) (e: Expr<SyntaxToken>) : SemType =
        let node = CstKeys.siteOfExpr e
        let nodeTv = freshTv ctx node.Key

        let inferredTy =
            match e with
            | Expr.Const c -> inferConst ctx c
            | Expr.Ident _ -> inferIdent ctx e node
            | Expr.LongIdentOrOp _ -> inferIdent ctx e node
            | Expr.App(fn, args) -> inferApp infer ctx node fn args
            | Expr.HighPrecedenceApp(funcExpr = fn; argExpr = arg) -> inferHighPrecApp infer ctx node fn arg
            | Expr.InfixApp(left, _, right) -> inferInfix infer ctx node left right
            | Expr.PrefixApp(_, operand) -> inferPrefix infer ctx node operand
            | Expr.Fun(argumentPats = argPats; expr = body) -> inferFun infer ctx argPats body
            | Expr.LetOrUse(keyword = kw; bindings = bindings; body = body) -> inferLet ctx kw bindings body
            | Expr.EnclosedBlock(lParen = ParenKind.List _; expr = inner; rParen = rTok) ->
                checkLiteralClose ctx node.Tok rTok Token.KWRBracket "]"
                inferListLikeLiteral infer ctx node.Tok inner false
            | Expr.EnclosedBlock(lParen = ParenKind.Array _; expr = inner; rParen = rTok) ->
                checkLiteralClose ctx node.Tok rTok Token.KWRArrayBracket "|]"
                inferListLikeLiteral infer ctx node.Tok inner true
            | Expr.EnclosedBlock(expr = inner) -> infer ctx inner
            | Expr.IfThenElse(condition = cond; thenExpr = thenE; elifBranches = elifs; elseBranch = elseB) ->
                inferIfThenElse infer ctx node.Tok cond thenE elifs elseB
            | Expr.Tuple(exprs = items) -> inferTuple infer ctx items
            | Expr.Sequential(exprs = items) -> inferSequential infer ctx node.Tok items
            | Expr.TypeAnnotation(expr = inner; typ = t) -> inferTypeAnnotation infer ctx node inner t
            | Expr.StaticUpcast(expr = inner; typ = t) -> inferStaticUpcast infer ctx node inner t
            | Expr.DynamicTypeTest(expr = inner; typ = t) -> inferDynamicTypeTest infer ctx node inner t
            | Expr.DynamicDowncast(expr = inner; typ = t) -> inferDynamicDowncast infer ctx node inner t
            | Expr.EmptyBlock(lParen = ParenKind.List _; rParen = rTok) ->
                checkLiteralClose ctx node.Tok rTok Token.KWRBracket "]"
                emptyListLikeLiteral ctx node.Tok false
            | Expr.EmptyBlock(lParen = ParenKind.Array _; rParen = rTok) ->
                checkLiteralClose ctx node.Tok rTok Token.KWRArrayBracket "|]"
                emptyListLikeLiteral ctx node.Tok true
            | Expr.EmptyBlock _ -> ctx.Intrinsics.Unit
            | Expr.While(condition = cond; body = body) -> inferWhile infer ctx node.Tok cond body
            | Expr.ForTo(ident = ident; startExpr = startE; endExpr = endE; body = body) ->
                inferForTo infer ctx node.Tok ident startE endE body
            | Expr.ForIn(pat = pat; enumerableExpr = src; body = body) -> inferForIn infer ctx node pat src body
            | Expr.String(parts = parts) -> inferString infer ctx parts
            | Expr.Match(matchExpr = scrutinee; rules = Rules(rules = rules)) ->
                inferMatch infer ctx node.Tok scrutinee rules
            | Expr.Function(rules = Rules(rules = rules)) -> inferFunction infer ctx node.Tok rules
            | Expr.TryWith(expr = body; rules = Rules(rules = rules)) -> inferTryWith infer ctx node.Tok body rules
            | Expr.TryFinally(tryExpr = body; finallyExpr = finallyE) ->
                inferTryFinally infer ctx node.Tok body finallyE
            // `x?name <- value` — the dynamic setter, routed through `op_DynamicAssignment`.
            | Expr.Assignment(leftExpr = Expr.DynamicLookup(expr = objArg); rightExpr = right) ->
                inferDynamicSet infer ctx node objArg right
            | Expr.Assignment(leftExpr = left; rightExpr = right) -> inferAssignment infer ctx node left right
            | Expr.Range(fromExpr = a; toExpr = b) -> inferRange infer ctx node.Tok a ValueNone b
            | Expr.SteppedRange(fromExpr = a; stepExpr = s; toExpr = b) ->
                inferRange infer ctx node.Tok a (ValueSome s) b
            | Expr.Null _ ->
                // No reference-type bound yet, so a free var lets the context pin it. An
                // `obj`-typed context ABSORBS rather than pins, so register the var to settle
                // after the walk.
                let tv = freshTyVar ctx
                ctx.RegisterNullLiteral(tv, node.Tok)
                TyVar tv
            | Expr.Record(fieldInitializers = inits) -> inferRecord infer ctx node inits
            | Expr.RecordClone(expr = src; fieldInitializers = inits) -> inferRecordClone infer ctx node src inits
            | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                // A type-name qualifier (`EqualityComparer<int>.Default`) resolves its
                // static member through the provider, so the qualifier is never `infer`d
                // as a value. Instance access (`value.Member`) takes the fallback.
                match tryExternalTypeQualifier ctx r with
                | ValueSome(declTypeKey, typeArgsCst) ->
                    let args = [ for t in typeArgsCst -> translateType ctx t ]
                    inferExternalStaticMember ctx node.Key declTypeKey args li.Idents.[0]
                | ValueNone ->
                    match tryLocalTypeAppStaticMember ctx node r li.Idents.[0] with
                    | ValueSome ty -> ty
                    | ValueNone -> inferFieldAccess infer ctx node r li.Idents.[0]
            | Expr.IndexedLookup(expr = objArg; indexExpr = idx) -> inferIndexedLookup infer ctx node objArg idx
            | Expr.New(typ = t; expr = argExpr) -> inferNew infer ctx node t argExpr
            | Expr.ILIntrinsic(args = args; returnType = rt) -> inferILIntrinsic infer ctx args rt
            | Expr.LibraryOnlyStaticOptimization(defaultExpr = defaultE; clauses = clauses) ->
                inferLibraryOnlyStaticOptimization infer ctx node.Key defaultE clauses
            | Expr.StaticMemberInvocation(membersign = msig; expr = argExpr) ->
                inferStaticMemberInvocation infer ctx msig argExpr
            | Expr.TypeApp(expr = inner; types = typeArgs) -> inferTypeApp infer ctx node.Tok inner typeArgs
            // `x?name` — dynamic member access, routed through the `op_Dynamic`
            // operator so its `default ^TResult : dynamic` drives target typing.
            | Expr.DynamicLookup(expr = objArg) -> inferDynamicLookup infer ctx node objArg
            | _ -> failwithf "infer: TODO %A" e

        ctx.Store.SetLink(UnionFind.find ctx.Store nodeTv, ValueSome inferredTy)
        inferredTy

    /// The disposal capability's `Dispose` member key, taken from the resolved
    /// `ctx.CapabilityIds.Disposable` rather than a hardcoded `System.IDisposable`.
    /// `ValueNone` only for a compile with no disposable capability at all.
    and private capabilityDisposeSlot (ctx: PassContext) : SymbolKey voption =
        match ctx.CapabilityIds.Disposable with
        | ValueSome disp -> ValueSome(SymbolKeyOps.memberKey disp.Key "Dispose" EqArray.empty 0 MemberKind.Method)
        | ValueNone -> ValueNone

    /// The disposal path of a `use` bound variable of *external* (BCL) type. PRIMARY: the
    /// instantiated interface set carries `ctx.CapabilityIds.Disposable`, which also catches
    /// a `Dispose` declared on a base. `ValueNone` ⇒ not disposable.
    and private tryExternalDispose (ctx: PassContext) (declKey: TypeKey) (args: EqArray<SemType>) : Disposal voption =
        // The directly-implemented interface set an external nominal carries: a class's
        // `FrozenInterfaces` or a union's `interface <ty>` impls. An external RECORD carries
        // none, so a disposable external record resolves only via its own `Dispose` below.
        let externalInterfaces () : SemType[] =
            match ctx.Provider.TryLookupType declKey with
            | ValueSome(ExternalTypeShape.Class shape) ->
                ExternalSymbols.instantiateInterfaces shape (args.AsSpan().ToArray())
            | ValueSome(ExternalTypeShape.Union(_, _, ifaces, _, _)) ->
                ExternalSymbols.instantiateInterfacesOf ifaces (args.AsSpan().ToArray())
            // A capability interface that inherits another (`enumerator : disposable`) makes
            // `use e` on an abstract `enumerator<'T>` disposable, matching the BCL's
            // `IEnumerator<'T> : IDisposable`.
            | ValueSome(ExternalTypeShape.IntrinsicInterface iface) ->
                ExternalSymbols.instantiateInterfacesOf iface.Interfaces (args.AsSpan().ToArray())
            | _ -> [||]

        let viaInterface =
            match capabilityDisposeSlot ctx with
            | ValueSome slot when RuntimeNames.carriesCapability ctx.CapabilityIds.Disposable (externalInterfaces ()) ->
                ValueSome(Disposal.ViaCapability slot)
            | _ -> ValueNone

        match viaInterface with
        | ValueSome _ -> viaInterface
        // Fallback for an external non-`IDisposable` ref struct: its own pattern
        // `Dispose()`, which can't be reached through a boxed interface slot.
        | ValueNone ->
            match ctx.Provider.TryLookupMember(declKey, "Dispose") with
            | ValueSome m when not m.IsStatic && not m.IsValueMember ->
                ValueSome(Disposal.ViaOwnMember(SymbolKey.Member m.Key))
            | _ -> ValueNone

    /// True iff a project-local nominal type's `InterfaceImpls` carry a resolved interface
    /// whose type-constructor key matches `ctx.CapabilityIds.Disposable`.
    and private localImplementsDisposable
        (ctx: PassContext)
        (host: IInterfaceImplHost)
        (args: EqArray<SemType>)
        : bool =
        host.InterfaceImpls
        |> Array.exists (fun impl ->
            match InterfaceImplResolution.tryIface impl.Resolution with
            | ValueSome resolved ->
                match zonk ctx.Store (instantiateMember ctx.Store (host.TypeParams, args) resolved) with
                | TyClass(ifaceKey, _) -> RuntimeNames.matchesKey ctx.CapabilityIds.Disposable ifaceKey
                | _ -> false
            | ValueNone -> false
        )

    /// The ref-struct carve-out: a `[<IsByRefLike>]` class can't be boxed to `IDisposable`,
    /// so its duck-typed pattern `Dispose()` is called directly. Returns that member's key
    /// when the class is byref-like and exposes one; `ValueNone` otherwise.
    and private tryRefStructOwnDispose (ctx: PassContext) (clsKey: TypeKey) : SymbolKey voption =
        match TypeRegistry.tryClassByKey ctx.Types clsKey with
        | ValueSome info when info.IsByRefLike ->
            let hasDispose =
                info.Members
                |> Array.exists (fun m -> m.Name = "Dispose" && not m.IsStatic && m.Kind = ClassMemberKind.Method)

            if hasDispose then
                ValueSome(SymbolKeyOps.memberKey info.TypeKey "Dispose" EqArray.empty 0 MemberKind.Method)
            else
                ValueNone
        | _ -> ValueNone

    /// Resolve one `use` binding's disposal into `UseDispose`. Disposal is INTERFACE-REQUIRED
    /// (real-F# parity); the `[<IsByRefLike>]` ref struct and an external type with an
    /// own-`Dispose` and no `IDisposable` are the carve-out. Neither ⇒ a diagnostic, entry unset.
    and private resolveUseDispose (ctx: PassContext) (b: Binding<SyntaxToken>) : unit =
        match b.pattern with
        // `use _ = e` disposes exactly like a named bound variable; the body just has no name for it.
        | Pat.NamedSimple _
        | Pat.Wildcard _ ->
            let patKey = CstKeys.ofPat b.pattern
            let boundVarTy = zonk ctx.Store (TyVar(tvOf ctx patKey))

            let notDisposable (display: string) =
                ctx.Report(
                    CstKeys.firstTokenOfPat b.pattern,
                    Kind.Message(
                        sprintf
                            "The type '%s' cannot be used with 'use': a 'use' binding requires its type to implement 'disposable' ('System.IDisposable')"
                            display
                    )
                )

            let resolveLocal (host: IInterfaceImplHost) (tyCtorKey: TypeKey) (simple: string) (args: EqArray<SemType>) =
                match
                    (if localImplementsDisposable ctx host args then
                         capabilityDisposeSlot ctx |> ValueOption.map Disposal.ViaCapability
                     else
                         tryRefStructOwnDispose ctx tyCtorKey |> ValueOption.map Disposal.ViaOwnMember)
                with
                | ValueSome disposal -> ctx.Resolution.UseDispose.Set(patKey, disposal)
                | ValueNone -> notDisposable simple

            match resolveStep ctx.Store boundVarTy with
            | TyClass(tyCtorKey, args)
            | TyUnion(tyCtorKey, args)
            | TyRecord(tyCtorKey, args) ->
                // `simple` is for the diagnostic text only; the host resolves by the
                // arity-qualified key, which an arity-overloaded host needs.
                let (DisplayName simple) = SymbolKeyOps.typeSimpleName tyCtorKey

                match TypeRegistry.tryInterfaceImplHostByKey ctx.Types tyCtorKey with
                | ValueSome host -> resolveLocal host tyCtorKey simple args
                | ValueNone ->
                    match tryExternalDispose ctx tyCtorKey args with
                    | ValueSome disposal -> ctx.Resolution.UseDispose.Set(patKey, disposal)
                    | ValueNone -> notDisposable (SymbolKeyOps.typeMetaName tyCtorKey)
            | _ -> ()
        | _ -> ()

    and private inferLet
        (ctx: PassContext)
        (keyword: LetOrUseKeyword<SyntaxToken>)
        (bindings: ImmutableArray<Binding<SyntaxToken>>)
        (body: Expr<SyntaxToken> voption)
        : SemType =
        inferBindingGroup ctx bindings

        // `use` binds a disposable: resolve each bound variable's `Dispose` so disposal can be
        // keyed for codegen and a non-disposable diagnosed. `let` skips this.
        match keyword with
        | LetOrUseKeyword.Use _
        | LetOrUseKeyword.UseBang _ ->
            for b in bindings do
                resolveUseDispose ctx b
        | LetOrUseKeyword.Let _
        | LetOrUseKeyword.LetBang _ -> ()

        infer ctx (CstWalk.requireLetBody body)

    and inferBinding (ctx: PassContext) (b: Binding<SyntaxToken>) : unit =
        // One typar scope per binding signature: explicit `<'a>` typars seed
        // it first so later implicit `'a` mentions share the same TyVar.
        let enclosingScope = ctx.Resolution.TyparScope

        use _ =
            ctx.PushTyparScope(
                Dictionary<string, TyVarId>(System.StringComparer.Ordinal),
                ctx.Resolution.TyparScopeStrict
            )

        // Inherit the lexically-enclosing binding's typars (lowest priority) so a named typar
        // in a *nested* `let rec loop (t': Tree<'T>)` resolves to the enclosing function's
        // TyVar rather than minting a fresh, ungrounded `'T`, matching F#'s lexical typar scoping.
        for kv in enclosingScope do
            ctx.Resolution.TyparScope.[kv.Key] <- kv.Value

        // `EnclosingTypars` carries the class `<'T>` and, for a generic member's body walk,
        // the member's own typars. Seed them so a signature annotation (`(x: 'T)`, `: Set<'T>`)
        // resolves rather than reporting a free type parameter; the binding's own seed shadows.
        match ctx.Resolution.EnclosingTypars with
        | ValueSome enclosing ->
            for kv in enclosing do
                ctx.Resolution.TyparScope.[kv.Key] <- kv.Value
        | ValueNone -> ()

        seedBindingTypars ctx b

        // Elaborate needs the binding's explicit `<'b,'a>` typars in SOURCE order to put a free
        // function's declared typars first, and cannot recover that order from the scheme.
        match b.typarDefns with
        | ValueSome(TyparDefns(defns = ds)) ->
            let declared =
                [
                    for TyparDefn(typar = t) in ds do
                        match t with
                        | Typar.Named(ident = id)
                        | Typar.Static(ident = id) ->
                            let n = ctx.NameOf id

                            match ctx.Resolution.TyparScope.TryGetValue n with
                            | true, tv -> yield (n, tv)
                            | _ -> ()
                        | Typar.Anon _ -> ()
                ]

            if not (List.isEmpty declared) then
                ctx.Bindings.DeclaredTypars.Set(CstKeys.ofBinding b, declared)
        | ValueNone -> ()

        match b.typarDefns with
        | ValueSome(TyparDefns(constraints = ValueSome cs)) -> translateConstraints ctx cs
        | _ -> ()

        // The member-typar seed is for this binding's own typars only; clear it so a nested
        // `let` in the body mints fresh typars rather than reusing the member's prototypes.
        ctx.Resolution.BindingTyparSeed <- ValueNone

        let bindTok = (CstKeys.siteOfBinding b).Tok
        let patTy = inferPat ctx b.pattern

        // The return annotation is translated *before* the body so a return-only typar
        // (`let f () : 'T list = …`) seeds the scope first; otherwise the body mints a
        // fresh `'T` and the return translates into a different one.
        let rhsTy =
            if b.argumentPats.IsEmpty then
                match b.returnType with
                | ValueSome(ReturnType(typ = t)) ->
                    let annTy = translateType ctx t

                    // Provenance: `let x : T = e` writes the bound variable's type explicitly.
                    ctx.MarkTypeDeclared(CstKeys.ofPat b.pattern, annTy)

                    // A format-string literal bound to a `PrintfFormat`-family annotation
                    // types AS the format, not `string`: skip `infer` on the literal, and
                    // stamp the annotation's format type onto the literal node.
                    match tryTypeFormatLiteral ctx bindTok b.expr annTy with
                    | ValueSome fmt ->
                        ctx.Store.SetLink(UnionFind.find ctx.Store (freshTv ctx (CstKeys.ofExpr b.expr)), ValueSome fmt)

                        annTy
                    | ValueNone ->
                        let bodyTy = infer ctx b.expr
                        unifyAnnotation ctx bindTok bodyTy annTy
                        annTy
                | ValueNone -> infer ctx b.expr
            else
                let argTypes = [ for p in b.argumentPats -> inferPat ctx p ]

                let bodyTy =
                    match b.returnType with
                    | ValueSome(ReturnType(typ = t)) ->
                        let annTy = translateType ctx t
                        let bodyTy = infer ctx b.expr

                        // Provenance: a `let f … : T = body` return annotation writes the
                        // BODY's type; each parameter's is recorded by `inferPat`.
                        ctx.MarkTypeDeclared(CstKeys.ofExpr b.expr, annTy)

                        unifyAnnotation ctx bindTok bodyTy annTy
                        annTy
                    | ValueNone -> infer ctx b.expr

                List.foldBack (fun a r -> TyFun(a, r)) argTypes bodyTy

        unify ctx bindTok patTy rhsTy

        // A binding whose value is a format-string literal AND whose type resolved to a
        // `PrintfFormat` gets that literal stashed by binding site, so a later
        // `sprintf fmt …` recovers it and lowers natively.
        match resolveStep ctx.Store patTy with
        | TyClass(fmtKey, _) when fmtKey = RuntimeNames.printfFormatKey ->
            match peelToFormatString ctx b.expr with
            | ValueSome lit -> ctx.PrintfFormatLiterals.Set(CstKeys.ofPat b.pattern, lit)
            | ValueNone -> ()
        | _ -> ()

    /// Type a `let` / `let rec` group. Sibling binding-pattern TyVars are pre-allocated so a forward
    /// reference from inside one RHS finds the sibling's TyVar at THIS group's level rather
    /// than lazy-minting at a deeper one. Generalisation runs against the outer level.
    and inferBindingGroup (ctx: PassContext) (bindings: ImmutableArray<Binding<SyntaxToken>>) : unit =
        let outerLevel = ctx.CurrentLevel
        enterLevel ctx

        for b in bindings do
            match b.pattern with
            | Pat.NamedSimple _
            | Pat.Op _ ->
                let key = CstKeys.ofPat b.pattern
                tvOf ctx key |> ignore
                barPolymorphicRecursion ctx key
            | _ -> ()

        for b in bindings do
            inferBinding ctx b

        exitLevel ctx

        for b in bindings do
            if shouldGeneralise b then
                let key = CstKeys.ofPat b.pattern
                let patTv = tvOf ctx key
                let zonked = zonk ctx.Store (TyVar patTv)

                if not (hasPendingDotAccess ctx.Store zonked) then
                    // Settle flexible list-literal containers first, then re-zonk so the
                    // now-linked list element generalises.
                    prepareListLiterals ctx zonked outerLevel
                    let scheme = generalise ctx.Store (zonk ctx.Store zonked) outerLevel
                    ctx.Bindings.Scheme.Set(key, scheme)
