namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
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
open UnificationInferControlFlow
open UnificationInferIdentExpr
open UnificationInferApp

module UnificationInfer =

    let rec infer (ctx: PassContext) (e: Expr<SyntaxToken>) : SemType =
        let key = CstKeys.ofExpr e
        let nodeTv = freshTv ctx key

        let inferredTy =
            match e with
            | Expr.Const c -> inferConst ctx c
            | Expr.Ident _ -> inferIdent ctx e key
            | Expr.LongIdentOrOp _ -> inferIdent ctx e key
            | Expr.App(fn, args) -> inferApp infer ctx key fn args
            | Expr.HighPrecedenceApp(funcExpr = fn; argExpr = arg) -> inferHighPrecApp infer ctx key fn arg
            | Expr.InfixApp(left, _, right) -> inferInfix infer ctx key left right
            | Expr.PrefixApp(_, operand) -> inferPrefix infer ctx key operand
            | Expr.Fun(argumentPats = argPats; expr = body) -> inferFun infer ctx argPats body
            | Expr.LetOrUse(keyword = kw; bindings = bindings; body = body) -> inferLet ctx key kw bindings body
            | Expr.EnclosedBlock(lParen = ParenKind.List _; expr = inner; rParen = rTok) ->
                checkLiteralClose ctx key rTok Token.KWRBracket "]"
                inferListLikeLiteral infer ctx key inner false
            | Expr.EnclosedBlock(lParen = ParenKind.Array _; expr = inner; rParen = rTok) ->
                checkLiteralClose ctx key rTok Token.KWRArrayBracket "|]"
                inferListLikeLiteral infer ctx key inner true
            | Expr.EnclosedBlock(expr = inner) -> infer ctx inner
            | Expr.IfThenElse(condition = cond; thenExpr = thenE; elifBranches = elifs; elseBranch = elseB) ->
                inferIfThenElse infer ctx key cond thenE elifs elseB
            | Expr.Tuple(exprs = items) -> inferTuple infer ctx items
            | Expr.Sequential(exprs = items) -> inferSequential infer ctx key items
            | Expr.TypeAnnotation(expr = inner; typ = t) -> inferTypeAnnotation infer ctx key inner t
            | Expr.StaticUpcast(expr = inner; typ = t) -> inferStaticUpcast infer ctx key inner t
            | Expr.DynamicTypeTest(expr = inner; typ = t) -> inferDynamicTypeTest infer ctx key inner t
            | Expr.DynamicDowncast(expr = inner; typ = t) -> inferDynamicDowncast infer ctx key inner t
            | Expr.EmptyBlock(lParen = ParenKind.List _; rParen = rTok) ->
                checkLiteralClose ctx key rTok Token.KWRBracket "]"
                emptyListLikeLiteral ctx key false
            | Expr.EmptyBlock(lParen = ParenKind.Array _; rParen = rTok) ->
                checkLiteralClose ctx key rTok Token.KWRArrayBracket "|]"
                emptyListLikeLiteral ctx key true
            | Expr.EmptyBlock _ -> BuiltinTypes.tyUnit
            | Expr.While(condition = cond; body = body) -> inferWhile infer ctx key cond body
            | Expr.ForTo(ident = ident; startExpr = startE; endExpr = endE; body = body) ->
                inferForTo infer ctx key ident startE endE body
            | Expr.ForIn(pat = pat; enumerableExpr = src; body = body) -> inferForIn infer ctx key pat src body
            | Expr.String(parts = parts) -> inferString infer ctx key parts
            | Expr.Match(matchExpr = scrutinee; rules = Rules(rules = rules)) ->
                inferMatch infer ctx key scrutinee rules
            | Expr.Function(rules = Rules(rules = rules)) -> inferFunction infer ctx key rules
            | Expr.TryWith(expr = body; rules = Rules(rules = rules)) -> inferTryWith infer ctx key body rules
            | Expr.TryFinally(tryExpr = body; finallyExpr = finallyE) -> inferTryFinally infer ctx key body finallyE
            | Expr.Assignment(leftExpr = left; rightExpr = right) -> inferAssignment infer ctx key left right
            | Expr.Range(fromExpr = a; toExpr = b) -> inferRange infer ctx key a ValueNone b
            | Expr.SteppedRange(fromExpr = a; stepExpr = s; toExpr = b) -> inferRange infer ctx key a (ValueSome s) b
            | Expr.Null _ ->
                // No reference-type bound yet — free TypeVar so surrounding
                // context can pin it.
                TyVar(freshTyVar ctx)
            | Expr.Record(fieldInitializers = inits) -> inferRecord infer ctx key inits
            | Expr.RecordClone(expr = src; fieldInitializers = inits) -> inferRecordClone infer ctx key src inits
            | Expr.DotLookup(expr = recv; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                // A type-name receiver (`EqualityComparer<int>.Default`) resolves
                // its static member through the provider — probed once here, ahead
                // of the field-access fallback so the receiver isn't `infer`d as a
                // value. Instance access (`value.Member`) takes the fallback.
                match tryExternalTypeReceiver ctx recv with
                | ValueSome(metaName, typeArgsCst) ->
                    let args = [ for t in typeArgsCst -> translateType ctx t ]
                    inferExternalStaticMember ctx key metaName args li.Idents.[0]
                | ValueNone ->
                    // `ClassName<'args>.Member` on a *local* class/union — resolve its
                    // static member before falling to value-receiver field access.
                    match tryLocalTypeAppStaticMember ctx recv li.Idents.[0] with
                    | ValueSome ty -> ty
                    | ValueNone -> inferFieldAccess infer ctx key recv li.Idents.[0]
            | Expr.IndexedLookup(expr = recv; indexExpr = idx) -> inferIndexedLookup infer ctx key recv idx
            | Expr.New(typ = t; expr = argExpr) -> inferNew infer ctx key t argExpr
            | Expr.ILIntrinsic(args = args; returnType = rt) -> inferILIntrinsic infer ctx args rt
            | Expr.LibraryOnlyStaticOptimization(expr = baseE; constraints = cs; optimizedExpr = optE) ->
                inferLibraryOnlyStaticOptimization infer ctx key baseE cs optE
            | Expr.StaticMemberInvocation(expr = argExpr) -> inferStaticMemberInvocation infer ctx argExpr
            | Expr.TypeApp(expr = inner; types = typeArgs) -> inferTypeApp infer ctx key inner typeArgs
            | _ ->
                // Surface the unhandled case loudly rather than fabricating a
                // free TyVar and silently producing a broken type for every
                // use site. Matches the precedent in
                // `Freeze.translateExpr` (file: Freeze.fs).
                failwithf "infer: TODO %A" e

        nodeTv.Link <- ValueSome inferredTy
        inferredTy

    /// Resolve a keyed `Dispose` for a `use` binder of *external* (BCL) type.
    /// Prefer the type's *own* declared `Dispose`
    /// — a duck-typed pattern dispose, including a non-`IDisposable` ref struct —
    /// then fall back to `System.IDisposable::Dispose` when the type implements the
    /// interface (the common BCL case: `Dispose` is declared on a base, so
    /// `TryLookupMember` — `DeclaredOnly` — misses it, but `GetInterfaces` surfaces
    /// `IDisposable` transitively). `ValueNone` ⇒ the type exposes no `Dispose`.
    and private tryExternalDispose (ctx: PassContext) (name: string) (args: EqArray<SemType>) : SymbolKey voption =
        match ctx.Provider.TryLookupMember(name, "Dispose") with
        | ValueSome m when not m.IsStatic && not m.IsProperty -> ValueSome m.Key
        | _ ->
            match ctx.Provider.TryLookupType name with
            | ValueSome(ExternalTypeShape.Class shape) when
                ExternalSymbols.instantiateInterfaces shape (args.AsSpan().ToArray())
                |> Array.exists (fun (n, _) -> n = "System.IDisposable")
                ->
                ValueSome(
                    SymbolKey.MemberKey(
                        SymbolKey.TypeKey(None, "System", "IDisposable"),
                        "Dispose",
                        EqArray.empty,
                        MemberKind.Method
                    )
                )
            | _ -> ValueNone

    /// Resolve the disposal target for one `use` binding. A *project-local*
    /// binder keeps the duck-typed direct `Dispose()` call (codegen resolves it via
    /// the local member table), recorded as nothing so Freeze leaves
    /// `TExpr.Use.dispose = ValueNone`. An *external* binder's keyed `Dispose` is
    /// stashed in `UseDispose` for Freeze. A binder with no `Dispose` is a
    /// `use`-over-non-disposable error (C# parity); an unresolved binder type is
    /// left alone (pre-existing behaviour).
    and private resolveUseDispose (ctx: PassContext) (b: Binding<SyntaxToken>) : unit =
        match b.headPat with
        | Pat.NamedSimple _ ->
            let patKey = CstKeys.ofPat b.headPat
            let binderTy = zonk (TyVar(tvOf ctx patKey))

            match resolveStep binderTy with
            | TyClass(clsKey, args) ->
                let simple = SymbolKeyOps.simpleName clsKey

                match TypeRegistry.tryClass ctx.Types simple with
                | ValueSome _ ->
                    match tryClassChainMember ctx simple args "Dispose" with
                    | ValueSome _ -> ()
                    | ValueNone ->
                        ctx.Error(
                            patKey,
                            sprintf "The type '%s' has no 'Dispose' member; it cannot be used with 'use'" simple
                        )
                | ValueNone ->
                    let qual = SymbolKeyOps.qualifiedName clsKey

                    match tryExternalDispose ctx qual args with
                    | ValueSome key -> ctx.Resolution.UseDispose.Set(patKey, key)
                    | ValueNone ->
                        ctx.Error(
                            patKey,
                            sprintf "The type '%s' has no 'Dispose' member; it cannot be used with 'use'" qual
                        )
            | _ -> ()
        | _ -> ()

    and private inferLet
        (ctx: PassContext)
        (key: NodeKey)
        (keyword: LetOrUseKeyword<SyntaxToken>)
        (bindings: ImmutableArray<Binding<SyntaxToken>>)
        (body: Expr<SyntaxToken> voption)
        : SemType =
        inferBindingGroup ctx bindings

        // `use` binds a disposable: resolve each binder's `Dispose` so an external
        // (BCL) disposal can be keyed for codegen and a non-disposable diagnosed
        // (§4.3). `let` skips this.
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
        let savedScope = ctx.Resolution.TyparScope
        ctx.Resolution.TyparScope <- Dictionary<string, TypeVar>(System.StringComparer.Ordinal)

        // Inherit the lexically-enclosing binding's typars (lowest priority) so a
        // named typar inside a *nested* `let` resolves to the same TyVar as the
        // enclosing function's — F#'s lexical typar scoping. Without this, a
        // nested `let rec loop (t': Tree<'T>) …` inside a generic module function
        // `toList (t: Tree<'T>)` would mint a *fresh* `'T`, generalise `loop` over
        // it independently, and leave the (now decoupled) typar ungrounded — a
        // leaked `TyVar` that surfaces only at codegen (a closure capturing `t'`
        // froze with `Tree<?ungrounded>`). `savedScope` is the enclosing binding's
        // scope precisely because the `finally` restores it per binding, so a
        // *sibling* binding (already restored) never bleeds through — only a true
        // lexical parent does. Enclosing-type / member typars override below.
        for kv in savedScope do
            ctx.Resolution.TyparScope.[kv.Key] <- kv.Value

        // Seed the enclosing type's typars (class / union `<'T>`) next so a
        // generic member's signature annotation (`(x: 'T)`, `: Set<'T>`) resolves
        // them rather than diagnosing "Free type parameter 'T" under strict scope.
        // The binding's own `<'a>` typars seed below, shadowing on a name clash.
        // `EnclosingTypars` carries the class typars (G11) and, for a generic
        // member's body walk, the member's own explicit `<'C>` + implicit signature
        // typars (G12) — so both the signature annotation here and any nested `let`
        // in the body resolve them rather than diagnosing them free under strict
        // member scope.
        match ctx.Resolution.EnclosingTypars with
        | ValueSome enclosing ->
            for kv in enclosing do
                ctx.Resolution.TyparScope.[kv.Key] <- kv.Value
        | ValueNone -> ()

        seedBindingTypars ctx b

        match b.typarDefns with
        | ValueSome(TyparDefns(constraints = ValueSome cs)) -> translateConstraints ctx cs
        | _ -> ()

        // The member-typar seed (B-12) is for this binding's own typars only;
        // clear it so a nested `let`-binding in the body mints fresh typars
        // rather than reusing the member's prototypes.
        ctx.Resolution.BindingTyparSeed <- ValueNone

        try
            let patTy = inferPat ctx b.headPat

            // Typar order is explicit `<'T>` → args → return → body, all sharing
            // one TyparScope. The return annotation is translated *before* the
            // body so a return-only typar (`let f () : 'T list = …`) seeds the
            // scope first; otherwise the body would mint a fresh `'T` and the
            // return would translate into a different one.
            let rhsTy =
                if b.argumentPats.IsEmpty then
                    match b.returnType with
                    | ValueSome(ReturnType(typ = t)) ->
                        let annTy = translateType ctx t
                        let bodyTy = infer ctx b.expr
                        // Annotation reconciliation: `unifyAnnotation` admits the
                        // value→union assignability (`let x: int | string = 1`) while
                        // staying symmetric `unify` for every nominal/`obj` annotation.
                        unifyAnnotation ctx (CstKeys.ofBinding b) bodyTy annTy
                        annTy
                    | ValueNone -> infer ctx b.expr
                else
                    let argTypes = [ for p in b.argumentPats -> inferPat ctx p ]

                    let bodyTy =
                        match b.returnType with
                        | ValueSome(ReturnType(typ = t)) ->
                            let annTy = translateType ctx t
                            let bodyTy = infer ctx b.expr
                            // Annotation reconciliation against the written return type
                            // — see the no-arg twin above.
                            unifyAnnotation ctx (CstKeys.ofBinding b) bodyTy annTy
                            annTy
                        | ValueNone -> infer ctx b.expr

                    List.foldBack (fun a r -> TyFun(a, r)) argTypes bodyTy

            unify ctx (CstKeys.ofBinding b) patTy rhsTy
        finally
            ctx.Resolution.TyparScope <- savedScope

    /// Type a `let` / `let rec` group with Rémy-level discipline. Key
    /// subtlety: pre-allocate single-name sibling headPat TyVars (step 2) so
    /// forward references from inside one RHS (or a nested let) find the
    /// sibling's TyVar at this group's level rather than lazy-minting at a
    /// deeper one — which would let a nested let generalise a var that
    /// actually belongs to an un-typed outer sibling. RHSes type at the
    /// pushed level (sibling lookups stay monomorphic — no scheme written
    /// yet); generalisation happens against the outer level after popping.
    and inferBindingGroup (ctx: PassContext) (bindings: ImmutableArray<Binding<SyntaxToken>>) : unit =
        let outerLevel = ctx.CurrentLevel
        enterLevel ctx

        for b in bindings do
            match b.headPat with
            | Pat.NamedSimple _
            | Pat.Op _ ->
                let key = CstKeys.ofPat b.headPat
                tvOf ctx key |> ignore
                // Drop any annotation-derived forward scheme
                // (`prebindModuleFunctionSchemes`) so this group's bodies type with
                // monomorphic self/sibling references — no polymorphic recursion,
                // exactly as before the pre-pass. The real scheme is rebuilt below.
                ctx.Bindings.Scheme.Remove key
            | _ -> ()

        for b in bindings do
            inferBinding ctx b

        exitLevel ctx

        for b in bindings do
            if shouldGeneralise b then
                let key = CstKeys.ofPat b.headPat
                let headTv = tvOf ctx key
                let zonked = zonk (TyVar headTv)

                if not (hasPendingDotAccess zonked) then
                    // Settle flexible list-literal containers first (R3), then
                    // re-zonk so the (now-linked) FSharpList element generalises.
                    prepareListLiterals ctx zonked outerLevel
                    let scheme = generalise (zonk zonked) outerLevel
                    ctx.Bindings.Scheme.Set(key, scheme)
