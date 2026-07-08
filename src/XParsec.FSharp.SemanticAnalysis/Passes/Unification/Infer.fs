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
open UnificationInferControlFlow
open UnificationInferIdentExpr
open UnificationInferApp

module UnificationInfer =

    /// Peel paren / ascription wrappers to a binding RHS's underlying format-string
    /// literal (`let fmt : Fmt = "%d"` → the `"%d"`; `let fmt = ("%d" : Fmt)` → the
    /// inner `"%d"`). `ValueNone` unless the peeled expr is an `Expr.String` whose
    /// specifiers parse (`formatSpecifiers`). Drives the E1(b) const-prop registration
    /// in `inferBinding` — gated there on the binding's type being a `PrintfFormat`,
    /// so a plain-string `let s = "%d"` (which reaches printf nowhere) is never
    /// recorded.
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
            | Expr.EmptyBlock _ -> ctx.Intrinsics.Unit
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
            // `recv?name <- value` — the dynamic setter, routed through
            // `op_DynamicAssignment` (parses as `Assignment(DynamicLookup(...), v)`).
            | Expr.Assignment(leftExpr = Expr.DynamicLookup(expr = recv); rightExpr = right) ->
                inferDynamicSet infer ctx key recv right
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
            // `recv?name` — dynamic member access, routed through the `op_Dynamic`
            // operator so its `default ^TResult : dynamic` drives target typing.
            | Expr.DynamicLookup(expr = recv) -> inferDynamicLookup infer ctx key recv
            | _ ->
                // Surface the unhandled case loudly rather than fabricating a
                // free TyVar and silently producing a broken type for every
                // use site. Matches the precedent in
                // `Freeze.translateExpr` (file: Freeze.fs).
                failwithf "infer: TODO %A" e

        nodeTv.Link <- ValueSome inferredTy
        inferredTy

    /// Resolve a keyed `Dispose` for a `use` binder of *external* (BCL) type.
    /// The PRIMARY qualifier is the disposable-capability *interface*: scan the type's
    /// (instantiated) interfaces for `caps.Disposable` and mint the interface's
    /// `Dispose` member key. This is the real-F# rule (a `use` binder must implement
    /// `System.IDisposable`) and covers the common BCL case where `Dispose` is declared
    /// on a base — `MemoryStream` inherits `Stream.Dispose`, so the `DeclaredOnly`
    /// `TryLookupMember` misses it but `GetInterfaces` surfaces the interface
    /// transitively. The own-`Dispose` fallback survives only for a *non-`IDisposable`*
    /// ref struct (it can't be boxed to the interface, so its own pattern `Dispose()` is
    /// called directly). `ValueNone` ⇒ not disposable. The interface key is the
    /// §5.0-resolved disposable identity (`ctx.CapabilityIds.Disposable`), NOT a
    /// hardcoded `System.IDisposable` — so this `dispose` key crosses Freeze
    /// target-neutrally (each backend lowers it to its own slot: the CLR
    /// `IDisposable::Dispose`, the JS `Symbol.dispose`).
    and private tryExternalDispose (ctx: PassContext) (name: string) (args: EqArray<SemType>) : SymbolKey voption =
        // The directly-implemented interface set an external nominal carries today: a
        // class's `FrozenInterfaces` or a union's `interface <ty>` impls (the union
        // analogue, the cons-list's `interface seq<'T>` channel). Scanned kind-agnostically
        // so a BCL/contract union that is `IDisposable`-via-interface disposes through the
        // slot, exactly as `InferControlFlow.tryForInEnumerator` admits Class+Union for
        // iteration. (An external RECORD carries no interfaces — `DeferredBody.Record`
        // captures no `interface` CSTs — so a disposable external record resolves only via
        // its own `Dispose` below; that boundary moves the day records gain a contract
        // interface channel.)
        let externalInterfaces () : (string * SemType[])[] =
            match ctx.Provider.TryLookupType name with
            | ValueSome(ExternalTypeShape.Class shape) ->
                ExternalSymbols.instantiateInterfaces shape (args.AsSpan().ToArray())
            | ValueSome(ExternalTypeShape.Union(_, _, ifaces, _)) ->
                ExternalSymbols.instantiateInterfacesOf ifaces (args.AsSpan().ToArray())
            | _ -> [||]

        let viaInterface =
            match ctx.CapabilityIds.Disposable with
            | ValueSome disp when externalInterfaces () |> Array.exists (fun (n, _) -> disp.MatchesName n) ->
                ValueSome(SymbolKey.MemberKey(disp.Key, "Dispose", EqArray.empty, MemberKind.Method))
            | _ -> ValueNone

        match viaInterface with
        | ValueSome _ -> viaInterface
        // Fallback for an external non-`IDisposable` ref struct: its own pattern
        // `Dispose()`, which can't be reached through a boxed interface slot.
        | ValueNone ->
            match ctx.Provider.TryLookupMember(name, "Dispose") with
            | ValueSome m when not m.IsStatic && not m.IsValueMember -> ValueSome m.Key
            | _ -> ValueNone

    /// True iff a project-local nominal type (class / union / record) implements the
    /// disposable capability interface — its `InterfaceImpls` carry a resolved interface
    /// whose head key matches `caps.Disposable`. Mirrors
    /// `InferControlFlow.probeLocalEnumerator`'s for-in finally probe.
    and private localImplementsDisposable
        (ctx: PassContext)
        (host: IInterfaceImplHost)
        (args: EqArray<SemType>)
        : bool =
        host.InterfaceImpls
        |> Array.exists (fun impl ->
            match impl.Resolved with
            | ValueSome resolved ->
                match zonk (instantiateMember (host.TypeParams, args) resolved) with
                | TyClass(ifaceKey, _) -> RuntimeNames.matchesKey ctx.CapabilityIds.Disposable ifaceKey
                | _ -> false
            | ValueNone -> false
        )

    /// The ref-struct carve-out: a `[<IsByRefLike>]` class can't be boxed to
    /// `IDisposable`, so a duck-typed pattern `Dispose()` is disposed by calling its
    /// own method directly — recorded as a keyed member call so Freeze stamps
    /// `dispose = ValueSome own-key` (each backend then calls the binder's own method,
    /// NOT the capability slot). Returns the own-`Dispose` member key when the class is
    /// byref-like and exposes such a member; `ValueNone` otherwise.
    and private tryRefStructOwnDispose (ctx: PassContext) (clsKey: SymbolKey) : SymbolKey voption =
        match TypeRegistry.tryClassByKey ctx.Types clsKey with
        | ValueSome info when info.IsByRefLike ->
            let hasDispose =
                info.Members
                |> Array.exists (fun m -> m.Name = "Dispose" && not m.IsStatic && m.Kind = ClassMemberKind.Method)

            if hasDispose then
                ValueSome(SymbolKey.MemberKey(clsKey, "Dispose", EqArray.empty, MemberKind.Method))
            else
                ValueNone
        | _ -> ValueNone

    /// Resolve the disposal target for one `use` binding. The §3b flip makes disposal
    /// INTERFACE-REQUIRED (real-F# parity): a *project-local* binder qualifies iff it
    /// implements the `disposable` capability interface — recorded as nothing so Freeze
    /// leaves `TExpr.Use.dispose = ValueNone` (each backend lowers to its own slot: the
    /// CLR `IDisposable::Dispose`, the JS `[Symbol.dispose]`). A `[<IsByRefLike>]` ref
    /// struct that can't implement the interface but exposes a pattern `Dispose` is the
    /// carve-out — its own method is recorded keyed (`ValueSome`). An *external* binder's
    /// keyed `Dispose` (interface, or an own-`Dispose` ref-struct fallback) is stashed in
    /// `UseDispose` for Freeze. A binder that is none of these is a `use`-over-non-
    /// disposable error; an unresolved binder type is left alone (pre-existing behaviour).
    and private resolveUseDispose (ctx: PassContext) (b: Binding<SyntaxToken>) : unit =
        match b.headPat with
        | Pat.NamedSimple _ ->
            let patKey = CstKeys.ofPat b.headPat
            let binderTy = zonk (TyVar(tvOf ctx patKey))

            let notDisposable (display: string) =
                ctx.Error(
                    patKey,
                    sprintf
                        "The type '%s' cannot be used with 'use': a 'use' binding requires its type to implement 'disposable' ('System.IDisposable')"
                        display
                )

            // A project-local nominal binder (class / union / record). It qualifies for
            // `use` iff it implements the `disposable` capability interface; else the
            // ref-struct carve-out; else an error.
            let resolveLocal (host: IInterfaceImplHost) (headKey: SymbolKey) (simple: string) (args: EqArray<SemType>) =
                if localImplementsDisposable ctx host args then
                    ()
                else
                    match tryRefStructOwnDispose ctx headKey with
                    | ValueSome key -> ctx.Resolution.UseDispose.Set(patKey, key)
                    | ValueNone -> notDisposable simple

            // Any nominal binder (class / union / record) resolves the same way: a
            // project-local host qualifies via its interface impls (or the ref-struct
            // carve-out), otherwise it must be an external `disposable` — else it's a
            // `use`-over-non-disposable error. `tryExternalDispose` scans whichever
            // interface set the external shape carries (class or union); an external
            // record carries none, so it resolves only via an own-`Dispose` there. This
            // routes all three kinds rather than silently accepting an unknown head.
            match resolveStep binderTy with
            | TyClass(headKey, args)
            | TyUnion(headKey, args)
            | TyRecord(headKey, args) ->
                // Resolve the local host by the arity-qualified key, not the bare
                // name: an arity-overloaded host (`Foo`2`/`Foo`3`) has no bare alias.
                // `simple` is kept only for the diagnostic text.
                let simple = SymbolKeyOps.simpleName headKey

                match TypeRegistry.tryInterfaceImplHostByKey ctx.Types headKey with
                | ValueSome host -> resolveLocal host headKey simple args
                | ValueNone ->
                    let qual = SymbolKeyOps.qualifiedName headKey

                    match tryExternalDispose ctx qual args with
                    | ValueSome key -> ctx.Resolution.UseDispose.Set(patKey, key)
                    | ValueNone -> notDisposable qual
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

        // Capture the binding's explicit `<'b,'a>` typars in SOURCE order, paired
        // with the TypeVar `seedBindingTypars` just bound for each, while the
        // transient `TyparScope` is still live — it is restored per binding (the
        // `finally` below), so this mapping is unrecoverable by Elaborate, which
        // needs it to order a free function's method typars declared-first (the F#
        // rule). Keyed by the binding so a nested/sibling binding cannot collide.
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

                        // Type provenance: a value binding `let x : T = e` writes the
                        // binder's type explicitly.
                        ctx.MarkTypeDeclared(CstKeys.ofPat b.headPat, annTy)

                        // E1(a): a format-string literal bound to a `PrintfFormat`-family
                        // annotation (`let fmt : StringFormat<_> = "%d"`) types AS the
                        // format, not `string`. Skip `infer` on the literal (it would type
                        // it `string`); the helper unifies the specifiers' printer into the
                        // annotation (pinning a `<_>` wildcard printer), and we stamp the
                        // annotation's format type onto the literal node.
                        match tryTypeFormatLiteral ctx (CstKeys.ofBinding b) b.expr annTy with
                        | ValueSome fmt ->
                            (freshTv ctx (CstKeys.ofExpr b.expr)).Link <- ValueSome fmt
                            annTy
                        | ValueNone ->
                            let bodyTy = infer ctx b.expr
                            // Annotation reconciliation: `unifyAnnotation` admits the
                            // value→union assignability (`let x: int | string = 1`) and the
                            // concrete-subtype→supertype upcast (`: exn = e`) while staying
                            // symmetric `unify` for every other nominal annotation.
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

                            // Type provenance: a `let f … : T = body` return annotation
                            // writes the BODY's type explicitly (each parameter's
                            // provenance is recorded independently by `inferPat`, so a
                            // partially-annotated binding is never overstated).
                            ctx.MarkTypeDeclared(CstKeys.ofExpr b.expr, annTy)

                            // Annotation reconciliation against the written return type
                            // — see the no-arg twin above.
                            unifyAnnotation ctx (CstKeys.ofBinding b) bodyTy annTy
                            annTy
                        | ValueNone -> infer ctx b.expr

                    List.foldBack (fun a r -> TyFun(a, r)) argTypes bodyTy

            unify ctx (CstKeys.ofBinding b) patTy rhsTy

            // E1(b) const-prop registration: a binding whose value is a format-string
            // literal (possibly paren/ascription-wrapped) AND whose type resolved to a
            // `PrintfFormat` gets its literal stashed by binding site, so a later
            // `sprintf fmt …` recovers it and lowers natively (§ `PrintfFormatLiterals`
            // — there is no cold runtime for a format value in the self-host contract).
            // Gated on the `PrintfFormat` type so an unannotated plain-string `let`
            // (which cannot legally reach a printf format slot) is never recorded.
            match resolveStep patTy with
            | TyClass(fmtKey, _) when RuntimeNames.isPrintfFormatKey fmtKey ->
                match peelToFormatString ctx b.expr with
                | ValueSome lit -> ctx.PrintfFormatLiterals.Set(CstKeys.ofPat b.headPat, lit)
                | ValueNone -> ()
            | _ -> ()
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
