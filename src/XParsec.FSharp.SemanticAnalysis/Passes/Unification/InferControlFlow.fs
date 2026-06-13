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

module internal UnificationInferControlFlow =

    let rec inferIfThenElse
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (cond: Expr<SyntaxToken>)
        (thenE: Expr<SyntaxToken>)
        (elifs: ImmutableArray<ElifBranch<SyntaxToken>>)
        (elseB: ElseBranch<SyntaxToken> voption)
        : SemType =
        let condTy = infer ctx cond
        unify ctx key condTy BuiltinTypes.tyBool

        let thenTy = infer ctx thenE

        for elif_ in elifs do
            let elifCond, elifExpr =
                match elif_ with
                | ElifBranch.Elif(condition = c; expr = e)
                | ElifBranch.ElseIf(condition = c; expr = e) -> c, e

            let elifCondTy = infer ctx elifCond
            unify ctx key elifCondTy BuiltinTypes.tyBool
            let elifTy = infer ctx elifExpr
            unify ctx key thenTy elifTy

        match elseB with
        | ValueSome(ElseBranch(expr = elseExpr)) ->
            let elseTy = infer ctx elseExpr
            unify ctx key thenTy elseTy
            thenTy
        | ValueNone ->
            // `if c then e` (no else): the then-branch must be `unit` and the whole
            // expression is `unit` (F# spec — a missing else is `else ()`). The elif
            // branches above were already unified with `thenTy`, so this one `unify`
            // forces all branches to `unit`.
            unify ctx key thenTy BuiltinTypes.tyUnit
            BuiltinTypes.tyUnit

    and inferFun
        (infer: Infer)
        (ctx: PassContext)
        (argPats: ImmutableArray<Pat<SyntaxToken>>)
        (body: Expr<SyntaxToken>)
        : SemType =
        let argTypes = [ for p in argPats -> inferPat ctx p ]
        let bodyTy = infer ctx body
        List.foldBack (fun a r -> TyFun(a, r)) argTypes bodyTy

    and inferTuple (infer: Infer) (ctx: PassContext) (items: ImmutableArray<Expr<SyntaxToken>>) : SemType =
        TyTuple(EqArray.ofSeq (seq { for e in items -> infer ctx e }))

    and inferSequential
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (items: ImmutableArray<Expr<SyntaxToken>>)
        : SemType =
        if items.Length = 0 then
            BuiltinTypes.tyUnit
        else
            for i = 0 to items.Length - 2 do
                let ty = infer ctx items.[i]
                unify ctx key ty BuiltinTypes.tyUnit

            infer ctx items.[items.Length - 1]

    and inferWhile
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (cond: Expr<SyntaxToken>)
        (body: Expr<SyntaxToken>)
        : SemType =
        let condTy = infer ctx cond
        unify ctx key condTy BuiltinTypes.tyBool
        let bodyTy = infer ctx body
        unify ctx key bodyTy BuiltinTypes.tyUnit
        BuiltinTypes.tyUnit

    and inferForTo
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (ident: SyntaxToken)
        (startE: Expr<SyntaxToken>)
        (endE: Expr<SyntaxToken>)
        (body: Expr<SyntaxToken>)
        : SemType =
        let startTy = infer ctx startE
        unify ctx key startTy BuiltinTypes.tyInt
        let endTy = infer ctx endE
        unify ctx key endTy BuiltinTypes.tyInt
        let varKey = CstKeys.ofForToVar ident
        let varTv = freshTv ctx varKey
        varTv.Link <- ValueSome BuiltinTypes.tyInt
        let bodyTy = infer ctx body
        unify ctx key bodyTy BuiltinTypes.tyUnit
        BuiltinTypes.tyUnit

    /// The §4.4 duck-typed enumerator probe: C#'s pattern-based `foreach` accepts
    /// any source exposing a public parameterless `GetEnumerator()` whose return
    /// type `E` exposes `MoveNext(): bool` and a `Current` property — no
    /// `IEnumerable<'T>` required (`List<'T>` hands back its non-boxing
    /// `struct Enumerator` this way). Returns the element type (`Current`'s type)
    /// and the resolved `DuckTyped` descriptor so codegen can pick value-receiver
    /// emission. `srcArgs` are the source class's type arguments — the substitution
    /// for `GetEnumerator`'s (and thereby `E`'s) typars.
    and tryDuckTypedEnumerator
        (ctx: PassContext)
        (shape: ExternalClassShape)
        (srcArgs: SemType[])
        : (SemType * ForInEnumerator) voption =
        match
            shape.Members
            |> Array.tryFind (fun m -> m.Name = "GetEnumerator" && not m.IsStatic && not m.IsProperty)
        with
        | None -> ValueNone
        | Some ge ->
            // `GetEnumerator` reads as `unit → E`; `E` carries the enumerator type's
            // own instantiation (`List`1+Enumerator` over the source's `'T`).
            match ExternalSymbols.openSignature ge srcArgs with
            | TyFun(_, (TyClass(enumKey, enumArgsEq) as enumTy)) ->
                match ExternalSymbols.tryLookupType ctx.Provider enumKey with
                | ValueSome(ExternalTypeShape.Class enumShape) ->
                    let enumArgs = enumArgsEq.AsSpan().ToArray()

                    let moveNext =
                        enumShape.Members
                        |> Array.tryFind (fun m -> m.Name = "MoveNext" && not m.IsStatic && not m.IsProperty)

                    let current =
                        enumShape.Members
                        |> Array.tryFind (fun m -> m.Name = "Current" && not m.IsStatic && m.IsProperty)

                    match moveNext, current with
                    | Some mn, Some cur ->
                        match ExternalSymbols.openSignature mn enumArgs with
                        | TyFun(_, TyConst("bool", _)) ->
                            let elemTy = ExternalSymbols.openSignature cur enumArgs

                            // The enumerator only needs disposing — and the `finally`
                            // region only exists — when it is `IDisposable` (C# parity).
                            let dispose =
                                if
                                    ExternalSymbols.instantiateInterfaces enumShape enumArgs
                                    |> Array.exists (fun (n, _) -> n = "System.IDisposable")
                                then
                                    ValueSome(
                                        SymbolKey.MemberKey(
                                            SymbolKey.TypeKey(None, "System", "IDisposable"),
                                            "Dispose",
                                            EqArray.empty,
                                            MemberKind.Method
                                        )
                                    )
                                else
                                    ValueNone

                            ValueSome(
                                elemTy,
                                ForInEnumeratorG.DuckTyped(
                                    enumTy,
                                    ge.Key,
                                    mn.Key,
                                    cur.Key,
                                    enumShape.Flags.IsValueType,
                                    dispose
                                )
                            )
                        | _ -> ValueNone
                    | _ -> ValueNone
                | _ -> ValueNone
            | _ -> ValueNone

    /// Gap 2 pure-pattern variant: the project-local analogue of
    /// `tryDuckTypedEnumerator`. A user class exposing a parameterless
    /// `GetEnumerator()` whose return type `E` is *itself* a user class with
    /// `MoveNext(): bool` and a `Current` property is a valid `for … in` source
    /// even without implementing `IEnumerable<'T>` (C#'s non-boxing `foreach`,
    /// project-local). Handles a reference or value-type (`[<Struct>]`) user
    /// enumerator — the latter walks by address (`constrained.`), no boxing. An
    /// *external* enumerator type (codegen routes user members through the local
    /// machinery only) still falls back to `ValueNone`, letting the interface probe
    /// or the "not a supported enumerable" diagnostic take over.
    and tryLocalDuckTypedEnumerator
        (ctx: PassContext)
        (nameKey: SymbolKey)
        (args: EqArray<SemType>)
        : (SemType * ForInEnumerator) voption =
        match TypeRegistry.tryClassByKey ctx.Types nameKey with
        | ValueSome info ->
            match
                info.Members
                |> Array.tryFind (fun m ->
                    m.Name = "GetEnumerator" && not m.IsStatic && m.Kind = ClassMemberKind.Method
                )
            with
            | Some ge ->
                // `GetEnumerator : unit → E`; instantiate the source's typars so `E`
                // carries the use-site element type.
                match zonk (instantiateMember (info.TypeParams, args) ge.Type) with
                | TyFun(_, (TyClass(enumKey, enumArgs) as enumTy)) ->
                    match TypeRegistry.tryClassByKey ctx.Types enumKey with
                    // A user enumerator, reference or value-type: a `[<Struct>]`
                    // enumerator walks by address (`ldloca` + `constrained. <E>`),
                    // the non-boxing path the value-receiver member-call IL already
                    // emits for local struct members.
                    | ValueSome enumInfo ->
                        let moveNext =
                            enumInfo.Members
                            |> Array.tryFind (fun m ->
                                m.Name = "MoveNext" && not m.IsStatic && m.Kind = ClassMemberKind.Method
                            )

                        let current =
                            enumInfo.Members
                            |> Array.tryFind (fun m ->
                                m.Name = "Current" && not m.IsStatic && m.Kind = ClassMemberKind.Property
                            )

                        match moveNext, current with
                        | Some mn, Some cur ->
                            match zonk (instantiateMember (enumInfo.TypeParams, enumArgs) mn.Type) with
                            | TyFun(_, TyConst("bool", _)) ->
                                let elemTy = instantiateMember (enumInfo.TypeParams, enumArgs) cur.Type

                                // C# parity: only a disposable enumerator gets a
                                // `finally`. Scan the user enumerator's interface
                                // impls for `System.IDisposable`; codegen disposes
                                // through the interface slot regardless of where the
                                // member is stored.
                                let dispose =
                                    enumInfo.InterfaceImpls
                                    |> Array.exists (fun impl ->
                                        match impl.Resolved with
                                        | ValueSome resolved ->
                                            match
                                                zonk (instantiateMember (enumInfo.TypeParams, enumArgs) resolved)
                                            with
                                            | TyClass(ifaceKey, _) ->
                                                SymbolKeyOps.qualifiedName ifaceKey = "System.IDisposable"
                                            | _ -> false
                                        | ValueNone -> false
                                    )

                                ValueSome(elemTy, ForInEnumeratorG.UserDuckTyped(enumTy, enumInfo.IsValueType, dispose))
                            | _ -> ValueNone
                        | _ -> ValueNone
                    | _ -> ValueNone
                | _ -> ValueNone
            | None -> ValueNone
        | ValueNone -> ValueNone

    and tryLocalInterfaceEnumerator
        (ctx: PassContext)
        (nameKey: SymbolKey)
        (args: EqArray<SemType>)
        : (SemType * ForInEnumerator) voption =
        let ienumName = "System.Collections.Generic.IEnumerable`1"

        match TypeRegistry.tryClassByKey ctx.Types nameKey with
        | ValueSome info ->
            let picked =
                info.InterfaceImpls
                |> Array.tryPick (fun impl ->
                    match impl.Resolved with
                    | ValueSome resolved ->
                        match zonk (instantiateMember (info.TypeParams, args) resolved) with
                        | TyClass(ifaceKey, ifaceArgs) when
                            SymbolKeyOps.qualifiedName ifaceKey = ienumName && ifaceArgs.Length = 1
                            ->
                            Some(ifaceArgs.[0], ForInEnumeratorG.Interface)
                        | _ -> None
                    | ValueNone -> None
                )

            match picked with
            | Some r -> ValueSome r
            | None -> ValueNone
        | ValueNone -> ValueNone

    /// `srcTy` is either `IEnumerable<'T>` itself, an external class that
    /// implements it (the directly-implemented interface set the metadata layer
    /// surfaces through `ExternalClassShape.Interfaces`), or
    /// a source exposing a pattern-based `GetEnumerator()`. Returns the
    /// `'T` so `inferForIn` can pin the loop pattern's type, plus the
    /// `ForInEnumerator` codegen reads off the frozen node.
    and tryForInEnumerator (ctx: PassContext) (srcTy: SemType) : (SemType * ForInEnumerator) voption =
        let ienumName = "System.Collections.Generic.IEnumerable`1"

        match zonk srcTy with
        | TyClass(nameKey, args) when SymbolKeyOps.qualifiedName nameKey = ienumName && args.Length = 1 ->
            ValueSome(args.[0], ForInEnumeratorG.Interface)
        | TyClass(nameKey, args) ->
            match ExternalSymbols.tryLookupType ctx.Provider nameKey with
            | ValueSome(ExternalTypeShape.Class shape) ->
                let argArr = args.AsSpan().ToArray()

                // C# precedence: a pattern-based `GetEnumerator()` wins over the
                // `IEnumerable<'T>` interface, so `List<'T>` walks its non-boxing
                // struct `Enumerator` (§4.4) rather than the boxing interface
                // enumerator (now that value-type member-call emission has landed).
                // Fall back to the interface shape (§4.2) for a source that only
                // implements `IEnumerable<'T>` and exposes no usable pattern
                // `GetEnumerator()`.
                match tryDuckTypedEnumerator ctx shape argArr with
                | ValueSome r -> ValueSome r
                | ValueNone ->
                    match
                        ExternalSymbols.instantiateInterfaces shape argArr
                        |> Array.tryPick (fun (n, ta) -> if n = ienumName && ta.Length = 1 then Some ta.[0] else None)
                    with
                    | Some elem -> ValueSome(elem, ForInEnumeratorG.Interface)
                    | None -> ValueNone
            // A project-local source is invisible to the external provider; fall
            // back to the user probes. C# precedence: a pattern `GetEnumerator()`
            // (Gap 2 pure-pattern variant) wins over the `IEnumerable<'T>`
            // interface (Gap 2 interface variant).
            | _ ->
                match tryLocalDuckTypedEnumerator ctx nameKey args with
                | ValueSome r -> ValueSome r
                | ValueNone -> tryLocalInterfaceEnumerator ctx nameKey args
        | _ -> ValueNone

    and inferForIn
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (pat: Pat<SyntaxToken>)
        (src: Expr<SyntaxToken>)
        (body: Expr<SyntaxToken>)
        : SemType =
        // Int-range source: element type is int. Any other source must be an
        // `IEnumerable<'T>` (a BCL collection in v1, B-6) — the element type is
        // recovered from its interface set and the loop pattern unified with it.
        let srcTy = infer ctx src
        let patTy = inferPat ctx pat

        let isRangeSource =
            match src with
            | Expr.Range _
            | Expr.SteppedRange _ -> true
            | Expr.EnclosedBlock(expr = Expr.Range _)
            | Expr.EnclosedBlock(expr = Expr.SteppedRange _) -> true
            | _ -> false

        if isRangeSource then
            unify ctx key srcTy BuiltinTypes.tySeqInt
            unify ctx key patTy BuiltinTypes.tyInt
        else
            match tryForInEnumerator ctx srcTy with
            | ValueSome(elemTy, shape) ->
                unify ctx key patTy elemTy
                ctx.Resolution.ForInShape.Set(key, shape)
            | ValueNone ->
                ctx.Error(
                    key,
                    "for-in: source is not a supported enumerable (expected IEnumerable<'T> or a pattern-based GetEnumerator())"
                )

        let bodyTy = infer ctx body
        unify ctx key bodyTy BuiltinTypes.tyUnit
        BuiltinTypes.tyUnit

    and inferRules
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (scrutineeTy: SemType)
        (resultTy: SemType)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : unit =
        for r in rules do
            match r with
            | Rule.Rule(pat = pat; guard = guard; expr = body) ->
                let patTy = inferPat ctx pat
                unify ctx key patTy scrutineeTy

                match guard with
                | ValueSome(PatternGuard(expr = g)) ->
                    let gTy = infer ctx g
                    unify ctx key gTy BuiltinTypes.tyBool
                | ValueNone -> ()

                let bodyTy = infer ctx body
                unify ctx key bodyTy resultTy
            | _ -> ()

    and inferMatch
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (scrutinee: Expr<SyntaxToken>)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : SemType =
        let scrutineeTy = infer ctx scrutinee
        let resultTy = TyVar(freshTyVar ctx)
        inferRules infer ctx key scrutineeTy resultTy rules
        resultTy

    and inferFunction
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : SemType =
        // `function … ` ~ `fun x -> match x with …`. The synthesised
        // parameter's TypeVar IS the scrutinee's — every arm's pattern
        // unifies with it.
        let paramTy = TyVar(freshTyVar ctx)
        let resultTy = TyVar(freshTyVar ctx)
        inferRules infer ctx key paramTy resultTy rules
        TyFun(paramTy, resultTy)

    and inferTryWith
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (body: Expr<SyntaxToken>)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : SemType =
        // Until a real `exn` type lands, pin the scrutinee to placeholder
        // `TyConst "exn"`. A fresh TyVar would let wildcard / variable arm
        // patterns carry an unresolved TyVar into the TAST, which
        // `ResolvedTypes` correctly flags.
        let resultTy = infer ctx body
        let exnTy = TyConst("exn", EqArray.empty)
        inferRules infer ctx key exnTy resultTy rules
        resultTy

    and inferTryFinally
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (body: Expr<SyntaxToken>)
        (finallyE: Expr<SyntaxToken>)
        : SemType =
        let resultTy = infer ctx body
        let finallyTy = infer ctx finallyE
        unify ctx key finallyTy BuiltinTypes.tyUnit
        resultTy

    and inferAssignment
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (left: Expr<SyntaxToken>)
        (right: Expr<SyntaxToken>)
        : SemType =
        // Mutability of the LHS is a Validation concern; here we only typecheck.
        let leftTy = infer ctx left
        let rightTy = infer ctx right
        unify ctx key leftTy rightTy
        BuiltinTypes.tyUnit
