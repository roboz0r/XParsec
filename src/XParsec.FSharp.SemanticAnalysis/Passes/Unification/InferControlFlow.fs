namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationSubsume
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

    /// The resolved members of an *external* duck-typed enumerator `E` — the shared
    /// result of probing `E`'s `ExternalClassShape` for `MoveNext(): bool` and a
    /// `Current` property. Both `Pattern` forms with an external `E` (external source
    /// and project-local source) wrap this identically as
    /// `ForInEnumMembers.External`; only the `ForInGetEnum` axis differs. `Disposable`
    /// is `true` iff `E : IDisposable`.
    type private ExternalEnumProbe =
        {
            ElemTy: SemType
            MoveNext: SymbolKey
            Current: SymbolKey
            IsValueType: bool
            Disposable: bool
        }

    /// Probe an external enumerator shape `E` for the duck-typed `for … in` members:
    /// a parameterless `MoveNext(): bool` and a `Current` property, with `enumArgs`
    /// being `E`'s own instantiation. `ValueNone` unless both are present and
    /// `MoveNext` returns `bool`.
    let private probeExternalEnumerator
        (ctx: PassContext)
        (enumShape: ExternalClassShape)
        (enumArgs: SemType[])
        : ExternalEnumProbe voption =
        let moveNext =
            enumShape.Members
            |> Array.tryFind (fun m -> m.Name = "MoveNext" && not m.IsStatic && not m.IsValueMember)

        let current =
            enumShape.Members
            |> Array.tryFind (fun m -> m.Name = "Current" && not m.IsStatic && m.IsValueMember)

        match moveNext, current with
        | Some mn, Some cur ->
            match ExternalSymbols.openSignature mn enumArgs with
            | TyFun(_, TyBool) ->
                // F# parity: the `finally` exists only when `E : IDisposable`. Disposal
                // is always the `System.IDisposable::Dispose` interface slot (codegen
                // mints it), so only the bool matters here, not a member key. (For the
                // future non-`IDisposable` ref-struct pattern-`Dispose()` case, see the
                // TODO on the local-enumerator branch of `tryLocalDuckTypedEnumerator`.)
                // Unnamed disposable ⇒ no finally fires (the structural path had its
                // chance; an unresolvable disposable surfaces downstream honestly) —
                // `matchesName` folds that `ValueNone` arm in.
                let disposable =
                    ExternalSymbols.instantiateInterfaces enumShape enumArgs
                    |> Array.exists (fun (n, _) -> RuntimeNames.matchesName ctx.CapabilityIds.Disposable n)

                ValueSome
                    {
                        ElemTy = ExternalSymbols.openSignature cur enumArgs
                        MoveNext = mn.Key
                        Current = cur.Key
                        IsValueType = enumShape.Flags.IsValueType
                        Disposable = disposable
                    }
            | _ -> ValueNone
        | _ -> ValueNone

    /// From a realised interface set (`(compiled-name, type-args)` pairs), pick the
    /// element type of the enumerable capability (`seq<'T>`/`IEnumerable<'T>`): the
    /// single type-arg of the first interface whose name matches the enumerable
    /// capability identity. Shared by the class and union arms of `tryForInEnumerator`.
    let private pickEnumerableElem (ctx: PassContext) (interfaces: (string * SemType[])[]) : SemType option =
        interfaces
        |> Array.tryPick (fun (n, ta) ->
            if RuntimeNames.matchesName ctx.CapabilityIds.Enumerable n && ta.Length = 1 then
                Some ta.[0]
            else
                None
        )

    /// Eagerly pin a flexible bare list-literal source to the Vesper cons-list.
    ///
    /// A bare `for x in [1;2;3]` leaves its source as a fresh `TypeVar` registered in
    /// `ctx.ListLiterals` (R3 defers the FSharpList-vs-Vesper choice to
    /// `resolveListLiterals`). But the for-in needs the source pinned NOW to read its
    /// enumerable surface, and the only list the compiler emits is the Vesper cons-list
    /// — which implements `seq<'T>` (§14.6). So flip the literal to
    /// `TyUnion(vesperListKey, [elem])` here, exactly the flip a `List.fold` consumer
    /// triggers; the union arm of `tryForInEnumerator` then admits it. (`1 :: 2 :: 3 ::
    /// []` already types as the Vesper union, so it bypasses this and is admitted
    /// directly.) A non-literal source is left untouched.
    let private pinListLiteralToVesper (ctx: PassContext) (key: NodeKey) (srcTy: SemType) : unit =
        match zonk srcTy with
        | TyVar tv ->
            match tryListLiteralElem ctx (UnionFind.find tv) with
            | ValueSome elemTy -> unify ctx key srcTy (TyUnion(RuntimeNames.vesperListKey, EqArray.singleton elemTy))
            | ValueNone -> ()
        | _ -> ()

    /// The project-local analogue of `probeExternalEnumerator`: probe a *user* class
    /// `E` for the duck-typed `for … in` members — a parameterless `MoveNext(): bool`
    /// and a `Current` property — with `enumArgs` being `E`'s own instantiation.
    /// Returns `(elemTy, isValueType, disposable)`; unlike the external probe it
    /// carries no member keys (the local axis is `ForInEnumMembers.Local`, so codegen
    /// resolves the members itself via `resolveInstanceMember`). `ValueNone` unless
    /// both members are present and `MoveNext` returns `bool`.
    let private probeLocalEnumerator
        (ctx: PassContext)
        (enumInfo: ClassTypeInfo)
        (enumArgs: EqArray<SemType>)
        : (SemType * bool * bool) voption =
        let inst (t: SemType) =
            zonk (instantiateMember (enumInfo.TypeParams, enumArgs) t)

        let moveNext =
            enumInfo.Members
            |> Array.tryFind (fun m -> m.Name = "MoveNext" && not m.IsStatic && m.Kind = ClassMemberKind.Method)

        let current =
            enumInfo.Members
            |> Array.tryFind (fun m -> m.Name = "Current" && not m.IsStatic && m.Kind = ClassMemberKind.Property)

        match moveNext, current with
        | Some mn, Some cur ->
            match inst mn.Type with
            | TyFun(_, TyBool) ->
                // F# parity: a `finally` exists only when `E : IDisposable`. Scan the
                // user enumerator's interface impls for `System.IDisposable`; codegen
                // disposes through the interface slot regardless of where the member is
                // stored.
                //
                // TODO (ref-struct pattern-Dispose): once a byref-like predicate exists
                // (`SemType` has no ref-struct case today — see `InlineExpansion.fs` /
                // `Regions.fs`), also dispose a *non-`IDisposable`* `[<IsByRefLike>]` `E`
                // that exposes a public `Dispose()`, calling its own method (a ref struct
                // can't be boxed to `IDisposable`). That mirrors the `use`-binder
                // precedent `Infer.tryExternalDispose` (prefer the type's own `Dispose`,
                // fall back to the interface slot) and would need the `Pattern`
                // descriptor's `dispose` to carry *which* `Dispose` to call, not just a
                // bool.
                let disposable =
                    enumInfo.InterfaceImpls
                    |> Array.exists (fun impl ->
                        match impl.Resolved with
                        | ValueSome resolved ->
                            match inst resolved with
                            | TyClass(ifaceKey, _) -> RuntimeNames.matchesKey ctx.CapabilityIds.Disposable ifaceKey
                            | _ -> false
                        | ValueNone -> false
                    )

                // `Current`'s instantiated type is the loop element type.
                ValueSome(instantiateMember (enumInfo.TypeParams, enumArgs) cur.Type, enumInfo.IsValueType, disposable)
            | _ -> ValueNone
        | _ -> ValueNone

    /// A short display name for an anonymous-union member in an incomplete-match
    /// diagnostic. v1 members are ground annotation types (`int`, `string`,
    /// `null`), so the bare `TyConst` name reads well; anything compound falls
    /// back to `%A`.
    let rec private describeUnionMember (m: SemType) : string =
        match resolveStep m with
        | TyConst(key, args) when args.IsEmpty -> SymbolKeyOps.simpleName key
        | TyOr inner ->
            inner.Members
            |> EqSet.toList
            |> List.map describeUnionMember
            |> String.concat " | "
        | other -> sprintf "%A" other

    /// Per-arm scrutinee narrowing for a closed anonymous-union match
    /// (`match (x: A | B) with …`). Pure over the arm
    /// *patterns* — it reads nothing from body typing — so it runs as a pre-pass
    /// ahead of `inferRules`'s typing loop. Returns the narrowed scrutinee each
    /// arm's binder should see (the residual union of members not yet caught by an
    /// earlier *unguarded* arm) paired 1:1 with `rules`, plus the final uncovered
    /// `residual`. A non-empty residual is a non-exhaustiveness warning — provable
    /// here because a *closed* union enumerates its members, unlike the open
    /// `obj`/inheritance case.
    ///
    /// For a non-union scrutinee every arm just sees `scrutineeTy` and the residual
    /// is empty (nothing to prove).
    let private computeArmNarrowing
        (ctx: PassContext)
        (scrutineeTy: SemType)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : SemType list * SemType list =
        match zonk scrutineeTy with
        | TyOr members ->
            // The members a `:? T` / `:? T as _` arm tests for, recursing through
            // `|` alternatives. Anything else tests no member.
            let rec patTests pat =
                match pat with
                | Pat.TypeTest(typ = t)
                | Pat.TypeTestAs(typ = t) -> [ translateType ctx t ]
                | Pat.Or(left = l; right = r) -> patTests l @ patTests r
                | Pat.EnclosedBlock(pat = p)
                | Pat.Attributed(pat = p) -> patTests p
                | _ -> []

            // A binder / wildcard with no type test catches the whole residual.
            let rec isCatchAll pat =
                match pat with
                | Pat.Wildcard _
                | Pat.NamedSimple _ -> true
                | Pat.As(pat = p)
                | Pat.EnclosedBlock(pat = p)
                | Pat.Attributed(pat = p) -> isCatchAll p
                | _ -> false

            let mutable residual = EqSet.toList members.Members
            let armScruts = ResizeArray(rules.Length)

            for r in rules do
                // The binder narrows against the members still live *before* this
                // arm. Once the residual is exhausted, fall back to the full
                // scrutinee rather than pin a redundant trailing binder to `never`.
                armScruts.Add(
                    if List.isEmpty residual then
                        scrutineeTy
                    else
                        mkUnion residual
                )

                // Shrink the residual by the members this arm definitively catches.
                // A guarded arm may fail at runtime, so it removes nothing.
                match r with
                | Rule.Rule(pat = pat; guard = ValueNone) ->
                    if isCatchAll pat then
                        residual <- []
                    else
                        let tests = patTests pat

                        if not (List.isEmpty tests) then
                            residual <-
                                residual
                                |> List.filter (fun m ->
                                    tests |> List.forall (fun tst -> subsumes ctx m tst = SubsumeOutcome.Unrelated)
                                )
                | _ -> ()

            List.ofSeq armScruts, residual
        | _ -> [ for _ in rules -> scrutineeTy ], []

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
        unify ctx key condTy ctx.Intrinsics.Bool

        let thenTy = infer ctx thenE

        for elif_ in elifs do
            let elifCond, elifExpr =
                match elif_ with
                | ElifBranch.Elif(condition = c; expr = e)
                | ElifBranch.ElseIf(condition = c; expr = e) -> c, e

            let elifCondTy = infer ctx elifCond
            unify ctx key elifCondTy ctx.Intrinsics.Bool
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
            unify ctx key thenTy ctx.Intrinsics.Unit
            ctx.Intrinsics.Unit

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
            ctx.Intrinsics.Unit
        else
            for i = 0 to items.Length - 2 do
                let ty = infer ctx items.[i]
                unify ctx key ty ctx.Intrinsics.Unit

            infer ctx items.[items.Length - 1]

    and inferWhile
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (cond: Expr<SyntaxToken>)
        (body: Expr<SyntaxToken>)
        : SemType =
        let condTy = infer ctx cond
        unify ctx key condTy ctx.Intrinsics.Bool
        let bodyTy = infer ctx body
        unify ctx key bodyTy ctx.Intrinsics.Unit
        ctx.Intrinsics.Unit

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
        unify ctx key startTy ctx.Intrinsics.Int
        let endTy = infer ctx endE
        unify ctx key endTy ctx.Intrinsics.Int
        let varKey = CstKeys.ofForToVar ident
        let varTv = freshTv ctx varKey
        varTv.Link <- ValueSome ctx.Intrinsics.Int
        let bodyTy = infer ctx body
        unify ctx key bodyTy ctx.Intrinsics.Unit
        ctx.Intrinsics.Unit

    /// The §4.4 duck-typed enumerator probe: C#'s pattern-based `foreach` accepts
    /// any source exposing a public parameterless `GetEnumerator()` whose return
    /// type `E` exposes `MoveNext(): bool` and a `Current` property — no
    /// `IEnumerable<'T>` required (`List<'T>` hands back its non-boxing
    /// `struct Enumerator` this way). Returns the element type (`Current`'s type)
    /// and the resolved `Pattern` descriptor (both axes `External`) so codegen can
    /// pick value-receiver emission. `srcArgs` are the source class's type arguments
    /// — the substitution for `GetEnumerator`'s (and thereby `E`'s) typars.
    and tryDuckTypedEnumerator
        (ctx: PassContext)
        (shape: ExternalClassShape)
        (srcArgs: SemType[])
        : (SemType * ForInEnumerator) voption =
        match
            shape.Members
            |> Array.tryFind (fun m -> m.Name = "GetEnumerator" && not m.IsStatic && not m.IsValueMember)
        with
        | None -> ValueNone
        | Some ge ->
            // `GetEnumerator` reads as `unit → E`; `E` carries the enumerator type's
            // own instantiation (`List`1+Enumerator` over the source's `'T`).
            match ExternalSymbols.openSignature ge srcArgs with
            | TyFun(_, (TyClass(enumKey, enumArgsEq) as enumTy)) ->
                match ExternalSymbols.tryLookupType ctx.Provider enumKey with
                | ValueSome(ExternalTypeShape.Class enumShape) ->
                    match probeExternalEnumerator ctx enumShape (enumArgsEq.AsSpan().ToArray()) with
                    | ValueSome probe ->
                        // External source, external `E`: both axes external.
                        ValueSome(
                            probe.ElemTy,
                            ForInEnumeratorG.Pattern(
                                enumTy,
                                ForInGetEnum.External ge.Key,
                                ForInEnumMembers.External(probe.MoveNext, probe.Current),
                                probe.IsValueType,
                                probe.Disposable
                            )
                        )
                    | ValueNone -> ValueNone
                | _ -> ValueNone
            | _ -> ValueNone

    /// Gap 2 pure-pattern variant: the project-local analogue of
    /// `tryDuckTypedEnumerator`. A user class exposing a parameterless
    /// `GetEnumerator()` whose return type `E` is *itself* a user class with
    /// `MoveNext(): bool` and a `Current` property is a valid `for … in` source
    /// even without implementing `IEnumerable<'T>` (C#'s non-boxing `foreach`,
    /// project-local). Handles a reference or value-type (`[<Struct>]`) user
    /// enumerator — the latter walks by address (`ldloca` + a by-address `call`),
    /// no boxing. When
    /// the enumerator type `E` is instead *external* (a BCL `List<'T>.Enumerator`),
    /// the Gap 3 hybrid kicks in: the local `GetEnumerator` is kept, but `E`'s
    /// `MoveNext` / `Current` / `Dispose` are probed off its `ExternalClassShape`
    /// and emitted via `ExternalMemberRefOn` — i.e. `Pattern` with a `Local`
    /// `ForInGetEnum` and an `External` `ForInEnumMembers`.
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
                    // enumerator walks by address (`ldloca` + a by-address `call`), the
                    // non-boxing path the value-receiver member-call IL already emits for
                    // local struct members. Local source, local `E`: both axes
                    // project-local.
                    | ValueSome enumInfo ->
                        probeLocalEnumerator ctx enumInfo enumArgs
                        |> ValueOption.map (fun (elemTy, isValueType, dispose) ->
                            elemTy,
                            ForInEnumeratorG.Pattern(
                                enumTy,
                                ForInGetEnum.Local,
                                ForInEnumMembers.Local,
                                isValueType,
                                dispose
                            )
                        )
                    // Gap 3: `E` is not project-local — try the *external* enumerator
                    // shape. `GetEnumerator` stays a local member; `MoveNext` /
                    // `Current` / `Dispose` are read off `E`'s `ExternalClassShape`
                    // (the §4.4 external-enumerator probe), and codegen mints them via
                    // `ExternalMemberRefOn`. Local source, external `E`: a local
                    // `GetEnumerator`, external enumerator members.
                    | ValueNone ->
                        match ExternalSymbols.tryLookupType ctx.Provider enumKey with
                        | ValueSome(ExternalTypeShape.Class enumShape) ->
                            probeExternalEnumerator ctx enumShape (enumArgs.AsSpan().ToArray())
                            |> ValueOption.map (fun probe ->
                                probe.ElemTy,
                                ForInEnumeratorG.Pattern(
                                    enumTy,
                                    ForInGetEnum.Local,
                                    ForInEnumMembers.External(probe.MoveNext, probe.Current),
                                    probe.IsValueType,
                                    probe.Disposable
                                )
                            )
                        | _ -> ValueNone
                | _ -> ValueNone
            | None -> ValueNone
        | ValueNone -> ValueNone

    and tryLocalInterfaceEnumerator
        (ctx: PassContext)
        (nameKey: SymbolKey)
        (args: EqArray<SemType>)
        : (SemType * ForInEnumerator) voption =
        match TypeRegistry.tryClassByKey ctx.Types nameKey with
        | ValueSome info ->
            let picked =
                info.InterfaceImpls
                |> Array.tryPick (fun impl ->
                    match impl.Resolved with
                    | ValueSome resolved ->
                        match zonk (instantiateMember (info.TypeParams, args) resolved) with
                        | TyClass(ifaceKey, ifaceArgs) when
                            RuntimeNames.matchesKey ctx.CapabilityIds.Enumerable ifaceKey
                            && ifaceArgs.Length = 1
                            ->
                            Some(ifaceArgs.[0], ForInEnumeratorG.Interface)
                        | _ -> None
                    | ValueNone -> None
                )

            match picked with
            | Some r -> ValueSome r
            | None -> ValueNone
        | ValueNone -> ValueNone

    /// Rung-3: resolve the enumerator `E` returned by a constrained `GetEnumerator`
    /// into the loop element type + the `ForInEnumMembers` axis + value-type-ness +
    /// disposability. `E` is either a *concrete* project-local enumerator (struct or
    /// class) exposing public pattern `MoveNext`/`Current` — the existing local walk —
    /// or *itself a typar* constrained to an enumerator interface, in which case its
    /// members dispatch via `constrained. <E> callvirt` too.
    and tryConstrainedEnumeratorMembers
        (ctx: PassContext)
        (enumTy: SemType)
        : (SemType * ForInEnumMembers * bool * bool) voption =
        match zonk enumTy with
        | TyClass(enumKey, enumArgs) ->
            match TypeRegistry.tryClassByKey ctx.Types enumKey with
            | ValueSome enumInfo ->
                probeLocalEnumerator ctx enumInfo enumArgs
                |> ValueOption.map (fun (elemTy, isValueType, dispose) ->
                    elemTy, ForInEnumMembersG.Local, isValueType, dispose
                )
            // An *external* enumerator returned by a constrained source is not yet
            // reachable in practice (a project-local seq interface hands back a
            // project-local `E`); defer rather than guess.
            | ValueNone -> ValueNone
        | TyVar etv -> tryConstrainedTyparEnumerator ctx etv
        | _ -> ValueNone

    /// `E` is itself a generic typar constrained to an enumerator interface
    /// (`'E :> IStructEnumerator`). Scan its `Coercion` constraints for an interface
    /// declaring `MoveNext(): bool` and a `Current` property; on a hit the members
    /// dispatch via `constrained. <E> callvirt`. The element type is `Current`'s type.
    and tryConstrainedTyparEnumerator
        (ctx: PassContext)
        (tv: TypeVar)
        : (SemType * ForInEnumMembers * bool * bool) voption =
        let rec scan (cs: SemanticConstraint list) =
            match cs with
            | [] -> ValueNone
            | c :: rest ->
                match c.Kind with
                | SemanticConstraintKind.Coercion target ->
                    match resolveStep target with
                    | TyClass(ifaceKey, ifaceArgs) ->
                        // Resolve by the interface's key, not a bare name: an
                        // arity-overloaded interface (`Fun`2`/`Fun`3`) has no bare alias.
                        match TypeRegistry.tryClassByKey ctx.Types ifaceKey with
                        | ValueSome info when info.IsInterface ->
                            match
                                tryClassChainMember ctx ifaceKey ifaceArgs "MoveNext",
                                tryClassChainMember ctx ifaceKey ifaceArgs "Current"
                            with
                            | ValueSome mnTy, ValueSome curTy ->
                                match zonk mnTy with
                                | TyFun(_, TyBool) ->
                                    // `Current` is a property — its type IS the element type.
                                    ValueSome(
                                        zonk curTy,
                                        ForInEnumMembersG.ConstrainedInterface(ifaceKey, ifaceArgs),
                                        false,
                                        false
                                    )
                                | _ -> scan rest
                            | _ -> scan rest
                        | _ -> scan rest
                    | _ -> scan rest
                | _ -> scan rest

        scan tv.Constraints

    /// Rung-3: resolve `for x in s` where the source `s` is a *generic typar*
    /// constrained to a project-local seq interface (`'S :> ISeq` / `'S :> IStructSeq<'E>`)
    /// declaring a `GetEnumerator(): E`. Scans the typar's `Coercion` constraints
    /// (Wall B's machinery) for such an interface, resolves the enumerator `E`'s walk
    /// members, and produces a `Pattern` descriptor whose `GetEnumerator` (and, when
    /// `E` is itself a typar, `MoveNext`/`Current`) dispatch via `constrained. callvirt`.
    and tryTyparSeqSource (ctx: PassContext) (tv: TypeVar) : (SemType * ForInEnumerator) voption =
        let rec scan (cs: SemanticConstraint list) =
            match cs with
            | [] -> ValueNone
            | c :: rest ->
                match c.Kind with
                | SemanticConstraintKind.Coercion target ->
                    match resolveStep target with
                    | TyClass(ifaceKey, ifaceArgs) ->
                        // Resolve by the interface's key, not a bare name: an
                        // arity-overloaded interface (`Fun`2`/`Fun`3`) has no bare alias.
                        match TypeRegistry.tryClassByKey ctx.Types ifaceKey with
                        | ValueSome info when info.IsInterface ->
                            match tryClassChainMember ctx ifaceKey ifaceArgs "GetEnumerator" with
                            | ValueSome mty ->
                                match zonk mty with
                                | TyFun(_, enumTy) ->
                                    match tryConstrainedEnumeratorMembers ctx enumTy with
                                    | ValueSome(elemTy, members, isValueType, dispose) ->
                                        ValueSome(
                                            elemTy,
                                            ForInEnumeratorG.Pattern(
                                                enumTy,
                                                ForInGetEnumG.ConstrainedInterface(ifaceKey, ifaceArgs),
                                                members,
                                                isValueType,
                                                dispose
                                            )
                                        )
                                    | ValueNone -> scan rest
                                | _ -> scan rest
                            | ValueNone -> scan rest
                        | _ -> scan rest
                    | _ -> scan rest
                | _ -> scan rest

        scan tv.Constraints

    /// `srcTy` is either `IEnumerable<'T>` itself, an external class that
    /// implements it (the directly-implemented interface set the metadata layer
    /// surfaces through `ExternalClassShape.Interfaces`), or
    /// a source exposing a pattern-based `GetEnumerator()`. Returns the
    /// `'T` so `inferForIn` can pin the loop pattern's type, plus the
    /// `ForInEnumerator` codegen reads off the frozen node.
    and tryForInEnumerator (ctx: PassContext) (srcTy: SemType) : (SemType * ForInEnumerator) voption =
        match zonk srcTy with
        | TyClass(nameKey, args) when RuntimeNames.matchesKey ctx.CapabilityIds.Enumerable nameKey && args.Length = 1 ->
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
                    match pickEnumerableElem ctx (ExternalSymbols.instantiateInterfaces shape argArr) with
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
        // A nominal UNION source (a bare cons-list `[1;2;3]` is `TyUnion(List, [elem])`).
        // The external union carries its directly-declared `interface <ty>` impls in
        // `ExternalTypeShape.Union.interfaces` (the `.fsi` union's `interface seq<'T>`);
        // match that against the enumerable capability exactly as the class arm does,
        // returning the boxing `Interface` enumerator (the union's `GetEnumerator` impl
        // is dispatched through `IEnumerable<'T>` — `List` implements it on both targets).
        | TyUnion(nameKey, args) ->
            match ExternalSymbols.tryLookupType ctx.Provider nameKey with
            | ValueSome(ExternalTypeShape.Union(_, _, interfaces, _)) ->
                let argArr = args.AsSpan().ToArray()

                match pickEnumerableElem ctx (ExternalSymbols.instantiateInterfacesOf interfaces argArr) with
                | Some elem -> ValueSome(elem, ForInEnumeratorG.Interface)
                | None -> ValueNone
            | _ -> ValueNone
        // Rung-3: a *generic typar* source (`'S :> ISeq`/`IStructSeq<'E>`) — resolve
        // its enumerable surface through the `Coercion` constraint, dispatching
        // `GetEnumerator` via `constrained. callvirt` (the zero-alloc struct path).
        | TyVar tv -> tryTyparSeqSource ctx tv
        | _ -> ValueNone

    and inferForIn
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (pat: Pat<SyntaxToken>)
        (src: Expr<SyntaxToken>)
        (body: Expr<SyntaxToken>)
        : SemType =
        // Int-range source: element type is int (`inferRange` already pinned the
        // endpoints to int; the range materialises no seq, so its own type is unused
        // here — only the loop pattern is pinned). Any other source must be an
        // `IEnumerable<'T>` (a BCL collection in v1, B-6) — the element type is
        // recovered from its interface set and the loop pattern unified with it.
        let srcTy = infer ctx src
        let patTy = inferPat ctx pat

        pinListLiteralToVesper ctx key srcTy

        let isRangeSource =
            match src with
            | Expr.Range _
            | Expr.SteppedRange _ -> true
            | Expr.EnclosedBlock(expr = Expr.Range _)
            | Expr.EnclosedBlock(expr = Expr.SteppedRange _) -> true
            | _ -> false

        if isRangeSource then
            unify ctx key patTy ctx.Intrinsics.Int
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
        unify ctx key bodyTy ctx.Intrinsics.Unit
        ctx.Intrinsics.Unit

    and inferRules
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (scrutineeTy: SemType)
        (resultTy: SemType)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : unit =
        // Closed anonymous-union scrutinee (`match (x: A | B) with …`): each arm's
        // binder narrows against the residual union — the members not yet caught by
        // an earlier unguarded arm — and an unguarded shortfall is a
        // non-exhaustiveness warning. The residual reads
        // nothing from body typing, so it is a pure pre-pass and the loop below
        // stays a flat fold over arms (see `computeArmNarrowing`).
        let armScruts, residual = computeArmNarrowing ctx scrutineeTy rules

        for r, armScrut in Seq.zip rules armScruts do
            match r with
            | Rule.Rule(pat = pat; guard = guard; expr = body) ->
                let patTy = inferPat ctx pat
                unify ctx key patTy armScrut

                match guard with
                | ValueSome(PatternGuard(expr = g)) ->
                    let gTy = infer ctx g
                    unify ctx key gTy ctx.Intrinsics.Bool
                | ValueNone -> ()

                let bodyTy = infer ctx body
                unify ctx key bodyTy resultTy
            | _ -> ()

        match residual with
        | _ :: _ ->
            let names = residual |> List.map describeUnionMember |> String.concat " | "

            ctx.Warn(key, sprintf "Incomplete pattern match on anonymous union: member(s) '%s' not handled" names)
        | [] -> ()

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
        let exnTy = TyConst(RuntimeNames.exnKey, EqArray.empty)
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
        unify ctx key finallyTy ctx.Intrinsics.Unit
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

        // `arr.[i] <- v` mints a `SetArray`/`SetIndex` `External` head in Freeze's
        // `translateAssignment` (the write mirror of the `GetArray`/`GetIndex` read
        // resolved by `inferIndexedLookup` above). Unlike the read intrinsics, the
        // WRITE intrinsic is never resolved by the plain type-check, so resolve it
        // here and stamp its identity under this `Assignment` node key — the same key
        // Freeze reads — so `InlineExpansion` splices the `stelem` / `$0[$1] = $2`
        // body by KEY. Receiver classification mirrors `translateAssignment` exactly
        // (an index-signature class → `SetIndex`, else `SetArray`): the receiver is
        // already grounded by the read resolution, so both passes agree.
        let rec unwrapLhs e =
            match e with
            | Expr.EnclosedBlock(expr = inner)
            | Expr.TypeAnnotation(expr = inner) -> unwrapLhs inner
            | _ -> e

        match unwrapLhs left with
        | Expr.IndexedLookup(expr = arrE) ->
            let arrTy = zonk (TyVar(tvOf ctx (CstKeys.ofExpr arrE)))

            let setName =
                match arrTy with
                | TyClass(clsKey, _) when not (ctx.Provider.TryLookupIndexSignature clsKey |> List.isEmpty) ->
                    "SetIndex"
                | _ -> "SetArray"

            match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Provider.TryLookup setName with
            | ValueSome sym -> ctx.Resolution.IntrinsicKey.Set(key, sym.Key)
            | ValueNone -> ()
        | _ -> ()

        ctx.Intrinsics.Unit
