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
open UnificationInferRecordAccess

module internal UnificationInferControlFlow =

    /// The walk a duck-typed enumerator `E` supports: its element type, how codegen reaches
    /// `MoveNext` / `Current`, and whether `E` is a struct and disposable.
    type EnumProbe =
        {
            ElemTy: SemType
            Members: ForInEnumMembers
            IsValueType: bool
            Disposable: bool
        }

    /// Probe an external enumerator shape `E` for the duck-typed `for … in` members: a
    /// parameterless `MoveNext(): bool` and a `Current` property, with `enumArgs` being
    /// `E`'s own instantiation. `ValueNone` unless both are present and `MoveNext : bool`.
    let private probeExternalEnumerator
        (ctx: PassContext)
        (enumShape: ExternalClassShape)
        (enumArgs: SemType[])
        : EnumProbe voption =
        let moveNext =
            enumShape.Members
            |> EqArray.tryFind (fun m -> m.Name = "MoveNext" && not m.IsStatic && not m.IsValueMember)

        let current =
            enumShape.Members
            |> EqArray.tryFind (fun m -> m.Name = "Current" && not m.IsStatic && m.IsValueMember)

        match moveNext, current with
        | ValueSome mn, ValueSome cur ->
            match ExternalSymbols.openSignature mn enumArgs with
            | TyFun(_, TyBool) ->
                // The `finally` exists only when `E : IDisposable`, and disposal always
                // goes through the `System.IDisposable::Dispose` interface slot, so a
                // bool is enough here, no member key.
                let disposable =
                    ExternalSymbols.instantiateInterfaces enumShape enumArgs
                    |> RuntimeNames.carriesCapability ctx.CapabilityIds.Disposable

                ValueSome
                    {
                        ElemTy = ExternalSymbols.openSignature cur enumArgs
                        Members = ForInEnumMembers.External(SymbolKey.Member mn.Key, SymbolKey.Member cur.Key)
                        IsValueType = enumShape.Flags.IsValueType
                        Disposable = disposable
                    }
            | _ -> ValueNone
        | _ -> ValueNone

    /// From a realised interface set, the element type of the enumerable capability: the
    /// single type-arg of the first `seq<'T>` / `IEnumerable<'T>`.
    let private pickEnumerableElem (ctx: PassContext) (interfaces: SemType[]) : SemType option =
        ctx.CapabilityIds.Enumerable
        |> ValueOption.bind (fun cap -> RuntimeNames.tryCapabilityArgs cap interfaces)
        |> function
            | ValueSome ta when ta.Length = 1 -> Some ta.[0]
            | _ -> None

    /// Eagerly pin a flexible bare list-literal source (`for x in [1;2;3]`) to the Vesper
    /// cons-list: a for-in needs its source pinned NOW to read the enumerable surface, and
    /// that is the only list emitted. A non-literal source is left untouched.
    let private pinListLiteralToVesper (ctx: PassContext) (tok: SyntaxToken) (srcTy: SemType) : unit =
        match zonk ctx.Store srcTy with
        | TyVar tv ->
            match tryListLiteralElem ctx (UnionFind.find ctx.Store tv).Id with
            | ValueSome elemTy -> unify ctx tok srcTy (RuntimeNames.consListTy elemTy)
            | ValueNone -> ()
        | _ -> ()

    /// Probe a *user* class `E` for the duck-typed `for … in` members: a parameterless
    /// `MoveNext(): bool` and a `Current` property. `enumArgs` is `E`'s own instantiation.
    let private probeLocalEnumerator
        (ctx: PassContext)
        (enumInfo: ClassTypeInfo)
        (enumArgs: EqArray<SemType>)
        : EnumProbe voption =
        let inst (t: SemType) =
            zonk ctx.Store (instantiateMember ctx.Store (enumInfo.TypeParams, enumArgs) t)

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
                // A ref-struct `E` with a pattern `Dispose()` but no `IDisposable` is not
                // disposed at all: the descriptor carries a bool, not which member to call.
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
                ValueSome
                    {
                        ElemTy = instantiateMember ctx.Store (enumInfo.TypeParams, enumArgs) cur.Type
                        Members = ForInEnumMembers.Local
                        IsValueType = enumInfo.IsValueType
                        Disposable = disposable
                    }
            | _ -> ValueNone
        | _ -> ValueNone

    /// A `'T :> IFace<…>` bound whose target is a *project-local* interface. Resolved by key,
    /// not bare name: `Fun` at arity 2 and at arity 3 are different interfaces sharing one name.
    [<return: Struct>]
    let private (|CoercedToLocalInterface|_|)
        (ctx: PassContext)
        (c: SemanticConstraint)
        : (TypeKey * EqArray<SemType>) voption =
        match c.Kind with
        | SemanticConstraintKind.Coercion target ->
            match resolveStep ctx.Store target with
            | TyClass(ifaceKey, ifaceArgs) ->
                match TypeRegistry.tryClassByKey ctx.Types ifaceKey with
                | ValueSome info when info.IsInterface -> ValueSome(ifaceKey, ifaceArgs)
                | _ -> ValueNone
            | _ -> ValueNone
        | _ -> ValueNone

    /// The first local-interface bound on `tv` that `pick` accepts. A typar may carry several
    /// bounds, so a `ValueNone` from `pick` is a miss on that bound, not on the typar.
    let private tryPickCoercedInterface
        (ctx: PassContext)
        (tv: TyVarId)
        (pick: TypeKey -> EqArray<SemType> -> 'a voption)
        : 'a voption =
        let rec scan (cs: SemanticConstraint list) =
            match cs with
            | [] -> ValueNone
            | c :: rest ->
                match c with
                | CoercedToLocalInterface ctx (ifaceKey, ifaceArgs) ->
                    match pick ifaceKey ifaceArgs with
                    | ValueSome _ as hit -> hit
                    | ValueNone -> scan rest
                | _ -> scan rest

        scan (ctx.Store.Constraints.Items(UnionFind.find ctx.Store tv))

    /// A short display name for an anonymous-union member in an incomplete-match
    /// diagnostic. Members are ground annotation types (`int`, `string`, `null`), so the
    /// bare `TyConst` name reads well; anything compound falls back to `%A`.
    let rec private describeDisjunct (store: TypeStore) (d: SemType) : string =
        match resolveStep store d with
        | TyConst(key, args) when args.IsEmpty ->
            let (DisplayName shown) = SymbolKeyOps.typeSimpleName key
            shown
        | TyOr inner ->
            inner.Disjuncts
            |> EqSet.toList
            |> List.map (describeDisjunct store)
            |> String.concat " | "
        | other -> sprintf "%A" other

    /// Per-arm scrutinee narrowing for a closed anonymous-union match (`match (x: A | B)
    /// with …`): the disjuncts not yet caught by an earlier *unguarded* arm, paired 1:1 with
    /// `rules`, plus the final uncovered residual, reported as an incomplete match if non-empty.
    let private computeArmNarrowing
        (ctx: PassContext)
        (scrutineeTy: SemType)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : SemType list * SemType list =
        match zonk ctx.Store scrutineeTy with
        | TyOr disjuncts ->
            // The disjuncts a `:? T` / `:? T as _` arm tests for, recursing through
            // `|` alternatives. Anything else tests for none.
            let rec patTests pat =
                match pat with
                | Pat.TypeTest(typ = t)
                | Pat.TypeTestAs(typ = t) -> [ translateType ctx t ]
                | Pat.Or(left = l; right = r) -> patTests l @ patTests r
                | Pat.EnclosedBlock(pat = p)
                | Pat.Attributed(pat = p) -> patTests p
                | _ -> []

            // A bound variable / wildcard with no type test catches the whole residual.
            let rec isCatchAll pat =
                match pat with
                | Pat.Wildcard _
                | Pat.NamedSimple _ -> true
                | Pat.As(pat = p)
                | Pat.EnclosedBlock(pat = p)
                | Pat.Attributed(pat = p) -> isCatchAll p
                | _ -> false

            let mutable residual = EqSet.toList disjuncts.Disjuncts
            let armScruts = ResizeArray(rules.Length)

            for r in rules do
                // The bound variable narrows against the disjuncts still live *before* this arm. Once
                // the residual is exhausted, fall back to the full scrutinee rather than
                // pin a redundant trailing bound variable to an empty union.
                armScruts.Add(
                    if List.isEmpty residual then
                        scrutineeTy
                    else
                        mkUnion residual
                )

                // Shrink the residual by the disjuncts this arm definitively catches.
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
                                |> List.filter (fun d ->
                                    tests |> List.forall (fun tst -> subsumes ctx d tst = SubsumeOutcome.Unrelated)
                                )
                | _ -> ()

            List.ofSeq armScruts, residual
        | _ -> [ for _ in rules -> scrutineeTy ], []

    let rec inferIfThenElse
        (infer: Infer)
        (ctx: PassContext)
        (tok: SyntaxToken)
        (cond: Expr<SyntaxToken>)
        (thenE: Expr<SyntaxToken>)
        (elifs: ImmutableArray<ElifBranch<SyntaxToken>>)
        (elseB: ElseBranch<SyntaxToken> voption)
        : SemType =
        let condTy = infer ctx cond
        unify ctx tok condTy ctx.Intrinsics.Bool

        let thenTy = infer ctx thenE

        for elif_ in elifs do
            let elifCond, elifExpr =
                match elif_ with
                | ElifBranch.Elif(condition = c; expr = e)
                | ElifBranch.ElseIf(condition = c; expr = e) -> c, e

            let elifCondTy = infer ctx elifCond
            unify ctx tok elifCondTy ctx.Intrinsics.Bool
            let elifTy = infer ctx elifExpr
            unify ctx tok thenTy elifTy

        match elseB with
        | ValueSome(ElseBranch(expr = elseExpr)) ->
            let elseTy = infer ctx elseExpr
            unify ctx tok thenTy elseTy
            thenTy
        | ValueNone ->
            // `if c then e` with no else: F# reads the missing branch as `else ()`, so the
            // then-branch must be `unit`. The elifs were already unified with `thenTy`, so
            // this one `unify` forces them all.
            unify ctx tok thenTy ctx.Intrinsics.Unit
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
        (tok: SyntaxToken)
        (items: ImmutableArray<Expr<SyntaxToken>>)
        : SemType =
        if items.Length = 0 then
            ctx.Intrinsics.Unit
        else
            for i = 0 to items.Length - 2 do
                let ty = infer ctx items.[i]
                unify ctx tok ty ctx.Intrinsics.Unit

            infer ctx items.[items.Length - 1]

    and inferWhile
        (infer: Infer)
        (ctx: PassContext)
        (tok: SyntaxToken)
        (cond: Expr<SyntaxToken>)
        (body: Expr<SyntaxToken>)
        : SemType =
        let condTy = infer ctx cond
        unify ctx tok condTy ctx.Intrinsics.Bool
        let bodyTy = infer ctx body
        unify ctx tok bodyTy ctx.Intrinsics.Unit
        ctx.Intrinsics.Unit

    and inferForTo
        (infer: Infer)
        (ctx: PassContext)
        (tok: SyntaxToken)
        (ident: SyntaxToken)
        (startE: Expr<SyntaxToken>)
        (endE: Expr<SyntaxToken>)
        (body: Expr<SyntaxToken>)
        : SemType =
        let startTy = infer ctx startE
        unify ctx tok startTy ctx.Intrinsics.Int
        let endTy = infer ctx endE
        unify ctx tok endTy ctx.Intrinsics.Int
        let varKey = CstKeys.ofForToVar ident
        let varTv = freshTv ctx varKey
        ctx.Store.SetLink(UnionFind.find ctx.Store varTv, ValueSome ctx.Intrinsics.Int)
        let bodyTy = infer ctx body
        unify ctx tok bodyTy ctx.Intrinsics.Unit
        ctx.Intrinsics.Unit

    /// The duck-typed enumerator probe: C#'s pattern-based `foreach` accepts a source with a
    /// public parameterless `GetEnumerator()` whose return `E` exposes `MoveNext(): bool` and
    /// `Current`, with no `IEnumerable<'T>`. `srcArgs` substitutes the source class's typars.
    and tryDuckTypedEnumerator
        (ctx: PassContext)
        (shape: ExternalClassShape)
        (srcArgs: SemType[])
        : (SemType * ForInEnumerator) voption =
        match
            shape.Members
            |> EqArray.tryFind (fun m -> m.Name = "GetEnumerator" && not m.IsStatic && not m.IsValueMember)
        with
        | ValueNone -> ValueNone
        | ValueSome ge ->
            // `GetEnumerator` reads as `unit → E`; `E` carries the enumerator type's own
            // instantiation (`List<'T>.Enumerator` over the source's `'T`).
            match ExternalSymbols.openSignature ge srcArgs with
            | TyFun(_, (TyClass(enumKey, enumArgsEq) as enumTy)) ->
                match ctx.Provider.TryLookupType enumKey with
                | ValueSome(ExternalTypeShape.Class enumShape) ->
                    match probeExternalEnumerator ctx enumShape (enumArgsEq.AsSpan().ToArray()) with
                    | ValueSome probe ->
                        // External source, external `E`: both axes external.
                        ValueSome(
                            probe.ElemTy,
                            ForInEnumeratorG.Pattern(
                                enumTy,
                                ForInGetEnum.External(SymbolKey.Member ge.Key),
                                probe.Members,
                                probe.IsValueType,
                                probe.Disposable
                            )
                        )
                    | ValueNone -> ValueNone
                | _ -> ValueNone
            | _ -> ValueNone

    /// A *user* class exposing a parameterless `GetEnumerator()` is a valid `for … in`
    /// source without implementing `IEnumerable<'T>`. Its enumerator `E` may be another
    /// user class, or external (a BCL `List<'T>.Enumerator`), making the pair a hybrid of both axes.
    and tryLocalDuckTypedEnumerator
        (ctx: PassContext)
        (nameKey: TypeKey)
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
                match zonk ctx.Store (instantiateMember ctx.Store (info.TypeParams, args) ge.Type) with
                | TyFun(_, (TyClass(enumKey, enumArgs) as enumTy)) ->
                    match TypeRegistry.tryClassByKey ctx.Types enumKey with
                    // A user enumerator, reference or value type: a `[<Struct>]` one walks
                    // by address (`ldloca` + a by-address `call`), no boxing.
                    | ValueSome enumInfo ->
                        probeLocalEnumerator ctx enumInfo enumArgs
                        |> ValueOption.map (fun probe ->
                            probe.ElemTy,
                            ForInEnumeratorG.Pattern(
                                enumTy,
                                ForInGetEnum.Local,
                                probe.Members,
                                probe.IsValueType,
                                probe.Disposable
                            )
                        )
                    // `E` is not project-local: keep the local `GetEnumerator`, but read
                    // `MoveNext` / `Current` / `Dispose` off `E`'s external shape.
                    | ValueNone ->
                        match ctx.Provider.TryLookupType enumKey with
                        | ValueSome(ExternalTypeShape.Class enumShape) ->
                            probeExternalEnumerator ctx enumShape (enumArgs.AsSpan().ToArray())
                            |> ValueOption.map (fun probe ->
                                probe.ElemTy,
                                ForInEnumeratorG.Pattern(
                                    enumTy,
                                    ForInGetEnum.Local,
                                    probe.Members,
                                    probe.IsValueType,
                                    probe.Disposable
                                )
                            )
                        | _ -> ValueNone
                | _ -> ValueNone
            | None -> ValueNone
        | ValueNone -> ValueNone

    /// The enumerable surface a *project-local* nominal source publishes through its
    /// `interface` impls, over `IInterfaceImplHost` so that a class, a union and a record
    /// source share one resolver.
    and tryLocalInterfaceEnumeratorOn
        (ctx: PassContext)
        (host: IInterfaceImplHost)
        (args: EqArray<SemType>)
        : (SemType * ForInEnumerator) voption =
        let picked =
            host.InterfaceImpls
            |> Array.tryPick (fun impl ->
                match impl.Resolved with
                | ValueSome resolved ->
                    match zonk ctx.Store (instantiateMember ctx.Store (host.TypeParams, args) resolved) with
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

    and tryLocalInterfaceEnumerator
        (ctx: PassContext)
        (nameKey: TypeKey)
        (args: EqArray<SemType>)
        : (SemType * ForInEnumerator) voption =
        match TypeRegistry.tryClassByKey ctx.Types nameKey with
        | ValueSome info -> tryLocalInterfaceEnumeratorOn ctx info args
        | ValueNone -> ValueNone

    /// Resolve the enumerator `E` returned by a constrained `GetEnumerator`. `E` is either a
    /// concrete project-local enumerator with public `MoveNext`/`Current`, or itself a
    /// constrained typar.
    and tryConstrainedEnumeratorMembers (ctx: PassContext) (enumTy: SemType) : EnumProbe voption =
        match zonk ctx.Store enumTy with
        | TyClass(enumKey, enumArgs) ->
            match TypeRegistry.tryClassByKey ctx.Types enumKey with
            | ValueSome enumInfo -> probeLocalEnumerator ctx enumInfo enumArgs
            | ValueNone -> ValueNone
        | TyVar etv -> tryConstrainedTyparEnumerator ctx etv
        | _ -> ValueNone

    /// `E` is itself a generic typar constrained to an enumerator interface
    /// (`'E :> IStructEnumerator<'T>`). Scan its `Coercion` constraints for an interface with
    /// `MoveNext(): bool` and `Current`; those members dispatch via `constrained. callvirt`.
    and tryConstrainedTyparEnumerator (ctx: PassContext) (tv: TyVarId) : EnumProbe voption =
        tryPickCoercedInterface
            ctx
            tv
            (fun ifaceKey ifaceArgs ->
                match
                    tryClassChainMember ctx ifaceKey ifaceArgs "MoveNext",
                    tryClassChainMember ctx ifaceKey ifaceArgs "Current"
                with
                | ValueSome mnTy, ValueSome curTy ->
                    match zonk ctx.Store mnTy with
                    | TyFun(_, TyBool) ->
                        ValueSome
                            {
                                // `Current` is a property, so its type IS the element type.
                                ElemTy = zonk ctx.Store curTy
                                Members = ForInEnumMembers.ConstrainedInterface(ifaceKey, ifaceArgs)
                                // `constrained.` already addresses a struct `E`, and no
                                // constraint here is probed for `IDisposable`.
                                IsValueType = false
                                Disposable = false
                            }
                    | _ -> ValueNone
                | _ -> ValueNone
            )

    /// `for x in s` where the source `s` is a *generic typar* constrained to a project-local
    /// seq interface (`'S :> IStructSeq<'T, 'E>`) declaring `GetEnumerator(): E`. Resolves
    /// `E`'s walk members; the calls then dispatch via `constrained. callvirt`.
    and tryTyparSeqSource (ctx: PassContext) (tv: TyVarId) : (SemType * ForInEnumerator) voption =
        tryPickCoercedInterface
            ctx
            tv
            (fun ifaceKey ifaceArgs ->
                match tryClassChainMember ctx ifaceKey ifaceArgs "GetEnumerator" with
                | ValueSome mty ->
                    match zonk ctx.Store mty with
                    | TyFun(_, enumTy) ->
                        match tryConstrainedEnumeratorMembers ctx enumTy with
                        | ValueSome probe ->
                            ValueSome(
                                probe.ElemTy,
                                ForInEnumeratorG.Pattern(
                                    enumTy,
                                    ForInGetEnumG.ConstrainedInterface(ifaceKey, ifaceArgs),
                                    probe.Members,
                                    probe.IsValueType,
                                    probe.Disposable
                                )
                            )
                        | ValueNone -> ValueNone
                    | _ -> ValueNone
                | ValueNone -> ValueNone
            )

    /// `srcTy` is `IEnumerable<'T>` itself, a type implementing it, or a source exposing a
    /// pattern-based `GetEnumerator()`. Returns the `'T` the loop pattern is pinned to,
    /// plus the `ForInEnumerator` codegen reads off the frozen node.
    and tryForInEnumerator (ctx: PassContext) (srcTy: SemType) : (SemType * ForInEnumerator) voption =
        match zonk ctx.Store srcTy with
        | TyClass(nameKey, args) when RuntimeNames.matchesKey ctx.CapabilityIds.Enumerable nameKey && args.Length = 1 ->
            ValueSome(args.[0], ForInEnumeratorG.Interface)
        | TyClass(nameKey, args) ->
            match ctx.Provider.TryLookupType nameKey with
            | ValueSome(ExternalTypeShape.Class shape) ->
                let argArr = args.AsSpan().ToArray()

                // C# precedence: a pattern-based `GetEnumerator()` wins over the
                // `IEnumerable<'T>` interface, so `List<'T>` walks its non-boxing struct
                // `Enumerator` rather than the boxing interface enumerator.
                match tryDuckTypedEnumerator ctx shape argArr with
                | ValueSome r -> ValueSome r
                | ValueNone ->
                    match pickEnumerableElem ctx (ExternalSymbols.instantiateInterfaces shape argArr) with
                    | Some elem -> ValueSome(elem, ForInEnumeratorG.Interface)
                    | None -> ValueNone
            // A project-local source is invisible to the external provider; fall back to
            // the user probes, with the same pattern-over-interface precedence.
            | _ ->
                match tryLocalDuckTypedEnumerator ctx nameKey args with
                | ValueSome r -> ValueSome r
                | ValueNone -> tryLocalInterfaceEnumerator ctx nameKey args
        // A nominal UNION source (a bare cons-list `[1;2;3]` is `TyUnion(List, [elem])`).
        // Its declared `interface seq<'T>` impl is matched against the enumerable
        // capability as in the class arm, giving the boxing `Interface` enumerator.
        | TyUnion(nameKey, args) ->
            match ctx.Provider.TryLookupType nameKey with
            | ValueSome(ExternalTypeShape.Union(_, _, interfaces, _, _)) ->
                let argArr = args.AsSpan().ToArray()

                match pickEnumerableElem ctx (ExternalSymbols.instantiateInterfacesOf interfaces argArr) with
                | Some elem -> ValueSome(elem, ForInEnumeratorG.Interface)
                | None -> ValueNone
            | _ -> ValueNone
        // A PRIMITIVE source (`for x in arr`): an `extern` type declares its interfaces on
        // the intrinsic's class surface, matched against the capability as in the union arm.
        | TyConst(key, args) ->
            match ctx.Provider.TryLookupType key with
            | ValueSome(ExternalTypeShape.Intrinsic { Class = ValueSome surface }) ->
                let argArr = args.AsSpan().ToArray()

                match pickEnumerableElem ctx (ExternalSymbols.instantiateInterfacesOf surface.Interfaces argArr) with
                | Some elem -> ValueSome(elem, ForInEnumeratorG.Interface)
                | None -> ValueNone
            | _ -> ValueNone
        // A project-local RECORD source implementing the iteration capability
        // (`interface seq<'T>`) takes the boxing `Interface` walk, exactly as for a class.
        | TyRecord(nameKey, args) ->
            match TypeRegistry.tryRecordByKey ctx.Types nameKey with
            | ValueSome info -> tryLocalInterfaceEnumeratorOn ctx info args
            | ValueNone -> ValueNone
        // A *generic typar* source resolves its enumerable surface through the `Coercion`
        // constraint, dispatching `GetEnumerator` via `constrained. callvirt`.
        | TyVar tv -> tryTyparSeqSource ctx tv
        | _ -> ValueNone

    and inferForIn
        (infer: Infer)
        (ctx: PassContext)
        (node: NodeSite)
        (pat: Pat<SyntaxToken>)
        (src: Expr<SyntaxToken>)
        (body: Expr<SyntaxToken>)
        : SemType =
        // An int-range source materialises no seq, so its own type goes unused and only
        // the loop pattern is pinned, to `int`. Any other source must publish an
        // enumerable surface, and the pattern is unified with its element type.
        let srcTy = infer ctx src
        let patTy = inferPat ctx pat

        pinListLiteralToVesper ctx node.Tok srcTy

        let isRangeSource =
            match src with
            | Expr.Range _
            | Expr.SteppedRange _ -> true
            | Expr.EnclosedBlock(expr = Expr.Range _)
            | Expr.EnclosedBlock(expr = Expr.SteppedRange _) -> true
            | _ -> false

        if isRangeSource then
            unify ctx node.Tok patTy ctx.Intrinsics.Int
        else
            match tryForInEnumerator ctx srcTy with
            | ValueSome(elemTy, shape) ->
                unify ctx node.Tok patTy elemTy
                ctx.Resolution.ForInShape.Set(node.Key, shape)
            | ValueNone ->
                ctx.Report(
                    node.Tok,
                    Kind.Message
                        "for-in: source is not a supported enumerable (expected IEnumerable<'T> or a pattern-based GetEnumerator())"
                )

        let bodyTy = infer ctx body
        unify ctx node.Tok bodyTy ctx.Intrinsics.Unit
        ctx.Intrinsics.Unit

    and inferRules
        (infer: Infer)
        (ctx: PassContext)
        (tok: SyntaxToken)
        (scrutineeTy: SemType)
        (resultTy: SemType)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : unit =
        let armScruts, residual = computeArmNarrowing ctx scrutineeTy rules

        for r, armScrut in Seq.zip rules armScruts do
            match r with
            | Rule.Rule(pat = pat; guard = guard; expr = body) ->
                let patTy = inferPat ctx pat
                unify ctx tok patTy armScrut

                match guard with
                | ValueSome(PatternGuard(expr = g)) ->
                    let gTy = infer ctx g
                    unify ctx tok gTy ctx.Intrinsics.Bool
                | ValueNone -> ()

                let bodyTy = infer ctx body
                unify ctx tok bodyTy resultTy
            | _ -> ()

        match residual with
        | _ :: _ -> ctx.Report(tok, Kind.IncompleteAnonUnionMatch(residual |> List.map (describeDisjunct ctx.Store)))
        | [] -> ()

    and inferMatch
        (infer: Infer)
        (ctx: PassContext)
        (tok: SyntaxToken)
        (scrutinee: Expr<SyntaxToken>)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : SemType =
        let scrutineeTy = infer ctx scrutinee
        let resultTy = TyVar(freshTyVar ctx)
        inferRules infer ctx tok scrutineeTy resultTy rules
        resultTy

    and inferFunction
        (infer: Infer)
        (ctx: PassContext)
        (tok: SyntaxToken)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : SemType =
        // `function …` ~ `fun x -> match x with …`: the synthesised parameter's TypeVar
        // IS the scrutinee's, so every arm's pattern unifies with it.
        let paramTy = TyVar(freshTyVar ctx)
        let resultTy = TyVar(freshTyVar ctx)
        inferRules infer ctx tok paramTy resultTy rules
        TyFun(paramTy, resultTy)

    and inferTryWith
        (infer: Infer)
        (ctx: PassContext)
        (tok: SyntaxToken)
        (body: Expr<SyntaxToken>)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : SemType =
        // Pin the scrutinee to the primitive `exn` placeholder: a fresh TyVar would let a
        // wildcard or variable arm pattern carry an unresolved TyVar into the TAST.
        let resultTy = infer ctx body
        let exnTy = TyConst(RuntimeNames.exnKey, EqArray.empty)
        inferRules infer ctx tok exnTy resultTy rules
        resultTy

    and inferTryFinally
        (infer: Infer)
        (ctx: PassContext)
        (tok: SyntaxToken)
        (body: Expr<SyntaxToken>)
        (finallyE: Expr<SyntaxToken>)
        : SemType =
        let resultTy = infer ctx body
        let finallyTy = infer ctx finallyE
        unify ctx tok finallyTy ctx.Intrinsics.Unit
        resultTy
