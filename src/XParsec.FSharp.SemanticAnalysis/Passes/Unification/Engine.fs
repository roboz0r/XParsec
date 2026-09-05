namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationSubsume
open UnificationConstraintCheck

/// The MUTATING unifier: `unify`, its on-link discharges (deferred dot-accesses,
/// typar constraints, SRTP traits), and the argument / annotation coercion walkers.
module UnificationEngine =

    /// The flat `FunN` arity a parameter slot constrains its argument to, or `ValueNone`
    /// for a non-`Fun`-constrained parameter: `'TF :> Fun<a,b>` is arity 1, up through
    /// `Fun<a,b,c,d,e>` arity 4.
    let funSlotArityOf (store: TypeStore) (param: SemType) : int voption =
        match resolveStep store param with
        | TyVar tv ->
            let root = UnionFind.find store tv

            store.Constraints.Items root
            |> List.tryPick (fun c ->
                match c.Kind with
                | SemanticConstraintKind.Coercion target ->
                    match resolveStep store target with
                    | TyClass(tk, targs) -> funSlotArityOfArgs tk targs.Length
                    | _ -> None
                | _ -> None
            )
            |> function
                | Some n -> ValueSome n
                | None -> ValueNone
        | _ -> ValueNone

    let private fieldLookup (fields: RecordFieldInfo[]) (name: string) : SemType voption =
        match fields |> Array.tryFind (fun f -> f.Name = name) with
        | Some f -> ValueSome f.Type
        | None -> ValueNone

    let private memberLookup (members: TypeMemberInfo[]) (name: string) : SemType voption =
        match members |> Array.tryFind (fun m -> m.Name = name && not m.IsStatic) with
        | Some m -> ValueSome m.Type
        | None -> ValueNone

    /// The outcome of resolving a TyVar's link target to a dot-access source.
    /// `NotNominal` — not a record/class/union, nothing to discharge; `UnknownType` —
    /// named a nominal type the registry doesn't know.
    [<RequireQualifiedAccess>]
    type private DotSource =
        | NotNominal
        | UnknownType of name: string * kind: NominalKind
        | Resolved of
            name: string *
            memberNoun: MemberNoun *
            subst: Dictionary<TyVarId, SemType> *
            lookup: (string -> SemType voption)
        /// A project-local class: member lookup walks the inheritance chain, threading the
        /// substitution up per parent, so no single `subst` + `lookup` pair describes it.
        | ClassChain of key: TypeKey * args: EqArray<SemType>
        /// An *external* class/interface (`System.Collections.IEqualityComparer`): its
        /// member resolves through the provider, addressed by the resolved `key`.
        | ExternalClass of key: TypeKey * args: EqArray<SemType>

    let private resolveDotSource (ctx: PassContext) (linkTarget: SemType) : DotSource =
        match tryResolveNominal ctx.Store linkTarget with
        | ValueNone -> DotSource.NotNominal
        | ValueSome(NominalKind.Record, key, args) ->
            let (DisplayName name) = SymbolKeyOps.typeSimpleName key

            match TypeRegistry.tryRecordByKey ctx.Types key with
            | ValueSome info ->
                DotSource.Resolved(
                    name,
                    MemberNoun.Field,
                    mkNamedTypeSubst ctx.Store info.TypeParams args,
                    fieldLookup info.Fields
                )
            | ValueNone -> DotSource.UnknownType(name, NominalKind.Record)
        | ValueSome(NominalKind.Class, key, args) ->
            if TypeRegistry.containsClassKey ctx.Types key then
                DotSource.ClassChain(key, args)
            else
                DotSource.ExternalClass(key, args)
        | ValueSome(NominalKind.Union, key, args) ->
            let (DisplayName name) = SymbolKeyOps.typeSimpleName key

            match TypeRegistry.tryUnionByKey ctx.Types key with
            | ValueSome info ->
                DotSource.Resolved(
                    name,
                    MemberNoun.InstanceMember,
                    mkNamedTypeSubst ctx.Store info.TypeParams args,
                    memberLookup info.Members
                )
            | ValueNone -> DotSource.UnknownType(name, NominalKind.Union)

    let isObjType (t: SemType) : bool =
        match t with
        | TyObj -> true
        | _ -> false

    /// `true` when `expected` is the universal `obj` supertype (after one resolve
    /// step): an argument coercion must ACCEPT the actual *without* unifying, because the
    /// implicit boxing upcast F# inserts must never ground the actual's typar.
    let absorbsAsObj (store: TypeStore) (expected: SemType) : bool = isObjType (resolveStep store expected)

    /// Matches a carried type-level node (`keyof`/`T[K]`/conditional) that ground-folds
    /// to a CONCRETE (non-carrier) type, binding the folded result. A non-carrier or
    /// a still-inert node does not match.
    let private (|FoldedCarrier|_|) (ctx: PassContext) (t: SemType) : SemType option =
        match tryFoldCarried ctx t with
        | ValueSome folded -> Some folded
        | ValueNone -> None

    /// A foreign parameter whose type IS a platform type id shared by several primitives
    /// (JS `number` <- int/float/float32) admits any member of that family: returns the
    /// family as a `TyOr`, else `ValueNone` for the ordinary single-canon case.
    let private numericFamilyOr (ctx: PassContext) (ty: SemType) : SemType voption =
        match resolveStep ctx.Store ty with
        // The axis is keyed by platform type id, so only a type whose identity IS a platform
        // name can hit it: a Vesper-qualified `number` is refused.
        | TyConst(PlatformName platform, args) when args.Length = 0 ->
            match IntrinsicTypeMap.canonsOf platform ctx.IntrinsicTypeMap.Value with
            | canons when canons.Length > 1 ->
                ValueSome(SemType.MkUnion(seq { for c in canons -> TyConst(c, EqArray.empty) }))
            | _ -> ValueNone
        | _ -> ValueNone

    /// Two intrinsic canons are REPR-SIBLINGS iff some platform type id covers BOTH: on JS
    /// the numeric family (`"number"` -> int/float/float32); on CLR, never.
    let private reprSiblings (ctx: PassContext) (a: SemType) (b: SemType) : bool =
        match resolveStep ctx.Store a, resolveStep ctx.Store b with
        | TyConst(k1, a1), TyConst(k2, a2) when a1.Length = 0 && a2.Length = 0 ->
            IntrinsicTypeMap.familyOf k1 ctx.IntrinsicTypeMap.Value |> EqArray.contains k2
        | _ -> false

    /// A Vesper RECORD satisfies an EXTERNAL interface parameter by WIDTH: every required
    /// (non-optional) value member must be supplied by a same-named record field whose
    /// type coerces in. Pure read (no `Link`), so it is safe as a guard.
    let private tryStructuralWiden (ctx: PassContext) (actual: SemType) (expected: SemType) : bool =
        match resolveStep ctx.Store expected with
        | TyClass(ikey, iargs) ->
            match ctx.Provider.TryLookupType ikey with
            | ValueSome(ExternalSymbols.ExternalInterfaceMembers ifaceMembers) ->
                match resolveStep ctx.Store actual with
                | TyRecord(rkey, rargs) ->
                    match TypeRegistry.tryRecordByKey ctx.Types rkey with
                    | ValueSome info ->
                        let subst = mkNamedTypeSubst ctx.Store info.TypeParams rargs

                        let fieldTy (name: string) : SemType voption =
                            match info.Fields |> Array.tryFind (fun f -> f.Name = name) with
                            | Some f -> ValueSome(substituteWith ctx.Store subst f.Type)
                            | None -> ValueNone

                        let declArgs = EqArray.toArray iargs

                        ifaceMembers
                        |> EqArray.filter (fun m -> not m.IsStatic && m.IsValueMember && not m.IsOptional)
                        |> EqArray.forall (fun m ->
                            match fieldTy m.Name with
                            | ValueNone -> false
                            | ValueSome argTy ->
                                // A `number` member resolves as `float`, so an `int` record
                                // field satisfies it as a repr-sibling, not by `subsumes`.
                                let expectedTy =
                                    ExternalSymbols.instantiateSignature ctx m declArgs ctx.CurrentLevel

                                subsumes ctx argTy expectedTy <> SubsumeOutcome.Unrelated
                                || reprSiblings ctx argTy expectedTy
                        )
                    | ValueNone -> false
                | _ -> false
            | _ -> false
        | _ -> false

    /// How an expected type admits one actual type at an argument position.
    [<RequireQualifiedAccess>]
    type private Absorption =
        /// The expected type accepts the actual as-is, with no unification.
        | Accepts
        /// The expected type is an absorbing shape the actual does not satisfy.
        | Refuses
        /// The expected type is not an absorbing shape.
        | NotAbsorbing

    /// The absorbing shapes, in order: the universal `obj` supertype; a union slot, which is
    /// `obj` restricted to an enumerated member set; a platform type id numeric family; and an
    /// external interface met by a record's width. Each admits the actual WITHOUT unifying,
    /// since pinning a typar argument here would ground the enclosing type's parameter.
    /// The arms match disjoint resolved shapes of `expected`, so `Refuses` is final.
    let private absorbsWithoutPinning (ctx: PassContext) (actual: SemType) (expected: SemType) : Absorption =
        let accepts (target: SemType) =
            if subsumes ctx actual target <> SubsumeOutcome.Unrelated then
                Absorption.Accepts
            else
                Absorption.Refuses

        if absorbsAsObj ctx.Store expected then
            Absorption.Accepts
        else
            match resolveStep ctx.Store expected with
            | TyOr _ -> accepts expected
            | _ ->
                match numericFamilyOr ctx expected with
                | ValueSome fam -> accepts fam
                | ValueNone ->
                    if tryStructuralWiden ctx actual expected then
                        Absorption.Accepts
                    else
                        Absorption.NotAbsorbing

    let rec unify (ctx: PassContext) (tok: SyntaxToken) (a: SemType) (b: SemType) =
        let a = resolveStep ctx.Store a
        let b = resolveStep ctx.Store b

        match a, b with
        // Ground-fold a carried type-level computation that has become CONCRETE: once
        // `Events[Key]` grounds with `Key := "ping"`, the node collapses to the member type
        // instead of linking a var to an inert carrier.
        | FoldedCarrier ctx folded, _ -> unify ctx tok folded b
        | _, FoldedCarrier ctx folded -> unify ctx tok a folded
        // A position with no type unifies with nothing: no Link, so one broken one can't
        // cascade. Only the two reasons a contract bakes report, and `ReportOnce` collapses
        // each to ONE message per file, however many positions and uses meet it.
        | TyUnknown reason, _
        | _, TyUnknown reason ->
            match reason with
            // A name written in this unit's source is already diagnosed at its own site
            // (`UndefinedTypeNames`); what reports here is a name a baked contract could not
            // resolve.
            | UnknownReason.UndefinedName name ->
                if not (ctx.UndefinedTypeNames.Contains name) then
                    ctx.ReportOnce(
                        tok,
                        Kind.Message(
                            sprintf
                                "Type '%s' could not be resolved during contract extraction. Is a package dependency missing?"
                                name
                        )
                    )
            // A feature gap in this compiler's extractor, not a missing dependency, so it
            // reports as `NotYetSupported`, the verdict an unmodelled annotation also gets.
            | UnknownReason.UnfreezableExternal what ->
                ctx.ReportOnce(tok, Kind.NotYetSupported(sprintf "%s, so its signature did not extract" what))
            | UnknownReason.UnresolvedTypar
            | UnknownReason.Deferred
            | UnknownReason.ArityMismatch
            | UnknownReason.NoValueType -> ()
        | TyConst(k1, a1), TyConst(k2, a2) when k1 = k2 && a1.Length = a2.Length -> unifyArgs ctx tok a1 a2
        | TyRecord(n1, a1), TyRecord(n2, a2) when n1 = n2 && a1.Length = a2.Length -> unifyArgs ctx tok a1 a2
        | TyUnion(n1, a1), TyUnion(n2, a2) when n1 = n2 && a1.Length = a2.Length -> unifyArgs ctx tok a1 a2
        // A capability interface reaches `unify` under EITHER of its two names (a BCL
        // `IEnumerable\`1` met by a declared `seq`), so `n1 = n2` fails on the same type.
        | TyClass(n1, a1), TyClass(n2, a2) when sameNominalKey ctx n1 n2 && a1.Length = a2.Length ->
            unifyArgs ctx tok a1 a2
        // An enum is a DISTINCT nominal, never structurally its underlying type, so
        // `let n: int = E.C1` is a genuine type error.
        | TyEnum k1, TyEnum k2 when k1 = k2 -> ()
        | TyLiteral v1, TyLiteral v2 when v1 = v2 -> ()
        | TyFun(a1, r1), TyFun(a2, r2) ->
            unify ctx tok a1 a2
            unify ctx tok r1 r2
        | TyTuple xs, TyTuple ys when xs.Length = ys.Length -> unifyArgs ctx tok xs ys
        // Anonymous unions unify by *set equality only*, because `EqSet` makes `string | int` and
        // `int | string` the SAME value, and members are ground, so there is nothing to
        // link. Membership (`int ≤ int | string`) belongs to `subsumes`.
        | TyOr m1, TyOr m2 when m1 = m2 -> ()
        // The carried type-level computations unify STRUCTURALLY, as opaque constructors:
        // same type constructor, children pairwise. NOT evaluation: no `keyof` expansion.
        | TyKeyOf t1, TyKeyOf t2 -> unify ctx tok t1 t2
        | TyIndexedAccess(o1, i1), TyIndexedAccess(o2, i2) ->
            unify ctx tok o1 o2
            unify ctx tok i1 i2
        | TyConditional c1, TyConditional c2 ->
            unify ctx tok c1.Check c2.Check
            unify ctx tok c1.Extends c2.Extends
            unify ctx tok c1.WhenTrue c2.WhenTrue
            unify ctx tok c1.WhenFalse c2.WhenFalse
        | TyVar tv1, TyVar tv2 when tv1 = tv2 -> ()
        | TyVar tv1, TyVar tv2 ->
            let r1 = UnionFind.find ctx.Store tv1
            let r2 = UnionFind.find ctx.Store tv2
            let unitsA = ctx.Store.Units r1
            let unitsB = ctx.Store.Units r2
            let linkA = ctx.Store.Link r1
            let linkB = ctx.Store.Link r2
            UnionFind.union ctx.Store r1.Id r2.Id
            let newRoot = UnionFind.find ctx.Store r1.Id

            let merged = if newRoot = r1 then r2 else r1

            // Fold the loser's deferred-constraint payload into the surviving
            // representative, because payload lives only under the rep id.
            ctx.Store.MergePayloads(newRoot, merged)
            mergeUnits ctx tok newRoot unitsA unitsB

            match linkA, linkB with
            | ValueNone, ValueNone -> ()
            | ValueSome t, ValueNone
            | ValueNone, ValueSome t ->
                ctx.Store.SetLink(newRoot, ValueSome t)
                dischargeAll ctx tok newRoot t
            | ValueSome a, ValueSome b ->
                ctx.Store.SetLink(newRoot, linkA)
                unify ctx tok a b
                dischargeAll ctx tok newRoot a
        | TyVar tv, other
        | other, TyVar tv ->
            let root = UnionFind.find ctx.Store tv

            if occursAndAdjust ctx.Store root other then
                ctx.Report(
                    tok,
                    Kind.Message(
                        sprintf
                            "Occurs check: cannot construct infinite type %A = %A"
                            (zonk ctx.Store (TyVar root.Id))
                            (zonk ctx.Store other)
                    )
                )
            else
                match ctx.Store.Units root, other with
                | ValueSome m, TyConst _ when not m.IsDimensionless ->
                    ctx.Report(tok, Kind.DimensionlessMeasureMismatch(string m))
                | _ -> ()

                ctx.Store.SetLink(root, ValueSome other)
                dischargeAll ctx tok root other
        // `unify` is symmetric, so neither side can be named the expected one here. A seam
        // that knows which is written (`(e : T)`) reports its own directional message.
        | _ -> ctx.Report(tok, Kind.Message(sprintf "Type mismatch: %s vs %s" (shown ctx.Store a) (shown ctx.Store b)))

    and private unifyArgs (ctx: PassContext) (tok: SyntaxToken) (xs: EqArray<SemType>) (ys: EqArray<SemType>) : unit =
        for i in 0 .. xs.Length - 1 do
            unify ctx tok xs.[i] ys.[i]

    /// Coerce one argument position against its expected parameter type, deferring to the
    /// absorbing shapes and, failing those, unifying.
    and private unifyArgCoerce (ctx: PassContext) (tok: SyntaxToken) (actual: SemType) (expected: SemType) : unit =
        match resolveStep ctx.Store actual, resolveStep ctx.Store expected with
        | TyTuple aa, TyTuple bb when aa.Length = bb.Length ->
            for i in 0 .. aa.Length - 1 do
                unifyArgCoerce ctx tok aa.[i] bb.[i]
        | a, b ->
            match absorbsWithoutPinning ctx a b with
            | Absorption.Accepts -> ()
            | Absorption.Refuses
            | Absorption.NotAbsorbing -> unify ctx tok a b

    /// Unify an *applied callable* shape against a resolved member signature, coercing
    /// each argument position rather than unifying it: `comparer.GetHashCode(x)` builds
    /// `TyFun('T, result)`, met by the signature `TyFun(obj, int)`. Results unify exactly.
    and unifyAppliedSig (ctx: PassContext) (tok: SyntaxToken) (actual: SemType) (expected: SemType) : unit =
        match resolveStep ctx.Store actual, resolveStep ctx.Store expected with
        | TyFun(ad, ar), TyFun(ed, er) ->
            unifyArgCoerce ctx tok ad ed
            unifyAppliedSig ctx tok ar er
        | a, b -> unify ctx tok a b

    and private dischargeAll (ctx: PassContext) (tok: SyntaxToken) (root: Rep) (t: SemType) : unit =
        dischargePendingDotAccess ctx root t
        dischargeConstraints ctx tok root t
        dischargeSrtpTraits ctx tok root t

    /// Resolve the dot-access constraints parked on a TyVar now its `Link` has settled.
    /// When the object argument is generic its arg list substitutes for the declared typars, so
    /// `(b : Box<int>).Value` resolves to `int`, not `Box`'s prototype `'a`.
    and private dischargePendingDotAccess (ctx: PassContext) (root: Rep) (linkTarget: SemType) : unit =
        let pending = ctx.Store.Pda.Live root

        if not (List.isEmpty pending) then
            // Every resolving branch discharges the whole snapshot up front, so a
            // reentrant discharge (the `unify`s below) sees it gone. Only `NotNominal`
            // leaves the accesses parked for a later `Link`.
            let solveAll () =
                for d in pending do
                    ctx.Store.Pda.Solve d

            match resolveDotSource ctx linkTarget with
            | DotSource.NotNominal -> ()
            | DotSource.UnknownType(name, kind) ->
                solveAll ()

                for d in pending do
                    ctx.Report(d.Use.Tok, Kind.UnknownNominalType(kind, name))
            | DotSource.Resolved(name, memberNoun, subst, lookup) ->
                solveAll ()

                for d in pending do
                    match lookup d.MemberName with
                    | ValueSome ty -> unify ctx d.Use.Tok (TyVar d.ResultTv) (substituteWith ctx.Store subst ty)
                    | ValueNone -> ctx.Report(d.Use.Tok, Kind.NoMember(name, memberNoun, d.MemberName))
            | DotSource.ClassChain(key, args) ->
                solveAll ()

                let (DisplayName shown) = SymbolKeyOps.typeSimpleName key

                for d in pending do
                    match tryClassChainMemberOrField ctx key args d.MemberName with
                    | ValueSome ty -> unify ctx d.Use.Tok (TyVar d.ResultTv) ty
                    | ValueNone -> ctx.Report(d.Use.Tok, Kind.NoMember(shown, MemberNoun.InstanceMember, d.MemberName))
            | DotSource.ExternalClass(key, args) ->
                solveAll ()

                let argArr = args.AsSpan().ToArray()
                let lookupKey = capabilityPlatformKey ctx key

                for d in pending do
                    match ctx.Provider.TryLookupMember(lookupKey, d.MemberName) with
                    | ValueSome m when not m.IsStatic ->
                        let memberSig = ExternalSymbols.openSignature ctx m argArr

                        ctx.Resolution.ExternalAccess.Set(d.Use.Key, ResolvedExternalMember.OfMember(m, memberSig))

                        // The application linked the arg into `d.ResultTv`'s domain while the
                        // object argument was deferred, so the `obj` parameter of `GetHashCode(obj)`
                        // must absorb a typar argument here, not ground it.
                        unifyAppliedSig ctx d.Use.Tok (TyVar d.ResultTv) memberSig
                    | _ ->
                        ctx.Report(
                            d.Use.Tok,
                            Kind.NoMember(SymbolKeyOps.typeMetaName key, MemberNoun.InstanceMember, d.MemberName)
                        )

    /// On-unified callback for type-parameter constraints. Satisfied constraints are
    /// dropped; deferred ones remain on the root, and a compound `Defer` also copies the
    /// constraint onto each still-free arg so a Link on any of them re-evaluates the rule.
    and private dischargeConstraints (ctx: PassContext) (tok: SyntaxToken) (root: Rep) (linkTarget: SemType) : unit =
        if ctx.Store.Constraints.IsEmpty root then
            ()
        else
            let cs = ctx.Store.Constraints.Items root
            ctx.Store.Constraints.Set(root, [])
            let mutable remaining = []

            for c in cs do
                match c.Kind with
                | SemanticConstraintKind.Coercion target ->
                    let targetNominal = subtypeNominalOf ctx (zonk ctx.Store target)

                    // Dependent-typar inference: a constraint `'a :> IFace<'b>` whose target carries
                    // free vars. Once `'a` grounds to a nominal implementing `IFace`, pin `'b`
                    // to the witnessed args, so `'S :> IStructSeq<'T,'E>` grounds its `'T` and `'E`.
                    match targetNominal with
                    | ValueSome(struct (tname, targs)) when
                        targs.Length > 0
                        && targs
                           |> EqArray.exists (fun a ->
                               match resolveStep ctx.Store a with
                               | TyVar _ -> true
                               | _ -> false
                           )
                        ->
                        match tryUpcastWitness ctx linkTarget tname with
                        | ValueSome wargs when wargs.Length = targs.Length ->
                            for i in 0 .. targs.Length - 1 do
                                unify ctx tok wargs.[i] targs.[i]
                        | _ -> ()
                    | _ -> ()

                    // The INVERSE direction: a source lambda with STILL-FREE domains
                    // (`fun x y -> x + y`) coerced into a GROUND slot (`'TF :> Fun<int,int,int>`)
                    // grounds from the slot's args, so the body's SRTP operators can resolve.
                    match targetNominal, resolveStep ctx.Store linkTarget with
                    | ValueSome(struct (tname, targs)), TyFun(a, b) ->
                        match tryFunSlotPeel ctx.Store tname targs a b with
                        | ValueSome tys -> tys |> List.iteri (fun i s -> unify ctx tok s targs.[i])
                        | ValueNone -> ()
                    | _ -> ()
                // `'a : enum<'u>` grounds `'u` from the enum the typar linked to.
                | SemanticConstraintKind.Enum underlying ->
                    match resolveStep ctx.Store linkTarget with
                    | TyEnum key ->
                        match enumUnderlyingType ctx key with
                        | ValueSome enumUnderlying -> unify ctx tok enumUnderlying underlying
                        | ValueNone -> ()
                    | _ -> ()
                | _ -> ()

                match checkConstraint ctx c linkTarget with
                | Satisfied -> ()
                | Violated -> reportConstraintViolation ctx tok c linkTarget
                | Defer ->
                    remaining <- c :: remaining
                    propagateToFreeArgs ctx c linkTarget

            ctx.Store.Constraints.Set(root, List.rev remaining)

    /// Commit the picked trait member: unify it against the trait's expected signature,
    /// in the same shape the read-only search matched it with.
    and private unifySrtpAgainst
        (ctx: PassContext)
        (tok: SyntaxToken)
        (candidate: SemType)
        (memberTrait: MemberSignature)
        : unit =
        unify
            ctx
            tok
            candidate
            (UnificationTraitMembers.expectedShape ctx.Store memberTrait.ArgTypes memberTrait.ReturnType candidate)

    /// Attempt one SRTP member trait through the shared search. With `force = false`
    /// an unpinned support type defers the trait: solving against the first-pinned host
    /// would force the other operands to ITS signature, and `3 * v` would pin to `int`'s
    /// member before `Vec2`'s `int * Vec2` could win.
    and private trySolveSrtpTrait (ctx: PassContext) (tok: SyntaxToken) (b: MemberSignature) (force: bool) : unit =
        let supportTys = EqArray.toArray b.SupportTys

        // A QUANTIFIED free support typar is a scheme's type parameter: the trait travels
        // with the template and inline expansion dispatches it per use site, so a forced
        // attempt must not link it.
        let anyQuantifiedFree () =
            supportTys
            |> Array.exists (fun t ->
                match resolveStep ctx.Store (zonk ctx.Store t) with
                | TyVar tv -> ctx.Store.Quantified(UnionFind.find ctx.Store tv)
                | _ -> false
            )

        if not (force && anyQuantifiedFree ()) then
            match UnificationTraitMembers.pick ctx b.MemberName b.ArgTypes b.ReturnType supportTys force with
            | UnificationTraitMembers.TraitPick.Incomplete -> ()
            | UnificationTraitMembers.TraitPick.NoSupport
            | UnificationTraitMembers.TraitPick.NameOnly _ -> ctx.Store.Srtp.Solve b
            // An ambiguous pick still commits the first winner: its signature grounds
            // the operands for subsequent inference.
            | UnificationTraitMembers.TraitPick.Resolved c
            | UnificationTraitMembers.TraitPick.Ambiguous(first = c) ->
                ctx.Store.Srtp.Solve b
                unifySrtpAgainst ctx tok c.Ty b

    /// On-unified callback for SRTP member traits. The one `MemberSignature` is
    /// shared by reference across every participating typar, so solving it through any
    /// of them makes the others skip it. `tok` is the user's call site.
    and private dischargeSrtpTraits (ctx: PassContext) (tok: SyntaxToken) (root: Rep) (linkTarget: SemType) : unit =
        let traits = ctx.Store.Srtp.Live root

        if List.isEmpty traits then
            ()
        else
            for b in traits do
                // A sibling / reentrant discharge may have solved `b` since this snapshot.
                if not (ctx.Store.Srtp.IsSolved b) then
                    trySolveSrtpTrait ctx tok b false

    /// Force-attempt every live SRTP trait in the store; `tok` attributes any verdict.
    /// The binding-boundary settling of SRTP traits; the on-link discharge is only the
    /// eager path.
    let sweepSrtpTraits (ctx: PassContext) (tok: SyntaxToken) : unit =
        for b in ctx.Store.Srtp.LiveEntries() do
            if not (ctx.Store.Srtp.IsSolved b) then
                trySolveSrtpTrait ctx tok b true

    /// Coerce `src` to the nominal target `tgt` as an implicit/`:>` upcast: when `src`
    /// (or a base / interface) instantiates `tgt`'s nominal, `unify` the witness's type
    /// args against `tgt`'s, pinning the `_` in `this :> seq<_>`. Unlike `subsumes`, MUTATES.
    let tryCoerceUpcast (ctx: PassContext) (tok: SyntaxToken) (src: SemType) (tgt: SemType) : bool =
        match absorbsWithoutPinning ctx src tgt with
        | Absorption.Accepts -> true
        | Absorption.Refuses -> false
        | Absorption.NotAbsorbing ->
            match subtypeNominalOf ctx tgt with
            | ValueNone -> false
            | ValueSome(struct (tname, targs)) ->
                match tryUpcastWitness ctx src tname with
                | ValueSome sargs when sargs.Length = targs.Length ->
                    for i in 0 .. targs.Length - 1 do
                        unify ctx tok sargs.[i] targs.[i]

                    true
                | _ -> false

    /// Unify an *argument* against its expected parameter type, admitting the implicit
    /// class→interface / class→base upcast F# inserts at a coercion point: a
    /// `Comparer<'T>` value is passed where an `IComparer<'T>` is expected. Tuples walk
    /// element-wise.
    let rec unifyArg (ctx: PassContext) (tok: SyntaxToken) (actual: SemType) (expected: SemType) : unit =
        match resolveStep ctx.Store actual, resolveStep ctx.Store expected with
        | TyTuple aa, TyTuple bb when aa.Length = bb.Length ->
            for i in 0 .. aa.Length - 1 do
                unifyArg ctx tok aa.[i] bb.[i]
        | a, b ->
            if not (tryCoerceUpcast ctx tok a b) then
                unify ctx tok a b

    /// How a written annotation admits the inferred type beside it, or grounds it.
    [<RequireQualifiedAccess>]
    type private Admission =
        /// A union annotation covering the inferred type (`let x: int | string = 1`).
        | UnionSubsumption
        /// A literal-bearing inferred type against a wider annotation (`let s: string = "a"`).
        | LiteralWidening
        /// A strict supertype annotation (`let toExn (e: InvalidOperationException) : exn = e`).
        | NominalUpcast
        /// Symmetric `unify`, which pins the inferred type's free variables to the annotation.
        | Grounding

    let private classifyAdmission (ctx: PassContext) (actual: SemType) (expected: SemType) : Admission =
        let rec isLiteralBearing t =
            match resolveStep ctx.Store t with
            | TyLiteral _ -> true
            | TyOr ds -> ds.Disjuncts |> EqSet.forall isLiteralBearing
            | _ -> false

        match subsumes ctx actual expected, resolveStep ctx.Store expected with
        | SubsumeOutcome.Unrelated, _ -> Admission.Grounding
        | _, TyOr _ -> Admission.UnionSubsumption
        | _, _ when isLiteralBearing actual -> Admission.LiteralWidening
        | SubsumeOutcome.Subtype, _ -> Admission.NominalUpcast
        | SubsumeOutcome.Equal, _ -> Admission.Grounding

    /// Reconcile an inferred type against a *written annotation*, at both the `let x : T = e`
    /// binding and the `(e : T)` ascription.
    let unifyAnnotation (ctx: PassContext) (tok: SyntaxToken) (actual: SemType) (expected: SemType) : unit =
        match classifyAdmission ctx actual expected with
        | Admission.UnionSubsumption
        | Admission.LiteralWidening
        | Admission.NominalUpcast -> ()
        | Admission.Grounding -> unify ctx tok actual expected
