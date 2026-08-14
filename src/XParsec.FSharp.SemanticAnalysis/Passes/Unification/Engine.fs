namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationSubsume

/// The MUTATING unifier: `unify`, its on-link discharges (deferred dot-accesses,
/// typar constraints, SRTP bounds), and the argument / annotation coercion walkers.
module UnificationEngine =

    /// The flat `FunN` arity a parameter slot constrains its argument to, or `ValueNone`
    /// for a non-`Fun`-bounded parameter: `'TF :> Fun<a,b>` is arity 1, up through
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
        | ExternalClass of key: SymbolKey * args: EqArray<SemType>

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
                DotSource.ExternalClass(SymbolKey.Type key, args)
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

    /// `Defer` is the "I don't know yet" answer: the target is still free (or
    /// compound-with-free-args). It stays on the TyVar and re-fires on the next `Link`.
    type ConstraintOutcome =
        | Satisfied
        | Violated
        | Defer

    /// `int` is equatable because `prim-types-min.fsi` declares `interface equatable<int>`.
    let private primitiveDeclares
        (ctx: PassContext)
        (cap: RuntimeNames.CapabilityIdentity voption)
        (key: SymbolKey)
        : bool =
        match ctx.Provider.TryLookupType key with
        | ValueSome(ExternalTypeShape.Intrinsic { Class = ValueSome surface }) ->
            RuntimeNames.declaresCapability cap surface.Interfaces
        | _ -> false

    let private constraintKindName (k: SemanticConstraintKind) : string =
        match k with
        | SemanticConstraintKind.Equality -> "equality"
        | SemanticConstraintKind.Comparison -> "comparison"
        | SemanticConstraintKind.Struct -> "struct"
        | SemanticConstraintKind.ReferenceType -> "not struct"
        | SemanticConstraintKind.Nullness -> "null"
        | SemanticConstraintKind.NotNull -> "not null"
        | SemanticConstraintKind.Coercion target -> sprintf "subtype of %A" target

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

    /// A foreign parameter whose type IS a platform repr shared by several primitives
    /// (JS `number` <- int/float/float32) admits any member of that family: returns the
    /// family as a `TyOr`, else `ValueNone` for the ordinary single-canon case.
    let private numericFamilyOr (ctx: PassContext) (ty: SemType) : SemType voption =
        match resolveStep ctx.Store ty with
        // The axis is keyed by platform repr, so only a type whose identity IS a platform
        // name can hit it: a Vesper-qualified `number` is refused.
        | TyConst(PlatformName platform, args) when args.Length = 0 ->
            match IntrinsicTypeMap.canonsOf platform ctx.IntrinsicTypeMap.Value with
            | canons when canons.Length > 1 ->
                ValueSome(SemType.MkUnion(seq { for c in canons -> TyConst(c, EqArray.empty) }))
            | _ -> ValueNone
        | _ -> ValueNone

    /// Two intrinsic canons are REPR-SIBLINGS iff some platform repr names BOTH: on JS
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
            match ctx.Provider.TryLookupType(SymbolKey.Type ikey) with
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
                                    ExternalSymbols.instantiateSignature ctx.Store m declArgs ctx.CurrentLevel

                                subsumes ctx argTy expectedTy <> SubsumeOutcome.Unrelated
                                || reprSiblings ctx argTy expectedTy
                        )
                    | ValueNone -> false
                | _ -> false
            | _ -> false
        | _ -> false

    let rec unify (ctx: PassContext) (tok: SyntaxToken) (a: SemType) (b: SemType) =
        let a = resolveStep ctx.Store a
        let b = resolveStep ctx.Store b

        match a, b with
        // Ground-fold a carried type-level computation that has become CONCRETE: once
        // `Events[Key]` grounds with `Key := "ping"`, the node collapses to the member type
        // instead of linking a var to an inert carrier.
        | FoldedCarrier ctx folded, _ -> unify ctx tok folded b
        | _, FoldedCarrier ctx folded -> unify ctx tok a folded
        // An unresolved type constructor unifies with nothing: no Link, so one broken one can't
        // cascade. A name this unit's source wrote was already blamed where it was written
        // (`UndefinedTypeNames`); what reports here is a name a baked contract could not resolve.
        | TyUnknown name, _
        | _, TyUnknown name ->
            if not (ctx.UndefinedTypeNames.Contains name) then
                ctx.Report(
                    tok,
                    Kind.Message(
                        sprintf
                            "Type '%s' could not be resolved during contract extraction — is a package dependency missing?"
                            name
                    )
                )
        | TyConst(k1, a1), TyConst(k2, a2) when k1 = k2 && a1.Length = a2.Length -> unifyArgs ctx tok a1 a2
        | TyRecord(n1, a1), TyRecord(n2, a2) when n1 = n2 && a1.Length = a2.Length -> unifyArgs ctx tok a1 a2
        | TyUnion(n1, a1), TyUnion(n2, a2) when n1 = n2 && a1.Length = a2.Length -> unifyArgs ctx tok a1 a2
        // A capability interface reaches `unify` under EITHER of its two names (a BCL
        // `IEnumerable\`1` met by a declared `seq`), so `n1 = n2` fails on the same type.
        | TyClass(n1, a1), TyClass(n2, a2) when
            sameNominalKey ctx (SymbolKey.Type n1) (SymbolKey.Type n2)
            && a1.Length = a2.Length
            ->
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

            if occursAndAdjust ctx.Store root.Id other then
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
                    ctx.Report(tok, Kind.Message(sprintf "Dimensionless %A used where <%O> expected" other m))
                | _ -> ()

                ctx.Store.SetLink(root, ValueSome other)
                dischargeAll ctx tok root other
        // `unify` is symmetric, so neither side can be named the expected one here. A seam
        // that knows which is written (`(e : T)`) reports its own directional message.
        | _ -> ctx.Report(tok, Kind.Message(sprintf "Type mismatch: %s vs %s" (shown ctx.Store a) (shown ctx.Store b)))

    and private unifyArgs (ctx: PassContext) (tok: SyntaxToken) (xs: EqArray<SemType>) (ys: EqArray<SemType>) : unit =
        for i in 0 .. xs.Length - 1 do
            unify ctx tok xs.[i] ys.[i]

    /// Coerce one argument position against its expected parameter type: an `obj` or
    /// union-typed parameter absorbs *any* subsuming argument without unifying, since
    /// pinning a typar argument to `obj` would ground the enclosing type's parameter.
    and private unifyArgCoerce (ctx: PassContext) (tok: SyntaxToken) (actual: SemType) (expected: SemType) : unit =
        match resolveStep ctx.Store actual, resolveStep ctx.Store expected with
        | TyTuple aa, TyTuple bb when aa.Length = bb.Length ->
            for i in 0 .. aa.Length - 1 do
                unifyArgCoerce ctx tok aa.[i] bb.[i]
        | a, b ->
            if absorbsAsObj ctx.Store b then
                ()
            else
                match b with
                | TyOr _ when subsumes ctx a b <> SubsumeOutcome.Unrelated -> ()
                | _ ->
                    match numericFamilyOr ctx b with
                    | ValueSome fam when subsumes ctx a fam <> SubsumeOutcome.Unrelated -> ()
                    | _ -> if tryStructuralWiden ctx a b then () else unify ctx tok a b

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
        dischargeSrtpBounds ctx tok root t

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
                    match tryClassChainMember ctx key args d.MemberName with
                    | ValueSome ty -> unify ctx d.Use.Tok (TyVar d.ResultTv) ty
                    | ValueNone -> ctx.Report(d.Use.Tok, Kind.NoMember(shown, MemberNoun.InstanceMember, d.MemberName))
            | DotSource.ExternalClass(key, args) ->
                solveAll ()

                let argArr = args.AsSpan().ToArray()
                let lookupKey = capabilityPlatformKey ctx key

                for d in pending do
                    match ctx.Provider.TryLookupMember(lookupKey, d.MemberName) with
                    | ValueSome m when not m.IsStatic ->
                        let memberSig = ExternalSymbols.openSignature m argArr

                        ctx.Resolution.ExternalAccess.Set(
                            d.Use.Key,
                            {
                                Key = SymbolKey.Member m.Key
                                IsStatic = false
                                Storage = m.Storage
                                Signature = memberSig
                                ArgGroupWidths = ExternalSignature.argGroupWidths m.Signature
                                OptionalDefaults = m.OptionalDefaults
                            }
                        )

                        // The application linked the arg into `d.ResultTv`'s domain while the
                        // object argument was deferred, so the `obj` parameter of `GetHashCode(obj)`
                        // must absorb a typar argument here, not ground it.
                        unifyAppliedSig ctx d.Use.Tok (TyVar d.ResultTv) memberSig
                    | _ ->
                        ctx.Report(
                            d.Use.Tok,
                            Kind.NoMember(SymbolKeyOps.qualifiedName key, MemberNoun.InstanceMember, d.MemberName)
                        )

    /// `ValueSome true` = constraint holds; `ValueSome false` = violation;
    /// `ValueNone` = no answer, fall through to structural / deferred handling.
    and private primitiveSupports (ctx: PassContext) (kind: SemanticConstraintKind) (key: SymbolKey) : bool voption =
        // By KEY, not by name: a user type merely spelled `int` in its own namespace reaches
        // no contract shape, so it declares no capability.
        match kind with
        // An undeclared capability defers rather than refusing: `decimal` on JS has no
        // contract to reach, so it has said nothing, not "no".
        | SemanticConstraintKind.Equality ->
            if primitiveDeclares ctx ctx.CapabilityIds.Equatable key then
                ValueSome true
            else
                ValueNone
        | SemanticConstraintKind.Comparison ->
            if primitiveDeclares ctx ctx.CapabilityIds.Comparable key then
                ValueSome true
            else
                ValueNone
        // Nullness is answered structurally and value-ness by the target, for primitives as
        // much as for anything else, so neither reaches this table.
        | SemanticConstraintKind.Struct
        | SemanticConstraintKind.ReferenceType
        | SemanticConstraintKind.Nullness
        | SemanticConstraintKind.NotNull
        | SemanticConstraintKind.Coercion _ -> ValueNone

    /// `Violated` is sticky (once any element fails, the whole compound fails);
    /// `Defer` propagates when no element failed but at least one is still pending.
    and private reduceOutcome (check: SemType -> ConstraintOutcome) (items: seq<SemType>) : ConstraintOutcome =
        let mutable result = Satisfied

        for item in items do
            match result, check item with
            | Violated, _ -> ()
            | _, Violated -> result <- Violated
            | Defer, _
            | _, Defer -> result <- Defer
            | Satisfied, Satisfied -> ()

        result

    /// The dual of `reduceOutcome`: `Satisfied` is sticky (one element proving it proves the
    /// whole); `Defer` propagates when none proved it but at least one is still pending.
    and private reduceAny (check: SemType -> ConstraintOutcome) (items: seq<SemType>) : ConstraintOutcome =
        let mutable result = Violated

        for item in items do
            match result, check item with
            | Satisfied, _ -> ()
            | _, Satisfied -> result <- Satisfied
            | Defer, _
            | _, Defer -> result <- Defer
            | Violated, Violated -> ()

        result

    and private negate (outcome: ConstraintOutcome) : ConstraintOutcome =
        match outcome with
        | Satisfied -> Violated
        | Violated -> Satisfied
        | Defer -> Defer

    /// Does `null` inhabit this type? `null` is a union MEMBER, not a property of a type:
    /// `objnull` is `obj | null` and admits it where bare `obj` does not. The CLR's
    /// reference-null is erased at the ABI seam, so the answer is the same on every target.
    and private admitsNull (ctx: PassContext) (t: SemType) : ConstraintOutcome =
        match resolveStep ctx.Store t with
        // Not ground yet, so it states nothing either way: the next `Link` re-fires the check.
        | TyVar _
        | TyUnknown _
        | TyTypar _
        | TyKeyOf _
        | TyIndexedAccess _
        | TyConditional _ -> Defer
        | TyNull -> Satisfied
        // ANY member carrying `null` admits it, so one `null` member decides the union and an
        // ungrounded member defers. `never` has no member to carry `null`.
        | TyOr members -> reduceAny (admitsNull ctx) (members.Members.Underlying :> seq<SemType>)
        // `[<AllowNullLiteral>]` is the class's own statement that `null` inhabits it, which is
        // what makes `let empty: T = null` legal on such a class.
        | TyClass(classKey, _) ->
            match TypeRegistry.tryClassByKey ctx.Types classKey with
            | ValueSome info ->
                if info.Declared.AllowNullLiteral then
                    Satisfied
                else
                    Violated
            | ValueNone ->
                match ctx.Provider.TryLookupType(SymbolKey.Type classKey) with
                | ValueSome(ExternalTypeShape.Class shape) ->
                    if shape.Flags.Declared.AllowNullLiteral then
                        Satisfied
                    else
                        Violated
                // No class shape in hand, so refusing a legal `isNull` here would be a guess.
                | _ -> Defer
        // Every other ground shape (primitives, tuples, functions, records, unions, enums)
        // carries no `null` member.
        | _ -> Violated

    /// The layout query as a constraint verdict. No answer is a `Defer`, never a refusal: a
    /// compile composing no platform states nothing about either polarity, and neither does
    /// the front end about a tuple, whose layout only its backend encoding settles.
    and private valueLayoutOutcome (ctx: PassContext) (t: SemType) : ConstraintOutcome =
        match TypeLayout.ofSemType ctx t with
        | TypeLayout.Value -> Satisfied
        | TypeLayout.Reference -> Violated
        | TypeLayout.Unanswered -> Defer

    /// Free TyVars return `Defer` so the next `Link` assignment re-fires the check; nested
    /// compounds recurse compositionally.
    and checkConstraint (ctx: PassContext) (c: SemanticConstraint) (t: SemType) : ConstraintOutcome =
        // The type's stamped equality / comparison verdict overrides the field-walk: a
        // `Custom` type is `Satisfied` by its own members, and its fields may individually
        // lack equality.
        let verdictOutcome
            (eq: EqualityVerdict)
            (cmp: ComparisonVerdict)
            (fieldsOf: unit -> seq<SemType>)
            : ConstraintOutcome =
            match c.Kind, eq, cmp with
            | SemanticConstraintKind.Equality, EqualityVerdict.NoEquality, _ -> Violated
            | SemanticConstraintKind.Equality, (EqualityVerdict.Reference | EqualityVerdict.Custom), _ -> Satisfied
            | SemanticConstraintKind.Comparison, _, ComparisonVerdict.NoComparison -> Violated
            | SemanticConstraintKind.Comparison, _, ComparisonVerdict.Custom -> Satisfied
            | _ -> reduceOutcome (checkConstraint ctx c) (fieldsOf ())

        match c.Kind, resolveStep ctx.Store t with
        | _, TyVar _ -> Defer
        // An unresolved contract type supports no constraint, but the mismatch was
        // already reported where it unified, so a second error would be a duplicate.
        | _, TyUnknown _ -> Defer
        | _, TyTypar _ -> Defer
        // A carried type-level computation can decide no constraint until it grounds.
        | _, (TyKeyOf _ | TyIndexedAccess _ | TyConditional _) -> Defer
        | SemanticConstraintKind.Nullness, ty -> admitsNull ctx ty
        | SemanticConstraintKind.NotNull, ty -> negate (admitsNull ctx ty)
        // One query and its negation, over every shape a layout is decided for. A LITERAL is
        // excluded so it widens to its base primitive first.
        | SemanticConstraintKind.Struct,
          ((TyConst _ | TyTuple _ | TyFun _ | TyRecord _ | TyUnion _ | TyClass _ | TyOr _ | TyEnum _) as ty) ->
            valueLayoutOutcome ctx ty
        | SemanticConstraintKind.ReferenceType,
          ((TyConst _ | TyTuple _ | TyFun _ | TyRecord _ | TyUnion _ | TyClass _ | TyOr _ | TyEnum _) as ty) ->
            negate (valueLayoutOutcome ctx ty)
        // Equality on an enum is universal and comparison on one is out of scope, so
        // neither is ever proved or refused here.
        | _, TyEnum _ -> Defer
        // A structural literal erases to its base primitive, so re-entering with it judges
        // every kind, `Coercion` included, exactly as the base primitive would be.
        | _, TyLiteral v -> checkConstraint ctx c (TyConst(RuntimeNames.literalBaseKey v, EqArray.empty))
        | SemanticConstraintKind.Coercion target, _ ->
            // `'e :> exn`: `subsumes` walks user and BCL `inherit` chains, so a thrown
            // `InvalidOperationException` reaches `exn`. Past the `TyVar _` guard above,
            // `Unrelated` is a real violation, not "unknown yet".
            match subsumes ctx t target with
            | SubsumeOutcome.Equal
            | SubsumeOutcome.Subtype -> Satisfied
            | SubsumeOutcome.Unrelated -> Violated
        | k, TyConst(nameKey, _) ->
            match primitiveSupports ctx k nameKey with
            | ValueSome true -> Satisfied
            | ValueSome false -> Violated
            | ValueNone -> Defer
        | (SemanticConstraintKind.Equality | SemanticConstraintKind.Comparison), TyFun _ -> Violated
        | (SemanticConstraintKind.Equality | SemanticConstraintKind.Comparison), TyTuple items ->
            reduceOutcome (checkConstraint ctx c) (items.Underlying :> seq<SemType>)
        | (SemanticConstraintKind.Equality | SemanticConstraintKind.Comparison), TyRecord(recKey, args) ->
            match TypeRegistry.tryRecordByKey ctx.Types recKey with
            | ValueSome info ->
                verdictOutcome
                    info.EqualitySupport
                    info.ComparisonSupport
                    (fun () ->
                        let subst = mkNamedTypeSubst ctx.Store info.TypeParams args

                        info.Fields |> Seq.map (fun f -> substituteWith ctx.Store subst f.Type)
                    )
            | ValueNone -> Defer
        | (SemanticConstraintKind.Equality | SemanticConstraintKind.Comparison), TyUnion(unionKey, args) ->
            match TypeRegistry.tryUnionByKey ctx.Types unionKey with
            | ValueSome info ->
                verdictOutcome
                    info.EqualitySupport
                    info.ComparisonSupport
                    (fun () ->
                        let subst = mkNamedTypeSubst ctx.Store info.TypeParams args
                        let fields = ResizeArray<SemType>()

                        for case in info.Cases do
                            for field in case.Fields do
                                fields.Add(substituteWith ctx.Store subst field)

                        fields :> seq<SemType>
                    )
            | ValueNone -> Defer
        | (SemanticConstraintKind.Equality | SemanticConstraintKind.Comparison), TyClass(classKey, args) ->
            match TypeRegistry.tryClassByKey ctx.Types classKey with
            | ValueSome info ->
                verdictOutcome
                    info.EqualitySupport
                    info.ComparisonSupport
                    (fun () ->
                        let subst = mkNamedTypeSubst ctx.Store info.TypeParams args

                        info.InstanceFields |> Seq.map (fun f -> substituteWith ctx.Store subst f.Type)
                    )
            | ValueNone -> Defer
        | SemanticConstraintKind.Equality, TyOr members ->
            // EQUALITY iff EVERY member has it: generic equality is total on the union's
            // boxed repr, a cross-member `=` returning `false` rather than throwing.
            reduceOutcome (checkConstraint ctx c) (members.Members.Underlying :> seq<SemType>)
        | SemanticConstraintKind.Comparison, TyOr members ->
            // COMPARISON does NOT reduce member-wise: `(1).CompareTo("a")` throws, so a
            // heterogeneous union is non-comparable even when each member is comparable.
            if members.Members.IsEmpty then Satisfied else Violated

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
                // Dependent-typar inference: a bound `'a :> IFace<'b>` whose target carries
                // free vars. Once `'a` grounds to a nominal implementing `IFace`, pin `'b`
                // to the witnessed args, so `'S :> IStructSeq<'T,'E>` grounds its `'T` and `'E`.
                match c.Kind with
                | SemanticConstraintKind.Coercion target ->
                    match subtypeNominalOf ctx (zonk ctx.Store target) with
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
                | _ -> ()

                // The INVERSE direction: a source lambda with STILL-FREE domains
                // (`fun x y -> x + y`) coerced into a GROUND slot (`'TF :> Fun<int,int,int>`)
                // grounds from the slot's args, so the body's SRTP operators can resolve.
                match c.Kind with
                | SemanticConstraintKind.Coercion target ->
                    match subtypeNominalOf ctx (zonk ctx.Store target), resolveStep ctx.Store linkTarget with
                    | ValueSome(struct (tname, targs)), TyFun(a, b) when
                        funSlotArityOfSymbol tname targs.Length |> Option.isSome
                        ->
                        match peelFunDomains ctx.Store (targs.Length - 1) a b with
                        | Some tys -> tys |> List.iteri (fun i s -> unify ctx tok s targs.[i])
                        | None -> ()
                    | _ -> ()
                | _ -> ()

                match checkConstraint ctx c linkTarget with
                | Satisfied -> ()
                | Violated -> reportConstraintViolation ctx tok c linkTarget
                | Defer ->
                    remaining <- c :: remaining
                    propagateToFreeArgs ctx c linkTarget

            ctx.Store.Constraints.Set(root, List.rev remaining)

    and reportConstraintViolation
        (ctx: PassContext)
        (tok: SyntaxToken)
        (c: SemanticConstraint)
        (target: SemType)
        : unit =
        ctx.Report(tok, Kind.ConstraintNotSupported(shown ctx.Store target, constraintKindName c.Kind))

    /// When a compound shape is partially resolved, the parent constraint is
    /// satisfied iff every component supports it, so a still-free component
    /// carries the same constraint forward.
    and propagateToFreeArgs (ctx: PassContext) (c: SemanticConstraint) (t: SemType) : unit =
        let rec walk t =
            match resolveStep ctx.Store t with
            | TyVar tv ->
                let root = UnionFind.find ctx.Store tv
                addConstraintByKind ctx.Store root.Id c
            | t -> SemType.iterChildren walk t

        walk t

    and private tryDeclaredIntrinsicMember
        (ctx: PassContext)
        (key: SymbolKey)
        (args: EqArray<SemType>)
        (memberName: string)
        : SemType voption =
        let fromHost =
            match ctx.Types.IntrinsicAbbrevHost.TryGetValue key with
            | true, info ->
                match info.Members |> Array.tryFind (fun m -> m.IsStatic && m.Name = memberName) with
                | Some m -> ValueSome(instantiateMember ctx.Store (info.TypeParams, args) m.Type)
                | None -> ValueNone
            | false, _ -> ValueNone

        match fromHost with
        | ValueSome _ -> fromHost
        | ValueNone ->
            match ctx.Provider.TryLookupMember(key, memberName) with
            | ValueSome m when m.IsStatic -> ValueSome(ExternalSymbols.openSignature m (EqArray.toArray args))
            | _ -> ValueNone

    /// Build the expected trait signature tupled or curried, picking whichever matches
    /// the candidate's shape: F# accepts both `static member (+)(a, b)` and
    /// `static member (+) a b` as satisfying a trait declared `^T * ^T -> ^T`.
    and private unifySrtpAgainst
        (ctx: PassContext)
        (tok: SyntaxToken)
        (candidate: SemType)
        (bound: MemberSignature)
        : unit =
        let argTys = bound.ArgTypes

        let tupled =
            match argTys.Length with
            | 0 -> bound.ReturnType
            | 1 -> TyFun(argTys.[0], bound.ReturnType)
            | _ -> TyFun(TyTuple argTys, bound.ReturnType)

        match resolveStep ctx.Store candidate with
        | TyFun(TyTuple _, _) -> unify ctx tok candidate tupled
        | _ when argTys.Length >= 2 ->
            let curried = EqArray.foldBack (fun a r -> TyFun(a, r)) argTys bound.ReturnType

            unify ctx tok candidate curried
        | _ -> unify ctx tok candidate tupled

    /// On-unified callback for SRTP member-trait bounds. The one `MemberSignature` is
    /// shared by reference across every participating typar, so solving it through
    /// whichever links first makes the others skip it. `tok` is the user's call site.
    and private dischargeSrtpBounds (ctx: PassContext) (tok: SyntaxToken) (root: Rep) (linkTarget: SemType) : unit =
        let bounds = ctx.Store.Srtp.Live root

        if List.isEmpty bounds then
            ()
        else
            for b in bounds do
                // A sibling / reentrant discharge may have solved `b` since this snapshot.
                if not (ctx.Store.Srtp.IsSolved b) then
                    match resolveStep ctx.Store linkTarget with
                    | TyConst(primKey, primArgs) ->
                        match tryDeclaredIntrinsicMember ctx primKey primArgs b.MemberName with
                        | ValueSome candTy ->
                            ctx.Store.Srtp.Solve b
                            unifySrtpAgainst ctx tok candTy b
                        | ValueNone ->
                            let primName = SymbolKeyOps.intrinsicName primKey

                            ctx.Report(tok, Kind.NoMember(primName, MemberNoun.BuiltInStaticMember, b.MemberName))

                            ctx.Store.Srtp.Solve b
                    | TyClass(classKey, classArgs) ->
                        match TypeRegistry.tryClassByKey ctx.Types classKey with
                        | ValueSome info ->
                            match info.Members |> Array.tryFind (fun m -> m.IsStatic && m.Name = b.MemberName) with
                            | Some m ->
                                let candTy = instantiateMember ctx.Store (info.TypeParams, classArgs) m.Type
                                ctx.Store.Srtp.Solve b
                                unifySrtpAgainst ctx tok candTy b
                            | None ->
                                let (DisplayName shown) = SymbolKeyOps.typeSimpleName classKey

                                ctx.Report(tok, Kind.NoMember(shown, MemberNoun.StaticMember, b.MemberName))

                                ctx.Store.Srtp.Solve b
                        | ValueNone ->
                            // Not project-local: a consumer dispatching `s + t` on an
                            // `.fsi`-imported type reaches here.
                            match ctx.Provider.TryLookupMember(SymbolKey.Type classKey, b.MemberName) with
                            | ValueSome m when m.IsStatic ->
                                let candTy = ExternalSymbols.openSignature m (EqArray.toArray classArgs)

                                ctx.Store.Srtp.Solve b
                                unifySrtpAgainst ctx tok candTy b
                            | _ ->
                                // Unknown class, so the SRTP stays unsolved.
                                ()
                    | _ ->
                        // Target not yet a concrete type-bearing shape, so the SRTP stays unsolved.
                        ()

    /// Coerce `src` to the nominal target `tgt` as an implicit/`:>` upcast: when `src`
    /// (or a base / interface) instantiates `tgt`'s nominal, `unify` the witness's type
    /// args against `tgt`'s, pinning the `_` in `this :> seq<_>`. Unlike `subsumes`, MUTATES.
    let tryCoerceUpcast (ctx: PassContext) (tok: SyntaxToken) (src: SemType) (tgt: SemType) : bool =
        if absorbsAsObj ctx.Store tgt then
            true
        else

            // A union-typed slot is `obj` restricted to an enumerated member set: it accepts
            // any value subsuming into a member, with the same no-pin discipline as `obj`.
            match resolveStep ctx.Store tgt with
            | TyOr _ -> subsumes ctx src tgt <> SubsumeOutcome.Unrelated
            | _ ->

                match numericFamilyOr ctx tgt with
                | ValueSome fam -> subsumes ctx src fam <> SubsumeOutcome.Unrelated
                | ValueNone ->

                    if tryStructuralWiden ctx src tgt then
                        true
                    else

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

    /// Reconcile an inferred type against a *written annotation*. Admits value→union
    /// (`let x: int | string = 1`) and a concrete subtype into a supertype annotation
    /// without unifying; every other annotation GROUNDS via symmetric `unify`.
    let unifyAnnotation (ctx: PassContext) (tok: SyntaxToken) (actual: SemType) (expected: SemType) : unit =
        // The only actual admitted to the outward-widening arm below.
        let rec isLiteralBearing t =
            match resolveStep ctx.Store t with
            | TyLiteral _ -> true
            | TyOr ms -> ms.Members |> EqSet.forall isLiteralBearing
            | _ -> false

        // A `TyOr` annotation admits any subsuming actual (`let x: int | string = 1`); a
        // non-union one only a literal actual, widening OUTWARD (`let s: string = m()`).
        match resolveStep ctx.Store expected with
        | expected' when
            (match expected' with
             | TyOr _ -> true
             | _ -> isLiteralBearing actual)
            && subsumes ctx actual expected <> SubsumeOutcome.Unrelated
            ->
            ()
        // NOMINAL upcast: a concrete actual annotated to a strict SUPERTYPE
        // (`let toExn (e: InvalidOperationException) : exn = e`). STRICTLY `Subtype`, because
        // a same-nominal annotation grounds via `unify`.
        | _ when subsumes ctx actual expected = SubsumeOutcome.Subtype -> ()
        | _ -> unify ctx tok actual expected
