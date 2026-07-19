namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationEngine
open UnificationTranslate
open UnificationSubsume

module UnificationInferOverload =

    /// A union whose every member is a structural literal — kept by-VALUE in overload
    /// filtering (its `argSigOf` spelling is sharp), unlike a union with a function /
    /// carried-node member, which is applicability-opaque.
    let isPureLiteralUnion (store: TypeStore) (ms: UnionMembers) : bool =
        ms.Members
        |> EqSet.forall (fun m ->
            match zonk store m with
            | TyLiteral _ -> true
            | _ -> false
        )

    /// Does `t` carry a not-yet-ground type-level computation anywhere inside it?
    /// Only such a type is genuinely applicability-OPAQUE; a plain nominal /
    /// primitive union must NOT act as a filtering wildcard (it would perturb BCL
    /// overload sets — any union-typed argument would match every same-arity
    /// parameter of every overloaded external method).
    let rec hasCarriedNode (store: TypeStore) (t: SemType) : bool =
        match zonk store t with
        | TyKeyOf _
        | TyIndexedAccess _
        | TyConditional _ -> true
        | t -> SemType.existsChild (hasCarriedNode store) t

    /// The trial substitution accumulated by `matchTypes` for ONE candidate — the
    /// scratch that makes the applicability pre-check exact WITHOUT touching the
    /// shared graph. Two bindable leaves, each keyed to match its identity:
    ///   * `MethodTypars` — the candidate's OWN method typars, keyed by Method-axis
    ///     index. `M<'T>('T, 'T)` opens (`ExternalSymbols.openSignature`) to the SAME
    ///     `TyTypar(Method, 0)` at every position, so consistency across argument
    ///     positions is index equality — this is what forgetting per-position let the
    ///     over-accept bug through.
    ///   * `CallerVars` — the caller-side free metavars the trial touches, keyed by
    ///     union-find ROOT identity (`TypeVar` is a sealed class, so `Dictionary`
    ///     uses reference equality; `HashIdentity.Reference` states it).
    /// Local to the trial and DROPPED on failure. `matchTypes` writes only here — never
    /// `Link`/`union`/`drainAll` — so a failed trial leaves no residue in the shared
    /// union-find (all of `unify`'s mutation is confined to its metavar arms, `Engine.fs`,
    /// reachable only through a `Link` write, which this never performs).
    type private TrialBindings =
        {
            MethodTypars: Dictionary<int, SemType>
            CallerVars: Dictionary<TypeVar, SemType>
        }

        static member Create() =
            {
                MethodTypars = Dictionary()
                CallerVars = Dictionary(HashIdentity.Reference)
            }

    /// The bindings-accumulating structural matcher and the SOLE overload FILTER: the SAME
    /// arm structure as `unify`'s concrete-head arms (`Engine.fs`,
    /// `TyConst`/`TyRecord`/`TyUnion`/`TyClass`/`TyFun`/`TyTuple`/…), kept auditably parallel
    /// so a future reader can diff the two. The one difference is that the two arms which are
    /// genuinely BINDABLE — an open method typar and a free metavar — RECORD their binding in
    /// `binds` and check it for consistency, instead of returning `true` unconditionally. That
    /// fixes the over-accept (a shared `'T` now remembers its first binding) and the
    /// under-accept (a free caller `TyVar` against a concrete parameter now BINDS rather than
    /// falling to `| _ -> false`).
    ///
    /// The other wildcard arms STAY "matches anything": the carried type-level nodes and
    /// the non-pure / carried-node unions are applicability-OPAQUE, not bindable — their
    /// real admission is still the `unifyAppliedSig` commit seam. `matchTypes` itself stays
    /// pure-structural: no hierarchy walk enters this recursion. The applicable tier layers
    /// the WHOLE-TYPE `subsumes` test on TOP of it, per argument — v1 subtyping is invariant
    /// in type args, so subsumption cannot be threaded structurally through here.
    ///
    /// Read-only w.r.t. the shared graph. The head `zonk` follows any already-committed
    /// `Link` (subsuming `resolveStep`), so a bound caller var is resolved BEFORE the
    /// metavar arm; a still-free var reaches the arm and is looked up two-tier — the
    /// scratch `binds.CallerVars` after the graph — binding it if unseen.
    let rec private matchTypes
        (store: TypeStore)
        (canon: SymbolKey -> SymbolKey)
        (binds: TrialBindings)
        (a: SemType)
        (b: SemType)
        : bool =
        match zonk store a, zonk store b with
        // Binder arm (was the `-> true` wildcard): a generic method's own typar
        // (`TyTypar(Method, i)`) binds to whatever it first meets and must AGREE at every
        // later occurrence — index equality across positions.
        | TyTypar(TyparAxis.Method, i), other
        | other, TyTypar(TyparAxis.Method, i) -> matchMethodTypar store canon binds i other
        // Applicability-OPAQUE, not bindable: their structural identity can't be decided
        // until a call site grounds them, so they stay "matches anything" (the
        // `unifyAppliedSig` commit seam does the real work).
        | (TyKeyOf _ | TyIndexedAccess _ | TyConditional _), _
        | _, (TyKeyOf _ | TyIndexedAccess _ | TyConditional _) -> true
        // A non-literal union parameter / a carried-node union argument stay opaque —
        // their members can carry a not-yet-ground node.
        | _, TyOr ms when not (isPureLiteralUnion store ms) -> true
        | TyOr ms, _ when EqSet.exists (hasCarriedNode store) ms.Members -> true
        | TyLiteral v1, TyLiteral v2 -> v1 = v2
        | TyConst(k1, xs), TyConst(k2, ys) -> k1 = k2 && EqArray.forall2 (matchTypes store canon binds) xs ys
        // Binder arm (was `ReferenceEquals(find x, find y)`, which failed a free caller
        // var against a concrete parameter — the under-accept bug): a free metavar on
        // EITHER side binds to the opposite type, or agrees if already bound.
        | TyVar tv, other
        | other, TyVar tv -> matchVar store canon binds tv other
        | TyFun(a1, r1), TyFun(a2, r2) -> matchTypes store canon binds a1 a2 && matchTypes store canon binds r1 r2
        | TyTuple xs, TyTuple ys -> EqArray.forall2 (matchTypes store canon binds) xs ys
        | TyRecord(n1, xs), TyRecord(n2, ys)
        | TyUnion(n1, xs), TyUnion(n2, ys)
        | TyClass(n1, xs), TyClass(n2, ys) ->
            (n1 = n2 || canon (SymbolKey.Type n1) = canon (SymbolKey.Type n2))
            && EqArray.forall2 (matchTypes store canon binds) xs ys
        | _ -> false

    /// A method typar binds on first sight and must agree thereafter (index equality
    /// carries the shared-`'T` constraint the old per-position wildcard forgot).
    and private matchMethodTypar
        (store: TypeStore)
        (canon: SymbolKey -> SymbolKey)
        (binds: TrialBindings)
        (i: int)
        (other: SemType)
        : bool =
        match binds.MethodTypars.TryGetValue i with
        | true, bound -> matchTypes store canon binds bound other
        | _ ->
            binds.MethodTypars.[i] <- other
            true

    /// A free caller metavar: two-tier lookup — the shared graph was already consulted by
    /// `matchTypes`'s head `zonk` (so a committed `Link` never reaches here), leaving only
    /// the scratch `binds.CallerVars`. Bound ⇒ recurse against the binding; unseen ⇒ record
    /// it (writing ONLY the scratch dictionary). Two free vars already unified in the graph
    /// match with no new binding — the success case of the old `ReferenceEquals` arm.
    and private matchVar
        (store: TypeStore)
        (canon: SymbolKey -> SymbolKey)
        (binds: TrialBindings)
        (tv: TypeVar)
        (other: SemType)
        : bool =
        let root = UnionFind.find store tv

        match binds.CallerVars.TryGetValue root with
        | true, bound -> matchTypes store canon binds bound other
        | _ ->
            match other with
            | TyVar tv2 when System.Object.ReferenceEquals(UnionFind.find store tv2, root) -> true
            | _ ->
                binds.CallerVars.[root] <- other
                true

    /// Flattens the tupled signature back to N parameters. The `argSig` length — the
    /// member's own identity — distinguishes a flattened N-param method from a genuine
    /// single tuple param; the signature alone cannot.
    let memberParamTypes (store: TypeStore) (typeArgs: SemType[]) (m: ExternalMember) : SemType list =
        let n = m.Key.ArgSig.Length

        match zonk store (ExternalSymbols.openSignature m typeArgs) with
        | TyFun(TyTuple elems, _) when n >= 2 && elems.Length = n -> EqArray.toList elems
        | TyFun(TyUnit, _) when n = 0 -> []
        | TyFun(p, _) -> [ p ]
        | _ -> []

    /// Lift a boolean "at least as good" predicate to a three-way comparison (fsc's
    /// `compareCond`, ConstraintSolver.fs:3759): `+1` when `x` one-directionally dominates
    /// `y`, `-1` when `y` dominates `x`, `0` when they are mutually good (equal) or mutually
    /// incomparable. The sole ordering primitive the betterness ranking is built from.
    let private compareCond (p: 'T -> 'T -> bool) (x: 'T) (y: 'T) : int =
        match p x y, p y x with
        | true, false -> 1
        | false, true -> -1
        | _ -> 0

    /// `t1` beats `t2` (`+1`) when it is the MORE DERIVED of the two under `subsumes` —
    /// `M(Base)` beats `M(obj)` for a `Derived` argument, since `Base` coerces to `obj`
    /// but not the reverse. fsc's `compareTypes` (ConstraintSolver.fs:3762) over
    /// `TypeFeasiblySubsumesType`; here `subsumes ctx a b` is that feasibly-subsumes
    /// relation (`a` coerces to `b` ⇒ `a` is the subtype ⇒ the more derived, so it wins).
    /// The old `isObjectTy` special case is the degenerate `_ ≤ obj` instance and
    /// disappears into this.
    let private compareTypes (ctx: PassContext) (t1: SemType) (t2: SemType) : int =
        compareCond
            (fun a b ->
                match subsumes ctx a b with
                | SubsumeOutcome.Equal
                | SubsumeOutcome.Subtype -> true
                | SubsumeOutcome.Unrelated -> false
            )
            t1
            t2

    /// A candidate abstracted to exactly what the tier + betterness ranking needs — its
    /// value-parameter types (already declaring-typar-substituted for the call), its method
    /// typar arity, and an opaque identity handle recovered on a win. `ExternalMember` and
    /// `TypeMemberInfo` both project into it, so the ONE ranking rule serves the external
    /// vocabulary and user-declared members without a second copy.
    type RankCandidate<'T> =
        {
            Params: SemType list
            MethodTyparArity: int
            Item: 'T
        }

    /// The picker's three-way verdict, distinguishing the two call-site diagnostics the
    /// old `ValueNone` collapsed: `NoneApplicable` (no overload's parameters admit the
    /// arguments — fsc's `csMethodNotFound`) vs `Ambiguous` (applicable set non-empty but
    /// no unique best — fsc's FS0041). `One` carries the sole winner.
    [<RequireQualifiedAccess>]
    type PickResult<'T> =
        | One of 'T
        | NoneApplicable
        | Ambiguous of 'T list

    /// fsc's two ordered tiers (`ResolveOverloadingCore`) — exact match, then subsumption —
    /// then betterness, over an ABSTRACT candidate. The single home of the ranking rule
    /// (`pickBestOverload` and `resolveMember` both project into it). Assumes the caller
    /// already applied the single-candidate fast path; a lone survivor here is a filtering
    /// result, not the whole-set shortcut.
    ///
    /// One scratch substitution per candidate trial (`matchTypes`'s binder arms record a
    /// shared method typar / a free caller var so both stay consistent across positions),
    /// dropped when the `forall2` short-circuits false, so a rejected trial leaves no residue
    /// in the shared union-find. The exact tier passes `subtyping = false` (structural +
    /// binding only); the applicable tier adds the WHOLE-TYPE `subsumes` test per argument.
    /// `matchTypes`'s binder arms run FIRST (they are `TypeFeasiblySubsumesType`'s
    /// `TType_var` rule and must precede any hierarchy walk); v1 subtyping is invariant in
    /// type args (`subsumesNominal`), so the subsumption check is top-level per-argument,
    /// never threaded structurally through `matchTypes`.
    let rankCandidates (ctx: PassContext) (candidates: RankCandidate<'T>[]) (argElems: SemType list) : PickResult<'T> =
        let canon = capabilityCanonKey ctx
        let arity = List.length argElems

        let filterTier (subtyping: bool) =
            candidates
            |> Array.filter (fun c ->
                List.length c.Params = arity
                && (let binds = TrialBindings.Create()

                    List.forall2
                        (fun a p ->
                            matchTypes ctx.Store canon binds a p
                            || (subtyping && subsumes ctx a p <> SubsumeOutcome.Unrelated)
                        )
                        argElems
                        c.Params)
            )

        // A generic candidate's OWN where-constraints (`ExternalConstraint.Trait`/`Coercion`
        // — fsc's `TypesMustSubsume` instantiation check both tiers run regardless) are NOT
        // verified here: they are stamped on the fresh TyVars at the `unifyAppliedSig` commit
        // seam and fire `checkConstraint`/`subsumes` on the first `Link`. A candidate that
        // survives filtering but violates its own bound therefore surfaces as a commit error,
        // the same deferral the shared-typar over-accept takes.
        match filterTier false with
        // Exact-match tier: a single structural survivor wins with no betterness reasoning
        // (`Show(int)` / `Show(string)` needs no specificity). 0 or ≥2 survivors fall to the
        // applicable tier (a superset — `matchTypes || subsumes` ⊇ `matchTypes`).
        | [| only |] -> PickResult.One only.Item
        | _ ->
            match filterTier true with
            | [||] -> PickResult.NoneApplicable
            | [| only |] -> PickResult.One only.Item
            | many ->
                // `a` beats `b` when its argument list dominates element-wise under
                // `compareTypes`, then — the only tiebreaker we port beyond argument
                // specificity — when it is non-generic and `b` is generic (fsc's
                // `compare CalledTyArgs.IsEmpty`, ConstraintSolver.fs:3883). DEFERRED
                // tiebreakers, each ranking a feature we do not model: type-directed
                // conversions, param arrays, out/optional args, extension members,
                // `Func<_>`-beats-delegate, `T`-beats-`inref<T>`, `T`-beats-`Nullable<T>`.
                let compareCandidates (a: RankCandidate<'T>) (b: RankCandidate<'T>) : int =
                    let cmps = List.map2 (compareTypes ctx) a.Params b.Params

                    let argCmp =
                        if List.forall (fun c -> c >= 0) cmps && List.exists (fun c -> c > 0) cmps then
                            1
                        elif List.forall (fun c -> c <= 0) cmps && List.exists (fun c -> c < 0) cmps then
                            -1
                        else
                            0

                    if argCmp <> 0 then
                        argCmp
                    else
                        compare (a.MethodTyparArity = 0) (b.MethodTyparArity = 0)

                let best =
                    many
                    |> Array.filter (fun a ->
                        many
                        |> Array.forall (fun b -> System.Object.ReferenceEquals(a, b) || compareCandidates a b > 0)
                    )

                match best with
                | [| unique |] -> PickResult.One unique.Item
                | _ -> PickResult.Ambiguous [ for m in many -> m.Item ]

    /// `ValueNone` = none applicable, or no unique best (ambiguous). Static/instance/ctor
    /// agnostic over any `ExternalMember[]` candidate set (callers pre-filter by
    /// static-ness): arity, then the shared `rankCandidates` tiers + betterness. The
    /// single-candidate fast path (fsc ConstraintSolver.fs:3614) short-circuits before the
    /// trial machinery — a lone name/arity mismatch surfaces at the commit seam, matching
    /// the call sites, which decline to the single-pick path when a name has ≤1 overload.
    let pickBestOverload
        (ctx: PassContext)
        (typeArgs: SemType[])
        (candidates: ExternalMember[])
        (argElems: SemType list)
        : ExternalMember voption =
        match candidates with
        | [||] -> ValueNone
        | [| only |] -> ValueSome only
        | _ ->
            let arity = List.length argElems

            // The `Key.ArgSig.Length` guard is the external-only totality check (a malformed
            // provider member whose signature disagrees with its recorded arity is rejected);
            // for the projected candidate the length of `Params` is the sole arity axis.
            let rcs =
                candidates
                |> Array.choose (fun m ->
                    let ps = memberParamTypes ctx.Store typeArgs m

                    if m.Key.ArgSig.Length = arity && List.length ps = arity then
                        Some
                            {
                                Params = ps
                                MethodTyparArity = m.MethodTyparArity
                                Item = m
                            }
                    else
                        None
                )

            match rankCandidates ctx rcs argElems with
            | PickResult.One m -> ValueSome m
            | PickResult.NoneApplicable
            | PickResult.Ambiguous _ -> ValueNone

    // --- User-declared member overload resolution ---------------------------
    //
    // A project-local class / union / record member participates in the SAME
    // `rankCandidates` machinery as an external member: only the projection into a
    // `RankCandidate` differs (peel the member's `.Type` arrow spine rather than an
    // `ExternalSignature`), so the ranking rule is never copied.

    /// Peel a member's (single-tupled) arrow spine to its value parameters: `unit → r`
    /// is zero parameters, a single `TyTuple` domain flattens to its elements, any other
    /// single domain is one parameter. The by-VALUE analogue of `memberParamTypes`.
    let private flatParamsOf (store: TypeStore) (mty: SemType) : SemType list =
        let rec arrows t =
            match resolveStep store t with
            | TyFun(a, b) ->
                let ps, r = arrows b
                a :: ps, r
            | o -> [], o

        match arrows mty with
        | [ single ], _ ->
            match resolveStep store single with
            | TyTuple es -> EqArray.toList es
            | TyConst(k, a) when a.IsEmpty && k = RuntimeNames.unitKey -> []
            | o -> [ o ]
        | ps, _ -> ps

    /// The member's value-parameter types AT THE CALL SITE: its declaring typars
    /// substituted from the receiver's `args`, its own method typars freshened per call
    /// (so the trial matcher binds them like external `openSignature`'s method vars).
    let userMemberParams
        (ctx: PassContext)
        (typeParams: EqArray<string * TypeVar>)
        (args: EqArray<SemType>)
        (m: TypeMemberInfo)
        : SemType list =
        flatParamsOf ctx.Store (instantiateMemberCall ctx (typeParams, args) m.EffectiveMethodTypars m.Type)

    /// Positional `TyVar root → axis index` map for a typar list, following any committed
    /// `Link` (mirrors `Elaborate.mkTyparEnv`). Used to freeze a member's parameter typars
    /// back to their self-describing `FTTypar(axis, i)` placeholders.
    let private frozenAxisEnv (store: TypeStore) (typars: EqArray<string * TypeVar>) : Dictionary<TypeVar, int> =
        let d = Dictionary<TypeVar, int>(HashIdentity.Reference)

        for i in 0 .. typars.Length - 1 do
            let (_, ptv) = typars.[i]

            match zonk store (TyVar ptv) with
            | TyVar root ->
                if not (d.ContainsKey root) then
                    d.[root] <- i
            | _ -> ()

        d

    /// Freeze a user member's value-parameter `SemType`s into the declaring type's open
    /// typars (`FTTypar(Declaring, i)`) and its own method typars (`FTTypar(Method, j)`) —
    /// the SAME structural, call-site-independent form `ExternalSymbols.argSigOfParameters`
    /// mints for an external member. This is what makes the `MemberKey` a TOTAL overload
    /// identity: `Show(int)` and `Show(string)` freeze to `[int]` / `[string]` argSigs,
    /// distinct by construction.
    let freezeUserMemberArgSig
        (store: TypeStore)
        (declTypars: EqArray<string * TypeVar>)
        (m: TypeMemberInfo)
        : EqArray<FrozenType> =
        let declEnv = frozenAxisEnv store declTypars
        let methodEnv = frozenAxisEnv store m.EffectiveMethodTypars

        let onVar (v: SemType) : FrozenType =
            match v with
            | TyVar tv ->
                let root = UnionFind.find store tv

                match declEnv.TryGetValue root with
                | true, i -> FTTypar(TyparAxis.Declaring, i)
                | _ ->
                    match methodEnv.TryGetValue root with
                    | true, j -> FTTypar(TyparAxis.Method, j)
                    | _ -> FTUnknown ""
            | _ -> FTUnknown ""

        EqArray.ofList
            [
                for p in flatParamsOf store (zonk store m.Type) -> FrozenTypeBridge.toFrozenWith onVar (zonk store p)
            ]

    /// The kind-mapped `MemberKind` of a member (methods dispatch, properties store).
    let private memberKindOf (m: TypeMemberInfo) : MemberKind =
        match m.Kind with
        | ClassMemberKind.Property -> MemberKind.Property
        | ClassMemberKind.Method -> MemberKind.Method

    /// The TOTAL frozen `MemberKey` identity of a resolved user member on declaring type
    /// `declKey`: the frozen argSig (declaring-open) + real method-typar arity, so two
    /// same-name overloads mint DISTINCT keys. Recorded by inference on the overloaded
    /// call node's side table and read back verbatim by Elaborate/Freeze.
    let frozenUserMemberKey
        (store: TypeStore)
        (declKey: TypeKey)
        (declTypars: EqArray<string * TypeVar>)
        (m: TypeMemberInfo)
        : SymbolKey =
        SymbolKeyOps.memberKey
            declKey
            m.Name
            (freezeUserMemberArgSig store declTypars m)
            m.EffectiveMethodTypars.Length
            (memberKindOf m)

    /// A member's overload-identity signature key for duplicate detection: same name,
    /// static-ness, kind, `feasiblySubsumes`-identical parameter shape (the frozen argSig)
    /// and method-typar arity are the axes fsc's FS0438 collapses. A genuine overload
    /// (distinct param types / arity) mints a distinct key and coexists.
    let memberSignatureKey
        (store: TypeStore)
        (declTypars: EqArray<string * TypeVar>)
        (m: TypeMemberInfo)
        : struct (string * bool * MemberKind * EqArray<FrozenType> * int) =
        struct (m.Name,
                m.IsStatic,
                memberKindOf m,
                freezeUserMemberArgSig store declTypars m,
                m.EffectiveMethodTypars.Length)

    /// The user-member resolution verdict. `NotOverloaded` (0/1 candidate) tells the caller
    /// to keep its single-pick path unchanged; the other three mirror `PickResult`.
    [<RequireQualifiedAccess>]
    type MemberPick =
        | NotOverloaded
        | Resolved of TypeMemberInfo
        | NoneApplicable
        | Ambiguous of TypeMemberInfo list

    /// The shared user-member resolver, routed through the genuine method-call site (the
    /// call-seam probe). Filters by name + static-ness, then: `NotOverloaded` for the 0/1
    /// candidate case (the caller keeps its single-pick path — the vast majority, no
    /// trial); else the generalised `rankCandidates` picker over the value-parameter
    /// projection, distinguishing the ambiguous and no-applicable verdicts so the two
    /// call-site diagnostics stay separate.
    let resolveMember
        (ctx: PassContext)
        (typeParams: EqArray<string * TypeVar>)
        (args: EqArray<SemType>)
        (members: TypeMemberInfo[])
        (memberName: string)
        (isStatic: bool)
        (argElems: SemType list)
        : MemberPick =
        match members |> Array.filter (fun m -> m.Name = memberName && m.IsStatic = isStatic) with
        | [||]
        | [| _ |] -> MemberPick.NotOverloaded
        | cands ->
            let rcs =
                cands
                |> Array.map (fun m ->
                    {
                        Params = userMemberParams ctx typeParams args m
                        MethodTyparArity = m.EffectiveMethodTypars.Length
                        Item = m
                    }
                )

            match rankCandidates ctx rcs argElems with
            | PickResult.One m -> MemberPick.Resolved m
            | PickResult.NoneApplicable -> MemberPick.NoneApplicable
            | PickResult.Ambiguous ms -> MemberPick.Ambiguous ms
