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
    /// filtering, unlike a union with a function / carried-node member, which is
    /// applicability-opaque.
    let isPureLiteralUnion (store: TypeStore) (ms: UnionMembers) : bool =
        ms.Members
        |> EqSet.forall (fun m ->
            match zonk store m with
            | TyLiteral _ -> true
            | _ -> false
        )

    /// The trial substitution `matchTypes` accumulates for ONE candidate: the candidate's own
    /// method typars keyed by Method-axis index (`M<'T>('T,'T)` opens to the same index at
    /// every position), and caller-side free metavars keyed by union-find root id.
    type private TrialBindings =
        {
            MethodTypars: Dictionary<int, SemType>
            CallerVars: Dictionary<TyVarId, SemType>
        }

        static member Create() =
            {
                MethodTypars = Dictionary()
                CallerVars = Dictionary()
            }

    /// The SOLE overload FILTER: `unify`'s concrete-type-constructor arm structure, except that an open
    /// method typar and a free metavar RECORD their binding in `binds` instead of matching
    /// unconditionally. Read-only w.r.t. the shared graph; no hierarchy walk enters here.
    let rec private matchTypes
        (store: TypeStore)
        (canon: SymbolKey -> SymbolKey)
        (binds: TrialBindings)
        (a: SemType)
        (b: SemType)
        : bool =
        match zonk store a, zonk store b with
        // A generic method's own typar binds to whatever it first meets and must AGREE at
        // every later occurrence — index equality across argument positions.
        | TyTypar(TyparAxis.Method, i), other
        | other, TyTypar(TyparAxis.Method, i) -> matchMethodTypar store canon binds i other
        // Applicability-OPAQUE, not bindable: their structural identity can't be decided until
        // a call site grounds them, so they stay "matches anything" and the commit seam decides.
        | (TyKeyOf _ | TyIndexedAccess _ | TyConditional _), _
        | _, (TyKeyOf _ | TyIndexedAccess _ | TyConditional _) -> true
        // A non-literal union parameter / a carried-node union argument stay opaque —
        // their members can carry a not-yet-ground node.
        | _, TyOr ms when not (isPureLiteralUnion store ms) -> true
        | TyOr ms, _ when EqSet.exists (hasCarriedNode store) ms.Members -> true
        | TyLiteral v1, TyLiteral v2 -> v1 = v2
        | TyConst(k1, xs), TyConst(k2, ys) -> k1 = k2 && EqArray.forall2 (matchTypes store canon binds) xs ys
        // A free metavar on EITHER side binds to the opposite type, or agrees if already bound.
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

    /// A method typar binds on first sight and must agree thereafter.
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

    /// A free caller metavar: `matchTypes`'s shallow `zonk` already consulted the shared graph, so
    /// a committed `Link` never reaches here and only `binds.CallerVars` is left. Bound ⇒
    /// recurse; unseen ⇒ record it. Two vars already unified in the graph need no new binding.
    and private matchVar
        (store: TypeStore)
        (canon: SymbolKey -> SymbolKey)
        (binds: TrialBindings)
        (tv: TyVarId)
        (other: SemType)
        : bool =
        let root = UnionFind.find store tv

        match binds.CallerVars.TryGetValue root.Id with
        | true, bound -> matchTypes store canon binds bound other
        | _ ->
            match other with
            | TyVar tv2 when UnionFind.find store tv2 = root -> true
            | _ ->
                binds.CallerVars.[root.Id] <- other
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

    /// Lift a boolean "at least as good" predicate to a three-way comparison: `+1` when `x`
    /// one-directionally dominates `y`, `-1` when `y` dominates `x`, `0` when they are
    /// mutually good (equal) or mutually incomparable.
    let private compareCond (p: 'T -> 'T -> bool) (x: 'T) (y: 'T) : int =
        match p x y, p y x with
        | true, false -> 1
        | false, true -> -1
        | _ -> 0

    /// `t1` beats `t2` (`+1`) when it is the MORE DERIVED of the two: `M(Base)` beats
    /// `M(obj)` for a `Derived` argument, since `Base` coerces to `obj` but not the
    /// reverse. `subsumes ctx a b` = `a` coerces to `b`, so `a` is the subtype and wins.
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

    /// A candidate abstracted to what the tier + betterness ranking needs: its value-parameter
    /// types (already declaring-typar-substituted for the call), its method typar arity, and an
    /// opaque identity handle recovered on a win. `ExternalMember` / `TypeMemberInfo` project in.
    type RankCandidate<'T> =
        {
            Params: SemType list
            MethodTyparArity: int
            Item: 'T
        }

    /// The picker's three-way verdict: `NoneApplicable` (no overload's parameters admit the
    /// arguments) vs `Ambiguous` (applicable set non-empty but no unique best — F#'s FS0041).
    /// `One` carries the sole winner.
    [<RequireQualifiedAccess>]
    type PickResult<'T> =
        | One of 'T
        | NoneApplicable
        | Ambiguous of 'T list

    /// Two ordered tiers — exact match, then subsumption — then betterness, over an ABSTRACT
    /// candidate. Subsumption is top-level PER ARGUMENT, never threaded structurally through
    /// `matchTypes`: v1 subtyping is invariant in type args. Callers apply the 1-candidate path.
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

        // A generic candidate's OWN where-constraints (`ExternalConstraint.Trait`/`Coercion`)
        // are NOT verified here: they are stamped on the fresh TyVars at the commit seam and
        // fire on the first `Link`, so a violated bound surfaces as a commit error instead.
        match filterTier false with
        // Exact-match tier: a single structural survivor wins with no betterness reasoning
        // (`Show(int)` / `Show(string)` needs no specificity).
        | [| only |] -> PickResult.One only.Item
        | _ ->
            match filterTier true with
            | [||] -> PickResult.NoneApplicable
            | [| only |] -> PickResult.One only.Item
            | many ->
                // `a` beats `b` when its argument list dominates element-wise, then — the only
                // tiebreaker beyond argument specificity — when it is non-generic and `b` is
                // generic. Not modelled: conversions, param arrays, out/optional, extensions.
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
    /// agnostic over any `ExternalMember[]` candidate set (callers pre-filter by static-ness).
    /// A lone candidate short-circuits the trial machinery; its mismatch surfaces at commit.
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

            // A provider member whose signature disagrees with its recorded `Key.ArgSig` arity
            // is rejected; for the projected candidate, `Params` length is the sole arity axis.
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
    // A project-local member reaches the same `rankCandidates` machinery; only the projection
    // into a `RankCandidate` differs (peel the member's `.Type` function type).

    /// Peel a member's (single-tupled) function type to its value parameters: `unit → r`
    /// is zero parameters, a single `TyTuple` domain flattens to its elements, any other
    /// single domain is one parameter. The by-VALUE analogue of `memberParamTypes`.
    let private flatParamsOf (store: TypeStore) (mty: SemType) : SemType list =
        let rec peelFuns t =
            match resolveStep store t with
            | TyFun(a, b) ->
                let ps, r = peelFuns b
                a :: ps, r
            | o -> [], o

        match peelFuns mty with
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
        (typeParams: EqArray<string * TyVarId>)
        (args: EqArray<SemType>)
        (m: TypeMemberInfo)
        : SemType list =
        flatParamsOf ctx.Store (instantiateMemberCall ctx (typeParams, args) m.EffectiveMethodTypars m.Type)

    /// Positional `TyVar root → axis index` map for a typar list, following any committed
    /// `Link`. Used to freeze a member's parameter typars back to `FTTypar(axis, i)`.
    let private frozenAxisEnv (store: TypeStore) (typars: EqArray<string * TyVarId>) : Dictionary<TyVarId, int> =
        let d = Dictionary<TyVarId, int>()

        for i in 0 .. typars.Length - 1 do
            let (_, ptv) = typars.[i]

            match zonk store (TyVar ptv) with
            | TyVar root ->
                if not (d.ContainsKey root) then
                    d.[root] <- i
            | _ -> ()

        d

    /// Freeze a user member's value-parameter `SemType`s into the declaring type's open typars
    /// (`FTTypar(TyparAxis.Declaring, i)`) and its own method typars — the same structural,
    /// call-site-independent form an external member's argSig takes. `Show(int)` → `[int]`.
    let freezeUserMemberArgSig
        (store: TypeStore)
        (declTypars: EqArray<string * TyVarId>)
        (m: TypeMemberInfo)
        : EqArray<FrozenType> =
        let declEnv = frozenAxisEnv store declTypars
        let methodEnv = frozenAxisEnv store m.EffectiveMethodTypars

        let onVar (v: SemType) : FrozenType =
            match v with
            | TyVar tv ->
                let root = UnionFind.find store tv

                match declEnv.TryGetValue root.Id with
                | true, i -> FTTypar(TyparAxis.Declaring, i)
                | _ ->
                    match methodEnv.TryGetValue root.Id with
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

    /// The frozen `MemberKey` identity of a resolved user member on declaring type `declKey`:
    /// the declaring-open argSig + real method-typar arity, so two same-name overloads mint
    /// DISTINCT keys. Recorded on the call node's side table and read back by Elaborate/Freeze.
    let frozenUserMemberKey
        (store: TypeStore)
        (declKey: TypeKey)
        (declTypars: EqArray<string * TyVarId>)
        (m: TypeMemberInfo)
        : SymbolKey =
        SymbolKeyOps.memberKey
            declKey
            m.Name
            (freezeUserMemberArgSig store declTypars m)
            m.EffectiveMethodTypars.Length
            (memberKindOf m)

    /// A member's overload-identity signature key for duplicate detection: name, static-ness,
    /// kind, frozen argSig and method-typar arity are the axes F#'s FS0438 collapses. A genuine
    /// overload (distinct param types / arity) mints a distinct key and coexists.
    let memberSignatureKey
        (store: TypeStore)
        (declTypars: EqArray<string * TyVarId>)
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

    /// The shared user-member resolver. Filters by name + static-ness, then: `NotOverloaded`
    /// for the 0/1-candidate case (the caller keeps its single-pick path, no trial); else
    /// `rankCandidates` over the value-parameter projection.
    let resolveMember
        (ctx: PassContext)
        (typeParams: EqArray<string * TyVarId>)
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
