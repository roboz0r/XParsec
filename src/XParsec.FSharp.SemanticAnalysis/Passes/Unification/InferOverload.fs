namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open Vesper
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationEngine
open UnificationTranslate
open UnificationSubsume

module UnificationInferOverload =

    /// A union whose every disjunct is a structural literal, so overload filtering can keep it
    /// by-VALUE, unlike a union with a function / carried-node disjunct, which is
    /// applicability-opaque.
    let isPureLiteralUnion (store: TypeStore) (ds: TyDisjuncts) : bool =
        ds.Disjuncts
        |> EqSet.forall (fun d ->
            match zonk store d with
            | TyLiteral _ -> true
            | _ -> false
        )

    /// The trial substitution `matchTypes` accumulates for ONE candidate: the candidate's own
    /// method typars keyed by their index (`M<'T>('T,'T)` opens to the same index at
    /// every position), and caller-side free metavars keyed by union-find root id.
    type private TrialBindings =
        {
            MethodTypars: Dictionary<int<typeSlot>, SemType>
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
        (canon: TypeKey -> TypeKey)
        (binds: TrialBindings)
        (a: SemType)
        (b: SemType)
        : bool =
        match zonk store a, zonk store b with
        // A generic method's own typar binds to whatever it first meets and must AGREE at
        // every later occurrence, matched by index across argument positions.
        | TyFunctionTypar i, other
        | other, TyFunctionTypar i -> matchMethodTypar store canon binds i other
        // Applicability-OPAQUE, not bindable: their structural identity can't be decided until
        // a call site grounds them, so they stay "matches anything" and the commit seam decides.
        | TyCarrier, _
        | _, TyCarrier -> true
        // A non-literal union parameter / a carried-node union argument stay opaque,
        // because their disjuncts can carry a not-yet-ground node.
        | _, TyOr ds when not (isPureLiteralUnion store ds) -> true
        | TyOr ds, _ when EqSet.exists (hasCarriedNode store) ds.Disjuncts -> true
        | TyLiteral v1, TyLiteral v2 -> v1 = v2
        | TyConst(k1, xs), TyConst(k2, ys) -> k1 = k2 && Block.forall2 (matchTypes store canon binds) xs ys
        // A free metavar on EITHER side binds to the opposite type, or agrees if already bound.
        | TyVar tv, other
        | other, TyVar tv -> matchVar store canon binds tv other
        | TyFun(a1, r1), TyFun(a2, r2) -> matchTypes store canon binds a1 a2 && matchTypes store canon binds r1 r2
        | TyTuple xs, TyTuple ys -> Block.forall2 (matchTypes store canon binds) xs ys
        | TyRecord(n1, xs), TyRecord(n2, ys)
        | TyUnion(n1, xs), TyUnion(n2, ys)
        | TyClass(n1, xs), TyClass(n2, ys) ->
            (n1 = n2 || canon n1 = canon n2)
            && Block.forall2 (matchTypes store canon binds) xs ys
        | _ -> false

    /// A method typar binds on first sight and must agree thereafter.
    and private matchMethodTypar
        (store: TypeStore)
        (canon: TypeKey -> TypeKey)
        (binds: TrialBindings)
        (i: int<typeSlot>)
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
        (canon: TypeKey -> TypeKey)
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

    /// One entry per DECLARED parameter: the instantiated signature peeled one `->` per argument
    /// group, each domain untupled to its width. The width comes from the FROZEN group, which
    /// is what tells a flattened 2-param group from a genuine single tuple param.
    let memberParamTypes (ctx: PassContext) (typeArgs: SemType[]) (m: ExternalMember) : SemType list =
        let widths = ExternalSignature.argGroupWidths m.Signature

        let rec peel (i: int) (ty: SemType) : SemType list =
            if i = widths.Length then
                []
            else
                match ty with
                | TyFun(domain, rest) ->
                    let here =
                        match widths.[i], domain with
                        | 0, _ -> []
                        | n, TyTuple elems when n >= 2 && elems.Length = n -> Block.toList elems
                        | _ -> [ domain ]

                    here @ peel (i + 1) rest
                | _ -> []

        peel 0 (zonk ctx.Store (ExternalSymbols.openSignature ctx m typeArgs))

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
            MethodTyparArity: int<typeSlot>
            Item: 'T
        }

    /// The picker's three-way verdict: `NoneApplicable` (no overload's parameters admit the
    /// arguments) vs `Ambiguous` (applicable set non-empty but no unique best, as in F#'s FS0041).
    /// `One` carries the sole winner.
    [<RequireQualifiedAccess>]
    type PickResult<'T> =
        | One of 'T
        | NoneApplicable
        | Ambiguous of 'T list

    /// Two ordered tiers (exact match, then subsumption), then betterness, over an ABSTRACT
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

        // A generic candidate's OWN where-constraints (its typars' `ConstraintSet`s)
        // are NOT verified here: they are stamped on the fresh TyVars at the commit seam and
        // fire on the first `Link`, so a violated constraint surfaces as a commit error instead.
        match filterTier false with
        // Exact-match tier: a single structural survivor wins with no betterness reasoning
        // (`Show(int)` / `Show(string)` needs no specificity).
        | [| only |] -> PickResult.One only.Item
        | _ ->
            match filterTier true with
            | [||] -> PickResult.NoneApplicable
            | [| only |] -> PickResult.One only.Item
            | many ->
                // `a` beats `b` when its argument list dominates element-wise, and failing that
                // when it is non-generic and `b` is generic, the only tiebreaker beyond argument
                // specificity. Not modelled: conversions, param arrays, out/optional, extensions.
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
                        compare (a.MethodTyparArity = 0<_>) (b.MethodTyparArity = 0<_>)

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
        (candidates: Block<ExternalMember>)
        (argElems: SemType list)
        : ExternalMember voption =
        match candidates with
        | BlockEmpty -> ValueNone
        | BlockOne only -> ValueSome only
        | _ ->
            let arity = List.length argElems

            // A provider member whose signature disagrees with its recorded `Key.ArgSig` arity
            // is rejected; for the projected candidate, `Params` length is the sole arity axis.
            let rcs =
                Block.toArray candidates
                |> Array.choose (fun m ->
                    let ps = memberParamTypes ctx typeArgs m

                    if m.Key.ArgSig.Length = arity && List.length ps = arity then
                        Some
                            {
                                Params = ps
                                MethodTyparArity = m.Signature.MethodTyparArity
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
            | TyTuple es -> Block.toList es
            | TyConst(k, a) when a.IsEmpty && k = RuntimeNames.unitKey -> []
            | o -> [ o ]
        | ps, _ -> ps

    /// The member's value-parameter types AT THE CALL SITE: its declaring typars
    /// substituted from the object argument's `args`, its own method typars freshened per call
    /// (so the trial matcher binds them like external `openSignature`'s method vars).
    let userMemberParams
        (ctx: PassContext)
        (typeParams: Block<DeclaredTypar>)
        (args: Block<SemType>)
        (m: TypeMemberInfo)
        : SemType list =
        flatParamsOf ctx.Store (instantiateMemberCall ctx (typeParams, args) m.EffectiveMethodTypars m.Type)

    /// Each type-kinded prototype's zonked root mapped to its `Types` slot, which is how a
    /// member's parameter typars freeze back to `FTTypar(scope, i)`.
    let private frozenScopeEnv (store: TypeStore) (typars: Block<DeclaredTypar>) : Dictionary<TyVarId, int<typeSlot>> =
        let d = Dictionary<TyVarId, int<typeSlot>>()

        DeclaredTypar.typeKinded typars
        |> Block.iteri (fun slot tp ->
            match zonk store (TyVar tp.TyVar) with
            | TyVar root ->
                if not (d.ContainsKey root) then
                    d.[root] <- slot
            | _ -> ()
        )

        d

    /// Freeze a user member's value-parameter `SemType`s into the declaring type's open typars
    /// (`FTTypar(Type declKey, i)`) and its own (`FTTypar(Member declKey, j)`), yielding the
    /// call-site-independent form of an external member's argSig. `Show(int)` → `[int]`.
    let freezeUserMemberArgSig
        (store: TypeStore)
        (declKey: TypeKey)
        (declTypars: Block<DeclaredTypar>)
        (m: TypeMemberInfo)
        : Block<FrozenType> =
        let declEnv = frozenScopeEnv store declTypars
        let methodEnv = frozenScopeEnv store m.EffectiveMethodTypars
        let ownScope = TyparScope.Member declKey

        // A metavar in NEITHER scope: not generic in anything the key can denote. It goes into
        // an argSig, which is a key, so every such position must freeze to the SAME value, or a
        // half-inferred signature mints a different key per inference run.
        let onVar (tv: TyVarId) : FrozenType =
            let root = UnionFind.find store tv

            match declEnv.TryGetValue root.Id with
            | true, i -> FTTypar(TyparScope.Type declKey, i)
            | _ ->
                match methodEnv.TryGetValue root.Id with
                | true, j -> FTTypar(ownScope, j)
                | _ -> FTUnknown UnknownReason.UnresolvedTypar

        Block.ofList
            [
                for p in flatParamsOf store (zonk store m.Type) -> FrozenTypeBridge.freezeWith store onVar p
            ]

    /// The kind-mapped `MemberKind` of a member (methods dispatch, properties store).
    let private memberKindOf (m: TypeMemberInfo) : MemberKind = TMemberKind.keyKind m.Kind

    /// The frozen `MemberKey` identity of a resolved user member on declaring type `declKey`:
    /// the declaring-open argSig + real method-typar arity, so two same-name overloads mint
    /// DISTINCT keys. Recorded on the call node's side table and read back by Elaborate/Freeze.
    let frozenUserMemberKey
        (store: TypeStore)
        (declKey: TypeKey)
        (declTypars: Block<DeclaredTypar>)
        (m: TypeMemberInfo)
        : MemberKey =
        SymbolKeyOps.memberKeyOf
            declKey
            m.Name
            (freezeUserMemberArgSig store declKey declTypars m)
            m.EffectiveMethodTypars.Length
            (memberKindOf m)

    /// `frozenUserMemberKey` for a member found on a nominal, minted against its declaration.
    let frozenNominalMemberKey (store: TypeStore) (nm: TypeRegistry.NominalMember) : MemberKey =
        frozenUserMemberKey store nm.Decl.TypeKey nm.Decl.TypeParams nm.Member

    /// A member's overload-identity signature key for duplicate detection: name, static-ness,
    /// kind, frozen argSig and method-typar arity are the axes F#'s FS0438 collapses. A genuine
    /// overload (distinct param types / arity) mints a distinct key and coexists.
    let memberSignatureKey
        (store: TypeStore)
        (declKey: TypeKey)
        (declTypars: Block<DeclaredTypar>)
        (m: TypeMemberInfo)
        : struct (string * bool * MemberKind * Block<FrozenType> * int) =
        struct (m.Name,
                m.IsStatic,
                memberKindOf m,
                freezeUserMemberArgSig store declKey declTypars m,
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
        (typeParams: Block<DeclaredTypar>)
        (args: Block<SemType>)
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
                        MethodTyparArity = DeclaredTypar.typeArity m.EffectiveMethodTypars
                        Item = m
                    }
                )

            match rankCandidates ctx rcs argElems with
            | PickResult.One m -> MemberPick.Resolved m
            | PickResult.NoneApplicable -> MemberPick.NoneApplicable
            | PickResult.Ambiguous ms -> MemberPick.Ambiguous ms

    // --- Project-local constructor overload resolution -----------------------
    // ONE catalogue and ONE pick, shared by `new T(args)`, the ctor-sugar application and
    // Elaborate's object-argument wrapping, so the three agree on which overload a
    // construction selected. Codegen's `EmitConstruct.pickLocalCtor` selects on the same two
    // axes over the emitted handles.

    /// Which constructor of a project-local class a construction selected, with the declared
    /// parameter types it was picked at, substituted for the call.
    [<RequireQualifiedAccess>]
    type LocalCtorPick =
        | Primary of parameters: SemType list
        | Secondary of parameters: SemType list

        member this.Parameters: SemType list =
            match this with
            | LocalCtorPick.Primary ps
            | LocalCtorPick.Secondary ps -> ps

    /// The constructor `argElems` selects on `info`: by ARITY, and where the class declares two
    /// of the same arity (`Shape(x: int)` beside `new(s: string)`), by argument type.
    /// `substitute` maps a declared parameter type into the call's instantiation. A same-arity
    /// set that no argument type separates — one argument still an unresolved metavar — falls
    /// to declaration order, which is the primary when the class declares one.
    /// `ValueNone` when the class declares no constructor of that arity.
    let pickLocalCtor
        (ctx: PassContext)
        (substitute: SemType -> SemType)
        (info: ClassTypeInfo)
        (argElems: SemType list)
        : LocalCtorPick voption =
        let arity = List.length argElems

        let ofArity (ps: SemType list) (mk: SemType list -> LocalCtorPick) : LocalCtorPick list =
            match List.length ps = arity with
            | true -> [ mk (List.map substitute ps) ]
            | false -> []

        // The primary first, then each secondary in declaration order: the catalogue
        // `FrozenSignature.ctorsOf` publishes and `NominalEmit` emits.
        let candidates =
            [
                if info.HasPrimaryCtor then
                    yield! ofArity [ for p in info.CtorParams -> p.Type ] LocalCtorPick.Primary

                for sc in info.Body.SecondaryCtors do
                    yield! ofArity [ for p in sc.Params -> p.Type ] LocalCtorPick.Secondary
            ]

        match candidates with
        | [] -> ValueNone
        | [ only ] -> ValueSome only
        | first :: _ ->
            let rcs =
                [|
                    for c in candidates ->
                        {
                            Params = c.Parameters
                            MethodTyparArity = 0<_>
                            Item = c
                        }
                |]

            match rankCandidates ctx rcs argElems with
            | PickResult.One c -> ValueSome c
            | PickResult.NoneApplicable
            | PickResult.Ambiguous _ -> ValueSome first

    /// A parameter-shape rendering for an overload diagnostic: each named type's simple name
    /// (`int`), and `_` for anything else.
    let showParams (ctx: PassContext) (ps: SemType list) : string =
        let one (t: SemType) =
            match zonk ctx.Store t with
            | TyConst(k, _) -> let (DisplayName n) = SymbolKeyOps.typeSimpleName k in n
            | TyClass(k, _)
            | TyRecord(k, _)
            | TyUnion(k, _) -> let (DisplayName n) = SymbolKeyOps.typeSimpleName k in n
            | _ -> "_"

        ps |> List.map one |> String.concat ", "

    [<NoEquality; NoComparison>]
    type private ChainCandidate =
        {
            Level: ChainLevel
            Member: TypeMemberInfo
        }

    /// `NotFound` is not `NoneApplicable`: no level declares the name at all.
    [<RequireQualifiedAccess>]
    type private ChainPick =
        | NotFound
        | Resolved of ChainCandidate
        | NoneApplicable
        | Ambiguous of ChainCandidate list

    /// The axes two levels must agree on to be declaring the SAME member, which is what F#'s
    /// hiding rule compares.
    [<Struct>]
    type private LevelSignature =
        {
            ArgSig: Block<FrozenType>
            MethodTyparArity: int
            Kind: MemberKind
        }

    /// A member's signature at ONE chain level, with that level's type arguments already
    /// applied: `Base<int>.get_Item : 'T -> _` and `Derived.get_Item : int -> _` both come
    /// out `[int]`. The member's own typars are written under `scope`, shared by every level,
    /// so generic members compare by index.
    let private levelSignature
        (ctx: PassContext)
        (scope: TyparScope)
        (level: ChainLevel)
        (m: TypeMemberInfo)
        : LevelSignature =
        let store = ctx.Store
        let methodEnv = frozenScopeEnv store m.EffectiveMethodTypars

        let onVar (tv: TyVarId) : FrozenType =
            match methodEnv.TryGetValue (UnionFind.find store tv).Id with
            | true, j -> FTTypar(scope, j)
            | _ -> FTUnknown UnknownReason.UnresolvedTypar

        let atLevel = instantiateMember store (level.TypeParams, level.Args) m.Type

        {
            ArgSig =
                Block.ofList
                    [
                        for p in flatParamsOf store (zonk store atLevel) -> FrozenTypeBridge.freezeWith store onVar p
                    ]
            MethodTyparArity = m.EffectiveMethodTypars.Length
            Kind = memberKindOf m
        }

    /// Resolve over EVERY level of an `inherit` chain, not the first that declares the name:
    /// `get_Item(string)` on the base survives `get_Item(int)` on the derived. Levels arrive
    /// most derived first, so dropping later duplicates of a signature applies F#'s hiding rule.
    let private resolveChainMember (ctx: PassContext) (levels: ChainLevel list) (argElems: SemType list) : ChainPick =
        let declared =
            [
                for level in levels do
                    for m in level.Candidates -> { Level = level; Member = m }
            ]

        let candidates =
            match levels with
            // A single level hides nothing, and the signature projection is not free.
            | []
            | [ _ ] -> declared
            | _ ->
                // A single scope shared across the levels, so `M<'a>('a)` on a base and on a
                // derived fold to one signature.
                let scope = TyparScope.Member (List.head levels).DeclKey
                declared |> List.distinctBy (fun c -> levelSignature ctx scope c.Level c.Member)

        match candidates with
        | [] -> ChainPick.NotFound
        // A lone candidate short-circuits the trial machinery; its mismatch surfaces at commit.
        | [ single ] -> ChainPick.Resolved single
        | cands ->
            let rcs =
                cands
                |> List.map (fun c ->
                    {
                        Params = userMemberParams ctx c.Level.TypeParams c.Level.Args c.Member
                        MethodTyparArity = DeclaredTypar.typeArity c.Member.EffectiveMethodTypars
                        Item = c
                    }
                )
                |> Array.ofList

            match rankCandidates ctx rcs argElems with
            | PickResult.One c -> ChainPick.Resolved c
            | PickResult.NoneApplicable -> ChainPick.NoneApplicable
            | PickResult.Ambiguous tied -> ChainPick.Ambiguous tied

    /// A resolved instance member at the DECLARING level's identity: `DeclKey` mints the call
    /// key, `DeclaringTy` the object argument's upcast.
    [<NoEquality; NoComparison>]
    type InstanceMember =
        {
            DeclKey: TypeKey
            DeclaringTy: SemType
            TypeParams: Block<DeclaredTypar>
            Member: TypeMemberInfo
            /// Instantiated for this call site.
            MemberTy: SemType
        }

    /// A resolved instance member, or the diagnostic its verdict earns. `NotFound` earns none:
    /// nothing declares the name, and every caller handles it with its own fall-through.
    [<RequireQualifiedAccess>]
    type InstanceMemberPick =
        | Resolved of InstanceMember
        | Unresolved of Kind
        | NotFound

    /// A NON-STATIC member of the object argument's type, resolved over its `inherit` chain.
    /// `argElems` fixes the arity and ranks the overloads. A name the chain declares ONCE
    /// resolves whatever they say, so a caller depending on arity re-checks the returned type.
    let pickInstanceMember
        (ctx: PassContext)
        (objArgTy: SemType)
        (memberName: string)
        (argElems: SemType list)
        : InstanceMemberPick =
        match resolveChainMember ctx (memberLevels ctx objArgTy memberName) argElems with
        | ChainPick.Resolved c ->
            InstanceMemberPick.Resolved
                {
                    DeclKey = c.Level.DeclKey
                    DeclaringTy = c.Level.DeclaringTy
                    TypeParams = c.Level.TypeParams
                    Member = c.Member
                    MemberTy = chainMemberTy ctx c.Level c.Member
                }
        | ChainPick.NotFound -> InstanceMemberPick.NotFound
        | ChainPick.NoneApplicable ->
            InstanceMemberPick.Unresolved(
                Kind.Message(
                    sprintf "No overload for '%s' takes the given arguments (%s)" memberName (showParams ctx argElems)
                )
            )
        | ChainPick.Ambiguous tied ->
            let candidates =
                tied
                |> List.map (fun c ->
                    sprintf
                        "%s(%s)"
                        memberName
                        (showParams ctx (userMemberParams ctx c.Level.TypeParams c.Level.Args c.Member))
                )
                |> String.concat "; "

            InstanceMemberPick.Unresolved(
                Kind.Message(sprintf "Ambiguous access to overloaded '%s'; candidates: %s" memberName candidates)
            )

    /// Pin the resolved member on its access node, so Elaborate reads both halves back rather
    /// than re-deriving either from the object argument's type.
    let stampInstanceMember (ctx: PassContext) (key: NodeKey) (m: InstanceMember) : unit =
        ctx.Resolution.LocalMemberCall.Set(
            key,
            {
                Key = frozenUserMemberKey ctx.Store m.DeclKey m.TypeParams m.Member
                DeclaringTy = m.DeclaringTy
            }
        )
