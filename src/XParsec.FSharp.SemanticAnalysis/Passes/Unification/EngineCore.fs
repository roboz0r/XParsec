namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis


/// The unification SUBSTRATE: resolution/zonking, occurs/level adjustment, substitution
/// + member instantiation, and the nominal subtype walks.
module UnificationEngineCore =

    /// One level deep, so full resolution means calling recursively. Stops at a measure-bearing
    /// root: following `Link` through to the bare carrier would drop the measure.
    let resolveStep (store: TypeStore) (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = UnionFind.find store tv

            match store.Link root with
            | ValueSome t' when (store.Units root).IsNone -> t'
            | _ -> TyVar root.Id
        | _ -> t

    /// Drop the `null` member from every anonymous union in `t`, collapsing a resulting
    /// singleton: `obj | null` → `obj`, the same `System.Object` slot on the CLR ABI.
    let rec stripReferenceNull (store: TypeStore) (t: SemType) : SemType =
        // Resolve at each node: an annotated `objnull` param can arrive behind a `TyVar`
        // Link, and `mapChildren` treats a `TyVar` as a leaf, so a raw walk misses the union.
        match resolveStep store t with
        | TyOr ds ->
            ds.Disjuncts
            |> EqSet.toList
            |> List.filter (
                function
                | TyNull -> false
                | _ -> true
            )
            |> List.map (stripReferenceNull store)
            |> mkUnion
        | resolved -> SemType.mapChildren (stripReferenceNull store) resolved

    /// Fully resolve a SemType: walk all TyVar chains AND recurse into compound shapes. A
    /// measure-bearing TyVar survives as a TyVar, so a consumer can still read the measure.
    let rec zonk (store: TypeStore) (t: SemType) : SemType =
        match t with
        | TyVar _ ->
            match UnionFind.zonkShallow store t with
            | TyVar _ as v -> v
            | resolved -> zonk store resolved
        | t -> SemType.mapChildren (zonk store) t

    let argElemsOf (store: TypeStore) (argTy: SemType) : SemType list =
        match zonk store argTy with
        | TyTuple xs -> EqArray.toList xs
        | TyUnit -> []
        | single -> [ single ]

    let argArityOf (store: TypeStore) (argTy: SemType) : int =
        match resolveStep store argTy with
        | TyTuple xs -> xs.Length
        | TyUnit -> 0
        | _ -> 1

    let tupleOrSingle (intrinsics: IntrinsicSet) (paramTys: SemType list) : SemType =
        match paramTys with
        | [] -> intrinsics.Unit
        | [ t ] -> t
        | many -> TyTuple(EqArray.ofList many)

    /// One walk, two jobs. Occurs check: does `target` (a union-find root) appear inside
    /// `t`, which would cycle Link pointers and make zonk loop (`let rec f x = f`)? And,
    /// since linking `target` to `t` co-scopes them, lower every level above `target`'s.
    let rec occursAndAdjust (store: TypeStore) (target: TyVarId) (t: SemType) : bool =
        match resolveStep store t with
        | TyVar tv ->
            let root = UnionFind.find store tv

            if root.Id = target then
                true
            else
                let targetRoot = UnionFind.find store target

                if store.Level root > store.Level targetRoot then
                    store.SetLevel(root, store.Level targetRoot)

                false
        | t -> SemType.existsChild (occursAndAdjust store target) t

    /// Two non-equal measures emit a diagnostic; one of them is kept on the
    /// survivor so further unifications against it stay coherent.
    let mergeUnits
        (ctx: PassContext)
        (tok: SyntaxToken)
        (newRoot: Rep)
        (unitsA: MeasureTerm voption)
        (unitsB: MeasureTerm voption)
        : unit =
        match unitsA, unitsB with
        | ValueNone, ValueNone -> ()
        | ValueSome m, ValueNone
        | ValueNone, ValueSome m -> ctx.Store.SetUnits(newRoot, ValueSome m)
        | ValueSome m1, ValueSome m2 when m1.Equals(m2) -> ctx.Store.SetUnits(newRoot, ValueSome m1)
        | ValueSome m1, ValueSome m2 ->
            ctx.Store.SetUnits(newRoot, ValueSome m1)

            ctx.Report(tok, Kind.MeasureMismatch(string m1, string m2))

    /// Replace TyVar roots that key `subst` with their target, recursing into compound
    /// shapes. A non-key TyVar resolves through its `Link`, else stays as its own root.
    let rec substituteWith (store: TypeStore) (subst: Dictionary<TyVarId, SemType>) (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = UnionFind.find store tv

            match subst.TryGetValue root.Id with
            | true, target -> target
            | false, _ ->
                // A registry field / case-arg type is a *placeholder* TyVar, never itself a
                // `subst` key (keys are the declared type's `TypeParams`), so follow `Link`
                // to reach one. Measure-bearing roots stop.
                match store.Link root with
                | ValueSome target when (store.Units root).IsNone -> substituteWith store subst target
                | _ -> TyVar root.Id
        | t -> SemType.mapChildren (substituteWith store subst) t

    /// Empty when the lengths don't match, so an arity mismatch leaves the field types
    /// unsubstituted rather than silently pairing the wrong ones.
    let mkNamedTypeSubst
        (store: TypeStore)
        (typeParams: EqArray<string * TyVarId>)
        (args: EqArray<SemType>)
        : Dictionary<TyVarId, SemType> =
        let subst = Dictionary<TyVarId, SemType>()

        if typeParams.Length = args.Length then
            let mutable i = 0

            for (_, tp) in typeParams do
                subst.[(UnionFind.find store tp).Id] <- args.[i]
                i <- i + 1

        subst

    let instantiateMember
        (store: TypeStore)
        (typeParams: EqArray<string * TyVarId>, args: EqArray<SemType>)
        (ty: SemType)
        : SemType =
        substituteWith store (mkNamedTypeSubst store typeParams args) ty

    /// Prepend `c` unless a constraint of the same `Kind` is already on `tv`, so a use site
    /// never accumulates duplicate SRTP / equality bounds.
    let addConstraintByKind (store: TypeStore) (tv: TyVarId) (c: SemanticConstraint) : unit =
        let root = UnionFind.find store tv

        if not (store.Constraints.Items root |> List.exists (fun e -> e.Kind = c.Kind)) then
            store.Constraints.Prepend(root, c)

    /// Mint a fresh instance TyVar at the current level carrying a deduped copy of
    /// `constraints`, so a use site re-evaluates them against its own substitution.
    let freshConstrainedTyVar (ctx: PassContext) (constraints: SemanticConstraint list) : TyVarId =
        let fresh = ctx.NewTypeVar()
        ctx.Store.SetLevel(UnionFind.find ctx.Store fresh, ctx.CurrentLevel)

        for c in constraints do
            addConstraintByKind ctx.Store fresh c

        fresh

    /// Instantiate a member's type for a CALL SITE: the declaring-axis substitution
    /// (`typeParams ↦ args`) plus a fresh `TyVar` at the current level for each of the
    /// member's OWN `methodTypars`, so one call site cannot ground the shared prototype.
    let instantiateMemberCall
        (ctx: PassContext)
        (typeParams: EqArray<string * TyVarId>, args: EqArray<SemType>)
        (methodTypars: EqArray<string * TyVarId>)
        (ty: SemType)
        : SemType =
        let subst = mkNamedTypeSubst ctx.Store typeParams args

        for (_, ptv) in methodTypars do
            let root = UnionFind.find ctx.Store ptv

            if (ctx.Store.Link root).IsNone && not (subst.ContainsKey root.Id) then
                subst.[root.Id] <- TyVar(freshConstrainedTyVar ctx (ctx.Store.Constraints.Items root))

        substituteWith ctx.Store subst ty

    /// A project-local class / union / record's OWN instance member, instantiated at the
    /// object argument's `args`.
    let tryLocalInstanceMember (ctx: PassContext) (objArgTy: SemType) (memberName: string) : SemType voption =
        match resolveStep ctx.Store objArgTy with
        | TyNominal(typeKey, args) ->
            match TypeRegistry.tryNominalByKey ctx.Types typeKey with
            | ValueSome decl ->
                match decl.Members |> Array.tryFind (fun m -> m.Name = memberName && not m.IsStatic) with
                | Some m -> ValueSome(instantiateMemberCall ctx (decl.TypeParams, args) m.EffectiveMethodTypars m.Type)
                | None -> ValueNone
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// The chain walk's result: the DECLARING class's instantiated nominal type, at the
    /// level the member was found, plus the member's type instantiated against that
    /// level's args.
    [<Struct>]
    type ChainMember =
        {
            DeclaringTy: SemType
            MemberTy: SemType
        }

    /// One level of an `inherit` chain that declares a name, at that level's own type args:
    /// the identity a member found here is keyed and called on.
    [<NoEquality; NoComparison>]
    type ChainLevel =
        {
            DeclKey: TypeKey
            DeclaringTy: SemType
            TypeParams: EqArray<string * TyVarId>
            Args: EqArray<SemType>
            /// Non-static, in declaration order; never empty.
            Candidates: TypeMemberInfo[]
        }

    /// Every level of a class's `inherit` chain declaring a NON-STATIC `memberName`, MOST
    /// DERIVED FIRST. `BaseType` is written in the DERIVED class's typar scope, so applying
    /// `TypeParams ↦ args` to it threads the derived args up to the parent.
    let classChainLevels
        (ctx: PassContext)
        (clsKey: TypeKey)
        (args: EqArray<SemType>)
        (memberName: string)
        : ChainLevel list =
        let seen = HashSet<TypeKey>()
        let levels = ResizeArray<ChainLevel>()

        let rec walk (clsKey: TypeKey) (args: EqArray<SemType>) : unit =
            if seen.Add clsKey then
                match TypeRegistry.tryClassByKey ctx.Types clsKey with
                | ValueSome info ->
                    match info.Members |> Array.filter (fun m -> m.Name = memberName && not m.IsStatic) with
                    | [||] -> ()
                    | candidates ->
                        levels.Add
                            {
                                DeclKey = info.TypeKey
                                DeclaringTy = TyClass(info.TypeKey, args)
                                TypeParams = info.TypeParams
                                Args = args
                                Candidates = candidates
                            }

                    match info.Base with
                    | ValueSome inh ->
                        match
                            resolveStep
                                ctx.Store
                                (instantiateMember ctx.Store (info.TypeParams, args) (BaseParent.ty inh.Parent))
                        with
                        | TyClass(parentKey, parentArgs) -> walk parentKey parentArgs
                        | _ -> ()
                    | ValueNone -> ()
                | ValueNone -> ()

        walk clsKey args
        List.ofSeq levels

    /// Every level of an object argument's declaration that declares a NON-STATIC `memberName`,
    /// MOST DERIVED FIRST: the `inherit` chain for a class, the single declaration for a union
    /// or record, which have no chain to walk. Empty ⇒ nothing project-local declares the name.
    let memberLevels (ctx: PassContext) (objArgTy: SemType) (memberName: string) : ChainLevel list =
        // `TyNominal` matches a class too, so the chain arm must come first.
        match resolveStep ctx.Store objArgTy with
        | TyClass(clsKey, args) -> classChainLevels ctx clsKey args memberName
        | TyNominal(typeKey, args) as selfTy ->
            match TypeRegistry.tryNominalByKey ctx.Types typeKey with
            | ValueSome decl ->
                match decl.Members |> Array.filter (fun m -> m.Name = memberName && not m.IsStatic) with
                | [||] -> []
                | candidates ->
                    [
                        {
                            DeclKey = decl.TypeKey
                            DeclaringTy = selfTy
                            TypeParams = decl.TypeParams
                            Args = args
                            Candidates = candidates
                        }
                    ]
            | ValueNone -> []
        | _ -> []

    /// The member type at one level, instantiated for a call against that level's args.
    let chainMemberTy (ctx: PassContext) (level: ChainLevel) (m: TypeMemberInfo) : SemType =
        instantiateMemberCall ctx (level.TypeParams, level.Args) m.EffectiveMethodTypars m.Type

    /// The first declaration of `memberName` up the chain, for a caller with no arguments to
    /// discriminate on. An overloaded name needs the ranker instead.
    let tryClassChainMemberDecl
        (ctx: PassContext)
        (clsKey: TypeKey)
        (args: EqArray<SemType>)
        (memberName: string)
        : ChainMember voption =
        match classChainLevels ctx clsKey args memberName with
        | [] -> ValueNone
        | level :: _ ->
            let m = level.Candidates.[0]

            ValueSome
                {
                    DeclaringTy = level.DeclaringTy
                    MemberTy = chainMemberTy ctx level m
                }

    let tryClassChainMember
        (ctx: PassContext)
        (clsKey: TypeKey)
        (args: EqArray<SemType>)
        (memberName: string)
        : SemType voption =
        match tryClassChainMemberDecl ctx clsKey args memberName with
        | ValueSome cm -> ValueSome cm.MemberTy
        | ValueNone -> ValueNone

    /// A class's own explicit `val x: T` instance field of that name, instantiated at `args`.
    let private tryClassInstanceField
        (ctx: PassContext)
        (info: ClassTypeInfo)
        (args: EqArray<SemType>)
        (memberName: string)
        : SemType voption =
        match info.InstanceFields |> Array.tryFind (fun f -> f.Name = memberName) with
        | Some fld -> ValueSome(instantiateMember ctx.Store (info.TypeParams, args) fld.Type)
        | None -> ValueNone

    /// `memberName` on a project-local class object argument: the `inherit` chain's members,
    /// then the class's own explicit `val x: T` instance fields, instantiated at `args`. This
    /// is what a `.member` access resolves against whether the object argument's type was known
    /// at the access or only settled later.
    let tryClassChainMemberOrFieldOf
        (ctx: PassContext)
        (info: ClassTypeInfo)
        (args: EqArray<SemType>)
        (memberName: string)
        : SemType voption =
        match tryClassChainMember ctx info.TypeKey args memberName with
        | ValueSome ty -> ValueSome ty
        | ValueNone -> tryClassInstanceField ctx info args memberName

    /// `tryClassChainMemberOrFieldOf` for a caller holding only the key.
    let tryClassChainMemberOrField
        (ctx: PassContext)
        (clsKey: TypeKey)
        (args: EqArray<SemType>)
        (memberName: string)
        : SemType voption =
        match tryClassChainMember ctx clsKey args memberName with
        | ValueSome ty -> ValueSome ty
        | ValueNone ->
            match TypeRegistry.tryClassByKey ctx.Types clsKey with
            | ValueSome info -> tryClassInstanceField ctx info args memberName
            | ValueNone -> ValueNone

    /// The flat `FunN` arity a matched `Fun`(k+1)` interface instantiation denotes.
    /// `Vesper.Fun`2<'A,'B>` through `Vesper.Fun`5<'A,'B,'C,'D,'E>` share one name and
    /// differ only by arity, so the arity is half the identity. Matched by KEY: a user
    /// interface named `Fun` in its own namespace is not a function slot.
    let funSlotArityOfArgs (tyCtor: TypeKey) (genericArity: int) : int option =
        if
            genericArity >= 2
            && genericArity <= 5
            && tyCtor = RuntimeNames.vesperFunKey genericArity
        then
            Some(genericArity - 1)
        else
            None

    /// Peel `k` domains off the `TyFun(a, b)` chain into the `k+1` types
    /// `[dom0; …; dom_{k-1}; residualCodomain]` aligned to a `Fun`(k+1)`'s type args, or
    /// `None` if the chain is too short. The residual codomain is returned WHOLE.
    let private peelFunDomains (store: TypeStore) (k: int) (a: SemType) (b: SemType) : SemType list option =
        let rec go i (dom: SemType) (cod: SemType) (acc: SemType list) =
            let acc = dom :: acc

            if i = k - 1 then
                Some(List.rev (cod :: acc))
            else
                match resolveStep store cod with
                | TyFun(d, c) -> go (i + 1) d c acc
                | _ -> None

        go 0 a b []

    /// The `k+1` types of the `TyFun(a, b)` chain aligned to `targs`, when `tyCtor`/`targs`
    /// instantiate a `Fun`(k+1)` slot and the chain is long enough. `ValueNone` for any other
    /// nominal, and for a chain shorter than the slot's arity.
    let tryFunSlotPeel
        (store: TypeStore)
        (tyCtor: TypeKey)
        (targs: EqArray<SemType>)
        (a: SemType)
        (b: SemType)
        : SemType list voption =
        match funSlotArityOfArgs tyCtor targs.Length with
        | Some k ->
            match peelFunDomains store k a b with
            | Some tys -> ValueSome tys
            | None -> ValueNone
        | None -> ValueNone

    /// `keyOf` applied to the capability that `key` spells under either of its two nominal
    /// names; any other key passes through. Allocation-free: `keyOf` is inlined at each site.
    let inline private capabilityKeyBy
        (ctx: PassContext)
        ([<InlineIfLambda>] keyOf: RuntimeNames.CapabilityIdentity -> TypeKey)
        (key: TypeKey)
        : TypeKey =
        let caps = ctx.CapabilityIds

        let inline pick (cap: RuntimeNames.CapabilityIdentity voption) : TypeKey voption =
            match cap with
            | ValueSome c when c.Matches key -> ValueSome(keyOf c)
            | _ -> ValueNone

        match pick caps.Enumerable with
        | ValueSome k -> k
        | ValueNone ->

            match pick caps.Enumerator with
            | ValueSome k -> k
            | ValueNone ->

                match pick caps.Disposable with
                | ValueSome k -> k
                | ValueNone ->

                    match pick caps.Equatable with
                    | ValueSome k -> k
                    | ValueNone ->

                        match pick caps.Comparable with
                        | ValueSome k -> k
                        | ValueNone -> key

    /// Fold a capability interface's two nominal keys to the canonical one: its BCL platform
    /// key (`System.Collections.Generic.IEnumerable\`1`) and its canonical key
    /// (`Vesper.Collections.seq`). Any other key passes through.
    let capabilityCanonKey (ctx: PassContext) (key: TypeKey) : TypeKey =
        capabilityKeyBy ctx (fun c -> ValueOption.defaultValue c.Key c.CanonKey) key

    /// The mirror fold, to a capability's PLATFORM key
    /// (`Vesper.Collections.enumerator\`1` → `System.Collections.Generic.IEnumerator\`1`).
    /// MEMBER LOOKUP only: the canonical shape carries no member table, the platform's does.
    let capabilityPlatformKey (ctx: PassContext) (key: TypeKey) : TypeKey =
        capabilityKeyBy ctx (fun c -> c.Key) key

    /// Do two nominal keys denote the same type, reconciling a capability's two names? For
    /// key-EQUALITY seams only, never inside the base/interface-chain LOOKUPS: rewriting a
    /// BCL platform key there erases its own bases (`IEnumerator\`1 :> IEnumerator`).
    let sameNominalKey (ctx: PassContext) (k1: TypeKey) (k2: TypeKey) : bool =
        k1 = k2 || capabilityCanonKey ctx k1 = capabilityCanonKey ctx k2

    // Canonical nominal IDENTITY for subtype comparison: the platform-INVARIANT front-end
    // `SymbolKey` (`Vesper.int`, `Vesper.exn`), looked up BY KEY, never by projected name.
    let private canonKey (ctx: PassContext) (key: TypeKey) : TypeKey =
        match ctx.IntrinsicCanonCache.TryGetValue key with
        | true, canon -> canon
        | _ ->
            let canon =
                if ctx.Types.IntrinsicReprKeys.ContainsKey key then
                    key
                else
                    match ctx.Provider.TryLookupType key with
                    | ValueSome(ExternalTypeShape.Intrinsic { Id = { Canon = canon } }) -> canon
                    | _ -> key

            ctx.IntrinsicCanonCache.[key] <- canon
            canon

    /// The PLATFORM name of an intrinsic: the runtime repr its `(# "…" #)` binding records
    /// (`"string"` ⇒ `"System.String"` on CLR). Falls back to the key's own identity name for
    /// a non-intrinsic, or an intrinsic with no repr on the compiling target (`decimal` on JS).
    let intrinsicPlatformName (ctx: PassContext) (key: TypeKey) : string =
        match IntrinsicTypeMap.tryPlatformRepr key ctx.IntrinsicTypeMap.Value with
        | ValueSome platform -> platform
        | ValueNone -> key.Name

    /// The external `(SymbolKey, typeArgs)` surfaces a provider member lookup keys on, MOST
    /// SPECIFIC FIRST: an intrinsic `TyConst` publishes its own contract surface, then the
    /// platform type's (`"hello".TryCopyTo` reaching `System.String`). Empty for a local class.
    let externalSurfaceKeys (ctx: PassContext) (ty: SemType) : struct (TypeKey * EqArray<SemType>) list =
        match resolveStep ctx.Store ty with
        | TyClass(clsKey, typeArgs) when (TypeRegistry.tryClassByKey ctx.Types clsKey).IsNone ->
            [ struct (clsKey, typeArgs) ]
        // A structural constructor (`'T []` / `byref`) reprs as the IL artefact `"!0[]"`, which
        // is not a nominal surface. Its own contract key is the only one to look a member up
        // on, and is where the array declares `Item` and `Length`.
        | TyStructuralCtor & TyConst(key, typeArgs) -> [ struct (key, typeArgs) ]
        | TyConst(key, typeArgs) ->
            let name = key.Name
            let platformQual = intrinsicPlatformName ctx key

            [
                struct (key, typeArgs)

                if platformQual <> name then
                    struct (SymbolKeyOps.qualifiedTypeKeyOf platformQual 0, typeArgs)
            ]
        | _ -> []

    // Surface a nominal `(canonKey, args)` for the comparison, covering `TyConst` (so the
    // `exn` bound participates) as well as `TyClass`. The canonical intrinsic identity, so
    // two spellings of one intrinsic compare equal by `=`.
    let subtypeNominalOf (ctx: PassContext) (ty: SemType) : struct (TypeKey * EqArray<SemType>) voption =
        match resolveStep ctx.Store ty with
        | TyClass(n, args) -> ValueSome(struct (canonKey ctx n, args))
        // A named DU or record enters the walk too, so its `interface … with` impls admit
        // a `:>` exactly like a class's. (Anonymous `TyOr` unions resolve structurally.)
        | TyUnion(n, args) -> ValueSome(struct (canonKey ctx n, args))
        | TyRecord(n, args) -> ValueSome(struct (canonKey ctx n, args))
        | TyConst(key, args) -> ValueSome(struct (canonKey ctx key, args))
        | _ -> ValueNone

    let private nominalKeyOf (store: TypeStore) (ty: SemType) : TypeKey voption =
        match resolveStep store ty with
        | TyClass(k, _)
        | TyUnion(k, _)
        | TyRecord(k, _) -> ValueSome k
        | _ -> ValueNone

    // The instantiated declared base of the nominal the walk is expanding. Either tier's
    // `BaseType` is written over the declaring typars, so `args` substitutes into it.
    let private subtypeParentOf
        (ctx: PassContext)
        (localKey: TypeKey voption)
        (key: TypeKey)
        (args: EqArray<SemType>)
        : SemType voption =
        let localInfo =
            match localKey with
            | ValueSome k -> TypeRegistry.tryClassByKey ctx.Types k
            | ValueNone -> ValueNone

        match localInfo with
        | ValueSome info ->
            match info.Base with
            | ValueSome inh -> ValueSome(instantiateMember ctx.Store (info.TypeParams, args) (BaseParent.ty inh.Parent))
            | ValueNone -> ValueNone
        | ValueNone ->
            match ctx.Provider.TryLookupType key with
            | ValueSome(ExternalTypeShape.Class shape) ->
                ExternalSymbols.instantiateBaseType shape (args.AsSpan().ToArray())
            // A primitive's declared `inherit` parent, so the subtype walk continues
            // `exn → obj → ⊥` off the contract chain. Only a heritable primitive declares one.
            | ValueSome(ExternalTypeShape.Intrinsic { Class = ValueSome surface }) ->
                ExternalSymbols.instantiateBaseTypeFrozen surface.BaseType (args.AsSpan().ToArray())
            | _ -> ValueNone

    // The interfaces a nominal declares, as instantiated nominal `SemType`s. The metadata
    // provider pre-flattens the transitive set but the TS-manifest one does not, so a
    // caller must recurse THROUGH each surfaced one.
    let private subtypeInterfacesOf
        (ctx: PassContext)
        (localKey: TypeKey voption)
        (key: TypeKey)
        (args: EqArray<SemType>)
        : SemType list =
        let localHost =
            match localKey with
            | ValueSome k -> TypeRegistry.tryInterfaceImplHostByKey ctx.Types k
            | ValueNone -> ValueNone

        match localHost with
        | ValueSome info ->
            [
                for impl in info.InterfaceImpls do
                    match InterfaceImplResolution.tryIface impl.Resolution with
                    | ValueSome ifaceTy -> yield instantiateMember ctx.Store (info.TypeParams, args) ifaceTy
                    | ValueNone -> ()
            ]
        | ValueNone ->
            match ctx.Provider.TryLookupType key with
            | ValueSome(ExternalTypeShape.Class shape) ->
                ExternalSymbols.instantiateInterfaces shape (args.AsSpan().ToArray())
                |> Array.toList
            // A primitive's declared interfaces (`'T[]` is a `seq<'T>`), so the walk crosses
            // off the contract chain as `subtypeParentOf` does for its base.
            | ValueSome(ExternalTypeShape.Intrinsic { Class = ValueSome surface }) ->
                ExternalSymbols.instantiateInterfacesOf surface.Interfaces (args.AsSpan().ToArray())
                |> Array.toList
            // A capability's own `inherit` chain (`enumerator : disposable`), so `e.Dispose()`
            // resolves off the contract rather than off whatever the platform spelling names.
            | ValueSome(ExternalTypeShape.IntrinsicInterface iface) ->
                ExternalSymbols.instantiateInterfacesOf iface.Interfaces (args.AsSpan().ToArray())
                |> Array.toList
            | _ -> []

    /// Find the instantiation of `src` (or one of its bases / interfaces) whose canonical
    /// nominal identity is `tgtKey`, returning that supertype's type args. Reflexive: `src`
    /// itself when its canon key is `tgtKey`. Read-only.
    let tryUpcastWitness (ctx: PassContext) (src: SemType) (tgtKey: TypeKey) : EqArray<SemType> voption =
        // An interface supertype is itself walked for its own bases: `C : B`, `B : A<int>`
        // reaches `A` only THROUGH `B`.
        let rec walk (seen: HashSet<TypeKey>) (cur: SemType) : EqArray<SemType> voption =
            match subtypeNominalOf ctx cur with
            | ValueNone -> ValueNone
            | ValueSome(struct (s, sa)) ->
                // Reconcile a capability's two names at the MATCH only; the walk below keys
                // off the RAW `s`, so a BCL platform type's own bases stay reachable.
                if sameNominalKey ctx s tgtKey then
                    ValueSome sa
                elif not (seen.Add s) then
                    ValueNone
                else
                    let localKey = nominalKeyOf ctx.Store cur

                    let rec pick =
                        function
                        | [] -> ValueNone
                        | iface :: rest ->
                            match walk seen iface with
                            | ValueSome _ as found -> found
                            | ValueNone -> pick rest

                    match pick (subtypeInterfacesOf ctx localKey s sa) with
                    | ValueSome _ as viaIface -> viaIface
                    | ValueNone ->
                        match subtypeParentOf ctx localKey s sa with
                        | ValueSome parentInstance -> walk seen parentInstance
                        | ValueNone -> ValueNone

        walk (HashSet<TypeKey>()) src

    /// Find an instance member `memberName` on an EXTERNAL SUPERTYPE of `objArgTy`, paired
    /// with the supertype's args as reached from it (`[int]` for a `Child :
    /// Base<int>`). SUPERTYPES ONLY, because the caller resolves its own members first.
    /// The walk is breadth-first, so a NEARER ancestor's member wins.
    let tryExternalInheritedMember
        (ctx: PassContext)
        (objArgTy: SemType)
        (memberName: string)
        : struct (ExternalMember * EqArray<SemType>) voption =
        // A node's direct supertypes: its interfaces, then its declared base type.
        let supertypesOf (node: SemType) : SemType list =
            match subtypeNominalOf ctx node with
            | ValueNone -> []
            | ValueSome(struct (s, sa)) ->
                let localKey = nominalKeyOf ctx.Store node

                [
                    yield! subtypeInterfacesOf ctx localKey s sa
                    match subtypeParentOf ctx localKey s sa with
                    | ValueSome parent -> yield parent
                    | ValueNone -> ()
                ]

        let seen = HashSet<TypeKey>()

        let rec walk (frontier: Fifo<SemType>) : struct (ExternalMember * EqArray<SemType>) voption =
            match Fifo.tryDequeue frontier with
            | ValueNone -> ValueNone
            | ValueSome(struct (node, rest)) ->
                match subtypeNominalOf ctx node with
                | ValueNone -> walk rest
                | ValueSome(struct (s, sa)) ->
                    if not (seen.Add s) then
                        walk rest
                    else
                        match ctx.Provider.TryLookupMember(s, memberName) with
                        | ValueSome m when not m.IsStatic -> ValueSome(struct (m, sa))
                        | _ -> walk (Fifo.enqueueAll (supertypesOf node) rest)

        walk (Fifo.ofList (supertypesOf objArgTy))

    /// How a `SemType` is NAMED to a user in a diagnostic. An unpinned typar prints as the
    /// anonymous `'a`. Zonks first, so no caller has to remember to.
    let rec shown (store: TypeStore) (t: SemType) : string =
        match UnionFind.zonkShallow store t with
        | TyConst(key, _) ->
            let (DisplayName name) = SymbolKeyOps.typeSimpleName key
            name
        | TyEnum key -> SymbolKeyOps.typeMetaName key
        | TyClass(k, _)
        | TyUnion(k, _)
        | TyRecord(k, _) -> SymbolKeyOps.typeMetaName k
        | TyVar _
        | TyTypar _ -> "'a"
        // `->` associates right, so only a function DOMAIN needs the parens:
        // `(int -> int) -> string`.
        | TyFun(dom, cod) ->
            match UnionFind.zonkShallow store dom with
            | TyFun _ -> sprintf "(%s) -> %s" (shown store dom) (shown store cod)
            | _ -> sprintf "%s -> %s" (shown store dom) (shown store cod)
        | TyTuple _ -> "tuple"
        | TyOr ds -> ds.Disjuncts |> EqSet.toList |> List.map (shown store) |> String.concat " | "
        | TyLiteral v -> sprintf "%A" v
        | TyUnknown reason -> reason.Render
        | TyKeyOf _
        | TyIndexedAccess _
        | TyConditional _ -> "type expression"

    /// Walk a `SemType` through TyVar Links to surface a nominal shape and report which
    /// kind it is.
    let rec tryResolveNominal (store: TypeStore) (t: SemType) : (NominalKind * TypeKey * EqArray<SemType>) voption =
        match t with
        | TyRecord(n, args) -> ValueSome(NominalKind.Record, n, args)
        | TyClass(n, args) -> ValueSome(NominalKind.Class, n, args)
        | TyUnion(n, args) -> ValueSome(NominalKind.Union, n, args)
        | TyVar tv ->
            match store.Link(UnionFind.find store tv) with
            | ValueSome target -> tryResolveNominal store target
            | ValueNone -> ValueNone
        | _ -> ValueNone
