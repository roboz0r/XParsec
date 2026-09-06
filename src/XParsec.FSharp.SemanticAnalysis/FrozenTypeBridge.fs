namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic

/// `SemType` ↔ `FrozenType`. `TyVar` is the sole `SemType` case with no frozen counterpart:
/// a measure-bearing `TyVar` freezes to its carrier's arity-1 claim over an `FTMeasure`, and
/// thaws back through `IMeasuredThaw`.
[<AutoOpen>]
module FrozenTypeBridge =

    /// The thaw of a frozen measured nominal `FTConst(key, [FTMeasure units])` into `Store`:
    /// a measured `TyVar` whose `Link` is the arity-1 abbreviation `key` expanded and whose
    /// `Units` is the term.
    type IMeasuredThaw =
        abstract Store: TypeStore
        abstract Measured: key: TypeKey * units: MeasureTerm -> SemType

    /// The assignment of types to a template's open typars. One value per instantiation
    /// event: templates instantiated through one value agree at every `(scope, index)`.
    type ITyparInstantiation =
        abstract Typar: scope: TyparScope * index: int -> SemType

    [<RequireQualifiedAccess>]
    module MeasuredThaw =
        /// The thaw for a surface without measured types: a CLR metadata row, a TypeScript
        /// declaration, a hand-built template. `Measured` throws.
        let noneOver (store: TypeStore) : IMeasuredThaw =
            { new IMeasuredThaw with
                member _.Store = store

                member _.Measured(key, units) =
                    failwithf
                        "MeasuredThaw.noneOver: the measured type %s<%O> reached a surface that carries no measure"
                        key.DeclaredPath
                        units
            }

    /// The arity-1 claim measuring `carrier`: `Vesper.float`1` for `Vesper.float`. Each
    /// measurable primitive is declared at arity 0 and, under the same name, at arity 1.
    let measuredClaimKey (carrier: TypeKey) : TypeKey = { carrier with TyparArity = 1 }

    let rec toFrozenWith (onVar: TyVarId -> FrozenType) (ty: SemType) : FrozenType =
        let go = toFrozenWith onVar

        match ty with
        | TyConst(key, args) -> FTConst(key, EqArray.map go args)
        | TyFun(arg, result) -> FTFun(go arg, go result)
        | TyTuple items -> FTTuple(EqArray.map go items)
        | TyRecord(key, args) -> FTRecord(key, EqArray.map go args)
        | TyUnion(key, args) -> FTUnion(key, EqArray.map go args)
        | TyClass(key, args) -> FTClass(key, EqArray.map go args)
        | TyEnum key -> FTEnum key
        // Freezing can collapse the set (two distinct disjuncts freezing equal),
        // so rebuild through `MkUnion` rather than mapping in place.
        | TyOr ds -> FrozenType.MkUnion(seq { for d in ds.Disjuncts -> go d })
        | TyLiteral v -> FTLiteral v
        // The type-level computations are carried across without being evaluated.
        | TyKeyOf t -> FTKeyOf(go t)
        | TyIndexedAccess(objTy, index) -> FTIndexedAccess(go objTy, go index)
        | TyConditional c ->
            FTConditional
                {
                    Check = go c.Check
                    Extends = go c.Extends
                    WhenTrue = go c.WhenTrue
                    WhenFalse = go c.WhenFalse
                }
        | TyTypar(scope, index) -> FTTypar(scope, index)
        | TyUnknown reason -> FTUnknown reason
        | TyVar tv -> onVar tv

    let toFrozen (ty: SemType) : FrozenType =
        toFrozenWith (fun tv -> failwithf "FrozenType.toFrozen: cannot freeze SemType: %A" (TyVar tv)) ty

    /// Freeze after a deep zonk. A measure-bearing root freezes to its carrier's arity-1
    /// claim over the term; `onVar` freezes every root still unlinked after the zonk.
    let freezeWith (store: TypeStore) (onVar: TyVarId -> FrozenType) (ty: SemType) : FrozenType =
        let onRoot (tv: TyVarId) : FrozenType =
            let root = UnionFind.find store tv

            match store.Units root, store.Link root with
            | ValueSome units, ValueSome carrier ->
                match UnionFind.zonk store carrier with
                | TyConst(key, EqEmpty) -> FTConst(measuredClaimKey key, EqArray.singleton (FTMeasure units))
                // The recovery type of a reported reference, already diagnosed at the source.
                | TyUnknown reason -> FTUnknown reason
                | other ->
                    failwithf "FrozenTypeBridge.freezeWith: a measure <%O> over a non-primitive carrier %A" units other
            | _ -> onVar tv

        UnionFind.zonk store ty |> toFrozenWith onRoot

    /// `freezeWith` refusing an unlinked root.
    let freeze (store: TypeStore) (ty: SemType) : FrozenType =
        freezeWith store (fun tv -> failwithf "FrozenType.freeze: cannot freeze SemType: %A" (TyVar tv)) ty

    /// `template` thawed: `thaw` serves a measured nominal, `inst` every open typar.
    let rec instantiateWith (thaw: IMeasuredThaw) (inst: ITyparInstantiation) (template: FrozenType) : SemType =
        let go = instantiateWith thaw inst

        match template with
        | FrozenType.MeasuredNominal(key, units) -> thaw.Measured(key, units)
        | FTConst(key, args) -> TyConst(key, EqArray.map go args)
        | FTMeasure units -> failwithf "FrozenTypeBridge: the measure <%O> reached type position" units
        | FTFun(arg, result) -> TyFun(go arg, go result)
        | FTTuple items -> TyTuple(EqArray.map go items)
        | FTRecord(key, args) -> TyRecord(key, EqArray.map go args)
        | FTUnion(key, args) -> TyUnion(key, EqArray.map go args)
        | FTClass(key, args) -> TyClass(key, EqArray.map go args)
        | FTEnum key -> TyEnum key
        // Instantiation can collapse the set (a typar disjunct becoming another
        // disjunct), so rebuild through `MkUnion`, not a raw `TyOr`.
        | FTOr ds -> SemType.MkUnion(seq { for d in ds.Disjuncts -> go d })
        | FTLiteral v -> TyLiteral v
        // The type-level computations instantiate their children but are NOT evaluated.
        | FTKeyOf t -> TyKeyOf(go t)
        | FTIndexedAccess(objTy, index) -> TyIndexedAccess(go objTy, go index)
        | FTConditional c ->
            TyConditional
                {
                    Check = go c.Check
                    Extends = go c.Extends
                    WhenTrue = go c.WhenTrue
                    WhenFalse = go c.WhenFalse
                }
        | FTTypar(scope, index) -> inst.Typar(scope, index)
        | FTUnknown reason -> TyUnknown reason

    [<Struct>]
    type internal LocalTyparKey = { Binding: LocalBindingId; Index: int }

    /// The identity instantiation: each declared typar maps back to its own `TyTypar`
    /// marker. A local typar has no marker to map to, so it MINTS a fresh `TyVar`, memoised
    /// per `(binding, index)` so repeated occurrences share one cell.
    let ofFrozen (thaw: IMeasuredThaw) (ft: FrozenType) : SemType =
        let localCache = Dictionary<LocalTyparKey, SemType>()

        instantiateWith
            thaw
            { new ITyparInstantiation with
                member _.Typar(scope, index) =
                    match scope with
                    | TyparScope.LocalFunction binding ->
                        let key = { Binding = binding; Index = index }

                        match localCache.TryGetValue key with
                        | true, v -> v
                        | _ ->
                            let v = TyVar(thaw.Store.NewTypeVar())
                            localCache.[key] <- v
                            v
                    | _ -> TyTypar(scope, index)
            }
            ft

    // A template is an external descriptor's body with its open typars baked as `FTTypar`
    // leaves. It carries type shape only, never constraints.

    /// Stands in for a body that can't be built at extraction time, because it may
    /// forward-reference a type registered later in the same package.
    let deferredTemplate: FrozenType = FTUnknown UnknownReason.Deferred

    /// A local typar arises only inside a decl's BODY (a body-local `let`'s own generalised
    /// scheme), so one in a SIGNATURE / type-shape template is a producer bug.
    let localTyparInTemplate (binding: LocalBindingId) (k: int) : SemType =
        failwithf
            "FrozenTypeBridge.localTyparInTemplate: unexpected body-local typar %d of binding %O in a signature template"
            k
            binding

    [<RequireQualifiedAccess>]
    module TyparInstantiation =

        /// A declaration's own typars from `args`: a type shape's (record field, union-case
        /// field, interface arg, base type, abbreviation body) or a module function's scheme.
        /// An index past `args` degrades to `TyUnknown UnknownReason.ArityMismatch`; a
        /// member's or a body-local typar in the template is a producer bug.
        let declaringOnly (args: SemType[]) : ITyparInstantiation =
            { new ITyparInstantiation with
                member _.Typar(scope, index) =
                    match scope with
                    | TyparScope.Type _
                    | TyparScope.ModuleFunction _ ->
                        if index < args.Length then
                            args.[index]
                        else
                            TyUnknown UnknownReason.ArityMismatch
                    | TyparScope.Member _ ->
                        failwithf
                            "TyparInstantiation.declaringOnly: unexpected member typar %d in a type-shape template"
                            index
                    | TyparScope.LocalFunction binding -> localTyparInTemplate binding index
            }

        /// A declaring index past `declaringArgs` in a member template is a provider bug.
        let private declaringArg (policy: string) (declaringArgs: SemType[]) (i: int) : SemType =
            if i < declaringArgs.Length then
                declaringArgs.[i]
            else
                failwithf
                    "TyparInstantiation.%s: declaring typar %d out of range for %d declaring args"
                    policy
                    i
                    declaringArgs.Length

        /// The applicability-filtering form: the declaring type's typars substituted from
        /// `declaringArgs`, a member's or a module function's own left an inert `TyTypar`
        /// marker.
        let openMethod (declaringArgs: SemType[]) : ITyparInstantiation =
            { new ITyparInstantiation with
                member _.Typar(scope, index) =
                    match scope with
                    | TyparScope.Type _ -> declaringArg "openMethod" declaringArgs index
                    | TyparScope.Member _
                    | TyparScope.ModuleFunction _ -> TyTypar(scope, index)
                    | TyparScope.LocalFunction binding -> localTyparInTemplate binding index
            }

        /// A member call's instantiation at `level`: one fresh `TyVar` per own typar,
        /// memoised in the value, so every template instantiated through ONE value shares
        /// cells. `seed` pre-binds own typar indices with types instead of fresh vars.
        let atCallSite
            (store: TypeStore)
            (level: int)
            (seed: (int * SemType) list)
            (declaringArgs: SemType[])
            : ITyparInstantiation =
            let cache = Dictionary<int, SemType>()

            for (j, ty) in seed do
                cache.[j] <- ty

            { new ITyparInstantiation with
                member _.Typar(scope, index) =
                    match scope with
                    | TyparScope.Type _ -> declaringArg "atCallSite" declaringArgs index
                    | TyparScope.Member _
                    | TyparScope.ModuleFunction _ ->
                        match cache.TryGetValue index with
                        | true, v -> v
                        | _ ->
                            let tv = store.NewTypeVar()
                            store.SetLevel(UnionFind.find store tv, level)
                            let v = TyVar tv
                            cache.[index] <- v
                            v
                    | TyparScope.LocalFunction binding -> localTyparInTemplate binding index
            }

    let instantiateDeclaring (thaw: IMeasuredThaw) (template: FrozenType) (declaringArgs: SemType[]) : SemType =
        instantiateWith thaw (TyparInstantiation.declaringOnly declaringArgs) template

    /// Fully ground: no open typar of any scope and no `FTUnknown`.
    let rec ftIsGround (t: FrozenType) : bool =
        match t with
        | FTTypar _
        | FTUnknown _ -> false
        | t -> FrozenType.forallChildren ftIsGround t

    /// `FTTypar(Type _, i)` → `declaringArgs.[i]`, staying in `FrozenType` and touching no
    /// inference state. This is how an abbreviation body is expanded against use-site args.
    /// An under-applied generic abbrev is tolerated, not a crash.
    let rec substituteDeclaring (declaringArgs: FrozenType[]) (template: FrozenType) : FrozenType =
        match template with
        | FTTypar(TyparScope.Type _, i) ->
            if i < declaringArgs.Length then
                declaringArgs.[i]
            else
                FTUnknown UnknownReason.ArityMismatch
        | FTTypar(scope, j) ->
            failwithf "FrozenTypeBridge.substituteDeclaring: unexpected typar %d of %A in a type-shape template" j scope
        | t -> FrozenType.mapChildren (substituteDeclaring declaringArgs) t

    /// The impl in `ifaces` whose identity is `target`, with its args substituted at THIS
    /// object argument: `FTTypar(Type _, i) := declArgs.[i]`.
    let pickInterfaceWitness
        (target: TypeKey)
        (declArgs: FrozenType[])
        (ifaces: FrozenNominal seq)
        : EqArray<FrozenType> voption =
        match ifaces |> Seq.tryFind (fun iface -> iface.Key = target) with
        | Some iface -> ValueSome(iface.Args |> EqArray.map (substituteDeclaring declArgs))
        | None -> ValueNone
