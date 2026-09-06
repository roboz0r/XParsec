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
    /// event: templates instantiated through one value agree at every typar index.
    type ITyparInstantiation =
        abstract Declaring: i: int -> SemType
        abstract Method: j: int -> SemType
        abstract Local: scheme: SchemeId * k: int -> SemType

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
        | TyTypar(axis, index) -> FTTypar(axis, index)
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
        | FTTypar(TyparAxis.Declaring, i) -> inst.Declaring i
        | FTTypar(TyparAxis.Method, j) -> inst.Method j
        | FTLocalTypar(scheme, k) -> inst.Local(scheme, k)
        | FTUnknown reason -> TyUnknown reason

    [<Struct>]
    type internal LocalTyparKey = { Scheme: SchemeId; Index: int }

    /// The identity instantiation: each DECLARED placeholder maps back to its own
    /// `TyTypar` marker. `FTLocalTypar` has no marker to map to, so it MINTS a fresh
    /// `TyVar`, memoised per `(scheme, k)` so repeated occurrences share one cell.
    let ofFrozen (thaw: IMeasuredThaw) (ft: FrozenType) : SemType =
        let localCache = Dictionary<LocalTyparKey, SemType>()

        instantiateWith
            thaw
            { new ITyparInstantiation with
                member _.Declaring i = TyTypar(TyparAxis.Declaring, i)
                member _.Method j = TyTypar(TyparAxis.Method, j)

                member _.Local(scheme, k) =
                    let key = { Scheme = scheme; Index = k }

                    match localCache.TryGetValue key with
                    | true, v -> v
                    | _ ->
                        let v = TyVar(thaw.Store.NewTypeVar())
                        localCache.[key] <- v
                        v
            }
            ft

    // A template is an external descriptor's body with its open typars baked as
    // `FTTypar(Declaring,i)` / `FTTypar(Method,j)`. It carries type shape only,
    // never constraints.

    /// Stands in for a body that can't be built at extraction time, because it may
    /// forward-reference a type registered later in the same package.
    let deferredTemplate: FrozenType = FTUnknown UnknownReason.Deferred

    /// An `FTLocalTypar` arises only inside a decl's BODY (a body-local `let`'s own
    /// generalized scheme), so one in a SIGNATURE / type-shape template is a producer bug.
    let localTyparInTemplate (scheme: SchemeId) (k: int) : SemType =
        failwithf
            "FrozenTypeBridge.localTyparInTemplate: unexpected body-local typar %d of scheme %O in a signature template"
            k
            scheme

    [<RequireQualifiedAccess>]
    module TyparInstantiation =

        /// A type-shape descriptor's instantiation, declaring axis only: record field,
        /// union-case field, interface arg, base type, abbreviation body. An index past
        /// `declaringArgs` degrades to `TyUnknown UnknownReason.ArityMismatch`; a method
        /// or body-local typar in the template is a producer bug.
        let declaringOnly (declaringArgs: SemType[]) : ITyparInstantiation =
            { new ITyparInstantiation with
                member _.Declaring i =
                    if i < declaringArgs.Length then
                        declaringArgs.[i]
                    else
                        TyUnknown UnknownReason.ArityMismatch

                member _.Method j =
                    failwithf "TyparInstantiation.declaringOnly: unexpected method typar %d in a type-shape template" j

                member _.Local(scheme, k) = localTyparInTemplate scheme k
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

        /// The applicability-filtering form: declaring typars substituted from
        /// `declaringArgs`, `Method j` left an inert `TyTypar(Method, j)` marker.
        let openMethod (declaringArgs: SemType[]) : ITyparInstantiation =
            { new ITyparInstantiation with
                member _.Declaring i =
                    declaringArg "openMethod" declaringArgs i

                member _.Method j = TyTypar(TyparAxis.Method, j)
                member _.Local(scheme, k) = localTyparInTemplate scheme k
            }

        /// A member call's instantiation at `level`: one fresh `TyVar` per method typar,
        /// memoised in the value, so every template instantiated through ONE value shares
        /// cells. `seed` pre-binds method typar indices with types instead of fresh vars.
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
                member _.Declaring i =
                    declaringArg "atCallSite" declaringArgs i

                member _.Method j =
                    match cache.TryGetValue j with
                    | true, v -> v
                    | _ ->
                        let tv = store.NewTypeVar()
                        store.SetLevel(UnionFind.find store tv, level)
                        let v = TyVar tv
                        cache.[j] <- v
                        v

                member _.Local(scheme, k) = localTyparInTemplate scheme k
            }

    let instantiateDeclaring (thaw: IMeasuredThaw) (template: FrozenType) (declaringArgs: SemType[]) : SemType =
        instantiateWith thaw (TyparInstantiation.declaringOnly declaringArgs) template

    /// Every open typar, either axis, becomes a marker on `axis`, index preserved. A measured
    /// argument passes through untouched.
    let rec reaxisTo (axis: TyparAxis) (template: FrozenType) : FrozenType =
        match template with
        | FTTypar(_, i) -> FTTypar(axis, i)
        | FTLocalTypar(scheme, k) ->
            failwithf
                "FrozenTypeBridge.reaxisTo: unexpected body-local typar %d of scheme %O in a signature template"
                k
                scheme
        | t -> FrozenType.mapChildren (reaxisTo axis) t

    /// Contract extraction bakes EVERY typar on the `Declaring` axis, numbering the
    /// declaring type's own first, so a typar the member INTRODUCES (`<'a>`, or an
    /// implicit `'T`) lands at `i >= declaringTyparArity`: re-axis those to `Method`.
    let rec reaxisMethodTypars (declaringTyparArity: int) (template: FrozenType) : FrozenType =
        match template with
        | FTTypar(TyparAxis.Declaring, i) when i >= declaringTyparArity ->
            FTTypar(TyparAxis.Method, i - declaringTyparArity)
        | t -> FrozenType.mapChildren (reaxisMethodTypars declaringTyparArity) t

    /// Fully ground: no open typar on either axis, no body-local typar, and no `FTUnknown`.
    let rec ftIsGround (t: FrozenType) : bool =
        match t with
        | FTTypar _
        | FTLocalTypar _
        | FTUnknown _ -> false
        | t -> FrozenType.forallChildren ftIsGround t

    /// `FTTypar(Declaring,i)` → `declaringArgs.[i]`, staying in `FrozenType` and
    /// touching no inference state. This is how an abbreviation body is expanded
    /// against use-site args. An under-applied generic abbrev is tolerated, not a crash.
    let rec substituteDeclaring (declaringArgs: FrozenType[]) (template: FrozenType) : FrozenType =
        match template with
        | FTTypar(TyparAxis.Declaring, i) ->
            if i < declaringArgs.Length then
                declaringArgs.[i]
            else
                FTUnknown UnknownReason.ArityMismatch
        | FTTypar(TyparAxis.Method, j) ->
            failwithf "FrozenTypeBridge.substituteDeclaring: unexpected method typar %d in a type-shape template" j
        | t -> FrozenType.mapChildren (substituteDeclaring declaringArgs) t

    /// The impl in `ifaces` whose identity is `target`, with its args substituted at THIS
    /// object argument: `FTTypar(Declaring,i) := declArgs.[i]`.
    let pickInterfaceWitness
        (target: TypeKey)
        (declArgs: FrozenType[])
        (ifaces: FrozenNominal seq)
        : EqArray<FrozenType> voption =
        match ifaces |> Seq.tryFind (fun iface -> iface.Key = target) with
        | Some iface -> ValueSome(iface.Args |> EqArray.map (substituteDeclaring declArgs))
        | None -> ValueNone
