namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open Vesper

/// `SemType` ↔ `FrozenType`. `TyVar` is the sole `SemType` case with no frozen counterpart:
/// a measure-bearing `TyVar` freezes to its carrier's arity-1 claim over an `FTMeasure`, and
/// thaws back through `IMeasuredThaw`.
[<AutoOpen>]
module FrozenTypeBridge =

    /// The thaw of a frozen measured nominal into `Store`: a `TyVar` whose carrier is the
    /// abbreviation `key` expanded over `typeArgs`, measured by `units`.
    type IMeasuredThaw =
        abstract Store: TypeStore
        abstract Measured: key: TypeKey * typeArgs: BlockM<SemType, typeSlot> * units: MeasureTerm -> SemType
        /// A measure ARGUMENT: the metavar carrying `units` that fills a measure-kinded
        /// parameter slot.
        abstract Measure: units: MeasureTerm -> SemType

    /// The assignment of types to a template's open typars. One value per instantiation
    /// event: templates instantiated through one value agree at every `(scope, index)`.
    type ITyparInstantiation =
        abstract Typar: scope: TyparScope * index: int<typeSlot> -> SemType

    [<RequireQualifiedAccess>]
    module MeasuredThaw =
        /// The thaw for a surface without measured types: a CLR metadata row, a TypeScript
        /// declaration, a hand-built template. `Measured` throws.
        let noneOver (store: TypeStore) : IMeasuredThaw =
            { new IMeasuredThaw with
                member _.Store = store

                member _.Measured(key, _, units) =
                    failwithf
                        "MeasuredThaw.noneOver: the measured type %s<%O> reached a surface that carries no measure"
                        key.DeclaredPath
                        units

                member _.Measure(units) =
                    failwithf "MeasuredThaw.noneOver: the measure <%O> reached a surface that carries no measure" units
            }

    /// The claim measuring `carrier`: `Vesper.float<'u>` for `Vesper.float`. Each measurable
    /// primitive is declared at arity 0 and, under the same name, as an abbreviation over one
    /// measure parameter.
    let measuredClaimKey (carrier: TypeKey) : TypeKey =
        { carrier with
            TyparArity = KeyArity.Written 1<sigSlot>
        }

    let rec toFrozenWith (onVar: TyVarId -> FrozenType) (ty: SemType) : FrozenType =
        let go = toFrozenWith onVar

        match ty with
        | TyConst(key, args) -> FTConst(key, Block.map go args)
        | TyFun(arg, result) -> FTFun(go arg, go result)
        | TyTuple items -> FTTuple(Block.map go items)
        | TyRecord(key, args) -> FTRecord(key, Block.map go args)
        | TyUnion(key, args) -> FTUnion(key, Block.map go args)
        | TyClass(key, args) -> FTClass(key, Block.map go args)
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

    /// Freeze after a deep zonk. A measure argument freezes to `FTMeasure`, a measured root
    /// to its carrier's arity-1 claim over the term; `onVar` freezes every root still free
    /// after the zonk.
    let freezeWith (store: TypeStore) (onVar: TyVarId -> FrozenType) (ty: SemType) : FrozenType =
        let onRoot (tv: TyVarId) : FrozenType =
            let root = UnionFind.find store tv

            match store.State root with
            | RootState.Measure units -> FTMeasure units
            | RootState.Measured(units, carrier) ->
                match UnionFind.zonk store carrier with
                | TyConst(key, BlockEmpty) -> FTConst(measuredClaimKey key, Block.singleton (FTMeasure units))
                // The recovery type of a reported reference, already diagnosed at the source.
                | TyUnknown reason -> FTUnknown reason
                | other ->
                    failwithf "FrozenTypeBridge.freezeWith: a measure <%O> over a non-primitive carrier %A" units other
            | RootState.Linked target -> failwithf "FrozenTypeBridge.freezeWith: a zonked root linked to %A" target
            | RootState.Free -> onVar tv

        UnionFind.zonk store ty |> toFrozenWith onRoot

    /// `freezeWith` refusing an unlinked root.
    let freeze (store: TypeStore) (ty: SemType) : FrozenType =
        freezeWith store (fun tv -> failwithf "FrozenType.freeze: cannot freeze SemType: %A" (TyVar tv)) ty

    /// `template` thawed: `thaw` serves a measured nominal, `inst` every open typar.
    let rec instantiateWith (thaw: IMeasuredThaw) (inst: ITyparInstantiation) (template: FrozenType) : SemType =
        let go = instantiateWith thaw inst

        match template with
        | FrozenType.MeasuredNominal m -> thaw.Measured(m.Key, Block.map go m.TypeArgs, m.Units)
        | FTConst(key, args) -> TyConst(key, Block.map go args)
        | FTMeasure units -> thaw.Measure units
        | FTFun(arg, result) -> TyFun(go arg, go result)
        | FTTuple items -> TyTuple(Block.map go items)
        | FTRecord(key, args) -> TyRecord(key, Block.map go args)
        | FTUnion(key, args) -> TyUnion(key, Block.map go args)
        | FTClass(key, args) -> TyClass(key, Block.map go args)
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

    // A template is an external descriptor's body with its open typars baked as `FTTypar`
    // leaves. It carries type shape only, never constraints.

    /// Stands in for a body that can't be built at extraction time, because it may
    /// forward-reference a type registered later in the same package.
    let deferredTemplate: FrozenType = FTUnknown UnknownReason.Deferred

    /// A local typar arises only inside a decl's BODY (a body-local `let`'s own generalised
    /// scheme), so one in a SIGNATURE / type-shape template is a producer bug.
    let localTyparInTemplate (binding: LocalBindingId) (k: int<typeSlot>) : SemType =
        failwithf
            "FrozenTypeBridge.localTyparInTemplate: unexpected body-local typar %d of binding %O in a signature template"
            k
            binding

    [<RequireQualifiedAccess>]
    module TyparInstantiation =

        /// The instantiation dispatching each typar leaf to the handler for its scope.
        let ofScopes
            (onType: TypeKey -> int<typeSlot> -> SemType)
            (onMember: TypeKey -> int<typeSlot> -> SemType)
            (onFunction: BindingKey -> int<typeSlot> -> SemType)
            (onLocal: LocalBindingId -> int<typeSlot> -> SemType)
            : ITyparInstantiation =
            { new ITyparInstantiation with
                member _.Typar(scope, index) =
                    match scope with
                    | TyparScope.Type key -> onType key index
                    | TyparScope.Member owner -> onMember owner index
                    | TyparScope.ModuleFunction key -> onFunction key index
                    | TyparScope.LocalFunction binding -> onLocal binding index
            }

        let private marker (scope: TyparScope) (index: int<typeSlot>) : SemType = TyTypar(scope, index)

        /// A fresh `TyVar` per `(binding, index)`, memoised so repeated occurrences of one
        /// local typar share one cell.
        let mintLocals (store: TypeStore) : LocalBindingId -> int<typeSlot> -> SemType =
            let roots = LocalTyparRoots store
            fun binding index -> TyVar(roots.At(binding, index))

        /// A declaration's own typars from `args`: a type shape's (record field, union-case
        /// field, interface arg, base type, abbreviation body) or a module function's scheme.
        /// An index past `args` degrades to `TyUnknown UnknownReason.ArityMismatch`; a
        /// member's or a body-local typar in the template is a producer bug.
        let declaringOnly (args: SemType[]) : ITyparInstantiation =
            let arg (index: int<typeSlot>) =
                if int index < args.Length then
                    args.[int index]
                else
                    TyUnknown UnknownReason.ArityMismatch

            ofScopes
                (fun _ index -> arg index)
                (fun _ index ->
                    failwithf
                        "TyparInstantiation.declaringOnly: unexpected member typar %d in a type-shape template"
                        index
                )
                (fun _ index -> arg index)
                localTyparInTemplate

        /// A declaring index past `declaringArgs` in a member template is a provider bug.
        let private declaringArg (policy: string) (declaringArgs: SemType[]) (i: int<typeSlot>) : SemType =
            if int i < declaringArgs.Length then
                declaringArgs.[int i]
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
            ofScopes
                (fun _ index -> declaringArg "openMethod" declaringArgs index)
                (fun owner -> marker (TyparScope.Member owner))
                (fun key -> marker (TyparScope.ModuleFunction key))
                localTyparInTemplate

        /// A member call's own typar cells: one fresh `TyVar` per index at `level`, memoised so
        /// every template resolved through ONE of these shares cells. `seed` pre-binds indices
        /// with types instead of fresh vars.
        let ownCells (store: TypeStore) (level: int) (seed: (int<typeSlot> * SemType) list) : int<typeSlot> -> SemType =
            let cache = Dictionary<int<typeSlot>, SemType>()

            for (j, ty) in seed do
                cache.[j] <- ty

            fun index ->
                match cache.TryGetValue index with
                | true, v -> v
                | _ ->
                    let tv = store.NewTypeVar()
                    store.SetLevel(UnionFind.find store tv, level)
                    let v = TyVar tv
                    cache.[index] <- v
                    v

        /// A member call's instantiation: declaring typars from `declaringArgs`, own typars from
        /// `own`. Retain the same `ownCells` function to reach the cells it minted.
        let atCallSiteOver (own: int<typeSlot> -> SemType) (declaringArgs: SemType[]) : ITyparInstantiation =
            ofScopes
                (fun _ index -> declaringArg "atCallSiteOver" declaringArgs index)
                (fun _ index -> own index)
                (fun _ index -> own index)
                localTyparInTemplate

        /// The identity instantiation: each declared typar maps back to its own inert
        /// `TyTypar` marker. A local typar has no marker, so `mintLocals` supplies one cell.
        let identity (store: TypeStore) : ITyparInstantiation =
            ofScopes
                (fun key -> marker (TyparScope.Type key))
                (fun owner -> marker (TyparScope.Member owner))
                (fun key -> marker (TyparScope.ModuleFunction key))
                (mintLocals store)

    let ofFrozen (thaw: IMeasuredThaw) (ft: FrozenType) : SemType =
        instantiateWith thaw (TyparInstantiation.identity thaw.Store) ft

    let instantiateDeclaring (thaw: IMeasuredThaw) (template: FrozenType) (declaringArgs: SemType[]) : SemType =
        instantiateWith thaw (TyparInstantiation.declaringOnly declaringArgs) template

    /// Fully ground: no open typar of any scope and no `FTUnknown`.
    let rec ftIsGround (t: FrozenType) : bool =
        match t with
        | FTTypar _
        | FTUnknown _ -> false
        | t -> FrozenType.forallChildren ftIsGround t

    /// `FTTypar(Type _, i)` → `declaringArgs.[i]`, purely within `FrozenType`. This expands an
    /// abbreviation body against a use site's type-slot args. An under-applied generic abbrev
    /// yields `FTUnknown ArityMismatch` at the missing slots.
    let rec substituteDeclaring (declaringArgs: BlockM<FrozenType, typeSlot>) (template: FrozenType) : FrozenType =
        match template with
        | FTTypar(TyparScope.Type _, i) ->
            if i < declaringArgs.Length then
                declaringArgs.[i]
            else
                FTUnknown UnknownReason.ArityMismatch
        | FTTypar(scope, j) ->
            failwithf "FrozenTypeBridge.substituteDeclaring: unexpected typar %d of %A in a type-shape template" j scope
        | t -> FrozenType.mapChildren (substituteDeclaring declaringArgs) t

    /// The impl in `ifaces` whose identity is `target`, with its args substituted at the object
    /// argument's type-slot args: `FTTypar(Type _, i) := declArgs.[i]`.
    let pickInterfaceWitness
        (target: TypeKey)
        (declArgs: BlockM<FrozenType, typeSlot>)
        (ifaces: FrozenNominal seq)
        : Block<FrozenType> voption =
        match ifaces |> Seq.tryFind (fun iface -> iface.Key = target) with
        | Some iface -> ValueSome(iface.Args |> Block.map (substituteDeclaring declArgs))
        | None -> ValueNone
