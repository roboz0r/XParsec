namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis


/// The unification SUBSTRATE: resolution/zonking, occurs/level adjustment,
/// substitution + member instantiation, and the nominal subtype walks
/// (`canonKey`, `tryUpcastWitness`). Charter: everything the directional and
/// mutating layers stand on, with NO dependency on `subsumes` or `unify`.
/// The dependency rule is one-way — `UnificationEngineCore` <- `UnificationSubsume`
/// <- `UnificationEngine`; nothing in the lower layers calls back into `unify`.
module UnificationEngineCore =

    /// One level deep — call recursively for full resolution. Stops at a
    /// measure-bearing root so the measure stays attached: `unify` and
    /// `unitsOf` need the TyVar wrapper to see Units, and following Link
    /// straight through to the bare carrier would drop them.
    let resolveStep (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome t' when root.Units.IsNone -> t'
            | _ -> TyVar root
        | _ -> t

    /// Erase reference-type nullability (`T | null → T`) throughout `t`: drop the
    /// `null` member (`RuntimeNames.nullKey`) from every anonymous union, collapsing a
    /// resulting singleton (`obj | null → obj`). This is the CLR-ABI view of a nullable
    /// reference — `obj | null` and `obj` are the same `System.Object` slot — applied
    /// ONLY at ABI-matching seams (interface/override conformance), NOT in general
    /// typing, where the union is kept so `subsumes` still tracks `T <: T | null`. A
    /// value-type `int | null` (⇒ `System.Nullable<int>`, a distinct repr) is out of
    /// scope and erases to `int`, which the self-host never exercises.
    let rec stripReferenceNull (t: SemType) : SemType =
        // Resolve at each node first: an annotated `objnull` param can arrive behind a
        // `TyVar` Link, and `mapChildren` treats a `TyVar` as a leaf, so a raw walk
        // would miss the union.
        match resolveStep t with
        | TyOr members ->
            members.Members
            |> EqSet.toList
            |> List.filter (
                function
                | TyNull -> false
                | _ -> true
            )
            |> List.map stripReferenceNull
            |> mkUnion
        | resolved -> SemType.mapChildren stripReferenceNull resolved

    /// Fully resolve a SemType: walk all TyVar chains AND recurse into
    /// compound shapes. A measure-bearing TyVar (`Units` set on its root)
    /// is preserved as a TyVar rather than collapsed into its carrier —
    /// the measure rides on the root, so downstream consumers can read it
    /// off the returned `TyVar` (already a root).
    let rec zonk (t: SemType) : SemType =
        match t with
        // `headZonk` (UnionFind) owns the root + `.Link` chase, with the same
        // `Units`-measure stop; `zonk` adds only the recursive argument rebuild.
        // When the head resolves to a non-var, re-enter `zonk` so its arguments
        // zonk too (`headZonk` leaves them untouched).
        | TyVar _ ->
            match UnionFind.headZonk t with
            | TyVar _ as v -> v
            | resolved -> zonk resolved
        // Pure child recursion (`mapChildren` routes `TyOr` through the smart
        // constructor: resolving a member can collapse / reorder the set).
        | t -> SemType.mapChildren zonk t

    /// Decompose a (zonked) tupled-argument type into its element types: a
    /// .NET-style call passes one argument that is a tuple / unit / single
    /// value. The inverse of `tupleOrSingle`; used by call-site overload
    /// resolution (`String.Concat(…)`, external ctors).
    let argElemsOf (argTy: SemType) : SemType list =
        match zonk argTy with
        | TyTuple xs -> EqArray.toList xs
        | TyUnit -> []
        | single -> [ single ]

    /// The call-site argument arity of a (shallow-resolved) .NET-style tupled
    /// argument: the tuple width, `0` for `unit`, else `1`. The count `argElemsOf`
    /// would yield, without materialising the element list — used to select a
    /// constructor overload by arity.
    let argArityOf (argTy: SemType) : int =
        match resolveStep argTy with
        | TyTuple xs -> xs.Length
        | TyUnit -> 0
        | _ -> 1

    /// The single SemType a parameter list presents as a function argument:
    /// `unit` for none, the bare type for one, a tuple for many. Inverse of
    /// `argElemsOf`.
    let tupleOrSingle (ctx: PassContext) (paramTys: SemType list) : SemType =
        match paramTys with
        | [] -> ctx.Intrinsics.Unit
        | [ t ] -> t
        | many -> TyTuple(EqArray.ofList many)

    /// Collapses any pair whose `Kind` already appears on the target: two
    /// constraints with the same `Kind` discharge to the same predicate, so
    /// keeping both would fire the diagnostic twice for one rule.
    let private mergeConstraints (target: TypeVar) (additions: SemanticConstraint list) : unit =
        let mutable acc = target.Constraints

        for c in additions do
            if not (acc |> List.exists (fun existing -> existing.Kind = c.Kind)) then
                acc <- c :: acc

        target.Constraints <- acc

    /// Called whenever a TyVar is no longer the equivalence-class
    /// representative (either after union-find collapse, or when its Link is
    /// set). Bounds attached to a non-representative would otherwise never
    /// fire their on-unified callbacks.
    let migrateBounds (target: TypeVar) (source: TypeVar) : unit =
        if not (System.Object.ReferenceEquals(target, source)) then
            if not (List.isEmpty source.Constraints) then
                mergeConstraints target source.Constraints
                source.Constraints <- []

            if not (List.isEmpty source.SrtpBounds) then
                target.SrtpBounds <- source.SrtpBounds @ target.SrtpBounds
                source.SrtpBounds <- []

            if not (List.isEmpty source.PendingDotAccess) then
                target.PendingDotAccess <- source.PendingDotAccess @ target.PendingDotAccess
                source.PendingDotAccess <- []

            if not (List.isEmpty source.Defaults) then
                target.Defaults <- target.Defaults @ source.Defaults
                source.Defaults <- []
    // TODO: fire on-unified callbacks for newly-stable SRTP bounds once
    // the SRTP / IWSAM resolution machinery exists.

    /// Two passes folded into one walk:
    /// (a) **Occurs check** — does `target` (already a union-find root) appear
    ///     anywhere inside `t`? Stops the `let rec f x = f` / `let rec g = g g`
    ///     family from cycling Link pointers and making zonk loop.
    /// (b) **Level adjustment** — when `target` is about to be linked to `t`,
    ///     every TyVar reachable from `t` becomes co-scoped with `target`.
    ///     Lower any reachable level above `target.Level` down to it so
    ///     generalisation at the enclosing scope sees the right "free" set.
    /// Resolves through Links and recurses into compound shapes. The `||`
    /// short-circuit on occurs-fail leaves some reachable TyVars unadjusted,
    /// but a failed unification produces a diagnostic and there's nothing
    /// to generalise after; adjusting them would be wasted work.
    let rec occursAndAdjust (target: TypeVar) (t: SemType) : bool =
        match resolveStep t with
        | TyVar tv ->
            let root = UnionFind.find tv

            if System.Object.ReferenceEquals(root, target) then
                true
            else
                if root.Level > target.Level then
                    root.Level <- target.Level

                false
        // Pure child descent — a metavar buried in ANY child (the type-level
        // computations included) still needs detection + level adjustment.
        | t -> SemType.existsChild (occursAndAdjust target) t

    /// Two non-equal measures emit a diagnostic; one of them is kept on the
    /// survivor so further unifications against it stay coherent.
    let mergeUnits
        (ctx: PassContext)
        (key: NodeKey)
        (newRoot: TypeVar)
        (unitsA: MeasureTerm voption)
        (unitsB: MeasureTerm voption)
        : unit =
        match unitsA, unitsB with
        | ValueNone, ValueNone -> ()
        | ValueSome m, ValueNone
        | ValueNone, ValueSome m -> newRoot.Units <- ValueSome m
        | ValueSome m1, ValueSome m2 when m1.Equals(m2) -> newRoot.Units <- ValueSome m1
        | ValueSome m1, ValueSome m2 ->
            newRoot.Units <- ValueSome m1

            ctx.Error(key, sprintf "Measure mismatch: <%O> vs <%O>" m1 m2)

    /// Substitute TyVar roots that appear as keys in `subst` with their
    /// target `SemType`, recursing into compound shapes. Other TyVars are
    /// returned unchanged (followed through union-find but not their
    /// `Link`s — that's `zonk`'s job). Public so Elaborate can reuse the same
    /// substitution when reading field types off a generic receiver.
    let rec substituteWith (subst: Dictionary<TypeVar, SemType>) (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = UnionFind.find tv

            match subst.TryGetValue root with
            | true, target -> target
            | false, _ ->
                // Field / case-arg types stored on the registry are
                // *placeholder* TyVars whose root is never in `subst` (keys
                // are the declared type's `TypeParams`). Follow `Link` so a
                // placeholder targeting `TyVar typarRoot` resolves to
                // whatever `subst[typarRoot]` says. Stop at measure-bearing
                // roots (same rule `zonk` uses): a measured TyVar's `Link`
                // carries the bare carrier, and following through would drop
                // the `Units` on the root.
                match root.Link with
                | ValueSome target when root.Units.IsNone -> substituteWith subst target
                | _ -> TyVar root
        // Pure child recursion (`mapChildren` routes `TyOr` through the smart
        // constructor: substituting a typar member can collapse / reorder the set).
        | t -> SemType.mapChildren (substituteWith subst) t

    /// Empty when the lengths don't match — the caller has already (or
    /// should) emit an arity diagnostic, and an empty subst keeps the field
    /// types unsubstituted rather than silently mismatching. Public so
    /// Elaborate can rebuild the same substitution when projecting fields off a
    /// generic receiver in a field-chain.
    let mkNamedTypeSubst
        (typeParams: EqArray<string * TypeVar>)
        (args: EqArray<SemType>)
        : Dictionary<TypeVar, SemType> =
        let subst = Dictionary<TypeVar, SemType>(HashIdentity.Reference)

        if typeParams.Length = args.Length then
            let mutable i = 0

            for (_, tp) in typeParams do
                subst.[UnionFind.find tp] <- args.[i]
                i <- i + 1

        subst

    /// One-shot field / member instantiation: build the typar→arg subst from
    /// the declaring type's `TypeParams` and the receiver's `args`, then
    /// substitute it through `ty`. Hot single-substitution sites (record
    /// field read, class- and union-member access, SRTP static-member
    /// dispatch, abbreviation expansion) route through this helper. Sites
    /// that reuse the same subst across a loop / Array.map keep the explicit
    /// `mkNamedTypeSubst` + `substituteWith` pair so the dictionary is only
    /// built once.
    let instantiateMember (typeParams: EqArray<string * TypeVar>, args: EqArray<SemType>) (ty: SemType) : SemType =
        substituteWith (mkNamedTypeSubst typeParams args) ty

    /// Append `c` to `tv`'s constraints unless one of the same `Kind` is already
    /// present. Both per-use freshening paths (`freshConstrainedTyVar` here and
    /// `UnificationInferGeneralize.instantiate`'s scheme re-stamp) apply this
    /// dedup so a use site never accumulates duplicate SRTP / equality bounds.
    let addConstraintByKind (tv: TypeVar) (c: SemanticConstraint) : unit =
        if not (tv.Constraints |> List.exists (fun e -> e.Kind = c.Kind)) then
            tv.Constraints <- c :: tv.Constraints

    /// Mint a fresh instance TyVar at the current level carrying a deduped copy
    /// of `constraints`, so the use site re-evaluates SRTP / equality
    /// satisfaction against its own substitution rather than the shared prototype.
    let freshConstrainedTyVar (ctx: PassContext) (constraints: SemanticConstraint list) : TypeVar =
        let fresh = TypeVar()
        fresh.Level <- ctx.CurrentLevel

        for c in constraints do
            addConstraintByKind fresh c

        fresh

    /// Instantiate a member's type for a *call / use site*. As well as the
    /// declaring-type substitution (`typeParams ↦ args`, the declaring axis),
    /// freshen the member's OWN method typars (`methodTypars`, the method axis)
    /// — each gets a fresh `TyVar` at the current level so independent call
    /// sites instantiate the member's generic parameters separately rather than
    /// all sharing (and thereby grounding) the one registered prototype TyVar.
    /// Mirrors `UnificationInferGeneralize.instantiate`'s per-use freshening of a
    /// generalised scheme, but for a member resolved by name off its receiver.
    ///
    /// Without it, a generic member (`member _.Format(v: 'T)`) *called within the
    /// defining assembly* has its prototype `'T` unified with the first call's
    /// argument type, so `Elaborate.methodTypeParams` zonks it to a concrete type
    /// and drops it — the member emits as a single monomorphic method specialised
    /// to that first type. A second call at a different type then passes a
    /// wrong-typed argument to it, which the JIT rejects (`InvalidProgramException`).
    /// A property / field carries no method typars, so this collapses to
    /// `instantiateMember`.
    let instantiateMemberCall
        (ctx: PassContext)
        (typeParams: EqArray<string * TypeVar>, args: EqArray<SemType>)
        (methodTypars: EqArray<string * TypeVar>)
        (ty: SemType)
        : SemType =
        let subst = mkNamedTypeSubst typeParams args

        for (_, ptv) in methodTypars do
            let root = UnionFind.find ptv

            // A still-free prototype typar (the common case): mint a fresh
            // instance var. If it already links to a concrete type or is shadowed
            // by a declaring-axis arg, leave the existing mapping — substituteWith
            // follows the link / arg as before.
            if root.Link.IsNone && not (subst.ContainsKey root) then
                // Re-stamp constraints (SRTP / equality bounds) onto the fresh
                // instance so each site re-evaluates satisfaction independently.
                subst.[root] <- TyVar(freshConstrainedTyVar ctx root.Constraints)

        substituteWith subst ty

    /// Walk a class's inheritance chain for a *non-static* member named
    /// `memberName`, returning its type instantiated against the receiver's
    /// `args`. Derived members shadow inherited ones — the derived class's
    /// `Members` table is searched before recursing into `BaseType`, so an
    /// `override` wins over the parent's declaration of the same name. The
    /// parent type stored on `BaseType` is already expressed in the derived
    /// class's typar scope (NameResolution's `registerInheritedSlot`
    /// translated it), so substituting the derived class's `TypeParams ↦ args`
    /// map onto it threads generic instantiation up the chain (`IntBox` ⊳
    /// `Box<int>` resolves `Box`'s `'a` to `int`). `seen` guards a cyclic
    /// `inherit` chain. `ValueNone` when no class in the chain declares the
    /// member, or a parent name isn't a project-local class.
    /// The chain walk's full result: the *declaring* class's instantiated nominal
    /// type (`TyClass(info.TypeKey, args)` at the level the member was found) plus the
    /// member's type instantiated against that level's args. `ElaborateExpr` reads the
    /// declaring type to upcast the receiver onto the class that emits `get_<seg>`;
    /// `tryClassChainMember` keeps only the member type for inference's field-step.
    [<Struct>]
    type ChainMember =
        {
            DeclaringTy: SemType
            MemberTy: SemType
        }

    let tryClassChainMemberDecl
        (ctx: PassContext)
        (clsKey: TypeKey)
        (args: EqArray<SemType>)
        (memberName: string)
        : ChainMember voption =
        // Cycle guard keyed on the type's `TypeKey` identity (arity included), not a
        // reconstructed `name\`arity` string, so a self-inheriting arity overload
        // (`Foo\`2` : `Foo\`3`) can't collide.
        let seen = HashSet<TypeKey>()

        // Resolve by the `TypeKey` the receiver carries, never a bare-name strip:
        // an arity-overloaded class (`Fun\`2` vs `Fun\`3`, which does not resolve by
        // bare name) walks the correct chain, and the base-type recursion passes the
        // parent's key straight through with no arity round-trip.
        let rec walk (clsKey: TypeKey) (args: EqArray<SemType>) : ChainMember voption =
            if not (seen.Add clsKey) then
                ValueNone
            else
                match TypeRegistry.tryClassByKey ctx.Types clsKey with
                | ValueSome info ->
                    match info.Members |> Array.tryFind (fun m -> m.Name = memberName && not m.IsStatic) with
                    | Some m ->
                        ValueSome
                            {
                                DeclaringTy = TyClass(info.TypeKey, args)
                                MemberTy =
                                    instantiateMemberCall ctx (info.TypeParams, args) m.EffectiveMethodTypars m.Type
                            }
                    | None ->
                        match info.BaseType with
                        | ValueSome parentTy ->
                            match resolveStep (instantiateMember (info.TypeParams, args) parentTy) with
                            | TyClass(parentKey, parentArgs) -> walk parentKey parentArgs
                            | _ -> ValueNone
                        | ValueNone -> ValueNone
                | ValueNone -> ValueNone

        walk clsKey args

    let tryClassChainMember
        (ctx: PassContext)
        (clsKey: TypeKey)
        (args: EqArray<SemType>)
        (memberName: string)
        : SemType voption =
        match tryClassChainMemberDecl ctx clsKey args memberName with
        | ValueSome cm -> ValueSome cm.MemberTy
        | ValueNone -> ValueNone

    /// The bare (arity-suffix-stripped) qualified name of the canonical function
    /// interface family the codegen contract `SemType.TyFun` lowers to. The curried
    /// arity-1 `Vesper.Fun`2<'A,'B>` through the flat arity-4
    /// `Vesper.Fun`5<'A,'B,'C,'D,'E>` overload this ONE qualified name by generic
    /// arity, so every recognizer site matches this name AND discriminates on
    /// `targs.Length` (arity = length - 1, for 2..5 args) — the name alone never tells
    /// them apart. `subsumes` consults this for the
    /// arrow→`Fun` discharge rules; the unifier otherwise keeps `TyFun` structural.
    [<Literal>]
    let funInterfaceQualifiedName = "Vesper.Fun"

    /// The flat `FunN` arity a matched `Fun`(k+1)` interface instantiation denotes:
    /// `Some(genericArity - 1)` when `bareName` is the canonical `Fun` family AND the
    /// generic arity is 2..5 (⇒ arity 1..4), else `None`. The ONE predicate every
    /// arrow↔`Fun` recognizer shares (`funSlotArityOf`, `subsumes`, the Engine
    /// constraint-drain) — keeps the "name-match + 2..5 bound + length - 1" rule
    /// single-sourced so the sites cannot disagree on what counts as a `Fun` slot.
    let funSlotArityOfArgs (bareName: string) (genericArity: int) : int option =
        if bareName = funInterfaceQualifiedName && genericArity >= 2 && genericArity <= 5 then
            Some(genericArity - 1)
        else
            None

    /// Peel `k` domains off an arrow spine `TyFun(a, b)`, returning the `k+1` types
    /// `[dom0; …; dom_{k-1}; residualCodomain]` aligned to a `Fun`(k+1)`'s type args —
    /// or `None` if the spine is too short to peel `k` domains. The residual codomain
    /// is returned WHOLE (a further curried arrow — the printf `n > K` tail — is NOT
    /// peeled). `resolveStep` unwraps each codomain before the next arrow. SINGLE
    /// source of the arrow↔`Fun` spine shape shared by `subsumes` (checks each `Equal`)
    /// and the Engine constraint-drain (`unify`s each): the two MUST peel identically,
    /// else a green-lit coercion grounds to a different shape than was checked.
    /// `k >= 1` at every call site (a validated `Fun` slot is arity ≥ 1).
    let peelFunSpine (k: int) (a: SemType) (b: SemType) : SemType list option =
        let rec go i (dom: SemType) (cod: SemType) (acc: SemType list) =
            let acc = dom :: acc

            if i = k - 1 then
                Some(List.rev (cod :: acc))
            else
                match resolveStep cod with
                | TyFun(d, c) -> go (i + 1) d c acc
                | _ -> None

        go 0 a b []

    /// Fold a capability interface's two nominal faces — its BCL platform face
    /// (`System.Collections.Generic.IEnumerable\`1`) and its canonical face
    /// (`Vesper.Collections.seq`) — to the single canonical identity; a non-capability key
    /// is returned unchanged, so this is a strict generalisation of raw key `=`.
    ///
    /// Unlike the `exn === System.Exception` intrinsic (folded once at RESOLUTION via the
    /// reverse-canon map, so no raw BCL key reaches this walk), a capability is DELIBERATELY
    /// absent from that map — a `TyClass`-resolving interface there would force `IsInterface`
    /// guards into the reverse-map readers — so both faces reach the unify / subsume /
    /// overload seams un-normalized and this is the one place that reconciles them, driven by
    /// the resolved `CapabilityIds` (`Matches` — key equality against either face), never a
    /// hardcoded string.
    ///
    /// Not memoized: every caller gates behind a raw `=` first, so this runs only on a
    /// genuine mismatch (a bounded ≤5-way probe), and it can't share `IntrinsicCanonCache` —
    /// that maps `key → intrinsic canon`, under which a capability key resolves to itself.
    let capabilityCanonKey (ctx: PassContext) (key: SymbolKey) : SymbolKey =
        let caps = ctx.CapabilityIds

        // First matching capability's canonical face; `key` if none matches. A de-dented
        // match chain, not a closure/array scan, to keep the mismatch path allocation-free.
        let inline pick (cap: RuntimeNames.CapabilityIdentity voption) : SymbolKey voption =
            match cap with
            | ValueSome c when c.Matches key -> ValueSome(SymbolKey.Type(ValueOption.defaultValue c.Key c.CanonKey))
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

    /// Do two nominal keys denote the same type, reconciling a capability's two faces?
    /// Applied ONLY at the key-EQUALITY seams (`unify` / `subsumes` / overload filter /
    /// `tryUpcastWitness`), never inside `canonKey` / `subtypeNominalOf`: those drive the
    /// base/interface-chain LOOKUPS, and rewriting a BCL platform key there would erase the
    /// platform type's own bases, breaking a genuine `IEnumerator\`1 :> IEnumerator` upcast.
    /// Fast `k1 = k2` short-circuits before any capability probe.
    let sameNominalKey (ctx: PassContext) (k1: SymbolKey) (k2: SymbolKey) : bool =
        k1 = k2 || capabilityCanonKey ctx k1 = capabilityCanonKey ctx k2

    // Canonical nominal IDENTITY for subtype comparison: the type's platform-INVARIANT
    // front-end `SymbolKey` — the `.fsi` identity (`Vesper.int`, `Vesper.exn`), NOT a BCL
    // name. A primitive intrinsic binding (`type exn = (# "System.Exception" #)`,
    // prim-types-exn.fs) stays a *non-transparent* `TyConst exnKey` (Translate.fs); the
    // `exn === System.Exception` reconciliation does NOT live here — it happens once, at
    // resolution (`MetadataSymbols.tryBuildType` maps every non-interface `reverseCanon`
    // hit — sealed leaf or unsealed root — to its canon identity at surfacing time), so no
    // raw BCL key reaches this walk to reconcile. This keeps a JS build free of BCL names —
    // the base `.fs` repr only ever marks primitive-ness on JS, never the `platform` face.
    //
    // A **currency-only** map (`SymbolKey -> SymbolKey`): the reverse tier is gone, so a
    // key only ever routes FORWARD to an already-published intrinsic canon, else returns
    // itself. Resolution order:
    //   1. the compiled unit's OWN intrinsics (`ctx.Types.IntrinsicReprKeys`) — a key it
    //      holds IS a contract-stamped canon (`intrinsicKeyOf` wrote both together at
    //      registration), so it is its own canon;
    //   2. a *referenced* package's intrinsic, riding the provider as
    //      `ExternalTypeShape.Intrinsic` (whose `Id.Canon` is the authoritative key).
    // BOTH tiers answer BY KEY. Neither may project a name back out of `key` and match on
    // it: a user nominal whose SIMPLE name coincides with an intrinsic (`MyLib.int`) would
    // false-match the intrinsic and be canonicalised into it — the key's own `(ns, name,
    // arity)` misses both tiers, which is the correct answer. Memoized per `PassContext`:
    // `canonKey` runs inside the subtype recursive walk. A key that is neither caches its
    // own identity.
    let private canonKey (ctx: PassContext) (key: SymbolKey) : SymbolKey =
        match ctx.IntrinsicCanonCache.TryGetValue key with
        | true, canon -> canon
        | _ ->
            let canon =
                if ctx.Types.IntrinsicReprKeys.ContainsKey key then
                    key
                else
                    match ctx.Provider.TryLookupType key with
                    | ValueSome(ExternalTypeShape.Intrinsic { Id = { Canon = canon } }) -> SymbolKey.Type canon
                    | _ -> key

            ctx.IntrinsicCanonCache.[key] <- canon
            canon

    /// The **platform** face of an intrinsic: the runtime/BCL repr its `(# "…" #)`
    /// binding records (`"string"` ⇒ `"System.String"` on CLR, `prim-types-*.fs`).
    /// Keyed by the intrinsic's already-resolved canon `SymbolKey` — the
    /// opens-discharged identity the receiver `TyConst` carries — so BOTH halves of the
    /// forward axis answer by key and no name is ever projected back out of one: the
    /// self-compiling unit's own intrinsics from `IntrinsicReprKeys`, a referenced
    /// package's from the store's `IntrinsicForwardRepr`. Returns the key's identity name
    /// for a non-intrinsic key (a project-local / already-qualified name passes through)
    /// or an intrinsic with no repr on the compiling target (`decimal` on JS, absent from
    /// the forward map). Lets the dot-access resolvers (`resolveFieldStep`, the external
    /// instance-method probe) route an intrinsic *receiver*'s instance members through the
    /// provider keyed on the platform type name — distinct from `canonKey`'s identity axis
    /// (which stays on the `.fsi` `SymbolKey`).
    let intrinsicPlatformName (ctx: PassContext) (key: SymbolKey) : string =
        match ctx.Types.IntrinsicReprKeys.TryGetValue key with
        | true, repr -> repr.Platform
        | _ ->
            match ctx.Provider.IntrinsicForwardRepr.TryGetValue key with
            | true, platform -> platform
            | _ -> SymbolKeyOps.intrinsicName key

    /// Resolve a *receiver* type to the external `(SymbolKey, typeArgs)` a provider
    /// member lookup keys on: a non-project-local `TyClass` (a BCL / contract
    /// class) passes its resolved key through UNCHANGED, and an *intrinsic*
    /// `TyConst` whose `(# "…" #)` binding gives its platform type name
    /// (`intrinsicPlatformName`, via `prim-types-*.fs`) mints the lookup key from
    /// that repr HERE, once — consumers never see the string. `ValueNone` for a
    /// project-local class (which routes through `resolveLocalInstanceMember`), an
    /// array (`"[]"`), or byref (`"byref"`) — each keeps its own path. Shared by
    /// the dot-access resolver (`resolveFieldStep`) and the arg-aware external
    /// instance-method probe so neither re-derives the receiver→key mapping.
    let tryExternalReceiver (ctx: PassContext) (ty: SemType) : struct (SymbolKey * EqArray<SemType>) voption =
        match resolveStep ty with
        | TyClass(clsKey, typeArgs) when (TypeRegistry.tryClassByKey ctx.Types clsKey).IsNone ->
            ValueSome(struct (SymbolKey.Type clsKey, typeArgs))
        // A structural constructor (`'T []`/`byref`) is a generic intrinsic whose
        // `platform` repr (`"!0[]"`) is an IL/codegen artefact, NOT a nominal receiver
        // key — its members ride dedicated backend paths, so honour the documented
        // "keeps its own path" and decline BEFORE consulting the platform name (which
        // would otherwise differ from `name` and mis-route the lookup onto `"!0[]"`).
        | TyStructuralCtor -> ValueNone
        | TyConst(key, typeArgs) ->
            let name = SymbolKeyOps.intrinsicName key
            let platformQual = intrinsicPlatformName ctx key

            if platformQual <> name then
                ValueSome(struct (SymbolKeyOps.qualifiedTypeKey platformQual 0, typeArgs))
            else
                ValueNone
        | _ -> ValueNone

    // Surface a nominal `(canonKey, args)` for the comparison. Covers `TyConst` (so the
    // `exn` bound participates), not just `TyClass`. The surfaced `SymbolKey` is the
    // canonical intrinsic identity (`canonKey`), so two spellings of ONE intrinsic compare
    // EQUAL by exact `=`, and a non-reconciled nominal surfaces its own key. Consumers that
    // need the qualified STRING (a provider lookup, `funSlotArityOfArgs`) project it back
    // via `SymbolKeyOps.qualifiedName` at the boundary — the genuine string seam.
    let subtypeNominalOf (ctx: PassContext) (ty: SemType) : struct (SymbolKey * EqArray<SemType>) voption =
        match resolveStep ty with
        | TyClass(n, args) -> ValueSome(struct (canonKey ctx (SymbolKey.Type n), args))
        // A named DU enters the nominal subtype walk too, so its declared
        // `interface … with` impls (surfaced by `subtypeInterfacesOf` via
        // `tryInterfaceImplHostByKey`) admit `(u :> ISomeIface)` exactly like a class's.
        // (Anonymous `TyOr` unions resolve structurally in `subsumes`, never here.)
        | TyUnion(n, args) -> ValueSome(struct (canonKey ctx (SymbolKey.Type n), args))
        // A named record enters the nominal subtype walk too, so its declared
        // `interface … with` impls (surfaced by `subtypeInterfacesOf` via
        // `tryInterfaceImplHostByKey`) admit `(r :> ISomeIface)` exactly like a class's.
        | TyRecord(n, args) -> ValueSome(struct (canonKey ctx (SymbolKey.Type n), args))
        | TyConst(key, args) -> ValueSome(struct (canonKey ctx key, args))
        | _ -> ValueNone

    // The project-local registry key of a nominal `SemType` (`TyClass` / `TyUnion` /
    // `TyRecord`), or `ValueNone` for a `TyConst` / non-nominal. The subtype walk
    // resolves a local base / interface-impl host by this arity-qualified key rather
    // than a bare-name strip of the qualified canonical name: an arity-overloaded
    // local type (`Box`1`/`Box`2`) does not resolve by bare name, so a `shortName`
    // lookup would miss it and mis-route to the provider (mirrors `tryExternalReceiver`,
    // whose external test is likewise `(tryClassByKey key).IsNone`).
    let private nominalKeyOf (ty: SemType) : TypeKey voption =
        match resolveStep ty with
        | TyClass(k, _)
        | TyUnion(k, _)
        | TyRecord(k, _) -> ValueSome k
        | _ -> ValueNone

    // The instantiated declared base of the nominal the walk is expanding: the
    // project-local class table first (by `localKey`, the receiver's own arity-key),
    // then the external provider (by the canon `key`).
    // `ExternalTypeShape.Class.BaseType` carries the BCL `inherit` chain
    // (`InvalidOperationException :> Exception :> …`), written over the
    // declaring type's typars, so we apply the receiver's `args`, exactly
    // like the user-class `instantiateMember` path. Both reads are pure —
    // `tryClassByKey` is a plain lookup and `TryLookupType` is
    // contractually thread-safe and side-effect free — so `subsumes` stays
    // the read-only query the `:?` coercion site and the constraint checker
    // rely on (no undo trace).
    let private subtypeParentOf
        (ctx: PassContext)
        (localKey: TypeKey voption)
        (key: SymbolKey)
        (args: EqArray<SemType>)
        : SemType voption =
        // `key` is the canon `SymbolKey` `subtypeNominalOf` surfaces. `localKey` is the same
        // nominal's registry key when it came from a `TyClass`/`TyUnion`/`TyRecord`; the local
        // class table is keyed by that arity-qualified key, the provider by the canon key.
        let localInfo =
            match localKey with
            | ValueSome k -> TypeRegistry.tryClassByKey ctx.Types k
            | ValueNone -> ValueNone

        match localInfo with
        | ValueSome info ->
            match info.BaseType with
            | ValueSome parentTy -> ValueSome(instantiateMember (info.TypeParams, args) parentTy)
            | ValueNone -> ValueNone
        | ValueNone ->
            // `key` is already a resolved identity; the store face answers by key (an
            // open-scope funnel over its qualified name would be a no-op, since the name is
            // already fully qualified), normalising a capability's platform face internally.
            match ctx.Provider.TryLookupType key with
            | ValueSome(ExternalTypeShape.Class shape) ->
                ExternalSymbols.instantiateBaseType shape (args.AsSpan().ToArray())
            // A heritable primitive (`exn`)'s declared `inherit` parent (`obj`), so the
            // subtype walk continues `exn → obj → ⊥` off the contract chain.
            | ValueSome(ExternalTypeShape.Intrinsic { Class = ValueSome surface }) ->
                ExternalSymbols.instantiateBaseTypeFrozen surface.BaseType (args.AsSpan().ToArray())
            | _ -> ValueNone

    // The interfaces a nominal `(name, args)` declares, surfaced as instantiated
    // nominal `SemType`s (so the subtype walk treats an interface exactly like a
    // base — recomputing its own registry key / interfaces as it recurses THROUGH
    // it). Project-local `interface … with` impls first (their `Resolved` type is
    // written over the class's typars, so the receiver's `args` substitute exactly
    // as in `subtypeParentOf`), then the external provider's interface list. The
    // metadata provider pre-flattens the transitive set, but the TS-manifest
    // provider stores it un-flattened, so callers must recurse THROUGH each
    // surfaced interface for its own `extends`. An external interface is surfaced
    // as a `TyConst` (no local registry key, so its recursion routes back to the
    // provider by the canon key). Same purity contract as `subtypeParentOf`.
    let private subtypeInterfacesOf
        (ctx: PassContext)
        (localKey: TypeKey voption)
        (key: SymbolKey)
        (args: EqArray<SemType>)
        : SemType list =
        // A class, union, *or* record may declare `interface … with` impls; the subtype
        // walk treats every kind's interface list identically. Resolve the local host by
        // its arity-key (`localKey`, the receiver's own registry key), falling back to
        // the external provider by the canon `key`.
        let localHost =
            match localKey with
            | ValueSome k -> TypeRegistry.tryInterfaceImplHostByKey ctx.Types k
            | ValueNone -> ValueNone

        match localHost with
        | ValueSome info ->
            [
                for impl in info.InterfaceImpls do
                    match impl.Resolved with
                    | ValueSome ifaceTy -> yield instantiateMember (info.TypeParams, args) ifaceTy
                    | ValueNone -> ()
            ]
        | ValueNone ->
            // `key` is a resolved identity; the store face answers by key directly (its
            // qualified name is already fully qualified, so no open-scope probe applies).
            match ctx.Provider.TryLookupType key with
            | ValueSome(ExternalTypeShape.Class shape) ->
                // A class's implemented interfaces are NOMINAL types — surface them as
                // `TyClass`, exactly as they appear as a value's static type everywhere
                // else (an interface is an `ExternalTypeShape.Class` with `IsInterface`)
                // and exactly as the local branch above yields via `instantiateMember`.
                // `n` is an already-qualified interface compiled name harvested off the
                // resolved shape — not a source spelling — so the mint from the name is the
                // whole identity, and the surfaced supertype key compares EXACTLY EQUAL to
                // the one a written `A<int>` annotation resolves to (`externalTypeKey` on a
                // qualified name is this same mint).
                ExternalSymbols.instantiateInterfaces shape (args.AsSpan().ToArray())
                |> Array.toList
                |> List.map (fun (n, ta) -> TyClass(SymbolKeyOps.qualifiedTypeKeyOf n ta.Length, EqArray.ofArray ta))
            | _ -> []

    /// Find the instantiation of `src` (or one of its bases / interfaces) whose
    /// canonical nominal identity is `tgtKey`, returning that supertype's type
    /// args; `ValueNone` if `src` does not subtype `tgtKey`. Reflexive — `src`
    /// itself when its canon key is `tgtKey`. Read-only (it only *reads* the class
    /// table / provider, like `subsumes`); the caller `unify`s the returned args
    /// against the target's so a free var in the target is pinned. The single
    /// authoritative subtype walk: direct interfaces at each level (class→interface),
    /// then up the `inherit` chain (class→base, user + BCL); `subsumes`
    /// is layered on top of it. `seen` short-circuits a cyclic `inherit` chain.
    let tryUpcastWitness (ctx: PassContext) (src: SemType) (tgtKey: SymbolKey) : EqArray<SemType> voption =
        // Walk the nominal `SemType`: an interface supertype surfaced by
        // `subtypeInterfacesOf` is itself walked for its OWN `extends`-interfaces.
        // An external `interface C extends B`, `interface B extends A<int>` reaches
        // `A` only by recursing THROUGH `B` — a direct-match-only check at `C` (which
        // sees just `B`) would miss it. The metadata layer papers over this because
        // `GetInterfaces()` pre-flattens the transitive set; the TS-manifest layer
        // stores heritage un-flattened, so the walk must recurse. Staying on `SemType`
        // (not a bare `(key, args)` pair) lets each level recompute its own
        // `nominalKeyOf`, so the local base / interface-impl lookups resolve per-arity.
        // `seen` is keyed on the canon `SymbolKey`; the base / interface-impl lookups take
        // that same canon key `s` and address the store face by it.
        let rec walk (seen: HashSet<SymbolKey>) (cur: SemType) : EqArray<SemType> voption =
            match subtypeNominalOf ctx cur with
            | ValueNone -> ValueNone
            | ValueSome(struct (s, sa)) ->
                // `sameNominalKey` reconciles a capability's two faces at the MATCH only
                // (e.g. a `seq` source reaching an `IEnumerable\`1` target); the base /
                // interface walk below still keys off the RAW `s`, so a BCL platform type's
                // own bases stay reachable.
                if sameNominalKey ctx s tgtKey then
                    ValueSome sa
                elif not (seen.Add s) then
                    ValueNone
                else
                    // `localKey` is this nominal's registry key so the local base /
                    // interface-impl lookups resolve per-arity, not by bare name. The
                    // external base / interface lookups take the canon key `s` directly.
                    let localKey = nominalKeyOf cur

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

        walk (HashSet<SymbolKey>()) src

    /// Find an instance member `memberName` on an EXTERNAL SUPERTYPE of `receiver`
    /// (its base type / interfaces, transitively), returning the member paired with the
    /// type args to instantiate its signature over — the supertype's args as reached from
    /// the receiver (`Base<int>`'s `[int]` for a `Child : Base<int>` receiver). `ValueNone`
    /// when no supertype declares it. Walks SUPERTYPES ONLY: the receiver's OWN members are
    /// resolved by the caller first, and this lights up only on that miss. Needed because
    /// the TS-manifest provider stores heritage UN-FLATTENED (`FrozenInterfaces` /
    /// `FrozenBaseType`) and does not copy inherited members onto the subtype's `Members` —
    /// unlike the metadata layer, whose `GetInterfaces()` / `inherit` chain make the
    /// provider's own `TryLookupMember` already see the transitive set. Same read-only
    /// purity contract as `tryUpcastWitness`; `seen` short-circuits a cyclic chain.
    let tryExternalInheritedMember
        (ctx: PassContext)
        (receiver: SemType)
        (memberName: string)
        : struct (ExternalMember * EqArray<SemType>) voption =
        // A node's direct supertypes: its interfaces, then its declared base type.
        // Kept as `SemType`s so each carries its own `nominalKeyOf` — the local base /
        // interface-impl lookups resolve per-arity, exactly as in `tryUpcastWitness`.
        let supertypesOf (node: SemType) : SemType list =
            match subtypeNominalOf ctx node with
            | ValueNone -> []
            | ValueSome(struct (s, sa)) ->
                let localKey = nominalKeyOf node

                [
                    yield! subtypeInterfacesOf ctx localKey s sa
                    match subtypeParentOf ctx localKey s sa with
                    | ValueSome parent -> yield parent
                    | ValueNone -> ()
                ]

        let seen = HashSet<SymbolKey>()

        let rec walk (nodes: SemType list) : struct (ExternalMember * EqArray<SemType>) voption =
            match nodes with
            | [] -> ValueNone
            | node :: rest ->
                match subtypeNominalOf ctx node with
                | ValueNone -> walk rest
                | ValueSome(struct (s, sa)) ->
                    if not (seen.Add s) then
                        walk rest
                    else
                        match ctx.Provider.TryLookupMember(s, memberName) with
                        | ValueSome m when not m.IsStatic -> ValueSome(struct (m, sa))
                        // Breadth-first across the heritage graph: this node's supertypes are
                        // appended AFTER the remaining siblings, so a member on a nearer
                        // ancestor wins over one further up a parallel branch.
                        | _ -> walk (rest @ supertypesOf node)

        walk (supertypesOf receiver)

    [<RequireQualifiedAccess>]
    type NominalKind =
        | Record
        | Class
        | Union

    /// Walk a `SemType` through TyVar Links to surface a nominal shape
    /// (`TyRecord` / `TyClass` / `TyUnion`) and report which kind it is. The
    /// arg list rides along so `drainPendingDotAccess` can substitute the
    /// type's typars when resolving deferred field / member accesses.
    let rec tryResolveNominal (t: SemType) : (NominalKind * TypeKey * EqArray<SemType>) voption =
        match t with
        // The full key rides along so `resolveDotSource` can both project the simple
        // name (project-local table lookups: `ctx.Types.Record` bare, `tryUnion`
        // re-deriving arity from args) and recover the qualified name for an external
        // class's provider lookup.
        | TyRecord(n, args) -> ValueSome(NominalKind.Record, n, args)
        | TyClass(n, args) -> ValueSome(NominalKind.Class, n, args)
        | TyUnion(n, args) -> ValueSome(NominalKind.Union, n, args)
        | TyVar tv ->
            match (UnionFind.find tv).Link with
            | ValueSome target -> tryResolveNominal target
            | ValueNone -> ValueNone
        | _ -> ValueNone
