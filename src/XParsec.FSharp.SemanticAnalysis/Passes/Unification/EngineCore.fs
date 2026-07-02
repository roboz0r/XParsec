namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis


/// The unification SUBSTRATE: resolution/zonking, occurs/level adjustment,
/// substitution + member instantiation, and the nominal subtype walks
/// (`canonName`, `tryUpcastWitness`). Charter: everything the directional and
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
        | TyConst("unit", _) -> []
        | single -> [ single ]

    /// The call-site argument arity of a (shallow-resolved) .NET-style tupled
    /// argument: the tuple width, `0` for `unit`, else `1`. The count `argElemsOf`
    /// would yield, without materialising the element list — used to select a
    /// constructor overload by arity.
    let argArityOf (argTy: SemType) : int =
        match resolveStep argTy with
        | TyTuple xs -> xs.Length
        | TyConst("unit", _) -> 0
        | _ -> 1

    /// The single SemType a parameter list presents as a function argument:
    /// `unit` for none, the bare type for one, a tuple for many. Inverse of
    /// `argElemsOf`.
    let tupleOrSingle (paramTys: SemType list) : SemType =
        match paramTys with
        | [] -> BuiltinTypes.tyUnit
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
    /// `Link`s — that's `zonk`'s job). Public so Freeze can reuse the same
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
    /// Freeze can rebuild the same substitution when projecting fields off a
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
    /// class's typar scope (NameResolution's `registerInheritedSlots`
    /// translated it), so substituting the derived class's `TypeParams ↦ args`
    /// map onto it threads generic instantiation up the chain (`IntBox` ⊳
    /// `Box<int>` resolves `Box`'s `'a` to `int`). `seen` guards a cyclic
    /// `inherit` chain. `ValueNone` when no class in the chain declares the
    /// member, or a parent name isn't a project-local class.
    /// The chain walk's full result: the *declaring* class's instantiated nominal
    /// type (`TyClass(info.Key, args)` at the level the member was found) plus the
    /// member's type instantiated against that level's args. `FreezeExpr` reads the
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
        (clsName: string)
        (args: EqArray<SemType>)
        (memberName: string)
        : ChainMember voption =
        let seen = HashSet<string>()

        // Resolve by the arity-key (`name\`args.Length`), not the bare short name,
        // so an arity-overloaded class (`Fun\`2` vs `Fun\`3`, whose bare alias is
        // withdrawn) walks the correct chain. The receiver's type-arg count IS the
        // arity, so it is always in hand here.
        let rec walk (clsName: string) (args: EqArray<SemType>) : ChainMember voption =
            let arityKey = SymbolKeyOps.arityName clsName args.Length

            if not (seen.Add arityKey) then
                ValueNone
            else
                match TypeRegistry.tryClassArity ctx.Types clsName args.Length with
                | ValueSome info ->
                    match info.Members |> Array.tryFind (fun m -> m.Name = memberName && not m.IsStatic) with
                    | Some m ->
                        ValueSome
                            {
                                DeclaringTy = TyClass(info.Key, args)
                                MemberTy =
                                    instantiateMemberCall ctx (info.TypeParams, args) m.EffectiveMethodTypars m.Type
                            }
                    | None ->
                        match info.BaseType with
                        | ValueSome parentTy ->
                            match resolveStep (instantiateMember (info.TypeParams, args) parentTy) with
                            | TyClass(parentKey, parentArgs) -> walk (SymbolKeyOps.simpleName parentKey) parentArgs
                            | _ -> ValueNone
                        | ValueNone -> ValueNone
                | ValueNone -> ValueNone

        walk clsName args

    let tryClassChainMember
        (ctx: PassContext)
        (clsName: string)
        (args: EqArray<SemType>)
        (memberName: string)
        : SemType voption =
        match tryClassChainMemberDecl ctx clsName args memberName with
        | ValueSome cm -> ValueSome cm.MemberTy
        | ValueNone -> ValueNone

    /// The bare (arity-suffix-stripped) qualified name of the canonical function
    /// interface `Vesper.Fun`2` — the codegen contract `SemType.TyFun` lowers to.
    /// `subsumes` consults this for the single arrow→`Fun` discharge rule;
    /// the unifier otherwise keeps `TyFun` purely structural.
    [<Literal>]
    let funInterfaceQualifiedName = "Vesper.Fun"

    /// The canonical FLAT 2-arg function interface `Vesper.Fun2`3<'A,'B,'C>` — the
    /// arity-2 sibling of `funInterfaceQualifiedName`. A curried arrow
    /// `TyFun(a, TyFun(b,c))` subsumes into it; see the `subsumes` arm.
    [<Literal>]
    let fun2InterfaceQualifiedName = "Vesper.Fun2"

    // Canonical nominal name for subtype comparison: the type's platform-INVARIANT
    // front-end identity — the `.fsi` name itself (`int`, `exn`), NOT a BCL name.
    // A primitive intrinsic binding (`type exn = (# "System.Exception" #)`,
    // prim-types-exn.fs) stays a *non-transparent* `TyConst "exn"` (Translate.fs);
    // `canonName "exn"` is just `"exn"`. The reconciliation that used to live here —
    // `exn === System.Exception` — now runs in the OTHER direction: a metadata-
    // surfaced `TyClass("System.Exception", _)` (an `inherit`-chain element on CLR)
    // is mapped BACK to `"exn"` through the reverse `{ platform -> canon }` map
    // (`IExternalSymbolProvider.IntrinsicReverseCanon`), so the two still meet at
    // `"exn"`. This keeps a JS build free of BCL names — the base `.fs` repr only
    // ever marks primitive-ness on JS, never the `platform` face.
    //
    // Resolution order:
    //   1. the compiled unit's OWN intrinsics (`ctx.Types.IntrinsicReprTypes`) — a
    //      self-compiled `extern`'s key is its `.fsi` short name, which IS the canon;
    //   2. a *referenced* package's intrinsics, riding the provider as
    //      `ExternalTypeShape.Intrinsic` (its `canon` face = the short name);
    //   3. the reverse map, for an incoming BCL/native runtime name.
    // `n` is usually the unqualified nominal (`translateType` strips an external
    // intrinsic to its short name); the provider tier also collapses a qualified
    // `Vesper.exn` to the short `exn` via its `canon` face. Memoized per
    // `PassContext`: `canonName` runs inside the subtype recursive walk. A name that
    // is none of the above caches its own identity.
    let private canonName (ctx: PassContext) (n: string) : string =
        match ctx.IntrinsicCanonCache.TryGetValue n with
        | true, repr -> repr
        | _ ->
            let providerCanon (c: string) : string voption =
                match ctx.Provider.TryLookupType c with
                | ValueSome(ExternalTypeShape.Intrinsic(canon = canon)) -> ValueSome canon
                | _ -> ValueNone

            let repr =
                if ctx.Types.IntrinsicReprTypes.ContainsKey n then
                    // A self-compiled intrinsic: the short name it was declared under
                    // is the canonical identity (the dict's value is the platform repr).
                    n
                else
                    match OpenScope.tryResolve ctx.Resolution.OpenScope providerCanon n with
                    | ValueSome canon -> canon
                    | ValueNone ->
                        // Reverse axis: `n` may be a `platform` repr (a metadata BCL/
                        // native runtime name) whose front-end identity it reconciles
                        // with — `System.Exception` ⇒ `exn`. Built lazily (on first
                        // reverse miss) but in one shot — fully materialised AFTER
                        // NameResolution has populated `IntrinsicReprTypes`, so there is
                        // no ordering hazard with the forward-resolved names.
                        match ctx.IntrinsicReverseCanon.Value.TryGetValue n with
                        | true, canon -> canon
                        | _ -> n

            ctx.IntrinsicCanonCache.[n] <- repr
            repr

    /// The **platform** face of an intrinsic name: the runtime/BCL repr its
    /// `(# "…" #)` binding records (`"string"` ⇒ `"System.String"` on CLR,
    /// `prim-types-*.fs`; local-first, provider-fallback). Returns `n` unchanged for
    /// a name that is not a known intrinsic (so a project-local / already-qualified
    /// name passes through). Lets the dot-access resolvers (`resolveFieldStep`, the
    /// external instance-method probe) route an intrinsic *receiver*'s instance
    /// members through the provider keyed on the platform type name — distinct from
    /// `canonName`'s identity axis (which now stays on the short `.fsi` name).
    let intrinsicPlatformName (ctx: PassContext) (n: string) : string =
        match ctx.Types.IntrinsicReprTypes.TryGetValue n with
        | true, platform -> platform
        | _ ->
            let providerPlatform (c: string) : string voption =
                match ctx.Provider.TryLookupType c with
                | ValueSome(ExternalTypeShape.Intrinsic(platform = Some platform)) -> ValueSome platform
                // `platform = None`: a primitive with no repr on the compiling target
                // (`decimal` on JS) has no platform type name to key a member lookup on.
                | _ -> ValueNone

            match OpenScope.tryResolve ctx.Resolution.OpenScope providerPlatform n with
            | ValueSome platform -> platform
            | ValueNone -> n

    /// Resolve a *receiver* type to the external `(qualifiedPlatformName, typeArgs)`
    /// a provider member lookup keys on: a non-project-local `TyClass` (a BCL /
    /// contract class), or an *intrinsic* `TyConst` whose `(# "…" #)` binding gives
    /// its platform type name (`intrinsicPlatformName`, via `prim-types-*.fs`).
    /// `ValueNone` for a project-local class (which routes through
    /// `resolveLocalInstanceMember`), an array (`"[]"`), or byref (`"&"`) — each
    /// keeps its own path. Shared by the dot-access resolver
    /// (`resolveFieldStep`) and the arg-aware external instance-method probe so
    /// neither re-derives the receiver→platform-name mapping.
    let tryExternalReceiver (ctx: PassContext) (ty: SemType) : struct (string * EqArray<SemType>) voption =
        match resolveStep ty with
        | TyClass(clsKey, typeArgs) when (TypeRegistry.tryClassByKey ctx.Types clsKey).IsNone ->
            ValueSome(struct (SymbolKeyOps.qualifiedName clsKey, typeArgs))
        // A structural constructor (`'T []`/`byref`) is a generic intrinsic whose
        // `platform` repr (`"!0[]"`) is an IL/codegen artefact, NOT a nominal receiver
        // key — its members ride dedicated backend paths, so honour the documented
        // "keeps its own path" and decline BEFORE consulting the platform name (which
        // would otherwise differ from `name` and mis-route the lookup onto `"!0[]"`).
        | TyConst(name, _) when RuntimeNames.isStructuralConstructorName name -> ValueNone
        | TyConst(name, typeArgs) ->
            let platformQual = intrinsicPlatformName ctx name

            if platformQual <> name then
                ValueSome(struct (platformQual, typeArgs))
            else
                ValueNone
        | _ -> ValueNone

    // Surface a nominal `(name, args)` for the comparison. Covers `TyConst`
    // (so the `exn` bound participates), not just `TyClass`.
    let subtypeNominalOf (ctx: PassContext) (ty: SemType) : struct (string * EqArray<SemType>) voption =
        match resolveStep ty with
        // Surface the *qualified* canonical name so an external `TyClass`
        // (`System.Exception`) reconciles with the `exn` `TyConst` through
        // `canonName`'s repr map. `parentOf` splits the simple segment back off
        // for the project-local class lookup.
        | TyClass(n, args) -> ValueSome(struct (canonName ctx (SymbolKeyOps.qualifiedName n), args))
        // A named DU enters the nominal subtype walk too, so its declared
        // `interface … with` impls (surfaced by `subtypeInterfacesOf` via
        // `tryInterfaceImplHost`) admit `(u :> ISomeIface)` exactly like a class's.
        // (Anonymous `TyOr` unions resolve structurally in `subsumes`, never here.)
        | TyUnion(n, args) -> ValueSome(struct (canonName ctx (SymbolKeyOps.qualifiedName n), args))
        // A named record enters the nominal subtype walk too, so its declared
        // `interface … with` impls (surfaced by `subtypeInterfacesOf` via
        // `tryInterfaceImplHost`) admit `(r :> ISomeIface)` exactly like a class's.
        | TyRecord(n, args) -> ValueSome(struct (canonName ctx (SymbolKeyOps.qualifiedName n), args))
        | TyConst(n, args) -> ValueSome(struct (canonName ctx n, args))
        | _ -> ValueNone

    // The instantiated declared base of nominal `(name, args)`: the
    // project-local class table first, then the external provider.
    // `ExternalTypeShape.Class.BaseType` carries the BCL `inherit` chain
    // (`InvalidOperationException :> Exception :> …`), written over the
    // declaring type's typars, so we apply the receiver's `args`, exactly
    // like the user-class `instantiateMember` path. Both reads are pure —
    // `ctx.Types.Class` is a plain lookup and `TryLookupType` is
    // contractually thread-safe and side-effect free — so `subsumes` stays
    // the read-only query the `:?` coercion site and the constraint checker
    // rely on (no undo trace).
    let private subtypeParentOf (ctx: PassContext) (name: string) (args: EqArray<SemType>) : SemType voption =
        // `name` is the qualified canonical name `subtypeNominalOf` surfaces (so it
        // feeds `canonName`'s repr map); the project-local class table is keyed by the
        // bare simple segment, the provider by the qualified name. Re-derive the
        // simple segment through the shared `shortName` rule rather than a
        // hand-rolled last-`.` split — `shortName` also strips the `` `N `` arity
        // suffix the qualified name retains, so a generic local class
        // (`MyNs.Box`1`) resolves to its bare table key (`Box`) instead of missing.
        let simple = SymbolKeyOps.shortName name

        match ctx.Types.Class.TryGetValue simple with
        | true, info ->
            match info.BaseType with
            | ValueSome parentTy -> ValueSome(instantiateMember (info.TypeParams, args) parentTy)
            | ValueNone -> ValueNone
        | false, _ ->
            match ctx.Provider.TryLookupType name with
            | ValueSome(ExternalTypeShape.Class shape) ->
                ExternalSymbols.instantiateBaseType shape (args.AsSpan().ToArray())
            | _ -> ValueNone

    // The interfaces a nominal `(name, args)` declares, surfaced as
    // `(canonical-name, instantiated-args)` nominal pairs (the same form
    // `subtypeNominalOf` yields, so the subtype walk treats an interface exactly
    // like a base). Project-local `interface … with` impls first (their
    // `Resolved` type is written over the class's typars, so the receiver's
    // `args` substitute exactly as in `subtypeParentOf`), then the external
    // provider's frozen interface list (already a full transitive set from the
    // metadata `GetInterfaces()`). Same purity contract as `subtypeParentOf`.
    let private subtypeInterfacesOf
        (ctx: PassContext)
        (name: string)
        (args: EqArray<SemType>)
        : struct (string * EqArray<SemType>) list =
        let simple = SymbolKeyOps.shortName name

        // A class, union, *or* record may declare `interface … with` impls; the subtype
        // walk treats every kind's interface list identically.
        match TypeRegistry.tryInterfaceImplHost ctx.Types simple with
        | ValueSome info ->
            [
                for impl in info.InterfaceImpls do
                    match impl.Resolved with
                    | ValueSome ifaceTy ->
                        match subtypeNominalOf ctx (instantiateMember (info.TypeParams, args) ifaceTy) with
                        | ValueSome p -> yield p
                        | ValueNone -> ()
                    | ValueNone -> ()
            ]
        | ValueNone ->
            match ctx.Provider.TryLookupType name with
            | ValueSome(ExternalTypeShape.Class shape) ->
                ExternalSymbols.instantiateInterfaces shape (args.AsSpan().ToArray())
                |> Array.toList
                |> List.map (fun (n, ta) -> struct (canonName ctx n, EqArray.ofArray ta))
            | _ -> []

    /// Find the instantiation of `src` (or one of its bases / interfaces) whose
    /// canonical nominal name is `tgtName`, returning that supertype's type
    /// args; `ValueNone` if `src` does not subtype `tgtName`. Reflexive — `src`
    /// itself when its name is `tgtName`. Read-only (it only *reads* the class
    /// table / provider, like `subsumes`); the caller `unify`s the returned args
    /// against the target's so a free var in the target is pinned. The single
    /// authoritative subtype walk: direct interfaces at each level (class→interface),
    /// then up the `inherit` chain (class→base, user + BCL); `subsumes`
    /// is layered on top of it. `seen` short-circuits a cyclic `inherit` chain.
    let tryUpcastWitness (ctx: PassContext) (src: SemType) (tgtName: string) : EqArray<SemType> voption =
        let nominalOf = subtypeNominalOf ctx

        let rec walk (seen: HashSet<string>) (cur: SemType) : EqArray<SemType> voption =
            match nominalOf cur with
            | ValueNone -> ValueNone
            | ValueSome(struct (s, sa)) ->
                if s = tgtName then
                    ValueSome sa
                elif not (seen.Add s) then
                    ValueNone
                else
                    let viaIface =
                        subtypeInterfacesOf ctx s sa
                        |> List.tryPick (fun (struct (iname, ia)) -> if iname = tgtName then Some ia else None)

                    match viaIface with
                    | Some ia -> ValueSome ia
                    | None ->
                        match subtypeParentOf ctx s sa with
                        | ValueSome parentInstance -> walk seen parentInstance
                        | ValueNone -> ValueNone

        walk (HashSet<string>()) src

    [<RequireQualifiedAccess>]
    type NominalKind =
        | Record
        | Class
        | Union

    /// Walk a `SemType` through TyVar Links to surface a nominal shape
    /// (`TyRecord` / `TyClass` / `TyUnion`) and report which kind it is. The
    /// arg list rides along so `drainPendingDotAccess` can substitute the
    /// type's typars when resolving deferred field / member accesses.
    let rec tryResolveNominal (t: SemType) : (NominalKind * SymbolKey * EqArray<SemType>) voption =
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
