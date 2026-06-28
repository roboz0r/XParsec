namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

module UnificationEngine =

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
        | TyConst(n, args) -> TyConst(n, EqArray.map zonk args)
        | TyFun(a, r) -> TyFun(zonk a, zonk r)
        | TyTuple items -> TyTuple(EqArray.map zonk items)
        | TyRecord(n, args) -> TyRecord(n, EqArray.map zonk args)
        | TyUnion(n, args) -> TyUnion(n, EqArray.map zonk args)
        | TyClass(n, args) -> TyClass(n, EqArray.map zonk args)
        // Rebuild through `mkUnion`, not a bare `EqArray.map`: resolving a member
        // can collapse the set (`'T | string` with `'T := string` → `string | string`
        // → `string`) or reorder it, and only `mkUnion` re-establishes the canonical
        // (sorted/deduped/collapsed) form the equality layer's `n1 = n2` relies on.
        | TyOr members -> members.Map zonk
        | TyUnknown _ -> t
        // Post-freeze leaf; never produced during inference. Passthrough.
        | TyTypar _ -> t

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
    let private migrateBounds (target: TypeVar) (source: TypeVar) : unit =
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
        | TyConst(_, args) -> EqArray.exists (occursAndAdjust target) args
        | TyFun(a, r) -> occursAndAdjust target a || occursAndAdjust target r
        | TyTuple items -> EqArray.exists (occursAndAdjust target) items
        | TyRecord(_, args) -> EqArray.exists (occursAndAdjust target) args
        | TyUnion(_, args) -> EqArray.exists (occursAndAdjust target) args
        | TyClass(_, args) -> EqArray.exists (occursAndAdjust target) args
        | TyOr members -> EqArray.exists (occursAndAdjust target) members.Members
        | TyUnknown _ -> false
        // A post-freeze typar leaf is not a TyVar and holds none — never occurs.
        | TyTypar _ -> false

    /// Two non-equal measures emit a diagnostic; one of them is kept on the
    /// survivor so further unifications against it stay coherent.
    let private mergeUnits
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
        | TyConst(n, args) -> TyConst(n, EqArray.map (substituteWith subst) args)
        | TyFun(a, r) -> TyFun(substituteWith subst a, substituteWith subst r)
        | TyTuple xs -> TyTuple(EqArray.map (substituteWith subst) xs)
        | TyRecord(n, args) -> TyRecord(n, EqArray.map (substituteWith subst) args)
        | TyUnion(n, args) -> TyUnion(n, EqArray.map (substituteWith subst) args)
        | TyClass(n, args) -> TyClass(n, EqArray.map (substituteWith subst) args)
        // Through `mkUnion`: substituting a typar member can collapse / reorder the
        // set, so re-canonicalise rather than `EqArray.map` (see `zonk`).
        | TyOr members -> members.Map(substituteWith subst)
        | TyUnknown _ -> t
        // Post-freeze leaf; never produced during inference. Passthrough.
        | TyTypar _ -> t

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
                                MemberTy = instantiateMemberCall ctx (info.TypeParams, args) m.MethodTypeParams m.Type
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

    /// The result of the subtyping query `subsumes`: `Equal` when the two
    /// types are the same nominal type (with invariant args in v1), `Subtype`
    /// when `src` is a strict descendant of `tgt` — either along the `inherit`
    /// chain or because `src` (or a base) declares `tgt` as an interface —
    /// `Unrelated` otherwise.
    [<RequireQualifiedAccess>]
    type SubsumeOutcome =
        | Equal
        | Subtype
        | Unrelated

    /// The bare (arity-suffix-stripped) qualified name of the canonical function
    /// interface `Vesper.Fun`2` — the codegen contract `SemType.TyFun` lowers to.
    /// `subsumes` consults this for the single arrow→`Fun` discharge rule;
    /// the unifier otherwise keeps `TyFun` purely structural.
    [<Literal>]
    let private funInterfaceQualifiedName = "Vesper.Fun"

    /// The canonical FLAT 2-arg function interface `Vesper.Fun2`3<'A,'B,'C>` — the
    /// arity-2 sibling of `funInterfaceQualifiedName`. A curried arrow
    /// `TyFun(a, TyFun(b,c))` subsumes into it; see the `subsumes` arm.
    [<Literal>]
    let private fun2InterfaceQualifiedName = "Vesper.Fun2"

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
    let private subtypeNominalOf (ctx: PassContext) (ty: SemType) : struct (string * EqArray<SemType>) voption =
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

        // A class *or* a union may declare `interface … with` impls; the subtype
        // walk treats both kinds' interface lists identically.
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

    /// Subtyping query distinct from `unify`: does a value of type `src`
    /// coerce to the statically-known type `tgt`? A **pure read** of
    /// `ctx.Types.Class` — never mutates `Link` / `Constraints`, so it's safe
    /// to call from the read-only coercion site (`:?`) without an undo trace
    ///
    /// Reflexivity is `Equal` (callers distinguish a redundant cast from a real
    /// one); the parent-chain / interface walk yields `Subtype`. Args are
    /// invariant in v1 — `List<Circle>` does not subsume `List<Shape>`. The
    /// argument / `:>` coercion sites use `tryCoerceUpcast` instead, which
    /// *unifies* the witness's type args (so a free var in the target, e.g. the
    /// `_` in `this :> seq<_>`, is pinned).
    ///
    /// Layered on `tryUpcastWitness` — the witness is reflexive and stops at the
    /// first name match, exactly `subsumes`' semantics — so the subtype traversal
    /// lives in one place. `src` subsumes `tgt` iff `src` reaches `tgt`'s nominal
    /// with invariant-equal args; reflexive (same root nominal) is `Equal`, a
    /// base/interface hop is `Subtype`. Non-nominal operands fall back to identity.
    ///
    /// This is the union-aware dispatcher: a `TyOr` on either side resolves
    /// structurally (member set ⊆ member set, value ∈ member set) and the
    /// non-union case delegates to `subsumesNominal`, which carries the original
    /// inherit/interface walk. Splitting the two keeps the nominal traversal flat
    /// rather than nested under a union fallthrough.
    let rec subsumes (ctx: PassContext) (src: SemType) (tgt: SemType) : SubsumeOutcome =
        match resolveStep src, resolveStep tgt with
        // union → union (`A | B ≤ A | B | C`, order-insensitive): every member of
        // the source must land in some member of the target. Identical canonical
        // member sets are `Equal` (reflexivity, sound because both are sorted/deduped);
        // a member-wise subset is `Subtype`. `A | B ⋠ A | C` ⇒ `Unrelated`.
        | TyOr ss, TyOr ts ->
            let ssm = ss.Members
            let tsm = ts.Members

            if ssm = tsm then
                SubsumeOutcome.Equal
            elif
                ssm
                |> EqArray.forall (fun s ->
                    tsm |> EqArray.exists (fun t -> subsumes ctx s t <> SubsumeOutcome.Unrelated)
                )
            then
                SubsumeOutcome.Subtype
            else
                SubsumeOutcome.Unrelated
        // member → union (`A ≤ A | B`): `Equal` when `src` *is* a member by
        // structural `=`, `Subtype` when it subsumes into some member (e.g. a
        // subclass of a member). `src` is necessarily non-union here (the
        // union → union arm above caught that case).
        | src', TyOr ts ->
            let tsm = ts.Members

            if tsm |> EqArray.exists (fun t -> t = src') then
                SubsumeOutcome.Equal
            elif tsm |> EqArray.exists (fun t -> subsumes ctx src' t <> SubsumeOutcome.Unrelated) then
                SubsumeOutcome.Subtype
            else
                SubsumeOutcome.Unrelated
        // union → member/other (`A | B ⋠ A`): coerces only when *every* member
        // subsumes the target (target = `obj` or a wider type) — otherwise the
        // consumer must narrow first. `never` (`TyOr []`) subsumes into everything
        // (`forall` over the empty set).
        | TyOr ss, _ ->
            if
                ss.Members
                |> EqArray.forall (fun s -> subsumes ctx s tgt <> SubsumeOutcome.Unrelated)
            then
                SubsumeOutcome.Subtype
            else
                SubsumeOutcome.Unrelated
        // The arrow↔`Fun` correspondence: a structural arrow
        // `TyFun(a,b)` IS a subtype of the canonical `Vesper.Fun`2<a,b>` interface.
        // This is the ONE place the two layers meet — the unifier keeps seeing
        // `TyFun` as the structural arrow everywhere else (function-representation
        // §"Two layers"); only a `'TF :> Fun<…>` constrained-typar slot discharges
        // through here. Args are invariant (same rule as `subsumesNominal`): the
        // arrow's domain/codomain must each be `Equal` to the `Fun`'s type args. This
        // is a read-only check, not a `unify` — grounding a still-free `Fun`-arg FROM
        // the arrow is deferred, not yet exercised. Arity-1 `Fun`2`
        // — a curried `TyFun(a, TyFun(b,c))` against `Fun`2` falls out naturally
        // (codomain = the inner arrow), with no flat-`Fun2`/`Fun3` special-casing.
        | TyFun(a, b), (TyClass(tk, targs)) when
            SymbolKeyOps.bareName (SymbolKeyOps.qualifiedName tk) = funInterfaceQualifiedName
            && targs.Length = 2
            ->
            if
                subsumes ctx a targs.[0] = SubsumeOutcome.Equal
                && subsumes ctx b targs.[1] = SubsumeOutcome.Equal
            then
                SubsumeOutcome.Subtype
            else
                SubsumeOutcome.Unrelated
        // The FLAT-2 arrow↔`Fun2` correspondence: a CURRIED arrow
        // `TyFun(a, TyFun(b,c))` IS a subtype of the canonical
        // `Vesper.Fun2`3<a,b,c>` interface — a saturated 2-arg slot. Sibling of the
        // arity-1 `Vesper.Fun` arm above (`Fun2` does NOT inherit `Fun`,
        // so the two arms are independent). Same read-only, invariant-arg discipline:
        // the two arrow domains and the final codomain must each be `Equal` to the
        // `Fun2`'s three type args. The caller records the arity-2 verdict for the
        // lambda node (`inferApp`), keyed for the value-struct flat-`Invoke` lowering.
        | TyFun(a, TyFun(b, c)), (TyClass(tk, targs)) when
            SymbolKeyOps.bareName (SymbolKeyOps.qualifiedName tk) = fun2InterfaceQualifiedName
            && targs.Length = 3
            ->
            if
                subsumes ctx a targs.[0] = SubsumeOutcome.Equal
                && subsumes ctx b targs.[1] = SubsumeOutcome.Equal
                && subsumes ctx c targs.[2] = SubsumeOutcome.Equal
            then
                SubsumeOutcome.Subtype
            else
                SubsumeOutcome.Unrelated
        // Neither operand is a union: the nominal subtype walk.
        | _ -> subsumesNominal ctx src tgt

    /// The nominal core of `subsumes` (no union operands): `src` subsumes `tgt`
    /// iff `src` reaches `tgt`'s nominal via the inherit/interface witness with
    /// invariant-equal args — reflexive (same root nominal) is `Equal`, a
    /// base/interface hop is `Subtype`. Non-nominal operands (vars, funcs, tuples)
    /// fall back to identity. Mutually recursive with `subsumes` only through the
    /// invariant-arg check, which may itself face union args.
    and subsumesNominal (ctx: PassContext) (src: SemType) (tgt: SemType) : SubsumeOutcome =
        match subtypeNominalOf ctx src, subtypeNominalOf ctx tgt with
        | ValueSome(struct (s, _)), ValueSome(struct (t, ta)) ->
            match tryUpcastWitness ctx src t with
            // v1 args are invariant: every witnessed arg must itself be `Equal`.
            // The length guard is belt-and-suspenders — a name match implies equal
            // arity in a well-formed program.
            | ValueSome wargs when
                wargs.Length = ta.Length
                && EqArray.forall2 (fun a b -> subsumes ctx a b = SubsumeOutcome.Equal) wargs ta
                ->
                if s = t then
                    SubsumeOutcome.Equal
                else
                    SubsumeOutcome.Subtype
            | _ -> SubsumeOutcome.Unrelated
        | _ ->
            // Non-nominal operands (vars, funcs, tuples): identity only.
            if resolveStep src = resolveStep tgt then
                SubsumeOutcome.Equal
            else
                SubsumeOutcome.Unrelated

    /// The flat `FunN` arity a parameter slot constrains its argument to,
    /// or `ValueNone` for an ordinary (non-`Fun`-bounded) parameter. A combinator
    /// param `'TF :> Fun<a,b>` is arity 1; `'TF :> Fun2<a,b,c>` is arity 2. The
    /// `subsumes` arm decides the arrow↔`FunN` correspondence; this reads the SAME
    /// nominal bound so `inferApp` can record the verdict against the lambda
    /// argument's node (the value-struct flat-`Invoke` lowering reads it at codegen).
    /// Reads the coercion bound off the still-free typar's union-find root.
    let funSlotArityOf (param: SemType) : int voption =
        match resolveStep param with
        | TyVar tv ->
            let root = UnionFind.find tv

            root.Constraints
            |> List.tryPick (fun c ->
                match c.Kind with
                | SemanticConstraintKind.Coercion target ->
                    match resolveStep target with
                    | TyClass(tk, targs) ->
                        let bare = SymbolKeyOps.bareName (SymbolKeyOps.qualifiedName tk)

                        if bare = funInterfaceQualifiedName && targs.Length = 2 then
                            Some 1
                        elif bare = fun2InterfaceQualifiedName && targs.Length = 3 then
                            Some 2
                        else
                            None
                    | _ -> None
                | _ -> None
            )
            |> function
                | Some n -> ValueSome n
                | None -> ValueNone
        | _ -> ValueNone

    [<RequireQualifiedAccess>]
    type private NominalKind =
        | Record
        | Class
        | Union

    /// Walk a `SemType` through TyVar Links to surface a nominal shape
    /// (`TyRecord` / `TyClass` / `TyUnion`) and report which kind it is. The
    /// arg list rides along so `drainPendingDotAccess` can substitute the
    /// type's typars when resolving deferred field / member accesses.
    let rec private tryResolveNominal (t: SemType) : (NominalKind * SymbolKey * EqArray<SemType>) voption =
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

    let private fieldLookup (fields: RecordFieldInfo[]) (name: string) : SemType voption =
        match fields |> Array.tryFind (fun f -> f.Name = name) with
        | Some f -> ValueSome f.Type
        | None -> ValueNone

    /// Instance-member lookup, shared by the class and union arms (their
    /// `Members` arrays are the same `TypeMemberInfo[]`).
    let private memberLookup (members: TypeMemberInfo[]) (name: string) : SemType voption =
        match members |> Array.tryFind (fun m -> m.Name = name && not m.IsStatic) with
        | Some m -> ValueSome m.Type
        | None -> ValueNone

    /// The outcome of resolving a TyVar's link target to a dot-access source.
    /// `NotNominal` — not a record/class/union, nothing to drain.
    /// `UnknownType` — named a nominal type the registry doesn't know.
    /// `Resolved` — carries the member-noun used in diagnostics, the
    /// typar→arg substitution, and a name→type lookup over the members.
    [<RequireQualifiedAccess>]
    type private DotSource =
        | NotNominal
        | UnknownType of name: string * kind: string
        | Resolved of
            name: string *
            memberNoun: string *
            subst: Dictionary<TypeVar, SemType> *
            lookup: (string -> SemType voption)
        /// A project-local class: member lookup walks the inheritance chain, so
        /// it can't be expressed as the single `subst` + `lookup` pair the
        /// `Resolved` shape carries. The drain defers to `tryClassChainMember`,
        /// which threads the substitution up the chain per parent.
        | ClassChain of name: string * args: EqArray<SemType>
        /// An *external* class/interface (not in `ctx.Types.Class`): a deferred
        /// dot-access whose receiver TyVar resolved to a BCL/contract nominal
        /// (`System.Collections.IEqualityComparer`). The drain resolves the
        /// member through the provider — the deferred mirror of `resolveFieldStep`'s
        /// external arm — keyed by the *qualified* name (`qualName`).
        | ExternalClass of qualName: string * args: EqArray<SemType>

    let private resolveDotSource (ctx: PassContext) (linkTarget: SemType) : DotSource =
        match tryResolveNominal linkTarget with
        | ValueNone -> DotSource.NotNominal
        | ValueSome(NominalKind.Record, key, args) ->
            let name = SymbolKeyOps.simpleName key

            match ctx.Types.Record.TryGetValue name with
            | true, info ->
                DotSource.Resolved(name, "field", mkNamedTypeSubst info.TypeParams args, fieldLookup info.Fields)
            | false, _ -> DotSource.UnknownType(name, "record")
        | ValueSome(NominalKind.Class, key, args) ->
            let name = SymbolKeyOps.simpleName key

            if ctx.Types.Class.ContainsKey name then
                DotSource.ClassChain(name, args)
            else
                // Not project-local — an external (BCL/contract) class or interface
                // whose member resolves through the provider by its qualified name.
                DotSource.ExternalClass(SymbolKeyOps.qualifiedName key, args)
        | ValueSome(NominalKind.Union, key, args) ->
            let name = SymbolKeyOps.simpleName key

            match TypeRegistry.tryUnion ctx.Types name args.Length with
            | ValueSome info ->
                DotSource.Resolved(
                    name,
                    "instance member",
                    mkNamedTypeSubst info.TypeParams args,
                    memberLookup info.Members
                )
            | ValueNone -> DotSource.UnknownType(name, "union")

    /// `Defer` is the "I don't know yet" answer: the target is still free
    /// (or compound-with-free-args) and a future unification might pin it.
    /// `drainConstraints` keeps deferred constraints on the TyVar so they
    /// re-fire on the next `Link` change.
    type ConstraintOutcome =
        | Satisfied
        | Violated
        | Defer

    /// `string` is excluded and handled separately since it's a reference type.
    let private primitiveValueTypes =
        Set.ofList [ "int"; "int64"; "byte"; "bool"; "float"; "float32"; "char"; "unit" ]

    let constraintKindName (k: SemanticConstraintKind) : string =
        match k with
        | SemanticConstraintKind.Equality -> "equality"
        | SemanticConstraintKind.Comparison -> "comparison"
        | SemanticConstraintKind.Struct -> "struct"
        | SemanticConstraintKind.ReferenceType -> "not struct"
        | SemanticConstraintKind.Nullness -> "null"
        | SemanticConstraintKind.NotNull -> "not null"
        | SemanticConstraintKind.Coercion target -> sprintf "subtype of %A" target

    /// Decompose a nominal `SemType` into its `(kind, key, arity)` — the
    /// key-preserving companion to `tryResolveNominal` (which projects to a simple
    /// string). Used by the DEBUG asm-invariant guard, which needs the full
    /// `SymbolKey` (incl. home assembly) the unify arms compare on.
    let private nominalKey (t: SemType) : struct (NominalKind * SymbolKey * int) voption =
        match t with
        | TyRecord(k, a) -> ValueSome(struct (NominalKind.Record, k, a.Length))
        | TyUnion(k, a) -> ValueSome(struct (NominalKind.Union, k, a.Length))
        | TyClass(k, a) -> ValueSome(struct (NominalKind.Class, k, a.Length))
        | _ -> ValueNone

#if DEBUG
    /// Invariant guard. The nominal arms in `unify`
    /// compare the *full* `SymbolKey` (incl. `asm`, the home assembly), so two
    /// nominals of the same kind / namespace / name / arity that still fail to unify
    /// can only differ in `asm` — the silent failure mode where one mint path stamped
    /// `asm = None`/`Some "X"` and another `Some "Y"` for the same type.
    /// `recordKeyOrigin` polices local uniqueness but not cross-producer asm
    /// agreement, so surface a drifting mint path loudly in DEBUG rather than letting
    /// it read as a bare "type mismatch". Compiled out of release builds.
    let private checkAsmInvariant (a: SemType) (b: SemType) : unit =
        match nominalKey a, nominalKey b with
        | ValueSome(struct (kind1, k1, ar1)), ValueSome(struct (kind2, k2, ar2)) when
            kind1 = kind2
            && ar1 = ar2
            && k1 <> k2
            && SymbolKeyOps.qualifiedName k1 = SymbolKeyOps.qualifiedName k2
            ->
            failwithf
                "SymbolKey asm-invariant violated: %A and %A name the same type (%s) but carry different home assemblies — a mint path disagrees on the home assembly."
                k1
                k2
                (SymbolKeyOps.qualifiedName k1)
        | _ -> ()
#endif

    /// `obj` (either the user-facing `TyConst "obj"` `translateType` produces, or
    /// the provider's `TyClass "System.Object"` before `normalizeObj` bridges it).
    /// The universal supertype — every value implicitly upcasts (boxing) into it.
    let isObjType (t: SemType) : bool =
        match t with
        | TyConst(n, a) when a.IsEmpty && n = RuntimeNames.objAbbrevName -> true
        | TyClass(k, a) when a.IsEmpty && RuntimeNames.isSystemObjectKey k -> true
        | _ -> false

    /// THE obj-absorption policy: `true` when `expected` is the universal `obj`
    /// supertype (after one resolve step), so an argument coercion must ACCEPT the
    /// actual *without* unifying — the implicit boxing upcast F# inserts, which must
    /// never ground the actual's typar (codegen materialises the box —
    /// `EmitPattern.boxArgIntoObjParam`). The single home of the rule, applied by
    /// `tryCoerceUpcast` (the eager argument / `:>` path) and `unifyArgCoerce` /
    /// `unifyAppliedSig` (the in-`unify`-group deferred dot-access drain); the two
    /// coercion walkers exist only because they sit either side of `tryCoerceUpcast`
    /// in declaration order, not because the policy differs.
    let absorbsAsObj (expected: SemType) : bool = isObjType (resolveStep expected)

    /// Bridge an external signature's `System.Object` (minted by the provider as
    /// `TyClass("System.Object", [])`, since it isn't in `IntrinsicRepr.defaults`)
    /// to the user-facing `TyConst "obj"` that `translateType` produces — without
    /// this an external method's `obj` parameter (`IEqualityComparer.Equals(obj,
    /// obj)`) fails to unify with an `obj`-typed argument. Shared by the deferred
    /// drain here and `Unification`'s interface-conformance path; defined here so
    /// both — the drain upstream of that module and the conformance check — can
    /// normalise the external signatures they open.
    let rec normalizeObj (t: SemType) : SemType =
        match t with
        | TyClass(n, args) when args.IsEmpty && RuntimeNames.isSystemObjectKey n ->
            TyConst(RuntimeNames.objAbbrevName, EqArray.empty)
        | TyClass(n, args) -> TyClass(n, EqArray.map normalizeObj args)
        | TyFun(a, r) -> TyFun(normalizeObj a, normalizeObj r)
        | TyTuple xs -> TyTuple(EqArray.map normalizeObj xs)
        | TyRecord(n, args) -> TyRecord(n, EqArray.map normalizeObj args)
        | TyUnion(n, args) -> TyUnion(n, EqArray.map normalizeObj args)
        | TyConst(n, args) -> TyConst(n, EqArray.map normalizeObj args)
        // Rebuild through `mkUnion`: normalising a member to `obj` can collapse
        // the set (`System.Object | obj` → `obj`), so re-canonicalise rather than
        // a bare member map (see `zonk`). Reachable once external/provider
        // signatures carry a `TyOr` (the TS symbol-provider plan).
        | TyOr members -> members.Map normalizeObj
        | other -> other

    let rec unify (ctx: PassContext) (key: NodeKey) (a: SemType) (b: SemType) =
        let a = resolveStep a
        let b = resolveStep b

        match a, b with
        // An unresolved contract head unifies with nothing.
        // Report at the use site and stop — the other side is left untouched (no Link),
        // so one broken head can't cascade into a wrong inference elsewhere.
        | TyUnknown name, _
        | _, TyUnknown name ->
            ctx.Error(
                key,
                sprintf
                    "Type '%s' could not be resolved during contract extraction — is a package dependency missing?"
                    name
            )
        | TyConst(n1, a1), TyConst(n2, a2) when n1 = n2 && a1.Length = a2.Length -> unifyArgs ctx key a1 a2
        | TyRecord(n1, a1), TyRecord(n2, a2) when n1 = n2 && a1.Length = a2.Length -> unifyArgs ctx key a1 a2
        | TyUnion(n1, a1), TyUnion(n2, a2) when n1 = n2 && a1.Length = a2.Length -> unifyArgs ctx key a1 a2
        | TyClass(n1, a1), TyClass(n2, a2) when n1 = n2 && a1.Length = a2.Length -> unifyArgs ctx key a1 a2
        | TyFun(a1, r1), TyFun(a2, r2) ->
            unify ctx key a1 a2
            unify ctx key r1 r2
        | TyTuple xs, TyTuple ys when xs.Length = ys.Length -> unifyArgs ctx key xs ys
        // Anonymous unions unify by *structural equality only* — members are
        // canonical (sorted/deduped by `mkUnion`), so equal unions have identical
        // member vectors and unify positionally. Membership/assignability
        // (`int ≤ int | string`) is NOT handled here: it belongs to the directional
        // `subsumes` layer, never the symmetric core (the principality rule — this
        // arm never widens `int` into `int | string`).
        | TyOr m1, TyOr m2 when m1.Members.Length = m2.Members.Length -> unifyArgs ctx key m1.Members m2.Members
        | TyVar tv1, TyVar tv2 when System.Object.ReferenceEquals(tv1, tv2) -> ()
        | TyVar tv1, TyVar tv2 ->
            let r1 = UnionFind.find tv1
            let r2 = UnionFind.find tv2
            let unitsA = r1.Units
            let unitsB = r2.Units
            let linkA = r1.Link
            let linkB = r2.Link
            UnionFind.union r1 r2
            // After union, exactly one of r1/r2 still has Parent = ValueNone.
            let newRoot = UnionFind.find r1

            let merged =
                if System.Object.ReferenceEquals(newRoot, r1) then
                    r2
                else
                    r1

            migrateBounds newRoot merged
            mergeUnits ctx key newRoot unitsA unitsB
            // If both sides carried links, unify them so the carriers agree.
            match linkA, linkB with
            | ValueNone, ValueNone -> ()
            | ValueSome t, ValueNone
            | ValueNone, ValueSome t ->
                newRoot.Link <- ValueSome t
                drainAll ctx key newRoot t
            | ValueSome a, ValueSome b ->
                newRoot.Link <- linkA
                unify ctx key a b
                drainAll ctx key newRoot a
        | TyVar tv, other
        | other, TyVar tv ->
            let root = UnionFind.find tv

            if occursAndAdjust root other then
                ctx.Error(
                    key,
                    sprintf "Occurs check: cannot construct infinite type %A = %A" (zonk (TyVar root)) (zonk other)
                )
            else
                // Linking to a plain TyConst (a dimensionless carrier) when
                // the variable is already known to be measured is a
                // dimensionless-vs-measured mismatch.
                match root.Units, other with
                | ValueSome m, TyConst _ when not m.IsDimensionless ->
                    ctx.Error(key, sprintf "Dimensionless %A used where <%O> expected" other m)
                | _ -> ()

                root.Link <- ValueSome other
                drainAll ctx key root other
        | _ ->
#if DEBUG
            checkAsmInvariant a b
#endif
            ctx.Error(key, sprintf "Type mismatch: %A vs %A" (zonk a) (zonk b))

    /// Unify two same-length type-argument vectors positionally — the shared body
    /// of the `TyConst` / `TyRecord` / `TyUnion` / `TyClass` / `TyTuple` arms (each
    /// already guards `length` equality). In the `unify` rec group so it stays a
    /// direct call with no per-`unify` closure allocation on the hot path.
    and private unifyArgs (ctx: PassContext) (key: NodeKey) (xs: EqArray<SemType>) (ys: EqArray<SemType>) : unit =
        for i in 0 .. xs.Length - 1 do
            unify ctx key xs.[i] ys.[i]

    /// Coerce a single argument position against its expected parameter type: an
    /// `obj` parameter absorbs *any* argument (the implicit upcast / box F# inserts
    /// at the call), so it must NOT unify — pinning a typar argument (`x : 'T`) to
    /// `obj` would ground the enclosing type's parameter. Tuples walk element-wise
    /// (a tupled BCL call `Equals(obj, obj)`). The in-`unify`-group analogue of
    /// `unifyArg`'s `obj` rule, usable from the deferred-drain path below (`unifyArg`
    /// itself is defined after this group). Only `obj` is special-cased here; richer
    /// class→interface witness coercion stays in `unifyArg`/`tryCoerceUpcast` for the
    /// eager application path.
    and private unifyArgCoerce (ctx: PassContext) (key: NodeKey) (actual: SemType) (expected: SemType) : unit =
        match resolveStep actual, resolveStep expected with
        | TyTuple aa, TyTuple bb when aa.Length = bb.Length ->
            for i in 0 .. aa.Length - 1 do
                unifyArgCoerce ctx key aa.[i] bb.[i]
        | a, b ->
            // `b` is already `resolveStep`-ed by the match; `absorbsAsObj` (the one
            // obj-policy home) re-steps idempotently.
            if not (absorbsAsObj b) then
                unify ctx key a b

    /// Unify an *applied callable* shape against a resolved member signature,
    /// coercing each argument position rather than unifying it. `actual` is the
    /// call's applied shape — a curried `TyFun` chain whose domains are the argument
    /// types the application built (`comparer.GetHashCode(x)` → `TyFun('T, result)`);
    /// `expected` is the member's instantiated signature (`TyFun(obj, int)`). Each
    /// parameter position goes through `unifyArgCoerce` (so an `obj` parameter
    /// absorbs a typar / value-type argument instead of grounding it); result
    /// positions unify exactly. Used where a *whole* signature is unified against a
    /// pre-built `TyFun` (the deferred dot-access drain, the overload-commit), unlike
    /// `inferApp`'s spine walk which already coerces each argument as it applies it.
    and unifyAppliedSig (ctx: PassContext) (key: NodeKey) (actual: SemType) (expected: SemType) : unit =
        match resolveStep actual, resolveStep expected with
        | TyFun(ad, ar), TyFun(ed, er) ->
            unifyArgCoerce ctx key ad ed
            unifyAppliedSig ctx key ar er
        | a, b -> unify ctx key a b

    /// When a TyVar's Link resolves to a `TyRecord`/`TyClass`/`TyUnion`,
    /// resolve any dot-access constraints parked on it. When `T` is generic,
    /// the receiver's arg list substitutes for the type's declared typars so
    /// `(b : Box<int>).Value` resolves to `int`, not `Box`'s prototype `'a`.
    /// Fire all three on-link callbacks for a root whose `Link` just resolved
    /// to `t`: deferred dot-accesses, type-parameter constraints, and SRTP
    /// member-trait bounds.
    and private drainAll (ctx: PassContext) (key: NodeKey) (root: TypeVar) (t: SemType) : unit =
        drainPendingDotAccess ctx root t
        drainConstraints ctx key root t
        drainSrtpBounds ctx key root t

    and private drainPendingDotAccess (ctx: PassContext) (root: TypeVar) (linkTarget: SemType) : unit =
        if not (List.isEmpty root.PendingDotAccess) then
            match resolveDotSource ctx linkTarget with
            | DotSource.NotNominal -> ()
            | DotSource.UnknownType(name, kind) ->
                let pending = root.PendingDotAccess
                root.PendingDotAccess <- []

                for d in pending do
                    ctx.Error(d.UseKey, sprintf "Unknown %s type '%s'" kind name)
            | DotSource.Resolved(name, memberNoun, subst, lookup) ->
                let pending = root.PendingDotAccess
                root.PendingDotAccess <- []

                for d in pending do
                    match lookup d.MemberName with
                    | ValueSome ty -> unify ctx d.UseKey (TyVar d.ResultTv) (substituteWith subst ty)
                    | ValueNone -> ctx.Error(d.UseKey, sprintf "Type '%s' has no %s '%s'" name memberNoun d.MemberName)
            | DotSource.ClassChain(name, args) ->
                // `tryClassChainMember` already returns the type instantiated
                // against `args` (and any parent typar substitution), so no
                // further `substituteWith` is needed here.
                let pending = root.PendingDotAccess
                root.PendingDotAccess <- []

                for d in pending do
                    match tryClassChainMember ctx name args d.MemberName with
                    | ValueSome ty -> unify ctx d.UseKey (TyVar d.ResultTv) ty
                    | ValueNone ->
                        ctx.Error(d.UseKey, sprintf "Type '%s' has no instance member '%s'" name d.MemberName)
            | DotSource.ExternalClass(qualName, args) ->
                // Deferred mirror of `resolveFieldStep`'s external arm: the receiver
                // TyVar resolved to a BCL/contract class or interface (e.g. the
                // `comparer: IEqualityComparer` parameter of an `IStructuralEquatable`
                // member, pinned by the interface-conformance unify only *after* the
                // body — and its dot-accesses — were deferred). Resolve each member
                // through the provider and record it for Freeze.
                let pending = root.PendingDotAccess
                root.PendingDotAccess <- []
                let argArr = args.AsSpan().ToArray()

                for d in pending do
                    match ctx.Provider.TryLookupMember(qualName, d.MemberName) with
                    | ValueSome m when not m.IsStatic ->
                        let memberSig = normalizeObj (ExternalSymbols.openSignature m argArr)

                        ctx.Resolution.ExternalAccess.Set(
                            d.UseKey,
                            {
                                Key = m.Key
                                IsStatic = false
                                IsProperty = m.IsProperty
                                Signature = memberSig
                                OptionalDefaults = m.OptionalDefaults
                            }
                        )

                        // Coerce each argument position (`unifyAppliedSig`) rather
                        // than unify the whole signature: an `obj` parameter of a
                        // BCL member (`IEqualityComparer.GetHashCode(obj)`) must
                        // absorb a typar argument (`x : 'T`) by an implicit box,
                        // not ground the typar — the application already linked the
                        // arg into `d.ResultTv`'s domain while the receiver was
                        // still deferred, so the coercion happens here. The recorded
                        // `Signature` (the declared `obj`-bearing shape) is what
                        // Freeze reads for the box, since this unify deliberately
                        // leaves the node typed with the un-grounded arg typar.
                        unifyAppliedSig ctx d.UseKey (TyVar d.ResultTv) memberSig
                    | _ -> ctx.Error(d.UseKey, sprintf "Type '%s' has no instance member '%s'" qualName d.MemberName)

    /// `ValueSome true` = constraint holds; `ValueSome false` = violation;
    /// `ValueNone` = not in the table, fall through to structural / deferred
    /// handling.
    and private primitiveSupports (kind: SemanticConstraintKind) (name: string) : bool voption =
        let isValueType = Set.contains name primitiveValueTypes
        let isString = name = "string"

        match kind with
        | SemanticConstraintKind.Equality
        | SemanticConstraintKind.Comparison ->
            if isValueType || isString then
                ValueSome true
            else
                ValueNone
        | SemanticConstraintKind.Struct ->
            if isValueType then ValueSome true
            elif isString then ValueSome false
            else ValueNone
        | SemanticConstraintKind.ReferenceType ->
            if isString then ValueSome true
            elif isValueType then ValueSome false
            else ValueNone
        | SemanticConstraintKind.Nullness ->
            if isString then ValueSome true
            elif isValueType then ValueSome false
            else ValueNone
        | SemanticConstraintKind.NotNull ->
            if isValueType then ValueSome true
            elif isString then ValueSome false
            else ValueNone
        | SemanticConstraintKind.Coercion _ ->
            // Coercion has its own arm in `checkConstraint` (via `subsumes`) and
            // never reaches the primitive table; present only for exhaustiveness.
            ValueNone

    /// `Violated` is sticky (once any element fails, the whole compound
    /// fails); `Defer` propagates only when no element has failed but at
    /// least one is still pending.
    and private reduceOutcome (check: SemType -> ConstraintOutcome) (items: EqArray<SemType>) : ConstraintOutcome =
        let mutable result = Satisfied

        for item in items do
            match result, check item with
            | Violated, _ -> ()
            | _, Violated -> result <- Violated
            | Defer, _
            | _, Defer -> result <- Defer
            | Satisfied, Satisfied -> ()

        result

    /// Free TyVars return `Defer` so the next `Link` assignment re-fires the
    /// check via `drainConstraints`; nested compounds recurse compositionally.
    and checkConstraint (ctx: PassContext) (c: SemanticConstraint) (t: SemType) : ConstraintOutcome =
        // Shared verdict policy for the nominal data types (record / union / class):
        // the stamped equality / comparison verdict overrides the field-walk. A
        // `[<NoEquality>]` type at a `=` / `<>` use site is a diagnostic (`Violated`);
        // `Reference` / `Custom` equality is `Satisfied` (BCL `Object.Equals` resp. the
        // type's own members — field-walking a `Custom` type would be wrong, as its
        // fields may individually lack equality). Comparison is opt-in: `NoComparison`
        // ⇒ `Violated`, `Custom` ⇒ `Satisfied`, `Structural` falls through to the
        // field-walk (`fieldsOf`, computed lazily so the non-structural arms never
        // touch it).
        let verdictOutcome
            (eq: EqualityVerdict)
            (cmp: ComparisonVerdict)
            (fieldsOf: unit -> EqArray<SemType>)
            : ConstraintOutcome =
            match c.Kind, eq, cmp with
            | SemanticConstraintKind.Equality, EqualityVerdict.NoEquality, _ -> Violated
            | SemanticConstraintKind.Equality, (EqualityVerdict.Reference | EqualityVerdict.Custom), _ -> Satisfied
            | SemanticConstraintKind.Comparison, _, ComparisonVerdict.NoComparison -> Violated
            | SemanticConstraintKind.Comparison, _, ComparisonVerdict.Custom -> Satisfied
            | _ -> reduceOutcome (checkConstraint ctx c) (fieldsOf ())

        match c.Kind, resolveStep t with
        | _, TyVar _ -> Defer
        // An unresolved contract head supports no constraint, but the
        // mismatch is already reported where it unified — defer here so the
        // constraint quietly never re-fires rather than emitting a second error.
        | _, TyUnknown _ -> Defer
        // Post-freeze only; never reached during constraint solving. Defer
        // (consistent with TyUnknown) rather than crash.
        | _, TyTypar _ -> Defer
        | SemanticConstraintKind.Coercion target, _ ->
            // `'e :> exn`: now that `'e` has a nominal head, does it subsume to
            // the required supertype? `subsumes` walks user AND external (BCL)
            // `inherit` chains, and reconciles `exn`'s `TyConst` with the metadata
            // `TyClass("System.Exception", _)` via IntrinsicReprTypes — so a thrown
            // `InvalidOperationException` reaches `exn`. Read-only, so it's safe to
            // run from the drain callback (no undo trace). Past the `TyVar _` guard
            // above, `Unrelated` is a real violation, not "unknown yet".
            match subsumes ctx t target with
            | SubsumeOutcome.Equal
            | SubsumeOutcome.Subtype -> Satisfied
            | SubsumeOutcome.Unrelated -> Violated
        | k, TyConst(name, _) ->
            match primitiveSupports k name with
            | ValueSome true -> Satisfied
            | ValueSome false -> Violated
            | ValueNone -> Defer
        | (SemanticConstraintKind.Equality | SemanticConstraintKind.Comparison), TyFun _ ->
            // Function types support neither structural equality nor
            // comparison in F#.
            Violated
        | (SemanticConstraintKind.Equality | SemanticConstraintKind.Comparison), TyTuple items ->
            reduceOutcome (checkConstraint ctx c) items
        | (SemanticConstraintKind.Equality | SemanticConstraintKind.Comparison), TyRecord(recKey, args) ->
            match TypeRegistry.tryRecordByKey ctx.Types recKey with
            | ValueSome info ->
                verdictOutcome
                    info.EqualitySupport
                    info.ComparisonSupport
                    (fun () ->
                        let subst = mkNamedTypeSubst info.TypeParams args

                        info.Fields
                        |> Array.map (fun f -> substituteWith subst f.Type)
                        |> EqArray.ofArray
                    )
            | ValueNone -> Defer
        | (SemanticConstraintKind.Equality | SemanticConstraintKind.Comparison), TyUnion(unionKey, args) ->
            match TypeRegistry.tryUnionByKey ctx.Types unionKey with
            | ValueSome info ->
                verdictOutcome
                    info.EqualitySupport
                    info.ComparisonSupport
                    (fun () ->
                        let subst = mkNamedTypeSubst info.TypeParams args
                        let fields = ResizeArray<SemType>()

                        for case in info.Cases do
                            for field in case.Fields do
                                fields.Add(substituteWith subst field)

                        EqArray.ofResizeArray fields
                    )
            | ValueNone -> Defer
        | (SemanticConstraintKind.Equality | SemanticConstraintKind.Comparison), TyClass(classKey, args) ->
            // A reference class defaults to `Reference` equality (⇒ `=` Satisfied via
            // BCL `Object.Equals`) and `NoComparison`; a `[<Struct>]` value type
            // defaults to `Structural` (field-walk the instance fields);
            // `[<CustomEquality>]` / `[<CustomComparison>]` stamp `Custom`.
            match TypeRegistry.tryClassByKey ctx.Types classKey with
            | ValueSome info ->
                verdictOutcome
                    info.EqualitySupport
                    info.ComparisonSupport
                    (fun () ->
                        let subst = mkNamedTypeSubst info.TypeParams args

                        info.InstanceFields
                        |> Array.map (fun f -> substituteWith subst f.Type)
                        |> EqArray.ofArray
                    )
            | ValueNone -> Defer
        | SemanticConstraintKind.Equality, TyOr members ->
            // An anonymous union satisfies
            // EQUALITY iff EVERY member does — the all-members-or-defer reduction used
            // for tuple/record/union fields. Sound because F#'s generic equality is
            // *total* on the union's `obj`+`isinst` (CLR) / bare-value (JS) repr:
            // cross-member `=` returns `false` (different runtime types), never throws
            // — and `false` is the semantically correct answer (an int is not a
            // string). A non-equatable member (e.g. a `TyFun` arm) still fails the
            // reduction.
            reduceOutcome (checkConstraint ctx c) members.Members
        | SemanticConstraintKind.Comparison, TyOr members ->
            // COMPARISON does NOT reduce member-wise, unlike equality above. F#'s
            // generic `compare` on two `obj` of *different* runtime types THROWS
            // (`(1).CompareTo("a")` raises ArgumentException), so a genuinely
            // heterogeneous union is non-comparable even when every member is
            // individually comparable — admitting it would let `List.sort` on a
            // `(int | string) list` type-check and then throw at runtime. Any ≥2-member
            // union is heterogeneous (a singleton is collapsed away by `mkUnion`), so
            // the only comparable `TyOr` is the empty one (`never` = bottom), which
            // satisfies every constraint vacuously.
            if members.Members.IsEmpty then Satisfied else Violated
        | SemanticConstraintKind.Struct, (TyTuple _ | TyFun _ | TyRecord _ | TyUnion _ | TyClass _ | TyOr _) ->
            // v1: tuples, functions, and reference records / unions /
            // classes are all reference types. An anonymous union erases to the
            // backend's universal-supertype reference primitive (`obj`+`isinst`), so
            // it is a reference type too. `[<Struct>]`-attributed records / unions /
            // structs ship with the attribute walker.
            Violated
        | SemanticConstraintKind.ReferenceType, (TyTuple _ | TyFun _ | TyRecord _ | TyUnion _ | TyClass _ | TyOr _) ->
            Satisfied
        | SemanticConstraintKind.Nullness, _ ->
            // Nullness analysis is a separate track — defer until it
            // lands. Treating as `Defer` (not `Violated`) keeps existing
            // code that doesn't annotate nullability noise-free.
            Defer
        | SemanticConstraintKind.NotNull, _ -> Defer

    /// On-unified callback for type-parameter constraints. Satisfied
    /// constraints are dropped; deferred ones remain on the root and re-fire
    /// next time `Link` changes (which, after the first set, only happens
    /// during union-find collapse). For compound `Defer` outcomes, copy the
    /// constraint onto each still-free arg so the next Link on any of them
    /// re-evaluates the rule compositionally.
    and private drainConstraints (ctx: PassContext) (key: NodeKey) (root: TypeVar) (linkTarget: SemType) : unit =
        if List.isEmpty root.Constraints then
            ()
        else
            let cs = root.Constraints
            root.Constraints <- []
            let mutable remaining = []

            for c in cs do
                // Dependent-typar inference: a `Coercion` bound `'a :> IFace<'b>` whose
                // target carries free vars (`'b`) — once `'a` grounds to a nominal that
                // implements `IFace`, pin `'b` to the witnessed instantiation's args (the
                // same witness-unify `tryCoerceUpcast` performs at `:>` sites). This is
                // what lets a phantom typar reachable ONLY through the bound — `Seq.fold`'s
                // enumerator `'E` in `'S :> IStructSeq<'E>` — ground from the argument's
                // interface impl instead of leaking as an un-instantiated method typar at
                // the call's `MethodSpec`. Concrete / non-generic bounds (`'T :> IGetVal`,
                // `'e :> exn`) have no free-var args, so this is a no-op for them.
                match c.Kind with
                | SemanticConstraintKind.Coercion target ->
                    match subtypeNominalOf ctx (zonk target) with
                    | ValueSome(struct (tname, targs)) when
                        targs.Length > 0
                        && targs
                           |> EqArray.exists (fun a ->
                               match resolveStep a with
                               | TyVar _ -> true
                               | _ -> false
                           )
                        ->
                        match tryUpcastWitness ctx linkTarget tname with
                        | ValueSome wargs when wargs.Length = targs.Length ->
                            for i in 0 .. targs.Length - 1 do
                                unify ctx key wargs.[i] targs.[i]
                        | _ -> ()
                    | _ -> ()
                | _ -> ()

                // The INVERSE direction for the arrow↔`Fun`/`Fun2` correspondence:
                // a source lambda whose arrow has STILL-FREE
                // domains (`fun x y -> x + y` — no literal pins `x`/`y`) coerced into a
                // GROUND constrained slot (`'TF :> Fun2<int,int,int>`) must ground from
                // the slot's args, so the lambda body's SRTP operators resolve instead
                // of leaking `?ungrounded-operator`. `subsumes` itself stays read-only
                // (it only *checks* invariant-equality); this is the one place the
                // grounding `unify` lives. Arity-1 `Fun`2<a,b>` peels one arrow; flat-2
                // `Fun2`3<a,b,c>` peels two (curried codomain). Non-`Fun` coercions and
                // a non-arrow `linkTarget` are untouched.
                match c.Kind with
                | SemanticConstraintKind.Coercion target ->
                    match subtypeNominalOf ctx (zonk target), resolveStep linkTarget with
                    | ValueSome(struct (tname, targs)), TyFun(a, b) when
                        SymbolKeyOps.bareName tname = funInterfaceQualifiedName && targs.Length = 2
                        ->
                        unify ctx key a targs.[0]
                        unify ctx key b targs.[1]
                    | ValueSome(struct (tname, targs)), TyFun(a, bc) when
                        SymbolKeyOps.bareName tname = fun2InterfaceQualifiedName && targs.Length = 3
                        ->
                        match resolveStep bc with
                        | TyFun(b, c) ->
                            unify ctx key a targs.[0]
                            unify ctx key b targs.[1]
                            unify ctx key c targs.[2]
                        | _ -> ()
                    | _ -> ()
                | _ -> ()

                match checkConstraint ctx c linkTarget with
                | Satisfied -> ()
                | Violated ->
                    ctx.Error(
                        key,
                        sprintf
                            "The type '%A' does not support the '%s' constraint"
                            (zonk linkTarget)
                            (constraintKindName c.Kind)
                    )
                | Defer ->
                    remaining <- c :: remaining
                    propagateToFreeArgs ctx c linkTarget

            root.Constraints <- List.rev remaining

    /// When a compound shape is partially resolved, the parent constraint is
    /// satisfied iff every component supports it, so a still-free component
    /// carries the same constraint forward.
    and propagateToFreeArgs (ctx: PassContext) (c: SemanticConstraint) (t: SemType) : unit =
        let rec walk t =
            match resolveStep t with
            | TyVar tv ->
                let root = UnionFind.find tv

                if not (root.Constraints |> List.exists (fun e -> e.Kind = c.Kind)) then
                    root.Constraints <- c :: root.Constraints
            | TyConst(_, args) -> EqArray.iter walk args
            | TyFun(a, r) ->
                walk a
                walk r
            | TyTuple xs ->
                for x in xs do
                    walk x
            | TyRecord(_, args) ->
                for a in args do
                    walk a
            | TyUnion(_, args) ->
                for a in args do
                    walk a
            | TyClass(_, args) ->
                for a in args do
                    walk a
            | TyOr members ->
                // An anonymous union supports a structural constraint iff every
                // member does (`checkConstraint`'s all-members rule), so a still-free
                // member carries the constraint forward.
                for m in members.Members do
                    walk m
            | TyUnknown _ -> ()
            // A post-freeze typar leaf carries no free args.
            | TyTypar _ -> ()

        walk t

    /// SRTP arithmetic dispatch on numeric primitives. For `op_Addition`
    /// etc. on `int` the candidate "static member" type is `int * int ->
    /// int`; we synthesise it here so the unifier doesn't need to know
    /// which provider declared the primitive. The numeric name set is the
    /// shared `RuntimeNames.numericTypeNames` (one place to grow).
    and private numericPrimitives = RuntimeNames.numericTypeNames

    and private arithmeticBinaryOps =
        Set.ofList [ "op_Addition"; "op_Subtraction"; "op_Multiply"; "op_Division"; "op_Modulus" ]

    // Bitwise AND/OR/XOR have the same `^T * ^T -> ^T` primitive shape as
    // arithmetic; the shift operators differ — their second operand is `int32`,
    // not `^T` (`op_LeftShift`/`op_RightShift`: `^T * int32 -> ^T`).
    and private bitwiseBinaryOps =
        Set.ofList [ "op_BitwiseAnd"; "op_BitwiseOr"; "op_ExclusiveOr" ]

    and private shiftOps = Set.ofList [ "op_LeftShift"; "op_RightShift" ]

    // Unary `~-` / `~+` / `~~~` — one primitive operand, `^T -> ^T`.
    and private unaryPrimitiveOps =
        Set.ofList [ "op_UnaryNegation"; "op_UnaryPlus"; "op_LogicalNot" ]

    and private equalityBinaryOps = Set.ofList [ "op_Equality"; "op_Inequality" ]

    and private orderingBinaryOps =
        Set.ofList
            [
                "op_LessThan"
                "op_GreaterThan"
                "op_LessThanOrEqual"
                "op_GreaterThanOrEqual"
            ]

    // Equality stays in Vesper.Core, ordering in Vesper.Comparison. Both families
    // synthesise the same primitive trait shape (`prim*prim → bool`), so
    // `tryPrimitiveTraitCandidate` checks the union; the split is what lets the
    // decline-fallthrough diverge by family once the .fsi contracts become the live
    // provider (today they resolve identically).
    and private comparisonBinaryOps = Set.union equalityBinaryOps orderingBinaryOps

    and private tryPrimitiveTraitCandidate (memberName: string) (primName: string) (argCount: int) : SemType voption =
        if primName = "string" && memberName = "op_Addition" && argCount = 2 then
            // String concatenation: `string * string -> string`. `string` is not a
            // numeric primitive, but the `(+)` inline's `when ^T : string` clause
            // makes `string + string` valid (codegen lowers it to
            // `System.String.Concat`). Resolve the
            // SRTP trait here so a `(+)`-on-string bound drains cleanly instead of
            // erroring "string has no op_Addition" — the spurious diagnostic that
            // surfaced compiling `structural-printer.fs` (the first library to use
            // string `+`; bare programs emit it too but never gate on diagnostics).
            let t = TyConst("string", EqArray.empty)
            ValueSome(TyFun(TyTuple(EqArray.ofList [ t; t ]), t))
        elif not (Set.contains primName numericPrimitives) then
            ValueNone
        elif
            argCount = 2
            && (Set.contains memberName arithmeticBinaryOps
                || Set.contains memberName bitwiseBinaryOps)
        then
            let t = TyConst(primName, EqArray.empty)
            ValueSome(TyFun(TyTuple(EqArray.ofList [ t; t ]), t))
        elif argCount = 2 && Set.contains memberName shiftOps then
            // `value: ^T -> shift: int32 -> ^T` — the shift amount is always int32.
            let t = TyConst(primName, EqArray.empty)
            ValueSome(TyFun(TyTuple(EqArray.ofList [ t; TyConst("int", EqArray.empty) ]), t))
        elif argCount = 1 && Set.contains memberName unaryPrimitiveOps then
            let t = TyConst(primName, EqArray.empty)
            ValueSome(TyFun(t, t))
        elif argCount = 2 && Set.contains memberName comparisonBinaryOps then
            let t = TyConst(primName, EqArray.empty)
            ValueSome(TyFun(TyTuple(EqArray.ofList [ t; t ]), TyConst("bool", EqArray.empty)))
        else
            ValueNone

    /// Build the expected trait signature in tupled or curried form,
    /// picking whichever matches the candidate's shape. F# accepts both
    /// `static member (+)(a, b)` (tupled) and `static member (+) a b`
    /// (curried) as satisfying a trait declared `^T * ^T -> ^T`.
    and private unifySrtpAgainst
        (ctx: PassContext)
        (key: NodeKey)
        (candidate: SemType)
        (bound: MemberSignature)
        : unit =
        let argTys = bound.ArgTypes

        let tupled =
            match argTys.Length with
            | 0 -> bound.ReturnType
            | 1 -> TyFun(argTys.[0], bound.ReturnType)
            | _ -> TyFun(TyTuple argTys, bound.ReturnType)

        match resolveStep candidate with
        | TyFun(TyTuple _, _) -> unify ctx key candidate tupled
        | _ when argTys.Length >= 2 ->
            let curried = EqArray.foldBack (fun a r -> TyFun(a, r)) argTys bound.ReturnType

            unify ctx key candidate curried
        | _ -> unify ctx key candidate tupled

    /// On-unified callback for SRTP member-trait bounds. The `Resolved` flag
    /// on the shared `MemberSignature` instance (all participating typars
    /// hold the same record by reference) dedupes dispatch when multiple
    /// participating typars resolve in sequence — whichever links first runs
    /// the drain; the others see the flag set and skip. Bounds that can't
    /// dispatch yet (target is still a free TyVar) remain on the root.
    ///
    /// Diagnostics use `key` — the user's call site, threaded through from
    /// the caller — so "Type X has no static member Y" points there rather
    /// than at the prelude's `(+)` declaration.
    and private drainSrtpBounds (ctx: PassContext) (key: NodeKey) (root: TypeVar) (linkTarget: SemType) : unit =
        if List.isEmpty root.SrtpBounds then
            ()
        else
            let bounds = root.SrtpBounds
            root.SrtpBounds <- []
            let mutable remaining = []

            for b in bounds do
                if b.Resolved then
                    ()
                else
                    match resolveStep linkTarget with
                    | TyConst(primName, _) ->
                        match tryPrimitiveTraitCandidate b.MemberName primName b.ArgTypes.Length with
                        | ValueSome candTy ->
                            b.Resolved <- true
                            unifySrtpAgainst ctx key candTy b
                        | ValueNone ->
                            ctx.Error(key, sprintf "Type '%s' has no built-in static member '%s'" primName b.MemberName)
                            b.Resolved <- true
                    | TyClass(classKey, classArgs) ->
                        match TypeRegistry.tryClassByKey ctx.Types classKey with
                        | ValueSome info ->
                            match info.Members |> Array.tryFind (fun m -> m.IsStatic && m.Name = b.MemberName) with
                            | Some m ->
                                let candTy = instantiateMember (info.TypeParams, classArgs) m.Type
                                b.Resolved <- true
                                unifySrtpAgainst ctx key candTy b
                            | None ->
                                ctx.Error(
                                    key,
                                    sprintf
                                        "Type '%s' has no static member '%s'"
                                        (SymbolKeyOps.simpleName classKey)
                                        b.MemberName
                                )

                                b.Resolved <- true
                        | ValueNone ->
                            // Not a project-local class — try the external contract
                            // provider. A *consumer* dispatching `+` / `-` to an
                            // external type's static operator (a driver's `s + t` on
                            // an `.fsi`-imported `Vesper.Set`) reaches here: the
                            // `ValueSome` arm above only covers the producer side
                            // (same-assembly `Set`, in `ctx.Types`). Without this the
                            // bound parks forever and the operator result's element
                            // typar (`Set<?>`) never grounds — surfacing as a stray
                            // unresolved TyVar. `openSignature` substitutes the static
                            // member's declaring typars from `classArgs`, yielding the
                            // same `^T * ^T -> ^T` candidate shape the local arm builds.
                            let metaName = SymbolKeyOps.qualifiedName classKey

                            match ctx.Provider.TryLookupMember(metaName, b.MemberName) with
                            | ValueSome m when m.IsStatic ->
                                let candTy =
                                    ExternalSymbols.openSignature m (EqArray.toList classArgs |> List.toArray)

                                b.Resolved <- true
                                unifySrtpAgainst ctx key candTy b
                            | _ ->
                                // Unknown class — keep the bound so a later
                                // pass might still be able to dispatch.
                                remaining <- b :: remaining
                    | _ ->
                        // Target not yet a concrete type-bearing shape — defer.
                        remaining <- b :: remaining

            root.SrtpBounds <- List.rev remaining

    /// Coerce `src` to the nominal target `tgt` as an implicit/`:>` upcast: when
    /// `src` (or a base / interface) instantiates `tgt`'s nominal, `unify` the
    /// witness's type args against `tgt`'s — pinning any inference var in `tgt`
    /// (the `_` in `this :> seq<_>`) and any unresolved arg of `src`
    /// (`Comparer<'T> ⊳ IComparer<'T>` links the two `'T`s) — then return `true`.
    /// Returns `false` when `src` is not a subtype of `tgt`, leaving the caller
    /// to fall back to a plain `unify` (and its mismatch diagnostic). Unlike
    /// `subsumes` this *mutates* (it links type args), so it belongs only at the
    /// coercion sites — argument / ctor unification and `:>` — never the
    /// read-only constraint checker.
    let tryCoerceUpcast (ctx: PassContext) (key: NodeKey) (src: SemType) (tgt: SemType) : bool =
        // `obj` is the universal supertype: F# implicitly upcasts (boxing a value
        // type / a generic typar) any value into an `obj` slot, so accept *any*
        // `src` without unifying. Crucially this must NOT pin `src` — a generic
        // typar argument (`comparer.GetHashCode(x)` with `x : 'T`) flowing into an
        // `obj` parameter would otherwise unify `'T := obj`, grounding the
        // enclosing type's parameter (the Vesper.Set `Set<'T>` whole-class-typar
        // grounding). The box is inserted at codegen (the call site sees the param
        // is `obj` and the arg's static type is a typar / value type).
        if absorbsAsObj tgt then
            true
        else

            // A union-typed slot is `obj` restricted to an enumerated member set: it
            // accepts any value that subsumes into one of its members, with the *same*
            // no-pin discipline as `obj` above. `subsumes` is a pure read (no `Link`),
            // so a generic value threaded through a union-typed parameter is not wrongly
            // grounded — the `acceptsByAssignability` generalisation of `absorbsAsObj`.
            match resolveStep tgt with
            | TyOr _ -> subsumes ctx src tgt <> SubsumeOutcome.Unrelated
            | _ ->

                match subtypeNominalOf ctx tgt with
                | ValueNone -> false
                | ValueSome(struct (tname, targs)) ->
                    match tryUpcastWitness ctx src tname with
                    | ValueSome sargs when sargs.Length = targs.Length ->
                        for i in 0 .. targs.Length - 1 do
                            unify ctx key sargs.[i] targs.[i]

                        true
                    | _ -> false

    /// Unify an *argument* against its expected parameter type, admitting the
    /// implicit class→interface / class→base upcast F# inserts at a coercion
    /// point: a `Comparer<'T>` value flows into an `IComparer<'T>` slot, a
    /// derived class into a base-typed slot. `tryCoerceUpcast` both accepts the
    /// subtype and unifies its type args; anything that isn't a subtype defers to
    /// plain `unify`, which links type variables and reports a genuine mismatch.
    /// Tuples are walked element-wise so a tupled ctor argument
    /// (`Set(comparer, tree)`) coerces each component independently. Used at every
    /// argument / chain-call coercion site (application, primary/secondary ctors).
    let rec unifyArg (ctx: PassContext) (key: NodeKey) (actual: SemType) (expected: SemType) : unit =
        match resolveStep actual, resolveStep expected with
        | TyTuple aa, TyTuple bb when aa.Length = bb.Length ->
            for i in 0 .. aa.Length - 1 do
                unifyArg ctx key aa.[i] bb.[i]
        | a, b ->
            if not (tryCoerceUpcast ctx key a b) then
                unify ctx key a b

    /// Reconcile an inferred type against a *written annotation* (a `let` return
    /// type, a parameter `Pat.Typed`). Checking-mode, but narrower than `unifyArg`:
    /// the ONLY assignability admitted is value→union — when the annotation is a
    /// `TyOr` the actual subsumes into, accept *without* unifying, so `let x: int |
    /// string = 1` checks and the actual's typar stays free (the no-pin discipline).
    /// Every other annotation — `obj`, a base class, a
    /// plain nominal — falls through to symmetric `unify`, so a binder annotated
    /// `obj` still GROUNDS to `obj` (unlike `unifyArg`, whose `absorbsAsObj` accept
    /// would leave the binder's var unresolved and break signature encoding). The
    /// no-unify branch is reached only when `subsumes` already succeeds, i.e. the
    /// actual is concrete enough to subsume — so nothing is left ungrounded. The
    /// principality rule holds: a union enters only by an annotation, and `unify`
    /// never synthesises one.
    let unifyAnnotation (ctx: PassContext) (key: NodeKey) (actual: SemType) (expected: SemType) : unit =
        match resolveStep expected with
        | TyOr _ when subsumes ctx actual expected <> SubsumeOutcome.Unrelated -> ()
        | _ -> unify ctx key actual expected
