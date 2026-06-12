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

        let rec walk (clsName: string) (args: EqArray<SemType>) : ChainMember voption =
            if not (seen.Add clsName) then
                ValueNone
            else
                match ctx.Types.Class.TryGetValue clsName with
                | true, info ->
                    match info.Members |> Array.tryFind (fun m -> m.Name = memberName && not m.IsStatic) with
                    | Some m ->
                        ValueSome
                            {
                                DeclaringTy = TyClass(info.Key, args)
                                MemberTy = instantiateMember (info.TypeParams, args) m.Type
                            }
                    | None ->
                        match info.BaseType with
                        | ValueSome parentTy ->
                            match resolveStep (instantiateMember (info.TypeParams, args) parentTy) with
                            | TyClass(parentKey, parentArgs) -> walk (SymbolKeyOps.simpleName parentKey) parentArgs
                            | _ -> ValueNone
                        | ValueNone -> ValueNone
                | false, _ -> ValueNone

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

    // Canonical nominal name for subtype comparison. A primitive intrinsic
    // binding (`type exn = (# "System.Exception" #)`, prim-types-exn.fs) stays
    // a *non-transparent* `TyConst "exn"` (Translate.fs) — it never expands to
    // its RHS the way a plain abbreviation does. Its CLI representation is the
    // BCL type name the external `inherit`-chain walk surfaces. Mapping through
    // it makes a user-facing `exn` and a metadata-surfaced
    // `TyClass("System.Exception", _)` the *same* nominal. The identity
    // `exn === System.Exception` therefore originates from prim-types-exn.fs,
    // not a literal baked into the unifier — retarget the core lib and this follows.
    //
    // Resolution order (local-first / provider-fallback):
    //   1. the compiled unit's OWN intrinsics (`ctx.Types.IntrinsicReprTypes`,
    //      keyed by the unqualified name it declared);
    //   2. a *referenced* package's intrinsics, riding the provider as
    //      `ExternalTypeShape.Intrinsic repr` (the self-compiled `exn` is local;
    //      a consumer's `exn` comes from Vesper.Core through the provider).
    // `n` here is the unqualified nominal (`translateType` strips an external
    // intrinsic to its short name so `exn` unifies with literals), so the
    // provider tier resolves it through the *same* ambient open scope
    // `translateType` used — `exn` ⇒ `Vesper.exn` ⇒ `Intrinsic "System.Exception"`.
    // Memoized per `PassContext`: `canonName` runs inside the subtype recursive
    // walk, so without the cache every node would round-trip the composite
    // provider / MetadataLoadContext through that ambient candidate list. A name
    // that is neither a local nor a provider intrinsic caches its own identity.
    let private canonName (ctx: PassContext) (n: string) : string =
        match ctx.IntrinsicCanonCache.TryGetValue n with
        | true, repr -> repr
        | _ ->
            let providerIntrinsicRepr (c: string) : string voption =
                match ctx.Provider.TryLookupType c with
                | ValueSome(ExternalTypeShape.Intrinsic repr) -> ValueSome repr
                | _ -> ValueNone

            let repr =
                match ctx.Types.IntrinsicReprTypes.TryGetValue n with
                | true, repr -> repr
                | _ ->
                    match OpenScope.tryResolve ctx.Resolution.OpenScope providerIntrinsicRepr n with
                    | ValueSome repr -> repr
                    | ValueNone -> n

            ctx.IntrinsicCanonCache.[n] <- repr
            repr

    // Surface a nominal `(name, args)` for the comparison. Covers `TyConst`
    // (so the `exn` bound participates), not just `TyClass`.
    let private subtypeNominalOf (ctx: PassContext) (ty: SemType) : struct (string * EqArray<SemType>) voption =
        match resolveStep ty with
        // Surface the *qualified* canonical name so an external `TyClass`
        // (`System.Exception`) reconciles with the `exn` `TyConst` through
        // `canonName`'s repr map. `parentOf` splits the simple segment back off
        // for the project-local class lookup.
        | TyClass(n, args) -> ValueSome(struct (canonName ctx (SymbolKeyOps.qualifiedName n), args))
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

        match ctx.Types.Class.TryGetValue simple with
        | true, info ->
            [
                for impl in info.InterfaceImpls do
                    match impl.Resolved with
                    | ValueSome ifaceTy ->
                        match subtypeNominalOf ctx (instantiateMember (info.TypeParams, args) ifaceTy) with
                        | ValueSome p -> yield p
                        | ValueNone -> ()
                    | ValueNone -> ()
            ]
        | false, _ ->
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
    /// authoritative subtype walk: direct interfaces at each level (class→interface,
    /// G19/G20), then up the `inherit` chain (class→base, user + BCL); `subsumes`
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
    /// (inheritance-plan §"Why subsumes being read-only is load-bearing").
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
    let rec subsumes (ctx: PassContext) (src: SemType) (tgt: SemType) : SubsumeOutcome =
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
        // class's provider lookup (G22).
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
        /// (`System.Collections.IEqualityComparer`, G22). The drain resolves the
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
                // TyVar resolved to a BCL/contract class or interface (G22, e.g. the
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
                        ctx.Resolution.ExternalAccess.Set(
                            d.UseKey,
                            {
                                Key = m.Key
                                IsStatic = false
                                IsProperty = m.IsProperty
                            }
                        )

                        // Coerce each argument position (`unifyAppliedSig`) rather
                        // than unify the whole signature: an `obj` parameter of a
                        // BCL member (`IEqualityComparer.GetHashCode(obj)`) must
                        // absorb a typar argument (`x : 'T`) by an implicit box,
                        // not ground the typar — the application already linked the
                        // arg into `d.ResultTv`'s domain while the receiver was
                        // still deferred, so the coercion happens here.
                        unifyAppliedSig
                            ctx
                            d.UseKey
                            (TyVar d.ResultTv)
                            (normalizeObj (ExternalSymbols.openSignature m argArr))
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
                // C-Attr verdict overrides the field-walk. Equality: a
                // `[<NoEquality>]` record at a `=` / `<>` use site is a
                // diagnostic; a `[<ReferenceEquality>]` record satisfies the
                // equality predicate via BCL `Object.Equals`. Comparison
                // (brainstorm-comparison §9) is opt-in, so an unannotated
                // record is `NoComparison` ⇒ ordering use site rejected;
                // `[<StructuralComparison>]` falls through to the field-walk.
                match c.Kind, info.EqualitySupport, info.ComparisonSupport with
                | SemanticConstraintKind.Equality, EqualityVerdict.NoEquality, _ -> Violated
                | SemanticConstraintKind.Equality, EqualityVerdict.Reference, _ -> Satisfied
                | SemanticConstraintKind.Comparison, _, ComparisonVerdict.NoComparison -> Violated
                | _ ->
                    let subst = mkNamedTypeSubst info.TypeParams args

                    info.Fields
                    |> Array.map (fun f -> substituteWith subst f.Type)
                    |> EqArray.ofArray
                    |> reduceOutcome (checkConstraint ctx c)
            | ValueNone -> Defer
        | (SemanticConstraintKind.Equality | SemanticConstraintKind.Comparison), TyUnion(unionKey, args) ->
            match TypeRegistry.tryUnionByKey ctx.Types unionKey with
            | ValueSome info ->
                match c.Kind, info.EqualitySupport, info.ComparisonSupport with
                | SemanticConstraintKind.Equality, EqualityVerdict.NoEquality, _ -> Violated
                | SemanticConstraintKind.Equality, EqualityVerdict.Reference, _ -> Satisfied
                | SemanticConstraintKind.Comparison, _, ComparisonVerdict.NoComparison -> Violated
                | _ ->
                    let subst = mkNamedTypeSubst info.TypeParams args

                    let fields = ResizeArray<SemType>()

                    for case in info.Cases do
                        for field in case.Fields do
                            fields.Add(substituteWith subst field)

                    fields |> EqArray.ofResizeArray |> reduceOutcome (checkConstraint ctx c)
            | ValueNone -> Defer
        | (SemanticConstraintKind.Equality | SemanticConstraintKind.Comparison), TyClass _ ->
            // Per docs/classes-plan.md §Open questions: F# classes are
            // reference-equal by default; structural equality / comparison
            // for classes requires the attribute walker. Defer in v1.
            Defer
        | SemanticConstraintKind.Struct, (TyTuple _ | TyFun _ | TyRecord _ | TyUnion _ | TyClass _) ->
            // v1: tuples, functions, and reference records / unions /
            // classes are all reference types. `[<Struct>]`-attributed
            // records / unions / structs ship with the attribute walker.
            Violated
        | SemanticConstraintKind.ReferenceType, (TyTuple _ | TyFun _ | TyRecord _ | TyUnion _ | TyClass _) -> Satisfied
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
            | TyUnknown _ -> ()
            // A post-freeze typar leaf carries no free args.
            | TyTypar _ -> ()

        walk t

    /// SRTP arithmetic dispatch on numeric primitives. For `op_Addition`
    /// etc. on `int` the candidate "static member" type is `int * int ->
    /// int`; we synthesise it here so the unifier doesn't need to know
    /// which provider declared the primitive.
    and private numericPrimitives =
        Set.ofList
            [
                "int"
                "int8"
                "int16"
                "int32"
                "int64"
                "uint"
                "uint8"
                "uint16"
                "uint32"
                "uint64"
                "byte"
                "sbyte"
                "nativeint"
                "unativeint"
                "float"
                "float32"
                "double"
                "single"
                "decimal"
            ]

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

    // Split per operators-plan.md O4: equality stays in Vesper.Core, ordering in
    // Vesper.Comparison. Both families synthesise the same primitive trait shape
    // (`prim*prim → bool`), so `tryPrimitiveTraitCandidate` checks the union; the
    // split is what lets the decline-fallthrough diverge by family once the .fsi
    // contracts become the live provider (today they resolve identically).
    and private comparisonBinaryOps = Set.union equalityBinaryOps orderingBinaryOps

    and private tryPrimitiveTraitCandidate (memberName: string) (primName: string) (argCount: int) : SemType voption =
        if not (Set.contains primName numericPrimitives) then
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
    /// point (G19): a `Comparer<'T>` value flows into an `IComparer<'T>` slot, a
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
