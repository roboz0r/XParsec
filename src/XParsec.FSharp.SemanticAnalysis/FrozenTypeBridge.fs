namespace XParsec.FSharp.SemanticAnalysis

/// The `SemType` ↔ `FrozenType` bridge. `toFrozen` is
/// the Edge-A sink-side conversion; `ofFrozen` its inverse. On the *post-freeze*
/// `SemType` subset (`{TyConst, TyFun, TyTuple, TyRecord, TyUnion, TyClass,
/// TyTypar, TyUnknown}`) the two are mutual inverses — the cases are 1:1 with
/// `{FTConst, FTFun, FTTuple, FTRecord, FTUnion, FTClass, FTTypar, FTUnknown}`.
/// `TyVar` is the sole case with no `FrozenType` counterpart (the point of the
/// split): `toFrozen` rejects it with a hard error mirroring `ClrEncoder`'s
/// existing `cannot encode SemType: TyVar` crash, so a stray metavar fails here
/// — one hop out from where the catch-all failed before. AutoOpen so the
/// boundary callers can wrap a `.ty` in `toFrozen` unqualified.
[<AutoOpen>]
module FrozenTypeBridge =
    /// `toFrozen` with the `TyVar` leaf as a POLICY parameter — the single
    /// `SemType -> FrozenType` structural fold; `toFrozen` (hard error) and
    /// `Elaborate.freezeTy`'s documented-temporary lenient placeholder are its two
    /// instantiations, so the fold body cannot drift between them.
    let rec toFrozenWith (onVar: SemType -> FrozenType) (ty: SemType) : FrozenType =
        let go = toFrozenWith onVar

        match ty with
        | TyConst(key, args) -> FTConst(key, EqArray.map go args)
        | TyFun(arg, result) -> FTFun(go arg, go result)
        | TyTuple items -> FTTuple(EqArray.map go items)
        | TyRecord(key, args) -> FTRecord(key, EqArray.map go args)
        | TyUnion(key, args) -> FTUnion(key, EqArray.map go args)
        | TyClass(key, args) -> FTClass(key, EqArray.map go args)
        // Enums are niladic nominals (no args, no typars) — a pure key carry-over.
        | TyEnum key -> FTEnum key
        // Rebuild through the smart constructor — freezing members can collapse the
        // set (two distinct `SemType` members freezing equal), so never a raw map.
        | TyOr members -> FrozenType.MkUnion(seq { for m in members.Members -> go m })
        | TyLiteral v -> FTLiteral v
        // The type-level computations carry across as inert nodes; their children
        // freeze structurally (a still-open method var lands on the `onVar` policy).
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
        | TyUnknown name -> FTUnknown name
        | TyVar _ -> onVar ty

    /// `SemType -> FrozenType`. Total on the post-freeze subset; a hard error on
    /// `TyVar` (an inference metavar must never reach the frozen boundary).
    let toFrozen (ty: SemType) : FrozenType =
        toFrozenWith (fun v -> failwithf "FrozenType.toFrozen: cannot freeze SemType: %A" v) ty

    /// Realise a `FrozenType` template, resolving its open typars via the three
    /// supplied callbacks: `declaring i` yields the declaring type's i-th arg;
    /// `methodVar j` yields the method axis's j-th instantiation; `localTypar binder
    /// k` yields the realisation of typar #`k` of the local scheme bound at `binder`
    /// (`FTLocalTypar`, which — unlike the two declared axes — is NOT a position in
    /// any argument vector, so its policy can only MINT, never index; and which must
    /// be keyed on the `(binder, index)` PAIR, never the index alone). Every other
    /// case maps structurally. Callers that span more than one template of the *same*
    /// signature (a split parameter/return `ExternalSignature`) must share one
    /// `methodVar` memo so a repeated method index resolves to the same var across
    /// the whole signature; the same holds for `localTypar` across a thawed decl.
    /// `ofFrozen` is the identity case (both declared placeholders map straight
    /// back to their `TyTypar` markers).
    let rec instantiateWith
        (declaring: int -> SemType)
        (methodVar: int -> SemType)
        (localTypar: NodeKey -> int -> SemType)
        (template: FrozenType)
        : SemType =
        let go = instantiateWith declaring methodVar localTypar

        match template with
        | FTConst(key, args) -> TyConst(key, EqArray.map go args)
        | FTFun(arg, result) -> TyFun(go arg, go result)
        | FTTuple items -> TyTuple(EqArray.map go items)
        | FTRecord(key, args) -> TyRecord(key, EqArray.map go args)
        | FTUnion(key, args) -> TyUnion(key, EqArray.map go args)
        | FTClass(key, args) -> TyClass(key, EqArray.map go args)
        // Enums carry no args/typars — the key passes straight through both ways.
        | FTEnum key -> TyEnum key
        // Build through `MkUnion`, not a raw `TyOr`: realising members can collapse
        // the set (a typar member instantiating to another member), and `MkUnion` is
        // the sole producer.
        | FTOr members -> SemType.MkUnion(seq { for m in members -> go m })
        // A literal is a ground leaf — no typars to resolve, maps straight across.
        | FTLiteral v -> TyLiteral v
        // The type-level computations realise their children (which may carry the
        // declaring/method placeholders) but are NOT evaluated here — carried inert.
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
        | FTTypar(TyparAxis.Declaring, i) -> declaring i
        | FTTypar(TyparAxis.Method, j) -> methodVar j
        | FTLocalTypar(binder, k) -> localTypar binder k
        | FTUnknown name -> TyUnknown name

    /// `FrozenType -> SemType`. Total — every `FrozenType` case has a `SemType`
    /// counterpart (`FTTypar` lands on the post-freeze-only `TyTypar`). The
    /// identity realisation of `instantiateWith`: each DECLARED placeholder maps
    /// straight back to its self-describing `TyTypar` marker.
    ///
    /// `FTLocalTypar` is the one arm with no marker to map to — `SemType` has no
    /// local-typar case — so it MINTS a fresh unlinked `TyVar`, memoised per
    /// `(binder, index)` PAIR so repeated occurrences of one local typar share a
    /// cell across the realised template. So `ofFrozen` is not cell-free in that
    /// arm; the contract it actually owes is intact, because the cells it mints are
    /// the CALLER's, never a producer's (nothing on the other side of a frozen
    /// boundary can hold a reference to one).
    let ofFrozen (store: TypeStore) (ft: FrozenType) : SemType =
        let localCache =
            System.Collections.Generic.Dictionary<struct (NodeKey * int), SemType>()

        instantiateWith
            (fun i -> TyTypar(TyparAxis.Declaring, i))
            (fun j -> TyTypar(TyparAxis.Method, j))
            (fun binder k ->
                let key = struct (binder, k)

                match localCache.TryGetValue key with
                | true, v -> v
                | _ ->
                    let v = TyVar(store.NewTypeVar())
                    localCache.[key] <- v
                    v
            )
            ft

    // A `FrozenType` template is an external descriptor's body with its open
    // typars baked as `FTTypar(Declaring,i)` / `FTTypar(Method,j)` placeholders.
    // The realiser family below resolves declaring placeholders to the caller's
    // fresh declaring args and method placeholders to fresh metavars, all over
    // the shared `instantiateWith` walk. It is the data form of the legacy
    // `SemType[] -> SemType` closures (`BuildSignature` / `BuildType` / …):
    // inference reads templates here, codegen reads them directly. Constraint
    // stamping is NOT part of this — it stays in `ExternalSymbols.instantiateSymbol`,
    // applied *after* freshening (the type-shape half carries no constraints).

    /// The placeholder a contract-layer descriptor carries between extraction and
    /// the `ExtractCtx.toProvider` finalize pass.
    /// A body's `FrozenType` can't be built at extraction time — it may forward-
    /// reference a type registered later in the same package — so the shape holds
    /// this until `VesperLib.finalizeDeferred` translates the stashed CST and
    /// overwrites it. Never observed by a consumer.
    let deferredTemplate: FrozenType = FTUnknown "<deferred>"

    /// The standard method-typar freshener: a fresh `TyVar` at `level` per
    /// distinct index, memoised in `cache` so repeated occurrences of the same
    /// method index share one var. Mirrors `Infer.instantiateMethodTypars`.
    let methodFreshener
        (store: TypeStore)
        (cache: System.Collections.Generic.Dictionary<int, SemType>)
        (level: int)
        (j: int)
        : SemType =
        match cache.TryGetValue j with
        | true, v -> v
        | _ ->
            let tv = store.NewTypeVar()
            store.SetLevel(UnionFind.find store tv, level)
            let v = TyVar tv
            cache.[j] <- v
            v

    /// The `localTypar` policy for a SIGNATURE / type-shape template. Such a template
    /// describes a DECLARED type, and an `FTLocalTypar` only ever arises inside a
    /// decl's BODY (a body-local `let`'s own generalized scheme) — never in the decl's
    /// own type, which is exactly why `mkMethodQuantEnv` cannot map it to a declared
    /// axis. So one reaching a template realiser is a producer bug: fail loud rather
    /// than fabricate a var, mirroring the method-axis arm of `instantiateDeclaring`.
    /// Only a realiser of a whole frozen BODY (the inline-splice thaw) supplies a
    /// minting policy.
    let localTyparInTemplate (site: string) (binder: NodeKey) (k: int) : SemType =
        failwithf "%s: unexpected body-local typar %d of scheme %O in a signature template" site k binder

    /// Realise a *declaring-only* template (a type-shape descriptor — a record
    /// field, union-case field, interface arg, base type, or abbreviation body):
    /// `FTTypar(Declaring,i) → declaringArgs.[i]`. These descriptors carry no
    /// method axis (only members do), so a `FTTypar(Method,_)` here is a producer
    /// bug — it fails loud rather than fabricating a var. An out-of-range declaring
    /// index degrades to `TyUnknown` rather than crashing — the `SemType`
    /// counterpart of `substituteDeclaring`'s arity-mismatch arm — so a template
    /// that names more typars than the use site supplies (an under-applied generic
    /// abbrev, a body referencing an undeclared typar) surfaces as a use-site
    /// diagnostic instead of an `IndexOutOfRange`. Needs no `level`.
    let instantiateDeclaring (template: FrozenType) (declaringArgs: SemType[]) : SemType =
        instantiateWith
            (fun i ->
                if i < declaringArgs.Length then
                    declaringArgs.[i]
                else
                    TyUnknown "<arity-mismatch>"
            )
            (fun j ->
                failwithf
                    "FrozenTypeBridge.instantiateDeclaring: unexpected method typar %d in a type-shape template"
                    j
            )
            (localTyparInTemplate "FrozenTypeBridge.instantiateDeclaring")
            template

    /// The largest declaring-typar index a template references, or `-1` if it
    /// references none. `freezeMemberSig` uses this to DROP a member whose
    /// signature names a typar beyond the declaring type's arity
    /// (`maxDeclaringIndex >= declaringTyparArity`): such a member can't be instantiated
    /// from the receiver's declaring args alone, so it's removed rather than
    /// surfaced with an unrealisable slot. This is a policy choice — drop vs.
    /// degrade — not crash-avoidance: both realisers (`instantiateDeclaring`,
    /// `substituteDeclaring`) degrade an out-of-range declaring index to `Unknown`
    /// on their own. A method typar is a producer bug here (type-shape / contract
    /// templates carry no method axis).
    let rec maxDeclaringIndex (template: FrozenType) : int =
        match template with
        | FTTypar(TyparAxis.Declaring, i) -> i
        | FTTypar(TyparAxis.Method, j) ->
            failwithf "FrozenTypeBridge.maxDeclaringIndex: unexpected method typar %d in a type-shape template" j
        | t ->
            let mutable m = -1
            FrozenType.iterChildren (fun c -> m <- max m (maxDeclaringIndex c)) t
            m

    /// Split a freshly-translated member signature's single typar axis into the
    /// declaring + method axes. The contract-extraction translate (`translateType`)
    /// bakes EVERY typar on the `Declaring` axis — it threads one `TyparCollector`
    /// with no axis notion. A member's collector is seeded with the declaring type's
    /// own typars (indices `0 .. declaringTyparArity-1`) before its signature is walked,
    /// so any typar the member INTRODUCES — explicit `<'a>` or an implicit `'T`
    /// (`Formatter.AppendFormatted: 'T -> unit`) — lands at index `>= declaringTyparArity`.
    /// Those are the member's OWN generic parameters: rewrite each to
    /// `FTTypar(Method, i - declaringTyparArity)`, leaving the genuine declaring typars
    /// untouched. The `.fsi` analogue of `Elaborate.freezeTypars`' `methodEnv` flip;
    /// the single point that gives an extracted member its method axis (so codegen
    /// reads a real `MethodTyparArity` and mints the `MethodSpec`'s generic params).
    let rec reaxisMethodTypars (declaringTyparArity: int) (template: FrozenType) : FrozenType =
        match template with
        | FTTypar(TyparAxis.Declaring, i) when i >= declaringTyparArity ->
            FTTypar(TyparAxis.Method, i - declaringTyparArity)
        // Child recursion reaches a member-introduced typar buried in ANY child —
        // `keyof`/indexed/conditional included; `mapChildren` routes `FTOr` through
        // `MkUnion`, keeping the every-rebuild-canonicalises invariant.
        | t -> FrozenType.mapChildren (reaxisMethodTypars declaringTyparArity) t

    /// `true` when the type is fully ground: no open typar on either axis, no
    /// body-local free typar, and no `FTUnknown` (a leaked inference metavar the
    /// front end never resolved). The `FrozenType` sibling of
    /// `Passes.InlineExpansion`'s `SemType` `isGroundType`.
    let rec ftIsGround (t: FrozenType) : bool =
        match t with
        | FTTypar _
        // A body-local scheme's own root is open in exactly the sense that
        // matters here: nothing at a use site has instantiated it.
        | FTLocalTypar _
        | FTUnknown _ -> false
        // Every other node is ground iff every child is (vacuously ground leaves
        // included) — an open typar in any child keeps the whole node non-ground.
        | t -> FrozenType.forallChildren ftIsGround t

    /// The `FrozenType → FrozenType` use-site substitution codegen applies to a
    /// type-shape template directly: codegen reads the template and does its own
    /// `FTTypar(Declaring,i) ↦ tyArgs.[i]` substitution — a trivial total walk on
    /// `FrozenType`, touching no `SemType` and no inference state. The frozen
    /// sibling of `instantiateDeclaring`; a method typar is a producer bug
    /// (type-shape templates carry no method axis), so it fails loud.
    ///
    /// Used to expand an abbreviation body against use-site args. Because
    /// `resolveTypeName` deliberately tolerates an arity mismatch (an under-applied
    /// generic abbrev still resolves), a declaring index can land past the provided
    /// args; that leaf degrades to `FTUnknown` rather than crashing — the frozen
    /// counterpart of `translateType`'s unresolved-name → `FTUnknown` arm.
    let rec substituteDeclaring (declaringArgs: FrozenType[]) (template: FrozenType) : FrozenType =
        match template with
        | FTTypar(TyparAxis.Declaring, i) ->
            if i < declaringArgs.Length then
                declaringArgs.[i]
            else
                FTUnknown "<abbrev-arity-mismatch>"
        | FTTypar(TyparAxis.Method, j) ->
            failwithf "FrozenTypeBridge.substituteDeclaring: unexpected method typar %d in a type-shape template" j
        | t -> FrozenType.mapChildren (substituteDeclaring declaringArgs) t

    /// The shared tail of the project-local and external seq-interface witnesses
    /// (`EmitResolve.tryInterfaceWitness` / `ClrRecipes.tryExternalInterfaceWitness`):
    /// find the impl whose compiled name equals `target` (a `qualifiedName`) among
    /// `ifaces` (each `(compiled-name, args-over-declaring-typars)`) and return its
    /// args instantiated at THIS receiver — `FTTypar(Declaring,i) := declArgs.[i]`
    /// via `substituteDeclaring`. `ValueNone` if none matches. Each head reads its
    /// own registry (`env.Classes` vs the codegen symbol provider) and adapts it to
    /// the `(name, args)` shape; this picks + substitutes so the two can't drift.
    let pickInterfaceWitness
        (target: string)
        (declArgs: FrozenType[])
        (ifaces: (string * FrozenType[]) seq)
        : EqArray<FrozenType> voption =
        match
            ifaces
            |> Seq.tryPick (fun (iname, ifaceArgs) ->
                if iname = target then
                    Some(ifaceArgs |> Array.map (substituteDeclaring declArgs) |> EqArray.ofArray)
                else
                    None
            )
        with
        | Some ia -> ValueSome ia
        | None -> ValueNone
