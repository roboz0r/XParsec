namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationSubsume

/// The MUTATING unifier: `unify`, its on-link discharges (deferred dot-accesses,
/// typar constraints, SRTP bounds), and the argument / annotation coercion
/// walkers layered on it. The dependency rule is one-way —
/// `UnificationEngineCore` <- `UnificationSubsume` <- `UnificationEngine`;
/// the lower layers never call back into `unify`.
module UnificationEngine =

    /// The flat `FunN` arity a parameter slot constrains its argument to,
    /// or `ValueNone` for an ordinary (non-`Fun`-bounded) parameter. A combinator
    /// param `'TF :> Fun<a,b>` is arity 1, `'TF :> Fun<a,b,c>` arity 2, up through
    /// `'TF :> Fun<a,b,c,d,e>` arity 4 (arity = type-arg count - 1, for 2..5 args). The
    /// `subsumes` arm decides the `TyFun`↔`FunN` correspondence; this reads the SAME
    /// nominal bound so `inferApp` can record the verdict against the lambda
    /// argument's node (the value-struct flat-`Invoke` lowering reads it at codegen).
    /// Reads the coercion bound off the still-free typar's union-find root.
    let funSlotArityOf (store: TypeStore) (param: SemType) : int voption =
        match resolveStep store param with
        | TyVar tv ->
            let root = UnionFind.find store tv

            store.Constraints.Items root
            |> List.tryPick (fun c ->
                match c.Kind with
                | SemanticConstraintKind.Coercion target ->
                    match resolveStep store target with
                    | TyClass(tk, targs) ->
                        // `Fun`2`..`Fun`5` share this one qualified name, discriminated
                        // by type-arg count (arity = length - 1, for 2..5 args); anything
                        // else is not a recognised `Fun` slot. `funSlotArityOfArgs` is the
                        // single source of that rule (shared with `subsumes`/the discharge).
                        funSlotArityOfArgs (SymbolKeyOps.bareName (SymbolKeyOps.typeMetaName tk)) targs.Length
                    | _ -> None
                | _ -> None
            )
            |> function
                | Some n -> ValueSome n
                | None -> ValueNone
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
    /// `NotNominal` — not a record/class/union, nothing to discharge.
    /// `UnknownType` — named a nominal type the registry doesn't know.
    /// `Resolved` — carries the member-noun used in diagnostics, the
    /// typar→arg substitution, and a name→type lookup over the members.
    ///
    /// `kind` and `memberNoun` are the diagnostic's own discriminators (`NominalKind` /
    /// `MemberNoun`), not strings: a string in either slot is a discriminator nothing
    /// checks, and both are read straight back out into the verdict.
    [<RequireQualifiedAccess>]
    type private DotSource =
        | NotNominal
        | UnknownType of name: string * kind: NominalKind
        | Resolved of
            name: string *
            memberNoun: MemberNoun *
            subst: Dictionary<TyVarId, SemType> *
            lookup: (string -> SemType voption)
        /// A project-local class: member lookup walks the inheritance chain, so
        /// it can't be expressed as the single `subst` + `lookup` pair the
        /// `Resolved` shape carries. The discharge defers to `tryClassChainMember`,
        /// which threads the substitution up the chain per parent. Carries the
        /// class's `TypeKey` (arity included) so the walk keys per-arity; a bare
        /// name is derived only for the not-found diagnostic.
        | ClassChain of key: TypeKey * args: EqArray<SemType>
        /// An *external* class/interface (not in `ctx.Types.Class`): a deferred
        /// dot-access whose receiver TyVar resolved to a BCL/contract nominal
        /// (`System.Collections.IEqualityComparer`). The discharge resolves the
        /// member through the provider — the deferred mirror of `resolveFieldStep`'s
        /// external arm — addressed by the resolved type `key` (store view).
        | ExternalClass of key: SymbolKey * args: EqArray<SemType>

    let private resolveDotSource (ctx: PassContext) (linkTarget: SemType) : DotSource =
        match tryResolveNominal ctx.Store linkTarget with
        | ValueNone -> DotSource.NotNominal
        | ValueSome(NominalKind.Record, key, args) ->
            let (DisplayName name) = SymbolKeyOps.typeSimpleName key

            // Resolve by the arity-qualified key: an arity-overloaded record
            // (`Point`2`/`Point`3`) does not resolve by bare name. `name` still labels the DotSource.
            match TypeRegistry.tryRecordByKey ctx.Types key with
            | ValueSome info ->
                DotSource.Resolved(
                    name,
                    MemberNoun.Field,
                    mkNamedTypeSubst ctx.Store info.TypeParams args,
                    fieldLookup info.Fields
                )
            | ValueNone -> DotSource.UnknownType(name, NominalKind.Record)
        | ValueSome(NominalKind.Class, key, args) ->
            // Membership by the class's key (arity included): an arity-overloaded
            // local class (`Fun`2`/`Fun`3`) does not resolve by bare name, so a bare `ContainsKey`
            // would misclassify it as external. The key rides the `ClassChain` walk.
            if TypeRegistry.containsClassKey ctx.Types key then
                DotSource.ClassChain(key, args)
            else
                // Not project-local — an external (BCL/contract) class or interface
                // whose member resolves through the provider by its resolved key.
                DotSource.ExternalClass(SymbolKey.Type key, args)
        | ValueSome(NominalKind.Union, key, args) ->
            // Resolve by the arity-qualified key (mirror the record arm): an
            // arity-overloaded union (`Choice`2`/`Choice`3`) does not resolve by bare name.
            // `name` is display-only — the `Resolved` label and the `UnknownType`
            // diagnostic.
            let (DisplayName name) = SymbolKeyOps.typeSimpleName key

            match TypeRegistry.tryUnionByKey ctx.Types key with
            | ValueSome info ->
                DotSource.Resolved(
                    name,
                    MemberNoun.InstanceMember,
                    mkNamedTypeSubst ctx.Store info.TypeParams args,
                    memberLookup info.Members
                )
            | ValueNone -> DotSource.UnknownType(name, NominalKind.Union)

    /// `Defer` is the "I don't know yet" answer: the target is still free
    /// (or compound-with-free-args) and a future unification might pin it.
    /// `dischargeConstraints` keeps deferred constraints on the TyVar so they
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

    /// The canon `obj` intrinsic (`TyConst "obj"`) — the ONLY form the root takes
    /// past resolution: `translateType` produces it for source, and metadata
    /// surfacing (`tryBuildType`) eagerly canonicalizes a BCL `System.Object` to it,
    /// so no `TyClass "System.Object"` reaches the unify boundary.
    /// The universal supertype — every value implicitly upcasts (boxing) into it.
    let isObjType (t: SemType) : bool =
        match t with
        | TyObj -> true
        | _ -> false

    /// THE obj-absorption policy: `true` when `expected` is the universal `obj`
    /// supertype (after one resolve step), so an argument coercion must ACCEPT the
    /// actual *without* unifying — the implicit boxing upcast F# inserts, which must
    /// never ground the actual's typar (codegen materialises the box —
    /// `EmitPattern.boxArgIntoObjParam`). The single home of the rule, applied by
    /// `tryCoerceUpcast` (the eager argument / `:>` path) and `unifyArgCoerce` /
    /// `unifyAppliedSig` (the in-`unify`-group deferred dot-access discharge); the two
    /// coercion walkers exist only because they sit either side of `tryCoerceUpcast`
    /// in declaration order, not because the policy differs.
    let absorbsAsObj (store: TypeStore) (expected: SemType) : bool = isObjType (resolveStep store expected)

    /// Matches a carried type-level node (`keyof`/`T[K]`/conditional) that
    /// ground-folds to a CONCRETE (non-carrier) type, binding the folded result —
    /// the single `tryFoldCarried` evaluation serves as both guard and rewrite in
    /// `unify`'s pre-arms. A non-carrier or still-inert node does not match.
    let private (|FoldedCarrier|_|) (ctx: PassContext) (t: SemType) : SemType option =
        match tryFoldCarried ctx t with
        | ValueSome folded -> Some folded
        | ValueNone -> None

    /// The JS `number`-family CONTRAVARIANT widening. A foreign parameter whose canon
    /// is a *platform-repr shared by several primitives* (JS `number` <- int/float/
    /// float32) admits any member of that family at the argument position. Returns the
    /// family as a `TyOr` when `ty` is such a multi-canon repr key, else `ValueNone` —
    /// the overwhelmingly common single-canon case (a genuine `float`/`int` canon is
    /// NOT a platform-repr key, so it never widens; `boolean`/`undefined` are single-
    /// member and gate out by length). Purely data-driven off the reverse intrinsic
    /// axis, so it names no concrete type: the int/float/float32 = `number` relation
    /// lives entirely in `Vesper.Core`'s `.js.fs` intrinsic bindings, flowing here via
    /// the provider (the JS backend owns assignability / intrinsic repr). Confined to the
    /// argument-coercion seams below (`unifyArgCoerce` / `tryCoerceUpcast`) — never a
    /// general `unify`/`subsumes` edge, so nothing widens outside a foreign-call arg.
    let private numericFamilyOr (ctx: PassContext) (ty: SemType) : SemType voption =
        match resolveStep ctx.Store ty with
        // The reverse axis is keyed by the PLATFORM REPR, which is a string — so the only
        // types that can hit it are the ones whose identity IS a platform name: a foreign
        // (TS-manifest / native) type, minted `opaqueKey` in the global namespace. That is
        // what `PlatformName` matches; a Vesper-qualified identity (an intrinsic canon, a
        // user type that happens to be named `number`) is refused, so nothing resolves
        // here by display name.
        | TyConst(PlatformName platform, args) when args.Length = 0 ->
            match ctx.IntrinsicReverseCanon.Value.TryGetValue platform with
            | true, (_ :: _ :: _ as canons) ->
                ValueSome(SemType.MkUnion(seq { for c in canons -> TyConst(c, EqArray.empty) }))
            | _ -> ValueNone
        | _ -> ValueNone

    /// Two intrinsic canons are REPR-SIBLINGS iff they share a forward platform repr
    /// (`IntrinsicForwardRepr[a] = IntrinsicForwardRepr[b]`). On JS this fires exactly on
    /// the numeric family (all `→ "number"`) and extends automatically to any future
    /// shared-repr family; on CLR each canon reprs distinctly, so it never fires. Purely
    /// data-driven off the forward intrinsic axis — it names no concrete type, the
    /// int/float/float32 = `number` relation lives in `Vesper.Core`'s `.js.fs` bindings
    /// (the JS backend owns assignability / intrinsic repr). Used by the structural-width admission
    /// so an `int` record field satisfies a `float` (`number`-repr'd) interface member: a
    /// plain `subsumes` sees `int`≁`float`, but they carry the same runtime repr, so the
    /// value flows. Confined to that seam, never a general `subsumes`/`unify` edge.
    let private reprSiblings (ctx: PassContext) (a: SemType) (b: SemType) : bool =
        match resolveStep ctx.Store a, resolveStep ctx.Store b with
        | TyConst(k1, a1), TyConst(k2, a2) when a1.Length = 0 && a2.Length = 0 ->
            let fwd = ctx.Provider.IntrinsicForwardRepr

            match fwd.TryGetValue k1, fwd.TryGetValue k2 with
            | (true, r1), (true, r2) -> r1 = r2
            | _ -> false
        | _ -> false

    /// Structural inflow admission (G1): a Vesper RECORD satisfies an EXTERNAL interface
    /// PARAMETER by WIDTH. CONFINED to the argument-coercion seams below — gated on the
    /// provider's `IsInterface` (real TS interfaces AND the `@struct` erasing nominals),
    /// so the front end never recognises the `@struct` backend home
    /// (the freeze layer must stay backend-agnostic). Every REQUIRED (non-optional) value
    /// member of the interface must be supplied by a same-named record field whose type
    /// coerces into the member's type — an ordinary `subsumes`, plus the `number`-family
    /// admission (`numericFamilyOr`) so an `int` field satisfies a `number` member. Pure
    /// read (`subsumes`/provider lookups, no `Link`), so it is safe as a speculative guard
    /// before the plain-`unify` fallback. A Vesper CLASS arg (`TyClass`) is left to the
    /// nominal class→interface upcast path (`subtypeNominalOf`); only a record widens here.
    let private tryStructuralWiden (ctx: PassContext) (actual: SemType) (expected: SemType) : bool =
        match resolveStep ctx.Store expected with
        | TyClass(ikey, iargs) ->
            match ctx.Provider.TryLookupType(SymbolKey.Type ikey) with
            // Only an interface is a record-widen target (a capability `IntrinsicInterface` or
            // an interface-flagged `Class`); a non-interface class is excluded off its member
            // surface.
            | ValueSome(ExternalSymbols.ExternalInterfaceMembers ifaceMembers) ->
                match resolveStep ctx.Store actual with
                | TyRecord(rkey, rargs) ->
                    match TypeRegistry.tryRecordByKey ctx.Types rkey with
                    | ValueSome info ->
                        let subst = mkNamedTypeSubst ctx.Store info.TypeParams rargs

                        let fieldTy (name: string) : SemType voption =
                            match info.Fields |> Array.tryFind (fun f -> f.Name = name) with
                            | Some f -> ValueSome(substituteWith ctx.Store subst f.Type)
                            | None -> ValueNone

                        let declArgs = EqArray.toArray iargs

                        ifaceMembers
                        |> Array.filter (fun m -> not m.IsStatic && m.IsValueMember && not m.IsOptional)
                        |> Array.forall (fun m ->
                            match fieldTy m.Name with
                            | ValueNone -> false
                            | ValueSome argTy ->
                                // Realise the member NORMALLY — a covariant interface
                                // `number` value member reads as `float` (the provider's
                                // resolved signature). An `int` record field satisfies it
                                // not by `subsumes` (int ≁ float) but because the two are
                                // REPR-SIBLINGS (both carry JS repr `number`), so the value
                                // flows at this width seam.
                                let expectedTy =
                                    ExternalSymbols.instantiateSignature ctx.Store m declArgs ctx.CurrentLevel

                                subsumes ctx argTy expectedTy <> SubsumeOutcome.Unrelated
                                || reprSiblings ctx argTy expectedTy
                        )
                    | ValueNone -> false
                | _ -> false
            | _ -> false
        | _ -> false

    let rec unify (ctx: PassContext) (tok: SyntaxToken) (a: SemType) (b: SemType) =
        let a = resolveStep ctx.Store a
        let b = resolveStep ctx.Store b

        match a, b with
        // Ground-fold any carried type-level computation (`keyof`/`T[K]`/conditional)
        // that has become CONCRETE before the structural arms: once a method typar inside
        // grounds (`Events[Key]` with `Key := "ping"`), the node collapses to the member
        // type instead of linking a var to an inert carrier. `FoldedCarrier` matches only
        // when the node reaches a non-carrier type, so a STILL-deferred node falls through
        // to the structural carried-vs-carried arms below and cannot loop.
        | FoldedCarrier ctx folded, _ -> unify ctx tok folded b
        | _, FoldedCarrier ctx folded -> unify ctx tok a folded
        // An unresolved head unifies with nothing. Stop — the other side is left untouched
        // (no Link), so one broken head can't cascade into a wrong inference elsewhere.
        //
        // A name THIS UNIT'S SOURCE wrote and nothing defined was already blamed where it was
        // written (`UnificationTranslate`, which recorded it): the mistake is a spelling in
        // the source, not a missing dependency, and re-reporting it at every contact would
        // spray secondary errors across a program whose single fault the user has already been
        // told about. What is left to report here is the OTHER producer of `TyUnknown` — a
        // name a package's baked contract could not resolve, which no site in this file could
        // have blamed.
        | TyUnknown name, _
        | _, TyUnknown name ->
            if not (ctx.UndefinedTypeNames.Contains name) then
                ctx.Report(
                    tok,
                    Kind.Message(
                        sprintf
                            "Type '%s' could not be resolved during contract extraction — is a package dependency missing?"
                            name
                    )
                )
        | TyConst(k1, a1), TyConst(k2, a2) when k1 = k2 && a1.Length = a2.Length -> unifyArgs ctx tok a1 a2
        | TyRecord(n1, a1), TyRecord(n2, a2) when n1 = n2 && a1.Length = a2.Length -> unifyArgs ctx tok a1 a2
        | TyUnion(n1, a1), TyUnion(n2, a2) when n1 = n2 && a1.Length = a2.Length -> unifyArgs ctx tok a1 a2
        // A capability interface reaches `unify` under EITHER of its two names (e.g. a BCL
        // `Enumerable.Take` returns `IEnumerable\`1`, reconciled against a declared `seq`
        // return): the platform key and the canonical key differ, so `n1 = n2`
        // fails though they denote the SAME type. `sameNominalKey` reconciles them — a
        // no-op for every non-capability key (the common `n1 = n2` short-circuits first),
        // which is why capabilities need no entry in the resolution-time reverse-canon map.
        // This is the RETURN / plain-`unify` mirror of the argument-coercion reconciliation.
        | TyClass(n1, a1), TyClass(n2, a2) when
            sameNominalKey ctx (SymbolKey.Type n1) (SymbolKey.Type n2)
            && a1.Length = a2.Length
            ->
            unifyArgs ctx tok a1 a2
        // Two enums unify iff their nominal keys match (enums are niladic — no
        // args to recurse). A `TyEnum` against any other head (e.g. its underlying
        // `int`) falls to the catch-all mismatch below: an enum is a DISTINCT
        // nominal, never structurally its underlying type, so `let n: int = E.C1`
        // is a genuine type error.
        | TyEnum k1, TyEnum k2 when k1 = k2 -> ()
        // Two structural literals unify iff their value matches — external-vocabulary
        // only (nominalism invariant: Vesper never mints a literal). Both sides arise
        // from the SAME external seam: a refined literal argument (R4a step 3 item 2)
        // meets a conditional param folded to a literal (mitt's no-payload `emit`, whose
        // `... ? Key : never` folds to the addressed `Key` literal). A value MISMATCH
        // falls through to the catch-all below (a genuine wrong-key error).
        | TyLiteral v1, TyLiteral v2 when v1 = v2 -> ()
        | TyFun(a1, r1), TyFun(a2, r2) ->
            unify ctx tok a1 a2
            unify ctx tok r1 r2
        | TyTuple xs, TyTuple ys when xs.Length = ys.Length -> unifyArgs ctx tok xs ys
        // Anonymous unions unify by *set equality only* — `EqSet` makes
        // `string | int` and `int | string` the SAME value, so equal unions need no
        // member work (v1 union members are ground — the principality rule — so there
        // is nothing to link). Unequal unions fall through to the mismatch arm.
        // Membership/assignability (`int ≤ int | string`) is NOT handled here: it
        // belongs to the directional `subsumes` layer, never the symmetric core (this
        // arm never widens `int` into `int | string`).
        | TyOr m1, TyOr m2 when m1 = m2 -> ()
        // The carried type-level computations unify STRUCTURALLY, as opaque
        // constructors (like `TyTuple`/`TyFun`) — same head, children unify pairwise.
        // This is NOT evaluation (no `keyof` expansion); it just lets two occurrences
        // of the same carried node (e.g. the same member signature reused) agree.
        // Mismatched heads fall through to the catch-all mismatch below.
        | TyKeyOf t1, TyKeyOf t2 -> unify ctx tok t1 t2
        | TyIndexedAccess(o1, i1), TyIndexedAccess(o2, i2) ->
            unify ctx tok o1 o2
            unify ctx tok i1 i2
        | TyConditional c1, TyConditional c2 ->
            unify ctx tok c1.Check c2.Check
            unify ctx tok c1.Extends c2.Extends
            unify ctx tok c1.WhenTrue c2.WhenTrue
            unify ctx tok c1.WhenFalse c2.WhenFalse
        | TyVar tv1, TyVar tv2 when tv1 = tv2 -> ()
        | TyVar tv1, TyVar tv2 ->
            let r1 = UnionFind.find ctx.Store tv1
            let r2 = UnionFind.find ctx.Store tv2
            let unitsA = ctx.Store.Units r1
            let unitsB = ctx.Store.Units r2
            let linkA = ctx.Store.Link r1
            let linkB = ctx.Store.Link r2
            UnionFind.union ctx.Store r1.Id r2.Id
            // After union, exactly one of r1/r2 still has Parent = ValueNone.
            let newRoot = UnionFind.find ctx.Store r1.Id

            let merged = if newRoot = r1 then r2 else r1

            // Fold the loser's deferred-constraint payload into the surviving
            // representative — one associative set-union join per family (each family's
            // merge order is baked into its store table), replacing the former bespoke
            // `migrateBounds`. Payload lives only under the rep id.
            ctx.Store.MergePayloads(newRoot, merged)
            mergeUnits ctx tok newRoot unitsA unitsB
            // If both sides carried links, unify them so the carriers agree.
            match linkA, linkB with
            | ValueNone, ValueNone -> ()
            | ValueSome t, ValueNone
            | ValueNone, ValueSome t ->
                ctx.Store.SetLink(newRoot, ValueSome t)
                dischargeAll ctx tok newRoot t
            | ValueSome a, ValueSome b ->
                ctx.Store.SetLink(newRoot, linkA)
                unify ctx tok a b
                dischargeAll ctx tok newRoot a
        | TyVar tv, other
        | other, TyVar tv ->
            let root = UnionFind.find ctx.Store tv

            if occursAndAdjust ctx.Store root.Id other then
                ctx.Report(
                    tok,
                    Kind.Message(
                        sprintf
                            "Occurs check: cannot construct infinite type %A = %A"
                            (zonk ctx.Store (TyVar root.Id))
                            (zonk ctx.Store other)
                    )
                )
            else
                // Linking to a plain TyConst (a dimensionless carrier) when
                // the variable is already known to be measured is a
                // dimensionless-vs-measured mismatch.
                match ctx.Store.Units root, other with
                | ValueSome m, TyConst _ when not m.IsDimensionless ->
                    ctx.Report(tok, Kind.Message(sprintf "Dimensionless %A used where <%O> expected" other m))
                | _ -> ()

                ctx.Store.SetLink(root, ValueSome other)
                dischargeAll ctx tok root other
        | _ -> ctx.Report(tok, Kind.Message(sprintf "Type mismatch: %A vs %A" (zonk ctx.Store a) (zonk ctx.Store b)))

    /// Unify two same-length type-argument vectors positionally — the shared body
    /// of the `TyConst` / `TyRecord` / `TyUnion` / `TyClass` / `TyTuple` arms (each
    /// already guards `length` equality). In the `unify` rec group so it stays a
    /// direct call with no per-`unify` closure allocation on the hot path.
    and private unifyArgs (ctx: PassContext) (tok: SyntaxToken) (xs: EqArray<SemType>) (ys: EqArray<SemType>) : unit =
        for i in 0 .. xs.Length - 1 do
            unify ctx tok xs.[i] ys.[i]

    /// Coerce a single argument position against its expected parameter type: an
    /// `obj` parameter absorbs *any* argument (the implicit upcast / box F# inserts
    /// at the call), so it must NOT unify — pinning a typar argument (`x : 'T`) to
    /// `obj` would ground the enclosing type's parameter. Tuples walk element-wise
    /// (a tupled BCL call `Equals(obj, obj)`). The in-`unify`-group analogue of
    /// `unifyArg`'s `obj` rule, usable from the deferred-discharge path below (`unifyArg`
    /// itself is defined after this group). `obj` and union-typed parameters are the
    /// two no-pin absorptions here; richer class→interface witness coercion stays in
    /// `unifyArg`/`tryCoerceUpcast` for the eager application path.
    and private unifyArgCoerce (ctx: PassContext) (tok: SyntaxToken) (actual: SemType) (expected: SemType) : unit =
        match resolveStep ctx.Store actual, resolveStep ctx.Store expected with
        | TyTuple aa, TyTuple bb when aa.Length = bb.Length ->
            for i in 0 .. aa.Length - 1 do
                unifyArgCoerce ctx tok aa.[i] bb.[i]
        | a, b ->
            // `b` is already `resolveStep`-ed by the match; `absorbsAsObj` (the one
            // obj-policy home) re-steps idempotently.
            if absorbsAsObj ctx.Store b then
                ()
            else
                match b with
                // A union-typed parameter admits any argument that subsumes into a member,
                // with the same no-pin discipline as `obj` (the commit-path analogue of
                // `tryCoerceUpcast`'s `TyOr` arm). `subsumes` itself folds carried nodes
                // nested in union members (`foldMemberCarried`), so no pre-fold here. A
                // genuine non-member argument still falls through to `unify` and errors.
                | TyOr _ when subsumes ctx a b <> SubsumeOutcome.Unrelated -> ()
                // A foreign `number`-family parameter admits any member of its family
                // (int/float/float32) with the same no-pin discipline — the family is a
                // synthetic `TyOr` off the reverse intrinsic axis (`numericFamilyOr`).
                | _ ->
                    match numericFamilyOr ctx b with
                    | ValueSome fam when subsumes ctx a fam <> SubsumeOutcome.Unrelated -> ()
                    | _ -> if tryStructuralWiden ctx a b then () else unify ctx tok a b

    /// Unify an *applied callable* shape against a resolved member signature,
    /// coercing each argument position rather than unifying it. `actual` is the
    /// call's applied shape — a curried `TyFun` chain whose domains are the argument
    /// types the application built (`comparer.GetHashCode(x)` → `TyFun('T, result)`);
    /// `expected` is the member's instantiated signature (`TyFun(obj, int)`). Each
    /// parameter position goes through `unifyArgCoerce` (so an `obj` parameter
    /// absorbs a typar / value-type argument instead of grounding it); result
    /// positions unify exactly. Used where a *whole* signature is unified against a
    /// pre-built `TyFun` (the deferred dot-access discharge, the overload-commit), unlike
    /// `inferApp`'s argument walk which already coerces each argument as it applies it.
    and unifyAppliedSig (ctx: PassContext) (tok: SyntaxToken) (actual: SemType) (expected: SemType) : unit =
        match resolveStep ctx.Store actual, resolveStep ctx.Store expected with
        | TyFun(ad, ar), TyFun(ed, er) ->
            unifyArgCoerce ctx tok ad ed
            unifyAppliedSig ctx tok ar er
        | a, b -> unify ctx tok a b

    /// When a TyVar's Link resolves to a `TyRecord`/`TyClass`/`TyUnion`,
    /// resolve any dot-access constraints parked on it. When `T` is generic,
    /// the receiver's arg list substitutes for the type's declared typars so
    /// `(b : Box<int>).Value` resolves to `int`, not `Box`'s prototype `'a`.
    /// Fire all three on-link callbacks for a root whose `Link` just resolved
    /// to `t`: deferred dot-accesses, type-parameter constraints, and SRTP
    /// member-trait bounds.
    and private dischargeAll (ctx: PassContext) (tok: SyntaxToken) (root: Rep) (t: SemType) : unit =
        dischargePendingDotAccess ctx root t
        dischargeConstraints ctx tok root t
        dischargeSrtpBounds ctx tok root t

    and private dischargePendingDotAccess (ctx: PassContext) (root: Rep) (linkTarget: SemType) : unit =
        let pending = ctx.Store.Pda.Live root

        if not (List.isEmpty pending) then
            // Every resolving branch discharges the whole snapshot up front — so a
            // reentrant discharge (the `unify`s below) sees it gone and the leftover-
            // unresolved check never re-fires a resolved access. Only NotNominal leaves
            // the accesses parked for a later `Link`. The single home of that discharge.
            let solveAll () =
                for d in pending do
                    ctx.Store.Pda.Solve d

            match resolveDotSource ctx linkTarget with
            | DotSource.NotNominal -> ()
            | DotSource.UnknownType(name, kind) ->
                solveAll ()

                for d in pending do
                    ctx.Report(d.Use.Tok, Kind.UnknownNominalType(kind, name))
            | DotSource.Resolved(name, memberNoun, subst, lookup) ->
                solveAll ()

                for d in pending do
                    match lookup d.MemberName with
                    | ValueSome ty -> unify ctx d.Use.Tok (TyVar d.ResultTv) (substituteWith ctx.Store subst ty)
                    | ValueNone -> ctx.Report(d.Use.Tok, Kind.NoMember(name, memberNoun, d.MemberName))
            | DotSource.ClassChain(key, args) ->
                // `tryClassChainMember` already returns the type instantiated
                // against `args` (and any parent typar substitution), so no
                // further `substituteWith` is needed here.
                solveAll ()

                let (DisplayName shown) = SymbolKeyOps.typeSimpleName key

                for d in pending do
                    match tryClassChainMember ctx key args d.MemberName with
                    | ValueSome ty -> unify ctx d.Use.Tok (TyVar d.ResultTv) ty
                    | ValueNone -> ctx.Report(d.Use.Tok, Kind.NoMember(shown, MemberNoun.InstanceMember, d.MemberName))
            | DotSource.ExternalClass(key, args) ->
                // Deferred mirror of `resolveFieldStep`'s external arm: the receiver
                // TyVar resolved to a BCL/contract class or interface (e.g. the
                // `comparer: IEqualityComparer` parameter of an `IStructuralEquatable`
                // member, pinned by the interface-conformance unify only *after* the
                // body — and its dot-accesses — were deferred). Resolve each member
                // through the provider and record it for Elaborate.
                solveAll ()

                let argArr = args.AsSpan().ToArray()

                for d in pending do
                    match ctx.Provider.TryLookupMember(key, d.MemberName) with
                    | ValueSome m when not m.IsStatic ->
                        let memberSig = ExternalSymbols.openSignature m argArr

                        ctx.Resolution.ExternalAccess.Set(
                            d.Use.Key,
                            {
                                Key = SymbolKey.Member m.Key
                                IsStatic = false
                                Storage = m.Storage
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
                        // Elaborate reads for the box, since this unify deliberately
                        // leaves the node typed with the un-grounded arg typar.
                        unifyAppliedSig ctx d.Use.Tok (TyVar d.ResultTv) memberSig
                    | _ ->
                        ctx.Report(
                            d.Use.Tok,
                            Kind.NoMember(SymbolKeyOps.qualifiedName key, MemberNoun.InstanceMember, d.MemberName)
                        )

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
    /// least one is still pending. Takes any `seq` — it only iterates — so
    /// callers pass their natural element view without materialising a copy.
    and private reduceOutcome (check: SemType -> ConstraintOutcome) (items: seq<SemType>) : ConstraintOutcome =
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
    /// check via `dischargeConstraints`; nested compounds recurse compositionally.
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
            (fieldsOf: unit -> seq<SemType>)
            : ConstraintOutcome =
            match c.Kind, eq, cmp with
            | SemanticConstraintKind.Equality, EqualityVerdict.NoEquality, _ -> Violated
            | SemanticConstraintKind.Equality, (EqualityVerdict.Reference | EqualityVerdict.Custom), _ -> Satisfied
            | SemanticConstraintKind.Comparison, _, ComparisonVerdict.NoComparison -> Violated
            | SemanticConstraintKind.Comparison, _, ComparisonVerdict.Custom -> Satisfied
            | _ -> reduceOutcome (checkConstraint ctx c) (fieldsOf ())

        match c.Kind, resolveStep ctx.Store t with
        | _, TyVar _ -> Defer
        // An unresolved contract head supports no constraint, but the
        // mismatch is already reported where it unified — defer here so the
        // constraint quietly never re-fires rather than emitting a second error.
        | _, TyUnknown _ -> Defer
        // Post-freeze only; never reached during constraint solving. Defer
        // (consistent with TyUnknown) rather than crash.
        | _, TyTypar _ -> Defer
        // A `TyEnum` can now appear here — use-site annotations (`(x: E)`) and
        // member access (`E.C1`) produce one. v1 deliberately gives enums no
        // bespoke constraint verdict: equality on an enum is structural /
        // universal (so a `when 'T : equality` instantiation needs nothing
        // proved), and comparison / `<` on enums is out of v1 scope. With no
        // enum-specific verdict this arm falls through to `Defer` — the safe
        // default shared with the sibling `TyUnknown` / `TyTypar` arms (never
        // a false `Violated`).
        | _, TyEnum _ -> Defer
        // A carried type-level computation (keyof / indexed / conditional) can't have
        // ANY constraint decided until it grounds (step 3), so Defer for every kind —
        // the safe default shared with the TyUnknown / TyTypar / TyEnum arms (never a
        // false `Violated`). Placed before the `Coercion` / primitive arms so it wins
        // regardless of kind. External-vocabulary only; a Vesper program never puts one
        // under a constraint.
        | _, (TyKeyOf _ | TyIndexedAccess _ | TyConditional _) -> Defer
        // A structural literal erases to its base primitive — delegate the verdict to
        // it (external-vocabulary only, so this is defensive; Vesper never mints one).
        // Genuine delegation (not a copy of the `TyConst` arm) so EVERY kind — including
        // `Coercion`, whose arm sits below and decides via `subsumes` — is judged
        // exactly as the base primitive would be.
        | _, TyLiteral v -> checkConstraint ctx c (TyConst(RuntimeNames.primitiveKey v.BaseName, EqArray.empty))
        | SemanticConstraintKind.Coercion target, _ ->
            // `'e :> exn`: now that `'e` has a nominal head, does it subsume to
            // the required supertype? `subsumes` walks user AND external (BCL)
            // `inherit` chains, and reconciles `exn`'s `TyConst` with the metadata
            // `TyClass("System.Exception", _)` via IntrinsicReprTypes — so a thrown
            // `InvalidOperationException` reaches `exn`. Read-only, so it's safe to
            // run from the discharge callback (no undo trace). Past the `TyVar _` guard
            // above, `Unrelated` is a real violation, not "unknown yet".
            match subsumes ctx t target with
            | SubsumeOutcome.Equal
            | SubsumeOutcome.Subtype -> Satisfied
            | SubsumeOutcome.Unrelated -> Violated
        | k, TyConst(nameKey, _) ->
            match primitiveSupports k (SymbolKeyOps.intrinsicName nameKey) with
            | ValueSome true -> Satisfied
            | ValueSome false -> Violated
            | ValueNone -> Defer
        | (SemanticConstraintKind.Equality | SemanticConstraintKind.Comparison), TyFun _ ->
            // Function types support neither structural equality nor
            // comparison in F#.
            Violated
        | (SemanticConstraintKind.Equality | SemanticConstraintKind.Comparison), TyTuple items ->
            reduceOutcome (checkConstraint ctx c) (items.Underlying :> seq<SemType>)
        | (SemanticConstraintKind.Equality | SemanticConstraintKind.Comparison), TyRecord(recKey, args) ->
            match TypeRegistry.tryRecordByKey ctx.Types recKey with
            | ValueSome info ->
                verdictOutcome
                    info.EqualitySupport
                    info.ComparisonSupport
                    (fun () ->
                        let subst = mkNamedTypeSubst ctx.Store info.TypeParams args

                        info.Fields |> Seq.map (fun f -> substituteWith ctx.Store subst f.Type)
                    )
            | ValueNone -> Defer
        | (SemanticConstraintKind.Equality | SemanticConstraintKind.Comparison), TyUnion(unionKey, args) ->
            match TypeRegistry.tryUnionByKey ctx.Types unionKey with
            | ValueSome info ->
                verdictOutcome
                    info.EqualitySupport
                    info.ComparisonSupport
                    (fun () ->
                        let subst = mkNamedTypeSubst ctx.Store info.TypeParams args
                        let fields = ResizeArray<SemType>()

                        for case in info.Cases do
                            for field in case.Fields do
                                fields.Add(substituteWith ctx.Store subst field)

                        fields :> seq<SemType>
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
                        let subst = mkNamedTypeSubst ctx.Store info.TypeParams args

                        info.InstanceFields |> Seq.map (fun f -> substituteWith ctx.Store subst f.Type)
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
            reduceOutcome (checkConstraint ctx c) (members.Members.Underlying :> seq<SemType>)
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
    and private dischargeConstraints (ctx: PassContext) (tok: SyntaxToken) (root: Rep) (linkTarget: SemType) : unit =
        if ctx.Store.Constraints.IsEmpty root then
            ()
        else
            let cs = ctx.Store.Constraints.Items root
            ctx.Store.Constraints.Set(root, [])
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
                    match subtypeNominalOf ctx (zonk ctx.Store target) with
                    | ValueSome(struct (tname, targs)) when
                        targs.Length > 0
                        && targs
                           |> EqArray.exists (fun a ->
                               match resolveStep ctx.Store a with
                               | TyVar _ -> true
                               | _ -> false
                           )
                        ->
                        match tryUpcastWitness ctx linkTarget tname with
                        | ValueSome wargs when wargs.Length = targs.Length ->
                            for i in 0 .. targs.Length - 1 do
                                unify ctx tok wargs.[i] targs.[i]
                        | _ -> ()
                    | _ -> ()
                | _ -> ()

                // The INVERSE direction for the `TyFun`↔`Fun`2`..`Fun`5` correspondence:
                // a source lambda whose function type has STILL-FREE
                // domains (`fun x y -> x + y` — no literal pins `x`/`y`) coerced into a
                // GROUND constrained slot (`'TF :> Fun<int,int,int>`) must ground from
                // the slot's args, so the lambda body's SRTP operators resolve instead
                // of leaking `?free-typar`. `subsumes` itself stays read-only
                // (it only *checks* invariant-equality); this is the one place the
                // grounding `unify` lives. Arity-parametric (1..4): peel exactly
                // `targs.Length - 1` domains, `unify` each with the slot's ground
                // arg, then `unify` the residual codomain with the last arg (matched
                // whole — a `n > K` printf tail stays curried, not peeled). A chain too
                // short to peel `k` domains grounds NOTHING. Non-`Fun` coercions and a
                // non-`TyFun` `linkTarget` are untouched.
                match c.Kind with
                | SemanticConstraintKind.Coercion target ->
                    match subtypeNominalOf ctx (zonk ctx.Store target), resolveStep ctx.Store linkTarget with
                    | ValueSome(struct (tname, targs)), TyFun(a, b) when
                        funSlotArityOfArgs (SymbolKeyOps.bareName (SymbolKeyOps.qualifiedName tname)) targs.Length
                        |> Option.isSome
                        ->
                        // `peelFunDomains` (shared with `subsumes`) yields the `k+1` types
                        // aligned to `targs` — a too-short chain grounds NOTHING. The
                        // check-side and this grounding side peel identically by
                        // construction.
                        match peelFunDomains ctx.Store (targs.Length - 1) a b with
                        | Some tys -> tys |> List.iteri (fun i s -> unify ctx tok s targs.[i])
                        | None -> ()
                    | _ -> ()
                | _ -> ()

                match checkConstraint ctx c linkTarget with
                | Satisfied -> ()
                | Violated -> reportConstraintViolation ctx tok c linkTarget
                | Defer ->
                    remaining <- c :: remaining
                    propagateToFreeArgs ctx c linkTarget

            ctx.Store.Constraints.Set(root, List.rev remaining)

    /// THE one spelling of "this type does not answer that constraint". Both places a
    /// constraint is checked against a settled type — the discharge above and abbreviation
    /// expansion — report through here, so the two cannot render the same violation
    /// differently.
    and reportConstraintViolation
        (ctx: PassContext)
        (tok: SyntaxToken)
        (c: SemanticConstraint)
        (target: SemType)
        : unit =
        ctx.Report(tok, Kind.ConstraintNotSupported(shown ctx.Store target, constraintKindName c.Kind))

    /// When a compound shape is partially resolved, the parent constraint is
    /// satisfied iff every component supports it, so a still-free component
    /// carries the same constraint forward.
    and propagateToFreeArgs (ctx: PassContext) (c: SemanticConstraint) (t: SemType) : unit =
        // Every child of a compound carries the constraint forward: an anonymous
        // union supports a structural constraint iff every member does
        // (`checkConstraint`'s all-members rule), and a carried type-level
        // computation defers its constraint, so its still-free children take it
        // so the check re-fires on grounding.
        let rec walk t =
            match resolveStep ctx.Store t with
            | TyVar tv ->
                let root = UnionFind.find ctx.Store tv
                addConstraintByKind ctx.Store root.Id c
            | t -> SemType.iterChildren walk t

        walk t

    and private tryDeclaredIntrinsicMember
        (ctx: PassContext)
        (key: SymbolKey)
        (args: EqArray<SemType>)
        (memberName: string)
        : SemType voption =
        let fromHost =
            match ctx.Types.IntrinsicAbbrevHost.TryGetValue(SymbolKeyOps.intrinsicName key) with
            | true, info ->
                match info.Members |> Array.tryFind (fun m -> m.IsStatic && m.Name = memberName) with
                | Some m -> ValueSome(instantiateMember ctx.Store (info.TypeParams, args) m.Type)
                | None -> ValueNone
            | false, _ -> ValueNone

        match fromHost with
        | ValueSome _ -> fromHost
        | ValueNone ->
            match ctx.Provider.TryLookupMember(key, memberName) with
            | ValueSome m when m.IsStatic -> ValueSome(ExternalSymbols.openSignature m (EqArray.toArray args))
            | _ -> ValueNone

    /// Build the expected trait signature in tupled or curried form,
    /// picking whichever matches the candidate's shape. F# accepts both
    /// `static member (+)(a, b)` (tupled) and `static member (+) a b`
    /// (curried) as satisfying a trait declared `^T * ^T -> ^T`.
    and private unifySrtpAgainst
        (ctx: PassContext)
        (tok: SyntaxToken)
        (candidate: SemType)
        (bound: MemberSignature)
        : unit =
        let argTys = bound.ArgTypes

        let tupled =
            match argTys.Length with
            | 0 -> bound.ReturnType
            | 1 -> TyFun(argTys.[0], bound.ReturnType)
            | _ -> TyFun(TyTuple argTys, bound.ReturnType)

        match resolveStep ctx.Store candidate with
        | TyFun(TyTuple _, _) -> unify ctx tok candidate tupled
        | _ when argTys.Length >= 2 ->
            let curried = EqArray.foldBack (fun a r -> TyFun(a, r)) argTys bound.ReturnType

            unify ctx tok candidate curried
        | _ -> unify ctx tok candidate tupled

    /// On-unified callback for SRTP member-trait bounds. Bounds live in the store's
    /// `Srtp` table under the representative id; a dispatched bound is recorded in the
    /// `solved` set — since the one `MemberSignature` instance is shared by reference
    /// across every participating typar, solving it through whichever typar links
    /// first makes the others' discharges skip it. A bound that cannot dispatch yet
    /// (target still a free TyVar, or an unknown class) is left UNSOLVED and grows in
    /// place, so the next `Link` change re-attempts it — no remainder is written back.
    ///
    /// Diagnostics use `tok` — the user's call site, threaded through from
    /// the caller — so "Type X has no static member Y" points there rather
    /// than at the prelude's `(+)` declaration.
    and private dischargeSrtpBounds (ctx: PassContext) (tok: SyntaxToken) (root: Rep) (linkTarget: SemType) : unit =
        let bounds = ctx.Store.Srtp.Live root

        if List.isEmpty bounds then
            ()
        else
            for b in bounds do
                // A sibling / reentrant discharge (via `unifySrtpAgainst`) may have solved
                // `b` since this snapshot — skip it, as the shared `Resolved` flag used
                // to.
                if not (ctx.Store.Srtp.IsSolved b) then
                    match resolveStep ctx.Store linkTarget with
                    | TyConst(primKey, primArgs) ->
                        match tryDeclaredIntrinsicMember ctx primKey primArgs b.MemberName with
                        | ValueSome candTy ->
                            ctx.Store.Srtp.Solve b
                            unifySrtpAgainst ctx tok candTy b
                        | ValueNone ->
                            let primName = SymbolKeyOps.intrinsicName primKey

                            ctx.Report(tok, Kind.NoMember(primName, MemberNoun.BuiltInStaticMember, b.MemberName))

                            ctx.Store.Srtp.Solve b
                    | TyClass(classKey, classArgs) ->
                        match TypeRegistry.tryClassByKey ctx.Types classKey with
                        | ValueSome info ->
                            match info.Members |> Array.tryFind (fun m -> m.IsStatic && m.Name = b.MemberName) with
                            | Some m ->
                                let candTy = instantiateMember ctx.Store (info.TypeParams, classArgs) m.Type
                                ctx.Store.Srtp.Solve b
                                unifySrtpAgainst ctx tok candTy b
                            | None ->
                                let (DisplayName shown) = SymbolKeyOps.typeSimpleName classKey

                                ctx.Report(tok, Kind.NoMember(shown, MemberNoun.StaticMember, b.MemberName))

                                ctx.Store.Srtp.Solve b
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
                            match ctx.Provider.TryLookupMember(SymbolKey.Type classKey, b.MemberName) with
                            | ValueSome m when m.IsStatic ->
                                let candTy = ExternalSymbols.openSignature m (EqArray.toArray classArgs)

                                ctx.Store.Srtp.Solve b
                                unifySrtpAgainst ctx tok candTy b
                            | _ ->
                                // Unknown class — leave unsolved so a later pass may
                                // dispatch.
                                ()
                    | _ ->
                        // Target not yet a concrete type-bearing shape — leave unsolved.
                        ()

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
    let tryCoerceUpcast (ctx: PassContext) (tok: SyntaxToken) (src: SemType) (tgt: SemType) : bool =
        // `obj` is the universal supertype: F# implicitly upcasts (boxing a value
        // type / a generic typar) any value into an `obj` slot, so accept *any*
        // `src` without unifying. Crucially this must NOT pin `src` — a generic
        // typar argument (`comparer.GetHashCode(x)` with `x : 'T`) flowing into an
        // `obj` parameter would otherwise unify `'T := obj`, grounding the
        // enclosing type's parameter (the Vesper.Set `Set<'T>` whole-class-typar
        // grounding). The box is inserted at codegen (the call site sees the param
        // is `obj` and the arg's static type is a typar / value type).
        if absorbsAsObj ctx.Store tgt then
            true
        else

            // A union-typed slot is `obj` restricted to an enumerated member set: it
            // accepts any value that subsumes into one of its members, with the *same*
            // no-pin discipline as `obj` above. `subsumes` is a pure read (no `Link`),
            // so a generic value threaded through a union-typed parameter is not wrongly
            // grounded — the `acceptsByAssignability` generalisation of `absorbsAsObj`.
            match resolveStep ctx.Store tgt with
            // `subsumes` itself folds carried nodes nested in union members
            // (`foldMemberCarried`), so no pre-fold here.
            | TyOr _ -> subsumes ctx src tgt <> SubsumeOutcome.Unrelated
            | _ ->

                // A foreign `number`-family parameter (int/float/float32 share JS repr
                // `number`) admits any family member at the eager application seam, same
                // no-pin discipline as the `TyOr`/`obj` arms. Off the reverse intrinsic axis.
                match numericFamilyOr ctx tgt with
                | ValueSome fam -> subsumes ctx src fam <> SubsumeOutcome.Unrelated
                | ValueNone ->

                    if tryStructuralWiden ctx src tgt then
                        true
                    else

                        match subtypeNominalOf ctx tgt with
                        | ValueNone -> false
                        | ValueSome(struct (tname, targs)) ->
                            match tryUpcastWitness ctx src tname with
                            | ValueSome sargs when sargs.Length = targs.Length ->
                                for i in 0 .. targs.Length - 1 do
                                    unify ctx tok sargs.[i] targs.[i]

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
    let rec unifyArg (ctx: PassContext) (tok: SyntaxToken) (actual: SemType) (expected: SemType) : unit =
        match resolveStep ctx.Store actual, resolveStep ctx.Store expected with
        | TyTuple aa, TyTuple bb when aa.Length = bb.Length ->
            for i in 0 .. aa.Length - 1 do
                unifyArg ctx tok aa.[i] bb.[i]
        | a, b ->
            if not (tryCoerceUpcast ctx tok a b) then
                unify ctx tok a b

    /// Reconcile an inferred type against a *written annotation* (a `let` return
    /// type, a parameter `Pat.Typed`). Checking-mode, but narrower than `unifyArg`:
    /// the assignability admitted is value→union — when the annotation is a
    /// `TyOr` the actual subsumes into, accept *without* unifying, so `let x: int |
    /// string = 1` checks and the actual's typar stays free (the no-pin discipline) —
    /// plus the nominal upcast of a CONCRETE subtype actual into a supertype
    /// annotation (`: exn = e`, `: obj = e` — the read-only `subsumes` walk, strict
    /// `Subtype` only). Every other annotation — a same-nominal, an unrelated type —
    /// falls through to symmetric `unify`, so a binder annotated `obj` whose actual
    /// is a still-free var GROUNDS to `obj` (unlike `unifyArg`, whose `absorbsAsObj`
    /// accept would leave the binder's var unresolved and break signature encoding).
    /// The no-unify branches are reached only when `subsumes` already succeeds, i.e.
    /// the actual is concrete enough to subsume — so nothing is left ungrounded. The
    /// principality rule holds: a union enters only by an annotation, and `unify`
    /// never synthesises one.
    let unifyAnnotation (ctx: PassContext) (tok: SyntaxToken) (actual: SemType) (expected: SemType) : unit =
        // A literal / pure-literal-union actual — the only actual admitted to the
        // outward-widening arm below (keeps that arm strictly additive: a non-literal
        // union still grounds via symmetric `unify`, unchanged).
        let rec isLiteralBearing t =
            match resolveStep ctx.Store t with
            | TyLiteral _ -> true
            | TyOr ms -> ms.Members |> EqSet.forall isLiteralBearing
            | _ -> false

        // Both admissions reduce to the SAME check — the actual subsumes into the
        // annotation (`subsumes <> Unrelated`) — under complementary guards, so they
        // merge into one arm: a `TyOr` annotation admits any subsuming actual
        // (value→union, `let x: int | string = 1`); a non-union annotation admits it
        // only for a literal-bearing actual (OUTWARD widening, `let s: string =
        // getMode()`, design §"reading a literal-typed value back into Vesper needs
        // nothing new"). Everything else — a plain nominal / non-literal union
        // annotated to a supertype — still GROUNDS via symmetric `unify`.
        match resolveStep ctx.Store expected with
        | expected' when
            (match expected' with
             | TyOr _ -> true
             | _ -> isLiteralBearing actual)
            && subsumes ctx actual expected <> SubsumeOutcome.Unrelated
            ->
            ()
        // NOMINAL upcast admission: a concrete nominal actual annotated to a strict
        // SUPERTYPE (`let toExn (e: InvalidOperationException) : exn = e`, `: obj = e`)
        // coerces by the read-only subtype walk. STRICTLY `Subtype`, never `Equal` — a
        // same-nominal annotation still grounds via `unify` so its type args link. The
        // walk succeeds only through genuine declared/metadata heritage (a struct /
        // primitive actual has no chain to `obj`), so this admits no value→`obj` boxing
        // seam; and a `TyVar` actual has no nominal identity (`subtypeNominalOf` misses),
        // so an unresolved binder still GROUNDS via `unify` below.
        | _ when subsumes ctx actual expected = SubsumeOutcome.Subtype -> ()
        | _ -> unify ctx tok actual expected
