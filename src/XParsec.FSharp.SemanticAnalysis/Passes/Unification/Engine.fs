namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationSubsume

/// The MUTATING unifier: `unify`, its on-link drains (deferred dot-accesses,
/// typar constraints, SRTP bounds), and the argument / annotation coercion
/// walkers layered on it. The dependency rule is one-way —
/// `UnificationEngineCore` <- `UnificationSubsume` <- `UnificationEngine`;
/// the lower layers never call back into `unify`.
module UnificationEngine =

    /// The flat `FunN` arity a parameter slot constrains its argument to,
    /// or `ValueNone` for an ordinary (non-`Fun`-bounded) parameter. A combinator
    /// param `'TF :> Fun<a,b>` is arity 1, `'TF :> Fun<a,b,c>` arity 2, up through
    /// `'TF :> Fun<a,b,c,d,e>` arity 4 (arity = type-arg count - 1, for 2..5 args). The
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
                        // `Fun`2`..`Fun`5` share this one qualified name, discriminated
                        // by type-arg count (arity = length - 1, for 2..5 args); anything
                        // else is not a recognised `Fun` slot. `funSlotArityOfArgs` is the
                        // single source of that rule (shared with `subsumes`/the drain).
                        funSlotArityOfArgs (SymbolKeyOps.bareName (SymbolKeyOps.qualifiedName tk)) targs.Length
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
        /// which threads the substitution up the chain per parent. Carries the
        /// class's `SymbolKey` (arity included) so the walk keys per-arity; a bare
        /// name is derived only for the not-found diagnostic.
        | ClassChain of key: SymbolKey * args: EqArray<SemType>
        /// An *external* class/interface (not in `ctx.Types.Class`): a deferred
        /// dot-access whose receiver TyVar resolved to a BCL/contract nominal
        /// (`System.Collections.IEqualityComparer`). The drain resolves the
        /// member through the provider — the deferred mirror of `resolveFieldStep`'s
        /// external arm — addressed by the resolved type `key` (store face).
        | ExternalClass of key: SymbolKey * args: EqArray<SemType>

    let private resolveDotSource (ctx: PassContext) (linkTarget: SemType) : DotSource =
        match tryResolveNominal linkTarget with
        | ValueNone -> DotSource.NotNominal
        | ValueSome(NominalKind.Record, key, args) ->
            let (DisplayName name) = SymbolKeyOps.simpleName key

            // Resolve by the arity-qualified key: an arity-overloaded record
            // (`Point`2`/`Point`3`) does not resolve by bare name. `name` still labels the DotSource.
            match TypeRegistry.tryRecordByKey ctx.Types key with
            | ValueSome info ->
                DotSource.Resolved(name, "field", mkNamedTypeSubst info.TypeParams args, fieldLookup info.Fields)
            | ValueNone -> DotSource.UnknownType(name, "record")
        | ValueSome(NominalKind.Class, key, args) ->
            // Membership by the class's key (arity included): an arity-overloaded
            // local class (`Fun`2`/`Fun`3`) does not resolve by bare name, so a bare `ContainsKey`
            // would misclassify it as external. The key rides the `ClassChain` walk.
            if TypeRegistry.containsClassKey ctx.Types key then
                DotSource.ClassChain(key, args)
            else
                // Not project-local — an external (BCL/contract) class or interface
                // whose member resolves through the provider by its resolved key.
                DotSource.ExternalClass(key, args)
        | ValueSome(NominalKind.Union, key, args) ->
            // Resolve by the arity-qualified key (mirror the record arm): an
            // arity-overloaded union (`Choice`2`/`Choice`3`) does not resolve by bare name.
            // `name` is display-only — the `Resolved` label and the `UnknownType`
            // diagnostic.
            let (DisplayName name) = SymbolKeyOps.simpleName key

            match TypeRegistry.tryUnionByKey ctx.Types key with
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
    /// `unifyAppliedSig` (the in-`unify`-group deferred dot-access drain); the two
    /// coercion walkers exist only because they sit either side of `tryCoerceUpcast`
    /// in declaration order, not because the policy differs.
    let absorbsAsObj (expected: SemType) : bool = isObjType (resolveStep expected)

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
        match resolveStep ty with
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
        match resolveStep a, resolveStep b with
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
        match resolveStep expected with
        | TyClass(ikey, iargs) ->
            match ctx.Provider.TryLookupType ikey with
            // Only an interface is a record-widen target (a capability `IntrinsicInterface` or
            // an interface-flagged `Class`); a non-interface class is excluded off its member
            // surface.
            | ValueSome(ExternalSymbols.ExternalInterfaceMembers ifaceMembers) ->
                match resolveStep actual with
                | TyRecord(rkey, rargs) ->
                    match TypeRegistry.tryRecordByKey ctx.Types rkey with
                    | ValueSome info ->
                        let subst = mkNamedTypeSubst info.TypeParams rargs

                        let fieldTy (name: string) : SemType voption =
                            match info.Fields |> Array.tryFind (fun f -> f.Name = name) with
                            | Some f -> ValueSome(substituteWith subst f.Type)
                            | None -> ValueNone

                        let declArgs = iargs |> EqArray.toList |> List.toArray

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
                                let expectedTy = ExternalSymbols.instantiateSignature m declArgs ctx.CurrentLevel

                                subsumes ctx argTy expectedTy <> SubsumeOutcome.Unrelated
                                || reprSiblings ctx argTy expectedTy
                        )
                    | ValueNone -> false
                | _ -> false
            | _ -> false
        | _ -> false

    let rec unify (ctx: PassContext) (key: NodeKey) (a: SemType) (b: SemType) =
        let a = resolveStep a
        let b = resolveStep b

        match a, b with
        // Ground-fold any carried type-level computation (`keyof`/`T[K]`/conditional)
        // that has become CONCRETE before the structural arms: once a method typar inside
        // grounds (`Events[Key]` with `Key := "ping"`), the node collapses to the member
        // type instead of linking a var to an inert carrier. `FoldedCarrier` matches only
        // when the node reaches a non-carrier type, so a STILL-deferred node falls through
        // to the structural carried-vs-carried arms below and cannot loop.
        | FoldedCarrier ctx folded, _ -> unify ctx key folded b
        | _, FoldedCarrier ctx folded -> unify ctx key a folded
        // An unresolved head unifies with nothing. Stop — the other side is left untouched
        // (no Link), so one broken head can't cascade into a wrong inference elsewhere.
        //
        // A name THIS UNIT'S SOURCE wrote and nothing defined was already blamed where it was
        // written (`UnificationTranslate`, which recorded it): the mistake is a spelling in
        // the source, not a missing dependency, and re-reporting it at every contact would
        // spray secondary errors across a program whose single fault the user has already been
        // told about. What is left to report here is the OTHER producer of `TyUnknown` — a
        // name a package's baked contract could not resolve, which no site in this unit could
        // have blamed.
        | TyUnknown name, _
        | _, TyUnknown name ->
            if not (ctx.UndefinedTypeNames.Contains name) then
                ctx.Error(
                    key,
                    sprintf
                        "Type '%s' could not be resolved during contract extraction — is a package dependency missing?"
                        name
                )
        | TyConst(k1, a1), TyConst(k2, a2) when k1 = k2 && a1.Length = a2.Length -> unifyArgs ctx key a1 a2
        | TyRecord(n1, a1), TyRecord(n2, a2) when n1 = n2 && a1.Length = a2.Length -> unifyArgs ctx key a1 a2
        | TyUnion(n1, a1), TyUnion(n2, a2) when n1 = n2 && a1.Length = a2.Length -> unifyArgs ctx key a1 a2
        // A capability interface reaches `unify` as EITHER of its two faces (e.g. a BCL
        // `Enumerable.Take` returns `IEnumerable\`1`, reconciled against a declared `seq`
        // return): the platform-face key and the canonical-face key differ, so `n1 = n2`
        // fails though they denote the SAME type. `sameNominalKey` reconciles them — a
        // no-op for every non-capability key (the common `n1 = n2` short-circuits first),
        // which is why capabilities need no entry in the resolution-time reverse-canon map.
        // This is the RETURN / plain-`unify` mirror of the argument-coercion reconciliation.
        | TyClass(n1, a1), TyClass(n2, a2) when sameNominalKey ctx n1 n2 && a1.Length = a2.Length ->
            unifyArgs ctx key a1 a2
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
            unify ctx key a1 a2
            unify ctx key r1 r2
        | TyTuple xs, TyTuple ys when xs.Length = ys.Length -> unifyArgs ctx key xs ys
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
        | TyKeyOf t1, TyKeyOf t2 -> unify ctx key t1 t2
        | TyIndexedAccess(o1, i1), TyIndexedAccess(o2, i2) ->
            unify ctx key o1 o2
            unify ctx key i1 i2
        | TyConditional c1, TyConditional c2 ->
            unify ctx key c1.Check c2.Check
            unify ctx key c1.Extends c2.Extends
            unify ctx key c1.WhenTrue c2.WhenTrue
            unify ctx key c1.WhenFalse c2.WhenFalse
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
        | _ -> ctx.Error(key, sprintf "Type mismatch: %A vs %A" (zonk a) (zonk b))

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
    /// itself is defined after this group). `obj` and union-typed parameters are the
    /// two no-pin absorptions here; richer class→interface witness coercion stays in
    /// `unifyArg`/`tryCoerceUpcast` for the eager application path.
    and private unifyArgCoerce (ctx: PassContext) (key: NodeKey) (actual: SemType) (expected: SemType) : unit =
        match resolveStep actual, resolveStep expected with
        | TyTuple aa, TyTuple bb when aa.Length = bb.Length ->
            for i in 0 .. aa.Length - 1 do
                unifyArgCoerce ctx key aa.[i] bb.[i]
        | a, b ->
            // `b` is already `resolveStep`-ed by the match; `absorbsAsObj` (the one
            // obj-policy home) re-steps idempotently.
            if absorbsAsObj b then
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
                    | _ -> if tryStructuralWiden ctx a b then () else unify ctx key a b

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
            | DotSource.ClassChain(key, args) ->
                // `tryClassChainMember` already returns the type instantiated
                // against `args` (and any parent typar substitution), so no
                // further `substituteWith` is needed here.
                let pending = root.PendingDotAccess
                root.PendingDotAccess <- []

                let (DisplayName shown) = SymbolKeyOps.simpleName key

                for d in pending do
                    match tryClassChainMember ctx key args d.MemberName with
                    | ValueSome ty -> unify ctx d.UseKey (TyVar d.ResultTv) ty
                    | ValueNone ->
                        ctx.Error(d.UseKey, sprintf "Type '%s' has no instance member '%s'" shown d.MemberName)
            | DotSource.ExternalClass(key, args) ->
                // Deferred mirror of `resolveFieldStep`'s external arm: the receiver
                // TyVar resolved to a BCL/contract class or interface (e.g. the
                // `comparer: IEqualityComparer` parameter of an `IStructuralEquatable`
                // member, pinned by the interface-conformance unify only *after* the
                // body — and its dot-accesses — were deferred). Resolve each member
                // through the provider and record it for Elaborate.
                let pending = root.PendingDotAccess
                root.PendingDotAccess <- []
                let argArr = args.AsSpan().ToArray()

                for d in pending do
                    match ctx.Provider.TryLookupMember(key, d.MemberName) with
                    | ValueSome m when not m.IsStatic ->
                        let memberSig = ExternalSymbols.openSignature m argArr

                        ctx.Resolution.ExternalAccess.Set(
                            d.UseKey,
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
                        unifyAppliedSig ctx d.UseKey (TyVar d.ResultTv) memberSig
                    | _ ->
                        ctx.Error(
                            d.UseKey,
                            sprintf
                                "Type '%s' has no instance member '%s'"
                                (SymbolKeyOps.qualifiedName key)
                                d.MemberName
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
            (fieldsOf: unit -> seq<SemType>)
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
            // run from the drain callback (no undo trace). Past the `TyVar _` guard
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
                        let subst = mkNamedTypeSubst info.TypeParams args

                        info.Fields |> Seq.map (fun f -> substituteWith subst f.Type)
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
                        let subst = mkNamedTypeSubst info.TypeParams args

                        info.InstanceFields |> Seq.map (fun f -> substituteWith subst f.Type)
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

                // The INVERSE direction for the arrow↔`Fun`2`..`Fun`5` correspondence:
                // a source lambda whose arrow has STILL-FREE
                // domains (`fun x y -> x + y` — no literal pins `x`/`y`) coerced into a
                // GROUND constrained slot (`'TF :> Fun<int,int,int>`) must ground from
                // the slot's args, so the lambda body's SRTP operators resolve instead
                // of leaking `?free-typar`. `subsumes` itself stays read-only
                // (it only *checks* invariant-equality); this is the one place the
                // grounding `unify` lives. Arity-parametric (1..4): peel exactly
                // `targs.Length - 1` arrow domains, `unify` each with the slot's ground
                // arg, then `unify` the residual codomain with the last arg (matched
                // whole — a `n > K` printf tail stays curried, not peeled). A spine too
                // short to peel `k` domains grounds NOTHING. Non-`Fun` coercions and a
                // non-arrow `linkTarget` are untouched.
                match c.Kind with
                | SemanticConstraintKind.Coercion target ->
                    match subtypeNominalOf ctx (zonk target), resolveStep linkTarget with
                    | ValueSome(struct (tname, targs)), TyFun(a, b) when
                        funSlotArityOfArgs (SymbolKeyOps.bareName (SymbolKeyOps.qualifiedName tname)) targs.Length
                        |> Option.isSome
                        ->
                        // `peelFunSpine` (shared with `subsumes`) yields the `k+1` types
                        // aligned to `targs` — a too-short spine grounds NOTHING. The
                        // check-side and this grounding side peel identically by
                        // construction.
                        match peelFunSpine (targs.Length - 1) a b with
                        | Some tys -> tys |> List.iteri (fun i s -> unify ctx key s targs.[i])
                        | None -> ()
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
        // Every child of a compound carries the constraint forward: an anonymous
        // union supports a structural constraint iff every member does
        // (`checkConstraint`'s all-members rule), and a carried type-level
        // computation defers its constraint, so its still-free children take it
        // so the check re-fires on grounding.
        let rec walk t =
            match resolveStep t with
            | TyVar tv ->
                let root = UnionFind.find tv

                if not (root.Constraints |> List.exists (fun e -> e.Kind = c.Kind)) then
                    root.Constraints <- c :: root.Constraints
            | t -> SemType.iterChildren walk t

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
            let t = TyConst(RuntimeNames.stringKey, EqArray.empty)
            ValueSome(TyFun(TyTuple(EqArray.ofList [ t; t ]), t))
        elif not (Set.contains primName numericPrimitives) then
            ValueNone
        elif
            argCount = 2
            && (Set.contains memberName arithmeticBinaryOps
                || Set.contains memberName bitwiseBinaryOps)
        then
            let t = TyConst(RuntimeNames.primitiveKey primName, EqArray.empty)
            ValueSome(TyFun(TyTuple(EqArray.ofList [ t; t ]), t))
        elif argCount = 2 && Set.contains memberName shiftOps then
            // `value: ^T -> shift: int32 -> ^T` — the shift amount is always int32.
            let t = TyConst(RuntimeNames.primitiveKey primName, EqArray.empty)
            ValueSome(TyFun(TyTuple(EqArray.ofList [ t; TyConst(RuntimeNames.intKey, EqArray.empty) ]), t))
        elif argCount = 1 && Set.contains memberName unaryPrimitiveOps then
            let t = TyConst(RuntimeNames.primitiveKey primName, EqArray.empty)
            ValueSome(TyFun(t, t))
        elif argCount = 2 && Set.contains memberName comparisonBinaryOps then
            let t = TyConst(RuntimeNames.primitiveKey primName, EqArray.empty)
            ValueSome(TyFun(TyTuple(EqArray.ofList [ t; t ]), TyConst(RuntimeNames.boolKey, EqArray.empty)))
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
                    | TyConst(primKey, _) ->
                        let primName = SymbolKeyOps.intrinsicName primKey

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
                                let (DisplayName shown) = SymbolKeyOps.simpleName classKey

                                ctx.Error(key, sprintf "Type '%s' has no static member '%s'" shown b.MemberName)

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
                            match ctx.Provider.TryLookupMember(classKey, b.MemberName) with
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
    let unifyAnnotation (ctx: PassContext) (key: NodeKey) (actual: SemType) (expected: SemType) : unit =
        // A literal / pure-literal-union actual — the only actual admitted to the
        // outward-widening arm below (keeps that arm strictly additive: a non-literal
        // union still grounds via symmetric `unify`, unchanged).
        let rec isLiteralBearing t =
            match resolveStep t with
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
        match resolveStep expected with
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
        | _ -> unify ctx key actual expected
