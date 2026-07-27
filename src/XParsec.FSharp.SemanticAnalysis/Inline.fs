namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Parser

// `let inline` expansion helper. The pre-freeze `Passes.InlineExpansion` pass
// invokes it once per call site, between `Elaborate.elaborate` and the
// `freezeTypars` cut, where `zonk` / union-find are still native. It stays in
// this module (rather than the pass) because `openMethodSignature` below shares it
// and `Codegen` no longer references the inline machinery at all.
//
// The cross-unit inline-body channel rides the resolved entry itself
// (`ExternalSymbol.InlineBody` / `ExternalMember.InlineBody`, reached by key), and
// what it carries is FROZEN — `Frozen.TDecl`, cell-free. `thawBody` below is the one
// place that turns it back into a `SemType` tree, minting the consumer's OWN cells.
//
// So by the time `inlineExpand` runs, an inline binding's typars are free `TyVar`
// roots either way: a SAME-unit template still holds the roots its generalised scheme
// quantified (the pre-freeze pass sees it directly), and a CROSS-unit one holds the
// roots `thawBody` just minted. `inlineExpand` substitutes those roots to the caller's
// concrete types; `freshen` does the NodeKey renaming so independent call sites don't
// alias each other's bound names (and thus codegen local slots). The caller still owns
// argument (beta) reduction of the resulting lambda against the actual arguments — it
// needs the call-site args the caller holds.

module Inline =

    /// An SRTP trait call `inlineExpand` could NOT resolve: the substituted receiver is
    /// not a nominal, so no type can carry the named static member. `Receiver` is the
    /// SUBSTITUTED receiver type and `MemberName` its compiled member name
    /// (`op_Addition`).
    ///
    /// Reported as data, not as a message: the expander runs off the type-erased
    /// `TastWalk.Mapper` surface with no `PassContext`, and the spliced body's own tokens
    /// address the LIBRARY file it came from — so the caller (`Passes.InlineExpansion`)
    /// owns both the wording and the call-site key the diagnostic must be anchored at.
    /// Every expansion path returns these, so none can splice a body while quietly
    /// leaving an unresolvable trait call in it — neither backend has a `TraitCall` arm.
    type UnresolvedTrait =
        {
            Receiver: SemType
            MemberName: string
        }

    /// A typar leaf of a FROZEN template, across all three axes — the key of the
    /// thaw's freshener cache. One cache, one key type: a `Declaring 0` and a
    /// `Method 0` are different typars and must not collide, and an `FTLocalTypar`
    /// is identified by the `(binder, index)` PAIR, never the index alone.
    [<RequireQualifiedAccess>]
    type private TyparLeaf =
        | Declaring of declIndex: int
        | Method of methodIndex: int
        | Local of binder: NodeKey * localIndex: int

    /// THE immutable→mutable transition: realise a frozen inline body in the CONSUMER's
    /// `SemType` domain, minting one fresh `TyVar` cell per distinct typar leaf.
    ///
    /// This is the seam the whole frozen inline-body channel rests on. The provider hands
    /// out `FrozenType` — cell-free, so nothing a consumer does can reach back into a
    /// producer's inference state. The cells the splice then unifies against are minted
    /// HERE, by the consumer, out of leaves that name nothing but positions in the
    /// template. So `substType` / `freshen` / SRTP resolution run unchanged: they key on
    /// `TyVar` roots, and after this the roots exist and are this unit's.
    ///
    /// ONE cache for the WHOLE decl, shared across all three axes — not one per node. Two
    /// occurrences of one typar must land on ONE cell, or the body's internal type links
    /// (a parameter's type and the use of that parameter) come apart.
    ///
    /// It consults no ambient unit state: a leaf is interpreted against the body carrying
    /// it and nothing else. That is what makes an `FTLocalTypar`'s body-relative `NodeKey`
    /// binder safe across units, whose `NodeKey`s collide freely (there is no file id in
    /// one, by design).
    let thawBody (store: TypeStore) (decl: Frozen.TDecl) : TDecl =
        let cache = Dictionary<TyparLeaf, SemType>()

        let mint (leaf: TyparLeaf) : SemType =
            match cache.TryGetValue leaf with
            | true, v -> v
            | _ ->
                let v = TyVar(store.NewTypeVar())
                cache.[leaf] <- v
                v

        TastConvert.decl
            (FrozenTypeBridge.instantiateWith
                (fun i -> mint (TyparLeaf.Declaring i))
                (fun j -> mint (TyparLeaf.Method j))
                (fun binder k -> mint (TyparLeaf.Local(binder, k))))
            decl

    /// A module-level `let` value whose body is EXACTLY one intrinsic expression with
    /// NO operands (`let undefined : undefined = (# "undefined" : undefined #)`).
    /// Returns the intrinsic body to splice, else `ValueNone`.
    ///
    /// Such a binding is a compile-time ALIAS for the intrinsic's emitted form: with no
    /// operands there is nothing to substitute, and it carries no typars, so the body IS
    /// the splice. The JS backend treats it as inline — it emits NO lowered definition
    /// (a `const undefined = undefined` would be both nonsensical and self-referential),
    /// and every reference splices the intrinsic body (`(# "undefined" #)` → bare
    /// `undefined`). The shape is deliberately narrow (one intrinsic, zero operands) so
    /// the alias can never lose or duplicate an operand. `InlineExpansion` splices it at
    /// each `External` reference; `Freeze` publishes it in the unit's inline vocabulary
    /// (it is a splice template, `inline` keyword or not) so a consumer's provider serves
    /// the body. Publication is additive, here as for a `let inline`: the binding stays in
    /// `Decls`, and it is the JS backend's own reference splicing — not the freeze — that
    /// leaves it with no lowered definition.
    let nullaryIntrinsicValueBody (decl: TDecl) : TExpr voption =
        match decl with
        | TDecl.Let(_, (TExpr.ILIntrinsic(_, _, args, _, _) as body), _, _) when args.Length = 0 -> ValueSome body
        | _ -> ValueNone

    /// Quantified typars of an inline binding, in the canonical order codegen
    /// must use when supplying type arguments to `inlineExpand`: first
    /// occurrence in a pre-order walk of the binding's generalised type. This
    /// reproduces the order `Unification.generalise` collects them in — it
    /// walks the same zonked type — so an order recovered from the frozen
    /// TAST lines up with the scheme that produced it. A measure-bearing root
    /// (Link set to its carrier) is *not* a typar; like `generalise` we skip
    /// it by following the Link rather than collecting the root.
    ///
    /// Keying by `TyVar` root is correct for a THAWED body too, and stays correct:
    /// `thawBody` re-mints a fresh `TyVar` cell per frozen typar leaf BEFORE the
    /// splice, so by the time this runs the template's typars are roots again — this
    /// unit's roots. It never sees a `TyTypar`.
    let quantifiedTypars (store: TypeStore) (declTy: SemType) : TyVarId[] =
        let acc = ResizeArray<TyVarId>()
        let seen = HashSet<TyVarId>()
        SemTypeWalk.collectLinkedRoots store acc seen declTy
        acc.ToArray()

    /// Substitute typar roots present in `subst`. A template's free typar is a
    /// `TyVar` root with no Link (the producer's, pre-freeze; a freshly minted one
    /// of this unit's, post-`thawBody`); chase to the union-find root and swap.
    /// Roots absent from `subst` stay abstract.
    let rec private substType (store: TypeStore) (subst: Dictionary<TyVarId, SemType>) (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = UnionFind.find store tv

            match subst.TryGetValue root.Id with
            | true, repl -> repl
            | _ -> TyVar root.Id
        // Pure child recursion (`mapChildren` routes `TyOr` through the smart
        // constructor: substituting a typar member can collapse / reorder the set).
        | t -> SemType.mapChildren (substType store subst) t

    /// Structural match of two (already typar-substituted) `SemType`s for a
    /// static-optimization `when ^T : Type` clause. `TyVar`s compare by union-find
    /// root identity — so a REFLEXIVE `when ^T : ^T` clause (both sides the same
    /// typar) matches unconditionally, which is what makes it a user catch-all
    /// whether or not the operand was ever pinned.
    ///
    /// `TyConst`s compare by exact `SymbolKey` identity — the same `=` the nominal arms
    /// below (`TyRecord`/`TyUnion`/`TyClass`) use — with no alias canonicalisation, because
    /// there are no aliases left to canonicalise: an intrinsic ABBREVIATION (`type single =
    /// float32`, `type int32 = int` — every prim-types alias whose right-hand side is not
    /// itself a `(# … #)` binding) is registered in `AbbreviationTypes` and expanded eagerly
    /// by `Translate.resolveBareTypeName`. Both the operand's type and the clause's required
    /// type pass through it, so both sides arrive here already canonical, and a name compare
    /// would only be a lossy `=` that drops the identity's declaring namespace.
    let rec private staticOptTypesMatch (store: TypeStore) (a: SemType) (b: SemType) : bool =
        match a, b with
        | TyVar x, TyVar y -> UnionFind.find store x = UnionFind.find store y
        | TyConst(k1, xs), TyConst(k2, ys) -> k1 = k2 && EqArray.forall2 (staticOptTypesMatch store) xs ys
        | TyFun(a1, r1), TyFun(a2, r2) -> staticOptTypesMatch store a1 a2 && staticOptTypesMatch store r1 r2
        | TyTuple xs, TyTuple ys -> EqArray.forall2 (staticOptTypesMatch store) xs ys
        | TyRecord(n1, xs), TyRecord(n2, ys)
        | TyUnion(n1, xs), TyUnion(n2, ys)
        | TyClass(n1, xs), TyClass(n2, ys) -> n1 = n2 && EqArray.forall2 (staticOptTypesMatch store) xs ys
        | _ -> false

    /// Approximate `when ^T : struct` for the value-type primitives the operator
    /// surface can reach; anything else is treated as non-struct. Full struct
    /// detection on user types awaits the attribute walker. Names arrive dealiased
    /// (`int32`→`int`, `single`→`float32`, `double`→`float`), so only the canonical
    /// spellings are listed.
    let private isStructType (t: SemType) : bool =
        match t with
        | TyConst(key, _) ->
            match SymbolKeyOps.intrinsicName key with
            | "int"
            | "int64"
            | "byte"
            | "float"
            | "float32"
            | "bool"
            | "char"
            | "decimal" -> true
            | _ -> false
        | _ -> false

    /// The declaring `TypeKey` of a nominal (class / union / record) — the operand
    /// shape that can carry a static operator member, and so the ONLY shape an SRTP
    /// trait call can dispatch to. The single definition of "is a nominal operand";
    /// `resolveTraitCall` alone consults it, and a receiver it declines becomes an
    /// `UnresolvedTrait` — "this type does not support this operator".
    let private nominalHeadKey (store: TypeStore) (t: SemType) : TypeKey voption =
        match UnionFind.headZonk store t with
        | TyClass(k, _)
        | TyUnion(k, _)
        | TyRecord(k, _) -> ValueSome k
        | _ -> ValueNone

    /// Build the typar-substituting mapper for one inline expansion. The
    /// `StaticOptimization` override is the only customisation: at call-site
    /// expansion the typars have been pinned, so pick the first clause whose
    /// constraints hold and keep only its (substituted) body. Everything else falls through to the default
    /// rewrite, which threads `substType subst` through every embedded `ty`.
    ///
    /// `declined` is the sink for trait calls `resolveTraitCall` cannot resolve. The
    /// mapper is the one place that decides a trait call is unresolvable, so it is the
    /// one place that can say so — a second walk of the expanded body to re-discover
    /// them would be rediscovering what this already knew.
    let rec private substMapper
        (ctx: PassContext)
        (declined: ResizeArray<UnresolvedTrait>)
        (subst: Dictionary<TyVarId, SemType>)
        : TastWalk.Mapper =
        let sub = substType ctx.Store subst

        let holds (c: TStaticOptConstraint) =
            match c with
            | TStaticOptConstraint.TyconEquals(typar, required) ->
                staticOptTypesMatch ctx.Store (sub typar) (sub required)
            | TStaticOptConstraint.IsStruct typar -> isStructType (sub typar)

        // The clause conditions ALONE decide. No clause body is a trait call: the
        // arithmetic bodies carry the SRTP dispatch in the BASE (an ungated position),
        // with an explicit clause per supported primitive — so a `when ^T : Type` clause
        // is selected iff its type matches, and an operand that matches none falls to the
        // base, where `resolveTraitCall` decides whether the type supports the operator.
        let clauseSelected (cl: TStaticOptClause) = cl.Constraints |> EqArray.forall holds

        let resolveStaticOpt (clauses: EqArray<TStaticOptClause>) (defaultExpr: TExpr) : TExpr =
            let m = substMapper ctx declined subst

            match clauses |> EqArray.tryFind clauseSelected with
            | ValueSome cl -> TastWalk.mapExpr m cl.Body
            | ValueNone -> TastWalk.mapExpr m defaultExpr

        // Resolve a `TraitCall` once the trait typars have been substituted and the
        // receiver is a concrete nominal: rewrite it to a `StaticMethodCall` on that
        // type's static operator member (class, union, OR record). Two miss cases record
        // an `UnresolvedTrait` (reported by the caller at the call site) rather than mint a
        // call: the receiver does not pin to a nominal at all (an unpinned `^T`, or a
        // `TyConst` with no clause of its own), OR it pins to nominal `k` but `k` carries
        // no such member — the honest "type does not support this operator" verdict, which
        // the total-key mint surfaces (the former placeholder minted a key for the absent
        // member and failed opaquely downstream). The result type is `sub ty` (`^T3`), NOT
        // the receiver's — a heterogeneous operator (`Vec2 * float -> Vec2`) returns
        // neither operand's type.
        let resolveTraitCall
            (m: TastWalk.Mapper)
            (recvTy: SemType)
            (memberName: string)
            (args: EqArray<TExpr>)
            (ty: SemType)
            (tok: SyntaxToken)
            : TExpr voption =
            let decline () =
                declined.Add
                    {
                        Receiver = sub recvTy
                        MemberName = memberName
                    }

                ValueNone

            match nominalHeadKey ctx.Store (sub recvTy) with
            | ValueSome k ->
                // The total `MemberKey` freezes the resolved operator's real parameter
                // signature; its `ArgSig.Length` still carries the operand arity codegen's
                // external member-ref param-flatten reads (`op_Addition(Set, Set)` ⇒ two
                // parameters, not one `ValueTuple`). The dispatch may target an *external*
                // declaring type (an `.fsi`-imported `Vesper.Set`), handled by the minter's
                // provider arm. The rewritten node replaces the `TraitCall`, so it keeps `tok`.
                //
                // The operands are POST-substitution (`sub recvTy` already pinned `k`): the
                // receiver's declaring-type args and the substituted operand element types
                // discriminate a same-arity external operator overload (`op_Addition(Vec2,
                // Vec2)` vs `op_Addition(Vec2, float)`). A still-unpinned operand declines to
                // the best-by-arity single inside the minter.
                let operands =
                    LocalMemberKeys.externalOperands
                        ctx.Store
                        (LocalMemberKeys.nominalArgs ctx.Store (sub recvTy))
                        [ for a in args -> sub (TastWalk.exprTy a) ]

                match LocalMemberKeys.totalMemberKey ctx k memberName operands with
                | ValueSome memberKey ->
                    ValueSome(TExpr.StaticMethodCall(memberKey, EqArray.map (TastWalk.mapExpr m) args, sub ty, tok))
                | ValueNone -> decline ()
            | ValueNone -> decline ()

        { TastWalk.identityMapper with
            MapType = sub
            OverrideExpr =
                fun m e ->
                    match e with
                    | TExpr.StaticOptimization(clauses, def, _, _) -> ValueSome(resolveStaticOpt clauses def)
                    | TExpr.TraitCall(recvTy, memberName, args, ty, tok) ->
                        resolveTraitCall m recvTy memberName args ty tok
                    | _ -> ValueNone
        }

    /// Expand an `inline` binding's retained body for one call site. `typeArgs` are the
    /// caller's concrete types for the binding's quantified typars, in
    /// `quantifiedTypars` order. Returns the binding's `value` with every typar
    /// substituted, its `StaticOptimization` clauses resolved and its `TraitCall`s
    /// dispatched — paired with the trait calls that could NOT be dispatched, which the
    /// caller must report. The body shares NodeKeys with the original (the caller
    /// freshens them per expansion, and reduces the resulting lambda against the actual
    /// arguments). Supplying fewer `typeArgs` than there are typars substitutes the
    /// leading ones and leaves the rest abstract.
    ///
    /// The substituting walk runs even when there is nothing to substitute (a
    /// monomorphic binding, or a bare reference with no spine to derive typars from):
    /// it is what resolves `StaticOptimization` and `TraitCall` nodes, and NEITHER
    /// backend can emit those. Short-circuiting an empty substitution — the shape this
    /// once had — let both node kinds ride an un-substituted body straight through to
    /// codegen's `failwithf` catch-all.
    let inlineExpand (ctx: PassContext) (decl: TDecl) (typeArgs: SemType[]) : TExpr * UnresolvedTrait list =
        match decl with
        | TDecl.Let(_, value, _, declTy) ->
            let typars = quantifiedTypars ctx.Store declTy
            let subst = Dictionary<TyVarId, SemType>()

            typars
            |> Array.iteri (fun i tv ->
                if i < typeArgs.Length then
                    subst.[tv] <- typeArgs.[i]
            )

            let declined = ResizeArray<UnresolvedTrait>()
            let expanded = TastWalk.mapExpr (substMapper ctx declined subst) value
            expanded, List.ofSeq declined
        | TDecl.Expression _ -> invalidArg "decl" "Inline.inlineExpand expects a TDecl.Let, got a TDecl.Expression"
        | TDecl.Type _ -> invalidArg "decl" "Inline.inlineExpand expects a TDecl.Let, got a TDecl.Type"

    /// Rename every binder NodeKey in `body` (and the references to it) to a
    /// fresh key from `mint`, returning a structurally-new TExpr. Two
    /// expansions of one inline body would otherwise share a binder key — and
    /// so a downstream codegen local slot — making nested call sites
    /// (`succ (succ x)`) clobber each other. A single pre-order rewrite:
    /// every binder (`TPat.NamedSimple` keys, `TExpr.ForTo` vars) mints a
    /// fresh key recorded `old → new`; every `TExpr.Var` is rewired through
    /// that map. **Free** vars — keys not bound within `body` (externals,
    /// captured outer locals) — are not in the map and pass through untouched.
    /// Because a use is always lexically inside its binder, pre-order visits
    /// the binder (populating the map) before any reference to it. The caller
    /// owns `mint` so its counter is shared across every expansion in a build.
    let freshen (mint: unit -> NodeKey) (body: TExpr) : TExpr =
        let remap = Dictionary<NodeKey, NodeKey>()

        let bind (k: NodeKey) : NodeKey =
            let k' = mint ()
            remap.[k] <- k'
            k'

        let useKey (k: NodeKey) : NodeKey =
            match remap.TryGetValue k with
            | true, k' -> k'
            | _ -> k

        // Two overrides: every `TPat.NamedSimple` binds (covers Lambda/Let/
        // Match-arm/ForIn binders and nested binders inside Tuple/Record/Union
        // sub-pats via default recursion); every `TExpr.Var` use rewrites
        // through the remap. `ForTo`'s binder is a bare `NodeKey` (not a
        // `TPat`), so it gets a manual override. The default `Let` / `Lambda`
        // / `Match` arms in TastWalk.mapExpr evaluate `mapPat m p` before
        // `mapExpr m body` — so binders are in the remap before any reference
        // to them is rewritten.
        let mapper: TastWalk.Mapper =
            { TastWalk.identityMapper with
                OverridePat =
                    fun _ p ->
                        match p with
                        | TPat.NamedSimple(k, t, tok) -> ValueSome(TPat.NamedSimple(bind k, t, tok))
                        | _ -> ValueNone
                OverrideExpr =
                    fun m e ->
                        match e with
                        | TExpr.Var(k, t, tok) -> ValueSome(TExpr.Var(useKey k, t, tok))
                        | TExpr.ForTo(var, identTok, s, e2, b, t, tok) ->
                            let var = bind var

                            ValueSome(
                                TExpr.ForTo(
                                    var,
                                    identTok,
                                    TastWalk.mapExpr m s,
                                    TastWalk.mapExpr m e2,
                                    TastWalk.mapExpr m b,
                                    t,
                                    tok
                                )
                            )
                        | _ -> ValueNone
            }

        TastWalk.mapExpr mapper body

    /// The open method signature of an external symbol: its full curried
    /// monotype with the method-owned typars resolved to self-describing
    /// `TyTypar(Method, i)` nodes (`MethodTyparArity` of them). This is the
    /// strictly-smaller precursor of the planned `instantiate :
    /// ExternalSignature -> level -> SemType` seam: it lets
    /// `ClrRecipes.emitExternalCall` reconstruct an
    /// external call's signature without ever authoring a `TyVar`. The fresh
    /// `TyVar`s `Instantiate` mints are transient and never escape this
    /// function — the returned `Signature` is `TyVar`-free.
    type OpenMethodSignature =
        {
            /// Curried `param -> … -> return` frozen template with method typars as
            /// `FTTypar(Method, i)`: the codegen-facing open signature is immutable
            /// `FrozenType` data, not a `SemType`.
            Signature: FrozenType
            /// Count of distinct method typars — the `MethodSpec` generic-parameter
            /// count. INCLUDES phantom typars present only in `Coercion` bounds (the
            /// enumerator `'E` in `'S :> IStructSeq<'T,'E>`), recovered by the
            /// dependent-typar pass so the count matches the producer's emitted IL.
            MethodTyparArity: int
            /// The symbol's `when 'a :> <ty>` bounds, frozen over the method-typar
            /// axis (`FTTypar(Method, i)` leaves) in the SAME `FrozenConstraint` shape
            /// the project-local `EmitCall` phantom-typar solve consumes — so the
            /// external solve is head-agnostic. Empty for a symbol with
            /// no subtype bounds.
            Constraints: FrozenConstraint list
        }

    /// Project a free function's contract `Scheme` onto the method axis: its own
    /// typars are baked `FTTypar(Declaring, i)`, with `i` the contract's CANONICAL
    /// order — explicit `<'T>` first in declaration order, then inferred typars by
    /// first appearance (`VesperLib`'s `registerExplicitTypars` then the finalize
    /// walk). That is EXACTLY the order the producer's static-method emit assigns
    /// its `!!i` slots (`Elaborate.mkMethodQuantEnv` ▸ `GeneralizedTypars.canonical`).
    /// So map each `Declaring i ↦ Method i` POSITIONALLY, preserving that order —
    /// do NOT re-derive it by first-appearance over the monotype. The old appearance
    /// walk silently dropped an explicit `<'b,'a>`'s declared order, so a call to
    /// `Set.fold<'T,'State>` (whose declared order differs from appearance) emitted
    /// a `MethodSpec` permuted from the callee's emitted `GenericParam` order — a
    /// `MissingMethodException` at JIT. `MethodTyparArity` is the scheme's own typar
    /// count. A free function's scheme carries no `Method`-axis typars, but the
    /// freshener maps that branch identically for totality.
    let openMethodSignature (sym: ExternalSymbol) : OpenMethodSignature =
        let openSig =
            FrozenTypeBridge.instantiateWith
                (fun i -> TyTypar(TyparAxis.Method, i))
                (fun j -> TyTypar(TyparAxis.Method, j))
                (FrozenTypeBridge.localTyparInTemplate "Inline.openMethodSignature")
                sym.Scheme

        // The scheme's `Coercion` bounds, re-expressed over the method-typar axis in the
        // SAME `FrozenConstraint` shape `EmitCall`'s project-local solve consumes — so the
        // external phantom-typar solve is head-agnostic. `typarIndex` is the CONSTRAINED
        // typar's method index (the `'S` receiver `EmitCall` reads); `target` (e.g.
        // `IStructSeq<'T,'E>`) carries the phantom typars to recover. A phantom (the
        // enumerator `'E`) is a declaring typar of the scheme that appears only inside a
        // `Coercion` target, never in a parameter/result — so it carries no `Signature`
        // position, but IS counted in `TyparArity` (hence `MethodTyparArity`) and gets its
        // method slot. Mapped POSITIONALLY (`Declaring i ↦ Method i`), matching
        // `Signature`'s declared-order projection — NOT re-derived by first-appearance.
        let constraints =
            [
                for c in sym.Constraints do
                    match c with
                    | ExternalConstraint.Coercion(i, target) ->
                        let openTarget =
                            FrozenTypeBridge.instantiateWith
                                (fun k -> TyTypar(TyparAxis.Method, k))
                                (fun k -> TyTypar(TyparAxis.Method, k))
                                (FrozenTypeBridge.localTyparInTemplate "Inline.openMethodSignature")
                                target

                        FrozenConstraint.Coercion(i, toFrozen openTarget)
                    | _ -> ()
            ]

        {
            Signature = toFrozen openSig
            MethodTyparArity = sym.TyparArity
            Constraints = constraints
        }
