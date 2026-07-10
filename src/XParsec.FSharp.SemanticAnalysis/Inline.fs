namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Parser

// `let inline` expansion helper. The pre-freeze `Passes.InlineExpansion` pass
// invokes it once per call site, between `Freeze.elaborate` and the
// `freezeTypars` cut, where `zonk` / union-find are still native. It lived
// codegen-side until beat (b) relocated the expander; it stays in
// this module (rather than the pass) because `openMethodSignature` below shares it
// and `Codegen` no longer references the inline machinery at all.
//
// The cross-package inline-body channel the pass uses now rides
// `IExternalSymbolProvider` directly (`TryLookupInlineBody`, by resolved
// `SymbolKey`), since `ExternalSymbols` compiles after `Tast` and can name `TDecl`
// — the sibling `IInlineBodyProvider` + the `box`/`:?` cast it required are gone.
//
// The retained body of an `inline` binding carries its typars as free
// `TyVar` roots: the binding's generalised scheme quantified them, and the
// ResolvedTypes validator guarantees no *other* free TyVar survives into the
// frozen TAST. `inlineExpand` substitutes those typars to the caller's
// concrete types; `freshen` does the NodeKey renaming so independent call
// sites don't alias each other's bound names (and thus codegen local slots).
// The caller still owns argument (beta) reduction of the resulting lambda
// against the actual arguments — it needs the call-site args the caller holds.

module Inline =

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
    /// each `External` reference; `SymbolProviders.collectInlineBodies` registers it as a
    /// cross-package `InlineBody` so a consumer's provider serves the body.
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
    /// TODO(frozen-type Phase 2): once `freeze` emits `TyTypar` for an inline
    /// binding's quantified typars, this collector must yield them by `index`
    /// instead of by `TyVar` root (the shared `SemTypeWalk` skeleton treats
    /// `TyTypar` as a no-op today). No-op until then.
    let quantifiedTypars (declTy: SemType) : TypeVar[] =
        let acc = ResizeArray<TypeVar>()
        let seen = HashSet<TypeVar>(HashIdentity.Reference)
        SemTypeWalk.collectLinkedRoots acc seen declTy
        acc.ToArray()

    /// Substitute typar roots present in `subst`. The frozen TAST is zonked,
    /// so a free typar is `TyVar root` with no Link; chase to the union-find
    /// root and swap. Roots absent from `subst` stay abstract.
    /// (TODO(frozen-type): substitute by `(axis,index)` once inline bindings carry
    /// `TyTypar` — it passes through `mapChildren`'s leaf arm until then.)
    let rec private substType (subst: Dictionary<TypeVar, SemType>) (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = UnionFind.find tv

            match subst.TryGetValue root with
            | true, repl -> repl
            | _ -> TyVar root
        // Pure child recursion (`mapChildren` routes `TyOr` through the smart
        // constructor: substituting a typar member can collapse / reorder the set).
        | t -> SemType.mapChildren (substType subst) t

    /// Canonicalise the primitive type-name aliases a static-optimization clause
    /// might use (`int32`/`int`, `double`/`float64`/`float`, `uint8`/`byte`) so a
    /// clause written against the BCL name matches an operand carrying the F#
    /// alias. Deliberately a pure (no-`ctx`) copy: static-opt resolution runs
    /// inside the inline expansion walker, which is called from the type-erased
    /// `TastWalk.Mapper` surface where `PassContext` is no longer in scope.
    /// `ctx.Types.IntrinsicReprTypes` (name → IL repr) carries the same alias
    /// equivalence at extract-time; the table here mirrors that data for the
    /// post-extract walker. Keep the two in sync when new primitives land.
    let private canonPrimName (name: string) : string =
        match name with
        | "int32" -> "int"
        | "double"
        | "float64" -> "float"
        | "uint8" -> "byte"
        | other -> other

    /// Structural match of two (already typar-substituted) `SemType`s for a
    /// static-optimization `when ^T : Type` clause. `TyVar`s compare by union-find
    /// root identity — so the catch-all `when ^T : ^T`, whose two sides are the
    /// same typar, matches once both substitute to one concrete type (or, if the
    /// operand type was never pinned, still matches as the generic fall clause).
    /// `TyConst`s compare by canonical primitive name.
    let rec private staticOptTypesMatch (a: SemType) (b: SemType) : bool =
        match a, b with
        | TyVar x, TyVar y -> System.Object.ReferenceEquals(UnionFind.find x, UnionFind.find y)
        | TyConst(k1, xs), TyConst(k2, ys) ->
            canonPrimName (SymbolKeyOps.intrinsicName k1) = canonPrimName (SymbolKeyOps.intrinsicName k2)
            && EqArray.forall2 staticOptTypesMatch xs ys
        | TyFun(a1, r1), TyFun(a2, r2) -> staticOptTypesMatch a1 a2 && staticOptTypesMatch r1 r2
        | TyTuple xs, TyTuple ys -> EqArray.forall2 staticOptTypesMatch xs ys
        | TyRecord(n1, xs), TyRecord(n2, ys)
        | TyUnion(n1, xs), TyUnion(n2, ys)
        | TyClass(n1, xs), TyClass(n2, ys) -> n1 = n2 && EqArray.forall2 staticOptTypesMatch xs ys
        | _ -> false

    /// Approximate `when ^T : struct` for the value-type primitives the operator
    /// surface can reach; anything else is treated as non-struct. Full struct
    /// detection on user types awaits the attribute walker.
    let private isStructType (t: SemType) : bool =
        match t with
        | TyConst(key, _) ->
            match SymbolKeyOps.intrinsicName key with
            | "int"
            | "int32"
            | "int64"
            | "byte"
            | "uint8"
            | "float"
            | "double"
            | "float64"
            | "bool"
            | "char"
            | "decimal" -> true
            | _ -> false
        | _ -> false

    /// The declaring `SymbolKey` of a project-local nominal (class / union /
    /// record) — the operand shape for which F#'s reflexive `when ^T : ^T`
    /// static-optimization condition holds (the type carries its own static
    /// operator member). The single definition of "is a nominal operand", so the
    /// clause-selection gate (`clauseSelected`) and the `TraitCall` resolution
    /// (`resolveTraitCall`) can never disagree on what counts — a record selects
    /// the clause *and* resolves, rather than selecting then failing.
    let private nominalHeadKey (t: SemType) : SymbolKey voption =
        match UnionFind.headZonk t with
        | TyClass(k, _)
        | TyUnion(k, _)
        | TyRecord(k, _) -> ValueSome k
        | _ -> ValueNone

    /// `true` when `t`'s head is a project-local nominal (class / union / record).
    /// Public so `InlineExpansion.isSpliceableOperatorArg` shares the one nominal
    /// predicate rather than re-matching the three cases against its own zonk.
    let isNominalType (t: SemType) : bool = (nominalHeadKey t).IsSome

    /// Build the typar-substituting mapper for one inline expansion. The
    /// `StaticOptimization` override is the only customisation: at call-site
    /// expansion the typars have been pinned, so pick the first clause whose
    /// constraints hold and keep only its (substituted) body. Everything else falls through to the default
    /// rewrite, which threads `substType subst` through every embedded `ty`.
    let rec private substMapper (subst: Dictionary<TypeVar, SemType>) : TastWalk.Mapper =
        let sub = substType subst

        let holds (c: TStaticOptConstraint) =
            match c with
            | TStaticOptConstraint.TyconEquals(typar, required) -> staticOptTypesMatch (sub typar) (sub required)
            | TStaticOptConstraint.IsStruct typar -> isStructType (sub typar)

        // A clause whose body is an SRTP member-trait call (the operators' `when ^T : ^T`
        // dispatch clause) additionally requires the substituted operand to be a nominal
        // carrying that member — F#'s "^T is a nominal type" condition. A primitive
        // operand therefore skips it and falls through to the operator's primitive
        // clauses / inline-IL base, while a plain-bodied `^T : ^T` clause (a user catch-all)
        // stays unconditional.
        let clauseSelected (cl: TStaticOptClause) =
            (cl.Constraints |> EqArray.forall holds)
            && (
                match cl.Body with
                | TExpr.TraitCall(recvTy, _, _, _, _) -> isNominalType (sub recvTy)
                | _ -> true
            )

        let resolveStaticOpt (clauses: EqArray<TStaticOptClause>) (defaultExpr: TExpr) : TExpr =
            let m = substMapper subst

            match clauses |> EqArray.tryFind clauseSelected with
            | ValueSome cl -> TastWalk.mapExpr m cl.Body
            | ValueNone -> TastWalk.mapExpr m defaultExpr

        // Resolve a `TraitCall` once the trait typar has been substituted to a concrete
        // nominal: rewrite it to a `StaticMethodCall` on that type's static operator
        // member. This fires for the `when ^T : ^T` clause body selected by `clauseSelected`
        // above (so the receiver is always a nominal here, via the SAME `nominalHeadKey`
        // — class, union, OR record); a non-nominal receiver is left as a substituted
        // `TraitCall` for a later phase to surface loudly.
        let resolveTraitCall
            (m: TastWalk.Mapper)
            (recvTy: SemType)
            (memberName: string)
            (args: EqArray<TExpr>)
            (ty: SemType)
            (tok: SyntaxToken)
            : TExpr voption =
            match nominalHeadKey (sub recvTy) with
            | ValueSome k ->
                // Carry the operand arity so codegen's external member-ref param-flatten
                // mints a `.NET`-tupled static operator's parameters correctly: a binary
                // `op_Addition(Set, Set)` must be two parameters, not one `ValueTuple`.
                // This dispatch may target an *external* declaring type (an `.fsi`-imported
                // `Vesper.Set`), which is exactly the case `ofMember`'s `arity` covers.
                // The rewritten node replaces the `TraitCall`, so it keeps its `tok`.
                let memberKey = LocalSymbolKey.ofMember k memberName args.Length MemberKind.Method
                ValueSome(TExpr.StaticMethodCall(memberKey, EqArray.map (TastWalk.mapExpr m) args, sub ty, tok))
            | ValueNone -> ValueNone

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

    let private substExpr (subst: Dictionary<TypeVar, SemType>) (e: TExpr) : TExpr =
        TastWalk.mapExpr (substMapper subst) e

    /// Expand an `inline` binding's retained body for one call site.
    /// `typeArgs` are the caller's concrete types for the binding's
    /// quantified typars, in `quantifiedTypars` order. The returned TExpr is
    /// the binding's `value` with every typar substituted; it shares NodeKeys
    /// with the original (codegen freshens them per expansion, and reduces the
    /// resulting lambda against the actual arguments). A monomorphic binding
    /// (no typars) round-trips its body unchanged. Supplying fewer `typeArgs`
    /// than there are typars substitutes the leading ones and leaves the rest
    /// abstract.
    let inlineExpand (decl: TDecl) (typeArgs: SemType[]) : TExpr =
        match decl with
        | TDecl.Let(_, value, _, declTy) ->
            let typars = quantifiedTypars declTy
            let subst = Dictionary<TypeVar, SemType>(HashIdentity.Reference)

            typars
            |> Array.iteri (fun i tv ->
                if i < typeArgs.Length then
                    subst.[tv] <- typeArgs.[i]
            )

            if subst.Count = 0 then value else substExpr subst value
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
                        | TExpr.ForTo(var, s, e2, b, t, tok) ->
                            let var = bind var

                            ValueSome(
                                TExpr.ForTo(
                                    var,
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
    /// `TyTypar(Method, i)` nodes (`MethodArity` of them). This is the
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
            MethodArity: int
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
    /// `MissingMethodException` at JIT. `MethodArity` is the scheme's own typar
    /// count. A free function's scheme carries no `Method`-axis typars, but the
    /// freshener maps that branch identically for totality.
    let openMethodSignature (sym: ExternalSymbol) : OpenMethodSignature =
        let openSig =
            FrozenTypeBridge.instantiateWith
                (fun i -> TyTypar(TyparAxis.Method, i))
                (fun j -> TyTypar(TyparAxis.Method, j))
                sym.Scheme

        // The scheme's `Coercion` bounds, re-expressed over the method-typar axis in the
        // SAME `FrozenConstraint` shape `EmitCall`'s project-local solve consumes — so the
        // external phantom-typar solve is head-agnostic. `typarIndex` is the CONSTRAINED
        // typar's method index (the `'S` receiver `EmitCall` reads); `target` (e.g.
        // `IStructSeq<'T,'E>`) carries the phantom typars to recover. A phantom (the
        // enumerator `'E`) is a declaring typar of the scheme that appears only inside a
        // `Coercion` target, never in a parameter/result — so it carries no `Signature`
        // position, but IS counted in `TyparArity` (hence `MethodArity`) and gets its
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
                                target

                        FrozenConstraint.Coercion(i, toFrozen openTarget)
                    | _ -> ()
            ]

        {
            Signature = toFrozen openSig
            MethodArity = sym.TyparArity
            Constraints = constraints
        }
