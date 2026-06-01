namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic

// Codegen-facing helper for `let inline` expansion. Lives here (not in a
// pass) because the front-end pipeline never calls it — codegen invokes it
// once per call site, after Freeze has produced a self-contained TAST and the
// side tables have been discarded.
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

    /// Quantified typars of an inline binding, in the canonical order codegen
    /// must use when supplying type arguments to `inlineExpand`: first
    /// occurrence in a pre-order walk of the binding's generalised type. This
    /// reproduces the order `Unification.generalise` collects them in — it
    /// walks the same zonked type — so an order recovered from the frozen
    /// TAST lines up with the scheme that produced it. A measure-bearing root
    /// (Link set to its carrier) is *not* a typar; like `generalise` we skip
    /// it by following the Link rather than collecting the root.
    let quantifiedTypars (declTy: SemType) : TypeVar[] =
        let acc = ResizeArray<TypeVar>()
        let seen = HashSet<TypeVar>(HashIdentity.Reference)

        let rec go t =
            match t with
            | TyVar tv ->
                let root = UnionFind.find tv

                match root.Link with
                | ValueSome target -> go target
                | ValueNone ->
                    if seen.Add root then
                        acc.Add root
            | TyConst(_, args) ->
                for a in args do
                    go a
            | TyFun(a, r) ->
                go a
                go r
            | TyTuple xs ->
                for x in xs do
                    go x
            | TyRecord(_, args)
            | TyUnion(_, args)
            | TyClass(_, args) ->
                for a in args do
                    go a
            | TyUnknown _ -> ()

        go declTy
        acc.ToArray()

    /// Substitute typar roots present in `subst`. The frozen TAST is zonked,
    /// so a free typar is `TyVar root` with no Link; chase to the union-find
    /// root and swap. Roots absent from `subst` stay abstract.
    let rec private substType (subst: Dictionary<TypeVar, SemType>) (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = UnionFind.find tv

            match subst.TryGetValue root with
            | true, repl -> repl
            | _ -> TyVar root
        | TyConst(n, args) -> TyConst(n, EqArray.map (substType subst) args)
        | TyFun(a, r) -> TyFun(substType subst a, substType subst r)
        | TyTuple xs -> TyTuple(EqArray.map (substType subst) xs)
        | TyRecord(n, args) -> TyRecord(n, EqArray.map (substType subst) args)
        | TyUnion(n, args) -> TyUnion(n, EqArray.map (substType subst) args)
        | TyClass(n, args) -> TyClass(n, EqArray.map (substType subst) args)
        | TyUnknown _ -> t

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
        | TyConst(n1, xs), TyConst(n2, ys) ->
            canonPrimName n1 = canonPrimName n2 && EqArray.forall2 staticOptTypesMatch xs ys
        | TyFun(a1, r1), TyFun(a2, r2) -> staticOptTypesMatch a1 a2 && staticOptTypesMatch r1 r2
        | TyTuple xs, TyTuple ys -> EqArray.forall2 staticOptTypesMatch xs ys
        | TyRecord(n1, xs), TyRecord(n2, ys)
        | TyUnion(n1, xs), TyUnion(n2, ys)
        | TyClass(n1, xs), TyClass(n2, ys) -> n1 = n2 && EqArray.forall2 staticOptTypesMatch xs ys
        | _ -> false

    /// Approximate `when ^T : struct` for the value-type primitives the operator
    /// surface can reach; anything else is treated as non-struct. Full struct
    /// detection on user types awaits the attribute walker (C-Attr).
    let private isStructType (t: SemType) : bool =
        match t with
        | TyConst(("int" | "int32" | "int64" | "byte" | "uint8" | "float" | "double" | "float64" | "bool" | "char" | "decimal"),
                  _) -> true
        | _ -> false

    /// Build the typar-substituting mapper for one inline expansion. The
    /// `StaticOptimization` override is the only customisation: at call-site
    /// expansion the typars have been pinned, so pick the first clause whose
    /// constraints hold and keep only its (substituted) body (prereq 3 — see
    /// docs/operators-plan.md). Everything else falls through to the default
    /// rewrite, which threads `substType subst` through every embedded `ty`.
    let rec private substMapper (subst: Dictionary<TypeVar, SemType>) : TastWalk.Mapper =
        let sub = substType subst

        let holds (c: TStaticOptConstraint) =
            match c with
            | TStaticOptConstraint.TyconEquals(typar, required) -> staticOptTypesMatch (sub typar) (sub required)
            | TStaticOptConstraint.IsStruct typar -> isStructType (sub typar)

        let resolveStaticOpt (clauses: EqArray<TStaticOptClause>) (defaultExpr: TExpr) : TExpr =
            let m = substMapper subst

            match clauses |> EqArray.tryFind (fun cl -> cl.Constraints |> EqArray.forall holds) with
            | ValueSome cl -> TastWalk.mapExpr m cl.Body
            | ValueNone -> TastWalk.mapExpr m defaultExpr

        { TastWalk.identityMapper with
            MapType = sub
            OverrideExpr =
                fun _ e ->
                    match e with
                    | TExpr.StaticOptimization(clauses, def, _) -> ValueSome(resolveStaticOpt clauses def)
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
                        | TPat.NamedSimple(k, t) -> ValueSome(TPat.NamedSimple(bind k, t))
                        | _ -> ValueNone
                OverrideExpr =
                    fun m e ->
                        match e with
                        | TExpr.Var(k, t) -> ValueSome(TExpr.Var(useKey k, t))
                        | TExpr.ForTo(var, s, e2, b, t) ->
                            let var = bind var

                            ValueSome(
                                TExpr.ForTo(var, TastWalk.mapExpr m s, TastWalk.mapExpr m e2, TastWalk.mapExpr m b, t)
                            )
                        | _ -> ValueNone
            }

        TastWalk.mapExpr mapper body
