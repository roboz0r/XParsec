namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic

// `let inline` expansion helper. The pre-freeze `Passes.InlineExpansion` pass
// invokes it once per call site, between `Freeze.elaborate` and the
// `freezeTypars` cut, where `zonk` / union-find are still native. It lived
// codegen-side until beat (b) relocated the expander; it stays in
// this module (rather than the pass) because `openMethodSignature` below shares it
// and `Codegen` no longer references the inline machinery at all.
//
// The cross-package inline-body channel the pass uses now rides
// `IExternalSymbolProvider` directly (`TryLookupInlineBody` / `…ByName`), since
// `ExternalSymbols` compiles after `Tast` and can name `TDecl` — the sibling
// `IInlineBodyProvider` + the `box`/`:?` cast it required are gone.
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
            // TODO(frozen-type Phase 2): once `freeze` emits `TyTypar` for an
            // inline binding's quantified typars, this collector must yield them
            // by `index` instead of by `TyVar` root. No-op until then.
            | TyTypar _ -> ()

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
        // TODO(frozen-type Phase 2): substitute by `(axis,index)` once inline
        // bindings carry `TyTypar`. Passthrough until then.
        | TyTypar _ -> t

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

    /// Follow union-find roots + `.Link` to the concrete head of a type (the local
    /// equivalent of `Unification.zonk`, which compiles after this module so cannot be
    /// referenced here). Only the head is needed by the callers below.
    let rec private zonkHead (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome target -> zonkHead target
            | ValueNone -> TyVar root
        | _ -> t

    /// A project-local nominal (class / union / record) — the operand shape for
    /// which F#'s reflexive `when ^T : ^T` static-optimization condition holds (the
    /// type carries its own static operator member). Drives both the `holds`
    /// gate below and the `TraitCall` resolution.
    let private isNominalType (t: SemType) : bool =
        match zonkHead t with
        | TyClass _
        | TyUnion _
        | TyRecord _ -> true
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
                | TExpr.TraitCall(recvTy, _, _, _) -> isNominalType (sub recvTy)
                | _ -> true
            )

        let resolveStaticOpt (clauses: EqArray<TStaticOptClause>) (defaultExpr: TExpr) : TExpr =
            let m = substMapper subst

            match clauses |> EqArray.tryFind clauseSelected with
            | ValueSome cl -> TastWalk.mapExpr m cl.Body
            | ValueNone -> TastWalk.mapExpr m defaultExpr

        // Resolve a `TraitCall` once the trait typar has been substituted to a concrete
        // nominal: rewrite it to a `StaticMethodCall` on that type's static operator
        // member. This fires for the `when ^T : ^T` clause body selected by `holds`
        // above (so the receiver is always a nominal here); a non-nominal receiver is
        // left as a substituted `TraitCall` for a later phase to surface loudly.
        let resolveTraitCall
            (m: TastWalk.Mapper)
            (recvTy: SemType)
            (memberName: string)
            (args: EqArray<TExpr>)
            (ty: SemType)
            : TExpr voption =
            match zonkHead (sub recvTy) with
            | TyClass(k, _)
            | TyUnion(k, _) ->
                let memberKey = LocalSymbolKey.ofMember k memberName MemberKind.Method
                ValueSome(TExpr.StaticMethodCall(memberKey, EqArray.map (TastWalk.mapExpr m) args, sub ty))
            | _ -> ValueNone

        { TastWalk.identityMapper with
            MapType = sub
            OverrideExpr =
                fun m e ->
                    match e with
                    | TExpr.StaticOptimization(clauses, def, _) -> ValueSome(resolveStaticOpt clauses def)
                    | TExpr.TraitCall(recvTy, memberName, args, ty) -> resolveTraitCall m recvTy memberName args ty
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
            /// Count of distinct method typars — the `MethodSpec` generic-parameter count.
            MethodArity: int
        }

    /// Instantiate `sym` and rewrite its method-owned typars to positional
    /// `TyTypar(Method, i)`, `i` = first-appearance order over a pre-order
    /// walk of the curried monotype (a `TyFun` visits its parameter before its
    /// result, so this is params-left-to-right then return — the order the
    /// producer's static-method emit assigns its `!!i` slots). Replaces the
    /// retired codegen `signatureTypars` / `toOpen`. Link-chases through
    /// `UnionFind.find` + `.Link` exactly as `quantifiedTypars` does (no zonk
    /// dependency — `UnificationEngine` compiles later); for a fresh, unlinked
    /// instantiation that is a structural no-op.
    let openMethodSignature (sym: ExternalSymbol) : OpenMethodSignature =
        let monoSig = sym.Instantiate 0
        let order = Dictionary<TypeVar, int>(HashIdentity.Reference)

        let rec collect (t: SemType) =
            match t with
            | TyVar tv ->
                let root = UnionFind.find tv

                match root.Link with
                | ValueSome target -> collect target
                | ValueNone ->
                    if not (order.ContainsKey root) then
                        order.[root] <- order.Count
            | TyFun(a, b) ->
                collect a
                collect b
            | TyConst(_, xs)
            | TyTuple xs
            | TyRecord(_, xs)
            | TyUnion(_, xs)
            | TyClass(_, xs) ->
                for x in xs do
                    collect x
            | TyUnknown _
            | TyTypar _ -> ()

        collect monoSig

        let rec toOpen (t: SemType) : SemType =
            match t with
            | TyVar tv ->
                let root = UnionFind.find tv

                match root.Link with
                | ValueSome target -> toOpen target
                | ValueNone ->
                    match order.TryGetValue root with
                    | true, i -> TyTypar(TyparAxis.Method, i)
                    | _ -> TyVar root
            | TyFun(a, b) -> TyFun(toOpen a, toOpen b)
            | TyTuple xs -> TyTuple(EqArray.map toOpen xs)
            | TyConst(n, xs) -> TyConst(n, EqArray.map toOpen xs)
            | TyRecord(n, xs) -> TyRecord(n, EqArray.map toOpen xs)
            | TyUnion(n, xs) -> TyUnion(n, EqArray.map toOpen xs)
            | TyClass(n, xs) -> TyClass(n, EqArray.map toOpen xs)
            | (TyUnknown _ | TyTypar _) as other -> other

        {
            Signature = toFrozen (toOpen monoSig)
            MethodArity = order.Count
        }
