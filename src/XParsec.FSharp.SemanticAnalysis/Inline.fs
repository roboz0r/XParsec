namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic

// Codegen-facing helper for `let inline` expansion. Lives here (not in a
// pass) because the front-end pipeline never calls it — it's invoked by the
// downstream codegen layer once per call site, after Freeze has produced a
// self-contained TAST and the side tables have been discarded.
//
// The retained body of an `inline` binding carries its typars as free
// `TyVar` roots: the binding's generalised scheme quantified them, and the
// ResolvedTypes validator guarantees no *other* free TyVar survives into the
// frozen TAST. To expand at a call site, codegen resolves those typars to
// the caller's concrete types and substitutes them through the body.
// `inlineExpand` does exactly that type substitution; `freshen` does the
// NodeKey renaming so independent call sites don't alias each other's bound
// names (and thus codegen local slots). One thing stays the caller's
// responsibility: argument (beta) reduction of the resulting lambda against
// the actual arguments — it needs the call-site args the caller holds.

module Inline =

    /// Quantified typars of an inline binding, in the canonical order codegen
    /// must use when supplying type arguments to `inlineExpand`: first
    /// occurrence in a pre-order walk of the binding's generalised type. This
    /// reproduces the order `Unification.generalise` collects them in — it
    /// walks the same zonked type — so an order recovered from the frozen
    /// TAST lines up with the scheme that produced it. A measure-bearing root
    /// (Link set to its carrier) is *not* a typar; like `generalise` we skip
    /// it by following the Link rather than collecting the root.
    let quantifiedTypars (declTy: SemType) : TypeVar list =
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
            | TyConst _ -> ()
            | TyFun(a, r) ->
                go a
                go r
            | TyTuple xs -> List.iter go xs
            | TyRecord(_, args)
            | TyUnion(_, args)
            | TyClass(_, args) -> List.iter go args

        go declTy
        List.ofSeq acc

    /// Substitute typar roots present in `subst`. The frozen TAST is already
    /// zonked, so a free typar shows up as `TyVar root` with no Link — we
    /// chase to the union-find root and swap it for the caller's type. Roots
    /// absent from `subst` are returned unchanged (still abstract).
    let rec private substType (subst: Dictionary<TypeVar, SemType>) (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = UnionFind.find tv

            match subst.TryGetValue root with
            | true, repl -> repl
            | _ -> TyVar root
        | TyConst _ -> t
        | TyFun(a, r) -> TyFun(substType subst a, substType subst r)
        | TyTuple xs -> TyTuple(List.map (substType subst) xs)
        | TyRecord(n, args) -> TyRecord(n, List.map (substType subst) args)
        | TyUnion(n, args) -> TyUnion(n, List.map (substType subst) args)
        | TyClass(n, args) -> TyClass(n, List.map (substType subst) args)

    let rec private substPat (subst: Dictionary<TypeVar, SemType>) (p: TPat) : TPat =
        match p with
        | TPat.NamedSimple(k, t) -> TPat.NamedSimple(k, substType subst t)
        | TPat.Wildcard t -> TPat.Wildcard(substType subst t)
        | TPat.Const(v, t) -> TPat.Const(v, substType subst t)
        | TPat.Tuple(items, t) -> TPat.Tuple(List.map (substPat subst) items, substType subst t)
        | TPat.Record(fields, t) -> TPat.Record([ for (n, sub) in fields -> n, substPat subst sub ], substType subst t)
        | TPat.Union(c, fields, t) -> TPat.Union(c, List.map (substPat subst) fields, substType subst t)

    let rec private substExpr (subst: Dictionary<TypeVar, SemType>) (e: TExpr) : TExpr =
        let sT t = substType subst t
        let sE e = substExpr subst e
        let sP p = substPat subst p

        match e with
        | TExpr.Const(v, t) -> TExpr.Const(v, sT t)
        | TExpr.Var(k, t) -> TExpr.Var(k, sT t)
        | TExpr.External(n, t) -> TExpr.External(n, sT t)
        | TExpr.Null t -> TExpr.Null(sT t)
        | TExpr.Lambda(p, b, t) -> TExpr.Lambda(sP p, sE b, sT t)
        | TExpr.App(f, a, t) -> TExpr.App(sE f, sE a, sT t)
        | TExpr.Let(b, v, body, t) -> TExpr.Let(sP b, sE v, sE body, sT t)
        | TExpr.IfThenElse(c, th, el, t) -> TExpr.IfThenElse(sE c, sE th, sE el, sT t)
        | TExpr.Tuple(items, t) -> TExpr.Tuple(List.map sE items, sT t)
        | TExpr.Sequential(items, t) -> TExpr.Sequential(List.map sE items, sT t)
        | TExpr.While(c, b, t) -> TExpr.While(sE c, sE b, sT t)
        | TExpr.ForTo(k, s, e2, b, t) -> TExpr.ForTo(k, sE s, sE e2, sE b, sT t)
        | TExpr.ForIn(p, src, b, t) -> TExpr.ForIn(sP p, sE src, sE b, sT t)
        | TExpr.Match(sc, arms, t) -> TExpr.Match(sE sc, List.map (substArm subst) arms, sT t)
        | TExpr.TryWith(b, arms, t) -> TExpr.TryWith(sE b, List.map (substArm subst) arms, sT t)
        | TExpr.TryFinally(b, c, t) -> TExpr.TryFinally(sE b, sE c, sT t)
        | TExpr.Assignment(l, r, t) -> TExpr.Assignment(sE l, sE r, sT t)
        | TExpr.Range(s, step, e2, t) -> TExpr.Range(sE s, Option.map sE step, sE e2, sT t)
        | TExpr.RecordCons(fields, t) -> TExpr.RecordCons([ for (n, v) in fields -> n, sE v ], sT t)
        | TExpr.RecordClone(src, ov, t) -> TExpr.RecordClone(sE src, [ for (n, v) in ov -> n, sE v ], sT t)
        | TExpr.FieldGet(r, n, t) -> TExpr.FieldGet(sE r, n, sT t)
        | TExpr.FieldSet(r, n, v, t) -> TExpr.FieldSet(sE r, n, sE v, sT t)
        | TExpr.UnionCons(c, args, t) -> TExpr.UnionCons(c, List.map sE args, sT t)
        | TExpr.New(c, args, t) -> TExpr.New(c, List.map sE args, sT t)
        | TExpr.MethodCall(r, n, args, t) -> TExpr.MethodCall(sE r, n, List.map sE args, sT t)
        | TExpr.PropertyGet(r, n, t) -> TExpr.PropertyGet(sE r, n, sT t)
        | TExpr.StaticMethodCall(c, n, args, t) -> TExpr.StaticMethodCall(c, n, List.map sE args, sT t)
        | TExpr.StaticPropertyGet(c, n, t) -> TExpr.StaticPropertyGet(c, n, sT t)

    and private substArm (subst: Dictionary<TypeVar, SemType>) (arm: TMatchArm) : TMatchArm =
        {
            Pat = substPat subst arm.Pat
            Guard = Option.map (substExpr subst) arm.Guard
            Body = substExpr subst arm.Body
        }

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
            |> List.iteri (fun i tv ->
                if i < typeArgs.Length then
                    subst.[tv] <- typeArgs.[i]
            )

            if subst.Count = 0 then value else substExpr subst value
        | TDecl.Expression _ -> invalidArg "decl" "Inline.inlineExpand expects a TDecl.Let, got a TDecl.Expression"

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

        let rec fP (p: TPat) : TPat =
            match p with
            | TPat.NamedSimple(k, t) -> TPat.NamedSimple(bind k, t)
            | TPat.Wildcard _ -> p
            | TPat.Const _ -> p
            | TPat.Tuple(items, t) -> TPat.Tuple(List.map fP items, t)
            | TPat.Record(fields, t) -> TPat.Record([ for (n, sub) in fields -> n, fP sub ], t)
            | TPat.Union(c, fields, t) -> TPat.Union(c, List.map fP fields, t)

        let rec fE (e: TExpr) : TExpr =
            match e with
            | TExpr.Const _ -> e
            | TExpr.Var(k, t) -> TExpr.Var(useKey k, t)
            | TExpr.External _ -> e
            | TExpr.Null _ -> e
            | TExpr.Lambda(p, b, t) ->
                let p = fP p
                TExpr.Lambda(p, fE b, t)
            | TExpr.App(f, a, t) -> TExpr.App(fE f, fE a, t)
            | TExpr.Let(b, v, body, t) ->
                // `value` is not in the binder's scope (non-rec), but the
                // binder key is globally unique so binding it first cannot
                // mis-rewrite `value`; binding first keeps the rule uniform.
                let b = fP b
                TExpr.Let(b, fE v, fE body, t)
            | TExpr.IfThenElse(c, th, el, t) -> TExpr.IfThenElse(fE c, fE th, fE el, t)
            | TExpr.Tuple(items, t) -> TExpr.Tuple(List.map fE items, t)
            | TExpr.Sequential(items, t) -> TExpr.Sequential(List.map fE items, t)
            | TExpr.While(c, b, t) -> TExpr.While(fE c, fE b, t)
            | TExpr.ForTo(var, s, e2, b, t) ->
                let var = bind var
                TExpr.ForTo(var, fE s, fE e2, fE b, t)
            | TExpr.ForIn(p, src, b, t) ->
                let p = fP p
                TExpr.ForIn(p, fE src, fE b, t)
            | TExpr.Match(sc, arms, t) -> TExpr.Match(fE sc, List.map fArm arms, t)
            | TExpr.TryWith(b, arms, t) -> TExpr.TryWith(fE b, List.map fArm arms, t)
            | TExpr.TryFinally(b, c, t) -> TExpr.TryFinally(fE b, fE c, t)
            | TExpr.Assignment(l, r, t) -> TExpr.Assignment(fE l, fE r, t)
            | TExpr.Range(s, step, e2, t) -> TExpr.Range(fE s, Option.map fE step, fE e2, t)
            | TExpr.RecordCons(fields, t) -> TExpr.RecordCons([ for (n, v) in fields -> n, fE v ], t)
            | TExpr.RecordClone(src, ov, t) -> TExpr.RecordClone(fE src, [ for (n, v) in ov -> n, fE v ], t)
            | TExpr.FieldGet(r, n, t) -> TExpr.FieldGet(fE r, n, t)
            | TExpr.FieldSet(r, n, v, t) -> TExpr.FieldSet(fE r, n, fE v, t)
            | TExpr.UnionCons(c, args, t) -> TExpr.UnionCons(c, List.map fE args, t)
            | TExpr.New(c, args, t) -> TExpr.New(c, List.map fE args, t)
            | TExpr.MethodCall(r, n, args, t) -> TExpr.MethodCall(fE r, n, List.map fE args, t)
            | TExpr.PropertyGet(r, n, t) -> TExpr.PropertyGet(fE r, n, t)
            | TExpr.StaticMethodCall(c, n, args, t) -> TExpr.StaticMethodCall(c, n, List.map fE args, t)
            | TExpr.StaticPropertyGet(c, n, t) -> TExpr.StaticPropertyGet(c, n, t)

        and fArm (arm: TMatchArm) : TMatchArm =
            {
                Pat = fP arm.Pat
                Guard = Option.map fE arm.Guard
                Body = fE arm.Body
            }

        fE body
