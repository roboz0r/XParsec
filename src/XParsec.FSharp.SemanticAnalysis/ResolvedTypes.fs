namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic

// Pre:  Freeze has produced a TastFile.
// Post: ctx.Diagnostics carries an Error per TDecl whose TAST still
//       references an unresolved TyVar.
//
// Invariant checked: after generalisation + freeze, every reachable TyVar
// should either bottom out in a concrete shape via union-find Link chains, or
// be a quantified typar of the enclosing generalised `let`. Anything else is
// an inference bug — codegen would later fail in much less informative ways.
//
// Stays on indefinitely (see docs/front-end-gaps-plan.md §D). Turning it off
// lets latent generalisation bugs surface as broken IL much later.

module ResolvedTypes =

    /// Walk `t` adding any free TyVar root (`Link.IsNone`) not in `allowed`
    /// to `acc`. Same chase-through-Link semantics as `Unification.zonk`.
    let private addFreeRoots (allowed: HashSet<TypeVar>) (acc: HashSet<TypeVar>) (t: SemType) : unit =
        let rec go t =
            match t with
            | TyVar tv ->
                let root = UnionFind.find tv

                match root.Link with
                | ValueSome target -> go target
                | ValueNone ->
                    if not (allowed.Contains root) then
                        acc.Add(root) |> ignore
            | TyConst _ -> ()
            | TyFun(a, r) ->
                go a
                go r
            | TyTuple items -> items |> List.iter go
            | TyRecord(_, args)
            | TyUnion(_, args)
            | TyClass(_, args) -> args |> List.iter go

        go t

    /// Add this binding's scheme's quantified roots to `allowed`. Returns
    /// the list of newly-added roots so the caller can pop them after the
    /// binding's body walk. Skip-if-already-present so outer-scope
    /// quantifieds aren't accidentally popped by an inner let.
    let private pushScheme (ctx: PassContext) (binding: TPat) (allowed: HashSet<TypeVar>) : TypeVar list =
        match binding with
        | TPat.NamedSimple(key, _) ->
            match ctx.Scheme.TryGetValue key with
            | ValueSome scheme ->
                [
                    for tv in scheme.Quantified do
                        let root = UnionFind.find tv

                        if allowed.Add root then
                            root
                ]
            | ValueNone -> []
        | _ -> []

    let private popScheme (allowed: HashSet<TypeVar>) (added: TypeVar list) : unit =
        for tv in added do
            allowed.Remove tv |> ignore

    let rec private walkPat (allowed: HashSet<TypeVar>) (acc: HashSet<TypeVar>) (p: TPat) : unit =
        match p with
        | TPat.NamedSimple(_, t)
        | TPat.Wildcard t
        | TPat.Const(_, t) -> addFreeRoots allowed acc t
        | TPat.Tuple(items, t) ->
            addFreeRoots allowed acc t

            for sub in items do
                walkPat allowed acc sub
        | TPat.Record(fields, t) ->
            addFreeRoots allowed acc t

            for (_, sub) in fields do
                walkPat allowed acc sub
        | TPat.Union(_, fields, t) ->
            addFreeRoots allowed acc t

            for sub in fields do
                walkPat allowed acc sub

    let rec private walkExpr (ctx: PassContext) (allowed: HashSet<TypeVar>) (acc: HashSet<TypeVar>) (e: TExpr) : unit =
        match e with
        | TExpr.Const(_, ty)
        | TExpr.Var(_, ty)
        | TExpr.External(_, ty)
        | TExpr.Null ty -> addFreeRoots allowed acc ty
        | TExpr.Lambda(p, body, ty) ->
            addFreeRoots allowed acc ty
            walkPat allowed acc p
            walkExpr ctx allowed acc body
        | TExpr.App(fn, arg, ty) ->
            addFreeRoots allowed acc ty
            walkExpr ctx allowed acc fn
            walkExpr ctx allowed acc arg
        | TExpr.Let(binding, value, body, ty) ->
            addFreeRoots allowed acc ty
            // Inner let's quantified set is scoped to the value RHS and the
            // binding pattern's type; restore on exit so it doesn't leak
            // into the body's check.
            let added = pushScheme ctx binding allowed
            walkPat allowed acc binding
            walkExpr ctx allowed acc value
            popScheme allowed added
            walkExpr ctx allowed acc body
        | TExpr.IfThenElse(c, t, e2, ty) ->
            addFreeRoots allowed acc ty
            walkExpr ctx allowed acc c
            walkExpr ctx allowed acc t
            walkExpr ctx allowed acc e2
        | TExpr.Tuple(items, ty)
        | TExpr.Sequential(items, ty) ->
            addFreeRoots allowed acc ty

            for x in items do
                walkExpr ctx allowed acc x
        | TExpr.While(c, b, ty) ->
            addFreeRoots allowed acc ty
            walkExpr ctx allowed acc c
            walkExpr ctx allowed acc b
        | TExpr.ForTo(_, s, e2, b, ty) ->
            addFreeRoots allowed acc ty
            walkExpr ctx allowed acc s
            walkExpr ctx allowed acc e2
            walkExpr ctx allowed acc b
        | TExpr.ForIn(p, src, body, ty) ->
            addFreeRoots allowed acc ty
            walkPat allowed acc p
            walkExpr ctx allowed acc src
            walkExpr ctx allowed acc body
        | TExpr.Match(sc, arms, ty) ->
            addFreeRoots allowed acc ty
            walkExpr ctx allowed acc sc

            for arm in arms do
                walkArm ctx allowed acc arm
        | TExpr.TryWith(body, arms, ty) ->
            addFreeRoots allowed acc ty
            walkExpr ctx allowed acc body

            for arm in arms do
                walkArm ctx allowed acc arm
        | TExpr.TryFinally(body, cleanup, ty) ->
            addFreeRoots allowed acc ty
            walkExpr ctx allowed acc body
            walkExpr ctx allowed acc cleanup
        | TExpr.Assignment(lhs, rhs, ty) ->
            addFreeRoots allowed acc ty
            walkExpr ctx allowed acc lhs
            walkExpr ctx allowed acc rhs
        | TExpr.Range(s, step, e2, ty) ->
            addFreeRoots allowed acc ty
            walkExpr ctx allowed acc s

            match step with
            | Some st -> walkExpr ctx allowed acc st
            | None -> ()

            walkExpr ctx allowed acc e2
        | TExpr.RecordCons(fields, ty) ->
            addFreeRoots allowed acc ty

            for (_, v) in fields do
                walkExpr ctx allowed acc v
        | TExpr.RecordClone(src, overrides, ty) ->
            addFreeRoots allowed acc ty
            walkExpr ctx allowed acc src

            for (_, v) in overrides do
                walkExpr ctx allowed acc v
        | TExpr.FieldGet(r, _, ty) ->
            addFreeRoots allowed acc ty
            walkExpr ctx allowed acc r
        | TExpr.FieldSet(r, _, v, ty) ->
            addFreeRoots allowed acc ty
            walkExpr ctx allowed acc r
            walkExpr ctx allowed acc v
        | TExpr.UnionCons(_, args, ty)
        | TExpr.New(_, args, ty) ->
            addFreeRoots allowed acc ty

            for a in args do
                walkExpr ctx allowed acc a
        | TExpr.MethodCall(r, _, args, ty) ->
            addFreeRoots allowed acc ty
            walkExpr ctx allowed acc r

            for a in args do
                walkExpr ctx allowed acc a
        | TExpr.PropertyGet(r, _, ty) ->
            addFreeRoots allowed acc ty
            walkExpr ctx allowed acc r
        | TExpr.StaticMethodCall(_, _, args, ty) ->
            addFreeRoots allowed acc ty

            for a in args do
                walkExpr ctx allowed acc a
        | TExpr.StaticPropertyGet(_, _, ty) -> addFreeRoots allowed acc ty
        | TExpr.Format(sink, segments, ty) ->
            addFreeRoots allowed acc ty

            match sink with
            | FormatSink.ToWriter w
            | FormatSink.ToBuilder w -> walkExpr ctx allowed acc w
            | FormatSink.ToStdOut _
            | FormatSink.ToStdErr _
            | FormatSink.ToString -> ()

            for seg in segments do
                match seg with
                | FormatSeg.Lit _ -> ()
                | FormatSeg.Hole(hole, arg) ->
                    addFreeRoots allowed acc hole.Ty
                    walkExpr ctx allowed acc arg
        | TExpr.ILIntrinsic(_, args, ty) ->
            addFreeRoots allowed acc ty

            for a in args do
                walkExpr ctx allowed acc a

    and private walkArm (ctx: PassContext) (allowed: HashSet<TypeVar>) (acc: HashSet<TypeVar>) (arm: TMatchArm) : unit =
        walkPat allowed acc arm.Pat

        match arm.Guard with
        | Some g -> walkExpr ctx allowed acc g
        | None -> ()

        walkExpr ctx allowed acc arm.Body

    /// Best-effort attribution NodeKey for a decl-level diagnostic. The TAST
    /// doesn't preserve a per-node NodeKey, so we use the binding's site if
    /// the head pattern is a NamedSimple, otherwise a synthetic-at-0 key.
    let private declKey (d: TDecl) : NodeKey =
        match d with
        | TDecl.Let(TPat.NamedSimple(k, _), _, _, _) -> k
        | _ -> NodeKey(0UL)

    let private walkDecl (ctx: PassContext) (allowed: HashSet<TypeVar>) (d: TDecl) : unit =
        let acc = HashSet<TypeVar>(HashIdentity.Reference)

        match d with
        | TDecl.Let(binding, value, _, ty) ->
            let added = pushScheme ctx binding allowed
            addFreeRoots allowed acc ty
            walkPat allowed acc binding
            walkExpr ctx allowed acc value
            popScheme allowed added
        | TDecl.Expression(e, ty) ->
            addFreeRoots allowed acc ty
            walkExpr ctx allowed acc e
        | TDecl.Type _ ->
            // Surfaced type declarations carry no inferred TyVars to resolve
            // (their signatures are already concrete / typar markers by Freeze).
            ()

        if acc.Count > 0 then
            ctx.Diagnostics.Add
                {
                    Key = declKey d
                    Message = sprintf "ResolvedTypes: TAST contains %d unresolved TyVar(s) — inference bug" acc.Count
                    Severity = Error
                }

    let run (ctx: PassContext) (tast: TastFile) : unit =
        let allowed = HashSet<TypeVar>(HashIdentity.Reference)

        for d in tast.Decls do
            walkDecl ctx allowed d
