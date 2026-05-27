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
            | TyTuple items ->
                for x in items do
                    go x
            | TyRecord(_, args)
            | TyUnion(_, args)
            | TyClass(_, args) ->
                for a in args do
                    go a

        go t

    /// Add this binding's scheme's quantified roots to `allowed`. Returns
    /// the list of newly-added roots so the caller can pop them after the
    /// binding's body walk. Skip-if-already-present so outer-scope
    /// quantifieds aren't accidentally popped by an inner let.
    let private pushScheme (ctx: PassContext) (binding: TPat) (allowed: HashSet<TypeVar>) : ResizeArray<TypeVar> =
        let added = ResizeArray<TypeVar>()

        match binding with
        | TPat.NamedSimple(key, _) ->
            match ctx.Bindings.Scheme.TryGetValue key with
            | ValueSome scheme ->
                for tv in scheme.Quantified do
                    let root = UnionFind.find tv

                    if allowed.Add root then
                        added.Add root
            | ValueNone -> ()
        | _ -> ()

        added

    let private popScheme (allowed: HashSet<TypeVar>) (added: ResizeArray<TypeVar>) : unit =
        for tv in added do
            allowed.Remove tv |> ignore

    /// Build the visit-only iter for one decl walk. Every node's `ty` is fed
    /// into `addFreeRoots`; `Let` push/pops the binding's scheme so the
    /// quantified roots are allowed only inside the binding's value (not its
    /// body); `Format` visits each hole's `Ty` (a per-hole side type the
    /// default walker doesn't surface).
    let private buildIter (ctx: PassContext) (allowed: HashSet<TypeVar>) (acc: HashSet<TypeVar>) : TastWalk.Iter =
        { TastWalk.identityIter with
            VisitExpr =
                fun it e ->
                    addFreeRoots allowed acc (TastWalk.exprTy e)

                    match e with
                    | TExpr.Let(binding, value, body, _) ->
                        // Inner let's quantified set is scoped to the value RHS and
                        // the binding pattern's type; restore on exit so it doesn't
                        // leak into the body's check.
                        let added = pushScheme ctx binding allowed
                        TastWalk.iterPat it binding
                        TastWalk.iterExpr it value
                        popScheme allowed added
                        TastWalk.iterExpr it body
                        false
                    | TExpr.Format(sink, segments, _) ->
                        match sink with
                        | FormatSink.ToWriter w
                        | FormatSink.ToBuilder w -> TastWalk.iterExpr it w
                        | FormatSink.ToStdOut _
                        | FormatSink.ToStdErr _
                        | FormatSink.ToString -> ()

                        for seg in segments do
                            match seg with
                            | FormatSeg.Lit _ -> ()
                            | FormatSeg.Hole(hole, arg) ->
                                addFreeRoots allowed acc hole.Ty
                                TastWalk.iterExpr it arg

                        false
                    | _ -> true
            VisitPat =
                fun _ p ->
                    addFreeRoots allowed acc (TastWalk.patTy p)
                    true
        }

    /// Best-effort attribution NodeKey for a decl-level diagnostic. The TAST
    /// doesn't preserve a per-node NodeKey, so we use the binding's site if
    /// the head pattern is a NamedSimple, otherwise a synthetic-at-0 key.
    let private declKey (d: TDecl) : NodeKey =
        match d with
        | TDecl.Let(TPat.NamedSimple(k, _), _, _, _) -> k
        | _ -> NodeKey(0UL)

    let private walkDecl (ctx: PassContext) (allowed: HashSet<TypeVar>) (d: TDecl) : unit =
        let acc = HashSet<TypeVar>(HashIdentity.Reference)
        let iter = buildIter ctx allowed acc

        match d with
        | TDecl.Let(binding, value, _, ty) ->
            let added = pushScheme ctx binding allowed
            addFreeRoots allowed acc ty
            TastWalk.iterPat iter binding
            TastWalk.iterExpr iter value
            popScheme allowed added
        | TDecl.Expression(e, ty) ->
            addFreeRoots allowed acc ty
            TastWalk.iterExpr iter e
        | TDecl.Type _ ->
            // Surfaced type declarations carry no inferred TyVars to resolve
            // (their signatures are already concrete / typar markers by Freeze).
            ()

        if acc.Count > 0 then
            ctx.Diagnostics.Add
                {
                    Key = declKey d
                    Message = sprintf "ResolvedTypes: TAST contains %d unresolved TyVar(s) — inference bug" acc.Count
                    Code = ""
                    Severity = Error
                }

    let run (ctx: PassContext) (tast: TastFile) : unit =
        let allowed = HashSet<TypeVar>(HashIdentity.Reference)

        for d in tast.Decls do
            walkDecl ctx allowed d
