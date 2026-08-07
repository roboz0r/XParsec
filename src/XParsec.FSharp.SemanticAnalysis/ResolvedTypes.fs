namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Parser

// Post: ctx.Diagnostics carries an Error per TDecl whose TAST still references an unresolved
//       TyVar — after generalisation every reachable TyVar should bottom out in a concrete
//       shape via Link chains, or be a quantified typar of the enclosing generalised `let`.

module ResolvedTypes =

    /// Walk `t` adding any free TyVar root — one whose `Link` is `ValueNone` — that is not
    /// in `allowed` to `acc`.
    let private addFreeRoots
        (store: TypeStore)
        (allowed: HashSet<TyVarId>)
        (acc: HashSet<TyVarId>)
        (t: SemType)
        : unit =
        let rec go t =
            match t with
            | TyVar tv ->
                let root = UnionFind.find store tv

                match store.Link root with
                | ValueSome target -> go target
                | ValueNone ->
                    if not (allowed.Contains root.Id) then
                        acc.Add(root.Id) |> ignore
            | t -> SemType.iterChildren go t

        go t

    /// Add this binding's scheme's quantified roots to `allowed`, returning the newly-added
    /// ones for the caller to pop. Skip-if-already-present, so an inner `let` cannot pop an
    /// outer scope's quantifieds.
    let private pushScheme (ctx: PassContext) (binding: TPat) (allowed: HashSet<TyVarId>) : ResizeArray<TyVarId> =
        let added = ResizeArray<TyVarId>()

        match binding with
        | TPat.NamedSimple(key, _, _) ->
            match ctx.Bindings.Scheme.TryGetValue key with
            | ValueSome scheme ->
                for tv in scheme.Quantified do
                    let root = UnionFind.find ctx.Store tv

                    if allowed.Add root.Id then
                        added.Add root.Id
            | ValueNone -> ()
        | _ -> ()

        added

    let private popScheme (allowed: HashSet<TyVarId>) (added: ResizeArray<TyVarId>) : unit =
        for tv in added do
            allowed.Remove tv |> ignore

    /// Build the visit-only iter for one decl walk: every node's `ty` feeds `addFreeRoots`,
    /// and so does each `Format` hole's `Ty`, which the default walker doesn't surface.
    let private buildIter (ctx: PassContext) (allowed: HashSet<TyVarId>) (acc: HashSet<TyVarId>) : TastWalk.Iter =
        { TastWalk.identityIter with
            VisitExpr =
                fun it e ->
                    addFreeRoots ctx.Store allowed acc (TastWalk.exprTy e)

                    match e with
                    | TExpr.Let(binding, value, body, _, _) ->
                        // The inner let's quantified roots are allowed in its value RHS and
                        // binding pattern only; popped before the body's check.
                        let added = pushScheme ctx binding allowed
                        TastWalk.iterPat it binding
                        TastWalk.iterExpr it value
                        popScheme allowed added
                        TastWalk.iterExpr it body
                        false
                    | TExpr.Format(sink, segments, _, _) ->
                        match sink with
                        | FormatSink.ToWriter(w, _)
                        | FormatSink.ToBuilder w -> TastWalk.iterExpr it w
                        | FormatSink.ToStdOut _
                        | FormatSink.ToStdErr _
                        | FormatSink.ToString -> ()

                        for seg in segments do
                            match seg with
                            | FormatSeg.Lit _ -> ()
                            | FormatSeg.Hole(hole, arg) ->
                                addFreeRoots ctx.Store allowed acc hole.Ty
                                TastWalk.iterExpr it arg
                            | FormatSeg.DynHole d ->
                                addFreeRoots ctx.Store allowed acc d.Spec.Ty
                                d.Width |> ValueOption.iter (TastWalk.iterExpr it)
                                d.Precision |> ValueOption.iter (TastWalk.iterExpr it)
                                TastWalk.iterExpr it d.Value
                            | FormatSeg.CallbackHole(spec, residue) ->
                                addFreeRoots ctx.Store allowed acc spec.Ty
                                TastWalk.iterExpr it residue

                        false
                    | _ -> true
            VisitPat =
                fun _ p ->
                    addFreeRoots ctx.Store allowed acc (TastWalk.patTy p)
                    true
        }

    /// Best-effort attribution for a decl-level diagnostic: the binding's own token where
    /// the head pattern is a `NamedSimple`, and no place in the file otherwise.
    let declSite (d: TDecl) : Site =
        match d with
        | TDecl.Let(TPat.NamedSimple(tok = tok), _, _, _) -> Site.ofToken tok
        | _ -> Site.Nowhere

    let private walkDecl (ctx: PassContext) (allowed: HashSet<TyVarId>) (d: TDecl) : unit =
        let acc = HashSet<TyVarId>()
        let iter = buildIter ctx allowed acc

        match d with
        | TDecl.Let(binding, value, _, ty) ->
            let added = pushScheme ctx binding allowed
            addFreeRoots ctx.Store allowed acc ty
            TastWalk.iterPat iter binding
            TastWalk.iterExpr iter value
            popScheme allowed added
        | TDecl.Expression(e, ty) ->
            addFreeRoots ctx.Store allowed acc ty
            TastWalk.iterExpr iter e
        | TDecl.Type _ ->
            // Surfaced type declarations carry no inferred TyVars to resolve
            // (their signatures are already concrete / typar markers by Elaborate).
            ()

        if acc.Count > 0 then
            ctx.Report(declSite d, Kind.Internal(InternalBreak.UnresolvedTyVars acc.Count))

    let run (ctx: PassContext) (tast: TastFile) : unit =
        let allowed = HashSet<TyVarId>()

        for d in tast.Decls do
            walkDecl ctx allowed d
