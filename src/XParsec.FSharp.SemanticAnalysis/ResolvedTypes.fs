namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Parser

// Post: ctx.Diagnostics carries an Error per TDecl whose TAST still references an unresolved
//       TyVar, because after generalisation every reachable TyVar should bottom out in a concrete
//       shape via Link chains, or be a quantified typar of the enclosing generalised `let`.

module ResolvedTypes =

    /// Walk `t` adding to `acc` any free TyVar root not in `allowed`. A root is free exactly
    /// when `Freeze` would lower it to `FTUnknown UnresolvedTypar`; a measured root is
    /// resolved when its carrier is.
    let private addFreeRoots
        (store: TypeStore)
        (allowed: HashSet<TyVarId>)
        (acc: HashSet<TyVarId>)
        (t: SemType)
        : unit =
        let rec go t =
            match t with
            | TyVar root ->
                if not (allowed.Contains root) then
                    acc.Add root |> ignore
            | t -> SemType.iterChildren go t

        go (UnionFind.zonkErased store t)

    /// Add the quantified roots of every scheme this binding pattern binds (each
    /// `NamedSimple`, at any nesting) to `allowed`, returning the newly-added ones for the
    /// caller to pop. Skip-if-already-present, so an inner `let` cannot pop an outer
    /// scope's quantifieds.
    let private pushScheme (ctx: PassContext) (binding: TPat) (allowed: HashSet<TyVarId>) : ResizeArray<TyVarId> =
        let added = ResizeArray<TyVarId>()

        for key in TastWalk.boundVarsOfTPat binding do
            match ctx.Bindings.Scheme.TryGetValue key with
            | ValueSome scheme ->
                for tv in scheme.Quantified do
                    let root = UnionFind.find ctx.Store tv

                    if allowed.Add root.Id then
                        added.Add root.Id
            | ValueNone -> ()

        added

    let private popScheme (allowed: HashSet<TyVarId>) (added: ResizeArray<TyVarId>) : unit =
        for tv in added do
            allowed.Remove tv |> ignore

    /// Run `walkMember` over every member of a binding group with every member's quantified
    /// roots allowed, because a member references its siblings.
    let private walkGroupMembers
        (ctx: PassContext)
        (allowed: HashSet<TyVarId>)
        (members: TLetMember seq)
        (walkMember: TLetMember -> unit)
        : unit =
        let added = ResizeArray<TyVarId>()

        for m in members do
            added.AddRange(pushScheme ctx m.Pattern allowed)

        for m in members do
            walkMember m

        popScheme allowed added

    /// Build the visit-only iter for one decl walk: every node's `ty` feeds `addFreeRoots`,
    /// and so does each `Format` hole's `Ty`, which the default walker doesn't surface.
    let private buildIter (ctx: PassContext) (allowed: HashSet<TyVarId>) (acc: HashSet<TyVarId>) : TastWalk.Iter =
        { TastWalk.identityIter with
            VisitExpr =
                fun it e ->
                    addFreeRoots ctx.Store allowed acc (TastWalk.exprTy e)

                    match e with
                    | TExpr.Let(binding = m; body = body) ->
                        // The inner let's quantified roots are allowed in its value RHS and
                        // binding pattern only; popped before the body's check.
                        let added = pushScheme ctx m.Pattern allowed
                        TastWalk.iterPat it m.Pattern
                        TastWalk.iterExpr it m.Value
                        popScheme allowed added
                        TastWalk.iterExpr it body
                        false
                    | TExpr.LetGroup(members, _, body, _, _) ->
                        walkGroupMembers
                            ctx
                            allowed
                            members
                            (fun m ->
                                TastWalk.iterPat it m.Pattern
                                TastWalk.iterExpr it m.Value
                            )

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
                    addFreeRoots ctx.Store allowed acc (TPatG.ty p)
                    true
        }

    /// Best-effort attribution for a decl-level diagnostic: the first `NamedSimple` bound
    /// in the binding's pattern, and no place in the file otherwise.
    let declSite (d: TDecl) : Site =
        let firstNamed (binding: TPat) =
            match TastWalk.namedSimplesOfTPat binding with
            | struct (_, tok) :: _ -> Site.ofToken tok
            | [] -> Site.Nowhere

        match TastWalk.declBindings d with
        | m :: _ -> firstNamed m.Pattern
        | [] -> Site.Nowhere

    let private walkDecl (ctx: PassContext) (allowed: HashSet<TyVarId>) (d: TDecl) : unit =
        let acc = HashSet<TyVarId>()
        let iter = buildIter ctx allowed acc

        match d with
        | TDecl.Let _
        | TDecl.LetGroup _ ->
            walkGroupMembers
                ctx
                allowed
                (TastWalk.declBindings d)
                (fun m ->
                    addFreeRoots ctx.Store allowed acc m.Ty
                    TastWalk.iterPat iter m.Pattern
                    TastWalk.iterExpr iter m.Value
                )
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
