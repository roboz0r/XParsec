namespace XParsec.FSharp.SemanticAnalysis

// Single point where the TAST's recursion shape is enumerated. Six passes used
// to each hand-roll a match over every `TExpr` case (`Inline.substExpr`,
// `Inline.freshen.fE`, `Freeze.mapExprTypes`, `RefCellPromotion`'s collector +
// rewriter, `ResolvedTypes.walkExpr`); a new TExpr case would silently slip
// past several of them via catch-alls. The walker centralises the enumeration
// — F#'s incomplete-match check now fires here in one place when the TAST
// grows a new case, instead of silently no-oping in five passes.
//
// Two flavours:
//   - `Mapper` (rewrite): the four passes that build a new TExpr per node.
//     `MapType` is applied to the embedded `ty` field of every node the
//     default arm rebuilds; `OverrideX` hooks let a pass short-circuit a case
//     (typically to inject custom binder logic or replace a node entirely).
//   - `Iter` (visit-only): the two passes that collect into mutable state via
//     closures. `VisitX` returns `false` to skip default child recursion
//     (when the handler walked children manually).
//
// Exhaustiveness: every `TExpr` / `TPat` / `FormatSink` / `FormatSeg` /
// `TStaticOptConstraint` case is enumerated explicitly with no `_ -> ()`
// catch-all. ResolvedTypes (and the rest) used to enumerate every case
// individually to keep this guarantee — the walker preserves it by being the
// one place that match happens.

[<RequireQualifiedAccess>]
module TastWalk =

    /// Project the `ty` field embedded in any `TExpr`. Every case carries one.
    /// Lets a visitor uniformly inspect a node's type without re-matching on
    /// the case.
    let exprTy (e: TExpr) : SemType =
        match e with
        | TExpr.Const(ty = ty)
        | TExpr.Var(ty = ty)
        | TExpr.External(ty = ty)
        | TExpr.Lambda(ty = ty)
        | TExpr.App(ty = ty)
        | TExpr.Let(ty = ty)
        | TExpr.Use(ty = ty)
        | TExpr.IfThenElse(ty = ty)
        | TExpr.Tuple(ty = ty)
        | TExpr.Sequential(ty = ty)
        | TExpr.While(ty = ty)
        | TExpr.ForTo(ty = ty)
        | TExpr.ForIn(ty = ty)
        | TExpr.Match(ty = ty)
        | TExpr.TryWith(ty = ty)
        | TExpr.TryFinally(ty = ty)
        | TExpr.Assignment(ty = ty)
        | TExpr.Null ty
        | TExpr.Range(ty = ty)
        | TExpr.RecordCons(ty = ty)
        | TExpr.RecordClone(ty = ty)
        | TExpr.FieldGet(ty = ty)
        | TExpr.FieldSet(ty = ty)
        | TExpr.UnionCons(ty = ty)
        | TExpr.New(ty = ty)
        | TExpr.MethodCall(ty = ty)
        | TExpr.PropertyGet(ty = ty)
        | TExpr.StaticMethodCall(ty = ty)
        | TExpr.StaticPropertyGet(ty = ty)
        | TExpr.StaticFieldGet(ty = ty)
        | TExpr.ExternalMember(ty = ty)
        | TExpr.Format(ty = ty)
        | TExpr.ILIntrinsic(ty = ty)
        | TExpr.StaticOptimization(ty = ty)
        | TExpr.Upcast(ty = ty)
        | TExpr.Downcast(ty = ty)
        | TExpr.TypeTest(ty = ty) -> ty

    /// Project the `ty` field embedded in any `TPat`.
    let patTy (p: TPat) : SemType =
        match p with
        | TPat.NamedSimple(ty = ty)
        | TPat.Wildcard ty
        | TPat.Tuple(ty = ty)
        | TPat.Const(ty = ty)
        | TPat.Record(ty = ty)
        | TPat.Union(ty = ty) -> ty

    /// Peel a curried `App` chain into its head and the arguments paired with
    /// each `App` node's *result* type. The inverse of `rebuildApp`. Shared by
    /// every spine-walking client (`EmitLower`'s eta/lowering, the pre-freeze
    /// `InlineExpansion` pass).
    let rec collectSpine (acc: (TExprG<'ty> * 'ty) list) (e: TExprG<'ty>) : TExprG<'ty> * (TExprG<'ty> * 'ty) list =
        match e with
        | TExprG.App(fn, arg, ty) -> collectSpine ((arg, ty) :: acc) fn
        | head -> head, acc

    /// Re-fold a head + (arg, result-type) spine back into a curried `App`
    /// chain. The inverse of `collectSpine`.
    let rebuildApp (head: TExprG<'ty>) (args: (TExprG<'ty> * 'ty) list) : TExprG<'ty> =
        List.fold (fun acc (arg, resTy) -> TExprG.App(acc, arg, resTy)) head args

    /// Rewrite hooks. Every `OverrideX` receives the active `Mapper` so an
    /// override can recurse manually with the same mapper (e.g. for binders
    /// where the override needs to bind a key before walking the body).
    /// Returning `ValueSome` replaces the node; `ValueNone` falls through to
    /// the default recursive rebuild (which applies `MapType` to the `ty`
    /// field and recurses on children via this mapper).
    [<NoEquality; NoComparison>]
    type Mapper =
        {
            /// Applied to the `ty` field of every node the default arm
            /// rebuilds. An override that constructs its own replacement is
            /// responsible for substituting types in the node it returns.
            MapType: SemType -> SemType
            OverrideExpr: Mapper -> TExpr -> TExpr voption
            OverridePat: Mapper -> TPat -> TPat voption
            OverrideArm: Mapper -> TMatchArm -> TMatchArm voption
        }

    /// Identity mapper: leaves every `ty` and node unchanged. Compose with
    /// `with` to override only the fields a pass needs.
    let identityMapper: Mapper =
        {
            MapType = id
            OverrideExpr = fun _ _ -> ValueNone
            OverridePat = fun _ _ -> ValueNone
            OverrideArm = fun _ _ -> ValueNone
        }

    let rec mapPat (m: Mapper) (p: TPat) : TPat =
        match m.OverridePat m p with
        | ValueSome p' -> p'
        | ValueNone ->
            let f = m.MapType

            match p with
            | TPat.NamedSimple(k, ty) -> TPat.NamedSimple(k, f ty)
            | TPat.Wildcard ty -> TPat.Wildcard(f ty)
            | TPat.Const(v, ty) -> TPat.Const(v, f ty)
            | TPat.Tuple(items, ty) -> TPat.Tuple(EqArray.map (mapPat m) items, f ty)
            | TPat.Record(fields, ty) -> TPat.Record(EqArray.map (fun (n, sub) -> n, mapPat m sub) fields, f ty)
            | TPat.Union(c, fields, ty) -> TPat.Union(c, EqArray.map (mapPat m) fields, f ty)

    let rec mapExpr (m: Mapper) (e: TExpr) : TExpr =
        match m.OverrideExpr m e with
        | ValueSome e' -> e'
        | ValueNone ->
            let f = m.MapType
            let pe = mapExpr m
            let pp = mapPat m
            let pa = mapArm m

            match e with
            | TExpr.Const(v, ty) -> TExpr.Const(v, f ty)
            | TExpr.Var(k, ty) -> TExpr.Var(k, f ty)
            | TExpr.External(n, k, ty) -> TExpr.External(n, k, f ty)
            | TExpr.Null ty -> TExpr.Null(f ty)
            // Tuple-constructor arguments evaluate left-to-right, so `pp p`
            // (binder) always runs before `pe b` / `pe v` / `pe body` — the
            // ordering `Inline.freshen` relies on for `Lambda` / `Let` /
            // `Match`-arm binders.
            | TExpr.Lambda(p, b, ty) -> TExpr.Lambda(pp p, pe b, f ty)
            | TExpr.App(fn, a, ty) -> TExpr.App(pe fn, pe a, f ty)
            | TExpr.Let(p, v, body, ty) -> TExpr.Let(pp p, pe v, pe body, f ty)
            | TExpr.Use(p, v, body, dispose, ty) -> TExpr.Use(pp p, pe v, pe body, dispose, f ty)
            | TExpr.IfThenElse(c, t, el, ty) -> TExpr.IfThenElse(pe c, pe t, pe el, f ty)
            | TExpr.Tuple(items, ty) -> TExpr.Tuple(EqArray.map pe items, f ty)
            | TExpr.Sequential(items, ty) -> TExpr.Sequential(EqArray.map pe items, f ty)
            | TExpr.While(c, b, ty) -> TExpr.While(pe c, pe b, f ty)
            // `var` is a `NodeKey`, not a `TPat`, so `OverridePat` cannot see
            // it — passes that rename binders (`Inline.freshen`) must override
            // `ForTo` at the expr level.
            | TExpr.ForTo(k, s, e2, b, ty) -> TExpr.ForTo(k, pe s, pe e2, pe b, f ty)
            | TExpr.ForIn(p, src, b, en, ty) -> TExpr.ForIn(pp p, pe src, pe b, en, f ty)
            | TExpr.Match(sc, arms, ty) -> TExpr.Match(pe sc, EqArray.map pa arms, f ty)
            | TExpr.TryWith(b, arms, ty) -> TExpr.TryWith(pe b, EqArray.map pa arms, f ty)
            | TExpr.TryFinally(b, c, ty) -> TExpr.TryFinally(pe b, pe c, f ty)
            | TExpr.Assignment(l, r, ty) -> TExpr.Assignment(pe l, pe r, f ty)
            | TExpr.Range(s, step, e2, ty) -> TExpr.Range(pe s, Option.map pe step, pe e2, f ty)
            | TExpr.RecordCons(fields, ty) -> TExpr.RecordCons(EqArray.map (fun (n, v) -> n, pe v) fields, f ty)
            | TExpr.RecordClone(src, ov, ty) -> TExpr.RecordClone(pe src, EqArray.map (fun (n, v) -> n, pe v) ov, f ty)
            | TExpr.FieldGet(r, n, ty) -> TExpr.FieldGet(pe r, n, f ty)
            | TExpr.FieldSet(r, n, v, ty) -> TExpr.FieldSet(pe r, n, pe v, f ty)
            | TExpr.UnionCons(c, args, ty) -> TExpr.UnionCons(c, EqArray.map pe args, f ty)
            | TExpr.New(c, args, ty) -> TExpr.New(c, EqArray.map pe args, f ty)
            | TExpr.MethodCall(r, k, via, args, ty) -> TExpr.MethodCall(pe r, k, via, EqArray.map pe args, f ty)
            | TExpr.PropertyGet(r, k, via, ty) -> TExpr.PropertyGet(pe r, k, via, f ty)
            | TExpr.StaticMethodCall(k, args, ty) -> TExpr.StaticMethodCall(k, EqArray.map pe args, f ty)
            | TExpr.StaticPropertyGet(k, ty) -> TExpr.StaticPropertyGet(k, f ty)
            | TExpr.StaticFieldGet(k, n, ty) -> TExpr.StaticFieldGet(k, n, f ty)
            | TExpr.ExternalMember(r, k, n, isProp, ty) ->
                TExpr.ExternalMember(ValueOption.map pe r, k, n, isProp, f ty)
            | TExpr.Format(sink, segs, ty) ->
                let sink =
                    match sink with
                    | FormatSink.ToWriter w -> FormatSink.ToWriter(pe w)
                    | FormatSink.ToBuilder w -> FormatSink.ToBuilder(pe w)
                    | FormatSink.ToStdOut _
                    | FormatSink.ToStdErr _
                    | FormatSink.ToString -> sink

                let segs =
                    segs
                    |> EqArray.map (fun seg ->
                        match seg with
                        | FormatSeg.Lit _ -> seg
                        | FormatSeg.Hole(h, a) -> FormatSeg.Hole({ h with Ty = f h.Ty }, pe a)
                    )

                TExpr.Format(sink, segs, f ty)
            | TExpr.ILIntrinsic(op, args, ty) -> TExpr.ILIntrinsic(op, EqArray.map pe args, f ty)
            // The default rebuild substitutes typars inside constraints too —
            // `Freeze.mapExprTypes` (used to push a remap through generic
            // member bodies) needs this. Passes that resolve clauses to a
            // single body (`Inline.substExpr`) override the node explicitly
            // and never reach this arm.
            | TExpr.StaticOptimization(clauses, def, ty) ->
                let mapConstraint c =
                    match c with
                    | TStaticOptConstraint.TyconEquals(tp, req) -> TStaticOptConstraint.TyconEquals(f tp, f req)
                    | TStaticOptConstraint.IsStruct tp -> TStaticOptConstraint.IsStruct(f tp)

                let clauses =
                    clauses
                    |> EqArray.map (fun cl ->
                        {
                            Constraints = EqArray.map mapConstraint cl.Constraints
                            Body = pe cl.Body
                        }
                    )

                TExpr.StaticOptimization(clauses, pe def, f ty)
            | TExpr.Upcast(src, ty) -> TExpr.Upcast(pe src, f ty)
            | TExpr.Downcast(src, ty) -> TExpr.Downcast(pe src, f ty)
            | TExpr.TypeTest(src, testTy, ty) -> TExpr.TypeTest(pe src, f testTy, f ty)

    and mapArm (m: Mapper) (arm: TMatchArm) : TMatchArm =
        match m.OverrideArm m arm with
        | ValueSome a' -> a'
        | ValueNone ->
            {
                Pat = mapPat m arm.Pat
                Guard = Option.map (mapExpr m) arm.Guard
                Body = mapExpr m arm.Body
            }

    /// Visit-only hooks. Returning `false` from a `VisitX` skips default child
    /// recursion (the override walked the children it wanted, or wants to skip
    /// them entirely); `true` continues with the default recursive walk.
    /// Every `VisitX` receives the active `Iter` so an override can recurse
    /// manually with the same iter.
    [<NoEquality; NoComparison>]
    type Iter =
        {
            VisitExpr: Iter -> TExpr -> bool
            VisitPat: Iter -> TPat -> bool
            VisitArm: Iter -> TMatchArm -> bool
        }

    /// Identity iter: visits every node and recurses with no extra work.
    /// Compose with `with` to override the cases a pass cares about.
    let identityIter: Iter =
        {
            VisitExpr = fun _ _ -> true
            VisitPat = fun _ _ -> true
            VisitArm = fun _ _ -> true
        }

    let rec iterPat (it: Iter) (p: TPat) : unit =
        if it.VisitPat it p then
            match p with
            | TPat.NamedSimple _
            | TPat.Wildcard _
            | TPat.Const _ -> ()
            | TPat.Tuple(items, _) ->
                for sub in items do
                    iterPat it sub
            | TPat.Record(fields, _) ->
                for (_, sub) in fields do
                    iterPat it sub
            | TPat.Union(_, fields, _) ->
                for sub in fields do
                    iterPat it sub

    let rec iterExpr (it: Iter) (e: TExpr) : unit =
        if it.VisitExpr it e then
            let walk = iterExpr it
            let walkPat = iterPat it
            let walkArm = iterArm it

            match e with
            | TExpr.Const _
            | TExpr.Var _
            | TExpr.External _
            | TExpr.Null _
            | TExpr.StaticPropertyGet _
            | TExpr.StaticFieldGet _ -> ()
            | TExpr.Lambda(p, b, _) ->
                walkPat p
                walk b
            | TExpr.App(fn, a, _) ->
                walk fn
                walk a
            | TExpr.Let(p, v, body, _)
            | TExpr.Use(p, v, body, _, _) ->
                walkPat p
                walk v
                walk body
            | TExpr.IfThenElse(c, t, el, _) ->
                walk c
                walk t
                walk el
            | TExpr.Tuple(items, _)
            | TExpr.Sequential(items, _) ->
                for x in items do
                    walk x
            | TExpr.While(c, b, _) ->
                walk c
                walk b
            | TExpr.ForTo(_, s, e2, b, _) ->
                walk s
                walk e2
                walk b
            | TExpr.ForIn(p, src, b, _, _) ->
                walkPat p
                walk src
                walk b
            | TExpr.Match(sc, arms, _) ->
                walk sc

                for arm in arms do
                    walkArm arm
            | TExpr.TryWith(b, arms, _) ->
                walk b

                for arm in arms do
                    walkArm arm
            | TExpr.TryFinally(b, c, _) ->
                walk b
                walk c
            | TExpr.Assignment(l, r, _) ->
                walk l
                walk r
            | TExpr.Range(s, step, e2, _) ->
                walk s
                step |> Option.iter walk
                walk e2
            | TExpr.RecordCons(fields, _) ->
                for (_, v) in fields do
                    walk v
            | TExpr.RecordClone(src, ov, _) ->
                walk src

                for (_, v) in ov do
                    walk v
            | TExpr.FieldGet(r, _, _) -> walk r
            | TExpr.FieldSet(r, _, v, _) ->
                walk r
                walk v
            | TExpr.UnionCons(_, args, _)
            | TExpr.New(_, args, _)
            | TExpr.StaticMethodCall(_, args, _)
            | TExpr.ILIntrinsic(_, args, _) ->
                for x in args do
                    walk x
            | TExpr.MethodCall(r, _, _, args, _) ->
                walk r

                for x in args do
                    walk x
            | TExpr.PropertyGet(r, _, _, _) -> walk r
            | TExpr.ExternalMember(r, _, _, _, _) -> r |> ValueOption.iter walk
            | TExpr.Format(sink, segs, _) ->
                match sink with
                | FormatSink.ToWriter w
                | FormatSink.ToBuilder w -> walk w
                | FormatSink.ToStdOut _
                | FormatSink.ToStdErr _
                | FormatSink.ToString -> ()

                for seg in segs do
                    match seg with
                    | FormatSeg.Lit _ -> ()
                    | FormatSeg.Hole(_, a) -> walk a
            // Default walk skips constraints (no expr children) — the
            // constraint typars are the binding's own quantified typars,
            // already known to passes that care (ResolvedTypes adds them to
            // `allowed`). A pass that needs to visit constraint types
            // overrides this case.
            | TExpr.StaticOptimization(clauses, def, _) ->
                walk def

                for c in clauses do
                    walk c.Body
            | TExpr.Upcast(src, _)
            | TExpr.Downcast(src, _)
            | TExpr.TypeTest(src, _, _) -> walk src

    and iterArm (it: Iter) (arm: TMatchArm) : unit =
        if it.VisitArm it arm then
            iterPat it arm.Pat
            arm.Guard |> Option.iter (iterExpr it)
            iterExpr it arm.Body
