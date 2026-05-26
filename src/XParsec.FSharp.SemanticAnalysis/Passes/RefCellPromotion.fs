namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis

// Pre:  Freeze has produced a `TastFile`; ctx.Escape (Regions) and ctx.Binding
//       (NameResolution) are populated.
// Post: every `let mutable x = init` whose binding-site has `Escape = HeapShared`
//       is rewritten into `let x = { contents = init } : Vesper.Ref<'T>`; every
//       `TExpr.Var x` in the binding's scope reads through `x.contents`; every
//       `TExpr.Assignment(Var x, v)` writes through `x.contents <- v`. The cell
//       type lives in `Vesper.Core.dll` and the codegen resolves it through
//       the cross-package record path (records-plan §B7) — no `TDecl.Type` is
//       synthesised into the consumer PE.

module RefCellPromotion =

    /// Canonical compiled name of the cell type as the contract layer surfaces
    /// it (the qualified compiled name keyed by `FSharpLib.extractRecordBody`).
    /// Codegen's `externalRecordRef` probes this name (and its arity-suffixed
    /// form) against the symbol provider stack.
    [<Literal>]
    let private RefTypeName = "Vesper.Ref"

    /// The cell's single field. F# convention; the `.fsi` declaration uses the
    /// same name.
    [<Literal>]
    let private ContentsField = "contents"

    /// Wrap a value's underlying type in `Vesper.Ref<_>`.
    let private refType (inner: SemType) : SemType = TyRecord(RefTypeName, [ inner ])

    /// Walk `decls` collecting binding-site `NodeKey`s for every `let mutable`
    /// whose `ctx.Escape` is `HeapShared`. The value bound at each key is the
    /// *post-promotion* type of the local (`Ref<'origTy>`).
    let private collectPromotions (ctx: PassContext) (decls: TDecl list) : Dictionary<NodeKey, SemType> =
        let promote = Dictionary<NodeKey, SemType>(HashIdentity.Structural)

        let consider (k: NodeKey) (origTy: SemType) =
            match ctx.Binding.TryGetValue k with
            | ValueSome rb when rb.IsMutable ->
                match ctx.Escape.TryGetValue k with
                | ValueSome HeapShared -> promote.[k] <- refType origTy
                | _ -> ()
            | _ -> ()

        let considerPat (p: TPat) =
            match p with
            | TPat.NamedSimple(k, t) -> consider k t
            | _ -> ()

        let rec walkExpr (e: TExpr) =
            match e with
            | TExpr.Let(pat, value, body, _) ->
                considerPat pat
                walkExpr value
                walkExpr body
            | TExpr.Lambda(_, b, _) -> walkExpr b
            | TExpr.App(f, a, _) ->
                walkExpr f
                walkExpr a
            | TExpr.IfThenElse(c, t, e, _) ->
                walkExpr c
                walkExpr t
                walkExpr e
            | TExpr.Tuple(items, _)
            | TExpr.Sequential(items, _) -> List.iter walkExpr items
            | TExpr.While(c, b, _) ->
                walkExpr c
                walkExpr b
            | TExpr.ForTo(_, s, e, b, _) ->
                walkExpr s
                walkExpr e
                walkExpr b
            | TExpr.ForIn(_, src, b, _) ->
                walkExpr src
                walkExpr b
            | TExpr.Match(sc, arms, _) ->
                walkExpr sc

                for arm in arms do
                    arm.Guard |> Option.iter walkExpr
                    walkExpr arm.Body
            | TExpr.TryWith(b, arms, _) ->
                walkExpr b

                for arm in arms do
                    arm.Guard |> Option.iter walkExpr
                    walkExpr arm.Body
            | TExpr.TryFinally(b, c, _) ->
                walkExpr b
                walkExpr c
            | TExpr.Assignment(l, r, _) ->
                walkExpr l
                walkExpr r
            | TExpr.RecordCons(fields, _) -> fields |> List.iter (fun (_, v) -> walkExpr v)
            | TExpr.RecordClone(src, ov, _) ->
                walkExpr src
                ov |> List.iter (fun (_, v) -> walkExpr v)
            | TExpr.FieldGet(r, _, _) -> walkExpr r
            | TExpr.FieldSet(r, _, v, _) ->
                walkExpr r
                walkExpr v
            | TExpr.UnionCons(_, args, _)
            | TExpr.New(_, args, _)
            | TExpr.StaticMethodCall(_, _, args, _)
            | TExpr.ILIntrinsic(_, args, _) -> List.iter walkExpr args
            | TExpr.MethodCall(r, _, args, _) ->
                walkExpr r
                List.iter walkExpr args
            | TExpr.PropertyGet(r, _, _) -> walkExpr r
            | TExpr.ExternalMember(r, _, _, _, _) -> r |> ValueOption.iter walkExpr
            | TExpr.Range(s, step, e, _) ->
                walkExpr s
                step |> Option.iter walkExpr
                walkExpr e
            | TExpr.Format(sink, segs, _) ->
                (match sink with
                 | FormatSink.ToWriter w
                 | FormatSink.ToBuilder w -> walkExpr w
                 | _ -> ())

                for seg in segs.Underlying do
                    match seg with
                    | FormatSeg.Lit _ -> ()
                    | FormatSeg.Hole(_, a) -> walkExpr a
            | TExpr.StaticOptimization(clauses, def, _) ->
                for c in clauses do
                    walkExpr c.Body

                walkExpr def
            | TExpr.Const _
            | TExpr.Var _
            | TExpr.External _
            | TExpr.Null _
            | TExpr.StaticPropertyGet _ -> ()

        for d in decls do
            match d with
            | TDecl.Let(pat, value, _, _) ->
                considerPat pat
                walkExpr value
            | TDecl.Expression(e, _) -> walkExpr e
            | TDecl.Type _ -> ()

        promote

    /// Rewrite a TExpr tree so every reference to a promoted binding reads
    /// through `.contents`, every assignment writes through it, and the
    /// binding's RHS is wrapped in `{ contents = … }`.
    let private rewriteExpr (promote: IReadOnlyDictionary<NodeKey, SemType>) (e: TExpr) : TExpr =
        let rec goPat (p: TPat) : TPat =
            match p with
            | TPat.NamedSimple(k, _) ->
                match promote.TryGetValue k with
                | true, refTy -> TPat.NamedSimple(k, refTy)
                | _ -> p
            | TPat.Wildcard _
            | TPat.Const _ -> p
            | TPat.Tuple(items, t) -> TPat.Tuple(List.map goPat items, t)
            | TPat.Record(fields, t) -> TPat.Record([ for (n, sub) in fields -> n, goPat sub ], t)
            | TPat.Union(c, fields, t) -> TPat.Union(c, List.map goPat fields, t)

        let wrapValueIfPromoted (pat: TPat) (value: TExpr) : TExpr =
            match pat with
            | TPat.NamedSimple(k, _) when promote.ContainsKey k ->
                TExpr.RecordCons([ ContentsField, value ], promote.[k])
            | _ -> value

        let rec goExpr (e: TExpr) : TExpr =
            match e with
            | TExpr.Var(k, ty) ->
                match promote.TryGetValue k with
                | true, refTy ->
                    // Bare reference to a promoted cell: read through `contents`.
                    // `ty` is the original (pre-promotion) value type, which is
                    // also the field's declared type after substitution.
                    TExpr.FieldGet(TExpr.Var(k, refTy), ContentsField, ty)
                | _ -> e
            | TExpr.Assignment(TExpr.Var(k, _), rhs, unitTy) when promote.ContainsKey k ->
                // `n <- v` on a promoted cell: write through `contents`. The
                // generic Assignment fall-through below would otherwise rewrite
                // the LHS `Var` into a `FieldGet` — which is wrong (we need a
                // FieldSet on the cell, not a read of the value).
                let refTy = promote.[k]
                TExpr.FieldSet(TExpr.Var(k, refTy), ContentsField, goExpr rhs, unitTy)
            | TExpr.Let(pat, value, body, ty) ->
                let pat' = goPat pat
                let value' = wrapValueIfPromoted pat (goExpr value)
                TExpr.Let(pat', value', goExpr body, ty)
            | TExpr.Lambda(p, b, t) -> TExpr.Lambda(goPat p, goExpr b, t)
            | TExpr.App(f, a, t) -> TExpr.App(goExpr f, goExpr a, t)
            | TExpr.IfThenElse(c, th, el, t) -> TExpr.IfThenElse(goExpr c, goExpr th, goExpr el, t)
            | TExpr.Tuple(items, t) -> TExpr.Tuple(List.map goExpr items, t)
            | TExpr.Sequential(items, t) -> TExpr.Sequential(List.map goExpr items, t)
            | TExpr.While(c, b, t) -> TExpr.While(goExpr c, goExpr b, t)
            | TExpr.ForTo(k, s, e, b, t) -> TExpr.ForTo(k, goExpr s, goExpr e, goExpr b, t)
            | TExpr.ForIn(p, src, b, t) -> TExpr.ForIn(goPat p, goExpr src, goExpr b, t)
            | TExpr.Match(sc, arms, t) -> TExpr.Match(goExpr sc, List.map goArm arms, t)
            | TExpr.TryWith(b, arms, t) -> TExpr.TryWith(goExpr b, List.map goArm arms, t)
            | TExpr.TryFinally(b, c, t) -> TExpr.TryFinally(goExpr b, goExpr c, t)
            | TExpr.Assignment(l, r, t) -> TExpr.Assignment(goExpr l, goExpr r, t)
            | TExpr.RecordCons(fields, t) -> TExpr.RecordCons([ for (n, v) in fields -> n, goExpr v ], t)
            | TExpr.RecordClone(src, ov, t) -> TExpr.RecordClone(goExpr src, [ for (n, v) in ov -> n, goExpr v ], t)
            | TExpr.FieldGet(r, n, t) -> TExpr.FieldGet(goExpr r, n, t)
            | TExpr.FieldSet(r, n, v, t) -> TExpr.FieldSet(goExpr r, n, goExpr v, t)
            | TExpr.UnionCons(c, args, t) -> TExpr.UnionCons(c, List.map goExpr args, t)
            | TExpr.New(c, args, t) -> TExpr.New(c, List.map goExpr args, t)
            | TExpr.MethodCall(r, n, args, t) -> TExpr.MethodCall(goExpr r, n, List.map goExpr args, t)
            | TExpr.PropertyGet(r, n, t) -> TExpr.PropertyGet(goExpr r, n, t)
            | TExpr.StaticMethodCall(c, n, args, t) -> TExpr.StaticMethodCall(c, n, List.map goExpr args, t)
            | TExpr.ExternalMember(r, k, n, isProp, t) ->
                TExpr.ExternalMember(ValueOption.map goExpr r, k, n, isProp, t)
            | TExpr.Range(s, step, e, t) -> TExpr.Range(goExpr s, Option.map goExpr step, goExpr e, t)
            | TExpr.Format(sink, segs, t) ->
                let sink' =
                    match sink with
                    | FormatSink.ToWriter w -> FormatSink.ToWriter(goExpr w)
                    | FormatSink.ToBuilder w -> FormatSink.ToBuilder(goExpr w)
                    | other -> other

                let segs' =
                    segs
                    |> EqArray.map (fun seg ->
                        match seg with
                        | FormatSeg.Lit _ -> seg
                        | FormatSeg.Hole(h, a) -> FormatSeg.Hole(h, goExpr a)
                    )

                TExpr.Format(sink', segs', t)
            | TExpr.ILIntrinsic(op, args, t) -> TExpr.ILIntrinsic(op, List.map goExpr args, t)
            | TExpr.StaticOptimization(clauses, def, t) ->
                let clauses' =
                    [
                        for c in clauses ->
                            {
                                Constraints = c.Constraints
                                Body = goExpr c.Body
                            }
                    ]

                TExpr.StaticOptimization(clauses', goExpr def, t)
            | TExpr.Const _
            | TExpr.External _
            | TExpr.Null _
            | TExpr.StaticPropertyGet _ -> e

        and goArm (arm: TMatchArm) : TMatchArm =
            {
                Pat = goPat arm.Pat
                Guard = Option.map goExpr arm.Guard
                Body = goExpr arm.Body
            }

        goExpr e

    let private rewriteDecl (promote: IReadOnlyDictionary<NodeKey, SemType>) (d: TDecl) : TDecl =
        match d with
        | TDecl.Let(pat, value, isInline, ty) ->
            // A top-level binding cannot itself be a promoted cell (module-level
            // mutables don't escape — they live in a static field), so the
            // pattern's type is unchanged; the rewrite reaches the inner
            // `let mutable` through the value's expression tree.
            TDecl.Let(pat, rewriteExpr promote value, isInline, ty)
        | TDecl.Expression(e, ty) -> TDecl.Expression(rewriteExpr promote e, ty)
        | TDecl.Type _ -> d

    let run (ctx: PassContext) (tast: TastFile) : TastFile =
        let promote = collectPromotions ctx tast.Decls

        if promote.Count = 0 then
            tast
        else
            let decls' = tast.Decls |> List.map (rewriteDecl promote)
            // Records-handoff Phase 2 follow-up: the cell type lives in
            // `Vesper.Core.dll`; the rewritten `TyRecord("Vesper.Ref", _)`
            // resolves through the codegen's external-record path. No
            // synthesised `TDecl.Type` ships with the consumer.
            { tast with Decls = decls' }
