namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis

// Pre:  Freeze has produced a `TastFile`; ctx.Bindings.Escape (Regions) and ctx.Bindings.Binding
//       (NameResolution) are populated.
// Post: every `let mutable x = init` whose binding-site has `Escape = HeapShared`
//       is rewritten into `let x = { contents = init } : Vesper.Ref<'T>`; every
//       `TExpr.Var x` in the binding's scope reads through `x.contents`; every
//       `TExpr.Assignment(Var x, v)` writes through `x.contents <- v`. The cell
//       type lives in `Vesper.Core.dll` and the codegen resolves it through
//       the cross-package record path — no `TDecl.Type` is
//       synthesised into the consumer PE.

module RefCellPromotion =

    /// The cell's single field. F# convention; the `.fsi` declaration uses the
    /// same name.
    [<Literal>]
    let private ContentsField = "contents"

    /// Wrap a value's underlying type in `Vesper.Ref<_>`.
    let private refType (inner: SemType) : SemType =
        TyRecord(RuntimeNames.vesperRefKey, EqArray.singleton inner)

    /// Walk `decls` collecting binding-site `NodeKey`s for every `let mutable`
    /// whose `ctx.Bindings.Escape` is `HeapShared`. The value bound at each key is the
    /// *post-promotion* type of the local (`Ref<'origTy>`).
    let private collectPromotions (ctx: PassContext) (decls: EqArray<TDecl>) : Dictionary<NodeKey, SemType> =
        let promote = Dictionary<NodeKey, SemType>(HashIdentity.Structural)

        let consider (k: NodeKey) (origTy: SemType) =
            match ctx.Bindings.Binding.TryGetValue k with
            | ValueSome rb when rb.IsMutable ->
                match ctx.Bindings.Escape.TryGetValue k with
                | ValueSome HeapShared -> promote.[k] <- refType origTy
                | _ -> ()
            | _ -> ()

        let considerPat (p: TPat) =
            match p with
            | TPat.NamedSimple(k, t, _) -> consider k t
            | _ -> ()

        // The only case-specific work is to fire `considerPat` on a `Let`'s
        // binder before the walker recurses into its value / body. Every
        // other case falls through to the default child recursion.
        let iter: TastWalk.Iter =
            { TastWalk.identityIter with
                VisitExpr =
                    fun _ e ->
                        match e with
                        | TExpr.Let(pat, _, _, _, _) -> considerPat pat
                        | _ -> ()

                        true
            }

        for d in decls do
            match d with
            | TDecl.Let(pat, value, _, _) ->
                considerPat pat
                TastWalk.iterExpr iter value
            | TDecl.LetFn _ ->
                failwith "LetFn is a frozen-phase node (produced at Freeze); unexpected in RefCellPromotion"
            | TDecl.Expression(e, _) -> TastWalk.iterExpr iter e
            | TDecl.Type _ -> ()

        promote

    /// Rewrite a TExpr tree so every reference to a promoted binding reads
    /// through `.contents`, every assignment writes through it, and the
    /// binding's RHS is wrapped in `{ contents = … }`.
    let private rewriteExpr (promote: IReadOnlyDictionary<NodeKey, SemType>) (e: TExpr) : TExpr =
        let wrapValueIfPromoted (pat: TPat) (value: TExpr) : TExpr =
            match pat with
            | TPat.NamedSimple(k, _, _) when promote.ContainsKey k ->
                // The synthesised cell wraps `value`; anchor it at the value's token.
                TExpr.RecordCons(EqArray.singleton (ContentsField, value), promote.[k], TastWalk.exprTok value)
            | _ -> value

        // Three overrides:
        //   - `TPat.NamedSimple(k, _)` for a promoted key: retype the binder
        //     to the cell's `Ref<_>` shape (covers nested binders inside
        //     Tuple/Record/Union sub-pats via default recursion).
        //   - `TExpr.Var(k)` of a promoted cell: read through `.contents`.
        //   - `TExpr.Assignment(Var k, rhs)` where `k` is promoted: write
        //     through `.contents`; the default Assignment arm would otherwise
        //     rewrite the LHS into a `FieldGet`, which is wrong (we need
        //     `FieldSet` on the cell, not a read of the value).
        //   - `TExpr.Let(pat, value, body)`: wrap the rewritten value in
        //     `{ contents = … }` when the binder is promoted.
        let mapper: TastWalk.Mapper =
            { TastWalk.identityMapper with
                OverridePat =
                    fun _ p ->
                        match p with
                        | TPat.NamedSimple(k, _, tok) ->
                            match promote.TryGetValue k with
                            | true, refTy -> ValueSome(TPat.NamedSimple(k, refTy, tok))
                            | _ -> ValueNone
                        | _ -> ValueNone
                OverrideExpr =
                    fun m e ->
                        match e with
                        | TExpr.Var(k, ty, tok) ->
                            match promote.TryGetValue k with
                            | true, refTy ->
                                // `ty` is the original (pre-promotion) value type,
                                // which is also the field's declared type after
                                // substitution. The read replaces the `Var`, so it
                                // keeps its token.
                                ValueSome(TExpr.FieldGet(TExpr.Var(k, refTy, tok), ContentsField, ty, tok))
                            | _ -> ValueNone
                        | TExpr.Assignment(TExpr.Var(k, _, varTok), rhs, unitTy, tok) when promote.ContainsKey k ->
                            let refTy = promote.[k]

                            ValueSome(
                                TExpr.FieldSet(
                                    TExpr.Var(k, refTy, varTok),
                                    ContentsField,
                                    TastWalk.mapExpr m rhs,
                                    unitTy,
                                    tok
                                )
                            )
                        | TExpr.Let(pat, value, body, ty, tok) ->
                            let pat' = TastWalk.mapPat m pat
                            let value' = wrapValueIfPromoted pat (TastWalk.mapExpr m value)
                            ValueSome(TExpr.Let(pat', value', TastWalk.mapExpr m body, ty, tok))
                        | _ -> ValueNone
            }

        TastWalk.mapExpr mapper e

    let private rewriteDecl (promote: IReadOnlyDictionary<NodeKey, SemType>) (d: TDecl) : TDecl =
        match d with
        | TDecl.LetFn _ -> failwith "LetFn is a frozen-phase node (produced at Freeze); unexpected in RefCellPromotion"
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
            let decls' = tast.Decls |> EqArray.map (rewriteDecl promote)
            // Records-handoff Phase 2 follow-up: the cell type lives in
            // `Vesper.Core.dll`; the rewritten `TyRecord("Vesper.Ref", _)`
            // resolves through the codegen's external-record path. No
            // synthesised `TDecl.Type` ships with the consumer.
            { tast with Decls = decls' }
