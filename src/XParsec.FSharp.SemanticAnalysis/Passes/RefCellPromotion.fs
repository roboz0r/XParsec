namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis

// Pre:  ctx.Bindings.Escape and ctx.Bindings.Binding populated; `tast` is elaborated.
// Post: `let mutable x = init` with `Escape = HeapShared` becomes
//       `let x = { contents = init } : Vesper.Ref<'T>`, reads become `x.contents`.

module RefCellPromotion =

    /// Matches the field name in `Vesper.Core`'s `core-types.fsi`.
    [<Literal>]
    let private ContentsField = "contents"

    /// Wrap a value's underlying type in `Vesper.Ref<_>`.
    let private refType (inner: SemType) : SemType =
        TyRecord(RuntimeNames.vesperRefKey, EqArray.singleton inner)

    /// Binding-site `NodeKey`s of every `let mutable` whose `Escape` is `HeapShared`,
    /// mapped to the local's POST-promotion type (`Ref<'origTy>`).
    let private collectPromotions (ctx: PassContext) (decls: EqArray<TDecl>) : Dictionary<NodeKey, SemType> =
        let promote = Dictionary<NodeKey, SemType>(HashIdentity.Structural)

        let iter: TastWalk.Iter =
            { TastWalk.identityIter with
                VisitExpr =
                    fun _ e ->
                        match e with
                        | TExpr.Let(TPat.NamedSimple(k, t, _, true), _, _, _, _, _) ->
                            match ctx.Bindings.Escape.TryGetValue k with
                            | ValueSome HeapShared -> promote.[k] <- refType t
                            | _ -> ()
                        | _ -> ()

                        true
            }

        for d in decls do
            match d with
            | TDecl.Let(_, value, _, _, _) ->
                // Only NESTED `let mutable` bound variables are candidates. A module-level mutable
                // is a static field (CLR) / reassignable `let` (JS), shared across closures
                // natively, and `rewriteDecl` leaves its declaration bare.
                TastWalk.iterExpr iter value
            | TDecl.Expression(e, _) -> TastWalk.iterExpr iter e
            | TDecl.Type _ -> ()

        promote

    /// Rewrite a TExpr tree so every reference to a promoted binding reads
    /// through `.contents`, every assignment writes through it, and the
    /// binding's RHS is wrapped in `{ contents = … }`.
    let private rewriteExpr (promote: IReadOnlyDictionary<NodeKey, SemType>) (e: TExpr) : TExpr =
        let wrapValueIfPromoted (pat: TPat) (value: TExpr) : TExpr =
            match pat with
            | TPat.NamedSimple(k, _, _, _) when promote.ContainsKey k ->
                // The synthesised cell wraps `value`; anchor it at the value's token.
                TExpr.RecordCons(EqArray.singleton (ContentsField, value), promote.[k], TastWalk.exprTok value)
            | _ -> value

        // `Assignment` needs its own arm: the default one rewrites the LHS `Var` into a
        // `FieldGet`, a READ of the cell's value, where the write needs a `FieldSet`
        // on the cell itself.
        let mapper: TastWalk.Mapper =
            { TastWalk.identityMapper with
                OverridePat =
                    fun _ p ->
                        match p with
                        | TPat.NamedSimple(k, _, tok, _) ->
                            match promote.TryGetValue k with
                            // The cell is bound once; only its `contents` is written.
                            | true, refTy -> ValueSome(TPat.NamedSimple(k, refTy, tok, false))
                            | _ -> ValueNone
                        | _ -> ValueNone
                OverrideExpr =
                    fun m e ->
                        match e with
                        | TExpr.Var(k, ty, tok) ->
                            match promote.TryGetValue k with
                            | true, refTy ->
                                // `ty` is the pre-promotion value type, which is also the
                                // field's declared type after substitution.
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
                        | TExpr.Let(pat, value, body, isRec, ty, tok) ->
                            let pat' = TastWalk.mapPat m pat
                            let value' = wrapValueIfPromoted pat (TastWalk.mapExpr m value)
                            ValueSome(TExpr.Let(pat', value', TastWalk.mapExpr m body, isRec, ty, tok))
                        | _ -> ValueNone
            }

        TastWalk.mapExpr mapper e

    let private rewriteDecl (promote: IReadOnlyDictionary<NodeKey, SemType>) (d: TDecl) : TDecl =
        match d with
        | TDecl.Let(pat, value, isInline, isRec, ty) ->
            // A top-level bound variable is never promoted, so the pattern's type is unchanged;
            // the rewrite reaches any inner `let mutable` through the value's tree.
            TDecl.Let(pat, rewriteExpr promote value, isInline, isRec, ty)
        | TDecl.Expression(e, ty) -> TDecl.Expression(rewriteExpr promote e, ty)
        | TDecl.Type _ -> d

    let run (ctx: PassContext) (tast: TastFile) : TastFile =
        let promote = collectPromotions ctx tast.Decls

        if promote.Count = 0 then
            tast
        else
            let decls' = tast.Decls |> EqArray.map (rewriteDecl promote)
            // The cell type lives in `Vesper.Core.dll` and resolves through the codegen's
            // external-record path, so no synthesised `TDecl.Type` ships with the consumer.
            { tast with Decls = decls' }
