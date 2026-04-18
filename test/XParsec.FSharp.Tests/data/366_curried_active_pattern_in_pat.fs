// Parameterized active pattern used in match-arm pattern position, where the
// active pattern itself takes paren-wrapped expression arguments AND a bound
// identifier, and is nested inside another structural pattern.
//
// Canonical form from CheckComputationExpressions.fs (lines 807-818):
//
//   | InExpr(SynExpr.App(_, _, CustomOpId (isCustomOperation ceenv)
//                                         (customOperationIsLikeZip ceenv) nm,
//                        ExprAsPat secondSourcePat, _),
//            secondSource,
//            mZipCore) -> ...
//
// F# parameterized active pattern syntax `PatName arg1 arg2 boundVar` uses
// curried argument application in pattern position — the `argN` are
// expressions evaluated at match time, the final `boundVar` receives the
// match result.

let classify ceenv synExpr =
    match synExpr with
    | SynExpr.App (_, _, CustomOpId (isCustomOperation ceenv) (customOperationIsLikeZip ceenv) nm,
                   ExprAsPat secondSourcePat, _) ->
        Some (nm, secondSourcePat)
    | CustomOpId (isCustomOperation ceenv) (customOperationIsLikeZip ceenv) nm ->
        Some (nm, arbPat ())
    | _ -> None
