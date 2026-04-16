// Deeply nested multi-line function call whose argument structure mixes
// parenthesized calls, tuple literals, and a nested paren-expr applied to an
// additional argument — all broken across many lines with intermediate indent
// levels.
// Affects: LowerComputedCollections.fs
let mkInvisibleLet a b c d = (a, b, c, d)
let mkTryFinally a b c d e f = (a, b, c, d, e, f)
let mkWhile a b = (a, b)
let callNon a b = (a, b)

let test g enumv inp guardExpr bodyExprR cleanupE mFor mIn ty1 ty2 =
    let exprR =
        mkInvisibleLet mFor enumv (callNon g inp)
            (mkTryFinally g
                (mkWhile g (true, "nsl", guardExpr,
                    (mkInvisibleLet mIn "v"
                        (callNon g enumv))
                        bodyExprR, mIn),
                cleanupE,
                mFor, ty1, ty2))
    exprR
