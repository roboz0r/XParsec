module FunBodyUndentation

// ============================================================
// `fun` BODY UNDENTATION INSIDE PARENS
// In parens, `fun` body can undent past `fun_col` down to
// the enclosing context's offside line (`let_col + 1`).
// ============================================================

module InParens =

    // Body at fun_col (normal, always works)
    let atFunCol =
        (fun x ->
        x + 1)

    // Body below fun_col but at let_col + 1 (undentation)
    let atLetColPlus1 =
        (fun x ->
     x + 1)

// ============================================================
// `fun` BODY WITHOUT PARENS (no undentation)
// Without parens, body must be at `fun_col + 1` minimum.
// ============================================================

module WithoutParens =
    let atFunColPlus1 =
        fun x ->
         x + 1       // Depends on `fun` indent + 1
