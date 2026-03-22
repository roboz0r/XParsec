module FunMinIndentation

// ============================================================
// PARAMETERS
// Must be at `fun_col + 1` minimum.
// ============================================================

let paramsOnNextLine =
    fun
     x ->             // Depends on `fun` indent + 1
     x + 1

// ============================================================
// ARROW (`->`)
// Must be at `fun_col + 1` minimum.
// ============================================================

let arrowOnNextLine =
    fun x
     -> x + 1         // `->` depends on `fun` indent + 1

// ============================================================
// BODY (after `->`)
// Must be at `fun_col + 1` minimum (when not inside parens).
// ============================================================

let bodyAtFunPlus1 =
    fun x ->
     x + 1            // Depends on `fun` indent + 1

// ============================================================
// FUN BODY UNDENTATION (inside parens)
// When `fun` is inside parens (or brackets, etc.), the body
// can undent below `fun_col + 1` down to the enclosing
// construct's offside line.
// ============================================================

// Body below `fun` col but above enclosing `let` col
let funInParens =
    (fun x ->
 x + 1)              // Undents past `fun` to enclosing `let` indent + 1

// Body below paren col but above enclosing `let` col
let funInParensDeep =
    (fun x ->
  x + 1)             // Undents past `(` col — limited by enclosing `let` indent + 1

// Fun as argument — same undentation rule
let f g = g 1
let funAsArg =
    f (fun x ->
     x + 1)          // Can undent past `fun` inside parens

// ============================================================
// MULTIPLE PARAMETERS
// All parameters at `fun_col + 1` minimum.
// ============================================================

let multiParams =
    fun
     x                // Depends on `fun` indent + 1
     y ->             // Depends on `fun` indent + 1
     x + y
