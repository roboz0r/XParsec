module IfMinIndentation

// ============================================================
// CONDITION
// The condition after `if` must be at `if_col + 1` minimum.
// ============================================================

let condOnNextLine =
    if
     true // Depends on `if` indent + 1
    then 1
    else 2

// ============================================================
// THEN / ELSE / ELIF KEYWORDS
// These keywords can appear at `if_col` (permitted undentation)
// but NOT before `if_col`.
// ============================================================

let keywordsAtIfCol x =
    if x > 0
    then x        // `then` at `if` indent (permitted undentation)
    else -x       // `else` at `if` indent (permitted undentation)

let elifAtIfCol x =
    if x > 0 then
        1
    elif x = 0 then  // `elif` at `if` indent (permitted undentation)
        0
    else              // `else` at `if` indent (permitted undentation)
        -1

// ============================================================
// THEN-BODY / ELSE-BODY
// The body expression after `then` or `else` must be at
// `if_col + 1` minimum. The position of `then`/`else` is
// irrelevant — only `if_col` matters.
// ============================================================

// Body at `if_col + 1` (minimal)
let bodyMinimal x =
    if x > 0 then
     x            // Depends on `if` indent + 1
    else
     -x           // Depends on `if` indent + 1

// `then` indented further, body still only needs `if_col + 1`
let thenIndented x =
    if x > 0
       then
     x            // Depends on `if` indent + 1 (NOT `then` indent + 1)
       else
     -x           // Depends on `if` indent + 1 (NOT `else` indent + 1)

// ============================================================
// THEN-BODY on same line as `then`
// When `then` is on the same line, the body is inline.
// Multi-expression body on next line still depends on `if_col + 1`.
// ============================================================

let inlineThen x =
    if x > 0
    then x        // Same-line body: no indentation issue
    else -x

// ============================================================
// NESTED IF
// Each `if` establishes its own offside context.
// ============================================================

let nestedIf x y =
    if x > 0 then
        if y > 0 then
         1        // Depends on inner `if` indent + 1
        else
         -1       // Depends on inner `if` indent + 1
    else
     0            // Depends on outer `if` indent + 1
