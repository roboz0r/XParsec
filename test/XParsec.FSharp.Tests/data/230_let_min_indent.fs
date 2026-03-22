module LetMinIndentation

// ============================================================
// let BINDING BODY
// The body of a `let` binding (after `=`) must be indented
// strictly past the `let` keyword: `let_col + 1` minimum.
// The position of `=` is irrelevant.
// ============================================================

// Body on next line, indented past `let`
let a =
 1 // Depends on `let` indent + 1

// Body well-indented
let b =
    1 // Depends on `let` indent + 1

// `=` on a different line — body still depends on `let`, not `=`
let c
      =
 1 // Depends on `let` indent + 1 (NOT `=` indent + 1)

// Function form — same rule
let f x =
 x + 1 // Depends on `let` indent + 1

// ============================================================
// let CONTINUATION IN SEQ BLOCK
// The continuation expression after a `let` binding
// must be at or past the `let` keyword's column.
// This is because `let` establishes the SeqBlock context.
// ============================================================

let outer1 =
    let a = 1
    a + 1 // Depends on `let` indent (same column as inner `let`)

let outer2 =
    let a = 1
     in // Explicit `in` — depends on `let` indent + 1
    a + 1

// ============================================================
// NESTED let
// Each nested `let` establishes its own body indentation
// relative to its own `let` keyword.
// ============================================================

let nested =
    let a =
     1 // Depends on inner `let` indent + 1
    let b =
     2 // Depends on inner `let` indent + 1
    a + b // Depends on inner `let` indent (seq block continuation)
