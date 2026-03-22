module WhileMinIndentation

// ============================================================
// CONDITION
// Must be at `while_col + 1` minimum.
// ============================================================

let condAtWhilePlus1 () =
    let mutable i = 0
    while
     i < 3            // Depends on `while` indent + 1
     do
    i <- i + 1

// ============================================================
// `do` KEYWORD
// Must be at `while_col + 1` minimum.
// Unlike `with`/`then`/`else`, `do` does NOT have permitted
// undentation to `while_col`.
// ============================================================

let doAtWhilePlus1 () =
    let mutable i = 0
    while
     i < 3
     do               // Depends on `while` indent + 1
    i <- i + 1

// ============================================================
// BODY (after `do`)
// Must be at `while_col` minimum (same column as `while`).
// The body establishes a SeqBlock context at the first token.
// ============================================================

let bodyAtWhileCol () =
    let mutable i = 0
    while i < 3 do
    i <- i + 1        // Depends on `while` indent

let bodyIndented () =
    let mutable i = 0
    while i < 3 do
        i <- i + 1    // Standard style

// ============================================================
// ALL ON SEPARATE LINES (most expanded form)
// ============================================================

let expandedForm () =
    let mutable i = 0
    while
     i < 3            // Depends on `while` indent + 1
     do               // Depends on `while` indent + 1
    i <- i + 1        // Depends on `while` indent
