module TryFinallyMinIndentation

// ============================================================
// TRY BODY
// Must be at `try_col` minimum (same column as `try`).
// ============================================================

let tryBodyAtTryCol () =
    try
    1                 // Depends on `try` indent
    finally
        printfn "done"

// ============================================================
// `finally` KEYWORD
// Must be at `try_col` or further right (permitted undentation).
// ============================================================

let finallyAtTryCol () =
    try
        1
    finally           // `finally` at `try` indent (permitted undentation)
        printfn "done"

// ============================================================
// FINALLY BODY
// Must be at `try_col` minimum (same column as `try`).
// ============================================================

let finallyBodyAtTryCol () =
    try
        1
    finally
    printfn "done"    // Depends on `try` indent

let finallyBodyIndented () =
    try
        1
    finally
        printfn "done" // Standard style
