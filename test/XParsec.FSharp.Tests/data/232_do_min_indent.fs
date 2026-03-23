module DoMinIndentation

// ============================================================
// `do` BODY
// Must be at `do_col + 1` minimum.
// ============================================================

do
 printfn "body at do_col + 1"  // Depends on `do` indent + 1

module Nested =
    do
     printfn "nested body at do_col + 1"  // Depends on `do` indent + 1
