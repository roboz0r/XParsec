module LazyMinIndentation

// ============================================================
// LAZY BODY
// The body of `lazy` must be at `lazy_col` minimum (same column).
// This is like `try` (same column), NOT like `let` (col + 1).
// ============================================================

// Body at `lazy` col (minimal)
let bodyAtLazyCol =
    lazy
    1                 // Depends on `lazy` indent

// Body indented (standard)
let bodyIndented =
    lazy
        1 + 2         // Standard indentation

// Body on same line as `lazy`
let bodySameLine = lazy 42
