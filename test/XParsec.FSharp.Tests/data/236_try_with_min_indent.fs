module TryWithMinIndentation

// ============================================================
// TRY BODY
// The body of `try` must be at `try_col` minimum (same column).
// This differs from `let` which requires `let_col + 1`.
// ============================================================

// Body at `try` col (minimal)
let tryBodyAtTryCol () =
    try
    1
    with _ -> 0

// Body indented further (standard style)
let tryBodyIndented () =
    try
        1
    with _ -> 0

// Body on same line as `try`
let tryBodySameLine () =
    try 1
    with _ -> 0

// ============================================================
// `with` KEYWORD
// Must be at `try_col` or further right (permitted undentation).
// Cannot be before `try_col`.
// ============================================================

let withAtTryCol () =
    try
        1
    with _ -> 0      // `with` at `try` indent (permitted undentation)

let withIndented () =
    try
        1
     with _ -> 0     // `with` past `try` indent — also valid

// ============================================================
// CLAUSE PATTERN (after `with`)
// Pattern on same line as `with` — no separate indentation rule.
// Pattern on next line with `|` — `|` at `try_col` (permitted undentation).
// ============================================================

let patternOnWithLine () =
    try
        failwith "x"
    with _ -> 0       // Pattern `_` on same line as `with`

let patternWithBar () =
    try
        failwith "x"
    with
    | _ -> 0          // `|` at `try` indent (permitted undentation)

// ============================================================
// CLAUSE BODY (expression after `->`)
// Must be at `try_col` minimum (same column as `try`).
// NOT `try_col + 1` — this is the critical finding.
// The body does NOT depend on the pattern position or `with` position.
// ============================================================

// Clause body at `try` col (minimal)
let clauseBodyAtTryCol () =
    try
        failwith "x"
    with _ ->
    0                 // Depends on `try` indent (NOT `try` indent + 1)

// Clause body indented (standard)
let clauseBodyIndented () =
    try
        failwith "x"
    with _ ->
        0             // Standard indentation

// Pattern on same line as `with`, body on next line at `try` col
let patOnWithLineBodyNextLine () =
    try
        failwith "x"
    with _ ->
    0                 // Depends on `try` indent

// Multi-line body at `try` col
let multiLineBody () =
    try
        failwith "x"
    with _ ->
    let r = 0
    r + 1             // Both lines at `try` indent

// ============================================================
// MULTIPLE CLAUSES
// `|` bars must align. Body of each clause at `try_col` minimum.
// ============================================================

let multipleClauses () =
    try
        failwith "x"
    with
    | :? System.ArgumentException ->
     -1               // Depends on `try` indent (at try_col + 1 here)
    | _ ->
     0                // Depends on `try` indent (at try_col + 1 here)

// Note: When `|` bars are at `try_col`, clause body at `try_col` is ambiguous
// because the next token at `try_col` is interpreted as a new `|` clause.
// In practice, clause body should be indented past `|` col.
let multipleClausesMinimal () =
    try
        failwith "x"
    with
    | :? System.ArgumentException ->
     99               // Depends on `|` indent + 1 when bars are used
    | _ ->
     0                // Depends on `|` indent + 1 when bars are used
