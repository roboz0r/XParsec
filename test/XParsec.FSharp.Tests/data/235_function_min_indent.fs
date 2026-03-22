module FunctionMinIndentation

// ============================================================
// `|` BAR IN CLAUSES
// Can appear at `function_col` (permitted undentation).
// Can also undent further — `|` has no strict lower bound
// relative to `function`, only relative to the enclosing context.
// ============================================================

let barAtFunctionCol =
    function
    | 1 -> "one"  // `|` at `function` indent (permitted undentation)
    | _ -> "other"

let barAtFunctionColPlus1 =
    function
     | 1 -> "one" // `|` at `function` indent + 1
     | _ -> "other"

// ============================================================
// CLAUSE BODY (expression after `->`)
// With `|` bars: must be at `|_col + 1` minimum.
// Same rule as `match` — `|` position determines the minimum.
// ============================================================

// `|` at function_col (col 4), body at col 5 (|_col + 1)
let bodyAtBarPlus1 =
    function
    | 1 ->
     "one"        // Depends on `|` indent + 1
    | _ ->
     "other"      // Depends on `|` indent + 1

// `|` indented to col 6, body must be at col 7 (|_col + 1)
let barIndentedBodyAtBarPlus1 =
    function
      | 1 ->
       "one"      // Depends on `|` indent + 1
      | _ ->
       "other"    // Depends on `|` indent + 1

// ============================================================
// CLAUSE BODY AT `function` KEYWORD COLUMN
// Body can appear at `function_col` when `|` is at `function_col`.
// This is the `function_col` = `|_col` edge case.
// ============================================================

let bodyAtFunctionCol =
    function
    | 1 -> "one"
    | _ ->
    "other"       // Last clause body at `|_col` = `function_col`

// ============================================================
// FIRST CLAUSE WITHOUT LEADING `|`
// Pattern starts at its own column.
// ============================================================

let noLeadingBar =
    function
     1 -> "one"   // Pattern at `function` indent + 1 (no `|`)
    | _ -> "other"

// ============================================================
// `|` AT ENCLOSING CONTEXT COLUMN
// `|` can appear all the way at the `let` binding column.
// ============================================================

let barAtLetCol = function
| 1 -> "one"      // `|` at col 0 (`let` col)
| _ -> "other"
