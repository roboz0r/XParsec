module MatchMinIndentation

// ============================================================
// MATCH EXPRESSION (the value being matched)
// Must be at `match_col + 1` minimum.
// ============================================================

let matchExprOnNextLine x =
    match
     x            // Depends on `match` indent + 1
    with
    | 1 -> "one"
    | _ -> "other"

// ============================================================
// `with` KEYWORD
// Can appear at `match_col` (permitted undentation).
// ============================================================

let withAtMatchCol x =
    match x
    with          // `with` at `match` indent (permitted undentation)
    | 1 -> "one"
    | _ -> "other"

// ============================================================
// `|` BAR IN CLAUSES
// Can appear at `match_col` (permitted undentation).
// Sets the alignment for subsequent `|` tokens.
// ============================================================

let barAtMatchCol x =
    match x with
    | 1 -> "one"  // `|` at `match` indent (permitted undentation)
    | _ -> "other"

// ============================================================
// CLAUSE BODY (expression after `->`)
// With `|` bars: must be at `|_col + 1` minimum.
// The `|` position determines the clause body minimum, not `match`.
// ============================================================

// `|` at match_col (col 4), body at col 5 (|_col + 1)
let bodyAtBarPlus1 x =
    match x with
    | 1 ->
     "one"        // Depends on `|` indent + 1
    | _ ->
     "other"      // Depends on `|` indent + 1

// `|` indented to col 6, body must be at col 7 (|_col + 1)
let barIndentedBodyAtBarPlus1 x =
    match x with
      | 1 ->
       "one"      // Depends on `|` indent + 1
      | _ ->
       "other"    // Depends on `|` indent + 1

// ============================================================
// FIRST CLAUSE WITHOUT LEADING `|`
// Pattern starts at its own column. Body depends on `match_col + 1`.
// ============================================================

let noLeadingBar x =
    match x with
     1 ->         // Pattern at `match` indent + 1 (no `|`)
     "one"        // Depends on `match` indent + 1
    | _ -> "other"

// ============================================================
// `with` ON NEXT LINE with clauses
// `with` at `match` col, clauses still use `|` alignment.
// ============================================================

let withOnNextLine x =
    match x
    with
    | 1 -> "one"
    | _ ->
     "other"      // Depends on `|` indent + 1

// ============================================================
// MULTIPLE CLAUSES — body alignment
// Each clause body independently must be past `|` col.
// ============================================================

let multipleClauses x =
    match x with
    | 1 ->
     "one"
    | 2 ->
     "two"
    | _ ->
     "other"
