module NestedTryWithMatch

// ============================================================
// `with` OWNERSHIP RESOLUTION
// When `try/with` is inside a `match` clause, `with` belongs to
// `try` ONLY if it is at `try_col` or greater. If `with` appears
// below `try_col`, it is treated as belonging to the `match`.
// ============================================================

// `with` at `try_col` — belongs to `try`
let tryWithAtTryCol x =
    match x with
    | 1 ->
        try
            failwith "oops"
        with _ -> "caught"  // `with` at `try` col — belongs to `try`
    | _ -> "other"

// `with` at `try_col` (inline try) — belongs to `try`
let tryWithInlineAtTryCol x =
    match x with
    | 1 ->
        try failwith "oops"
        with _ -> "caught"  // `with` at `try` col — belongs to `try`
    | _ -> "other"

// `with` at `match_col` — belongs to `match`
// (This makes `try` have no handler, which is a compile error)
// Cannot demonstrate in a compiling test file.
