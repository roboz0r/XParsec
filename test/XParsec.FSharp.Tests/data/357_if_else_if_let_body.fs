// `if / else if / else` chain inside a `let rec` body where all three arms'
// bodies share the same indent, which is LESS than the inner `if` keyword's
// column in the `else if` arm. Mirrors the pattern used throughout F# Core
// (e.g. `map.fs:43`, `set.fs:43`, `ilread.fs`).
//
// The `else if` collapses to a single chain arm (F# LexFilter sugar), so the
// arm's body offside line must be anchored at the `else` column, not the
// inner `if` column.

let rec sizeAux acc m =
    if isEmpty m then
        acc
    else if m.Height = 1 then
        acc + 1
    else
        sizeAux (sizeAux (acc + 1) m.Left) m.Right
