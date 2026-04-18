// Multi-line `else` / `if` where the inner `if` is written on the next line
// at or left of the `else` column. Per the collapse rule in `pElifOrElseIf`
// (same line OR `ifCol <= elseCol`), this collapses to a single chain arm.

let classify n =
    if n < 0 then
        "neg"
    else
    if n = 0 then
        "zero"
    else
        "positive"
