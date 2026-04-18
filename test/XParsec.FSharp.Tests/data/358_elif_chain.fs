// Plain `elif` chain. The body of an `elif` arm may share a column with other
// bodies in the same chain; the body's offside line is anchored at the `elif`
// keyword's column.

let classify n =
    if n < 0 then
        "neg"
    elif n = 0 then
        "zero"
    elif n < 10 then
        "small"
    else
        "large"
