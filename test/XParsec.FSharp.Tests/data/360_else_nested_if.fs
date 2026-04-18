// `else` followed by a genuinely nested `if` strictly to the right of `else`.
// Per the collapse rule in `pElifOrElseIf`, this does NOT collapse — the
// `else` branch is a normal block containing a single nested `if` expression.

let classify n =
    if n < 0 then
        "neg"
    else
        if n = 0 then
            "zero"
        else
            "positive"
