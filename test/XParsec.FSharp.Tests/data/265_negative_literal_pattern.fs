module TestNegativeLiteralPattern

let f x =
    match x with
    | -1 -> "minus one"
    | 0 -> "zero"
    | 1 -> "one"
    | -100 -> "minus hundred"
    | _ -> "other"

let g x =
    match x with
    | -1.0 -> "neg one"
    | 0.0 -> "zero"
    | _ -> "other"
