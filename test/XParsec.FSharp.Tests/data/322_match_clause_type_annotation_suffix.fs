module Test

// Type annotation suffix on match clause RHS
// F# grammar: patternAndGuard RARROW typedSeqExprBlock

let f x =
    match x with
    | Some v -> v + 1 : int
    | None -> 0 : int

// Same for function expressions
let g =
    function
    | Some v -> v + 1 : int
    | None -> 0 : int
