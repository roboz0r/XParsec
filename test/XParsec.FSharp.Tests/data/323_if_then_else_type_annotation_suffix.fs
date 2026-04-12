module Test

// Type annotation suffix on if/then/else branches
// F# grammar: IF typedSeqExpr THEN declExpr ELSE declExpr
// (branches are declExpr in pars.fsy but fsc accepts trailing : typ on each)

let f x = if x then 1 : int else 2 : int

let g x =
    if x then
        1 + 2 : int
    else
        3 + 4 : int
