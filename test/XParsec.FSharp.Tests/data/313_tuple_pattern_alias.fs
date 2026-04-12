module Test

// Tuple pattern destructuring with 'as' alias
// From CheckExpressions.fs line 2814

let processTuple () =
    let a, _, _, b as res = (1, 2, 3, 4)
    let x, _, y, _, z, _ as all = (1, 2, 3, 4, 5, 6)
    (a + b, res, x + y + z, all)
