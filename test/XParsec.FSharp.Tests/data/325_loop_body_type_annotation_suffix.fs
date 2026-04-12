module Test

// Type annotation suffix on for/while loop body
// F# grammar: FOR _ IN _ DO typedSeqExprBlock
//             WHILE _ DO typedSeqExprBlock

let f () =
    let mutable acc = 0
    for i in 1..3 do
        acc <- acc + i : unit
    acc

let g () =
    let mutable n = 0
    while n < 3 do
        n <- n + 1 : unit
    n
