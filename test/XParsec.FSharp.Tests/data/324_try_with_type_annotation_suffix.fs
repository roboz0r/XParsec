module Test

// Type annotation suffix on try body and try-with clause RHS
// F# grammar: TRY typedSeqExprBlock WITH clauses

let f () =
    try
        1 + 2 : int
    with _ ->
        0 : int

let g () =
    try
        1 + 2 : int
    finally
        printfn "done"
