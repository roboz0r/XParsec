// Test 9: Match Expression
let check value =
    match value with
    | 0 -> printfn "Zero"
    | 1 ->
        printfn "One"
        printfn "Is the loneliest number"
    | _ ->
        printfn "Something else"