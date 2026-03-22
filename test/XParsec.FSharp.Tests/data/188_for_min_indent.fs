module ForMinIndentation

let f xs = 
    for
     x  // Depends on `for` indent + 1
     in // Depends on `for` indent + 1
     xs // Depends on `for` indent + 1
     do // Depends on `for` indent + 1
    ignore x // Depends on `for` indent. Establishes the minimum indent for the `for` loop body seq block
    match x with
        | Some y -> printfn "Got %A" y
        | None -> printfn "Got nothing"

let g () =
    for
     i  // Depends on `for` indent + 1
     =  // Depends on `for` indent + 1
     1  // Depends on `for` indent + 1
     to // Depends on `for` indent + 1
     10 // Depends on `for` indent + 1
     do // Depends on `for` indent + 1
    printfn "i = %d" i // Depends on `for` indent. Establishes the minimum indent for the `for` loop body seq block
