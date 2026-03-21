module ForMinIndentation

let f xs = 
    for
     x 
     in
     xs
     do
    ignore x
    match x with
        | Some y -> printfn "Got %A" y
        | None -> printfn "Got nothing"

let g () =
    for
     i
     =
     1
     to
     10
     do
    printfn "i = %d" i
