let s () =
    let mutable acc = 0

    for i in 1..5 do
        acc <- acc + i

    acc

printfn "%d" (s ())
