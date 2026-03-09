let HashSample tab =
    tab.Iterate (fun c v ->
        printfn "Entry (%O,%O)" c v)
