module Test

// Combined: let-in, stmt sequence, init, and 'then' post-init clause
type Foo(x: int) =
    new(b: bool) =
        let y = if b then 1 else 0
        printfn "constructing %d" y
        Foo(y)
        then printfn "constructed"
    member _.X = x
