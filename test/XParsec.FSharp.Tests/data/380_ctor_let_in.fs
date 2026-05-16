module Test

// Additional constructor body: let-in form
type Foo(x: int) =
    new() =
        let y = 42 in Foo(y)
    member _.X = x
