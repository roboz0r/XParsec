module Test

// Additional constructor body: light-syntax let (virtual 'in' from offside rule)
type Foo(x: int) =
    new() =
        let y = 42
        Foo(y)
    member _.X = x
