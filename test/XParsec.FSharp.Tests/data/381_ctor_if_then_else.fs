module Test

// Additional constructor body: conditional form
type Foo(x: int) =
    new(b: bool) =
        if b then Foo(1) else Foo(0)
    member _.X = x
