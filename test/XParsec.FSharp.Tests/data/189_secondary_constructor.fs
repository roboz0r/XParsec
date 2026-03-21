module Test

type Foo(x: int) =
    new() = Foo(0)
    member _.X = x
