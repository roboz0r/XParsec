module Test

// Additional constructor body: stmt ; init (SequenceAfter)
type Foo(x: int) =
    new(_: string) =
        printfn "side effect before"
        Foo(99)
    member _.X = x
