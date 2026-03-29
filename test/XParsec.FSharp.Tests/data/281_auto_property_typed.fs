module Test

type Foo() =
    member val Items = [||]: int[] with get, set
    member val Name = "" with get, set
    member _.Bar = 42
