module Test

type Foo() =
    member inline _.Bar(x) = x + 1
