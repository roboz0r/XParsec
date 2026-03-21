type Foo =
    | Bar of int

and [<RequireQualifiedAccess>] Baz =
    | Qux of int
