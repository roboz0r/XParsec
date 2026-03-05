module SimplePatForms

// Form 1: bare ident
type A1(x) =
    member _.X = x

// Form 2: simple-pat ':' type  (annotation)
type A2(x: int) =
    member _.X = x

// Form 3: multiple, mixed
type A3(x: int, y: float, z) =
    member _.X = x
    member _.Y = y
    member _.Z = z
