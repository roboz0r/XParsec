// A generic `[<Struct>]` union: the `'T` payload is an ordinary generic value-type
// field, and the factories, matching and equality all go through the VALUETYPE-tagged
// self-`TypeSpec`.
[<Struct>]
type GBox<'T> =
    | Val of v: 'T
    | Num of n: int

let pick (g: GBox<int>) : int =
    match g with
    | Val v -> v
    | Num n -> n * 10

let a: GBox<int> = Val 3
let b: GBox<int> = Num 4

printfn "%d" (pick a)
printfn "%d" (pick b)
printfn "%b" (a = Val 3)
printfn "%b" (a = b)
