// A generic [<Struct>] union: the 'T payload field is an ordinary generic value-type
// field on the CLR, and an ordinary property on JS. Construction, matching and
// structural equality must agree across the two backends.
[<Struct>]
type G<'T> =
    | Val of v: 'T
    | Num of n: int

let pick (g: G<int>) : int =
    match g with
    | Val v -> v
    | Num n -> n * 10

let a: G<int> = Val 3
let b: G<int> = Num 4

printfn "%d" (pick a)
printfn "%d" (pick b)
printfn "%b" (a = Val 3)
printfn "%b" (a = b)

let s: G<string> = Val "hi"

let text (g: G<string>) : string =
    match g with
    | Val v -> v
    | Num _ -> "num"

printfn "%s" (text s)
printfn "%b" (s = Val "hi")
