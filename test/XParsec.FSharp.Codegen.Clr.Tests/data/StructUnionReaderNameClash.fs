// A case named `X_0` beside a payload-bearing `X`, which F# accepts. `X`'s first field reads
// through `Get_X_0` and `X_0`'s view through `GetPayload_X_0`, so the two reader families
// stay distinct however a case is named.
[<Struct>]
type Readers =
    | X of a: int * b: int
    | X_0 of c: string

let score (r: Readers) : int =
    match r with
    | X(a, b) -> a + b
    | X_0 c -> c.Length

let p = X(3, 4)
let q = X_0 "hello"

printfn "%d" (score p)
printfn "%d" (score q)
printfn "%b" (p = X(3, 4))
printfn "%b" (p = q)
