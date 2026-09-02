// A generic `[<Struct>]` union with an unmanaged case: the `_data` overlay and its case
// data struct are non-generic types nested in the generic union, so explicit layout stays
// off the generic type, while `Payload` redeclares `'T` for the exact slot.
[<Struct>]
type GShape<'T> =
    | Val of v: 'T
    | Pt of x: int * y: int

let sum (g: GShape<string>) : int =
    match g with
    | Val v -> v.Length
    | Pt(x, y) -> x + y

let a: GShape<string> = Val "abc"
let b: GShape<string> = Pt(4, 5)

printfn "%d" (sum a)
printfn "%d" (sum b)
printfn "%b" (b = Pt(4, 5))
printfn "%b" (a = b)
