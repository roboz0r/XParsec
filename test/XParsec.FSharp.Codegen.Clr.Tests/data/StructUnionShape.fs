// A `[<Struct>]` union: constructs through by-value factories, matches, and compares
// by structural equality. On the CLR it must emit as a `System.ValueType`-based sealed
// value type; the zero value is the tag-0 case (`Empty`).
[<Struct>]
type Shape =
    | Empty
    | Point of x: int
    | Pair of a: int * b: int

let describe (s: Shape) : int =
    match s with
    | Empty -> 0
    | Point x -> x
    | Pair(a, b) -> a + b

let s1 = Point 3
let zero: Shape = Unchecked.defaultof<Shape>

printfn "%d" (describe Empty)
printfn "%d" (describe s1)
printfn "%d" (describe (Pair(4, 5)))
printfn "%b" (s1 = Point 3)
printfn "%b" (s1 = Empty)
printfn "%d" (describe zero)
printfn "%b" (zero = Empty)
