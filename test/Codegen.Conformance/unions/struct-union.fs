// A [<Struct>] union. On the CLR it emits as a sealed System.ValueType-based value type;
// on JS, which has no value-type concept, it emits as the same reference object a plain
// union does. Both backends must produce every result below.
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

let s0 = Empty
let s1 = Point 3
let s2 = Pair(4, 5)

printfn "%d" (describe s0)
printfn "%d" (describe s1)
printfn "%d" (describe s2)
printfn "%b" (s1 = Point 3)
printfn "%b" (s1 = Empty)
printfn "%b" (Point 3 = s1)
printfn "%b" (s2 = Pair(4, 5))
printfn "%b" (s2 = Pair(4, 6))
