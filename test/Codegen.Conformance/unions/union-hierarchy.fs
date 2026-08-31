// Reference unions across the emitter's regimes: a single-case union (no discriminant),
// a two-case type-tested union, and a four-case tagged union, exercising construction,
// matching and structural equality on both backends.
type Meters = | M of int

type Shape =
    | Dot
    | Line of len: int

type Quad =
    | Q0
    | Q1 of int
    | Q2 of int * int
    | Q3 of int * int * int

let metersValue (m: Meters) : int =
    match m with
    | M v -> v

let describeShape (s: Shape) : int =
    match s with
    | Dot -> 0
    | Line len -> len

let describeQuad (q: Quad) : int =
    match q with
    | Q0 -> 0
    | Q1 x -> x
    | Q2(a, b) -> a + b
    | Q3(a, b, c) -> a + b + c

printfn "%d" (metersValue (M 7))
printfn "%d" (describeShape Dot)
printfn "%d" (describeShape (Line 4))
printfn "%d" (describeQuad Q0)
printfn "%d" (describeQuad (Q1 5))
printfn "%d" (describeQuad (Q2(2, 3)))
printfn "%d" (describeQuad (Q3(1, 2, 3)))
printfn "%b" (M 7 = M 7)
printfn "%b" (M 7 = M 8)
printfn "%b" (Dot = Dot)
printfn "%b" (Line 4 = Line 4)
printfn "%b" (Line 4 = Line 5)
printfn "%b" (Line 4 = Dot)
printfn "%b" (Q0 = Q0)
printfn "%b" (Q2(2, 3) = Q2(2, 3))
printfn "%b" (Q2(2, 3) = Q3(1, 2, 3))
