// Unary negation (`~-`) per width. A literal `-5` is a negative CONSTANT, not the
// operator, so each width routes through a function parameter to reach the
// contract's own `neg` clause.
let negI (x: int) = -x
let negL (x: int64) = -x
let negF (x: float) = -x
let negG (x: float32) = -x
let negS (x: sbyte) = -x
let negH (x: int16) = -x

printfn "%d" (negI 5)
printfn "%d" (negI (-5))
printfn "%O" (negL 1000000000000L)
printfn "%f" (negF 2.5)
printfn "%O" (negG 2.5f)
printfn "%d" (int (negS 100y))
printfn "%d" (int (negH 30000s))
