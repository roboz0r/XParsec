// Integer division by zero must FAULT, not produce a value — and a fault is a
// conformance property exactly as much as an answer is. This program has no golden:
// it declares its expected runtime error in the manifest instead.
//
// `1 / 0` is the int32 row, the one width whose clause already throws on the CLR (CIL
// `div` raises `DivideByZeroException`). The width-by-width family belongs here too,
// but only once every clause can throw.
printfn "%d" (1 / 0)
