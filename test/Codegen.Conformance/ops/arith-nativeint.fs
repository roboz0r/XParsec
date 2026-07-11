// nativeint — a pointer-width integer. It has a CLR representation
// (`System.IntPtr`) and no decided JS one, so this program is the asymmetry the
// manifest states: it RUNS on a target that can represent the width, and must be
// REJECTED on one that cannot.
let addN (a: nativeint) (b: nativeint) : nativeint = a + b
let subN (a: nativeint) (b: nativeint) : nativeint = a - b
let mulN (a: nativeint) (b: nativeint) : nativeint = a * b
let divN (a: nativeint) (b: nativeint) : nativeint = a / b
let remN (a: nativeint) (b: nativeint) : nativeint = a % b

printfn "%d" (int (addN 10n 20n))
printfn "%d" (int (subN 10n 20n))
printfn "%d" (int (mulN 6n 7n))
printfn "%d" (int (divN 100n 7n))
printfn "%d" (int (remN 100n 7n))
