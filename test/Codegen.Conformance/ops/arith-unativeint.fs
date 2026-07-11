// unativeint — the unsigned pointer-width integer; the `nativeint` asymmetry
// again, with the unsigned `/` and `%` clauses.
let addN (a: unativeint) (b: unativeint) : unativeint = a + b
let subN (a: unativeint) (b: unativeint) : unativeint = a - b
let mulN (a: unativeint) (b: unativeint) : unativeint = a * b
let divN (a: unativeint) (b: unativeint) : unativeint = a / b
let remN (a: unativeint) (b: unativeint) : unativeint = a % b

printfn "%d" (int (addN 10un 20un))
printfn "%d" (int (subN 30un 10un))
printfn "%d" (int (mulN 6un 7un))
printfn "%d" (int (divN 100un 7un))
printfn "%d" (int (remN 100un 7un))
