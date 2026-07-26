// A TOP-LEVEL `let inline` is two things at once, and this pins both halves against
// each other: F# emits it as an ordinary module function AND splices its body at the
// use sites that can take one, so a saturated call, a call through another inline, and
// a first-class reference must all compute the same number.
//
// It is also the program that catches an inline binding emitted BADLY. The emitted
// function is reachable from the module surface even when every use here splices, so a
// malformed one is not dead weight: on JS it is a module-level definition whose dangling
// reference fails to load before a single line is printed, and on the CLR a static method
// whose body cannot be built.
let inline double x = x * 2

let inline applySum f x y = f (x + y)

printfn "%d" (double 21)
printfn "%d" (applySum double 20 1)

// A first-class reference — no call spine to saturate, so the compiler has to produce a
// function VALUE for the binding rather than a spliced expression.
let d = double
printfn "%d" (d 7)
