// The preamble runs in DECLARATION order, once per construction: each `do` sees the
// `let`s above it and none below. The side effects are printed, so a backend that hoisted
// the `let`s over the `do`s (or ran the `do`s once, at type level) reorders this output.
type Ordered(n: int) =
    let a = n + 1
    do printfn "ctor a=%d" a
    let b = a * 3
    do printfn "ctor b=%d" b
    member this.B() = b

let o1 = Ordered(1)
printfn "b=%d" (o1.B())
let o2 = Ordered(5)
printfn "b=%d" (o2.B())
