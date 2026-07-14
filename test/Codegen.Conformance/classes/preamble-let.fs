// An instance `let` reading a primary-ctor parameter, read back through a member.
// The initialiser can only be evaluated after the ctor params are in place, so this
// program dies (or reads zero) on any backend that runs the preamble too early.
type Boxed(n: int) =
    let m = n + 1
    member this.M() = m * 10

let b = Boxed(4)
printfn "%d" (b.M())
