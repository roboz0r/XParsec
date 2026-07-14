// A function-valued `let` used FIRST-CLASS: passed out of the class as a value, not just
// applied in place. It closes over the ctor param, so the value that leaves the member has
// to carry the instance with it — a backend that compiled the `let` to a method and only
// knew how to call it directly cannot hand it to `twice`.
let twice (f: int -> int) (x: int) = f (f x)

type Adder(k: int) =
    let add (x: int) = x + k
    member this.Twice(n: int) = twice add n

let a = Adder(3)
printfn "%d" (a.Twice 10)
