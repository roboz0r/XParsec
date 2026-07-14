// A `let rec` in the preamble: the function-valued `let` is in scope in its own body, and
// stays callable from a member afterwards. Both the ctor-time call (`value`) and the
// member-time call go through whatever storage the backend chose for it.
type Factorial(n: int) =
    let rec fact k = if k <= 1 then 1 else k * fact (k - 1)

    let value = fact n
    member this.Value() = value
    member this.Of(k: int) = fact k

let f = Factorial(5)
printfn "%d" (f.Value())
printfn "%d" (f.Of 6)
