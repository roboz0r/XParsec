// A `let mutable` is ONE storage location on the instance, shared by the function-valued
// `let` that writes it and the member that reads it. Two calls are what makes that
// observable: a backend that gave the closure its own copy (a ref cell captured by value,
// a field re-initialised per call) prints 6 twice and never accumulates.
type Counter(step: int) =
    let mutable count = 0
    let bump (k: int) = count <- count + k * step

    member this.Bump(k: int) =
        let _ = bump k
        count

let c = Counter(2)
printfn "%d" (c.Bump 3)
printfn "%d" (c.Bump 4)
