[<Struct>]
type Counter =
    val mutable N: int
    member this.Bump() = this.N <- this.N + 1
    member this.Get() = this.N

let c = Counter()
