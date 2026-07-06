[<Struct>]
type Counter =
    val mutable N: int
    member this.Bump() = this.N <- this.N + 1
    member this.Get() = this.N

    static member Run() : int =
        let c = Counter()
        c.Bump()
        c.Bump()
        c.Get()
