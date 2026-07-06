[<Struct>]
type Counter =
    val mutable N: int
    member this.Get() = this.N

    static member Fresh() : int =
        let c = Counter()
        c.Get()
