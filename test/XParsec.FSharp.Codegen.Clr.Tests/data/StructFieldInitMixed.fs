[<Struct>]
type Iter =
    val mutable Cur: int
    val mutable Started: bool
    new(c: int) = { Cur = c; Started = false }
    member this.IsStarted() = this.Started
