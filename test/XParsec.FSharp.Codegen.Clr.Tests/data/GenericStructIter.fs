[<Struct>]
type Cell<'T> =
    val mutable Item: 'T
    val mutable Started: bool
    new(x: 'T) = { Item = x; Started = false }
    member this.Get() = this.Item
