[<Struct>]
type Box<'T>(value: 'T) =
    member this.Get() = value
