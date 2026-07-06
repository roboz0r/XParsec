[<Struct>]
type Box<'T>(value: 'T) =
    member this.Value = value

let b = Box<int>(42)
