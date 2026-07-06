[<Struct>]
type SPoint(x: int, y: int) =
    member this.X = x

    static member XOf() : int =
        let p = SPoint(5, 6)
        p.X
