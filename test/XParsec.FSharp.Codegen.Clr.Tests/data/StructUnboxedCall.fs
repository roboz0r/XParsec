[<Struct>]
type SPoint(x: int, y: int) =
    member this.Sum() = x + y

    static member SumOf() : int =
        let p = SPoint(3, 4)
        p.Sum()
