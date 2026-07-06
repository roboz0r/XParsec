[<Struct>]
type SPoint(x: int, y: int) =
    member this.Sum() = x + y

    static member SumOf(a: int, b: int) : int =
        let p = SPoint(a, b)
        p.Sum()
