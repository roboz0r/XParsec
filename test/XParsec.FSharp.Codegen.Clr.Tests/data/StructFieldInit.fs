[<Struct>]
type Pair =
    val mutable A: int
    val mutable B: int
    new(a: int, b: int) = { A = a; B = b }
    member this.Sum() = this.A + this.B

    static member SumOf(a: int, b: int) : int =
        let p = Pair(a, b)
        p.Sum()
