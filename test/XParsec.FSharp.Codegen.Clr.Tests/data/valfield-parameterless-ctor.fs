type S =
    val mutable X: int
    val mutable Y: int
    new() = { X = 42; Y = 7 }
    member this.Sum = this.X + this.Y

let s = S()
printfn "%d" s.X
printfn "%d" s.Y
