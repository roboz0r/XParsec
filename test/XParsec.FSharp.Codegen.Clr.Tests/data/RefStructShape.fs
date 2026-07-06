[<Struct; IsByRefLike>]
type RPoint(x: int, y: int) =
    member this.X = x

let p = RPoint(3, 4)
