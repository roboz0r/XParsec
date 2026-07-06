[<Struct>]
type Leaf =
    val mutable N : int
    new(n: int) = { N = n }
    member this.Bump() : int =
        this.N <- this.N + 1
        this.N
[<Struct>]
type Mid =
    val mutable L : Leaf
    new(l: Leaf) = { L = l }
[<Struct>]
type Top =
    val mutable M : Mid
    new(m: Mid) = { M = m }
    member this.Step() : int = this.M.L.Bump()
let run () =
    let mutable t = Top(Mid(Leaf(10)))
    let a = t.Step()
    let b = t.Step()
    printfn "%d %d" a b
run ()
