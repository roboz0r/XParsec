[<Struct>]
type Add1 =
    val N : int
    new(n: int) = { N = n }
    interface Fun<int, int> with
        member this.Invoke(x: int) : int = x + this.N
let apply (f: 'TF when 'TF :> Fun<int, int>) (x: int) : int = f.Invoke x
printfn "%d" (apply (Add1 1) 41)
