[<Struct>]
type Add1 =
    interface Fun<int, int> with
        member _.Invoke(x: int) : int = x + 1
let apply (f: 'TF when 'TF :> Fun<int, int>) (x: int) : int = f.Invoke x
printfn "%d" (apply (Add1()) 41)
