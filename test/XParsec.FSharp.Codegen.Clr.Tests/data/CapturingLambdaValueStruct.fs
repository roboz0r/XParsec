let apply (f: 'TF when 'TF :> Fun<int, int>) (x: int) : int = f.Invoke x
let mk (n: int) (x: int) : int = apply (fun y -> y + n) x
printfn "%d" (mk 1 41)
