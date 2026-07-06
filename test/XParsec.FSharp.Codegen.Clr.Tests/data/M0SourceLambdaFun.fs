let apply (f: 'TF when 'TF :> Fun<int, int>) (x: int) : int = f.Invoke x
printfn "%d" (apply (fun x -> x + 1) 41)
