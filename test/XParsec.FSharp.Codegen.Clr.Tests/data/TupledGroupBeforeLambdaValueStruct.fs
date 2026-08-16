let applyAfterPair (a: int, b: int) (f: 'TF when 'TF :> Fun<int, int>) : int = f.Invoke(a + b)
printfn "%d" (applyAfterPair (20, 21) (fun x -> x + 1))
