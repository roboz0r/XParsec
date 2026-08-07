let apply2 (f: 'TF when 'TF :> Fun<int, int, int>) (a: int) (b: int) : int = f.Invoke(a, b)
printfn "%d" (apply2 (fun x y -> x + y) 20 22)
