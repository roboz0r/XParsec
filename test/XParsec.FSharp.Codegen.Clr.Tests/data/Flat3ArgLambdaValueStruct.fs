let apply3 (f: 'TF when 'TF :> Fun<int, int, int, int>) (a: int) (b: int) (c: int) : int = f.Invoke(a, b, c)
printfn "%d" (apply3 (fun x y z -> x + y + z) 20 22 24)
