let apply4 (f: 'TF when 'TF :> Fun<int, int, int, int, int>) (a: int) (b: int) (c: int) (d: int) : int = f.Invoke(a, b, c, d)
printfn "%d" (apply4 (fun w x y z -> w + x + y + z) 10 20 30 40)
