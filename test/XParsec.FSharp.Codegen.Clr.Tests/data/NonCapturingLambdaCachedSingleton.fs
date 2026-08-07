let apply (f: int -> int) (x: int) : int = f x
printfn "%d" (apply (fun x -> x + 1) 41)
