let apply (f: int -> int) (x: int) : int = f x
let a = apply (fun x -> x + 1) 41
let b = apply (fun x -> x + 1) 9
printfn "%d" (a + b)
