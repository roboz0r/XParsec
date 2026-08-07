let apply (f: int -> int) (x: int) : int = f x
let outer (n: int) (x: int) : int = apply (fun y -> y + n) x
printfn "%d" (outer 1 41)
