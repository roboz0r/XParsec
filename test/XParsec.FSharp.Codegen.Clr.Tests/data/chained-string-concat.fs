let join3 (a: string) (b: string) (c: string) : string = a + b + c
let surround (s: string) : string = "(" + s + ")"
printfn "%s" (join3 "a" "b" "c")
printfn "%s" (surround "x")
