// `=` at the primitives the language prescribes equality for. JS does not lower it
// uniformly: `===` at int / bool / float, but a `structuralEquals` call at string.
printfn "%b" (1 = 1)
printfn "%b" (1 = 2)
printfn "%b" ("a" = "a")
printfn "%b" ("a" = "b")
printfn "%b" (true = false)
printfn "%b" (1.5 = 1.5)
