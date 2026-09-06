// A module-level tuple binding generalises each bound name on its own, as `fsc` does:
// `f : 'a -> 'a` and `g : 'b -> 'b` are independent, so one program uses each at two
// types. The binding has no name of its own to key a scope on, so each bound name carries
// its own module-function scope.
let (f, g) = ((fun x -> x), (fun y -> y))

let a = f 1
let b = f "a"
let c = g true
let d = g 'x'
printfn "%d %s %b %c" a b c d
