// A measured float is a float at runtime: the measure is checked by the front end and
// erased by every backend, so a measured value prints exactly as its carrier does, and
// measured arithmetic splices the carrier's operator.
[<Measure>]
type m

[<Measure>]
type s

let d = 100.0<m>
let t: float<s> = 8.0<s>
let v: float<m / s> = d / t
printfn "%f" d
printfn "%f" t
printfn "%f" v
