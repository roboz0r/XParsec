// A body-local `let` is generalised at its own `let`, so one local serves two types in
// the same body. F# forbids explicit typars on a local (FS0665), so the polymorphism is
// always implicit. No `inline` here: this is the control for `inline/inline-local-poly.fs`,
// so a failure here is generalisation or closure emission, not splicing.

// The local used at two types, the outer function monomorphic.
let twoUses (x: int) =
    let g y = y
    (g x, g "a")

// The local's own typar is generalised while the outer function's `'b`, captured through
// `x`, is not.
let entangled (x: 'b) =
    let g y = (y, x)
    (g 1, g "c")

// The local used at ONE type: the shape that does not need a generic local at all.
let oneUse (x: int) =
    let g y = y
    (g x, g 2)

match twoUses 1 with
| a1, a2 -> printfn "%d %s" a1 a2

match entangled "outer" with
| (c1, c1x), (c2, c2x) -> printfn "%d %s %s %s" c1 c1x c2 c2x

match oneUse 3 with
| o1, o2 -> printfn "%d %d" o1 o2
