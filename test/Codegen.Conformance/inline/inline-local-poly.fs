// A body-local `let` inside an inline function is generalised at its own `let`, so one
// local serves two types inside the same body. F# forbids explicit typars on a local
// (FS0665), so the local's polymorphism is always implicit, and a splice has to
// re-generalise the local rather than instantiate it once for the whole body. Each
// program below is accepted by F# and prints the components of a two-typed pair.

// The local used at two types, the outer function monomorphic.
let inline twoUses (x: int) =
    let g y = y
    (g x, g "a")

// An annotated `'a` on a local is a generalisable placeholder, not a declaration.
let inline annotated (x: int) =
    let g (y: 'a) : 'a = y
    (g x, g "b")

// The local's own typar is generalised while the outer function's `'b`, captured through
// `x`, is not: `g`'s two uses agree on `x`'s type and differ on `y`'s.
let inline entangled (x: 'b) =
    let g y = (y, x)
    (g 1, g "c")

// The inline body served across a module boundary.
module M =
    let inline served (x: int) =
        let g y = y
        (g x, g "e")

module N =
    let consume () = M.served 5

match twoUses 1 with
| a1, a2 -> printfn "%d %s" a1 a2

match annotated 2 with
| b1, b2 -> printfn "%d %s" b1 b2

match entangled "outer" with
| (c1, c1x), (c2, c2x) -> printfn "%d %s %s %s" c1 c1x c2 c2x

match N.consume () with
| e1, e2 -> printfn "%d %s" e1 e2
