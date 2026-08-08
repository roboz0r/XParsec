// The same duality for an inline binding that has a DECLARING MODULE. That is the shape
// with an exportable identity, so it is the one a consumer can splice — and it is also
// the one both backends place on a named module class rather than the anonymous program one, so
// "emitted as an ordinary module function" means something different here than it does
// for a top-level binding, and has to produce the same numbers.
module Scale =

    let inline twice x = x * 2

    // An inline binding calling an inline SIBLING: the sibling reference is what a
    // published template has to rewrite to an exportable identity, and what the emitted
    // ordinary function resolves through the module's own bound variable table. The two forms of
    // the same body must agree, and only a program that runs it can say so.
    let inline quadruple x = twice (twice x)

printfn "%d" (Scale.twice 21)
printfn "%d" (Scale.quadruple 10)

let q = Scale.quadruple
printfn "%d" (q 3)
