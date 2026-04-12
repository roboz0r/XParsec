module Test

// Per-element `as` aliases inside an unparenthesized tuple pattern
// (let-value binding head). The outer `as` form is covered by test 313.
//
// Pratt semantics match the F# compiler: `as` at bp 3 is looser than `,`
// at bp 21, but the first tuple element is parsed at the outer minBinding
// so it can still consume `as`. In `s as t, u as v`, the second `as v`
// lies at the outer level after the tuple is built and therefore aliases
// the whole tuple, not just `u` (verified against dotnet fsi).

let f () =
    let a as x, b = 1, 2
    let s as t, u as v = 100, 200
    a + b + s + u
