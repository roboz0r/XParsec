// Unary negation on an UNSIGNED width is not defined, and must be REJECTED — on every
// backend, for the same reason. `-x` on a `byte` has no result a byte can hold: the
// mathematical negation of 200 is not in 0..255, and every candidate a target could emit
// (JS's `-200`, CIL's `neg` on the int32 stack, a mod-256 wrap to 56) is a different
// value, none of them F#'s. F# itself carries no clause for it either — FSharp.Core's
// `UnaryNegationDynamic` lists the signed widths only.
//
// So neither operator contract has an unsigned `(~-)` clause, `-x` falls to the SRTP
// trait call in the base, `byte` has no `static member (~-)`, and the program is a
// compile error. This is the third conformance state — not a value, not a fault, a
// REJECTION — pinned here for the operator that used to yield 56 on JS and -200 on the
// CLR without either being wrong on paper.
let negB (x: byte) = -x
ignore negB
