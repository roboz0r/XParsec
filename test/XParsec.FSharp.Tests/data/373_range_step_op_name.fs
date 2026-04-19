// The step-range operator name `.. ..` is lexed as two separate `OpRange` tokens
// because the greedy operator scan can't span whitespace. Used only as an operator
// name inside parens (never infix) — e.g. `let inline (.. ..) start step finish = ...`
// in FSharp.Core/prim-types.fs. Arbitrary trivia (whitespace, newlines, comments)
// between the two `..` pieces is legal F#.

module Test

let inline (.. ..) start step finish = 0

let inline (.. // line ending comment
               ..) start step finish = 0
