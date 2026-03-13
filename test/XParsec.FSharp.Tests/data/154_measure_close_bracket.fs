// Measure annotation `1<_>` immediately before `]` causes the lexer
// to fuse `>]` into a single KWRAttrBracket token, preventing the
// parser from closing both the measure and the indexer.
let x = arr.[i + 1<_>]
