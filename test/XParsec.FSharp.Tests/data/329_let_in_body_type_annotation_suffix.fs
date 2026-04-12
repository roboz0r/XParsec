module Test

// Type annotation suffix on inline let-in body expression
// F# grammar: LET bindings IN typedSeqExprBlock

let f () =
    let x = 1 in x + 1 : int

let g () =
    let x = 1
    x + 1 : int
