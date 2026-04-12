module Test

// Type annotation suffix on lambda body
// F# grammar: FUN atomicPatterns RARROW typedSeqExprBlock

let f = fun x -> x + 1 : int

let g =
    fun x ->
        let y = x + 1
        y * 2 : int
