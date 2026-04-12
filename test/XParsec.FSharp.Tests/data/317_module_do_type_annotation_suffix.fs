module Test

// Type annotation suffix on module-level `do` binding body
// F# grammar: moduleDoBinding: opt_attributes DO typedSeqExprBlock

do printfn "hi" : unit
