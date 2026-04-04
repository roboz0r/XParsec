module Test

// DU case field access via .( :: ).N syntax
// From seqcore.fs line 461: cons.( :: ).1 <- tail
// After the first dot, '( :: )' is the DU case name in parens (special F# internal syntax)
// and '.1' accesses the second field (cons cell tail)

let inline SetFreshConsTail (cons: 'T list) (tail: 'T list) =
    cons.( :: ).1 <- tail
