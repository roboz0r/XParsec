module Test

// use ptr = fixed array
// From SemanticClassificationKey.fs line 60

open System

let writeBytes (data: byte[]) =
    use ptr = fixed data
    ignore ptr
