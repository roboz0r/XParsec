module Test

// Type annotation as suffix on let-bound expression: let x = expr : type
// From local.fs line 1155

let inline sub startIndex count (array: 'T array) =
    let res = Array.zeroCreate count : 'T array
    res
