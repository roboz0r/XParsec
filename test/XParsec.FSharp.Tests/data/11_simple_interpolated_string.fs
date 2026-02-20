// Simple single-dollar interpolated string
let name = "world"
let s = $"Hello, {name}!"
printfn "%s" s
let x = 10
let s = $"The value is { task { return x } }"
let s = $"The value is { task { [return x } }"
// TODO: Fix nested braces in expressions
// Parens, brackets and braces inside expressions must be balanced
// Probably need a stack-based approach to handle this correctly
// Also need to handle .NET format specifiers like below
let s = $"The value is {1.2:F2 + 3.4:F4}"

let f = task { return x } }