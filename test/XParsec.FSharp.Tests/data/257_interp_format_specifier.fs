module Test

let idx = 42
let name = "hello"

// Multiple format specifiers with escaped quotes in one string
let c = $"    %i{idx}[\"%s{name}\"]"
