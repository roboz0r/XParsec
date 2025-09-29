// Regular interpolated string with standard escape sequences
let item = "tab"
let s = $"Here is a newline:\nAnd here is a {item}:\t."
printfn "%s" s