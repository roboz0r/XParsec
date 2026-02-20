// Escaping curly braces in a single-dollar interpolated string
let value = 42
let s = $"To show curlies, use two: {{not interpolated}}, but this is: {value}."
printfn "%s" s
