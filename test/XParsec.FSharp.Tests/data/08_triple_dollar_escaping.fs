// Escaping curly braces in a triple-dollar string
let e = 60
let s = $$$"""This will print literal double curlies: {{{{{e}}}}}"""
printfn "%s" s
