// Escaping curly braces in a double-dollar string
let d = 50
let s = $$"""This will print literal single curlies: {{{d}}}"""
printfn "%s" s