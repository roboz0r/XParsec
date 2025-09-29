// Triple-dollar with single and double curly braces in the text
let a = 40
let s = $$$"""This string has {a} set of curlies, and {{a}} set of curlies, and an interpolated value: {{{a}}}"""
printfn "%s" s