// Multiline triple-dollar string
let g = "G"

let s =
    $$$"""
This is another multiline string.
It has {one} and {{two}} sets of curlies.
And an interpolated value: {{{g}}}
"""

printfn "%s" s
