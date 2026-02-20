// Multiline double-dollar string
let f = "F#"

let s =
    $$"""
This is a multiline string.
It contains {curly braces}.
And an interpolated value: {{f}}
"""

printfn "%s" s
