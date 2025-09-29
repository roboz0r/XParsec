// Triple-dollar with format specifier and percent signs in text.
let c = 789
let s = 
    $$$"""This is a test with a % sign and a %%%d{{{c}}}"""
printfn "%s" s