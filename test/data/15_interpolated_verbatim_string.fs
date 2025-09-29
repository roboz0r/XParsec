// Simple interpolated verbatim string ($@"...")
let path_component = "data"
let s = $@"C:\Users\{path_component}\file.txt"
printfn "%s" s