module Test158

let f ([<System.ParamArray>] args: int array) = args.Length

let g ([<System.Obsolete>] x: int) ([<System.Obsolete>] y: int) = x + y

type Container([<System.ParamArray>] items: obj array) =
    member _.Count = items.Length
