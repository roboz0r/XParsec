namespace XParsec.FSharp.SemanticAnalysis

[<AutoOpen>]
module internal Helpers =

    open System.Diagnostics

    let unreachable (msg: string) = raise (new UnreachableException(msg))

[<AutoOpen>]
module internal RefEquality =
    let inline refEq (x: 'T) (y: 'T) = System.Object.ReferenceEquals(x, y)
