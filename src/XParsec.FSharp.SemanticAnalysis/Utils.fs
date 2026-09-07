namespace XParsec.FSharp.SemanticAnalysis

[<AutoOpen>]
module internal RefEquality =
    let inline refEq (x: 'T) (y: 'T) = System.Object.ReferenceEquals(x, y)
