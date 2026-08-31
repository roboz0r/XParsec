namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis

[<AutoOpen>]
module internal TyparMarkers =

    /// A declaring type's own typars as self-describing nodes, in declaration order:
    /// position `i` encodes as `!i`. This is the instantiation that names a generic type
    /// from inside its own bodies (`Box\`1<!0>`).
    let declaringMarkers (count: int) : FrozenType list =
        [ for i in 0 .. count - 1 -> FTTypar(TyparAxis.Declaring, i) ]
