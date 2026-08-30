namespace Vesper

[<AutoOpen>]
module ComparisonRuntime =

    [<Import("structuralCompare", "./Vesper.Comparison.mjs")>]
    let structuralCompare (x: 'T) (y: 'T when 'T: comparison) : int = nativeOnly
