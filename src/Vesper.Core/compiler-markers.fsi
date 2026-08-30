namespace Vesper

[<AutoOpen>]
module CompilerMarkers =

    /// <summary>The body of an <c>[&lt;Import&gt;]</c>-attributed binding, whose
    /// implementation is the export the attribute declares.</summary>
    val inline nativeOnly<'T> : 'T
