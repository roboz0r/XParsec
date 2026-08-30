namespace Vesper

// Its own file, after `compiler-attributes`: `[<AutoOpen>]` resolves against preceding
// signature files, so the module carrying it cannot sit in the attribute's declaring file.

[<AutoOpen>]
module CompilerMarkers =

    let inline nativeOnly<'T> : 'T = (# "$use-import-attribute" : 'T #)
