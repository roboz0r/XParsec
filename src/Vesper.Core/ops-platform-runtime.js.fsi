namespace Vesper

// JS-only runtime entries for the equality family — moved out of the shared
// `ops-platform.fsi` because they are NOT part of the CLR contract: the CLR `=` / `<>`
// / `hash` bodies use `EqualityComparer<^T>.Default` inline and never reference these,
// so declaring them in a CLR-visible signature was an over-declaration (a `val` with no
// CLR `.fs` body — an FS0240-style conformance gap). On the JS backend the `=` / `<>` /
// `hash` base arms DO delegate to these for aggregate operands; the bodies are imported
// from `Vesper.Core.mjs` (a runtime asset, not a `.fs`). Listed in the manifest's
// `files-js`, so the JS front end resolves them while the CLR contract does not carry
// them.
[<AutoOpen>]
module StructuralRuntime =

    /// Structural equality of two values (JS runtime entry for aggregate operands).
    val structuralEquals: x: 'T -> y: 'T -> bool when 'T: equality

    /// Structural hash of a value (JS runtime entry for aggregate operands).
    val structuralHash: obj: 'T -> int when 'T: equality
