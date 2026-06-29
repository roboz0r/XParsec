namespace Vesper

// JS-only structural-comparison runtime entry — moved out of the shared `comparison.fsi`
// because it is NOT part of the CLR contract: the CLR `< > <= >=` bodies use
// `Comparer<^T>.Default` inline (comparison.fs) and never reference this, so a CLR-visible
// `val` with no CLR `.fs` body was an over-declaration (FS0240-style gap). On the JS
// backend the `< > <= >=` base arms delegate to it for a non-primitive operand; the body
// is imported from `Vesper.Comparison.mjs` (a runtime asset, not a `.fs`). Listed in the
// manifest's `files-js`, so the JS front end resolves it while the CLR contract does not.
// The ordering analogue of Vesper.Core's `StructuralRuntime.structuralEquals`.
[<AutoOpen>]
module ComparisonRuntime =

    /// Structural three-way comparison of two values — the runtime entry the JS
    /// `< > <= >=` aggregate bases call for a non-primitive operand. Returns a sign
    /// (-1 / 0 / 1) the bases test against 0. JS body: `Vesper.Comparison.mjs`'s curried
    /// `structuralCompare` (a shape-keyed walk; consistent with Vesper.Core's
    /// `structuralEquals` by construction, so equal values compare 0).
    val structuralCompare: x: 'T -> y: 'T -> int when 'T: comparison
