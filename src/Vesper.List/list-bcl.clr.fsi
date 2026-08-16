namespace Vesper.Collections

// CLR-only, and in a file of its own for that reason: the RHS resolves through the backend's
// platform metadata, which the js contract has none of.

/// <summary>An abbreviation for the CLI type <see cref="T:System.Collections.Generic.List`1"/></summary>
type ResizeArray<'T> = System.Collections.Generic.List<'T>
