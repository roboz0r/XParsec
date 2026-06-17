namespace Vesper

#nowarn "42"

// Runtime body of the cell type whose contract sits in `core-types.fsi`.
// Captured-mutable promotion rewrites escaping `let mutable` bindings into `Ref<'T>`,
// so the type must be a real `TypeDefinition` in `Vesper.Core.dll`.

[<ReferenceEquality>]
[<NoComparison>]
type Ref<'T> = { mutable contents: 'T }
