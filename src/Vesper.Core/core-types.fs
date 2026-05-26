namespace Vesper

#nowarn "42"

// Runtime body of the cell type whose contract sits in `core-types.fsi`.
// Captured-mutable promotion (Passes/RefCellPromotion) rewrites a `let mutable`
// whose binding escapes into a `Ref<'T>` cell, so the type must exist as a
// real `Vesper.Ref\`1` `TypeDefinition` in `Vesper.Core.dll` — every consuming
// PE resolves it through the codegen's external-record path, not a
// synthesised-local prepend. `[<ReferenceEquality>]` keeps two distinct cells
// from comparing equal (operators-plan.md O8).

[<ReferenceEquality>]
[<NoComparison>]
type Ref<'T> = { mutable contents: 'T }
