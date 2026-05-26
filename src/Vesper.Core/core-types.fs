namespace Vesper

#nowarn "42"

// Impl side: the runtime body of the cell type whose contract sits in
// `core-types.fsi`. Captured-mutable promotion (Passes/RefCellPromotion)
// rewrites a `let mutable` whose binding escapes into a `Ref<'T>` cell, so the
// type must exist as a real `Vesper.Ref\`1` `TypeDefinition` in
// `Vesper.Core.dll` (records-handoff.md Phase 2 follow-up F1) — not a
// synthesised-local prepend in every consuming PE. The `[<ReferenceEquality>]`
// + `[<NoComparison>]` attributes match the `.fsi`; the C-Attr verdict picks
// up `Reference` from `[<ReferenceEquality>]` (two distinct cells are never
// equal — operators-plan.md O8).

[<ReferenceEquality>]
[<NoComparison>]
type Ref<'T> = { mutable contents: 'T }
