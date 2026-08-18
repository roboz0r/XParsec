namespace Vesper

#nowarn "42"

[<ReferenceEquality>]
[<NoComparison>]
type Ref<'T> = { mutable contents: 'T }

and 'T ref = Ref<'T>
