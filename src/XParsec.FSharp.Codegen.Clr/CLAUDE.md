# XParsec.FSharp.Codegen.Clr

## Assert on the emitted IL

Reflection over a loaded assembly answers what the runtime tolerates, not what was written. The
emitter's real invariants are structural: row order is the table row, and field and method
handles are prefix-summed predictions. `verifyTypeHandle` checks handles against predictions but
cannot say the metadata is well-formed.

Where a change makes the emitter's layout carry meaning, assert on the metadata directly through
the `MetadataStructure` helpers (a `MetadataReader` over the PE: rows, flags, table order, range
contiguity) and treat that as part of the deliverable. Reach for it ahead of loading the
assembly when the question is "did we emit the right metadata" rather than "does it run".

## Loading an emitted PE

Load through `TestHelpers.loadAssembly`, which uses a fresh `AssemblyLoadContext` and
`LoadFromStream`, rather than `Assembly.Load bytes`. Two `Assembly.Load(byte[])` calls on the
same bytes produce two distinct assemblies, so a reflection round-trip that builds a value via
one and passes it into the other throws `"Object of type X cannot be converted to type X"`.

A reflection round-trip must load once and reflect every member and construct every value
through that single `Assembly`. A custom-context load still resolves FSharp.Core and
`Vesper.Printf` through the default-context fallback, so printf-bearing programs still run.
