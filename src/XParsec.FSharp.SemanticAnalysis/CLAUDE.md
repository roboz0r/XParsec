# XParsec.FSharp.SemanticAnalysis

## Levelling

A pass here must not know about a backend. When a gate appears to know about FSharp.Core, a
`.NET` format string or any other CLR artifact, that is a mislevelling to fix rather than code
to relocate. The decision a Freeze gate makes ("is this faithfully representable as a structured
node?") is target-neutral, because both backends defer the same cases, so it is semantic and
belongs here. The target dialect projection belongs only in the backend that has that target.

When a shared classifier is needed upstream of `Codegen.Common` but lives there, move the
target-neutral classification up into this project and push the dialect projection down into the
specific backend, rather than keeping a duplicate gate upstream.

## New type concepts

Prefer a real named intrinsic type plus operators and name-recognizers over a new
`SemType`/`FrozenType` DU case with special unifier behaviour. `undefined` ships as a JS-only
intrinsic rather than a name special-case in `canonName`, and `any` ships as an opaque
`type dynamic = (# "any" #)` with all behaviour in the `?`/`?<-` operators. A real type is
writable in Vesper source, rides unify-by-name and `applyDefaults`, needs no new DU threading
through the child-walk skeletons, and keeps special behaviour off the hot unify path.

Keep F# type discipline; do not import TypeScript's implicit conversions. `dynamic` has no
assignability edges: entry is explicit, exit is explicit, and `.member` on it is an error.

## Working on the unifier and Infer

When one fix surfaces several interacting issues, stop patching and write systematic isolation
tests first, then a plan that classifies root causes and sequences the fixes. Expect the tests
to go red; the red surface is the deliverable. A test is a minimal synthetic program through the
real pipeline asserting no error-severity diagnostics, which covers both a `unify` mismatch and
a leaked free type variable. Pin a known gap with `ptest` and the gap quoted in the name.

## Prefer the upstream fix to a per-target split

A `.clr.fs`/`.js.fs` split asserts that the two targets genuinely differ, and twice that
assertion was false while the real cause sat upstream. Before writing a second copy, state what
specifically the other target cannot do and verify it. Where the blocker is a front-end or
backend gap, size the fix and offer it rather than defaulting to the split: a bounded compiler
fix beats a per-target duplicate, which rots and hides the defect. Confirm causality with a
negative control, disabling the fix to show the original error returns.
