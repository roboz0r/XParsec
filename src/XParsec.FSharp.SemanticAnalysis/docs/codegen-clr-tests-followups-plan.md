# Codegen.Clr.Tests follow-ups — plan

Working document. Ephemeral: delete it when the work lands.

Raised by the comment-hygiene sweep of `test/XParsec.FSharp.Codegen.Clr.Tests`. The sweep itself
is complete. Of the defect and duplication items it raised (A1–A58, B1–B10), everything that
could be fixed without a design discussion landed on 2026-09-05 in six commits, each a
file-disjoint batch, plus one harness-adoption follow-up. This document now holds only the
items that were deferred because they need a decision, and the findings those fixes surfaced.

Line numbers are deliberately absent unless they pin a specific declaration — they rot.

---

# Deferred: needs a design decision

## A26. Is a top-level `let inline` exportable across files?

`InlineFreezeThawTests.fs` documented a module-held `let inline` as the ONLY shape with a
declaring container chain, hence the only exportable identity, and a top-level one as
spliceable within its own file only. `CrossFileTests.fs`'s test *"two files run: file 2 calls
and EXPANDS file 1's top-level bindings"* does exactly that, and passes. The doc was deleted.

Establish whether the publish boundary still refuses any shape, and state the surviving rule
once, somewhere enforceable.

## A27. `asSymbolScheme` passes a hand-written string naming its own call site

`InlineFreezeThawTests.asSymbolScheme` calls
`FrozenTypeBridge.localTyparInTemplate "InlineFreezeThawTests.asSymbolScheme"` — a key that
drifts silently on rename. Already recorded upstream in
`semantic-analysis-followups-plan.md`; noted here as a second consumer of whatever replaces it.

## A55. Does a bare constructor application resolve by written arity?

`ArityOverloadedClassTests.fs` holds both sides. Its deleted header called a bare `Box(…)`
across two arities an unresolved hazard, and the `Box` test writes explicit
`new Box<…>(…)` accordingly — while the last test in the same file writes a bare `Foo(1, 2)`
against a class `Foo<'A,'B>` and a union `Foo<'A>` and expects it to type-check.

Either bare application does resolve by written arity, and the `new` spelling is unnecessary,
or the last test passes for some other reason. The surviving comment is phrased neutrally
pending a verdict.

## B1. ALC placement policy wants to be a type, not six comment blocks

Six blocks in `TestHelpers.fs` (`PackageLoadContext`, the `buildPackage` Printf special case,
`vesperPrintfDll`, the ALC-separable header, `PrintfLoadContext`, `packageAlcPrintf`, plus
the `vesperStructuralPrintMethod` prologue) exist to restate one invariant: *every
participant in a run must resolve `Vesper.Core` to the same loaded assembly*.

The file has **five** distinct load contexts — Default, `packageAlc`, a throwaway per
contract-only package, `PrintfLoadContext`, and a per-engine/per-fixture non-collectible one
(now `compileSelfHostAssembly`) — and the rule for which one a given DLL belongs in lives only
in prose, with `manifest.Name = "Vesper.Printf"` as an inline string test inside
`buildPackage`.

Candidate: a small module owning the placement decision — a
`Placement = Default | PackageRegistry | Throwaway | Dedicated of name` returned by one
function keyed on package name.

## B2. The `name$<offset>` emitted-name scheme is re-implemented as a string prefix test

`ClassTests.fs` opens with `topLevelNameMatches`, which re-implements the emitted
metadata-name scheme (`x` → `x$<offset>`, so a shadowed `let x` stays a distinct row) as
`emitted = source || emitted.StartsWith(source + "$")`. A named type for an emitted top-level
name, constructible from a source name and comparable, removes both spellings.

## B4. `Closure.Repr` and `IsValueStruct` are two fields whose relationship is prose

Found in `src/` while verifying a test comment. `EmitTypes.fs` documents `Repr` as "the
front-end verdict that a readonly-struct shape is ADMISSIBLE. Necessary but not sufficient:
`IsValueStruct` is the codegen gate." Candidate: one field with an explicit
admissible/taken shape.

## B6. `withCore` is a producer protocol enforced only by prose

The same sentence — `printfn` binds `Vesper.Printf` and its deps, so their on-disk paths must
be resolvable references for the bundle to copy them — is cloned at three call sites in the
metadata/driver files. A caller who forgets `withCore` gets a `FileNotFoundException` at run
time, not a compile error.

Candidate: have `ProjectInfo.app` (or `materialiseApp`) derive the reference set from the
artifact's own dependencies. All three copies then delete.

## B9. The synthesised capability co-slots are a list in prose

`RecordTests.fs` spells the set out: non-generic `IEnumerable.GetEnumerator`,
`IEnumerator.Current`, `Reset`. The backend presumably holds the same set as a value. Exposing
it lets the test assert against it, which deletes the comment and makes the set checkable
rather than transcribed.

---

# Findings surfaced by the fixes

- **A28 (wrong-arity `Choice` annotation) produces two diagnostics.** Annotating a
  `Choice2Of2` value as `Choice<int, string, bool>` reports the expected
  `Type mismatch: Vesper.Choice`2 vs Vesper.Choice`3` **and** `internal compiler error: the
  frozen TAST holds 1 unresolved TyVar(s)`. A user-facing type error should not also trip the
  freeze's internal-error check. SemanticAnalysis issue; the test pins the mismatch message
  only.
- **Module-level `let mutable` is unsupported.** Adding the `while` rows to
  `ControlFlowTests.fs` showed a top-level `let mutable i` fails emission with
  `Emit: assignment to a variable with no local slot: BoundVarId 0`. The rows site the mutable
  inside a function. A gap in its own right, separate from loops.
- **A22 forced a two-stage seam.** `Layout`, `LayoutNodes`, `FileLayout` and
  `PartitionedTypeDecls` are all `internal` with no `InternalsVisibleTo`, so the shared
  discovery entry point is `FilePlan.create` followed by `FilePlan.discoverClosures` with
  caller-supplied member roots, rather than one call. A single call needs either
  `PartitionedTypeDecls` public or an `InternalsVisibleTo` for the test project.
- **Three "known gaps" no longer reproduce** and are now ordinary passing tests: a
  3-argument generic function (`a - b - c`), partial application of a user multi-arg function
  (`let inc = add 1`), cross-file union-case construction, and an inline DU match in argument
  position (A25, A42).

---

# Part C — extending the decompiled goldens past the conformance corpus

Moved here from the deleted `clr-codegen-improvements-plan.md`, where it was C2.

The conformance render (`test/XParsec.FSharp.Codegen.Clr.Tests/goldens/*.clr.cs`) cost nothing
measurable and produced the emission findings now in the git log on the first read. The next
candidates, in order of value per unit of work:

- `StructTests` and `StructSeqTests`, whose programs already live in `data/`, so a corpus entry
  and a type name are the only additions.
- `ClassTests`, where roughly 60 tests assert metadata shape alongside a runtime `Invoke`, and
  where near-identical programs are compiled two and three times over under different assembly
  names to assert different facets of the same emission. `ReflectionHarness.sharedType` is the
  compile-once shape `RecordTests` now uses for the same problem.

A golden replaces a shape assertion, never a behavioural one. It cannot see IL prefixes, opcode
choice, local signatures, table row order, duplicate mints, or assembly references, so the
digest gate and the `expectNoFSharpCore` checks stay.
