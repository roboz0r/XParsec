# Concat deletion — the frozen-signature projection-completeness cluster

*Ephemeral plan (`src/**/docs/*-plan.md`): scopes ONE body of work, delete once it lands.
Picks up where `multi-file-compilation-units-plan.md` Step D stops. That plan's Step D had
two halves: the **multi-file driver** (LANDED — `ClrDriver.compileAssembly[With]`, the shared
analyse→compose→emit seam, cross-file codegen, cross-unit type-name resolution) and **delete
the concat** (BLOCKED — this plan). Step D is accepted as blocked until this list clears.*

## The one blocker, stated once

Routing the real Vesper corpus off `String.concat "\n\n"` onto per-file analysis
(`ClrDriver.compileAssemblyWith`) requires `FrozenSignature.toProvider` to project
**everything a later file references in a prior file** onto the `IExternalSymbolProvider`
surface. Today it projects records, unions, classes, module vals/fns, inline bodies, intrinsic
*repr axes*, and (as of the type-annotation fix) module-held type *names*. It does **not** yet
project three things Vesper.Core leans on cross-file. Each is a `FrozenSignature` projection
slice — none is a freeze-tree gap (the data is already on the frozen `TastFile`; the projector
just doesn't emit it). All are **over-strict** misses ("not defined" / "no member"), never a
miscompile.

## Definition of done (the single integration proof)

There is no clean per-slice unit proof — the honest gate is the corpus itself:

1. In `test/XParsec.FSharp.Codegen.Clr.Tests/TestHelpers.fs`, delete BOTH `String.concat
   "\n\n"` joins — `vesperCoreDll` (~:132) and `buildPackage` (~:325) — and route each through
   `ClrDriver.compileAssemblyWith` (`Pipeline.analyseFor` for `vesperCoreDll`,
   `analyseForSelfHost` for `buildPackage`). Remove the "NOT migrated" comment blocks.
2. `./claude_tools.cmd -Action Test -TestProject "XParsec.FSharp.Codegen.Clr.Tests"` green,
   AND `SemanticAnalysis.Tests` green.
3. `vesperListDll` (single file) optionally routes through the seam for uniformity.

**Workflow: iterate the swap.** Attempt the swap, read the FIRST "not defined" / "no member"
error, fix that projection slice, re-run, repeat. Each fix uncovers the next (that is how this
cluster was found). Use `ftest` to converge ONE failing corpus test at a time; strip all
`ftest` focus before finishing. If a NEW slice surfaces beyond the three below, add it here.

## Prerequisite (this session — verify it landed)

The **consumer-side registered-key fix** (annotation stamp carries the registered `InModule`
key instead of a re-cut flattened `InNamespace` key; `byKeyCanonical` deleted). Without it a
cross-unit `let r : R = { … }` mismatches. If the residual ptest in `AssemblyUnitsTests.fs`
("annotation identity matches construction identity") is still a `ptest`, that fix has not
landed — do it first.

## The three slices

Anchors are approximate (`FrozenSignature.fs` is under active edit) — re-verify before cutting.

### 1. Intrinsic / primitive types as `TryLookupType` shapes
- **Symptom:** a prior file's `unit` / `int` / `string` / `obj` in annotation position →
  "The type '…' is not defined". `prim-types-int.fs`'s `type int32 = int`, and every member
  signature annotated with a primitive, fail once split from `prim-types-min.fs`.
- **Cause:** the projector emits only the intrinsic **repr** axes
  (`frozen.IntrinsicReprKeys` → `IntrinsicForwardRepr` / `IntrinsicReverseCanon`,
  `FrozenSignature.fs:~381-387`), NOT a `TryLookupType` type **shape** for the primitive's own
  nominal. The type-decl projection loop (`for decl in frozen.Decls do … TDecl.Type td when
  exported td.Key`, `FrozenSignature.fs:~153`) either skips primitive/intrinsic type decls or
  its `Kind` dispatch has no arm for them, so they never enter `typesByName` / `shapesByKey`.
- **Fix:** project intrinsic/primitive type declarations as type shapes (an `Intrinsic`
  `ExternalTypeShape`, or the nominal shape they already carry) so `TryLookupType` answers the
  name. Mirror how LOCAL registration exposes a primitive's nominal so a same-file annotation
  of `int` already resolves — do the same at the projection boundary. Note the interplay with
  the `.fsi` extractor, which DOES surface primitives cross-package (parity target).
- **Cross-ref:** the multi-file plan's "Projection coverage boundaries"; a sibling of
  `cross-unit-name-resolution-plan.md` item 5 (enums) — do enums here too if the corpus hits
  one (`TTypeKindG.Enum` is on the frozen tree, `Tast.fs:716`, just unprojected).

### 2. Interface members (decurried) in the projection
- **Symptom:** `core-types.fs`'s `interface Vesper.Fun with member _.Invoke …` →
  "does not define a member 'Invoke'" when `Vesper.Fun` is declared in a prior file.
- **Cause:** the projector's interface arm publishes the interface's name + arity but DEFERS
  the member set. The members are present on the frozen tree —
  `TTypeKindG.Interface of methods: EqArray<TAbstractMethodG<'ty>>` (`Tast.fs:638`) — so this
  is purely an emit gap in `FrozenSignature`'s `Interface` handling: walk `methods`, decurry
  each (the same axis-normalization the member projection already does for classes, via
  `ConformanceTypars` / the `Method→Declaring` normalization the module docs describe), and
  publish them as `ExternalMember`s under the interface key so `TryLookupMember(s)` answers.
- **Cross-ref:** `cross-unit-name-resolution-plan.md` item 6.

### 3. Base types (`inherit`) cross-unit
- **Symptom:** `compiler-attributes.fs`'s `inherit Attribute` → "Cannot inherit from unknown
  type 'Attribute'" when `Attribute` is a prior-file class.
- **Cause:** the `inherit`/base-type resolution path does not consult the composite provider
  for the base class the way construction / member dispatch now do. Investigate whether the
  base-type head is stamped (it is a type head in declaration position — the type-annotation
  fix may already resolve the NAME; confirm) and whether the class projection carries enough
  of the base type for the consumer's `inherit` lowering. Likely a small consumer-side wire-up
  once the name resolves (mirror how the type-annotation fix threaded the registered key), but
  VERIFY — it may need the class shape to publish its base.

## Ordering & risk

The three are independent projection slices; order by whichever the swap surfaces first
(intrinsics almost certainly first — the prim-types files are the base of Vesper.Core). Each
lands as its own small cut, built + committed separately, with a targeted cross-unit
`analyseAssembly` SA test where one is expressible, but the corpus swap is the real proof.
Expect the swap to reveal slices iteratively; do not assume the list is exhaustive until the
concat is actually gone and both suites are green.

## Working conventions (same as the multi-file workstream)

Build/test ONLY via `./claude_tools.cmd -Action Build` / `-Action Test -TestProject "…"`. Do
NOT run a repo-wide Fantomas Format (it reflows unrelated committed files). Small agent cuts;
instruct mirror-shaped agents to FACTOR, not copy. Gatekeeper every diff: read it, run both
suites independently, dedup check, THEN commit with a short message — committing in steps is
authorized for this workstream. Durable correctness belongs in the type system / code, not in
this doc; delete this doc once the concat is gone.
