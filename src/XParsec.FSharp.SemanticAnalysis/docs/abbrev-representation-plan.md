# Abbreviation representation

*Written 2026-09-01 against the code as it stands after the open overhaul (one ranked scope
stack; the written-`open` string channel deleted) and the `LocalModuleMembers` /
`ImplicitOpen.containers` follow-ups. Not started. All probes below were run on 2026-09-01,
against `dotnet fsi` and — for the cross-assembly cases — a compiled `ProbeLib.dll` referenced
through `#r`.*

## Two groups, one shared contract

An abbreviation binds a NAME to an entity that already exists. It declares nothing and compiles
to nothing. The two groups split on who can see the name:

- A **type abbreviation** (`type ages = Map<string, int>`) is a published surface fact: every
  F# consumer of the file — same assembly or referencing one — writes the name as if it were a
  real type, and every writing expands to the underlying type. No `TypeDef` is emitted for it.
- A **module abbreviation** (`module R = A.B.C`) is real to nobody but its own file. It is
  never published (probe 8), and inside the file it behaves as a *named `open`*: it binds one
  segment to one container.

The two are in opposite states in this codebase. Type abbreviations are modelled end to end and
need an audit plus pins. Module abbreviations are the last surviving string channel of the kind
the open overhaul deleted, with the same defect shape.

## Where module abbreviations stand — the defect

The whole representation is `OpenScope.Abbrevs: Map<string, string>` (`OpenScope.fs:25`),
alias → target *as written*, accumulated by the tree walk (`CstModuleTree.fs:231`). The one
consumer is `NameResolutionContainers.firstSegmentContainers` (`Containers.fs:76`), which on an
alias hit resolves the stored string through `atPath` (`Containers.fs:72`) — an ABSOLUTE probe
into `LocalContainers` and `Scope.TryContainer`. The scope the target was written relative to
is discarded — the defect the open overhaul closed for written `open`s, one field over.

Consequences, each confirmed against `dotnet fsi` (probes 1–6, 10 and 11 below):

| written | F# | this analysis |
|---|---|---|
| `open A` then `module R = M`, `M` inside `A` | accepted | alias resolves to nothing |
| `module R = Inner` beside `Inner` in one module | accepted | alias resolves to nothing |
| `module R = A.M` then `module S = R` | accepted | `S`'s target probe misses |
| `module R = A.M` then `open R` | accepted | `resolveOpens` never reads `Abbrevs` |
| `module R = System.Collections` | FS0965 at the declaration | silently inert |
| `module R = Undefined` | FS0039 at the declaration | silently inert |
| `module R = A` above a real `module R` | each use reads the nearest declaration above it | the alias wins wherever the two are both in scope |

The last row follows from `firstSegmentContainers` returning on an alias hit without consulting
the scope stack (`Containers.fs:76`). `Abbrevs` is threaded through a scope's elements in
declaration order, so a use written above the abbreviation already misses, and rebinding one
alias already reads the binding above each use — both match F# today (probes 11 and 12).

The declaration site reports nothing because no pass looks at a `ModuleAbbrev` element
(`Validation.fs:267`, `SignatureResolution.fs:757`): a bad abbreviation surfaces only as an
unrelated "unresolved" at some use site, or not at all.

What already matches F#: an absolute target, for values, functions, types and cases
(`LongIdentResolutionTests.fs`, "module abbreviation"); and the alias binding its segment
outright rather than joining the candidate set (`Containers.fs:63`).

`Abbrevs` being consulted before the scope stack gets probe 7 right and probe 10 wrong: F# ranks
the alias positionally against a real module of the same name, so a real module declared *below*
the abbreviation reclaims the name for uses below itself. A lookup that short-circuits on an
alias hit cannot express that.

## Where type abbreviations stand — landed, unaudited

- Local: `PassContextTypes.Abbreviation: KindRegistry<AbbreviationInfo>` (`TypeRegistry.fs:91`),
  filled lazily so declaration order within a group is free (`TypeInfos.fs:354`);
  `AbbreviationState` (`TypeInfos.fs:347`) is `NotFilled / InProgress / Filled / Broken`, a
  cycle reports FS0953 (`Diagnostics.fs:559`) and `Broken` expands to a fresh type variable.
- Transparent: a use site expands eagerly; downstream sees the underlying type longhand
  (`TypeRegistry.fs:89`), and elaboration surfaces the resolved RHS
  (`Elaborate/TypeDecls.fs:651`).
- Published: the `.fs` half through `FrozenSignature.fs:309`, the `.fsi` half through
  `SignatureResolution.publishAbbrev` (`:188`), both as
  `ExternalTypeShape.Abbrev(arity, frozen)` (`ExternalSymbols.fs:13`) into the blob; a
  consumer expands the frozen body against use-site args (`FrozenTypeBridge.fs:226`).
- Erased: CLR layout emits no `TypeDef` (`LayoutNodes.fs:110`); conformance treats an
  abbreviation as defining no symbols (`ConformanceSurface.fs:44`).

The unaudited edge is exactly the shared contract: the name *appears real* to a consumer while
no CLR/JS artifact exists. Any path that renders an abbreviation's `TypeKey` into a `TypeRef`
(or a JS import) binds to nothing and faults at load time, so the invariant is "every emission
read expands first". That is believed true and pinned nowhere end to end.

## Semantics, probed 2026-09-01

Single-unit cases in one `.fsx` each; cross-unit cases via `#load`. Each accepted case printed
its value; each refusal is quoted with the code fsi emitted.

1. **Target relative to an `open`.** `open A` then `module R = M` (`M` in `A`) — accepted.
2. **Target relative to the enclosing module.** `module R = Inner` beside `Inner` inside
   `Outer`, used as `R.v` — accepted.
3. **Namespace target.** `module R = System.Collections` — `FS0965: The path
   'System.Collections' is a namespace. A module abbreviation may not abbreviate a namespace.`
4. **Abbreviation of an abbreviation.** `module R = A.M` then `module S = R`, `S.v` — accepted.
5. **`open` through an abbreviation.** `module R = A.M` then `open R`, bare `v` — accepted.
6. **Forward target.** `module R = M` above `module M` — `FS0039: The namespace or module 'M'
   is not defined.` at the abbreviation. Scoping is top-down, as everywhere.
7. **Alias colliding with a real module.** `module Real = …` then `module Real = B`: accepted,
   and `Real.v` below the abbreviation reads `B`'s value. Probe 10 shows this is positional
   rather than a precedence of aliases over modules.
8. **File-locality.** From another compilation unit, the declaring unit's `R` is
   `FS0039: … 'R' is not defined` while its target module resolves fine.
9. **Directly in a namespace.** `namespace Test.B` / `module R = Test.A.M` — accepted
   (cross-unit, via `#load` of `.fs` files). The existing "module abbreviation" tests use this
   shape and pin real behaviour.

Second round, 2026-09-01, answering the questions the first round left open. A `.fsi`/`.fs` pair
loads as `#load "x.fsi" "x.fs"` in one directive; the cross-assembly cases `#r` a throwaway
library, `ProbeLib`, built from a synthetic `.fsproj`.

10. **A real module declared below the alias.** `module R = A` then `module R = …` (a real
    module) in one enclosing module: accepted, no duplicate-definition error. `R.v` above the
    real module reads `A`; `R.v` below it reads the real module. With probe 7 this makes the
    alias positional in both directions — the most recent declaration above the use site wins.
11. **The alias above its own declaration.** `R.v` written above `module R = A` —
    `FS0039: The value, namespace, type or module 'R' is not defined.`
12. **Rebinding one alias.** `module R = A` … `module R = B` in one module: accepted, and each
    use reads the target declared above it.
13. **In a `.fsi`.** `module R = Test.A` at namespace level in a signature file is accepted and
    `val f: R.T -> int` beside it resolves. In the companion `.fs`, `R.T` is
    `FS0039: The namespace or module 'R' is not defined.`; from a consumer file, `Test.R` is
    `FS0039: The value, constructor, namespace or type 'R' is not defined.` The signature file
    alone is the scope.
14. **`[<RequireQualifiedAccess>]` on the target.** `R.v` through the alias is accepted — the
    alias satisfies "qualified". `open R` is refused with the same code as opening the target
    directly, `FS0892: This declaration opens the module 'FSI_0001.M', which is marked as
    'RequireQualifiedAccess'. …`, and the message renders the target's full path, not the alias.
15. **Accessibility on a type abbreviation.** `type private T = int` is readable from the
    declaring module and its nested modules, and from a sibling module is
    `FS1092: The type 'T' is not accessible from this code location`. Across an assembly
    boundary both `private` and `internal` give the same FS1092; a public abbreviation, bare and
    generic, is consumed by name and expands. A public abbreviation over a private right-hand
    side is `warning FS0044: … The type 'Inner' is less accessible than the value, member or
    type 'Pub' it is used in.` — a warning, so the declaration stands.
16. **Compiled representation.** Reflecting over `ProbeLib.dll`: the emitted types are its three
    modules, plus the startup and assembly-attribute classes. No `TypeDef` exists for the four
    type abbreviations or for the module abbreviation, while a referencing assembly still writes
    `ProbeLib.A.Pub` by name — the published surface is the signature blob, not metadata.
17. **A namespace qualified by its own name.** Inside the first file declaring `namespace N`,
    both `N.Target.v` and `module R = N.Target` are `FS0039: … 'N' is not defined`, while the
    unqualified `Target` resolves. This is general namespace scoping rather than an
    abbreviation rule, and it is why probe 9's shape writes a *different* namespace's path.

## Target shape

**A module abbreviation is a named `open`, resolved once at its declaration.**

- The walk stays registry-free and carries the written form with its position:
  `LocalAbbrev { Alias; Path; Scope; ScopeDepth; Offset }`, the mirror of `LocalOpen`,
  replacing the `Map<string, string>`.
- `Containment.EnterElement` (`Containment.fs:140`) resolves the abbreviations in force beside
  `ScopeStackOf`, through the same outer-entries-then-own-scope search `resolveOpens` runs
  (`TypeRegistry.fs:251`), into a per-element `Map<string, ModuleContainer>` on
  `PassContextResolution`. Resolving innermost-last lets an abbreviation's target read the
  abbreviations and `open`s above it, which closes probes 1, 2 and 4 in one mechanism.
- The declaration reports: an unresolved target (probe 6's FS0039 analogue) and a target that
  is a namespace (probe 3's FS0965 analogue) — `ModuleContainer.InNamespace` is the refusal
  case, so the check is one pattern match on the resolved container.
- `firstSegmentContainers` reads the resolved container from the map and ranks it by the
  abbreviation's `Offset` against the scope-stack candidates, so a real module declared below
  the alias wins below itself (probe 10) and the alias wins above it (probe 7). An alias hit
  that short-circuits the scope stack fails probe 10. `atPath` is deleted.
- `resolveOpens` expands a written `open`'s anchor segment through the map (probe 5), and the
  RQA refusal on the expanded anchor renders the target's path (probe 14).
- Publication: none, as today — pinned by a cross-file test rather than assumed.
- A `.fsi`'s abbreviations scope over the signature file alone (probe 13). `sigNode` and
  `implNode` (`CstModuleTree.fs:140`, `:150`) already produce one `ModuleNode.Abbrev` shape from
  two walks, so the requirement on step 2 is that the two walks' `LocalAbbrev`s stay in separate
  per-element maps.

**A type abbreviation keeps its representation; the work is pins.** The contract to pin end to
end: a consumer writes the name and every reader — inference, conformance, CLR emission, JS
emission — sees the expansion, never the abbreviation's own key as an artifact reference.

## Staged plan

**Step 1 — pin module-abbreviation semantics. Done.** In `LongIdentResolutionTests.fs`'s
"module abbreviation" list: green pins for probes 11, 12, 13 (all three `.fsi` cases) and probe
14's qualified half; pending pins for probes 1, 2, 4, 5, 10 and 14's `open` half, and for the
two declaration diagnostics, probe 3 (FS0965) and probe 6 (FS0039). Each pending case was run
un-pended and fails for the reason this document gives — probes 1, 2 and 4 as
`UnresolvedQualifiedName`, probe 5 as an unresolved bare `v`, probes 3 and 6 with no diagnostic
at all.

**Step 2 — resolve at declaration.** `LocalAbbrev`, the `EnterElement` resolution and map, the
two declaration diagnostics, `firstSegmentContainers` reading the map and ranking it by offset,
`atPath` deleted. Un-pends probes 1, 2, 4, 10 and the `reports` cases, and holds probes 11 and
12 green.

**Step 3 — `open` through an abbreviation.** Anchor expansion in `resolveOpens`, and the RQA
refusal on an expanded anchor — which needs a `Kind` case for FS0892, since `Kind` carries none
today and the direct `open M` of an RQA module goes unreported as well. Un-pends probe 5 and
probe 14's `open` half. Separate change: it touches ranked-scope construction, not container
lookup.

**Step 4 — type-abbreviation audit and pins.** Separable; can run beside 2–3. End-to-end
tests: a cross-file (blob-mediated) use of a published abbreviation, bare and qualified; a
generic abbreviation applied at the use site; an abbreviation flowing through emitted CLR code
(`Codegen.Clr.Tests`) and JS code (`Codegen.Js.Tests`); `private` and `internal` abbreviations
expands an abbreviation rather than rendering its `TypeKey` into a `TypeRef` / import. Any hole
found becomes its own pinned fix.

Probe 15's three accessibility boundaries are already pinned green in `AssemblyFilesTests.fs`
beside the existing cross-file abbreviation tests: `private` readable from a nested module of
its declaring module, `private` refused from another file, `internal` readable across the
assembly. The remaining step-4 surface is the emission audit and the codegen pins.

## Semantics confirmed

Each question the first round left open, answered by probes 10–17.

1. **Positional shadowing between an alias and a real module** (probe 10). An alias and a real
   module of the same name coexist in one enclosing module, and each use reads whichever was
   declared last above it. The alias is one more top-down binding of one segment, with the same
   ranking rule as everything else. `Abbrevs` is already positional along its own axis — probes
   11 and 12 pin green — so the single defect is the alias-first read beating a real module
   declared below it. Step 2 owns it.
2. **A module abbreviation in a `.fsi`** (probe 13). The assumption holds: legal at namespace
   level, in scope for the rest of the signature file, out of scope in the companion `.fs` and
   for every consumer.
3. **`[<RequireQualifiedAccess>]` on the target** (probe 14). The alias satisfies "qualified",
   so `R.v` is accepted. `open R` is refused, with the code and wording of a direct `open` of
   the target and the target's full path in the message. Step 3 carries this.
4. **Accessibility on a type abbreviation** (probe 15). `private` scopes to the declaring module
   and its nested modules; `internal` to the assembly; a sibling module and a referencing
   assembly both get FS1092. A public abbreviation over a less accessible right-hand side is a
   warning, so publication must carry the abbreviation's own accessibility rather than infer it
   from the expansion. Step 4 owns this.
5. **The shared contract as stated** (probe 16). All three clauses hold. `ProbeLib.dll` emits a
   `TypeDef` for each module and none for any abbreviation, while a referencing assembly writes
   `ProbeLib.A.Pub` by name — the published surface is the signature blob, matching the
   `ExternalTypeShape.Abbrev` design already in place.

## Scope and risk

Module-abbreviation steps touch `OpenScope`, `CstModuleTree`, `Containment`, `PassContext`,
`Containers`, `TypeRegistry`, and the SemanticAnalysis tests. Nothing is published for them, so
no blob or codec change. Step 4 adds tests across both backends and changes code only where the
audit finds a hole. `XParsec.FSharp.SemanticAnalysis.Tests`, `XParsec.FSharp.Codegen.Clr.Tests`,
`XParsec.FSharp.Codegen.Js.Tests` and `Vesper.Tests` gate each step.

Sequencing: after the open overhaul, whose ranked machinery step 2 reuses. Step 2 deletes the
old string map in the same change it lands the replacement's readers — the map has exactly one
consumer, so the delete-separately rule buys nothing here.
