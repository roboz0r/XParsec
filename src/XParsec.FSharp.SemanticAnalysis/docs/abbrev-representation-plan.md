# Abbreviation representation

*Written 2026-09-01 against the code as it stands after the open overhaul (one ranked scope
stack; the written-`open` string channel deleted) and the `LocalModuleMembers` /
`ImplicitOpen.containers` follow-ups. Not started. All probes below were run against
`dotnet fsi` on 2026-09-01.*

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

Consequences, each confirmed against `dotnet fsi` (probes 1–5 below):

| written | F# | this analysis |
|---|---|---|
| `open A` then `module R = M`, `M` inside `A` | accepted | alias resolves to nothing |
| `module R = Inner` beside `Inner` in one module | accepted | alias resolves to nothing |
| `module R = A.M` then `module S = R` | accepted | `S`'s target probe misses |
| `module R = A.M` then `open R` | accepted | `resolveOpens` never reads `Abbrevs` |
| `module R = System.Collections` | FS0965 at the declaration | silently inert |
| `module R = Undefined` | FS0039 at the declaration | silently inert |

The declaration site reports nothing because no pass looks at a `ModuleAbbrev` element
(`Validation.fs:267`, `SignatureResolution.fs:757`): a bad abbreviation surfaces only as an
unrelated "unresolved" at some use site, or not at all.

What already matches F#: an absolute target, for values, functions, types and cases
(`LongIdentResolutionTests.fs`, "module abbreviation"); the alias binding its segment outright
rather than joining the candidate set (`Containers.fs:63`); and — by the accident of `Abbrevs`
being consulted before the scope stack — an abbreviation shadowing a real sibling module of the
same name (probe 7).

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
   and `Real.v` below the abbreviation reads `B`'s value — the abbreviation wins.
8. **File-locality.** From another compilation unit, the declaring unit's `R` is
   `FS0039: … 'R' is not defined` while its target module resolves fine.
9. **Directly in a namespace.** `namespace Test.B` / `module R = Test.A.M` — accepted
   (cross-unit, via `#load` of `.fs` files). The existing "module abbreviation" tests use this
   shape and pin real behaviour.

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
- `firstSegmentContainers` reads the resolved container from the map; `atPath` is deleted.
- `resolveOpens` expands a written `open`'s anchor segment through the map (probe 5).
- Publication: none, as today — pinned by a cross-file test rather than assumed.

**A type abbreviation keeps its representation; the work is pins.** The contract to pin end to
end: a consumer writes the name and every reader — inference, conformance, CLR emission, JS
emission — sees the expansion, never the abbreviation's own key as an artifact reference.

## Staged plan

**Step 1 — pin module-abbreviation semantics.** Through the pipeline
(`LongIdentResolutionTests.fs`'s "module abbreviation" list): `ptest`s for probes 1, 2, 4, 5;
`reports`-tests for 3 and 6 (also pending — nothing reports today); green pins for 7
(abbreviation wins), 8 (the alias is unresolved from a consumer file) and the existing 9-shape
cases. Red surface is the deliverable of this step.

**Step 2 — resolve at declaration.** `LocalAbbrev`, the `EnterElement` resolution and map, the
two declaration diagnostics, `firstSegmentContainers` reading the map, `atPath` deleted.
Un-pends probes 1, 2, 4 and both `reports` cases.

**Step 3 — `open` through an abbreviation.** Anchor expansion in `resolveOpens`. Un-pends
probe 5. Separate change: it touches ranked-scope construction, not container lookup.

**Step 4 — type-abbreviation audit and pins.** Separable; can run beside 2–3. End-to-end
tests: a cross-file (blob-mediated) use of a published abbreviation, bare and qualified; a
generic abbreviation applied at the use site; an abbreviation flowing through emitted CLR code
(`Codegen.Clr.Tests`) and JS code (`Codegen.Js.Tests`); and an audit that no emission path
renders an abbreviation `TypeKey` into a `TypeRef` / import. Any hole found becomes its own
pinned fix.

## Semantics to confirm

1. **Positional shadowing between an alias and a real module.** Probe 7 pinned one direction
   (abbreviation declared later wins below itself). The current alias-first read wins
   *regardless* of position; whether a real module declared BELOW the abbreviation reclaims the
   name is unprobed.
2. **A module abbreviation in a `.fsi`.** `walkSig` already carries them
   (`CstModuleTree.fs:150`); assumed to scope over the signature file alone, like the `.fs`
   rule. Unprobed.
3. **`[<RequireQualifiedAccess>]` on the target.** Whether `open R` is refused when `R`
   abbreviates an RQA module, and whether the alias itself satisfies "qualified". Unprobed.
4. **Accessibility on a type abbreviation** (`type private T = …`): publication must honour
   it; current behaviour unknown.
5. **The shared contract as stated.** An abbreviation never gets a compiled representation; a
   type abbreviation is still a real, nameable type to every F# consumer; a module abbreviation
   is real to nobody but its own file. This doc plans to that reading — correct it here if any
   clause is off.

## Scope and risk

Module-abbreviation steps touch `OpenScope`, `CstModuleTree`, `Containment`, `PassContext`,
`Containers`, `TypeRegistry`, and the SemanticAnalysis tests. Nothing is published for them, so
no blob or codec change. Step 4 adds tests across both backends and changes code only where the
audit finds a hole. `XParsec.FSharp.SemanticAnalysis.Tests`, `XParsec.FSharp.Codegen.Clr.Tests`,
`XParsec.FSharp.Codegen.Js.Tests` and `Vesper.Tests` gate each step.

Sequencing: after the open overhaul, whose ranked machinery step 2 reuses. Step 2 deletes the
old string map in the same change it lands the replacement's readers — the map has exactly one
consumer, so the delete-separately rule buys nothing here.
