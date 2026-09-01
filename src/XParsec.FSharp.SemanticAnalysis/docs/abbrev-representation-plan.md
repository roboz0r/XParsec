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

## Where module abbreviations stand — after step 3

The written form travels as `LocalAbbrev` (`OpenScope.fs:22`), the mirror of `LocalOpen`, and a
scope is one interleaved `OpenScope = LocalScopeDecl list` — `Open of LocalOpen | Abbrev of
LocalAbbrev`, most-recent-first, in source order. `TypeRegistry.resolveScopeDecls` folds the
list oldest-first into a `ScopeEnv { Opens; Aliases }`: each `open` and each abbreviation
target resolves against the environment accumulated above it, through the shared
`resolveInEnv` search — the aliases in the environment, then its `open`s, then the writing
scope's enclosing chain. `Containment.EnterElement` puts `env.Opens` on the scope stack and
the whole `env` into `PassContextResolution.Env`, and `firstSegmentContainers`
(`Containers.fs:98`) ranks the alias against the scope-stack candidates rather than
short-circuiting on it. `NameResolution` and `SignatureResolution` report an unresolved target
(FS0039) and a namespace target (FS0965) at the declaration, through
`Containment.ReportAbbrevTarget`, which resolves the target once more against the stored
`env`. The walker (`CstModuleTree.processElems`) hands an `open` or abbreviation element the
declarations above it even in a `rec` scope, matching F#'s top-down abbreviation resolution
there (an abbreviation of a real module below in a `rec` scope resolves via the hoist; an
abbreviation of an *alias* below is FS0039), so the stored `env` is the right one for the
report and nothing is re-folded.

A module's own position is part of `LocalContainer` (`TypeRegistry.fs:81`): its `module`
keyword, hoisted at registration to the enclosing `rec` scope's keyword
(`Containment.EnterContainment`). Every container lookup takes the offset it is read from, so
a module declared below the reading position is out of scope there — which is what lets a real
module reclaim a name from an alias below itself. `NameResolution.walkElems` and
`SignatureResolution` register every containment up front, so target resolution is independent
of how much of the file the walk has registered. An invisible local module does not blot the
name out: the referenced surface supplies it (`Containers.localOrReferenced`), matching F#'s
top-down environment where an earlier assembly's module of the same path is read above the
local declaration.

An `open` reaches its target through an alias, and an `open` of a
`[<RequireQualifiedAccess>]` module — reached directly or through an alias — is FS0892 at the
`open`, naming the target's full path (`Containment.ReportOpenTarget`).

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
  replacing the `Map<string, string>`. Opens and abbreviations travel as one interleaved,
  source-ordered `LocalScopeDecl list`.
- `Containment.EnterElement` resolves the list through the `resolveScopeDecls` fold, oldest
  first, into `ScopeEnv { Opens; Aliases }`; the aliases land in a per-element
  `Map<string, ScopeEntry>` on `PassContextResolution`, and the entry's `Opened` rank is what
  positions the alias. Each declaration reading only the environment above it closes probes 1,
  2 and 4 in one mechanism.
- The declaration reports: an unresolved target (probe 6's FS0039 analogue) and a target that
  is a namespace (probe 3's FS0965 analogue) — `ModuleContainer.InNamespace` is the refusal
  case, so the check is one pattern match on the resolved container.
- `firstSegmentContainers` reads the resolved container from the map and ranks it by the
  abbreviation's `Offset` against the scope-stack candidates, so a real module declared below
  the alias wins below itself (probe 10) and the alias wins above it (probe 7). An alias hit
  that short-circuits the scope stack fails probe 10. `atPath` is deleted.
- The `Open` case of the `resolveScopeDecls` fold expands a written `open`'s anchor segment
  through `env.Aliases` (probe 5), and the RQA refusal on the expanded anchor renders the
  target's path (probe 14).
- Publication: none, as today — pinned by a cross-file test rather than assumed.
- A `.fsi`'s abbreviations scope over the signature file alone (probe 13). `sigNode` and
  `implNode` produce one `ModuleNode.Abbrev` shape from two walks, each seeded `OpenScope.empty`,
  so the two walks' `LocalAbbrev`s stay in separate per-element maps.

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

**Step 2 — resolve at declaration. Done.** `LocalAbbrev`, the `EnterElement` resolution and map,
the two declaration diagnostics (`Kind.AbbreviatedNamespace` is new; the unresolved target files
under `Kind.UnresolvedQualifiedName`), `firstSegmentContainers` ranking the alias by offset,
`atPath` deleted. Probes 1, 2, 4, 10 and both `reports` cases are green, and probes 11 and 12
held.

Ranking the alias needed a module's own declaration position, so `LocalContainers` now stores a
`LocalContainer` carrying `VisibleFrom`, and `tryContainerUnder`, `tryContainerOfPath`,
`pathReaches` and `subContainer` each take the offset they are read from. That bounds a
written `open` by its own position too, which matches F#: `open M` above `module M` reaches
nothing.

A follow-up landed in the same shape: `OpenScope` became the interleaved `LocalScopeDecl list`
folded by `resolveScopeDecls`, the declaration report reads only the declarations above the
abbreviation (`resolveAbbrevTargetAt`), containments register up front in both resolution
passes, and `Containers.localOrReferenced` is the single local-then-referenced policy — an
invisible local module falls through to the referenced surface. Green pins: the `rec`-scope
abbreviation of a module below, the FS0039 refusal of a `rec`-scope abbreviation of an alias
below, and the referenced-module read above a local module of the same path. Pended: a term
annotation reading a type below it in a `rec` scope, which is a registration-order gap in the
classification scan, not an abbreviation defect.

**Step 3 — `open` through an abbreviation. Done.** The alias half was the small half: the
`Open` and `Abbrev` cases of the `resolveScopeDecls` fold share one `resolveInEnv`, which reads
`env.Aliases` for the anchor segment before falling back to `tryReachFrom`. Probe 5 is green.

The RQA half needed a channel that did not exist. `Kind.RequireQualifiedAccessModule` (FS0892)
is reported by `Containment.ReportOpenTarget`, which resolves the `open`'s target against the
`env` that `EnterElement` stored for it and asks `TypeRegistry.requiresQualifiedAccess`.
Answering that needed a module's `[<RequireQualifiedAccess>]` marker to reach a later file, and
no per-module published fact carried it: the surface published only compiled module names.

So `CompiledModuleNames` and `AutoOpenModules` became one dense `Modules` table, every declared
module → `ModuleFacts { CompiledName; RequiresQualifiedAccess; IsAutoOpen }`, along the whole
chain — `PassContextTypes`, `TastFile`, `FrozenFileResidue`, the codec, `PublishedSurface`, and
`IScopeContents.TryModule: ModuleKey -> ModuleFacts voption`. `ValueNone` means exactly "not
declared by this source"; a surface's `ImplicitOpens` is derived from the table in
`PublishedSurface.ofBuilder`, ordinal order on the full name giving outermost first.
`ICodegenSymbols.TryModule` replaced `ModuleClassNameOf`, and the three-way `ModuleClassName`
is gone: the CLR emitter reads `CompiledName.Emitted` off the facts and fails on `ValueNone`.

Green pins: probe 5, probe 14's `open` half asserted on the rendered path (`Test.A.Rqa`, the
target rather than the alias), a direct `open` of an RQA module, and an `open` of a plain
module as the negative control.

Known imprecision, step 5 below: `requiresQualifiedAccess` reads this file's own declaration
of the module first and the referenced surfaces second, because a `ModuleContainer` does not
carry which of the two it was resolved from. A file that declares `N.M` plainly while a
referenced assembly declares an RQA `N.M` would have its own `open N.M` refused.

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

**Step 5 — container provenance.** `tryContainerOfPath` and `tryDescend` (`TypeRegistry.fs`)
each read `types.LocalContainers` first and `scope.TryContainer` second, and return a bare
`ModuleContainer` that no longer says which of the two answered. `requiresQualifiedAccess`
then has to guess, and guesses local-first: a file that declares `N.M` plainly beside a
referenced RQA `N.M` has its own `open N.M` refused, and the opposite shadowing direction
would let an RQA module through.

The fix is to return what resolution already had in hand. `LocalContainer` grows the
`ModuleFacts` its `EnterContainment` registration reads (or points at the `Modules` entry), so
the local branch of `tryContainerOfPath` yields the facts beside the container, and the
referenced branch yields `scope.TryModule`. `resolveInEnv` then answers with a
`struct (ModuleContainer * ModuleFacts voption)`, `ReportOpenTarget` reads
`RequiresQualifiedAccess` off it, and `requiresQualifiedAccess` is deleted. Pin: a file
declaring a plain `N.M` after a referenced RQA `N.M`, and the reverse, each opened from the
same file. fsi is the oracle for which one F# reads in each direction.

Best point: its own change, immediately after step 3 and before step 4's codegen pins. It
touches only the container lookup and `ReportOpenTarget`, and every further reader of a
module fact off a resolved container (an RQA check on a bare use, `[<AutoOpen>]` through an
alias) would otherwise copy the guess. It must land before the module-path string channel is
retired, because that retirement rewrites the same two lookups and would have to carry the
provenance anyway.

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
   the target and the target's full path in the message. Step 3 carried this.
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
`Containers`, `TypeRegistry`, and the SemanticAnalysis tests. Nothing is published for an
abbreviation itself, but step 3's RQA refusal widened the per-module published table, so the
blob and its codec did change. Step 4 adds tests across both backends and changes code only where the
audit finds a hole. Step 5 touches `TypeRegistry` and `Containment` alone. `XParsec.FSharp.SemanticAnalysis.Tests`, `XParsec.FSharp.Codegen.Clr.Tests`,
`XParsec.FSharp.Codegen.Js.Tests` and `Vesper.Tests` gate each step.

Sequencing: after the open overhaul, whose ranked machinery step 2 reuses. Step 2 deletes the
old string map in the same change it lands the replacement's readers — the map has exactly one
consumer, so the delete-separately rule buys nothing here.
