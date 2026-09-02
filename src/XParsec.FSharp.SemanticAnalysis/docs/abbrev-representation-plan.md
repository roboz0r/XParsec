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

Third round, 2026-09-01, for step 5. Every case is a compiled consumer project referencing
`ProbeLib`, because `#load` puts a signature file's namespace under `FSI_0001` and the two
declarations then never share a path.

18. **A module path declared by this compilation and by a reference.** Two modules of one path
    in ONE assembly is `FS0248: Two modules named 'N.Dup' occur in two parts of this assembly`,
    so the shadow needs a reference. Across the boundary both declarations stand, and one
    `open` of the path reaches both: a consumer declaring `N.Plain` beside `ProbeLib`'s
    `N.Plain` reads its own `locV` and the reference's `extW` under a single `open Plain`.
19. **`[<RequireQualifiedAccess>]` on either declaration of a shared path.** Both directions are
    `FS0892`, rendering the shared path. A plain local `N.Rqa` beside a `[<RequireQualifiedAccess>]`
    referenced `N.Rqa` is refused, and so is a `[<RequireQualifiedAccess>]` local `N.Plain`
    beside a plain referenced `N.Plain`.
20. **`[<AutoOpen>]` on one declaration of a shared path.** `[<AutoOpen>]` on the local
    declaration auto-opens its own contents alone: the reference's `extPlain` at the same path
    is `FS0039: The value or constructor 'extPlain' is not defined`. An `open` is per-PATH; an
    auto-open is per-DECLARATION.

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
`env` that `EnterElement` stored for it and asks `requiresQualifiedAccess`. Answering that
needed a module's `[<RequireQualifiedAccess>]` marker to reach a later file, and no per-module
published fact carried it: the surface published only compiled module names.

So `CompiledModuleNames` and `AutoOpenModules` became one dense `Modules` table, every declared
module → `ModuleFacts { CompiledName; RequiresQualifiedAccess; IsAutoOpen }`, along the whole
chain — `PassContextTypes`, `TastFile`, `FrozenFileResidue`, the codec, `PublishedSurface`, and
the `IScopeContents` module channel, which step 5 below widened to
`DeclarationsOf: ModuleKey -> EqArray<ModuleFacts>`. Empty means exactly "not declared by this
source"; a surface's `ImplicitOpens` is derived from the table in
`PublishedSurface.ofBuilder`, ordinal order on the full name giving outermost first.
`ICodegenSymbols.DeclarationsOf` replaced `ModuleClassNameOf`, and the three-way
`ModuleClassName` is gone: the CLR emitter reads `CompiledName.Emitted` off the facts and fails
where no source declares the module.

Green pins: probe 5, probe 14's `open` half asserted on the rendered path (`Test.A.Rqa`, the
target rather than the alias), a direct `open` of an RQA module, and an `open` of a plain
module as the negative control.

This step left `requiresQualifiedAccess` reading this file's own declaration of the module and
falling through to the referenced surfaces, so a file declaring `N.M` plainly beside a
referenced RQA `N.M` admitted its own `open N.M`. Step 5 closed that.

**Step 4 — type-abbreviation audit and pins. Done.** The emission invariant held: no path
renders an abbreviation's `TypeKey` into a `TypeRef` or a JS import. It is pinned in
`TypeAbbreviationTests.fs` in both codegen suites — a record, a union, a class, a generic tuple
and an `int` alias, each written through its alias in a program that runs, with the PE's
`TypeDef` and `TypeRef` tables (`PeInspection.peTypeDefNames` / `peTypeRefNames`) and the
emitted JS text asserted alias-free; the same set across two files of one assembly, bare and
qualified, and published through a `.fsi`; and, in `CrossAssemblyEscapeTests.fs`, a `Pair`
abbreviation published by the producer's `.fsi`, consumed by name from another assembly, with no
type in the producer DLL.

The audit found three holes, each now fixed and pinned.

1. **An abbreviation name in expression or pattern position.** `S.Square 1`, `| S.Circle r`,
   `C(1)` and `C.Zero`, where `S` and `C` abbreviate a union and a class, were `StaticMember`
   items on the abbreviation itself (`NameResolutionLongIdent.inType`), so the pattern was
   `UndefinedPatternDiscriminator`, the constructor `Unresolved identifier`, and the static
   `UnresolvedQualifiedName`, locally and across files. The fix is one `dealias` step in
   `LongIdent`, applied where a `ResolvedTypeRef` is produced for expression or pattern
   position (`localType`, `externalType`), so every consumer reads an already-dealiased
   reference. An abbreviation is read through its key only when it ALIASES a keyed type: its
   body is that type applied to the abbreviation's own type parameters, each exactly once
   (`TypeRegistry.tryAliasedKey` over the filled body, `ExternalTypeShape.AliasedKey` over the
   frozen one). A `ResolvedTypeRef` and a `TypeKey` carry no instantiation, so an abbreviation
   instantiating any parameter of its body (`type IntBox = Box<int>`) stays itself, as does one
   whose body is unfilled at the read (a `rec` scope reading below itself). The later pass
   reads local classes and statics by WRITTEN name, so `TypeRegistry.tryKeyOfArity` and
   `tryKeyOfArglessName` admit an aliasing abbreviation claim under the same rule (`keyInKind`),
   which fixes `tryWrittenClass`, `tryStaticMember`, `tryUnionBare` and every other by-name
   kind lookup at once.
2. **An abbreviation closing an indented module body.** `module M =` / `    type intpair =
   int * int` followed by a dedent parsed the RHS as a unit-of-measure product, and `type t =
   int` in the same position parsed as a measure too, so the body filled as a bare type
   variable and every consumer read `'a` — with no diagnostic at the declaration, and none at
   an `int`-typed use, which is why the existing module-level abbreviation tests passed. The
   abbreviation branch of `TypeDefn.parse` peeks the token after the type to decide the
   measure retry, and `peekNextSyntaxToken` FAILS with an offside error on a dedented token,
   which the branch propagated and `choice` answered with the measure parser. Both retry sites
   (`TypeDefnParsing`, and `pTypeArg` in `TypeParsing`) now read a failed peek as "nothing
   follows". Golden `403_type_abbrev_closes_indented_module.fs`; no other golden changed. A
   `[<Measure>] type N = kg * s` in that position now parses as a tuple, which is what it
   parses as everywhere else — the type checker owns that disambiguation.
3. **Vacuous pins.** The cross-file `myalias = int` tests asserted only on error diagnostics,
   which an alias collapsing to a type variable never produces. The new pins destructure a
   tuple-bodied alias and add its components, so a collapse is an SRTP error.

Two gaps beside the audit are pended in `Codegen.Js.Tests/TypeAbbreviationTests.fs`, each
with a no-abbreviation control: the JS emitter refuses a top-level tuple-pattern binding
(`EmitJs: unsupported declaration`), and refuses constructing a class declared in another
file of the package (`construction of external type … has no JS analogue`). A record literal
resolves by the field set in scope rather than by the annotation's expected type, so a
cross-file literal needs the record's namespace opened; the fixtures open it.

Probe 15's three accessibility boundaries were already pinned green in `AssemblyFilesTests.fs`:
`private` readable from a nested module of its declaring module, `private` refused from another
file, `internal` readable across the assembly. A transparent generic alias in constructor position
(`type MyBox<'a> = Box<'a>` then `MyBox(1)`) is pinned green; an instantiating one
(`type IntBox = Box<int>` then `IntBox(1)`) is pinned as a `ptest` GAP, because reading it
through a key would drop `int` and infer a fresh argument. Closing it means carrying the
instantiation from the abbreviation's body into `ResolvedItem.Ctor` and the by-name kind
lookups, rather than a key alone.

**Step 5 — module facts across sources. Done.** The step was written on the premise that
resolution picks one of the two sources declaring a module path and loses which; probes 18–20
falsify it. Both declarations of a shared path stand, one `open` reaches both, and
`[<RequireQualifiedAccess>]` on either refuses it. There is no provenance to carry, because the
answer is the union rather than a choice.

So the multi-source read is the channel's own job. `IScopeContents.TryModule` became
`DeclarationsOf: ModuleKey -> EqArray<ModuleDeclaration>`, one entry per declaring surface, and
`ScopeContents.composite` concatenates it as it already did `UnionCasesNamed`. A
`ModuleDeclaration` is a `ModuleFacts` with its `SymbolHome`: a surface homes its declarations
in the file (`FrozenSignature.toSurface`, `SignatureResolution.run`), and
`ExternalSymbolProviders.stack` re-homes them in the package through `ScopeContents.decorate`,
exactly as it stamps every symbol's `SymbolOrigin`. `ModuleDeclarations` (its own file, lifted
out of `TypeRegistry`) joins this compilation's own declaration to that array in `reaching`,
where the declaration's `LocalContainer.VisibleFrom` precedes the use site, and
`ModuleDeclaration.anyRefusesOpen` is the one statement of the merge rule. That refuses probe
19's first direction, which the local-first guess admitted; the second direction was already
refused and holds. The planned threading — `LocalContainer` growing `ModuleFacts`,
`resolveInEnv` answering with a `struct (ModuleContainer * ModuleFacts voption)` — is not
built. `Containers.localOrReferenced` keeps its order: both branches yield the same
`ModuleContainer` for a shared path and differ only in the rank, where the local declaration's
own position is the higher one anyway.

`RequiresQualifiedAccess` is the only per-PATH field of `ModuleFacts`; `CompiledName` and
`IsAutoOpen` are per-DECLARATION (probe 20) and a reader selects rather than merges them. The
one selecting reader is `ClrEnv.externalModuleRef`, which takes the declaration whose `Home`
is the symbol's `origin.Home` and fails where none is.

The per-assembly uniqueness the union relies on (FS0248) is enforced at each unit:
`Containment.EnterContainment` reports `Kind.DuplicateModule` at a module's name on its first
entry when an earlier file of the compiling assembly — a declaration homed in the same
assembly — already declares the path.

Green pins in `OpenResolutionTests.fs`, over `ExternalSymbolProviders.composite` of the contract
stack with three published `Ref` modules, each publishing a value: each direction of probe 19,
probe 18's merge — `locV + extW` under one `open Other` — and a local
`[<RequireQualifiedAccess>]` declared BELOW the `open`, which does not refuse it. The stack is
the real one because a local module's `[<RequireQualifiedAccess>]` is read by resolving the
attribute type, which a bare `providerOfSurface` declares nothing for. Reverting `reaching` to
the local-first guess turns the first pin red. `ScopeContentsTests.fs` pins the composition
directly: two package stacks declaring one path, disagreeing about
`[<RequireQualifiedAccess>]`, each declaration homed in its assembly, and reverting the
concatenation to first-hit turns it red. `LongIdentResolutionTests.fs` pins FS0248 over two
files of one assembly.

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
audit finds a hole. Step 5 touched `SymbolKeys`, `ExternalSymbols`,
`PublishedSurface`, `TypeRegistry`, the new `ModuleDeclarations`, `Containment`, `Elaborate` and
the CLR backend's `CodegenSymbols` / `ClrEnv`. `XParsec.FSharp.SemanticAnalysis.Tests`, `XParsec.FSharp.Codegen.Clr.Tests`,
`XParsec.FSharp.Codegen.Js.Tests` and `Vesper.Tests` gate each step.

Sequencing: after the open overhaul, whose ranked machinery step 2 reuses. Step 2 deletes the
old string map in the same change it lands the replacement's readers — the map has exactly one
consumer, so the delete-separately rule buys nothing here.
