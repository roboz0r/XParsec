# Multi-file compilation units — design plan

*The freeze/thaw prerequisite is in place: the intra-assembly file-N→N+1 provider stack
requires `IExternalSymbolProvider` to speak only `FrozenType`, inline bodies included —
otherwise file N+1's splice mutates file N's live `SemType` cells (backward flow). The
inline-body channel now freezes end to end (`Freeze.run` → `ExternalSymbol.InlineBody` →
`Inline.thawBody`, the single immutable→mutable seam on the consumer's side); the code is
its record.*

## Status — Phase 1 progress (session handoff)

**Foundation + front end are DONE and committed on `semantic-analysis`; Step C (codegen
iterates units) and Step D (multi-file driver + delete concat) remain.** Everything below
was reviewed (diff + independent build + SA/Codegen.Clr/Codegen.Js suites) before commit.
All three suites are green: **SA 1019, Codegen.Clr 1341, Codegen.Js 401.**

### Landed (commit → what)

- **`e5824378`** — `ModuleMemberInfo` → `ModuleBindingInfo` rename (it sits over
  `BindingKey`/`SymbolKey.Binding`; "Member" collided with the type-member world).
- **`7f094a42`** — **freeze-time enrichment side tables** + **the projection**:
  - `type Accessibility = Public | Internal | Private` (`SideTypes.fs`), stored honestly
    on a `SymbolKey -> Accessibility` table on `TastFileG` (`Tast.fs`), populated from the
    CST access token at `Elaborate`. Two thresholds read it: `.fsi` = public-only,
    intra-assembly = internal-or-better.
  - `BindingValReprs` (module-function compiled arity, computed at freeze via
    `TastLower.valReprOf`) and `BindingTyparArities` (typar-axis width) side tables on
    `TastFileG`.
  - `FrozenSignature.toProvider (assemblyName) (frozen) : IExternalSymbolProvider`
    (`FrozenSignature.fs`) — walks a frozen file, emits `ExternalTypeShape`/`ExternalSymbol`/
    `ExternalMember` with home origin, internal-or-better filter, frozen inline bodies,
    scheme axis normalized to Declaring via `ConformanceTypars.toDeclaringAxis`. Parity-
    tested vs the `.fsi` extractor for saturated mono/generic values + curried/tupled/mixed
    functions.
- **`c9b5db83`** — **`SymbolOrigin` no-home rework**: `type Origin = Unstamped | InAssembly of
  AssemblyName` (`Local` → `Unstamped`; the ambiguous `string option` is gone). Plus
  `AnonymousOrigin.nameOfContent` (FNV-1a, `$anon.`-prefixed), wired to nothing.
- **`1a1e2157`** — **`AssemblyUnits`** (`AssemblyUnits.fs`), the multi-file **front-end**
  pipeline (no codegen): `analyseAssembly (assemblyName) (external) (files: (path*source) list)
  : Result<FrozenUnit, UnitError> list` — parse each file on its own, analyse against
  `composite(prior views nearest-first ++ [external])`, project via `FrozenSignature.toProvider`,
  push. `FrozenUnit = { Path; Input; Lexed; Frozen; View }`. `consolidatedDiagnostics` anchors
  each unit's `NodeKey` diagnostics to that unit's own `(path, line, col)`.
- **`b48ea5d5`** (review F1) — `Origin.AssemblyOption : string voption` + `Origin.IsStamped`
  replace the throwing accessor; the five open-coded local/external matches collapse onto them.
- **`367800cf`** (review F2–F5) — de-dup: shared `Pipeline.parse code source`;
  `ExternalSymbols.tupledParams`/`unitFrozen`; `ExternalSymbolProviders.KeyedLeaf.ofNamedWithMembers`
  (wires the by-name overload scan internally — no projection re-copies it);
  `AssemblyUnits` uses `LineIndex` for positions.

### Verified working

Cross-file name resolution is proven end-to-end at the front end (`AssemblyUnitsTests.fs`):
file N+1 resolves file N's type + saturated function both **qualified** and via **`open`-ed
bare** reference; **forward-only** scoping holds; **nearest-file-wins** compose ordering
(checked on differing schemes — order, not identity); per-unit diagnostic anchoring; offset-0
non-collision. This validates the thesis: a file's frozen output projects a provider view that
later files resolve by name, home-stamped, riding the existing same-assembly-is-not-a-clash rule
in `TypeRegistration.diagnoseExternalClaim`.

### Anchor corrections (early sections below have STALE line numbers — trust these)

- `NodeKey` struct: `NodeKey.fs` (~154), still single-string `offset:32`, no `FileId`.
- `PassContext.Input`/`Lexed`: `PassContext.fs` (~568), singular (NOT `SideTables.fs:1523`).
- Provider interfaces: `IExternalSymbolProvider` `ExternalSymbols.fs:996`; `ICodegenSymbols`
  `ExternalSymbols.fs:1043` (NOT `CodegenSymbols.fs`).
- `.fsi` `isAccessible`: `VesperLib.fs:593` (public-only).
- Production analyse seams: `Pipeline.analyseFor` / `analyseForSelfHost`.
- `Diagnostic`: `SideTypes.fs:25`, `{ Key: NodeKey; ... }` — no file identity (that's why
  diagnostics anchor in-unit).

### Next — Step C, then Step D

`FrozenSignature`/`AssemblyUnits` are **front-end only**; nothing wires them into codegen or a
driver yet. Remaining:

- **Step C — codegen iterates units** (the invasive one). The `Assembler`
  (`Codegen.Clr/Assembler.fs`) is built around a single `tast: Frozen.TastFile`; make emission
  consume a *sequence* of frozen units into one shared assembly/`MetadataContext`: a **Bind**
  pass over ALL units into shared name/`SymbolKey` registries, then a **Prepare** pass **per
  unit** with that unit's `NodeKey`-keyed tables (never flattened). The **home-assembly local
  branch**: 0a made origins name-comparable, so a cross-file symbol whose home = the project's
  own assembly resolves to a **local** `TypeDef`/handle instead of an `AssemblyRef` — the seam
  is `ClrEnv.externalAsmRef` / `externalModuleRef` (~`ClrEnv.fs:411/454`), which today
  `failwith`/`AssemblyRef` on any stamped home. Factor the backend-agnostic shape ("a
  compilation is an ordered sequence of frozen units" + bind-all-then-prepare) into
  **`Codegen.Common`** so `Codegen.Js` shares it. **Recommended: open with a read-only
  investigation** (map the `Assembler` Bind/Prepare structure, the local-vs-external handle
  resolution, the real Common-vs-Clr split), then decompose into small cuts — the pattern used
  for BindingKey / 0a / Step B worked well. Fold in the two open items below as cross-file calls
  exercise them.
- **Step D — multi-file driver + delete concat.** `ClrDriver` gains a multi-file entry routing
  through `AssemblyUnits` + Step-C codegen; **delete** the `String.concat "\n\n"` in
  `TestHelpers.fs` (`buildPackage` ~:310, `vesperCoreDll` ~:129) and route those through it. The
  single-`source` `ClrDriver.compile` stays for script/fragment callers. **Also add the two
  deferred-incompleteness demonstration tests from `cross-unit-name-resolution-plan.md`** (bare
  construction of a cross-unit RQA record wrongly accepted; a cross-unit record in an unopened
  namespace wrongly resolvable bare) — written to assert the CORRECT behaviour, so they are red
  until the RQA-threading (item 2) and ambient-scope gate (item 3) land, then flip green.

### Step C decomposition (post-investigation, 2026-07-15)

A read-only investigation refined the Step C shape. Three findings reshape the plan above:

1. **Home-local is mostly automatic.** Local types, construction, member dispatch, and pattern
   matches already resolve through the SymbolKey-keyed registries FIRST (`ClrEncoder.encodeType`,
   `EmitResolve`/`EmitConstruct`/`EmitMember`/`EmitPattern`), falling to `externalAsmRef` only on a
   miss. Once **Bind** populates those registries for every unit, cross-file nominal use resolves
   to local handles with **no new code**. The *only* genuine home-local seam is a cross-file
   **module-function call** (`f a b` freezes to `External`, bypassing the registries): it needs one
   new branch in `ClrRecipes.emitExternalCall` (`:412`) plus one new `localModuleFns` registry on
   `ClrEnv`, keyed by the fn's `ValueKey`. `externalAsmRef` keeps firing for genuinely-external
   (package) symbols, unchanged.
2. **`Layout.build` reads NodeKey-keyed tables** (`HolderPlan.create` → `ModuleMembers`/
   `TopLevelNames`/`GenericFnSchemes`; `discoverClosures` → `FunVerdicts`/`ClosureReprs`), so it is
   NOT decl-only and cannot naively span a tast list. Resolution: split it into **`buildUnit`**
   (per unit, reads that unit's own NodeKey tables) + **`combine`** (concatenates the per-unit row
   nodes into one global layout — the NodeKey reads stay confined to `buildUnit`; `combine` and
   `deriveHandles` touch no NodeKey table). The global handle/prefix-sum space is built by
   `combine`.
3. **Three single-tast assumptions** the combine must fix: (a) one `<Module>` + one Program holder
   per tast → `combine` emits exactly one `<Module>`, and the **entry unit is the LAST file**: only
   the last file may carry top-level expressions (bare `do`-style exprs), which become the synthetic
   Program holder + IL entry point. A non-last file with top-level expressions is an ERROR (combine
   rejects it); a library has no Program holder. The Program holder's emitted IL name MUST use a
   reserved-character convention (`<…>` / `$`, like `<Module>` and `<closure>$N`) so it can never
   collide with a source-declared type/module name; reconcile the exact name (current "Program"
   holder vs. a `Program$0`) while keeping the single-file path byte-identical. (b) closure
   names collide (`<closure>$0` per unit) → extract naming into a SHARED
   policy function (a single seam), threading one namer across units so names are assembly-globally
   unique. The name is the closure's `TypeDef` key, so the policy needs global uniqueness AND
   totality (every node, incl. anonymous lambdas). Default stays the counter scheme (unique + total);
   F#'s debuggable `<bound-name>@<line>` is a future drop-in policy in that one function, NOT the
   default — it is neither globally unique (same line across files) nor total (anonymous lambdas have
   no bound name). (c) same-named `module M` split across files: same FQN is a FRONT-END
   duplicate-definition error, NOT a codegen holder-merge. `combine` keeps only a fail-safe assertion
   (should be unreachable). VERIFY the same-assembly-is-not-a-clash rule in
   `TypeRegistration.diagnoseExternalClaim` (which enables cross-file resolution) does not silently
   permit a genuine duplicate-module redefinition — close that gap upstream if it does (a C6 check).

**Decision: NO `Codegen.Common` factoring for Step C.** Bind/prepare is an SRM artifact, not
backend-agnostic (JS has no bind phase — it concatenates per-unit statements). Everything stays in
`Codegen.Clr`; `compileUnits` takes `AssemblyUnits.FrozenUnit`/`Frozen.TastFile list` directly. The
shared shape is deferred to a possible future **Step E (multi-file `Codegen.Js`)**, where it falls
out naturally once a real second consumer exists.

**Ordered cuts (each builds green + committed separately; behavior-preserving through C5):**

- **C2** — merge `IntrinsicReprKeys` across units (Clr-local helper; single-unit = identity).
- **C3** — `localModuleFns` registry on `ClrEnv` + `RegisterLocalModuleFn` + dormant home-local
  branch in `emitExternalCall` (empty registry ⇒ identical to today).
- **C4** — split `Layout.build` → `buildUnit` + `combine` with a 1-unit shim (byte-identical);
  thread the closure-counter base (defaulting to 0).
- **C5** — Assembler loops `layout.Units`, fresh `EmitContext` per unit (shared nominal registries
  + ctx + provider; per-unit NodeKey dicts). *Highest risk — the invasive one.*
- **C6** — N-unit MECHANISM, kept byte-identical by still driving ONE unit. `combine` generalizes
  to N units (one `<Module>`; entry unit = the LAST unit for an Exe / none for a Library; mint the
  Program holder from it and set per-unit `EmitEntryPoint` on the entry unit ALONE so `PrepareMain`
  fires once; reject top-level expressions in a non-entry unit; holder-key de-dup guard). Add
  `Layout.buildMany` (one `ClosureNamer` → `buildUnit` each → `combine`). `UnitLayout` carries
  `FunVerdicts` so `buildPrelude` stops reading the single ctor `tast.FunVerdicts`. The `Assembler`
  takes `tasts: Frozen.TastFile list`, unions their `IntrinsicReprKeys`, and builds via `buildMany`.
  Bind registers each unit's module fns into `localModuleFns` (activating the C3 branch). Drop the
  now-dead singular `AssemblyLayout` fields (`Lowered`/`Plan`/`Closures`/`ClosureByNode`/
  `Partitioned`). `Codegen.assemble` still passes `[tast]`, so single-file emission is byte-identical
  (CLR suite green); the N-unit paths are reviewed but first EXERCISED at C7.
- **C6.5 (NEWLY FOUND — blocks N-unit; TWO steps, X then the re-key).** `MethodKey.StaticFn` /
  `FieldKey.ModuleValue` embed a bare per-file `NodeKey`, and `Layout.deriveHandles` builds ONE
  shared `Dictionary<MethodKey,_>` over the combined `layout.Methods` (`Layout.fs:1634/1648`) plus
  the Assembler's shared `fieldDefHandles`. The principled fix is to re-key those two cases by
  **`SymbolKey`** — name keying makes cross-file resolution collision-free BY CONSTRUCTION (two
  different fns in different files have different qualified names regardless of offset; the
  same-offset "collision" only exists under NodeKey keying), so **NO unit discriminator is needed**.
  The ONLY case where `SymbolKey` is not injective is **shadowing** — legal ONLY in the last (entry)
  file's top-level code, which today emits e.g. two static fields both literally named `x` on the
  Program holder, distinguished only by `NodeKey`.

  **Investigation verdict (2026-07-15): uniquify the names; do NOT gate on the Main-locals rework.**
  The "lower top-level values to `Main` locals" fix (X) does NOT fully evacuate the shadow collision:
  top-level **generic** values have NO non-generic `Main`-local form (they compile to 0-arg generic
  `StaticFn` static methods), and shadowed top-level **functions** and static-fn-**pinned** values
  also remain. So X leaves residue in BOTH key domains and STILL needs the uniquify fallback. The
  uniquify fallback is by contrast necessary AND sufficient on its own, and far smaller. Therefore:
    - **C6.5 (chosen).** Make entry-file top-level program values + holderless generic values
      offset-unique in their emitted metadata name via `topLevelName` (`EmitClosures.fs:234`) —
      `<name>$<NodeKey.Offset>` (functions already emit `fn$<offset>`). Then re-key
      `MethodKey.StaticFn` / `FieldKey.ModuleValue` from `NodeKey` to `SymbolKey`, minting the
      SymbolKey from that now-unique name. The map is injective + globally unique across units by
      name; `NodeKey` stays untouched; `LocalModuleFns` keeps `SymbolKey`. EMISSION change (names
      `x` → `x$<offset>`); only `MetadataStructureTests` field-name expectations update (~1 test) —
      runtime-output tests are unaffected. Also removes the latent duplicate-field-name sketchiness.
    - **X (deferred, OPTIONAL).** Lowering entry-file top-level non-generic ground values to `Main`
      locals (escaping ones captured) is a separate F#-fidelity / leaner-Program-holder improvement,
      NOT a prerequisite for the re-key. Reuses the existing ref-struct Main-local + capture
      machinery (`Emit.fs:76-80`, `buildVarLoad`, `discoverClosures` non-captured set); its only new
      work is a pinned-field analysis for values read by surviving static fns / member cctors.
  Must land BEFORE C7.
- **C7 (DONE — partial proof; a front-end gap found).** `Codegen.compileUnits (symbols) (project)
  (tasts)` + `compileUnitsWithBclReferences` added; `assemble` takes a `tasts` list; single-`tast`
  `compile` delegates as `compileUnits … [one]`. Surface: the caller composes
  `composite(views ++ external)` and hands codegen the already-composed provider + bare
  `Frozen.TastFile list` (Codegen stays agnostic of the front-end `FrozenUnit`). New test
  `CrossFileUnitsTests.fs` compiles TWO files into one assembly and RUNS it: unit 2 calls unit 1's
  module fn + generic fn → stdout `24`, and the PE carries no self-`AssemblyRef` (the cross-file
  call re-homed to a LOCAL `MethodDef`). **The N-unit codegen machinery needed ZERO changes — it was
  correct.**

  **FINDING — cross-file NOMINAL (record/union) use is blocked UPSTREAM in the front end**, not in
  codegen. A prior unit's `FrozenSignature.toProvider` view projects a nominal's SHAPE, but no USE
  SITE consults the provider for a nominal's own shape — they read only the analysing unit's LOCAL
  `TypeRegistry`: record construction (`InferRecordAccess.inferRecord` → `TypeRegistry.tryRecord` /
  `findUniqueRecordByFieldSet`), record field read (`resolveFieldStep` `TyRecord` arm →
  `tryRecordByKey ctx.Types`), union-case construction (local `CtorIndex`). So no record/union is
  usable across units today; only module fns + generic fns resolve. (The earlier "Verified working:
  file N's TYPE resolves" claim was too strong — the type NAME binds, but the nominal cannot be
  CONSTRUCTED / field-read / matched cross-file; `AssemblyUnitsTests` only filtered "Unresolved"
  errors so never caught it.) Closing it is a FRONT-END cut (wire use-site nominal resolution to the
  composite provider, or project prior-unit nominals into the consuming unit's `TypeRegistry`) — it
  is the real content of the "Projection coverage boundaries" open item below, now on the critical
  path for a records-bearing corpus. **LANDED** (the R1–R4b-2 commit series on `semantic-analysis`):
  cross-file record field read, construction (bare + qualified), and patterns all resolve through the composite
  provider (mirrors F#'s `eFieldLabels` + `BuildFieldMap`); codegen needed no change (`ldfld` /
  `newobj` re-home to the local `TypeDef`), proven by cross-file runtime tests. Records were the
  only blocked kind — unions/classes already had provider paths. The REMAINING cross-unit nominal /
  name-resolution gaps (RQA threading, ambient-scope gate, enums, interface members, the discovered
  type-annotation-by-`open` gap, cross-package records) are consolidated in
  `cross-unit-name-resolution-plan.md`, scheduled AFTER Step D.

### Open items to close during Step C/D (see "Known open items" section below for detail)

- **Point-free ValRepr — model RESOLVED as arity-0** (a point-free `let compose = f >> g` is a
  function-valued property, arity 0, matching F#'s `ValReprInfo`). Implement: make the `.fsi`
  extractor stop over-stating arity for point-free sigs so extractor/frozen/DLL-metadata agree,
  and make codegen's external-call path *invoke* a function-valued external rather than requiring
  `ValRepr` groups. Add the point-free parity case then.
- **Projection coverage boundaries** (fail-safe, close when the corpus references them): RQA
  union cases (`IsRequireQualifiedAccess` hardcoded `false`), enums unregistered, interface
  members not decurried, member-level accessibility not captured.

### Working conventions for this workstream

Build/test ONLY via `./claude_tools.cmd -Action Build` / `-Action Test -TestProject "..."` (not
raw `dotnet`). Prefer small agent cuts (large contexts take shortcuts — the point-free `ValRepr`
hole slipped in that way and was caught in review). Instruct mirror-shaped agents to *factor, not
copy* (a duplication pass crept in and was cleaned in `b48ea5d5`/`367800cf`). Gatekeeper every
agent diff: read it, run the suites independently, check for duplication, THEN commit — the user
authorized committing in steps for this workstream with short messages.

## Problem

Semantic analysis of a package's implementation currently concatenates every
`impl` `.fs` file into one string before parsing:

```fsharp
// test/XParsec.FSharp.Codegen.Clr.Tests/TestHelpers.fs  (buildPackage :301, vesperCoreDll :129)
let src =
    manifest.Impl
    |> List.map (fun rel -> IO.File.ReadAllText(...))
    |> String.concat "\n\n"
let lexed, file = parseFile src
let tast = Pipeline.analyse*For asmName provider src lexed file
```

`ClrDriver.compile` (the production seam) likewise takes one pre-assembled
`source: string`. There is no multi-file driver anywhere — the concat *is* the
package-build model.

The join does two jobs. One is correct and must survive; the other is a hack:

1. **"An assembly is one linear, top-down type-inference environment."** Correct,
   and matches F#. File order is the manifest `impl` order; forward inference flows
   file→file; recursion never crosses a file (`namespace rec` / `module rec` bound
   a scope *within* a file, exactly like `type` members). This is preserved.
2. **"A single global node-identity space."** The hack. `NodeKey` (`NodeKey.fs:146`,
   the primary key of every side table) packs `offset:32` = a character offset into
   the one `input` string. Two files each with a `let` at offset 0 collide. The
   concat makes offsets globally unique. The cost is that `PassContext.Input`/`Lexed`
   (`SideTables.fs:1523`) are singular, so `NameOf`/`ReadableOf`/diagnostics resolve
   against the *blob* — there is no path from a `NodeKey` back to which `.fs`/line.

## Confirmed premises

- Intra-assembly file order is **strictly linear**: file N sees files 1..N-1 only.
- `namespace rec` / `module rec` introduce a recursive scope **within a file** (or a
  section of one); recursion never spans files.
- ⇒ File N is fully settled (analysed + frozen) before N+1 begins. No backward flow.

## Model (B1): each file is a compilation unit that projects a provider

Intra-assembly cross-file resolution becomes the **same operation** as cross-package
resolution — `ReferencedProject.composeOrdered`, run at *file* granularity:

```
file N+1 resolves against:  [view(N); view(N-1); … ; view(1)]  ++  externalProvider
```

- An **assembly** = a *linear* compose of file-unit provider views, ahead of the
  package/external stack.
- A **package graph** = a *DAG* compose of package-unit providers (unchanged,
  `composeOrdered` over `depends-on`).
- Both bottom out in the same `IExternalSymbolProvider` layering.

Per file, in manifest order:
1. Parse the file on its own (`parseFileFull` already does this — keeps the file's
   own `Input`/`Lexed`/`Ast`; the `.fsi` extractor path is already this shape).
2. Analyse it with `provider = compose(prior file views ++ external provider)`,
   producing its frozen `TastFile` (`Pipeline.analyse*For`, unchanged internally).
3. Project its **inferred signature** to an `IExternalSymbolProvider` view; push on
   the stack. Retain its frozen `TastFile` for codegen + inline-body splicing.

Because each file owns its `PassContext` and side tables, offsets are unique within
their file and **never collide across files**. Cross-file references resolve by
*name* against prior views, never by `NodeKey`. **`NodeKey` is left exactly as it is
— no `FileId`, no reserved-bit surgery.** Authentic positions fall out for free:
each unit owns its `(input, lexed)`, so a diagnostic renders against its real file.

## Interface question: `IExternalSymbolProvider` is sufficient

No `IInternalSymbolProvider` is needed. The provider surface (`ExternalSymbols.fs:714`)
is entirely frozen-domain and covers every cross-file need — `TryLookupType`,
`TryLookup` (vals), `TryLookupMember(s)`, `TryLookupUnionCase`, `TryLookupIndexSignature`,
`TryLookupInlineBody(ByName)`, `IntrinsicReverseCanon`/`IntrinsicForwardRepr` (the
`prim-types` case), `AmbientOpenPrefixes`. File N's output is already frozen, so the
projection is domain-consistent. Its codegen twin `ICodegenSymbols` (`:866`) is served
by the same backing provider (`codegenView`), so one file-unit view feeds both the
front end and emission.

The interface was designed anticipating this: *"The provider's data may grow; its
interface stays a dumb oracle"* and *"Per-file pipelines run independent PassContexts
in parallel and may hit the same provider."* Keep that true — do not add
internal-specific members.

Every intra-assembly difference is **projection population policy**, not interface
shape:

- **Accessibility threshold = internal-or-better.** The `.fsi` extractor's
  `isAccessible` (`VesperLib.fs:572`) is public-only. The impl-file projection keeps
  `internal` (same-assembly visible), drops `private` (module-scoped). File N resolves
  its *own* privates through its own live ctx and never exports them.
- **Home-assembly origin preserved.** `composeOrdered`/`wrap` stamp a *foreign*
  `SymbolOrigin`; a file-N type is the *same assembly* as N+1 and is already
  home-stamped via `ctx.AssemblyName` (`LocalSymbolKey.ofType`). The file-level stack
  needs a **compose-without-re-origin** variant so N+1 mints the identical key.
- **Same-assembly `let inline` bodies** ride the inline channel as cross-package
  inline bodies do — but as **frozen** bodies thawed at the splice (`Inline.thawBody`).
  A `SemType` body here would let file N+1 mutate file N's cells; the frozen channel
  closes that.

## The one genuinely new component

**FrozenTAST → in-memory signature provider projection** — the compiler computing an
implementation file's *implicit signature*. We do not have this today: cross-package,
an implementation's signature is recovered by emitting a DLL and reading its metadata
(`MetadataSymbols`); intra-assembly the DLL does not exist until every file is done, so
the projection must run in memory. It walks a frozen `TastFile`'s decls and emits
`ExternalTypeShape` / `ExternalSymbol` / `ExternalMember` entries (types, module vals,
members, union cases, intrinsic reprs, ambient prefixes, index signatures, inline
bodies) with home-assembly origin and the internal-or-better filter. This is the bulk
of the work and the piece to design first. With the prerequisite in place the
projection is uniform — inline bodies are frozen `Frozen.TDecl` entries drawn from the
*same* frozen unit as everything else, not a pre-freeze side-artifact the unit must
separately retain.

## Design constraints (get these wrong and they bite)

1. **`NodeKey` untouched.** Guaranteed only if side tables stay per-file. The moment
   anything *merges* N units' `NodeKey`-keyed tables (`ClosureReprs`, `FunVerdicts`,
   `GenericFnSchemes`), cross-file offsets collide and we are forced back into `FileId`
   surgery. So:
2. **Codegen iterates units.** Emission consumes a *sequence* of frozen units, each
   with its own snapshot tables, switching the active tables per unit as it emits that
   unit's decls into the shared assembly. It must not flatten the `NodeKey`-keyed
   tables across files.
3. **Compose-without-re-origin** for the file-level stack (above). `ExternalSymbols.stack`
   already takes a `voption` origin, so this is `stack ValueNone …`, not a new variant.
4. **Inline bodies cross the boundary frozen**, thawed at the splice
   (`Inline.thawBody`). A `SemType` body would reintroduce backward flow.
5. **Diagnostics resolve in-unit.** `Diagnostic` carries only a `NodeKey` offset and no
   file identity (`SideTypes.fs:27`). Each unit's diagnostics must be paired with that
   unit's `input`/`lexed` and rendered at the unit boundary; flattening N units' bare
   `Diagnostic`s into one list and rendering later would reintroduce the `NodeKey`→file
   map constraint 1 exists to avoid.

## Driver / harness changes

- `ClrDriver` gains a multi-file entry: take the manifest's ordered file list (or an
  ordered `(path, source)` list), parse per file, run the file-unit pipeline, emit.
  The current single-`source` `compile` stays for script/fragment callers.
- Delete the `String.concat "\n\n"` from `buildPackage` / `vesperCoreDll`
  (`TestHelpers.fs`); route them through the multi-file driver.

## Migration risk

Today's blob is *more permissive* than real separate compilation: multiple files'
`namespace Vesper` blocks merge into one lexical stream. Blob order == manifest order
== file order (all linear), so nothing an accepted blob compiles *should* be rejected —
but a same-named `module M` split across files, or a file quietly leaning on a later
file's decl, will surface at the file boundary. This is a correctness tightening;
expect it to flush out one or two latent ordering assumptions in the existing manifests
when flipped on.

## Decisions (2026-07-15 implementation kickoff)

Confirmed against current code (anchors refreshed): `isAccessible` is `VesperLib.fs:593`
(public-only); `ICodegenSymbols` lives in `ExternalSymbols.fs:1043` (not
`CodegenSymbols.fs`); the production Pipeline seams are `analyseFor` /
`analyseForSelfHost`; the compose-without-re-origin seam is
`ExternalSymbolProviders.stack (stampOrigin: SymbolOrigin voption)` / `composite`
(= `stack ValueNone`). Crucially, the NodeKey-keyed side tables
(`IntrinsicReprKeys`, `ClosureReprs`, `FunVerdicts`, `GenericFnSchemes`) ride *inside*
`Frozen.TastFile` (`TastFileG`, `Tast.fs:1005`) and `Codegen.compile` takes no
`PassContext` — so per-unit table isolation falls out of having N frozen files.

1. **Concat is deleted in Step D**, not kept as a fallback — core-lib file order is
   trusted; no A/B period.
2. **Codegen-unit iteration is factored into `Codegen.Common`** where backend-agnostic
   (the "a compilation is an ordered sequence of frozen units" concept + the
   bind-all-then-prepare-per-unit shape), so `Codegen.Js` shares it; only SRM/CLR
   emission stays in `Codegen.Clr`.
3. **Phase 1 only** — authentic positions + per-file re-parse, zero type-checking
   semantic change. No incremental reuse (Phase 2), no DAG (Phase 3).

### Foundation reframe (front-end enrichment before the projection)

The projection needs three facts the frozen domain doesn't carry today. The
`BindingKey` *identity* is already total (module bindings don't overload and can't
shadow at module scope — a redefinition is a duplicate-binding diagnostic), so the
key is **not** widened. Instead the facts ride freeze-populated side tables:

- **Accessibility** — a token-free 3-state `Accessibility = Public | Internal |
  Private` (stored honestly, not pre-thresholded: cross-package export is public-only,
  intra-assembly is internal-or-better — two thresholds over one fact), exposed as a
  `SymbolKey -> Accessibility` side table on `TastFileG`, modeled on `IntrinsicReprKeys`.
- **`ValRepr`** (module-function compiled arity) — computed **upstream at freeze**
  (`peelValRepr` while the lambda spine still exists; backend-neutral), on its own
  `TastFileG` side table. Compile-order wall: `ValReprG` is defined in `Tast.fs`, so
  this cannot live on `ModuleBindingInfo` (`SideTypes.fs`, which must precede `Tast.fs`).
- **`TyparArity`** — the binding's single value/function typar-axis width, minted where
  the method-axis indices are minted.

The projection normalizes the typar axis once at its boundary via
`ConformanceTypars.normAxis` (Method→Declaring), rather than flipping
`ExternalSymbol.Scheme`'s Declaring convention. The frozen side tables and the
projection that reads them land **together** (no unread infra ships first).

Revised sequencing: **0a** `SymbolOrigin.Assembly: string` (+ standalone hash helper;
independent, before Step C) → **0b+A** freeze side tables + projection (together) →
**B/C/D**.

The cross-file codegen mechanism: one shared assembly/`MetadataContext`; a **Bind**
pass over *all* units registers every unit's type/member handles into the shared
**name/SymbolKey**-keyed registries (globally unique via qualified names); a **Prepare**
pass runs **per unit** with that unit's **NodeKey**-keyed tables active. Cross-file
calls resolve by home-origin name to the local `MethodDef`; the NodeKey tables are only
ever consulted for the owning unit's own bodies, so they never flatten (constraint 1/2).

## Phasing

- **Phase 1 (this plan):** file-unit-as-provider model, the signature projection, the
  four constraints, multi-file `ClrDriver`, concat deleted. Delivers authentic
  positions + single-file re-parse with **zero** change to type-checking semantics.
- **Phase 2 (enabled, not required):** incremental reuse. The per-file frozen `TastFile`
  + its projected view *are* the reuse boundary (the FCS `TcState` analogue): editing
  file N reuses views 1..N-1 and re-runs N..end. The linear model makes this natural;
  do not let it complicate phase 1.
- **Phase 3 (if ever):** intra-assembly DAG scheduling. Requires real file-level
  dependency analysis; a pure optimization over the linear model. The parallel-able DAG
  already exists at *package* granularity.

## Known open items from 0b+A (resolve before Step B consumes the projection)

The freeze side tables + `FrozenSignature.toProvider` projection landed correct for
*saturated* bindings (parity with the `.fsi` extractor is tested for mono/generic
values and curried/tupled/mixed functions). Outstanding:

- **Point-free / eta-reduced ValRepr (OPEN SEMANTIC QUESTION).** A binding with no
  syntactic lambda spine (`let compose = f >> g`) freezes to `ValRepr = ValueNone`
  (arity-0), because `peelValRepr` reads syntactic lambdas, not the type. This likely
  MATCHES F#'s `ValReprInfo` rule (arity comes from written parameters, so a point-free
  module binding compiles to a function-typed property, arity 0) — meaning the current
  behavior may be right and the `.fsi` extractor's full-arity reconstruction is the
  over-stater. Resolution: decide the point-free arity model, make extractor / frozen /
  DLL-metadata agree (probably arity-0), and ensure codegen's external-call path
  *invokes* a function-valued external rather than requiring `ValRepr` groups. Add a
  point-free case to the parity matrix once the model is fixed (it is deliberately
  absent now — it would assert the wrong thing until this is settled).
  **RESOLVED (arity-0 model accepted):** a point-free binding is a function-valued
  property (arity 0), matching F#'s `ValReprInfo`. Step-B work: make the `.fsi`
  extractor stop over-stating arity for point-free sigs so extractor / frozen /
  DLL-metadata agree on arity-0, and ensure codegen's external-call path *invokes* a
  function-valued external rather than requiring `ValRepr` groups. Add the point-free
  parity case then.
- **Projection coverage boundaries (fail-safe, close when the corpus needs them):**
  union-case `IsRequireQualifiedAccess` is hardcoded `false` (RQA union cases resolve
  bare — over-lenient); enum types are left unregistered (consumer falls back to nominal
  `TyConst`); interface member surfaces publish name/arity but not decurried members;
  member-level accessibility is not captured (a `member private` leaks — the `.fsi`
  extractor is public-only here). All over-export / over-accept — safe for resolution,
  never a miscompile. Each closes when the cross-file VesperCore corpus actually
  references it.

## To verify during implementation

- Enumerate the exact provider methods each resolution/codegen call site hits for a
  *local* (prior-decl) symbol today, to confirm the projection populates all of them
  (spot-checked: types, vals, members, union cases, intrinsic reprs, inline bodies,
  index sigs, ambient prefixes — all present on the interface).
- Confirm `codegenView` over a projected impl-file view returns correct
  `TryLookupOpenSignature` for a prior file's module function (home origin, method
  typars) so an intra-assembly cross-file call mints the right `MemberRef`.
- Confirm same-named `module M` across two files composes without key collision under
  the home-origin (non-re-stamping) variant.
