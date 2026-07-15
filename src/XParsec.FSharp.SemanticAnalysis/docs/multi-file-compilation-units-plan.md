# Multi-file compilation units — design plan

*The freeze/thaw prerequisite is in place: the intra-assembly file-N→N+1 provider stack
requires `IExternalSymbolProvider` to speak only `FrozenType`, inline bodies included —
otherwise file N+1's splice mutates file N's live `SemType` cells (backward flow). The
inline-body channel now freezes end to end (`Freeze.run` → `ExternalSymbol.InlineBody` →
`Inline.thawBody`, the single immutable→mutable seam on the consumer's side); the code is
its record.*

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
