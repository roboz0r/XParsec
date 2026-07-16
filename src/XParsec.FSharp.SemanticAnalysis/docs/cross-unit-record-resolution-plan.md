# Cross-unit record resolution — design plan

*Records are the one nominal kind with **no** cross-unit resolution path. Construction and
field-read read only the analysing unit's local `TypeRegistry`; classes and unions already
fall back to `ctx.Provider` on a local miss. This plan gives records the same provider path,
modelled on F#'s `eFieldLabels` + `BuildFieldMap`. It is **independently landable** — a
cross-package fix that the multi-file work merely exercises first — and multi-file resumes at
the end of Step C once it lands.*

## Why this is separate from multi-file

The gap is not cross-file-specific. None of the three record paths consult `ctx.Provider` at
all, so a record declared in a *referenced package* is equally unusable by construction /
field-read today — it simply has not surfaced because the current corpus accesses referenced
records through members or pattern-matching, or keeps them package-internal. So the fix is
"records resolve through the provider like classes and unions do," and it stands on its own
against the existing cross-package provider stack. The multi-file composite provider
(`FrozenSignature.toProvider`) is just the first consumer to hand records across a unit
boundary.

Landing order: this plan lands against the single-file pipeline (cross-package tests prove
it), then multi-file **Step C** (`CrossFileUnitsTests`) picks up records-bearing cross-file
corpora for free — no re-entry into the codegen machinery, which is already correct.

## The asymmetry, precisely

Two resolution worlds. `ctx.Types` (the `TypeRegistry`) holds **only the current unit's own
declarations** (`registerRecordTypeDefn` / `registerUnionTypeDefn`). `ctx.Provider` is the
external oracle (cross-package, and the multi-file composite). For each nominal operation, the
question is whether the code path falls back to the provider when `ctx.Types` misses:

| operation | local read | provider fallback today |
| --- | --- | --- |
| class member (`c.M`) | `tryClassByKey ctx.Types` | **yes** — `InferRecordAccess.fs:276+` |
| union member (`u.M`) | `tryUnionByKey ctx.Types` | **yes** — `InferRecordAccess.fs:362` |
| union-case ctor (`Some x`, `Color.Red`) | `CtorIndex` | **yes** — NameResolution stamps via `TryLookupUnionCase`; `InferIdentExpr.tryExternalCtorType` reads it (`InferIdentExpr.fs:128`); pattern position via `Scope.resolvesAsBareExternalCase` |
| **record field read** (`r.X`) | `tryRecordByKey ctx.Types` | **NO** — `resolveFieldStep` `TyRecord` arm errors on miss (`InferRecordAccess.fs:242‑253`) |
| **record ctor, qualified** (`{ R.X = … }`) | `tryRecord ctx.Types` | **NO** — `inferRecord`, `InferRecordAccess.fs:67` |
| **record ctor, field-set** (`{ X = …; Y = … }`) | `recordsWithField ctx.Types` | **NO** — `findUniqueRecordByFieldSet`, `InferResolve.fs:163` |

So the earlier "no record/union usable cross-file" was too strong for unions (they already
have provider paths that extend to the composite for free) and exactly right for records.
This plan closes the three record rows.

## F# reference (`D:\roboz0r\fsharp`)

F# splits record-literal resolution into a **dumb reverse index** and a **shared algorithm** —
the split this plan copies.

**Index — `eFieldLabels: NameMultiMap<RecdFieldRef>`** (`src/Compiler/Checking/NameResolution.fs:428`),
a `field-name -> [records declaring it]` multimap. Populated as each record type enters scope
(`NameResolution.fs:1280`), guarded by:

```fsharp
if isILOrRequiredQualifiedAccess || not tcref.IsRecordTycon || flds.Length = 0 then
    nenv.eFieldLabels            // NOT indexed
else … AddRecdField …           // indexed, one entry per field
```

⇒ **IL (non-F#) records and `[<RequireQualifiedAccess>]` records are excluded** — they must be
qualified; F# record types from referenced assemblies that are `open`-ed **are** indexed. This
is exactly "only Vesper-TAST-backed providers can answer it": F# reverse-indexes only F# record
tycons, never imported IL.

**Algorithm — `BuildFieldMap`** (`src/Compiler/Checking/Expressions/CheckExpressions.fs:1969`):
1. Per field, `ResolveField` → candidate record set from `eFieldLabels` (`NameResolution.fs:3913`).
2. **Intersect** the per-field sets by tycon equality (`CheckExpressions.fs:2001`).
3. Unique survivor → done. Not unique → **tie-break by field count**
   (`tc.TrueFieldsAsList.Length = flds.Length`, `CheckExpressions.fs:2009`), else the
   "fields do not determine a unique record type" diagnostic.
4. Short-circuit: if the expected type already pins a record (`tryTcrefOfAppTy ty`), skip the
   index and resolve type-directed (`NameResolution.fs:3928`).

The index never knows how to "match a field set"; it only answers "which records have a field
named `X`." The policy lives in one place. A `{ X = 1 }` against a `{X;Y}` record resolves the
*type* at the intersection and defers the *missing-field* error to construction — better UX than
Vesper's current "no record matches field set {X}."

## Design

### Adopt F#'s algorithm, not just its index

Vesper's `findUniqueRecordByFieldSet` currently does **exact set-equality** (`declared = nameSet`,
`InferResolve.fs:179`). Replace it with F#'s **intersection + count tie-break**. The reason is
forward-looking: exact-equality is a terminal test that can only answer "is this *complete*
field set a record?" — it structurally cannot answer "which records are still live given the
fields typed so far," because a partial set never equals a full declared set. Intersection can,
so the same function serves the checker (Phase 1) and future LSP completion (Phase 2). The two
consumers differ **only in the terminal condition**: the checker demands a unique survivor
(with count tie-break); completion wants the surviving set to suggest each candidate's remaining
fields. Same intersection, different stop rule.

Note `recordsWithField`'s own doc-comment already frames the local index as "the candidate set
a record literal / record pattern **intersects over**" (`TypeRegistry.fs:1029`) — the local
primitive is already the right shape and already visibility-scoped by `keyVisibleAt useSite`;
only the consumer's equality-vs-intersection logic changes.

### The one new interface member

Expose the reverse index **per field**, matching how F# consumes `eFieldLabels`
(`Map.find id.idText`, once per field) and how the provider composite already unions
`TryLookupUnionCase` / `TryLookupMembers` across leaves — a per-field method composes with a
plain union at each `stack` seam, where exposing a whole `Map` would force map-merging:

```fsharp
// IExternalSymbolProvider (ExternalSymbols.fs, beside TryLookupUnionCase)
abstract TryRecordsWithField: fieldName: string -> ExternalRecordCandidate[]
```

`ExternalRecordCandidate` mirrors `ExternalUnionCase` (`ExternalSymbols.fs:239`) — a record, not
a wide tuple, so the record's origin/RQA ride alongside identity without re-threading consumers:

```fsharp
type ExternalRecordCandidate =
    {
        /// The record's compiled (arity-suffixed) name — the `TypeKey` source.
        RecordName: string
        TyparArity: int
        Origin: SymbolOrigin
        /// Every declared field name — enough for the intersection AND the count
        /// tie-break. Field *types* come from the by-key shape path at construction,
        /// not from here.
        FieldNames: string[]
        /// True when the record is `[<RequireQualifiedAccess>]`: excluded from bare
        /// field-set resolution (F#'s `isILOrRequiredQualifiedAccess` guard). See the
        /// RQA coupling below.
        IsRequireQualifiedAccess: bool
    }
```

Array-with-empty return (the `TryLookupMembers` convention), `[||]` = no record here.

### Three entry points, one shared shape path

All three ride "given a record key, get its frozen field shapes" — `TryLookupType(key) ->
ExternalTypeShape.Record(arity, fields, origin)` (each `ExternalFieldShape` carries `Name` +
`Frozen` template, instantiated with the receiver's args via `instantiateFieldType`,
`ExternalSymbols.fs:157`). The reverse index is only the *unqualified* entry point.

1. **Field read** — `resolveFieldStep` `TyRecord` arm (`InferRecordAccess.fs:242`) gains a
   provider fallback mirroring the sibling `TyUnion` arm: on `tryRecordByKey` miss,
   `ctx.Provider.TryLookupType(SymbolKey.Type recKey)` → `Record` shape, find the field by name,
   `FrozenTypeBridge.instantiateDeclaring fieldShape.Frozen args`. **A field read must NOT stamp
   `ExternalAccess`** — verified against the Elaborate dispatcher (`ElaborateExpr.fs:289`): the
   `& ExternalAccess ctx info` arm fires BEFORE the local `translateDotLookup` arm (`:310`) and
   lowers to `TExpr.ExternalMember` (a property/method), so stamping would misroute a field to
   the member path. Unstamped, the node falls through to `translateDotLookup`'s `TyRecord` arm,
   which emits `TExpr.FieldGet(receiver, name, ty)` by name off the receiver's type
   (`Access.fs:178`); cross-file, `recKey` re-homes to a local `TypeDef` and codegen emits
   `ldfld` — no Elaborate/codegen change. A field-name **miss** falls back to an augmentation
   **member**: `TryLookupMember` + commit + **stamp `ExternalAccess`** (exactly the `TyUnion`
   arm), which the `:289` dispatcher then lowers — also no Elaborate change.
2. **Construction — qualified and unqualified, ONE path** (former R4+R5, merged). `inferRecord`
   takes **no expected type** (Infer.fs:105), so construction is bottom-up and an annotation only
   unifies against the result afterward — "annotated construction" is not a separate mechanism.
   Both the bare `{ X = … }` and qualified `{ R.X = … }` forms resolve through the **same** field
   label index:
   - Compute candidate `ResolvedRecord`s from the field set: local `recordsWithField` ∪ provider
     `TryRecordsWithField`, deduped by `TypeKey`, intersected per field.
   - **Qualifier path stays name-resolution** (F#/today): `tryRecord ctx.Types` first (LOCAL,
     byte-identical — do not route local qualified records through the field-set filter, which
     would change the mistyped-field diagnostic from "Type R has no field Q" to "no record
     matches"). On a local miss, THEN filter the field-set candidates by name==qualifier → external
     record. External-qualified is new, so its diagnostic is a fresh path (no regression), and the
     candidates carry `RecordName`, so no opens-aware name→key resolution is needed.
   - Apply the `ExactMatch` / `PartialMatches` verdict (below) → resolve or diagnose.
   - Construct from the chosen `ResolvedRecord`: local → the existing `RecordTypeInfo` path;
     external → fresh args + `FrozenTypeBridge.instantiateDeclaring` per field template, unify each
     initializer, return `TyRecord(extRecKey, args)`.

   Pattern position (`InferPat.fs:465`) shares `findUniqueRecordByFieldSet`, so record *patterns*
   resolve cross-unit by the same change. **Elaborate/codegen need no change** for the common case:
   `translateRecord` emits `TExpr.RecordCons(fields, ty, tok)` carrying identity on `ty`, and
   cross-file `recKey` re-homes to a local ctor. One deferred gap (below): `recordFieldTy`
   (`Resolve.fs:333`) is `LocalRecord`-only, so an external record with an explicitly `obj`-typed
   field skips `wrapObjArg` boxing — the *exact* existing limitation for external unions
   (`unionCaseFieldTys`, "Empty for an external union", `Resolve.fs:344`). Reified generics and
   concrete fields are unaffected; defer with a comment, consistent with unions.

   **The verdict** (pure core, no diagnostics): `{ ExactMatch: ResolvedRecord option;
   PartialMatches: ResolvedRecord list }`. PartialMatches = every record whose field set ⊇ the
   typed set (the intersection, deduped by `TypeKey`) — the LSP suggestion set. ExactMatch =
   `Some r` iff a *unique* PartialMatch has a field set **equal** to the typed set (which subsumes
   F#'s count tie-break: a superset with equal size ⟺ equal set). The checker wrapper owns all
   `ctx.Error`: `ExactMatch=Some`→resolve; else single-superset PartialMatch→resolve + defer the
   missing-field error (F# parity); else empty→"no record matches"; else→ambiguous. `ResolvedRecord`
   is a local|external unifier (`LocalRecord of RecordTypeInfo | ExternalRecord of
   ExternalRecordCandidate`) so construction dispatches on it. The pure core (dedup/intersect/
   classify) is what makes Phase-2 LSP a no-new-resolution add-on: it reads `PartialMatches`.

### RQA coupling (a real dependency)

F# excludes RQA records from `eFieldLabels`; `TryRecordsWithField` must exclude them too, or a
bare `{ X = … }` could construct an RQA record cross-unit (over-permissive — safe, never a
miscompile, but wrong). The obstacle: **the frozen tree does not model RQA today** —
`FrozenSignature.fs:177` hardcodes `IsRequireQualifiedAccess = false` for union cases with the
note "RQA is not modelled in the frozen tree," and the record projection (`FrozenSignature.fs:181`)
carries no RQA at all. So faithful exclusion requires threading the record's RQA flag into the
frozen `TTypeKind.Record` and out through the projection. Options:
- **Thread RQA through freeze** (correct, larger — touches the frozen tree and both the union
  and record projections; also closes the union-case RQA gap noted in the multi-file plan).
- **Ship over-permissive** (`IsRequireQualifiedAccess = false`, matching the existing union-case
  behaviour) and close it when the corpus has a cross-unit RQA record. Consistent with the
  multi-file plan's "Projection coverage boundaries" fail-safe stance.

**DECISION: deferred.** Ship over-permissive — `IsRequireQualifiedAccess = false` at the record
projection, matching the existing union-case hardcode (`FrozenSignature.fs:177`). The field is
present on `ExternalRecordCandidate` (the seam); the frozen RQA flag is the missing input. A WHY
comment at the projection **and** at `TryRecordsWithField`'s (currently no-op) RQA filter must
state the incompleteness: a cross-unit RQA record is wrongly constructible bare until R6 threads
RQA through the frozen tree. R6 (below) completes it, closing the union-case gap in the same cut.
The demonstration test — bare construction of a cross-unit RQA record wrongly accepted — is added
in **Step D of the multi-file plan** and flipped green by R6.

### Visibility / open scoping (confirm, don't over-build)

Local `recordsWithField` filters by `keyVisibleAt useSite` (forward-decl order within the file).
Provider candidates are fully-settled prior units, so no forward filter applies. **DECISION: deferred.** F#'s `eFieldLabels` holds only `open`-ed records, so bare field-set
resolution should only see records reachable *without a qualifier*. Phase 1 ships the looser
"any provider record with the field" (no `AmbientOpenPrefixes` / ambient-scope gate) — over-
permissive, same safety class as RQA (never a miscompile). A WHY comment at the local ∪ provider
union point in `findUniqueRecordByFieldSet` must state it: a cross-unit record in an unopened
namespace is wrongly resolvable bare until this is gated. The demonstration test is added in
**Step D of the multi-file plan** and flipped green when the ambient-scope gate lands.

## Ordered cuts (each builds green + committed separately)

- **R1** — `ExternalRecordCandidate` type + `IExternalSymbolProvider.TryRecordsWithField`, all
  existing providers return `[||]` (the test doubles in `CompositeTests`/`ConformanceTests`/etc.
  gain a one-line stub). Behaviour-preserving; no consumer yet.
- **R2** — `FrozenSignature.toProvider` builds a field-name → `ExternalRecordCandidate[]` index
  from the frozen records and answers `TryRecordsWithField`; the `stack`/`composite` seams union
  it (mirror the `TryLookupUnionCase` wiring). Parity-test the index vs a known frozen record.
- **R3** — record **field read**: provider fallback in `resolveFieldStep`'s `TyRecord` arm,
  mirroring the `TyUnion` arm. Field found → return type, NO `ExternalAccess` stamp; field miss →
  augmentation member (`TryLookupMember` + commit + stamp). Factor the member-commit if it would
  duplicate `TyUnion`/`TyClass` (`commitExternalMember`). SA-level cross-file field-read test +
  a cross-file runtime test (unit 1 factory returns the record, unit 2 reads `.X`) via
  `compileUnits` — the latter is where any codegen `ldfld` gap would surface.
- **R4 (construction — folds former R5)** — record construction through the provider, ONE cut for
  both qualified and unqualified. Introduce `ResolvedRecord` (`LocalRecord of RecordTypeInfo |
  ExternalRecord of ExternalRecordCandidate`) and the pure verdict core `{ ExactMatch;
  PartialMatches }` (local `recordsWithField` ∪ provider `TryRecordsWithField`, deduped by
  `TypeKey`, intersected). Rewrite `findUniqueRecordByFieldSet` from exact-set-equality to the
  intersection verdict; the qualifier (when present) filters candidates by name. `inferRecord`
  constructs from the chosen `ResolvedRecord` (local path unchanged; external via
  `FrozenTypeBridge.instantiateDeclaring`). Pattern position (`InferPat.fs:465`) falls out. Keep a
  local-only regression test proving byte-identical resolution for single-record-per-field-set
  cases. **Dedup candidates by `TypeKey`** — `stack.TryRecordsWithField` concatenates across
  sources without deduping (F#'s `eFieldLabels` lookup `ListSet.setify`s by tycon), so a record
  reachable through two compose layers must not count twice. External-`obj`-field boxing deferred
  (comment, consistent with unions). Tests: SA-level cross-file bare + qualified construction +
  record pattern; cross-file runtime (unit 2 builds unit 1's record, RUN).
  *Implemented as two green commits (highest-risk cut):* **R4a** — pure refactor of
  `findUniqueRecordByFieldSet` to the `ResolvedRecord`/`{ ExactMatch; PartialMatches }` verdict,
  LOCAL candidates only, proven byte-identical by the existing suite + a regression test; factor the
  per-field-type resolution so `inferRecord` and `inferPat` stop duplicating the local branch.
  **R4b** — add provider candidates to the core (∪, `TypeKey` dedup), external construction
  (`instantiateDeclaring`), and the qualifier-as-filter; the new cross-file tests land here.
- **R6 (optional, follow-up)** — thread RQA into the frozen tree; flip `TryRecordsWithField` to
  exclude RQA records; close the union-case RQA gap in the same cut.

## Scope: cross-FILE only; cross-PACKAGE deferred (cheaply)

This body of work targets **cross-file** records — the `FrozenSignature.toProvider` composite,
which is fully in-memory and available now (independent of publishing-format). Only
`FrozenSignature` gains a real `TryRecordsWithField` (R2); the `.fsi`-sourced providers
(`VesperLib` / `TyparCapture`) and the raw-IL provider (`MetadataSymbols`) keep the R1 `[||]`
stub, each with a WHY comment pointing here.

**Cross-package records are deferred as one coherent unit** — the provider fill AND codegen's
external-record emission together — because they land cheaply on the publishing-format plan
(`publishing-format-plan.md`), not on IL introspection:

- **Front end.** "Is this referenced type a record?" is a **contract** fact declared in the
  package's `.fsi` (PF1/PF8), not something to reverse-engineer from raw IL metadata. The
  `.fsi`-sourced (TAST-backed) provider knows recordness, field names/types, and RQA directly,
  so it answers `TryRecordsWithField` + `TryLookupType → Record` exactly as the cross-file
  composite does. Per PF3, the signature TAST **overrides** the raw `.dll` IL for that package —
  there is never an IL-introspection heuristic for recordness. So the front-end half is "fill the
  same method on the `.fsi` provider," no new mechanism.
- **Codegen.** Emitting a `newobj` / field-load against a referenced record needs a MemberRef /
  field-ref to another module — untested today (the front end never resolved an external record,
  so codegen never had to emit one). This is a separate codegen cut, taken with cross-package
  front-end fill, once the contract provider fully describes the record.

Until then the `[||]` stubs make cross-package bare-field-set construction simply *not resolve*
(a clean "unknown record" miss, never a miscompile).

## Test plan

Primary gates (both independent of publishing-format):
- **SA-level front-end resolution** (pure, no codegen — the correctness proof): via the
  `FrozenSignature` composite, unit N+1 constructs (qualified + bare field-set), field-reads,
  and pattern-matches a record declared in unit N, with **no** "Unknown record" diagnostics.
- **Cross-file runtime** via `Codegen.compileUnits` (C7, already landed — `CrossFileUnitsTests`):
  the same end-to-end, records re-homing to local `TypeDef`s (codegen known-correct).
- Unit: `TryRecordsWithField` index parity on a known frozen record (R2); intersection +
  tie-break on synthetic candidate sets incl. "same field count wins" and "ambiguous" (R5).
- Negative: bare literal whose field set matches no visible record; ambiguous field set.

Deferred (parked in multi-file Step D as red-until-complete demonstration tests): RQA
over-permissiveness (R6); unopened-namespace over-permissiveness (ambient-scope gate);
cross-package-DLL record construction (publishing-format follow-up).

## Anchors (current line numbers — verify before editing)

- `resolveFieldStep` `TyRecord` arm: `InferRecordAccess.fs:242`; sibling `TyClass` provider
  fallback (the template): `:276`; external-member commit helper: `:294`.
- `inferRecord`: `InferRecordAccess.fs:41` (qualifier at `:67`, field-set at `:73`).
- `findUniqueRecordByFieldSet`: `InferResolve.fs:163` (exact-set-equality at `:179`).
- Pattern-position field set: `InferPat.fs:465`.
- Local reverse index: `TypeRegistry.recordsWithField` `:1033`, over `types.FieldIndex`.
- Provider interface + `TryLookupUnionCase` precedent: `ExternalSymbols.fs:852`;
  `ExternalUnionCase` record shape: `:239`; `ExternalFieldShape`: `:157`.
- Compose seams: `ExternalSymbolProviders.fs` (`stack`/`composite`, the `TryLookupUnionCase`
  wiring at `:149` / `:330` / `:513` / `:574` / `:633`).
- Projection: `FrozenSignature.fs:181` (record), `:166` (union-case index, the RQA-hardcode
  precedent).

## Resume point

When this lands, the multi-file plan's Step C finding ("cross-file NOMINAL use is blocked
UPSTREAM") is closed for records; unions/classes were never blocked. Multi-file continues at
**Step D** (multi-file driver + delete concat) with a records-bearing corpus now compilable
cross-file.
