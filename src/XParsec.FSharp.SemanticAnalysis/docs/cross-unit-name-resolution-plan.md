# Cross-unit name resolution — remaining gaps (post-records)

*Cross-file **records** now resolve end to end — field read, construction (bare + qualified),
and patterns all fall back to the composite `IExternalSymbolProvider` like classes and unions
do (that work landed; its design lives in the code, not here). This plan is the consolidated
home for the cross-unit nominal / name-resolution gaps that were deferred while records landed,
plus one newly discovered gap. It supersedes and lets us delete `cross-unit-record-resolution-plan.md`.*

**Scheduling: after `multi-file-compilation-units-plan.md` Step D.** Item 1 (type-annotation) is
the only *incomplete* gap likely to bite a real cross-file corpus soon — investigate it first.
The rest are over-permissive (safe, never a miscompile) or close on corpus demand.

## Two safety classes

- **INCOMPLETE** — a *valid* cross-unit program fails to resolve. User-visible; fix on demand.
  Items 1 (type-annotation), 4 (obj-field boxing), 5 (enums), 6 (interface members).
- **OVER-PERMISSIVE** — an *invalid* program wrongly resolves; **never a miscompile**. Items 2
  (RQA), 3 (ambient-scope), 6 (member-level accessibility). Each has a red-until-fixed
  demonstration test scheduled in multi-file Step D.
- **SEPARATE TRACK** — item 7 (cross-package records) rides `publishing-format-plan.md`.

Every deferred item already carries a WHY comment at its seam in the code; this plan is the
design record behind those comments.

## 1. Type-annotation by `open` (INCOMPLETE — investigate first)

A type used in **annotation / signature position** whose declaration lives in a *prior unit*
does not resolve: `let r : R = …` or `(r : R)` where `type R` is declared in an earlier file
and brought in by `open`. Discovered while writing the R4b-2 record-pattern test, which
sidesteps it by *constructing* the scrutinee rather than annotating it; the pre-existing
`AssemblyUnitsTests` assertions filtered only `"Unresolved"` messages and never caught it.

**Not records-specific** — it is the type *name* in annotation position, so it affects any
cross-unit nominal (record / union / class) used in a signature, `val`, parameter, or return
annotation.

**Resolution site.** `Translate.translateType` → `resolveNamedGeneric` (`Translate.fs:590`)
resolves an external type ONLY through a NameResolution *stamp*
(`tryResolveExternalTypeStamped`); with no stamp it falls to `resolveLocalNamedGeneric`
(`:600`, local `ctx.Types` via `tryTypeClaim`) and, on a local miss, to `unresolvedHeadTy`
(a fresh `TyVar` + the "unresolved" diagnostic). So a bare annotation type name from a prior
unit unresolves because nothing stamped it external and the local registry never held it.

**Investigate → fix.** Determine WHY expression-position external types get stamped
(`Scope.fs` sets `ResolvedType` / `ExternalStaticReceiver` / `ExternalUnionRecordQualifier` for
ctor-sugar heads, static receivers, qualifiers, ~`Scope.fs:654-698`) but an annotation-position
type name does not. Then either (a) have NameResolution stamp external type names in annotation
position so `tryResolveExternalTypeStamped` fires, or (b) give `resolveLocalNamedGeneric` a
composite-provider fallback (`ctx.Provider.TryLookupType` by name) mirroring the record/union
field/member arms. Prefer whichever keeps the stamp-outranks-registry contract intact
(`resolveNamedGeneric`'s doc: "a stamp is NameResolution's committed external verdict"). Add a
`(r : R)` cross-unit SA test (red until this lands).

## 2. RQA threading (OVER-PERMISSIVE — was the record plan's R6)

The frozen tree does not model `[<RequireQualifiedAccess>]`. `FrozenSignature` hardcodes
`IsRequireQualifiedAccess = false` for BOTH union cases (`FrozenSignature.fs:~170`, "RQA is not
modelled in the frozen tree") and record candidates (the R2 index). So a cross-unit RQA union's
case resolves *bare* (F# requires `Color.Red`, not `Red`) and a cross-unit RQA record is
constructible by bare field set (F# requires the qualifier). F# excludes exactly these from its
unqualified indexes — `isILOrRequiredQualifiedAccess` guards `eFieldLabels`
(`fsharp/src/Compiler/Checking/NameResolution.fs:1280`).

**Fix (one cut, closes record + union-case RQA together).** Thread the RQA flag through freeze
into `Frozen.TTypeKind.Record` and the union kind, out through the projection; then the seams
already exist to honour it — `ExternalRecordCandidate.IsRequireQualifiedAccess` (drop RQA
records from `TryRecordsWithField`, or flag them so bare construction is rejected) and
`ExternalUnionCase.IsRequireQualifiedAccess` (already read by `ExternalUnionCase.matchesQualifier`
/ `Scope.resolvesAsBareExternalCase`). **Demonstration test (Step D):** bare construction of a
cross-unit RQA record is wrongly accepted → flips green here.

## 3. Ambient-scope / `open` gate (OVER-PERMISSIVE)

Bare nominal resolution should only see nominals reachable *without a qualifier* — F#'s
`eFieldLabels` holds only `open`-ed records. Phase 1 ships the looser "any provider record with
the field": `recordFieldSetVerdict` unions ALL provider `TryRecordsWithField` candidates with no
`AmbientOpenPrefixes` / ambient-scope filter (WHY comment at the union point in
`InferResolve.fs`). So a cross-unit record in an *unopened* namespace is wrongly resolvable by
bare field set. If item 1 takes the provider-fallback route, its bare form needs the same gate.

**Fix.** Gate provider candidates by the ambient/open scope (`AmbientOpenPrefixes`) the pipeline
already threads. **Demonstration test (Step D):** a cross-unit record in an unopened namespace
wrongly resolves bare → flips green here.

## 4. External-`obj`-field boxing (INCOMPLETE — matches the existing union limitation)

`translateRecord`'s `recordFieldTy` (`Elaborate/Resolve.fs:333`) is `LocalRecord`-only, so an
external record with an explicitly `obj`-typed field skips `wrapObjArg` boxing — the **exact**
existing limitation for external unions (`unionCaseFieldTys`, "Empty for an external union",
`Resolve.fs:344`). Reified generics and concrete-typed fields are unaffected; this only bites a
literal-position `obj`-typed field on a cross-unit nominal. **Fix:** extend `recordFieldTy` /
`unionCaseFieldTys` to read the external shape through the provider so the box fires; close the
record and union arms together.

## 5. Enum registration (INCOMPLETE — projection boundary)

`FrozenSignature` leaves enum types unregistered, so a cross-unit enum falls back to a nominal
`TyConst` (also listed under the multi-file plan's "Projection coverage boundaries"). **Fix:**
project enums as records/unions are. Closes when the corpus references a cross-unit enum.

## 6. Interface-member decurrying + member-level accessibility (projection boundaries)

From the multi-file plan's "Projection coverage boundaries": interface member surfaces publish
name/arity but not decurried members (INCOMPLETE for a cross-unit interface call), and
member-level accessibility is not captured, so a `member private` leaks across the unit boundary
(OVER-PERMISSIVE — the `.fsi` extractor is public-only here). Close each when the corpus
references it.

## 7. Cross-package records (SEPARATE TRACK — publishing-format)

The `.fsi`-sourced providers (`VesperLib` / `TyparCapture`) and the raw-IL provider
(`MetadataSymbols`) keep `TryRecordsWithField = [||]`, so a record declared in a *referenced
package* is not constructible / field-readable cross-package (a clean "unknown record" miss,
never a miscompile). Closing it is **one coherent unit** — the front-end provider fill AND
codegen's external-record `newobj`/field emission — that lands **cheaply on
`publishing-format-plan.md`**, not on IL introspection: recordness is a `.fsi` *contract* fact
(PF1/PF8), and per PF3 the signature TAST overrides the raw `.dll` IL for that package, so the
`.fsi`-sourced provider answers `TryRecordsWithField` + `TryLookupType → Record` exactly as the
cross-file composite already does. Tracked there; pointer only.

## Anchors (verify before editing)

- Type-name resolution: `Translate.resolveNamedGeneric` `:590`, `resolveLocalNamedGeneric` `:600`,
  `resolveQualifiedTypeName` `:549`; external stamp readers `Scope.fs:~654-698`.
- RQA seams: `FrozenSignature.fs` (union-case index ~`:170`, record index in the `Record` arm);
  `ExternalRecordCandidate.IsRequireQualifiedAccess`, `ExternalUnionCase.IsRequireQualifiedAccess`
  (`ExternalSymbols.fs`); F# guard `NameResolution.fs:1280`.
- Ambient-scope: the local ∪ provider union in `InferResolve.recordFieldSetVerdict`.
- obj-field boxing: `Elaborate/Resolve.fs:333` (`recordFieldTy`) / `:344` (`unionCaseFieldTys`).
- Cross-package: `publishing-format-plan.md` PF1/PF3/PF8.
