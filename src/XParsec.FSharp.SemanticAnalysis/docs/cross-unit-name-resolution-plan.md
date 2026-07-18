# Cross-unit name resolution — remaining gaps (post-records)

*Cross-file **records** resolve end to end, and the three name-resolution gaps that were open
when this plan was written have since landed too — type-annotation by `open` (was item 1),
cross-unit `[<RequireQualifiedAccess>]` on records + union cases (was item 2), and the
ambient-scope / `open` gate on bare record construction (was item 3). Their design lives in the
code and its tests, not here. What remains below are the gaps deliberately deferred until a
corpus demands them, plus the separate publishing-format track — **none is a live miscompile.**
The item numbers are kept as originally written so the WHY comments at each seam still match.*

## Two safety classes

- **INCOMPLETE** — a *valid* cross-unit program fails to resolve. User-visible; fix on demand.
  Items 4 (obj-field boxing), 5 (enums), 6 (interface members).
- **OVER-PERMISSIVE** — an *invalid* program wrongly resolves; **never a miscompile**. Item 6
  (member-level accessibility).
- **SEPARATE TRACK** — item 7 (cross-package records) rides `publishing-format-plan.md`.

Every deferred item already carries a WHY comment at its seam in the code; this plan is the
design record behind those comments.

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

When it lands, note that a bare record literal's open-scope gate
(`InferResolve.admitsBareExternalRecord`) already consults the ambient prelude (via the tail of
`ctx.Resolution.OpenScope`), so a prelude-auto-opened package record gate-admits bare — worth a
test at that point.

## Anchors (verify before editing)

- obj-field boxing: `Elaborate/Resolve.fs:333` (`recordFieldTy`) / `:344` (`unionCaseFieldTys`).
- Cross-package: `publishing-format-plan.md` PF1/PF3/PF8.
