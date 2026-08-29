# Cross-unit name resolution — remaining gaps (post-records)

*Cross-file **records** resolve end to end, and the name-resolution gaps that were open
when this plan was written have since landed too — type-annotation by `open` (was item 1),
cross-unit `[<RequireQualifiedAccess>]` on records + union cases (was item 2), the
ambient-scope / `open` gate on bare record construction (was item 3), **cross-unit enum
projection (item 5)**, and **cross-unit interface-member uncurrying + dispatch and member-level
accessibility (item 6)**. Their design lives in the code and its tests, not here. What remains below are the gaps
deliberately deferred until a corpus demands them, plus the separate publishing-format track —
**none is a live miscompile.** The item numbers are kept as originally written so the WHY
comments at each seam still match.*

## Two safety classes

- **INCOMPLETE** — a *valid* cross-unit program fails to resolve. User-visible; fix on demand.
  Item 4 (obj-field boxing). (Item 6's interface-member uncurrying was here; it has since
  landed — see below.)
- **OVER-PERMISSIVE** — an *invalid* program wrongly resolves; **never a miscompile**. Item 6's
  member-level accessibility was this class; it has since landed (see below).
- **SEPARATE TRACK** — item 7 (cross-package records) is tracked in `publishing-format-plan.md`.

Every deferred item already carries a WHY comment at its seam in the code; this plan is the
design record behind those comments.

## 4. `obj`-field boxing (records DONE; union cross-unit blocked by the union-construction gap)

A value assigned to an `obj` field (`{ X = 5 }` into `X: obj`, `C 5` into `C of obj`) is boxed
by `wrapObjArg`. F# accepts both via an implicit box; the original framing ("external records skip
`wrapObjArg`; extend the box functions through the provider") was WRONG — the box home was
unreachable because record field-init unified via *symmetric* `unify`, which rejected the value→obj
coercion before codegen (no miscompile — a clean type error).

**Records — DONE (local + cross-unit).** Record field-init now COERCES via `unifyArg`
(`InferRecordAccess.fs`, the argument-position rule already used by ctor / union-case slots), so
`{ X = 5 }` into an `obj` field type-checks. `recordFieldTy` (`Elaborate/Resolve.fs`) grew the
external arm so the box fires for a cross-unit record too — without it the cross-unit case
type-checked but emitted invalid IL (a real `InvalidProgramException`). Tested local + cross-unit,
IL round-trip through `:?>`.

**Unions — local DONE, cross-unit deferred.** A LOCAL `C 5` into `C of obj` already boxes
(union-cons uses `unifyArg` in `InferCtor`; `unionCaseFieldTys`'s local arm feeds the box). The
CROSS-UNIT case is blocked by the separate union-construction resolution gap (`Unresolved
identifier` / unresolved TyVar — not obj-specific; SCOPE NOTE in `CrossFileUnitsTests.fs`). When
that lands, `unionCaseFieldTys` needs the SAME external arm `recordFieldTy` now has, or a cross-unit
union `obj` case-field will type-check without a box — add it there with the box test the gap
currently prevents.

## 5. Enum registration — DONE

`FrozenSignature` now projects a frozen enum's closed case→literal table to an
`ExternalTypeShape.Enum` under its nominal key (mirroring the record / union arms), so a later
file resolves `(x: E)` / `E.Ci` against it instead of falling back to a nominal `TyConst`. The
shape is exactly what the consuming side already reads (`NameResolutionTypeRefStamp.tryExternalEnumCaseKey`
scans shapes — no case index needed). Numeric cases carry their `int64` value, string cases
their text; the integral kind is dropped (`ExternalEnumCaseValue` has none — external enums
are a JS-target feature and never reach CLR codegen). An unresolved case (`ValueNone`) is
dropped, matching the TS-manifest arm and `TEnumCases.classify`. Tested in
`FrozenSignatureTests` (numeric + string projection); the consuming stamp path is covered by
`ExternalEnumCaseStampTests` against the identical shape. The `.fsi` contract extractor still
produces no `Enum` shape (a TS-manifest-only shape today — see `TypeTranslate` line ~470);
cross-package enums are tracked on the separate publishing-format track alongside item 7.

## 6. Interface members: uncurrying DONE; member-level accessibility DONE

The original framing bundled two *unrelated* concerns — abstract-slot **uncurrying** (interface
members) and **member accessibility** (any type's `member private`). They share no code seam;
split here.

**Interface-member uncurrying — DONE (front end + codegen, cross-file).** The premise "interface
member surfaces publish name/arity but not decurried members" was already stale for the front end:
all three providers uncurry each abstract slot to an `ExternalMember` — `FrozenSignature`'s
`Interface` arm (`abstractMemberOf` → `memberFromParts`), the `.fsi` extractor's
`TypeSignatureElement.Abstract` arm (`extractTypeMembers`), and the metadata reader's
`enumerateClassMembers`. The consumer resolves + dispatches the slot cross-file (a grounded
`objArg.M` and a `'T :> IFace` bound both reach the provider's decurried member). The remaining
gap was CODEGEN: the interface's own nominal was resolved EXTERNAL-first, so a same-assembly
cross-file interface (home-stamped to our OWN assembly) minted an `AssemblyRef`-scoped `TypeRef`
back to ourselves — a self-`AssemblyRef` (non-fatal; the CLR resolves it, but the cross-file
self-ref invariant rejects it). Fixed by making both handle seams LOCAL-first (probe `userTypes`
before `externalClassRef`), matching the authority `encodeType`'s nominal arms already apply:
`ClrProvider.InterfaceHandleOf` (the `interface … with` `InterfaceImpl` row) and
`ClrExternalMembers.externalMemberRef` (the dispatch member-ref parent; the member analogue of
`localModuleFns`). `externalMemberRefOn` already re-homed (it parents through `encodeType`).
Proven end to end in `CrossFileUnitsTests` (a grounded `objArg.GetVal()` and a `'T :> IGetVal`
bound, both RUN and both carrying the full `peAssemblyRefs` self-ref guard the record tests use).

**Member-level accessibility — DONE.** A member's declared accessibility is now carried on
`TTypeMemberG.Accessibility` (captured in `Elaborate` from `MemberDefn.Member.access` — the
member-level `private`/`internal` token, NOT the inner `Binding.access`, which is always absent
for a member; an auto-property's own `member val private X` token wins via `autoPropertyAccess`),
and `FrozenSignature.membersOf` drops `Private` on the SAME internal-or-better threshold
`exported` applies to top-level entities. So a cross-unit `x.PrivateMember` no longer
resolves — it errors — while `internal`/public members stay same-assembly visible. The design
lives in the code and its test (`AssemblyUnitsTests`, the paired public/private dispatch case).
The `.fsi` extractor was already public-only here (a signature file lists no private members), so
only the frozen file→file projector needed the filter.

## 7. Cross-package records (SEPARATE TRACK — publishing-format)

The `.fsi`-sourced providers (`VesperLib` / `TyparCapture`) and the raw-IL provider
(`MetadataSymbols`) keep `TryRecordsWithField = [||]`, so a record declared in a *referenced
package* is not constructible / field-readable cross-package (a clean "unknown record" miss,
never a miscompile). Closing it is **one coherent unit** — the front-end provider fill AND
codegen's external-record `newobj`/field emission — that lands **cheaply on
`publishing-format-plan.md`**, not on IL introspection: recordness is a `.fsi` *contract* fact
(PF1/PF8), and per PF3 the signature TAST overrides the raw `.dll` IL for that package, so the
`.fsi`-sourced provider fills `TryRecordsWithField` + `TryLookupType → Record` exactly as the
cross-file composite already does. Tracked there; pointer only.

When it lands, note that a bare record literal's open-scope gate
(`InferResolve.admitsBareExternalRecord`) already consults the ambient prelude (via the tail of
`ctx.Resolution.OpenScope`), so a prelude-auto-opened package record gate-admits bare — worth a
test at that point.

## Anchors (verify before editing)

- obj-field boxing: `Elaborate/Resolve.fs:333` (`recordFieldTy`) / `:344` (`unionCaseFieldTys`).
- interface-nominal re-home (DONE): `ClrProvider.InterfaceHandleOf` /
  `ClrExternalMembers.externalMemberRef` — LOCAL-first (`userTypes` before `externalClassRef`).
- member accessibility (DONE): `Tast.fs` `TTypeMemberG.Accessibility` / `FrozenSignature.membersOf`
  (filter) / `Elaborate` `MemberDefn.Member.access` (capture site).
- Cross-package: `publishing-format-plan.md` PF1/PF3/PF8.
