# Plan: `[<Struct>]` discriminated unions (correct-first)

Ephemeral. Delete once landed. Struct **records** already emit as `System.ValueType`-based value
types (through the existing value-type machinery — record equality/comparison gained value-type IL
variants); this plan is the struct **union** half, which needs designing before it is attempted.

Relationship to `brainstorm-du-layout.md`: that doc designs the PERFORMANCE-optimal overlapping
"split payload" layout. This plan deliberately does NOT implement it first. See "Layout choice".

## Correctness spec (probed via `dotnet fsi`, `tmp/su1..su2.fsx`)

A `[<Struct>]` union is:
- a **value type** (no null; has a `default` — tag 0, all fields zero);
- **tag-discriminated**, one tag per case; nullary cases are tag-only (`su2`);
- **structurally equatable/comparable** (`One 1 = One 1` true, `One 1 = Empty` false — `su2`);
- **generic-capable** (`[<Struct>] type G<'T> = Val of 'T | Num of int` — `su2`).

**F#'s layout is primitive: `size = sum(all cases)`.** It does NOT overlap fields by offset.
The single affordance is that two cases' fields with the **same name AND type** collapse to one
shared slot — which is exactly why FS3585 exists ("all fields with the same name must be of the
same type"; unnamed fields collide via the generated `Item` name). Verified:
`A of x:int | B of x:string` is FS3585 (`su1`). That sharing is a modest size affordance, **not a
correctness requirement** — see below.

## The enabling fact — unions already emit FLAT (and the fork this introduces)

This compiler does NOT use F#'s base-class + per-case-subclass hierarchy. A union already emits
as ONE nominal type (`Layout.fs:792-845`, `NominalEmit.fs:116-128`):

```
_tag : <int>                                   (FieldKey.UnionTag)
<one field per (case, field-index)>            (FieldKey.UnionCaseField(key, case, i))
<one factory method per case>                  (MethodKey.UnionFactory(key, case))
nominalNode TypeSlotKind.Union td fields methodRows
```

So a **struct union is the same flat shape emitted as a value type**, not a new emission model.
A struct cannot inherit, which is why the correct-first version is tractable — flat is the ONLY
option for a struct, so it is correct and PERMANENT for the struct case.

**But flat is an early hack for REFERENCE unions**, and the eventual state is F#'s base-class +
per-case-subclass hierarchy for them (better devirtualization, per-case fields not co-resident,
`:?>` case-typed downcasts). The value-kind flag this plan adds is therefore the FORK POINT, not
just a struct marker: `struct ⇒ flat value type` (this plan) and `reference ⇒ hierarchy`
(the deferred reference-union-hierarchy refactor — see "Related future work"). So implement the
flag as the discriminator between two union representations, and keep the flat path OWNED by
struct-ness, so a later reference-union-hierarchy change forks cleanly rather than having to
disentangle a shared flat emission.

## Layout choice — flat, no sharing, no overlap (and why)

There are three layouts on a spectrum; pick the simplest correct one first.

| layout | rule | vs F# |
|---|---|---|
| **flat, per-case (ours, first-cut)** | every `(case, index)` field its own slot | ≥ F# by the shared-field savings only |
| flat, name+type shared (F#'s) | same-name-same-type fields collapse to one slot | = F# |
| overlapping split-payload (`brainstorm`) | differently-named fields overlap by offset, GC-safe | < F#, beyond F# |

Take **flat per-case** first:

- Fields are ALREADY keyed per `(case, index)` (`FieldKey.UnionCaseField`), so this is the
  existing reference-union field set emitted as a value type — the least new code. It is a hair
  larger than F# (no same-name sharing) and correct. We therefore do NOT enforce FS3585:
  `A of x:int | B of x:string` is representable here where F# rejects it — more permissive, still
  correct, consistent with correct-semantics-over-F#-parity.
- **Name+type sharing** (matching F#'s `sum` exactly) is a small additive optimization — dedup
  the field set by `(name, type)` — deferrable, and it only ever SHRINKS the layout, never
  changes semantics.
- **Overlapping** (`brainstorm-du-layout.md`) is pure perf, gated on GC/generic-overlap safety (a
  `'T` slot may overlap only an identical `'T`; a ref slot may not overlap unmanaged bytes), and
  emits a layout F# never does. Deferred; the brainstorm stays as its design.

Net first-cut layout = the existing reference-union field set, emitted as a sealed value type.

## Work items

1. **Value-kind on `TUnionG`.** The union decl carries no value kind at any level: `TUnionG`
   has no `ValueKind` field (`TRecordG`/`TClassG` do), so `TTypeDeclG.DefnKind` reads a
   `[<Struct>]` union as `TypeDefnKind.Union` (`TastDecl.fs`, the sited comment on the
   `TTypeKindG.Union` arm) — struct-ness is registration-side only, and every post-freeze
   consumer (the `AttributeVerdicts` legality matrices and views, the `AttrTarget` projection,
   both backends) sees a reference union. Add the field to `TUnionG`, set it at freeze, carry
   it through the codec, and thread it into `Layout`'s union arm and `TypeSlotKind.Union`
   (which take none today; only the class path has `ClassValueKind`).
2. **`UnionTypeInfo.IsValueType`** (SA): add the flag (currently only `ClassTypeInfo` has it),
   set at union registration from the `[<Struct>]` attribute (the same `classAttrs.IsValueType ||
   isStructShape` predicate `MemberRegistration.fs:604` already uses for classes; the struct-field
   cycle check already reads struct-ness off the CST via `isValueTypeDefn`), and thread it to the
   codegen IR through Elaborate (mirror the class path at `Elaborate.fs:1623`).
3. **CLR emission as a value type.** Route a struct union through the EXISTING value-type
   machinery: `RegisterUserValueType td.Key` (`Assembler.fs:160`), `System.ValueType` base +
   sealed via the struct-class layout path. The field set and factories are unchanged in shape.
   - **Factories return by value** — construct the struct (`initobj` + `stfld` on a local, or a
     value ctor) and return it, not `newobj` a heap ref.
   - **Tag / field access via address-of** — a value-type field read is `ldloca`/`ldflda` +
     `ldfld`, not `ldfld` on a ref. Pattern-match tag reads and case-field reads both change.
   - **Boxing at `obj`/interface boundaries** — reuse whatever the struct-class path already does
     (verify it covers unions).
4. **`default` semantics.** A struct union has a valid `default` (tag 0 → first case, fields
   zero). Confirm `Unchecked.defaultof<U>` and any zero-init path produce the tag-0 case and do
   not crash pattern matching. This is the one genuinely new semantic vs reference unions.
5. **Generic struct unions.** `[<Struct>] type G<'T> = Val of 'T | Num of int` — the `'T` field
   is an ordinary generic value-type field; confirm the generic value-type registration path
   (`RegisterGenericClass`/value-type variants) covers it.
6. **JS backend.** No-op by design: JS has no value types (it ignores `ClassValueKind` entirely),
   so a struct union emits as the same reference object a normal union does. Value/copy semantics
   is a pre-existing JS limitation shared with struct classes — document it, don't fake it.

## Verify

- Cross-backend conformance (`test/Codegen.Conformance/`): a standalone struct-union `.fs`
  compiled through BOTH backends, compared on stdout. Cases: nullary + payload mix; multi-field
  case; generic struct union; structural equality (`=` both ways); pattern match extracting a
  payload; `default`/zero value reaching a match.
- CLR-specific: assert the emitted type is a value type (`System.ValueType` base, sealed) via the
  metadata reader, per repo convention (assert on IL, not just reflection).
- Probe any uncertain F# semantic on a throwaway `tmp/*.fsx` first — do not infer.

## Open questions

1. **`default` reaching a match** — F# treats the zero value as the tag-0 case. Confirm our
   pattern-match lowering handles a struct union value whose tag is 0 with zeroed payload, and
   decide whether an "uninitialized struct union" needs any guard (F# does not add one).
2. **FS3585 relaxation** — confirm we are content being more permissive than F# (distinct-type
   same-name fields across cases). Recommended: yes, per correct-over-parity; revisit only if a
   consumer wants F#'s size profile, which is the name-sharing optimization, not this plan.
3. **Struct-union constraints F# imposes that we should still reject** — e.g. F# forbids a struct
   union with only nullary cases from also being `[<Struct>]`? (Not observed; probe before
   assuming.) A recursive struct union (`[<Struct>] type T = C of T`) is an infinite value type —
   F# rejects it (FS0954-style); our struct-field cycle check should already catch it, but pin it.

## Related future work

**Reference unions → F#-style class hierarchy.** The current flat single-type emission is an
early hack for reference unions; F# emits a base abstract class with a sealed subclass per case
(nullary cases as singletons). Moving to it is a separate refactor, unblocked by THIS plan's
value-kind fork: once struct-ness selects the representation, reference unions can adopt the
hierarchy without touching the struct path. Worth its own plan when taken up — it changes case
construction (`newobj` the case subclass), matching (`isinst`/tag), field access (on the
case-typed subclass), and both backends' union emission.

## Not in scope (deferred)

- The reference-union hierarchy above — a separate refactor.
- The overlapping split-payload optimization (`brainstorm-du-layout.md`) — pure perf, GC/generic
  overlap safety, a separate effort once the flat version is correct and pinned.
- Name-based field sharing (F#'s FS3585 behaviour) — a size optimization on the flat layout.
