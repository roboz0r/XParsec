# Top-down type registration

Scope: replace the nine-scan type-registration section of `NameResolution.walkElems`
with a single top-down scan that enforces F#'s file-order scoping by construction.

Delete this file once the work lands.

## The rule being implemented

F# type scoping is strictly ordered:

- A type may reference types declared **above** it.
- A type may **not** reference types declared below it.
- The one exception is a `type X = … and Y = …` recursive group, whose members see
  each other.

The AST already carries exactly this unit: `ModuleElem.Type of ImArr<TypeDefn<'T>>`
(`src/XParsec.FSharp/Declarations.fs:37`) — one `ModuleElem.Type` **is** one
recursive group. The registration pass currently discards that structure.

## What is wrong today

`NameResolution.walkElems` (`Passes/NameResolution.fs:601-660`) runs **nine** scans
over the whole element list before any body is walked:

| # | scan | line |
|---|------|------|
| 1 | `noteNominalTypeNames` | 611 |
| 2 | `registerTypeIdentities` | 620 |
| 3 | `registerRecordTypes` | 630 |
| 4 | `registerUnionTypes` | 633 |
| 5 | `registerEnumTypes` | 636 |
| 6 | `registerAbbreviationTypes` | 639 |
| 7 | `registerClassTypes` | 642 |
| 8 | `registerInheritedSlots` | 648 |
| 9 | `registerNominalMembers` | 655 |

This is not merely redundant. Scans 2–9 exist **in order to grant whole-file forward
visibility**, which is not F# semantics. The code states the intent plainly:

- `Passes/NameResolution/TypeRegistration.fs:267` — *"so a type may reference another
  declared anywhere in the file"*
- `Passes/NameResolution.fs:645` — `registerInheritedSlots` is a separate late pass
  *"so a parent declared later resolves"*

Both accept programs F# rejects (`type A = { x: B }` above `type B`; `type Derived() =
inherit Base()` above `type Base()`). The "unions must finish before classes" ordering
constraint documented at `NameResolution.fs:625` is an artefact of the same mistake —
under file-order scoping a class below a union sees its cases and a class above does
not, which is the correct answer rather than a constraint to engineer around.

Second-order costs of the split:

- The five kind registrars each re-derive from the CST the tuple `registerTypeIdentities`
  already computed (name, `NodeKey`, arity, the `Idents.Length <> 1` guard), then
  recover their identity via `TypeRegistry.tryOwnIdentity` (`TypeRegistry.fs:359`).
  Call sites: `TypeRegistration.fs:348` (record), `:497` (union), `:594` (enum),
  `:675` (abbrev), `MemberRegistration.fs:487` (class).
- `tryOwnIdentity` returning `ValueNone` registers **nothing, silently** — so any drift
  between the arity a registrar computes and the arity the identity pass claimed is an
  undiagnosed dropped type. `TypeIdentity.DeclKey` exists solely to support this
  round-trip.
- Three registrars re-find themselves by **bare name**, which the `TypeKey` re-key made
  ambiguity-intolerant. On an arity-overloaded type (`Box\`1` / `Box\`2`, a supported and
  tested feature) `tryClass` / `tryUnionBare` / `tryRecord` return `ValueNone` and the
  declaration's detail is silently discarded:
  - `MemberRegistration.fs:822` — the `inherit` clause; `BaseType` stays `ValueNone`.
  - `MemberRegistration.fs:900` / `:911` — the `with member …` augmentation block.

## Target structure

One scan, in source order. A `ModuleElem.Type` group is the unit of mutual recursion and
gets its own registration algorithm (below).

```fsharp
for w in elems do
    match w.Elem with
    | ModuleElem.Type defs -> registerGroup ctx w.Containment defs
    | _ -> ()
```

`registerDetail` dispatches on `TypeIdentity.Kind` and is handed the identity it needs,
so no registrar re-derives anything and none can be reached for a rejected duplicate.

### Registering a recursive group

Intra-group references split into two tiers, and only the second needs deferral.

**Tier 1 — satisfied by identity alone.** A nominal head in a field type, a member
signature, or a type-argument position needs only the referent's `TypeKey` and arity to
build its `SemType`. Claiming *every* identity in the group before registering *any*
detail satisfies all of these outright. `type A = { x: B } and B = { y: A }` needs
nothing further.

**Tier 2 — needs the referent's registered detail.** Two constructs read detail, not
identity, and so cannot be answered at the point a forward reference is seen:

- **`inherit` clauses.** `MemberRegistration.resolveInheritParent` reads
  `ClassTypeInfo` for a local parent and `IntrinsicReprTypes` for a heritable extern
  base — both written by the *detail* pass, not the identity pass.
- **Abbreviation right-hand sides.** `registerAbbreviationDefn` writes
  `IntrinsicReprTypes` / `HeritableExternBases`, which the above then reads. An abbrev
  whose RHS names a later member of its own group has the same problem.

So the group algorithm is three phases:

1. **Claim.** Mint the `TypeKey` and claim `(name, arity)` for every def in the group,
   in source order. Duplicates diagnose here and are excluded from the group's working
   set (see *Rejected duplicates* below). After this phase every type in the group is
   nameable, which discharges tier 1 entirely.
2. **Register detail, in source order.** A tier-2 reference to a group member that is
   not yet registered does **not** diagnose — it records a **pending slot** (a mutable
   cell on the referring type's info) and continues. A reference to anything *outside*
   the group and *below* it in the file is simply unknown and diagnoses immediately, as
   it should: nothing later can fill it.
3. **Close the group.** Fill every pending slot from the now-complete group detail. Any
   slot still unfilled diagnoses `unknown type '<name>'` — and at this point that
   diagnostic is exactly true, because the group is the last thing that could have
   filled it.

The mutable slot is not new machinery: `ClassTypeInfo.BaseType` is already
`ValueSome/ValueNone`-shaped and already filled after the fact by
`registerInheritedSlots`. This narrows that fill from a whole-file late pass to a
group-close step, which is what makes the scoping rule correct rather than merely
deferred.

### Cycle checks become group-local

Under file-order scoping a cycle **across** groups is unrepresentable — a type can only
name what is above it, and a cycle needs a back-edge. So every cycle that can still exist
is a cycle among one group's own members, over a graph at most the size of the group.

These are user-writable programs, so each needs a real diagnostic, not a `failwith`
backstop. The reference compiler's behaviour was confirmed with `dotnet fsi` probes;
match it:

| source | F# diagnostic |
|---|---|
| `type A() = inherit B() and B() = inherit A()` | `FS0954` — immediate cyclic reference through a struct field or inheritance relation |
| `[<Struct>] type A = { x: B } and [<Struct>] B = { y: A }` | `FS0954` — same code |
| `type A = B and B = A` | `FS0953` — immediate cyclic reference through an abbreviation |
| `type A = { x: B } and B = { y: A }` (reference types) | **legal** — no cycle error |

Two things to take from this:

- **The cycle is through *inheritance* edges and *struct-field* edges, not all field
  edges.** A cycle through a reference-type record field is legal (the indirection breaks
  it) — that is why the `and`-joined record pair compiles. Building the check over all
  field edges would reject a legal program.
- **Abbreviation cycles are a separate error class** (`FS0953`) from inheritance/struct
  cycles (`FS0954`), so they are two checks, not one.

`checkInheritanceCycles` as a whole-graph late sweep (`NameResolution.fs:651`) is
replaced by the group-close check.

### The diagnostics to match

All confirmed against `dotnet fsi`. Writing a throwaway `.fsx` and running
`dotnet fsi --nologo x.fsx` is the cheapest oracle for "what should this do" — use it
rather than inventing a message.

| source | F# diagnostic |
|---|---|
| `type A = { x: B }` then `type B = …` (separate groups) | `FS0039` — **The type 'B' is not defined** |
| `type Derived() = inherit Base()` then `type Base()` (separate groups) | `FS0039` — The type 'Base' is not defined |
| `type Foo = …` twice | `FS0037` — Duplicate definition of type, exception or module 'Foo' |

The first row is the whole point of this work: a forward reference across groups is a
plain **unknown-type** error, not a special "forward reference" error. So the group-close
sweep for unfilled slots does not need a bespoke message — it emits the ordinary
unknown-type diagnostic, which is exactly what F# reports. The current implementation
accepts both of the first two programs; after step 2 it must reject them.

### Rejected duplicates

A duplicate is diagnosed in the claim phase and then **excluded from the group's working
set**, so no registrar is ever reached for it. It is recorded in a separate
`RejectedDuplicates` collection on `PassContextTypes` for observability only — nothing in
name resolution, unification, or codegen may read it.

The consequence is intended: the first `type Foo` wins the claim, the second registers
nothing, and a reference to a member that existed only on the second fails member lookup
against the first. That must surface as a clean diagnostic (`member 'Bar' not found on
'Foo'`), never a crash. Verify the member-lookup path diagnoses rather than throwing; if
it throws, fix the diagnostic — do **not** fix it by letting the duplicate partially
register.

Consequences, all of which are deletions:

- `tryOwnIdentity` and `TypeIdentity.DeclKey` are unreachable — delete both.
- The five `Idents.Length <> 1` guards and five arity re-derivations collapse into
  `claimIdentity`. The `TypeDeclKind.Enum -> 0` rule, currently spelled twice
  (`TypeRegistration.fs:290-293` and again at `:594`), is stated once.
- The three bare-name lookups above become unrepresentable: a registrar is *given* its
  `TypeIdentity`, so it cannot look itself up at all. This fixes the arity-overload
  silent-drop bugs without a separate change.
- `CtorIndex` (written only by the union registrar, `TypeRegistration.fs:555-564`) fills
  in file order. Registration-time reads are all membership tests
  (`Scope.fs:181`, `:220`; `Elaborate/Patterns.fs:51`, `:168`; `Elaborate/Resolve.fs:397`);
  the payload readers are all in Unification (`InferResolve.fs:123`, `:134`;
  `InferPat.fs:40`, `:175`), which runs downstream. No ordering constraint survives.
- `registerInheritedSlots` (scan 8) folds into the group algorithm: a parent above
  resolves at once, a parent inside the group fills at group close, a parent below is a
  diagnostic. `checkInheritanceCycles` (scan 8's whole-graph late sweep) becomes the
  group-close cycle check.
- `registerNominalMembers` (scan 9) folds into `registerDetail`.

### The one scan that survives

`noteNominalTypeNames` (scan 1) must stay a pre-scan. `moduleHolderName`
(`TypeRegistration.fs:116`) needs the unit's full nominal-name set before the first key
is minted, because a module's compiled holder name (`List` vs `ListModule`) depends on
whether *any* same-named type exists in the unit. That is a name-set scan, not a
registration scan, and it should be the only survivor.

Note while here: `NominalTypeNames` is a `HashSet<string>` of **bare short names**
(`TypeRegistry.fs:161`), so it is namespace- and module-blind — a `type List` in one
namespace forces `module List` in an unrelated namespace to compile as `ListModule`.
Tracked separately in `docs/thermo-review-938dd9da34.md`; not a blocker here.

## Order of work

1. **Fold the kind registrars into one `registerDetail`, driven from the identity claim.**
   Keep the existing nine-scan driver in place; just make each registrar take a
   `TypeIdentity` instead of re-deriving one. Green here means `tryOwnIdentity` has no
   callers left.
2. **Collapse the driver to a single top-down scan.** This is the step that flips
   forward-reference programs from accepted to rejected.
3. **Delete the ordering scaffolding**: `registerInheritedSlots`,
   `TypeIdentity.DeclKey`, `tryOwnIdentity`, the whole-graph cycle sweep.

Tests that currently rely on forward visibility will go red at step 2. They are pinning
the wrong semantics; fix them against the corrected registration once it is in place.

## Landmine

`walkElems` also orders module-lets against `fillClassMembers` (ctor params fill
mid-pass), and that ordering is load-bearing and easy to break. The work above touches
only the *type-registration* section and must not disturb the module-let /
`fillClassMembers` axis. Land steps 1 and 2 as separate commits and confirm green
between them.
