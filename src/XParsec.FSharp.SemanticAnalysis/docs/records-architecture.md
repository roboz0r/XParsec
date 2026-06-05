# Records — architecture

How the semantic-analysis pipeline and the CLR back end handle F#
records, as built. Supersedes `records-plan.md` (the original landing
plan): the implementation has since gained generics, equality/comparison
postures, augmentation members, and external (referenced-package) record
resolution, and the codebase moved to the `SemType` / `FrozenType` split,
so the plan's data model and file:line anchors no longer describe the
code. File:line references here are anchors, not contracts — they drift.

Records share their machinery with discriminated unions and classes (see
[`du-architecture.md`](du-architecture.md)): one named-type registry
entry per declaration, field-set-driven inference, and pattern support
threaded through every pass. The three nominal kinds (`TyRecord` /
`TyUnion` / `TyClass`) carry the same shape — a resolved `SymbolKey` plus
a type-argument vector — and most unification sites treat them with one
combined arm. A record is, for the back end, "a union with one nameless
case and no tag", so most of codegen is reuse rather than new machinery.

The canonical surface handled today:

```fsharp
// Definition with one mutable field, optionally generic.
type Point = { X: int; mutable Y: int }
type Box<'T> = { Value: 'T }

// Literal — the field-name set uniquely identifies the type.
let p = { X = 1; Y = 2 }            //  p : Point
let b = { Value = 1 }              //  b : Box<int>

// Field access — driven by the receiver's type.
let xCoord = p.X                   //  xCoord : int

// Field assignment — allowed because Y is mutable; p.X <- 5 diagnoses.
p.Y <- 5

// Clone — copies p, overrides Y, types as the source record.
let p2 = { p with Y = 10 }         //  p2 : Point

// Pattern — destructures by field name (may omit fields).
let { X = x; Y = y } = p           //  x : int, y : int

// Disambiguation when two records share a field set.
type Vec = { X: float; Y: float }
let v : Vec = { X = 1.0; Y = 2.0 }      // annotation
let v2 = { Vec.X = 1.0; Y = 2.0 }       // qualified prefix

// External (referenced-package) records resolve through the provider —
// e.g. Vesper.Ref<'T>, the cell `let mutable` promotes into.
```

## Data model

### `SemType.TyRecord` (`SemanticInfo.fs:311`)

```fsharp
| TyRecord of key: SymbolKey * args: EqArray<SemType>
```

Identical shape to `TyUnion` (`:314`) and `TyClass` (`:318`). Field data
is **not** stored inline — it is reachable from the record registry by
`key`. The `key` is an arity-qualified `SymbolKey` carrying the type's
home assembly + declaring namespace; `args` is the type-argument vector
for generic records. Two `TyRecord`s unify iff their keys are equal
**and** their args unify pairwise (`Engine.fs`). `FrozenType` has the
post-freeze counterpart `FTRecord`; surviving open typars are rewritten
to `TyTypar` at freeze time.

### Registry (`SideTables.fs`)

`RecordFieldInfo` (`:44`) — one per field: `Name`, `Type` (declaration
type; starts as a placeholder `TyVar`, linked during fill-in),
`IsMutable`, `DeclKey`.

`RecordTypeInfo` (`:56`) — one per declaration:

- `Name`, `Key` (arity-qualified `SymbolKey`, minted by
  `stampLocalTypeKey` to match emitted metadata), `Fields`, `DeclKey`.
- `TypeParams : EqArray<string * TypeVar>` — declared typars in order,
  paired with source names. Each is a *prototype* TyVar, substituted out
  at every use site so independent instantiations get independent
  variables; a bare `'a` field type shares identity with the matching
  `TypeParams` entry.
- `TyparConstraints` — `when 'a : …` clauses, attached to the prototype
  TyVars during fill-in.
- `EqualitySupport` (default `Structural`) / `ComparisonSupport`
  (default `NoComparison`) — postures decoded from attributes at
  registration, read by `checkConstraint`, projected onto the TAST
  type-decl for codegen.

Augmentation members (`with member …`) live in the shared
`TypeMemberInfo` (`:120`) shape, the same one unions and classes use.

Storage lives on `PassContextTypes` (`SideTables.fs:453`):

- `Record : Dictionary<string, RecordTypeInfo>` (`:456`) — by record name.
- `FieldIndex : Dictionary<string, EqArray<RecordTypeInfo>>` (`:471`) —
  reverse index, field name → bucket of records declaring it. Used by
  literal / pattern field-set inference; an intersection across all of a
  literal's field names that leaves more than one record means the set is
  ambiguous and needs a qualifier or annotation.

### `TypeVar.PendingDotAccess` (`SemanticInfo.fs:500`)

The deferred-resolution channel for field access on a not-yet-pinned
receiver. A `r.X` whose receiver types to a free TyVar parks a
`DeferredMemberAccess` (`:453` — member name, use-site key, result TyVar)
on the TyVar's root. When `unify` later links that root to a `TyRecord`
(or `TyClass`), the list drains and each access resolves; anything still
pending at end of analysis is a Validation diagnostic. Generalised from
records' original pending-field-access gate to cover class members too —
hence "dot access", not "field access".

## Pass-by-pass

### NameResolution — registration (`Passes/NameResolution/TypeRegistration.fs`)

`registerRecordTypeDefn` (`:119`), driven by `registerRecordTypes`
(`:199`), walks each `TypeDefn.Record`:

- Single-segment type names only; multi-segment is skipped.
- Builds a `RecordFieldInfo[]` with one placeholder `TyVar` per field and
  the field's `IsMutable` flag.
- Mints the arity-qualified `Key` via `stampLocalTypeKey`, decodes the
  equality/comparison attributes, and registers into `ctx.Types.Record`.
- Appends the record to each of its fields' `FieldIndex` buckets.
- Arity-qualified duplicate check (a name collision, including a clash
  with a union of the same name, diagnoses).

Run order (`NameResolution.fs`): records, then unions, then classes, then
the expression walks — so a literal can resolve to a record declared
later in source. Record field names are not part of lexical scope; `r.X`
never resolves `X` through `ctx.Binding`.

### Unification

**Field-type fill-in** (`Passes/Unification.fs`). After the registry is
populated, `walkElems` runs `fillRecordFieldTypes` (alongside the union /
class fill passes) before any expression is typed: it sets each record's
typar scope, translates every field's CST type, `Link`s the placeholder
field TyVar to it, and attaches `TyparConstraints` to the matching
prototype TyVar. A field type may reference another type declared
elsewhere in the file, since the whole registry is populated first.

**Inference** (`Passes/Unification/Infer.fs`):

- **Literal** — `inferRecord` (`:849`). Resolves the target record by
  qualifier (`Vec.X`) or by intersecting the `FieldIndex` buckets of the
  literal's field names; validates the field set matches; unifies each
  initialiser against the (instantiated) declared field type; returns
  `TyRecord(info.Key, args)` with fresh typar instantiation (`:916`).
- **Field access** — the `Expr.DotLookup` arm (`:73`). Types the
  receiver; on `TyRecord` looks the field up and returns its instantiated
  type; on a free `TyVar` parks a `DeferredMemberAccess` on the root
  (`:1086`) and returns the result TyVar; otherwise diagnoses.
- **Clone** — `inferRecordClone` (`:918`). Resolves the source to
  `TyRecord(recKey, srcArgs)`, validates each override field belongs to
  the record, unifies each override expression, and re-returns the
  source's `TyRecord` (`:942`).
- **Field assignment** — ordinary `Expr.Assignment`: the `Expr.DotLookup`
  LHS infers to the field type and unifies with the RHS. The
  mutability check is Validation's.
- **Patterns** — `Pat.Record` (`InferPat.fs`). Mirrors the literal:
  field-set / qualifier / scrutinee-driven record resolution, then each
  sub-pattern unifies against its field's type. A pattern may list a
  strict subset of the fields.

**Engine arms** (`Passes/Unification/Engine.fs`): `zonk`,
`occursAndAdjust`, `substituteWith`, and `unify` (key + arg-vector
equality) all carry the combined nominal-kind arm. `migrateBounds` on a
TyVar–TyVar union appends `PendingDotAccess` (`:93`); the
`TyVar → TyRecord` link drains it. `checkConstraint` reads
`EqualitySupport` / `ComparisonSupport` and recurses into field types so
`r1 = r2` / `r1 < r2` diagnose against a `NoEquality` / un-annotated
record. `translateType` (`Translate.fs`) maps a `Type.NamedType` to
`TyRecord` for both local and external records.

### Regions (`Passes/Regions.fs`)

`isAllocation` returns `true` for `TyRecord _` (`:172`) — a record
literal mints a composite region that outlives each field initialiser's
region, the same shape as a tuple. `TExpr.FieldGet` shares the receiver's
region (`:366`): field reads and the record itself live together. This is
coarser than the plan's per-mutable-field cell regions but sound — a
mutable field's escape is tracked at record granularity. The escaping-
mutable case is handled upstream by `RefCellPromotion` (see Back end §7),
which rewrites the cell into a `Vesper.Ref<'T>` record before this pass
sees it.

### Validation (`Passes/Validation.fs`)

Two diagnostics:

- **Assignment to an immutable field** (`:89`, `:130`). An `r.X <- v`
  whose receiver zonks to a `TyRecord` whose field `X` has
  `IsMutable = false` diagnoses "Cannot assign to immutable field 'X'".
- **Unresolved dot access** — `checkUnresolvedDotAccesses` (`:150`).
  After unification, walks every TyVar root with a non-empty
  `PendingDotAccess` (the receiver type never pinned to a record/class)
  and emits one diagnostic per pending access (`:157`).

Non-Ident, non-`DotLookup` LHSes (array slot, deeper dotted access) stay
out of scope; assignment validation handles the single-segment field case.

### Freeze / TAST (`Tast.fs`, `FreezeExpr.fs`)

Four `TExpr` cases and one `TPat`:

```fsharp
// Tast.fs
| RecordCons  of fields: EqArray<string * TExprG<'ty>> * ty: 'ty        // :142
| RecordClone of source: TExprG<'ty> * overrides: EqArray<string * TExprG<'ty>> * ty: 'ty   // :147
| FieldGet    of receiver: TExprG<'ty> * fieldName: string * ty: 'ty    // :150
| FieldSet    of receiver: TExprG<'ty> * fieldName: string * value: TExprG<'ty> * ty: 'ty   // :153
// TPat
| Record of fields: EqArray<string * TPatG<'ty>> * ty: 'ty             // :46
```

`ty` on each is the `TyRecord` (for `FieldGet`/`FieldSet`, the field's
type); the declaring record is recovered via the registry at consumption
time. Freeze translation (`FreezeExpr.fs`): `Expr.Record → RecordCons`
(`:1005`), `Expr.RecordClone → RecordClone` (`:1016`), a `.X` step folds
to `FieldGet` (`:669`, the multi-segment `r.X.Y` chain at `:1289`), a
field-LHS `Expr.Assignment → FieldSet`, and `Pat.Record → TPat.Record`
(`:218`). A dotted access that resolves to an *external* member becomes a
keyed `TExpr.ExternalMember` instead of a project-local `FieldGet`.

The declaration itself freezes to `TDecl.Type { Kind = TTypeKind.Record
… }` (`Tast.fs:328` — `Record of fields * members`), carrying the field
list and augmentation members read off the registry.

## Back end (CLR codegen)

A record is the DU back end minus the tag, so most of this is reuse
(`project_du_structural_equality_ceq1`, `project_generic_du_equality_s4`).

1. **Partition.** `partitionTypeDecls` (`AssemblerScaffold.fs:41`) routes
   each `TTypeKind.Record` into the records bucket; `Codegen.fs` emits
   them from `asm.RecordDecls`.
2. **`TypeDef` emission** (`NominalEmit.fs:187`). A sealed reference
   class (`TypeAttributes.Sealed | Public`) with one **public** field per
   record field (a `mutable` field is the same field — mutability is a
   setter concern, not a layout one) and a constructor taking the fields
   in **declaration order**. Generic records reuse the generic-union
   machinery verbatim: `GenericFieldSignature`, the ambient `!0`
   type-typar set (`SetTypeTypars`), and the field/ctor refs on the
   type's `TypeSpec`. Augmentation members emit through the shared
   `buildMember` path.
3. **Value-level IL** (`EmitExpr.fs`). `RecordCons` (`:855`) reorders the
   source-order initialisers into declaration order and `newobj`s the
   ctor; `FieldGet` (`:899`) emits `ldfld`; `FieldSet` (`:921`) emits
   `stfld` and reifies `unit`; `RecordClone` (`:935`) stashes the source
   in a local, loads each field via `ldfld` unless overridden, and
   `newobj`s — no BCL `MemberwiseClone`, so it stays BCL-only and works
   for generic records. `TPat.Record` lowers (`EmitLower.fs`) to a
   tag-free `ldfld`-per-field recursion (a record match never fails on
   shape, only on its sub-patterns).
4. **Structural-equality triple** (`Emit.fs:564`). `buildRecordEquals`
   (`Equals(object)`) + `buildRecordEqualsTyped` (`IEquatable<Self>`) +
   `buildRecordGetHashCode` — the DU triple minus the tag-compare /
   tag-seed. Field compare/hash uses `EqualityComparer<F>.Default` /
   `System.HashCode`; generic records reuse the ambient `!0` machinery.
   Emission is gated on the `EqualitySupport` verdict (`NominalEmit.fs`):
   `Reference` / `NoEquality` emit no triple.
5. **Structural comparison** (`Emit.fs:786`). `buildRecordCompareTo` /
   `buildRecordCompareToObj` — `CompareTo(Self)` + `IComparable.CompareTo`
   plus the `IComparable<Self>` / `IComparable` interface rows, per-field
   `Comparer<F>.Default.Compare` with first-non-zero short-circuit.
   **Opt-in**: only an explicit `[<StructuralComparison>]` produces the
   pair; the decoder is `decodeComparisonAttributes` (`Attributes.fs:99`)
   and the verdict gates emission.
6. **Attribute gating** (`Passes/Attributes.fs`).
   `decodeEqualityAttributes` (`:69`) reads
   `[<StructuralEquality>]` / `[<ReferenceEquality>]` / `[<NoEquality>]`;
   default is `Structural` for an all-immutable record, `Reference` for a
   mutable one. The verdict feeds both the typar-constraint checker
   (use-site `=` diagnoses) and codegen.
7. **External records** (`ICodegenProvider.fs:209`, `ClrProvider.fs:291`).
   `TryEmitRecordCons` and `TryResolveExternalRecordField` resolve a
   record declared in a referenced package (keyed by the resolved
   symbol's `Origin`, not by name) through the external-symbol stack;
   `EmitExpr`'s `RecordCons` / `FieldGet` / `FieldSet` fall through to the
   provider when the local table misses. This is the path
   `RefCellPromotion` relies on: every `let mutable x = init` whose
   binding escapes (`HeapShared`) is rewritten into
   `{ contents = init } : Vesper.Ref<'T>` against the real
   `Vesper.Ref\`1` `TypeDefinition` in `Vesper.Core.dll`, so any user
   package can publish a generic record with no further compiler changes.

## Tests

- `NameResolutionTests.fs` — registration, `FieldIndex`, duplicate
  diagnostics.
- `UnificationTests.fs` — literal field-set inference, ambiguity +
  qualifier, field-set mismatch, field access on annotated parameter /
  on a later-pinned TyVar, clone-types-as-source, clone field validation.
- `ValidationTests.fs` — immutable-field assignment, mutable-field clean,
  unresolved-vs-resolved field access.
- `RegionsTests.fs` — record-literal allocation classification.
- Codegen `RecordTests.fs` — construct + field-get, clone overrides one /
  shares the rest, mutable-field round-trip through `FieldSet`,
  structural equality, generic `Box<'T>` at `int` and `string`,
  no-FSharp.Core-dependency.
- `EqualityAttributeTests.fs` — `EqualitySupport` verdict gating.
- `StructuralComparisonTests.fs` — the comparison pair, field order,
  opt-in gating.
- `CapturedMutableTests.fs` — the `let mutable` → `Vesper.Ref` promotion
  cross-cut (the external-record path).

## Known gaps

- **Per-field region precision.** Mutable-field escape is tracked at
  record granularity (a field shares its record's region), not per cell.
  Sound but coarse; the escaping-mutable case is covered by the
  `RefCellPromotion` rewrite rather than by field-level cells.
- **Cross-file record resolution.** `ctx.Types.Record` is a per-file
  table. Records used across modules / namespaces land with that work;
  the write-once registry overlays a provider-backed catalogue when it
  does. Cross-*package* user records (e.g. `Vesper.Ref`) already work via
  the provider (Back end §7).
- **Anonymous records** (`{| X = 1 |}`). Different runtime shape — the
  type *is* its field set, with no declaration to register. A follow-up.
- **Struct records** (`[<Struct>] type R = …`). Same field-resolution
  story, different region/codegen treatment (inline, not heap). Lands
  with structs.
- **Field-level access modifiers** (`private mutable X`). v1 ignores the
  `access` token; lands with module visibility.
- **Record-pattern exhaustiveness** (`match r with { X = 1 } -> …`). Part
  of the broader pattern-completeness work in Validation.
- **BCL types treated as records.** Surfacing an arbitrary `.NET`
  record-shaped type as a `TyRecord` lands with the .NET provider's
  named-type catalogue; only same-package user records are in scope.
