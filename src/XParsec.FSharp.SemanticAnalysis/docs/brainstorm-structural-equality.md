# Structural Equality and Hashing — design

Compile-time codegen + minimal runtime requirements for structural equality
(`=`, `<>`) and hashing (`hash`) in Vesper, targeting CoreCLR. **Ordering
(`<`, `compare`, `IComparable`) is out of scope** — this doc is equality +
hashing only.

Status: **design settled, not yet implemented.** Decisions below are locked
except where the "Open questions" section says otherwise. Promote to
`structural-equality-plan.md` (pass-by-pass, like `records-plan.md`) when
implementation starts.

## 1. Objective and principles

**Organizing principle: Data gets structural equality; State and Buffers get
reference equality.** "Data" is immutable (DUs, fully-immutable records,
`Block<'T>`, `string`, primitives) — compared by value. "State" (mutable
objects, `ref` cells) and "Buffers" (raw `T[]`, `Span`) are mutable — compared
by identity. The `Block<'T>`-vs-`T[]` split (§4) and the
record-equality-vs-reference question (§8) are both just this principle applied.
It also closes the mutable-key footgun by design: the mutable thing is
reference-keyed, so it can't corrupt a hash collection (§7.3).

Type-category mapping:

| Category | Mutability | Equality |
| :--- | :--- | :--- |
| DU | immutable by construction | **structural** |
| `Block<'T>`, `string`, primitives | immutable | **structural** |
| raw `T[]`, `Span`, buffers | mutable | **reference** |
| class instances, `ref` cells | mutable (State) | **reference** |
| record (all-immutable) | immutable (Data) | **structural** |
| record (any `mutable` field) | State | **no equality unless annotated** (§8) |
| function / closure | — | **none** (rejected, §3.1) |

Derived principles:

1. **One dispatch rule.** Equality/hashing of any structural value is
   `EqualityComparer<T>.Default.Equals/GetHashCode`. There is **no runtime
   intrinsic library** — the earlier draft's `Runtime.{Generic,Array}{Equals,Hash}`
   is deleted (see §4 for why it's unnecessary).
2. **Zero boxing for value types.** Lean on `EqualityComparer<T>.Default`
   devirtualization and `System.HashCode`; never cast through
   `IStructuralEquatable`.
3. **Structural arrays are a *type*, not a compiler rule.** Raw `T[]` keeps
   .NET reference semantics (it's a Buffer). A value-equal array is `Block<'T>`
   (§4) — an ordinary `IEquatable<T>` library type, so it needs no special
   interception.
4. **Compiler owns Data types' equality.** The compiler makes user DUs (and
   structural records) implement `IEquatable<T>` + override
   `Equals`/`GetHashCode` so they satisfy rule 1 recursively.

## 2. Current state (where this plugs in)

| Concern | Today | Gap for this work |
| :--- | :--- | :--- |
| `=` / `<>` typing | `op_Equality`/`op_Inequality` desugared from tokens (`Passes/Desugar.fs:36`), typed `t*t->bool` for **numeric primitives only** via `tryPrimitiveTraitCandidate` (`Passes/Unification.fs:651`) + a mono `int->int->bool` in `MockBuiltins` (`ExternalSymbols.fs:190`). | No path types `=` on a user record/DU. Must become polymorphic (§3.1). |
| `hash` | **Does not exist** as an intrinsic. | New `'T -> int` intrinsic (§3.1). |
| DU emission | Fully emitted — single class + `_tag` + per-case fields + factories (mono and generic `List\`1`), `Codegen.fs` union path. **No equality logic.** | Add the `IEquatable`/`Equals`/`GetHashCode` triple (§5). DUs are the first implementable slice. |
| Record emission | Front-end complete (`TyRecord`, `RecordCons`, …) but `TDecl.Type` records are **dropped in codegen** (handoff R6/P3e). | Record equality rides on R6 (record backend emission) landing first. |
| Runtime helpers | None. | None needed (§4). |
| `EqArray<'T>` | Compiler-internal structural array (`EqArray.fs`) — the proven prototype for `Block<'T>`. | `Block<'T>` is its language-level twin (§4). |

## 3. Compile-time dispatch (`=`, `<>`, `hash`)

### 3.1 Front-end typing

Make the operators polymorphic and **defer the "is this comparable?" check**
(project posture: relax the grammar/typing, reject later — cf.
`feedback_relax_parser_defer_to_typecheck`). No F#-style `equality` constraint
in v1.

- `op_Equality`, `op_Inequality` : `'T -> 'T -> bool`. When the
  primitive-trait candidate and the provider both decline, fall through to this
  polymorphic signature (rather than the current mono `int` form).
- `hash` : `'T -> int` (new intrinsic; resolves like `op_Equality`).
- A **Validation** check rejects equality/hash on types that don't satisfy
  `equality`. For **S1** that is just function types (`TyFun`); everything else
  (primitives, records, DUs, tuples, `Block`, BCL `IEquatable` types, raw `T[]`
  — reference-equal) is accepted. When records land (**S2**), this check
  generalises to the `equality`-satisfiability function materialized in the TAST
  (§5.3) — an all-immutable
  record satisfies it; a record with a `mutable` field does **not** unless
  annotated (§8) — applied recursively (an outer type containing a
  non-equality field doesn't satisfy it either). This is a bounded check at
  concrete `=`/`hash` sites and at record-emission (to decide whether to
  generate the equality triple), **not** inference-time constraint propagation:
  generic `let f x y = x = y` stays unconstrained and works at runtime via
  `EqualityComparer<'T>.Default`.

### 3.2 The single dispatch rule

For `e1 = e2` / `hash e` at the strictly-inferred operand type `T`, codegen
emits:

| `T` | `e1 = e2` | `hash e` |
| :--- | :--- | :--- |
| primitive (`int`, `float`, `char`, …) | `ceq` (existing primitive path) | existing primitive hash |
| **everything else** | `EqualityComparer<T>.Default.Equals(e1, e2)` | `EqualityComparer<T>.Default.GetHashCode(e)` |

"Everything else" covers records, DUs, `Block<'T>`, `string`, generic `'T`,
and raw `T[]` (which legitimately gets reference equality). The call site and
the per-field walk (§5.2) use the **same** rule, so there is exactly one
mechanism to implement. `EqualityComparer<T>.Default` is null-safe and, for a
type implementing `IEquatable<T>`, dispatches to the typed `Equals` with no
boxing.

`<>` is `not (e1 = e2)`.

**ValueTuple unroll (deferred).** F#-style tuples *could* be unrolled at
compile time — `(e1.Item1 = e2.Item1) && …` and `HashCode.Combine(…)` — to
avoid `EqualityComparer<ValueTuple<…>>` indirection. Deferred until tuple
emission is settled; until then tuples fall under the default rule. Listed in
§6 slices.

## 4. `Block<'T>` — the structural array type (replaces the "Runtime library")

The earlier draft introduced `Runtime.ArrayEquals/GenericEquals` so that `T[]`
fields compared structurally and a bare `'T` could be array-aware at runtime.
**Both are deleted.** Instead:

- Raw `T[]` keeps .NET reference equality. (Sound: mutable arrays in
  hash-keyed collections are a corruption footgun — §7.3 — so reference
  identity is the *safer* default for the mutable type.)
- A value-equal array is **`Block<'T>`**, a library type that implements
  structural equality itself and therefore satisfies §3.2's default rule with
  no special interception:

```fsharp
[<Struct>]
type Block<'T> = { private _values: 'T array }
    // structural Equals/GetHashCode over _values, element-wise via
    // EqualityComparer<'T>.Default (so nested Block / record / DU recurse),
    // default(Block) normalised to empty.
```

This makes `Block` the **only** place an array element-loop lives. The compiler
never special-cases arrays; it treats `Block<'T>` as an ordinary BCL-rule
type. Crucially `Block` is **not** subject to compiler equality-generation
(§5) — if it were, the field-walk would hit its private `'T array` and emit
reference equality. Its `Equals`/`GetHashCode` are hand-authored.

**Reference prototype.** `EqArray.fs` is this type, already in-tree for the
compiler's own TAST/`SemType` collections. `Block<'T>` is its language-level
twin. Differences and the parallel:

- Backing: `EqArray` uses `ImmutableArray<'T>`; `Block` uses `'T array` to keep
  Vesper.Core off `System.Collections.Immutable` (immutability is by
  construction — the field is private and never mutated post-construction).
- Both: `default` reads as empty (no NRE); element compare via
  `EqualityComparer<'T>.Default`; order-sensitive folded hash; `[<Struct>]`,
  one pointer wide, one heap allocation (the array).
- `EqArray`'s hash fold (`h*397 ^^^ cmp.GetHashCode x`) and `SequenceEqual`
  body are the template for `Block`'s.

**Where it lives / language.** Vesper.Core, Tier 1 (per
`minimal-core-lib-plan.md`). Authored as a **C# interim** first (it needs
`EqualityComparer<'T>.Default`, a loop, and `System.HashCode` — beyond the
current self-host rung), rewritten in Vesper when the ladder reaches it. This
is the only runtime artifact this whole feature adds, and it's one library
type, not an intrinsics namespace.

## 5. Type generation (records and DUs)

The compiler makes every emitted record and DU:

1. implement `System.IEquatable<T>` (`Equals(T other)`),
2. override `bool Equals(object? obj)`,
3. override `int GetHashCode()`.

### 5.1 Records

`Equals(T other)`: `ReferenceEquals(other, null)` guard, then each field by
§3.2 (`EqualityComparer<FieldTy>.Default.Equals(this.F, other.F)`),
short-circuit `&&`. `GetHashCode`: `System.HashCode` accumulator,
`hash.Add(this.F)` per field (`HashCode.Add<T>` itself uses
`EqualityComparer<T>.Default`, so this is the same rule), `ToHashCode()`.
`Equals(object?)` is `obj is T t && this.Equals(t)`.

Generated **only for fully-immutable records** if §8's mutable-record fork
resolves to the principle (a record with a `mutable` field is State → inherits
`Object` reference equality, no triple generated). Records depend on R6 record
backend emission landing first.

### 5.2 DUs (first slice — the only aggregate emittable today)

The existing layout is one class + `int _tag` + per-case payload fields
(`<Case>_<i>`). Equality/hash walk that layout:

- `Equals(T other)`: null guard → `this._tag = other._tag` guard → `switch
  (_tag)` comparing **only the active case's** payload fields by §3.2; a
  nullary case (no fields) is equal once tags match.
- `GetHashCode()`: `HashCode` seeded with `_tag`, then `hash.Add` the active
  case's fields; `ToHashCode()`.
- `Equals(object?)`: `obj is T t && this.Equals(t)`.

Nested DUs/records recurse for free (their fields hit §3.2, which calls *their*
overridden `Equals`). Generic DUs (`List\`1`) defer to a later slice — a field
of the type's own typar `'T` is fine via `EqualityComparer<!0>.Default`, but
the member sigs must be written in the type's `!0` (the R2 generic-member
machinery), so it's incremental work on top of mono DUs.

### 5.3 Equality satisfiability lives in the TAST

The "does this type satisfy `equality`?" verdict is **materialized in the
TAST**, not recomputed in the backend — one algorithm, three consumers:

- **Validation** computes it to diagnose `=`/`hash` on an unsatisfiable type;
- **Freeze** stamps the per-type verdict onto the type declaration;
- **Codegen** reads the stamp to decide whether to emit the
  `IEquatable`/`Equals`/`GetHashCode` triple (and which kind) — no
  re-derivation.

Same posture as `TastFile.IntrinsicReprTypes`: a front-end verdict carried for
the backend. Additive shape:

```fsharp
[<RequireQualifiedAccess>]
type EqualitySupport =
    /// Value equality. Compiler-owned decl ⇒ emit the triple; primitive /
    /// BCL `IEquatable` / tuple / `Block` ⇒ intrinsic, nothing to emit.
    | Structural
    /// Identity equality; `=` allowed, no triple emitted (inherits `Object`).
    /// `[<ReferenceEquality>]`, classes.
    | Reference
    /// `=` / `hash` rejected. Function-typed, mutable-unannotated record,
    /// `[<NoEquality>]`.
    | None
    // | Custom  — reserved for [<CustomEquality>]; later, additive.

// TTypeDecl gains:  Equality: EqualitySupport
```

`support : SemType -> EqualitySupport` is the shared derivation:
- primitive / `string` / `Block<'T>` → `Structural`
- `TyFun` → `None`
- `TyTuple items` → `None` if any item is `None`, else `Structural`
- `TyRecord` / union → that decl's stamped `Equality`
- generic instantiation → `Structural` (see simplification below)

**Computed where:** a `ctx.EqualitySupport` registry (name-keyed, like
`ctx.RecordTypes`) filled after Unification; Validation reads it to diagnose,
Freeze copies each verdict onto its `TTypeDecl` so the frozen TAST is
self-contained for the backend.

**Vesper simplification — no conditional/typar-dependent equality.** F# carries
a third "equatable *iff* the type args are" state (`'T list` is equatable iff
`'T` is). Vesper doesn't need it: `EqualityComparer<'T>.Default` is **total** at
runtime, so a generic type uniformly *has* equality regardless of its args. The
one leak: a function smuggled through a generic (`Box<int -> int>`) gets
reference equality at runtime instead of a compile error — accepted as a v1
trade for a flat 3-valued enum over a typar-constraint system. Direct `None`
cases (a bare function field, a mutable-unannotated record) are still caught
because `support` sees them structurally. Revisit only if the leak bites.

**Target-capability caveat (CLR vs JS).** "Total at runtime" is a **CoreCLR
capability, not a language invariant**: `System.Object` gives every value a
`GetHashCode`/`Equals`, so `hash`/`=` resolve on everything and identity hash is
free for State (the `[<ReferenceEquality>]` types). Treat **universal hashing +
identity hash as a required target capability** — declared per target like the
`extern` primitives (`prim-types-min.fsi`;
[fsi-target-brainstorm](fsi-target-brainstorm.md)). A target that provides it
keeps this flat, total model; one that cannot loses the simplification and
**converges on comparison's §7c arg-recursion** (and makes State non-hashable).
**JS provides it, but not for free:** no root `GetHashCode`, no native object
identity int, no `EqualityComparer<T>.Default`, so `Core.JS` must ship the small
equality/hash/`compare` **runtime dispatcher this design deleted for CLR (§4)** —
primitives hashed by authored functions, identity hash via a `WeakMap<object,int>`
+ counter, generated structural `equals`/`hash` recursed through it. Semantics
stay uniform with CLR; the cost is that one shim. See
[operators-plan](operators-plan.md) "Target portability of `hash`".

## 6. Implementation slices (dependency order)

All BCL-only; no slice needs a runtime intrinsic.

1. **S1 — Mono-DU equality + `=`/`<>`/`hash` dispatch.** §3 front-end (poly
   ops + `hash` + Validation reject of `TyFun`), §3.2 call-site dispatch, §5.2
   DU generation. Complete vertical slice with no prerequisite. *DUs with only
   primitive / nested-compiler-owned fields work end-to-end with zero runtime
   additions.* Introduces the `EqualitySupport` scaffold (§5.3) in its minimal
   form: DUs → `Structural`, `TyFun` → `None`.
2. **S2 — Record equality.** §5.1, gated on R6 record backend emission. Fills
   the recursive `support` derivation and the `ctx.EqualitySupport` registry
   (§5.3) — immutable records `Structural`, mutable-unannotated `None`.
3. **S3 — `Block<'T>`.** Author the type (C# interim), Tier 1. Independent of
   S1/S2; needed before any structural-array field is useful.
4. **S4 — Generic DUs/records.** Equality member sigs in the declaring typar
   (`!0`), via R2 generic-member machinery.
5. **S5 — ValueTuple compile-time unroll.** Once tuple emission settles.

`[|…|]` array-literal surface (does a literal mean `T[]` or `Block`?) is a
separate language-surface decision — see Open questions — not part of any slice
here.

## 7. Edge cases and defined behavior

### 7.1 Floating-point NaN
Adopt BCL `EqualityComparer<double>.Default` semantics: **`NaN = NaN` is
`true`** in structural contexts (records, DUs, `Block`). This **diverges** from
F#'s bare `=` on floats (where `nan = nan` is `false`); the uniform BCL rule is
the deliberate, simpler choice and is what `EqualityComparer`/`HashCode` give.

### 7.2 Cyclic object graphs
No visited-set tracking. `=`/`hash` on a cyclic graph → `StackOverflowException`
(defined, matches legacy F#, keeps the hot path allocation-free).

### 7.3 Mutability
Structural equality reads current state, so mutating a field after use as a hash
key corrupts the collection. §1's principle is the systematic defence: every
mutable thing is reference-keyed and therefore immune — raw `T[]`/buffers
always, and (if §8 resolves to the principle) records with `mutable` fields
too. `Block` is structural but immutable by construction, so it's safe. The
residual footgun exists only for a structural type whose author defeats
immutability through an escape hatch (`[<StructuralEquality>]` on a mutable
record), which is then their explicit choice.

### 7.4 `default(Block<'T>)`
The zero value has `_values = null`; normalised to empty (length 0) for both
equality and hash — never an NRE. (Same as `EqArray.Underlying`.)

### 7.5 BCL collections
`List<'T>`, `Set<'T>`, `Map<'K,'V>` etc. implement their own `IEquatable`/value
semantics where applicable and fall under §3.2's default rule — no special
interception.

## 8. Open questions

- **Mutable records — punt via fail-closed annotation (leaning).** Rather than
  silently pick structural or reference for a record with a `mutable` field,
  make it **not satisfy `equality` at all** — `=`/`hash` on it is a compile
  error — *unless* the author annotates `[<ReferenceEquality>]` (→ reference,
  no triple generated, inherits `Object`) or `[<StructuralEquality>]` (→
  structural, triple generated, author owns the §7.3 footgun). All-immutable
  records satisfy `equality` (structural) with no annotation; DUs unaffected
  (always structural). This defers the *default-semantics* decision by forcing
  intent, and lets real code reveal whether the annotation requirement is too
  noisy before committing. Mirrors F#'s attribute machinery; diverges only in
  the default (F# auto-structurals a mutable record). **Cost:** brings back the
  `equality`-satisfiability check (§3.1) — but it only bites at S2 (records,
  R6), so S1 mono-DUs are unaffected and **punting now is free.**
- **Raw `T[]` — silent reference equality vs. diagnose.** Current decision:
  **silent reference equality** (`[|1|] = [|1|]` ⇒ `false`), with `Block` the
  structural opt-in. Alternative: a Validation hint when `=` is applied to a
  bare `T[]` ("did you mean `Block`?"). Defaulting to silent; revisit if it
  surprises in practice.
- **Array-literal surface.** Does `[|1;2;3|]` produce `T[]` or `Block<'T>`?
  Parallels the list-literal consumer-driven typing already in the unifier
  (R3). Out of scope here; decide with the collection-literal surface work.
- **`EqArray` vs `Block` unification.** Keep parallel (compiler-internal vs.
  language-level, different backing) for now. Reconsider sharing an impl only
  if it pays off; the layers have different dependency budgets.
- **`hash` collision quality / `HashCode` vs hand fold.** `System.HashCode`
  (randomized per-process seed) for generated records/DUs; `EqArray`'s fixed
  `h*397` fold is fine for `Block`. No cross-process hash stability is promised
  (matches BCL).
- **Equality constraint — local check, no inference propagation.** S2 brings a
  *local* `equality`-satisfiability check (concrete `=`/`hash` sites +
  record-emission), materialized in the TAST as `EqualitySupport` (§5.3), which
  the mutable-record rule above needs. It does
  **not** add inference-time constraint *propagation* across generic boundaries:
  `let eq (a:'T) (b:'T) = a = b` stays unconstrained and works at runtime via
  `EqualityComparer<'T>.Default`. Full propagation (so the constraint surfaces
  on `'T` and gets checked at the *call* site) is a later call worth making only
  if generic equality code becomes common — it's about earlier diagnostics, not
  capability.
