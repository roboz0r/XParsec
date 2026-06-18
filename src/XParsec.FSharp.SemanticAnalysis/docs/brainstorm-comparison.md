# Structural Comparison (Ordering) — design

Compile-time codegen + minimal runtime for ordering (`<`, `>`, `<=`, `>=`,
`compare`, `min`, `max`) in Vesper, targeting CoreCLR. Sibling of
[`brainstorm-structural-equality.md`](brainstorm-structural-equality.md); that
doc owns equality + hashing. **This doc only adds ordering** and reuses the
equality doc's machinery wholesale — it is referenced, not re-explained.

Status: **design settled; implementation deferred.** Decided: **opt-in**
comparison (`[<StructuralComparison>]`), shipped as the **`Vesper.Comparison`
precursor package** and built together with `Set` / `Map` — their real
consumer, which **may force revision** of this contract (§10). Remaining leans
(NaN split, generic fail-loud, `Block` comparability) in §9.

## 1. How comparison differs from equality

Comparison is *almost* the equality design with `Comparer<T>.Default` swapped in
for `EqualityComparer<T>.Default` — but three differences drive every open end:

1. **Comparison is a strict subset of equality.** Many Data types have value
   equality but no meaningful order (anything containing a function, a `Block`
   that opts out, an unordered record). So `Comparison.Structural` **implies**
   `Equality.Structural`, never the reverse (§7). A type can be equatable and
   un-orderable; never the other way.
2. **There is no "reference comparison."** The equality principle's "State /
   Buffers → reference equality" has no ordering analog — you can't order by
   identity. So the mapping collapses to two states: a type either has a
   structural order or `<`/`compare` on it is rejected. (§2 table.)
3. **`Comparer<T>.Default` is *not* total at runtime.** Unlike
   `EqualityComparer<T>.Default` (which always returns something),
   `Comparer<T>.Default.Compare` **throws** if `T` implements neither
   `IComparable<T>` nor `IComparable`. This breaks the equality design's
   "generics are uniformly safe" simplification (§7).

The organizing principle still holds, narrowed: **ordered Data gets structural
comparison; everything else is not comparable.**

| Category | Equality (sibling doc) | Comparison |
| :--- | :--- | :--- |
| primitives, `string`, `char` | structural | **structural** (`clt`/`cgt` or `Comparer`) |
| DU (all fields comparable) | structural | **structural** (tag, then fields), opt-in (§9) |
| record (all-immutable, all fields comparable) | structural | **structural** (lexicographic), opt-in (§9) |
| `Block<'T>` | structural | **structural iff opted in** (lexicographic, §6) |
| raw `T[]`, buffers, State, classes | reference | **none** (no identity ordering) |
| anything with a function / non-comparable field | None | **none** |
| function / closure | none | **none** |

## 2. Current state

`op_LessThan` / `op_GreaterThan` / `op_LessThanOrEqual` / `op_GreaterThanOrEqual`
already exist: desugared from tokens (`Passes/Desugar.fs:32`), typed
`t*t->bool` for numeric primitives via `tryPrimitiveTraitCandidate` and the
`comparisonBinaryOps` set (`Passes/Unification.fs:651`), plus mono `int`
forms in `MockBuiltins` (`ExternalSymbols.fs:186`). Gaps: they're primitive-only
(no path orders a user record/DU); **`compare` / `min` / `max` don't exist** as
intrinsics; nothing emits `IComparable<T>`.

`EqArray.fs` is deliberately `[<NoComparison>]` (its doc comment: "nothing
orders SemType/TAST nodes") — so the compiler-internal array twin is *not* a
template for `Block`'s ordering; that's a fresh, opt-in method (§6).

## 3. Compile-time dispatch (`<`, `compare`, …)

### 3.1 Front-end typing

Same posture as equality §3.1: make the operators polymorphic and defer the
"is this orderable?" check.

- `op_LessThan` / `op_GreaterThan` / `op_LessThanOrEqual` /
  `op_GreaterThanOrEqual` : `'T -> 'T -> bool` (today: primitive-only; widen).
- `compare` : `'T -> 'T -> int` (new intrinsic, like `hash`).
- `min` / `max` : `'T -> 'T -> 'T` (built on `compare`).
- **Validation** rejects ordering on types whose `ComparisonSupport` is `None`
  (§7), the comparison analog of the equality `TyFun` reject. Wider net than
  equality's: a function field, *or* any non-comparable field, *or* (if §9
  opt-in) an un-annotated record makes the type non-orderable.

### 3.2 The single dispatch rule

For `e1 < e2` / `compare e1 e2` at operand type `T`:

| `T` | `compare e1 e2` | `e1 < e2` |
| :--- | :--- | :--- |
| primitive | `clt`/`cgt`/`ceq`-derived 3-way (existing path) | `clt` (IEEE, see §8.1) |
| **everything else** | `Comparer<T>.Default.Compare(e1, e2)` | `compare e1 e2 < 0` |

`<=` / `>` / `>=` derive from the same `compare` result sign (`<= 0`, `> 0`,
`>= 0`). `Comparer<T>.Default` dispatches to the type's `IComparable<T>` with no
boxing. Call sites and the generated per-field walk (§5) use the same rule —
one mechanism, as in equality.

## 4. No new runtime library

Nothing to add. The equality doc deleted `Runtime.*` and made structural arrays
a *type* (`Block<'T>`); comparison rides on the same decision. `Comparer<T>.Default`,
`IComparable<T>`, `IComparable` are all BCL. The only hand-authored ordering
code is `Block<'T>`'s `CompareTo` (§6), and only if `Block` is made comparable.

## 5. Type generation (records and DUs)

When a Data type is orderable (§7, subject to §9's opt-in/out), the compiler
makes it implement `System.IComparable<T>` (`CompareTo(T other)`) and — for BCL
interop (sorting through non-generic APIs) — `System.IComparable`
(`CompareTo(object)`), additive.

### 5.1 Records — lexicographic
`CompareTo(T other)` walks fields in **declaration order**: `let c =
Comparer<FieldTy>.Default.Compare(this.F, other.F)` then `if c <> 0 then c`;
return `0` after the last field. (Mirrors F# structural comparison.) Gated on R6
record backend emission, like equality §5.1.

### 5.2 DUs — tag, then fields
`CompareTo` compares `this._tag` vs `other._tag` first (so **case declaration
order is the inter-case order** — the footgun in §8.2); on tag equality,
compares the active case's payload fields lexicographically by the §3.2 rule. A
nullary case compares equal once tags match. This is the first implementable
slice (DUs are the only aggregate emittable today).

### 5.3 Null
`IComparable<T>.CompareTo(null)` returns positive (this sorts after null; null
is least), matching BCL convention.

## 6. `Block<'T>` comparison

If `Block<'T>` is comparable, it implements `IComparable<Block<'T>>` with a
**lexicographic** loop over elements via `Comparer<'T>.Default` (shorter-prefix
sorts first), hand-authored alongside its `Equals` (equality §4). Unlike its
equality (always present), `Block`'s *ordering* is the natural place to apply
§9's opt-in: comparison of collections is rarer and, for a `'T` that isn't
comparable, throws at runtime (§7). Raw `T[]` stays non-comparable (a Buffer).

## 7. Comparison satisfiability in the TAST

Mirror equality §5.3: materialize the verdict, don't re-derive it in the
backend. Add alongside `EqualitySupport`:

```fsharp
[<RequireQualifiedAccess>]
type ComparisonSupport =
    | Structural   // emit IComparable<T>/CompareTo; ordering allowed
    | None         // < / compare / min / max rejected
    // | Custom  — reserved for [<CustomComparison>]; later, additive.

// TTypeDecl gains:  Comparison: ComparisonSupport
// Invariant enforced by the front-end:
//   Comparison = Structural  ⇒  Equality = Structural
```

`comparisonSupport : SemType -> ComparisonSupport` derivation:
- primitive / `string` / `char` → `Structural`
- `TyFun` → `None`
- `TyTuple items` → `None` if any item is `None`, else `Structural`
- `TyRecord` / union → that decl's stamped `Comparison`
- raw `T[]` → `None` (Buffer; equality is `Reference`, comparison is `None`)
- `Block<'T>` → `Structural` only if §9/§6 makes it comparable
- generic instantiation → **recurse into the type args** (`Structural` iff every
  arg is non-`None`, exactly the `TyTuple` rule); a `TyVar` arg is assumed
  `Structural` (§7c)

Computed in a `ctx.ComparisonSupport` registry post-Unification; Validation
diagnoses, Freeze stamps onto `TTypeDecl`.

**The simplification does NOT carry over — DECIDED: §7c, arg-recursion.** Equality
treats generics as uniformly `Structural` because `EqualityComparer<'T>.Default`
is total; `Comparer<'T>.Default` **throws** for a non-comparable arg, so a generic
`Box<'T>`'s comparison genuinely depends on its args. Three ways to handle it; we
take **(c)**:
- **(a) Flat enum + fail-loud.** Treat generics as uniformly `Structural`;
  `compare` on a non-comparable instantiation **throws at runtime**. Flat, but
  pushes a catchable error to runtime. *Rejected.*
- **(b) Conditional comparison constraint (F#-faithful, heavier).** Carry an
  "orderable *iff* the type args are" state and **propagate it as a typar
  constraint** across generic function boundaries, so even
  `let f (x:'T) = compare x x` surfaces a constraint on `'T`. Full constraint
  machinery — **deferred**.
- **(c) Arg-recursion at concrete sites (DECIDED).** `comparisonSupport
  (TyUnion(decl, args))` for a `Structural`-stamped `decl` is `Structural` iff
  every *concrete* arg's support is non-`None` (the same recursion `TyTuple`
  already uses) — so a bad instantiation with **known** args
  (`Result<int->int, _>`) is a **compile error** at the `<`/`compare` site. A
  `TyVar` arg (a genuinely polymorphic position, no propagation) is assumed
  `Structural`, leaving (a)'s fail-loud throw as the **residual** only there.
  This is the bounded concrete-site check §5.3/§3.1 already sanction — strictly
  lighter than (b)'s propagation, strictly stricter than (a)'s flat shortcut. The
  asymmetry with equality (which keeps flat-generic) is justified: equality's
  comparer can't throw, so the leak is silent and tolerated; comparison's throws,
  so it's worth catching where the args are known.

## 8. Edge cases

### 8.1 Floating-point NaN — the total-order / IEEE split
`Comparer<double>.Default` imposes a **total order**: `NaN` sorts *less than
everything* and `NaN.Compare(NaN) = 0`. The primitive `clt` path follows
**IEEE**: `nan < 1.0`, `1.0 < nan`, and `nan < nan` are all `false`. So bare
`x < y` on floats is IEEE, but a float *field* compared structurally (via
`Comparer`) is total-order. This is exactly the equality doc's split (bare `=`
is `ceq`/IEEE, structural `=` is `EqualityComparer`/`NaN=NaN`) — the same wart,
consistently applied, inherited from BCL/F#. Flagged as an open end (§9) in case
we want to force consistency.

### 8.2 DU case-reorder footgun
Because §5.2 orders by tag, **reordering DU cases silently changes sort order**
and can reorder a `Set`/`Map` built on the type. Real cost; one of the
strongest arguments for §9 opt-in (you only get the footgun if you ask for
ordering).

### 8.3 Cyclic graphs
Like equality §7.2 — no visited-set tracking; `compare` on a cyclic graph →
`StackOverflowException`.

### 8.4 Consumers force the opt-in/opt-out hand
`min` / `max` / `List.sort` / and especially **`Set<'T>` / `Map<'K,'V>`**
(ordered trees in F#) *require* comparison on their element/key type. Because
comparison is opt-in (§9, decided), using a user type as a `Set` element
requires annotating it with `[<StructuralComparison>]` — the accepted ergonomic
cost. This work is built *with* `Set`/`Map` (§10) so the requirement arrives
alongside its consumer rather than as a standalone surprise.

## 9. Open ends

- **Opt-in vs. opt-out — DECIDED: opt-in.** Comparison is **off by default**;
  a Data type gets a structural order only when the author writes
  `[<StructuralComparison>]` (or `[<CustomComparison>]`). The asymmetry with
  equality (opt-out) is deliberate and principled: equality is near-universal on
  Data, ordering is not; ordering also carries the §8.2 reorder footgun, the
  §8.1 NaN wart, and a non-total runtime (§7), so requiring intent is the
  fail-closed choice that matches the project's taste (cf. equality's
  mutable-record rule). The accepted cost is §8.4 — using a user type as a
  `Set` element / `Map` key, or sorting it, requires the annotation — and
  divergence from F#'s opt-out. This is acceptable precisely because `Set`/`Map`
  are built *together with* this work (§10), so the annotation requirement lands
  with its only real consumer rather than as a standalone surprise.
- **NaN: keep the total-order/IEEE split (§8.1) or force consistency?** Keeping
  it mirrors the equality doc and F#; forcing consistency means either bare `<`
  routes through total-order `compare` (loses cheap IEEE `clt`) or structural
  comparison special-cases floats (complex). Lean: keep the split (consistency
  with the equality doc beats internal float consistency).
- **Generics — DECIDED: §7c, arg-recursion at concrete sites.** Neither the flat
  fail-loud (a) nor full typar-constraint propagation (b): the `comparisonSupport`
  derivation recurses through a generic instantiation's *concrete* type args (the
  `TyTuple` rule), so `Result<int->int,_>` is a compile error at the use site
  while a fully-polymorphic `'T` arg keeps the fail-loud throw as residual. See
  §7c.
- **Is `Block<'T>` comparable at all (§6)?** Only if there's a consumer (sorting
  blocks, blocks as `Map` keys). Defer until one exists; `EqArray` chose
  `NoComparison` for the same reason.
- **Build timing — DECIDED: with `Set`/`Map`.** Structural-comparison
  *generation* (§5) has no consumer until ordered collections arrive (`min`/`max`
  and bare `<` on primitives only need the S1 primitive path). So the design is
  settled now (this doc) but generation is implemented **together with `Set`/
  `Map`**, the consumer that forces it. Packaged as `Vesper.Comparison` (§10) —
  explicitly **provisional**: building `Set`/`Map` against it is expected to
  reshape the contract (custom comparers, `IComparer<T>` injection for map keys,
  etc.), so treat this doc's §5/§7 surface as a first cut, not frozen.

## 10. Packaging and implementation slices

**`Vesper.Comparison` — a precursor package.** Comparison ships as its own
package (like `Vesper.List` / `Vesper.Printf`; cf.
[`package-split-plan.md`](package-split-plan.md)), depended on by the future
ordered-collections package (`Set` / `Map`). It is **mostly contract**, not
runtime: the `compare` / `min` / `max` intrinsic signatures and the
`[<StructuralComparison>]` / `[<CustomComparison>]` / `[<NoComparison>]`
attribute hooks (cf. `prim-types-attr.fsi`). Comparison needs no runtime *type*
— it rides on BCL `IComparable<T>` / `Comparer<T>.Default`. **Provisional:**
building `Set` / `Map` against it will likely revise the contract (§9), so this
package lands first but is expected to churn with its consumer.

Slices (dependency order); mirrors equality §6; all BCL-only.

1. **S1 — primitive ordering + `compare`/`min`/`max` dispatch.** Widen the four
   ops to polymorphic, add `compare`/`min`/`max`, §3.2 primitive path,
   `ComparisonSupport` scaffold (`None` for `TyFun`). No generation yet. The
   only part with a consumer before `Set`/`Map`; can land standalone.
2. **S2 — mono-DU `CompareTo`** (§5.2), opt-in via `[<StructuralComparison>]`.
   Stamps DU `Comparison`. Build with `Set`/`Map`.
3. **S3 — record `CompareTo`** (§5.1), opt-in; gated on R6. Fills the recursive
   `comparisonSupport` registry + the `Comparison ⇒ Equality` invariant.
4. **S4 — generic DUs/records** (declaring-typar member sigs, R2 machinery).
5. **S5 — `Block<'T>` `CompareTo`** (§6), only if a consumer appears.

## 11. Cross-references
- [`brainstorm-structural-equality.md`](brainstorm-structural-equality.md) — the
  sibling design; all shared machinery (Data/State principle, the
  `*Support`-in-TAST pattern, `Block<'T>`, dispatch posture) lives there.
- [`package-split-plan.md`](package-split-plan.md) — the per-package split
  (`Vesper.List` / `Vesper.Printf` / …) this `Vesper.Comparison` precursor
  follows; `Set`/`Map` will be the consumer package.
