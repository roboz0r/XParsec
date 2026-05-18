# Type-parameter constraints plan

The build plan for **type-parameter constraints** — `when 'a : equality`,
`when 'a : comparison`, `when 'a : struct`, `when 'a : not struct`, and
the nullness pair. Constraints attach to declared typars in three CST
positions: a `TyparDefns`' trailing `when` clause
(`type Foo<'a when 'a : equality> = ...`), a binding-level constraint
(`let f<'a when 'a : comparison> (x: 'a) (y: 'a) = ...`), and an
inline `WhenConstrainedType` (`('a when 'a : equality) list`). The
parser captures all three uniformly as `TyparConstraints` records
(`Expr.fs:113`); semantic analysis is silent on every one.

The status quo: NameResolution and Unification walk past
`TyparConstraints` without inspecting them. `TypeVar.IfaceBounds` and
`TypeVar.SrtpBounds` (`SemanticInfo.fs:189-191`) exist but only carry
placeholder DUs — no producer writes them, no consumer reads them.
`translateType`'s `Type.WhenConstrainedType` case falls through the
wildcard arm and hands back a free TyVar, dropping the constraint.

The canonical examples we want to handle after this lands:

```fsharp
// Equality constraint — int satisfies, function types don't.
let eq<'a when 'a : equality> (x: 'a) (y: 'a) = x = y
let _ = eq 1 2                       // ok
let _ = eq (fun x -> x) (fun y -> y) // diagnostic: 'a -> 'a doesn't support equality

// Comparison constraint — int satisfies, records-without-comparison-attribute don't.
let cmp<'a when 'a : comparison> (x: 'a) (y: 'a) = compare x y
let _ = cmp 1 2                      // ok
type R = { X: int }
let _ = cmp { X = 1 } { X = 2 }      // diagnostic: R does not support comparison

// Struct constraint — propagates through generic types.
let useStruct<'a when 'a : struct> (x: 'a) = x
let _ = useStruct 1                  // ok (int is a value type)
let _ = useStruct "x"                // diagnostic: string is a reference type

// Constraint on a generic type definition's typar.
type Set<'a when 'a : comparison> = { Items: 'a list }
let s : Set<int> = { Items = [1; 2; 3] }            // ok
let s : Set<int -> int> = { Items = [] }            // diagnostic

// Constraint propagates through abbreviation expansion.
type SortedPair<'a when 'a : comparison> = 'a * 'a
let p : SortedPair<int> = (1, 2)                    // ok
let p : SortedPair<int -> int> = (id, id)           // diagnostic

// Inline `when`-constrained type (rare but legal).
let f (xs: ('a when 'a : equality) list) = List.distinct xs

// Multiple constraints chained with `and`.
let g<'a when 'a : equality and 'a : comparison> (x: 'a) (y: 'a) = ...
```

## Goal

After the pipeline finishes:

- **Constraint collection.** Every `TyparConstraints` block in the CST
  feeds entries into a per-`TypeVar` `Constraints` list. Constraints
  on a binding's `TyparDefns`, a type defn's `TyparDefns`, and on a
  `Type.WhenConstrainedType` all share the same storage and resolution
  callback.
- **Eager-where-possible satisfaction.** When `unify` links a
  constrained `TypeVar` to a concrete `SemType`, it runs each
  constraint against the link target. A built-in primitive whose
  trait table answers "yes" passes silently; "no" emits a precise
  diagnostic naming the failing constraint and the type that failed
  it. Free targets defer the check until a future unification pins
  them.
- **Constraint propagation through generics.** When the link target is
  a `TyRecord`, `TyUnion`, or `TyTuple`, the constraint flows to
  each argument by the established compositional rules (a tuple
  supports equality iff every element does; a record supports
  comparison iff it carries `[<StructuralComparison>]` and every
  field's type does).
- **Constraint merging on TyVar union.** Two TyVars merging via union-
  find combine their constraint lists; duplicates collapse by
  structural equality so `'a : equality` constrained from two sources
  doesn't fire twice.
- **No effect on generalisation.** Constraints don't alter `shouldGeneralise`
  or the value-restriction logic. A generalised scheme captures its
  TyVars' constraints in `TypeScheme.Constraints`; `instantiate`
  copies them onto the fresh TyVars so each use site re-evaluates
  satisfaction independently.

## What we have to build on

| Piece                                | Where                                  | Status |
|--------------------------------------|----------------------------------------|--------|
| `TyparConstraints` CST case          | `Expr.fs:113`                          | Done — `whenToken * constraints * ands`. |
| `Constraint<'T>` DU (12 cases)       | `Expr.fs:115-152`                      | Done — full F# constraint vocabulary. |
| `Type.WhenConstrainedType`           | `Expr.fs:82`                           | Done — inline `when`-constrained type. |
| `TyparDefns.constraints` field       | `Expr.fs:110`                          | Done — already optional in defn position. |
| `Binding.typarDefns`                 | `Expr.fs:187`                          | Done — binding-level constraint position. |
| `TypeVar.IfaceBounds` / `SrtpBounds` | `SemanticInfo.fs:189-191`              | Plumbed-but-placeholder — repurpose `IfaceBounds`. |
| `migrateBounds` in `unify`           | `Unification.fs:76`                    | Done — already merges bound lists on union-find. |
| `drainPendingFieldAccess` pattern    | `Unification.fs:323`                   | Done — template for "fire on Link set". |
| `typarNamesOfTypeName`               | `NameResolution.fs:287`                | Done — extracts ordered names. |
| `mkTypeParams`                       | `NameResolution.fs:321`                | Done — mints prototype TyVars at level 0. |
| `Pat.As`-style satisfaction reasoning | n/a                                   | Missing — the trait table is new. |

The pieces missing are:

1. **`SemanticConstraint`** — a small DU covering the v1 subset
   (`Equality`, `Comparison`, `Struct`, `ReferenceType`, `Nullness`,
   `NotNull`), each carrying the use-site `NodeKey` for diagnostics
   and a reference to the `TypeVar` it constrains so on-unified
   callbacks can identify the source.
2. **`TypeVar.Constraints`** — the actual storage, replacing the
   placeholder `IfaceBounds`. `SrtpBounds` stays a placeholder for
   the SRTP-resolution phase.
3. **`translateConstraints`** — translates a `TyparConstraints` block
   into a list of `SemanticConstraint` and attaches them to the
   appropriate TyVars via `ctx.TyparScope`.
4. **Constraint-collection hooks** — three insertion points:
   `Type.WhenConstrainedType` in `translateType`,
   `Binding.typarDefns.constraints` in `inferBindingGroup`'s typar-scope
   open, and `TypeName.typarDefns.constraints` in
   `registerRecordTypeDefn` / `registerUnionTypeDefn` /
   `registerAbbreviationDefn`.
5. **Satisfaction checker** — a `checkConstraint ctx c target` that
   answers `Satisfied`, `Violated`, or `Defer` (target still free or
   constrained-but-pending). Built-in primitive table for `int`,
   `bool`, `string`, etc.; structural rules for tuples / records /
   unions; cycle-safe.
6. **`drainConstraints` in `unify`** — when a `Link` is set, walk the
   root's `Constraints` and call `checkConstraint`. `Defer` results
   stay on the new root (migrated by `migrateBounds`).
7. **`TypeScheme.Constraints`** — generalisation captures constraints
   alongside quantified TyVars; `instantiate` re-stamps them onto
   the fresh substitutions.
8. **Tests** mirroring the records / DUs / generics / abbreviations splits.

## Why a callback table (and not symbolic theorem-proving)

Two reasonable designs for satisfaction:

- **Trait table.** A hard-coded F# function maps `(SemanticConstraint,
  SemType)` to `Satisfied | Violated | Defer`. Primitives are looked
  up in a flat table; compound shapes recurse compositionally.
  Diagnostics print verbatim what the table answered. Closed-world —
  every supported constraint has explicit code.
- **Logic-programming.** Constraints become Prolog-style predicates;
  satisfaction is unification + backtracking against a rule database.
  Flexible for user-defined `[<Equality>]` attributes and FCS-style
  inheritance lattices, but heavyweight and needs a separate
  diagnostic pipeline.

For v1 we pick the **trait table**. The F# constraint vocabulary is
small and closed (12 syntactic forms, of which v1 handles 6) and the
rules for primitives / tuples / records are mechanical enough to
encode directly. If user-defined constraints become load-bearing
(SRTPs in earnest, IWSAMs) we can swap the resolver out without
changing the CST-to-`SemanticConstraint` path.

This matches `tryMeasuredArith`'s design in `Unification.fs` — explicit
F# dispatch on the small, finite constraint vocabulary, not a generic
solver.

## The algorithm: collect, attach, drain on link

### Collection (translation-time)

`translateConstraint` runs anywhere a `TyparConstraints` block is in
scope:

1. **At `Type.WhenConstrainedType (typ, constraints)`:** translate `typ`,
   then for each `Constraint` in `constraints.constraints`, resolve the
   typar name through `ctx.TyparScope`, build a `SemanticConstraint`,
   and append it to that TyVar's `Constraints`.
2. **At `TyparDefns` in a binding's `typarDefns`:** the typar scope is
   already open by `inferBindingGroup` (per `generics-plan.md` §"Typar
   scope"). Walk `TyparDefns.constraints` and attach.
3. **At `TyparDefns` in a `TypeName.typarDefns`:** during the
   `register…` pass for records / unions / abbreviations. Constraints
   on prototype TyVars travel through every use-site instantiation —
   `instantiateRecordType` substitutes prototypes for fresh TyVars,
   and the existing substitution helpers carry the constraint list
   across.

Reject unsupported constraints with a diagnostic and skip — `Coercion`,
`MemberTrait`, `DefaultConstructor`, `Enum`, `Unmanaged`, `Delegate`,
and `Default` are deferred to their own phases. The skip is non-fatal:
inference continues without the constraint, matching how the v1
typevar / measure paths handle unsupported sub-grammars.

### Storage

```fsharp
[<RequireQualifiedAccess>]
type SemanticConstraintKind =
    | Equality
    | Comparison
    | Struct
    | ReferenceType   // `not struct`
    | Nullness        // `: null`
    | NotNull         // `: not null`

[<Struct>]
type SemanticConstraint =
    {
        Kind: SemanticConstraintKind
        /// Source location for the diagnostic when this constraint is
        /// violated. The constraint's text source (`when 'a : ...`),
        /// not the use site that pinned the typar — those diverge.
        DeclKey: NodeKey
    }
```

A constraint attaches to a `TypeVar` by appending to a new list:

```fsharp
type TypeVar() =
    // … existing fields …
    /// Constraints attached at declaration / use sites. Drained by
    /// `unify` when `Link` is set; merged on union-find. Empty for
    /// the overwhelming majority of TyVars.
    member val Constraints: SemanticConstraint list = [] with get, set
```

`IfaceBounds` and `SrtpBounds` stay where they are — different
resolution paths and they need richer payloads when their phases land.

### Resolution (drain on link)

The on-unified callback lives next to `drainPendingFieldAccess`. When
`unify`'s "TyVar vs. concrete" branch sets `root.Link <- ValueSome
other`, it follows with:

```fsharp
drainConstraints ctx root other
```

`drainConstraints` walks `root.Constraints`, classifies each against
`other`, and acts on the three outcomes:

| Outcome | Action |
|---------|--------|
| `Satisfied` | drop from the list (constraint discharged) |
| `Violated` | emit a diagnostic; drop from the list |
| `Defer`     | keep on the list; will re-fire on the next `Link` change |

`Defer` covers two cases: the link target is itself a free TyVar
(rare — TyVar/TyVar unifications go through `migrateBounds`, not
`drainConstraints`), or the target is a compound shape with at least
one still-free arg (a `TyRecord("R", [TyVar 'x])` waiting on `'x` to
be pinned). For the compound case, `drainConstraints` recursively
attaches the parent's constraint to the still-free arg TyVars so
the next link on any of them re-evaluates the rule.

### Trait table for primitives

```fsharp
let private primitiveSupports
    (kind: SemanticConstraintKind) (name: string) : bool option =
    match kind, name with
    | Equality, ("int" | "int64" | "byte" | "bool" | "string"
                 | "float" | "float32" | "char" | "unit") -> Some true
    | Comparison, ("int" | "int64" | "byte" | "bool" | "string"
                   | "float" | "float32" | "char" | "unit") -> Some true
    | Struct, ("int" | "int64" | "byte" | "bool" | "float"
               | "float32" | "char" | "unit") -> Some true
    | Struct, ("string" | "obj") -> Some false
    | ReferenceType, "string" -> Some true
    | ReferenceType, ("int" | "int64" | "byte" | "bool"
                      | "float" | "float32" | "char" | "unit") -> Some false
    | Nullness, "string" -> Some true
    | Nullness, ("int" | "int64" | "byte" | "bool"
                 | "float" | "float32" | "char" | "unit") -> Some false
    | NotNull, ("int" | "int64" | ...) -> Some true
    | _ -> None  // "I don't know" — fall through to provider / structural
```

The table is closed-world for the v1 primitive set. Anything outside
the table (`decimal`, `bigint`, FCS-known types) returns `None`,
which the dispatcher treats as `Defer` rather than `Violated`.
External-provider entries can extend the answer set when cross-file
resolution lands — the provider gains a `SupportsConstraint` query
that wraps a host-language trait check.

### Structural rules for compound shapes

```fsharp
let rec checkConstraint
    (ctx: PassContext) (c: SemanticConstraint) (t: SemType)
    : ConstraintOutcome =
    match c.Kind, resolveStep t with
    | _, TyVar _ -> Defer
    | k, TyConst name ->
        match primitiveSupports k name with
        | Some true -> Satisfied
        | Some false -> Violated
        | None -> Defer

    | (Equality | Comparison), TyFun(_, _) ->
        // Function types have neither structural equality nor comparison.
        Violated

    | (Equality | Comparison), TyTuple items ->
        // Element-wise — every element must support the same kind.
        items |> reduceOutcome (checkConstraint ctx c)

    | (Equality | Comparison), TyRecord(name, args) ->
        // Record-typed: structural equality iff every field's type does,
        // AND the record carries no `[<NoEquality>]` attribute. The
        // attribute machinery lands later — for v1 the rule is "every
        // field must support `kind`".
        match ctx.RecordTypes.TryGetValue name with
        | true, info ->
            let subst = mkNamedTypeSubst info.TypeParams args
            info.Fields
            |> Array.map (fun f -> substituteWith subst f.Type)
            |> Array.toList
            |> reduceOutcome (checkConstraint ctx c)
        | false, _ -> Defer

    | (Equality | Comparison), TyUnion(name, args) ->
        // Union: every case's every field must support `kind`.
        match ctx.UnionTypes.TryGetValue name with
        | true, info ->
            let subst = mkNamedTypeSubst info.TypeParams args
            [
                for case in info.Cases do
                    for field in case.Fields ->
                        substituteWith subst field
            ]
            |> reduceOutcome (checkConstraint ctx c)
        | false, _ -> Defer

    | Struct, (TyTuple _ | TyFun _ | TyRecord _ | TyUnion _) ->
        // In F#, tuples, functions, and reference records / unions
        // are all reference types. Struct records / unions exist
        // (`[<Struct>]` attribute) but the attribute walker lands
        // later — for v1 they're all reference.
        Violated

    | ReferenceType, (TyTuple _ | TyFun _ | TyRecord _ | TyUnion _) ->
        // Mirror of Struct.
        Satisfied

    | Nullness, _ -> Defer  // requires the nullness analysis from a different track

    | _ -> Defer
```

`reduceOutcome` folds the list:
- All `Satisfied` → `Satisfied`.
- Any `Violated` → `Violated`.
- Otherwise → `Defer`.

### Generalisation and instantiation

```fsharp
type TypeScheme =
    {
        Quantified: TypeVar list
        /// Constraints captured at generalisation time. Each entry
        /// pairs the constraint with the *quantified* TyVar it
        /// constrained at that point; `instantiate` swaps the
        /// TyVar through the substitution before re-stamping.
        Constraints: (TypeVar * SemanticConstraint) list
        Body: SemType
    }
```

`generalise` walks the now-zonked body, collects free TyVars whose
`Level > outerLevel`, and pulls each one's `Constraints` into the
scheme's constraint list. `instantiate` mints a fresh TyVar per
quantifier and re-attaches the constraint list, threading the
substitution so a constraint pointing at the old `'a` lands on the
fresh `'a'`.

Use-site arity / kind diagnostics that arise from the fresh
constraints (a use of `eq` with `int -> int`) surface at the
instantiation point, not at the binding's declaration.

## Data-model changes

### `SemanticInfo.fs`

```fsharp
// Replaces the placeholder `IfaceBoundPlaceholder` DU.
[<RequireQualifiedAccess>]
type SemanticConstraintKind =
    | Equality
    | Comparison
    | Struct
    | ReferenceType
    | Nullness
    | NotNull

[<Struct>]
type SemanticConstraint =
    {
        Kind: SemanticConstraintKind
        DeclKey: NodeKey
    }

and [<Sealed>] TypeVar() =
    // … existing fields …
    member val Constraints: SemanticConstraint list = [] with get, set
    // Keep SrtpBounds untouched — SRTP plumbs through a separate
    // path that lands with member-trait support.
    member val SrtpBounds: MemberSignature list = [] with get, set
```

`InterfaceBound` / `IfaceBounds` go away (no producer, no consumer,
freed for the new field). `MemberSignature` stays as a placeholder
for SRTPs.

### `TypeScheme`

```fsharp
type TypeScheme =
    {
        Quantified: TypeVar list
        Constraints: (TypeVar * SemanticConstraint) list   // new
        Body: SemType
    }
```

Empty list for every scheme whose quantified TyVars carry no
constraints — overwhelmingly the common case.

### `PassContext`

No new field. Constraints live on the TyVars they constrain; their
DeclKey carries the source-position info for diagnostics.

## Pass-by-pass changes

### `NameResolution`

No semantic change. The `register…` passes for records / unions /
abbreviations already mint `TypeParams`; we don't need to walk the
`TyparConstraints` block here because Unification owns translation
of types and the constraint translation rides on `translateType`.
The exception is that `registerRecordTypeDefn` /
`registerUnionTypeDefn` / `registerAbbreviationDefn` capture the
`TyparConstraints` CST node (currently dropped) onto the registry
entry so Unification can later walk it during fill-in.

```fsharp
// SideTables.fs adjustments — one new field on each registry type:
type RecordTypeInfo(..., typarConstraints: TyparConstraints<SyntaxToken> voption, ...) =
    member val TyparConstraints = typarConstraints
// Same on UnionTypeInfo, AbbreviationInfo.
```

### `Unification`

Three insertion points:

1. **`translateType` — `Type.WhenConstrainedType` arm.** Currently
   falls through the wildcard. Now: translate the inner type, then
   walk `constraints.constraints`, build a `SemanticConstraint` per
   syntactic case, and append to the constrained TyVar's `Constraints`.
   The TyVar is resolved through `ctx.TyparScope`; an unknown typar
   diagnoses the same way `Type.VarType` already does.

2. **Type-defn fill-in (`fillRecordFieldTypes` / `fillUnionFieldTypes`
   / `fillAbbreviationBodies`).** After the typar scope is seeded
   from the registry entry's `TypeParams`, walk
   `info.TyparConstraints` (if present) and attach each constraint
   to the corresponding prototype TyVar. The prototypes carry
   constraints; every use-site instantiation copies them across via
   `freshNamedInstance`'s substitution.

3. **Binding-level `typarDefns.constraints`.** In `inferBindingGroup`,
   after the binding's typar scope is open but before the RHS walk,
   translate the constraint block and attach. The TyVars come from
   the same scope `Type.VarType` consults.

`freshNamedInstance` (and `instantiate`) need updating:

```fsharp
let private freshNamedInstance
    (ctx: PassContext)
    (typeParams: (string * TypeVar) list)
    : SemType list * Dictionary<TypeVar, SemType> =
    let subst = Dictionary<TypeVar, SemType>(HashIdentity.Reference)
    let args =
        [
            for (_, tp) in typeParams ->
                let fresh = TypeVar()
                fresh.Level <- ctx.CurrentLevel
                // Copy constraints from prototype to fresh instance.
                fresh.Constraints <- (UnionFind.find tp).Constraints
                let asTy = TyVar fresh
                subst.[UnionFind.find tp] <- asTy
                asTy
        ]
    args, subst
```

`unify`'s TyVar-vs-concrete branch calls `drainConstraints` after
setting `Link`:

```fsharp
root.Link <- ValueSome other
drainPendingFieldAccess ctx root other
drainConstraints ctx root other          // new
```

`migrateBounds` extends one line:

```fsharp
target.Constraints <- target.Constraints @ source.Constraints |> dedupe
source.Constraints <- []
```

`dedupe` is a structural-equality fold; two constraints with the
same `Kind` collapse to the first occurrence (their `DeclKey` may
differ but the diagnostic source is whichever fires).

### `Validation`

One new check: after `Unification` finishes, walk every quantified
TyVar of every `TypeScheme` and confirm its `Constraints` list is
empty. A non-empty list at end-of-analysis means a use site never
pinned the TyVar enough to discharge the constraint — diagnose as
"Constraint not resolved" rather than letting the deferred entry
silently disappear. Compare with how mutable bindings' value-
restriction check runs as a Validation tail step today.

### `Regions` / `Freeze`

Unchanged. Constraints are a Unification-only concept; once they're
discharged (or diagnosed), the SemType graph carries no residue.

### `Tast.fs`

Unchanged. Constraints are inference-time only — the TAST consumer
sees fully resolved types.

## Pipeline integration

No new pass. Constraints land in three existing places — NameResolution
captures the CST node, Unification translates and resolves, Validation
catches unresolved residue. The pass order from
[`passes.md`](passes.md) is unchanged.

## Test strategy

Mirrors the abbreviations / generics splits.

**`NameResolutionTests.fs`:**

1. **Generic record with constraints captures the TyparConstraints CST.**
   `type Set<'a when 'a : comparison> = { Items: 'a list }` —
   `RecordTypes["Set"].TyparConstraints.IsSome`.
2. **Generic union with constraints captures the TyparConstraints CST.**
   `type Tree<'a when 'a : comparison> = | Leaf | Node of 'a * Tree<'a> * Tree<'a>`.

**`UnificationTests.fs`:**

3. **Equality constraint satisfied by int.**
   `let eq<'a when 'a : equality> (x: 'a) (y: 'a) = x = y\nlet _ = eq 1 2`
   — no diagnostics.
4. **Equality constraint violated by function type.**
   `let eq<'a when 'a : equality> ... \nlet _ = eq id id` — diagnostic
   contains "equality".
5. **Comparison constraint satisfied by int.**
6. **Comparison constraint violated by function type.**
7. **Comparison constraint satisfied by tuple of comparable elements.**
   `cmp (1, 2) (3, 4)` — no diagnostics.
8. **Comparison constraint violated by tuple containing a function.**
   `cmp (1, id) (2, id)` — diagnostic.
9. **Struct constraint satisfied by int.**
10. **Struct constraint violated by string.**
11. **Reference-type constraint satisfied by string.**
12. **Reference-type constraint violated by int.**
13. **Constraint on generic-record use site fires.**
    `type Set<'a when 'a : comparison> = { Items: 'a list }\nlet s : Set<int -> int> = { Items = [] }`
    — diagnostic.
14. **Constraint propagates through abbreviation expansion.**
    `type SortedPair<'a when 'a : comparison> = 'a * 'a\nlet p : SortedPair<int -> int> = (id, id)`
    — diagnostic.
15. **Inline `WhenConstrainedType`.**
    `let f (xs: ('a when 'a : equality) list) = ...` — `'a`'s TyVar
    gains an Equality constraint.
16. **Multiple constraints on one typar.**
    `let f<'a when 'a : equality and 'a : comparison> ...` — both
    attach.
17. **Constraint merging on TyVar union.**
    Two unified TyVars from different binding sites both contribute
    constraints; combined list dedupes.
18. **Deferred constraint surfaces only when the typar resolves.**
    A polymorphic value used in two positions — one int (satisfies),
    one function type (violates) — diagnoses only once for the
    second use.

**`CoverageTests.fs`:**

19. **TAST shape for a constraint-bearing binding.**
    `let eq<'a when 'a : equality> ... ` — the TypeScheme captures
    the constraint; the use site `eq 1 2` resolves with the
    int-substituted scheme and no residue.
20. **Constraint flows through generic record's field type.**
    `type Box<'a when 'a : equality> = { Value: 'a }\nlet b : Box<int> = { Value = 1 }` —
    `b : Box<int>` and no diagnostics.

**`ValidationTests.fs`:**

21. **Unresolved constraint at end of analysis diagnoses.**
    A scheme whose quantified TyVar carries an `Equality` constraint
    and is never pinned (e.g. `let f<'a when 'a : equality> () = ()`
    with no call site that fixes `'a`). Validation surfaces a
    "Constraint not resolved" diagnostic.

Each test follows the existing `analyse` → `declType` /
`Expect.equal` / `Expect.isEmpty Diagnostics` pattern.

## Open questions

- **`[<NoEquality>]` / `[<NoComparison>]` attributes.** F# lets a
  record / union opt out of structural equality / comparison via
  attribute. The attribute walker isn't built yet; v1 ignores them
  and treats every record / union as structurally derivable. Lands
  with the attribute pass.
- **`[<Struct>]` attribute on records / unions.** Same — without
  attributes, `Struct` constraint on a `TyRecord` / `TyUnion` is
  `Violated` for v1.
- **User-defined SRTP / IWSAM constraints.** `MemberTrait` and
  `Coercion` constraints carry richer payloads (signature ASTs,
  qualifier paths). They share the `Constraints` list but each kind
  has its own resolution callback. The shape of the list survives
  the extension; `drainConstraints` grows new arms.
- **Constraint-violation diagnostic location.** Today the diagnostic
  fires at the unification key — wherever `unify` was called from.
  For `eq id id` that's the application's NodeKey, which is correct
  but loses the "constrained at" location. The `SemanticConstraint`'s
  `DeclKey` already carries the constraint's source; consider
  emitting two-tone diagnostics (`primary: use site, related:
  declaration site`) once the diagnostic struct grows a `related`
  field.
- **Cross-file constraints.** When an external symbol carries
  constraints (e.g. `List.distinct<'a when 'a : equality>`), the
  provider has to surface them through `ExternalSymbol`. The shape
  is the same — a `Constraints` field on `ExternalSymbol` that
  `instantiate` reads — but the provider plumbing lands with
  cross-file resolution.
- **Constraint inference (vs declaration).** F# infers constraints in
  some positions — `let f x y = x = y` should infer `'a : equality`
  from the use of `=`. The trait-table approach handles satisfaction
  but not inference; inferring needs the symmetric direction (when
  `(=) : 'a -> 'a -> bool` is used, attach `Equality` to its first
  arg's TyVar). Defer to a follow-up; v1 reads declared constraints.

## Out of scope for this plan

- **`Constraint.Coercion`** (`when 'a :> IComparable`). Needs
  interface-implementation lookup; lands with the IWSAM phase per
  [`typevar.md`](typevar.md) §SRTP and IWSAM bounds.
- **`Constraint.MemberTrait`** (`^a : (member …)`). SRTP resolution
  is a separate machine — Hindley-Milner with explicit witness
  passing — and ships in its own phase. The `SrtpBounds` field
  stays plumbed for that work.
- **`Constraint.DefaultConstructor`, `Enum`, `Unmanaged`, `Delegate`**.
  Niche; ignored with a "constraint not yet supported" diagnostic.
- **`Constraint.Default`** (`default 'a Type`). This is a
  fall-through resolution rule for ambiguous SRTP / numeric type
  inference; lands with SRTPs.
- **Inferring constraints from use** (the `let f x y = x = y` story).
  v1 reads declared constraints only.
- **Attribute-driven satisfaction** (`[<NoEquality>]`,
  `[<StructuralComparison>]`, `[<Struct>]`). Lands with the attribute
  pass.
- **Variance-aware constraint propagation.** F# uses variance on
  delegates / interfaces only; not yet relevant.
- **Cross-file constraints.** Lands with the namespace / .NET-provider
  work.
