# Type abbreviations plan

The build plan for **type abbreviations** — `type Name = int`,
`type Pair<'a> = 'a * 'a`, `type Endo<'a> = 'a -> 'a`. Slots cleanly
into the pipeline that [`generics-plan.md`](generics-plan.md) just
landed: an abbreviation is a **transparent** name for a `SemType` —
two names that abbreviate the same thing unify exactly as if the user
had written the right-hand side. The typar scope, registry layout, and
substitution helpers are already in place.

The status quo is silent on abbreviations. `TypeDefn.Abbrev` is parsed
(`Expr.fs:683`) but never inspected by NameResolution; an abbreviation
name therefore reaches `translateType`'s `Type.NamedType` wildcard arm
and lands as an opaque `TyConst <name>` that only unifies against an
identical `TyConst`. `type IntPair = int * int` followed by
`let p : IntPair = (1, 2)` currently mismatches.

The canonical examples we want to handle after this lands:

```fsharp
// Trivial monomorphic abbreviation.
type Name = string
let n : Name = "alice"        // n : string

// Generic abbreviation.
type Pair<'a> = 'a * 'a
let p : Pair<int> = (1, 2)    // p : int * int
let q : Pair<bool> = (true, false)

// Abbreviation referencing another abbreviation.
type IntPair = Pair<int>
let r : IntPair = (3, 4)      // r : int * int

// Abbreviation expanding to a function type.
type Endo<'a> = 'a -> 'a
let id : Endo<int> = fun x -> x

// Abbreviation expanding to a generic record.
type Box<'a> = { Value: 'a }
type IntBox = Box<int>
let b : IntBox = { Value = 1 } // b : Box<int>

// Order-independent within a module.
type A = B * int
type B = bool                  // A is bool * int after fill-in

// Cycle — diagnosed, not infinite-looped.
type C = D
type D = C                     // diagnostic: "Type abbreviation 'C' is cyclic"
```

## Goal

After the pipeline finishes:

- **Registration.** Every `TypeDefn.Abbrev` writes an
  `AbbreviationInfo` entry to `ctx.AbbreviationTypes` keyed by name,
  with declared `TypeParams` (same `(string * TypeVar) list` shape as
  records / unions).
- **Eager expansion at use sites.** `translateType` resolves a named-
  or generic-type reference to an abbreviation by substituting fresh
  TyVars (or the user-provided args) into the stored body and
  returning the resulting `SemType` directly. No new `SemType` case —
  abbreviations vanish into their expansions.
- **Order-independent within a module.** A declaration may reference
  any other type in the same `let`-style group; bodies are filled in a
  post-registration pass that forces dependencies on demand.
- **Cycle detection.** Direct (`type A = A`) and indirect
  (`type A = B`/`type B = A`) cycles emit a diagnostic; the expansion
  is short-circuited to a free TyVar so subsequent unification fails
  gracefully instead of cascading.
- **Typar scope reuse.** An abbreviation's RHS is translated under a
  scope seeded from its `TypeParams`, identical to how
  `fillRecordFieldTypes` handles record fields. Implicit free typars
  (`type Bad = 'a`) diagnose just like they do on records / unions.

## What we have to build on

| Piece                                | Where                                  | Status |
|--------------------------------------|----------------------------------------|--------|
| `TypeDefn.Abbrev` CST case           | `Expr.fs:683`                          | Done — `typeName * equals * typ`. |
| `TypeName.typarDefns` / `prefixTypars` | `Expr.fs:487-489`                    | Done — used by generics. |
| `TyparScope` + strict mode           | `SideTables.fs`, `Unification.fs`      | Done — opens per-defn, diagnoses free typars. |
| `typarNamesOfTypeName`               | `NameResolution.fs`                    | Done — extracts ordered names. |
| `mkTypeParams`                       | `NameResolution.fs`                    | Done — mints prototype TyVars at level 0. |
| `substituteWith` / `mkNamedTypeSubst` | `Unification.fs` (public)             | Done — substitutes typar TyVars in a SemType. |
| `translateType` named-type lookup    | `Unification.fs`                       | Done for records / unions — extend with abbrev arm. |
| `RecordTypes` / `UnionTypes` registries | `SideTables.fs`                     | Done — `AbbreviationTypes` follows the same shape. |

The pieces missing are:

1. **`AbbreviationInfo`** — name, `TypeParams: (string * TypeVar) list`,
   `Body: SemType voption`, the CST node for fill-in, a `DeclKey`, and
   a `Filled` flag for cycle / readiness tracking.
2. **`PassContext.AbbreviationTypes`** — name-keyed registry.
3. **NameResolution registration** — `registerAbbreviationDefn` walks
   `TypeDefn.Abbrev`, mints `TypeParams`, stamps an unfilled
   `AbbreviationInfo`. Duplicate-name diagnostic shared with records /
   unions.
4. **Unification fill-in pass** — `fillAbbreviationBody ctx info`
   translates the RHS under a fresh typar scope seeded from
   `TypeParams`. Forces dependent abbreviations on demand; tracks an
   "in-progress" set to diagnose cycles. Runs before any expression
   walk, alongside `fillRecordFieldTypes` / `fillUnionFieldTypes`.
5. **`translateType` abbrev arms** — `Type.NamedType` and
   `Type.GenericType` resolve a single-segment name against
   `ctx.AbbreviationTypes` BEFORE falling through to the record /
   union lookup. Hit: arity-check, then substitute args into the
   stored body and return.
6. **Tests** mirroring the generics test split.

## Why eager expansion (and not `TyAbbrev`)

Two reasonable designs for abbreviations:

- **Eager**: `translateType` walks abbreviations into their stored
  bodies and returns the result. No new `SemType` case; every consumer
  (zonk, unify, generalise, Validation, Freeze, TAST) is untouched.
  Diagnostics show the *expanded* form (`int * int` instead of
  `IntPair`).
- **Nominal-with-expansion**: a new `TyAbbrev(name, args)` carries the
  abbreviation name. `unify` strips on demand. Lets diagnostics
  preserve the user's chosen name, at the cost of arms in every
  walker and a strip step on every unify branch.

For v1 we pick **eager**. The diagnostic-readability cost is small (F#
itself flips between forms in error messages) and the simplicity
matches everything else we've shipped on this branch. If preserving
the name becomes load-bearing we can wrap an opaque "display name"
side-channel later without changing unification semantics.

## The algorithm: register, fill on demand, expand at lookup

### Registration (NameResolution)

`registerAbbreviationDefn` runs alongside `registerRecordTypeDefn` /
`registerUnionTypeDefn` for every `ModuleElem.Type` in source order:

1. Pull the single-segment name from `TypeName.ident` (multi-segment
   names are out of scope for v1, same rule as records / unions).
2. Diagnose duplicates against `RecordTypes` / `UnionTypes` /
   `AbbreviationTypes` (one namespace).
3. Mint `TypeParams` from `typarNamesOfTypeName` via `mkTypeParams`
   (level 0, same as records / unions).
4. Build the `AbbreviationInfo` with `Body = ValueNone` and
   `Status = NotFilled`. Stash the RHS CST node alongside so Unification
   can translate it later.

### Fill-in (Unification)

A new pre-pass `fillAbbreviationBodies` runs once at the start of
`walkElems`, *before* `fillRecordFieldTypes` / `fillUnionFieldTypes`
so a record / union field can reference an abbreviation by name and
get the expanded type via `translateType`:

```fsharp
let private fillAbbreviationBodies (ctx: PassContext) (elems: ModuleElems<...>) : unit =
    // Iterate every Type group in source order. For each abbreviation
    // declared in this file, force its body. `forceFill` recurses into
    // dependencies and short-circuits cycles.
    for m in elems do
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                match td with
                | TypeDefn.Abbrev(typeName = tn) ->
                    let name = ctx.NameOf (tn |> nameTokenOf)
                    match ctx.AbbreviationTypes.TryGetValue name with
                    | true, info -> forceFill ctx info
                    | false, _ -> ()
                | _ -> ()
        | _ -> ()
```

`forceFill` is the cycle-aware translator:

```fsharp
let rec private forceFill (ctx: PassContext) (info: AbbreviationInfo) : unit =
    match info.Status with
    | Filled -> ()
    | InProgress ->
        // Reached this entry through its own body — direct or indirect
        // cycle. Emit a diagnostic; leave Body as ValueNone so the
        // expansion arm below substitutes a free TyVar.
        ctx.Diagnostics.Add { ... "Type abbreviation '%s' is cyclic" ... }
        info.Status <- Filled  // freeze so we don't re-diagnose
    | NotFilled ->
        info.Status <- InProgress

        // Translate the RHS under a typar scope seeded from
        // info.TypeParams (strict — implicit free typars diagnose).
        let savedScope, savedStrict = ctx.TyparScope, ctx.TyparScopeStrict
        ctx.TyparScope <- scopeOfTypeParams info.TypeParams
        ctx.TyparScopeStrict <- true
        try
            // translateType may recursively hit another abbreviation
            // via the expand-at-lookup arm; that arm calls forceFill
            // on demand, so dependencies fill themselves in DFS order.
            let body = translateType ctx info.RhsCst
            info.Body <- ValueSome body
        finally
            ctx.TyparScope <- savedScope
            ctx.TyparScopeStrict <- savedStrict
            info.Status <- Filled
```

### Expansion at lookup (`translateType`)

The `Type.NamedType` and `Type.GenericType` arms gain an abbreviation
branch BEFORE the existing record / union lookup. Same arity rule
records / unions follow:

```fsharp
// Inside the | _ -> branch of Type.NamedType li when li.Idents.Length = 1
match ctx.AbbreviationTypes.TryGetValue name with
| true, info ->
    forceFill ctx info          // safe re-entry: caches via Status
    let argCount = List.length info.TypeParams

    let args =
        // Bare reference to a generic abbreviation: back-fill with
        // fresh TyVars (matches the bare-record / bare-union arms).
        [ for _ in info.TypeParams -> TyVar(freshTyVar ctx) ]

    expandAbbreviation info args
| false, _ ->
    // existing record / union / TyConst fallthrough
    ...
```

```fsharp
// Inside the | Type.GenericType arm (after the measure-shaped case)
match ctx.AbbreviationTypes.TryGetValue name with
| true, info ->
    forceFill ctx info
    let expected = List.length info.TypeParams
    if expected <> argCount then diagnoseArity expected
    expandAbbreviation info translatedArgs
| false, _ -> ...
```

`expandAbbreviation` is just substitution:

```fsharp
let private expandAbbreviation (info: AbbreviationInfo) (args: SemType list) : SemType =
    match info.Body with
    | ValueSome body ->
        let subst = mkNamedTypeSubst info.TypeParams args
        substituteWith subst body
    | ValueNone ->
        // Either we're mid-cycle (Status was InProgress before the
        // diagnostic) or fill-in never ran. Hand back a fresh TyVar so
        // unification stays best-effort.
        TyVar(freshTyVar ctx)
```

The forwarding `forceFill` inside `translateType` is what makes
order-independence work: when filling `A = B`, hitting `B` in
translateType triggers `forceFill B`, which translates `B`'s body
(possibly recursing further). Already-filled entries short-circuit on
the `Filled` status check, so reaching `B` from a later abbreviation
is a no-op.

## Data-model changes

### `SideTables.fs`

```fsharp
[<RequireQualifiedAccess>]
type AbbreviationStatus =
    | NotFilled
    | InProgress
    | Filled

[<Sealed>]
type AbbreviationInfo(
    name: string,
    typeParams: (string * TypeVar) list,
    rhsCst: Type<SyntaxToken>,
    declKey: NodeKey
) =
    member val Name = name
    /// Same shape as `RecordTypeInfo.TypeParams`. Empty for non-generic
    /// abbreviations.
    member val TypeParams = typeParams
    /// CST of the abbreviation's right-hand side. Translated lazily by
    /// `forceFill` so order within the module doesn't matter.
    member val RhsCst = rhsCst
    member val DeclKey = declKey
    /// Filled by Unification's `forceFill` pre-pass. `ValueNone` after
    /// a cycle is detected — use sites substitute a fresh TyVar.
    member val Body: SemType voption = ValueNone with get, set
    /// Tracks fill-state for cycle detection and re-entry from
    /// `translateType`'s abbreviation arm.
    member val Status: AbbreviationStatus = AbbreviationStatus.NotFilled with get, set
```

On `PassContext`:

```fsharp
member val AbbreviationTypes = Dictionary<string, AbbreviationInfo>() with get
```

`SemType` is unchanged.

### `Tast.fs` / `Freeze.fs`

Unchanged. Abbreviations expand before any TAST is built, so consumers
see the underlying type (a `Box<int>` literal annotated as `IntBox`
still emits `RecordCons` with `ty = TyRecord("Box", [TyConst "int"])`).

## Pass-by-pass changes

### `NameResolution`

- Add `registerAbbreviationDefn` mirroring `registerRecordTypeDefn` /
  `registerUnionTypeDefn`. Single-segment names only; multi-segment
  diagnose (same fall-through as records / unions, which simply skip).
- The duplicate-type-defn check expands to include
  `AbbreviationTypes`.
- New iteration loop in `walkElems`:
  ```fsharp
  for m in elems do registerRecordTypes ctx m
  for m in elems do registerUnionTypes ctx m
  for m in elems do registerAbbreviationTypes ctx m   // <- new
  ```

### `Unification`

- Add `forceFill` and `expandAbbreviation` near `translateType`.
- Add the abbreviation arms in `Type.NamedType` and `Type.GenericType`
  cases of `translateType`. They run BEFORE record / union lookup so
  an abbreviation with the same name as a record (illegal anyway —
  caught by the duplicate-type-defn check) doesn't slip through.
- New top-level pre-pass `fillAbbreviationBodies`, invoked from
  `walkElems` before `fillRecordFieldTypes`:
  ```fsharp
  fillAbbreviationBodies ctx elems
  for m in elems do fillRecordFieldTypes ctx m
  for m in elems do fillUnionFieldTypes ctx m
  for m in elems do walkModuleElem ctx m
  ```
  Putting abbrev fill before record / union fill ensures
  `type R = { F: IntPair }` resolves `IntPair` to its expansion when
  the record's field type is translated.

### `Regions` / `Validation` / `Freeze`

No changes. Abbreviations vanish at translateType, so every later
pass sees the expanded `SemType` and behaves exactly as if the user
had written it longhand.

## Pipeline integration

No new pass. Abbreviations land entirely inside NameResolution
(registration) and Unification (fill-in + lookup expansion). The pass
order from [`passes.md`](passes.md) is unchanged.

## Test strategy

Mirrors the records / DUs / generics splits.

**`NameResolutionTests.fs`:**

1. **Monomorphic abbreviation registers with no TypeParams.**
   `type Name = string` → `AbbreviationTypes["Name"]` exists,
   `TypeParams.IsEmpty`.
2. **Generic abbreviation keeps declaration order.**
   `type Pair<'a, 'b> = 'a * 'b` → `TypeParams = [("'a", _); ("'b", _)]`.
3. **Duplicate name diagnoses.** `type Foo = int\ntype Foo = bool` →
   "Duplicate type definition" (shared message with records / unions).
4. **Abbreviation vs record same name diagnoses.**
   `type R = { X: int }\ntype R = int` → duplicate.

**`UnificationTests.fs`:**

5. **Monomorphic abbreviation transparently unifies.**
   `type Name = string\nlet n : Name = \"x\"` — `n : string`,
   no diagnostics.
6. **Generic abbreviation expands.**
   `type Pair<'a> = 'a * 'a\nlet p : Pair<int> = (1, 2)` —
   `p : int * int`.
7. **Chained abbreviation expands transitively.**
   `type A = B\ntype B = int\nlet x : A = 1` — `x : int`.
8. **Order-independent within a module.**
   `type IntPair = Pair<int>\ntype Pair<'a> = 'a * 'a\nlet p : IntPair = (1, 2)`
   — typechecks (Pair declared after its first use).
9. **Cycle diagnoses without infinite-looping.**
   `type A = B\ntype B = A` — "Type abbreviation 'A' is cyclic"
   (or 'B', depending on iteration order — assert message contains
   "cyclic").
10. **Arity mismatch on generic abbreviation diagnoses.**
    `type Pair<'a> = 'a * 'a\nlet p : Pair<int, bool> = (1, 2)` —
    "expects 1 type argument(s)".
11. **Abbreviation to function type.**
    `type Endo<'a> = 'a -> 'a\nlet inc : Endo<int> = fun x -> x + 1`
    — `inc : int -> int`.
12. **Abbreviation referencing a record.**
    `type Box<'a> = { Value: 'a }\ntype IntBox = Box<int>\nlet b : IntBox = { Value = 1 }`
    — `b : Box<int>` (the inferred type carries the expanded form).
13. **Abbreviation inside a record field type.**
    `type IntPair = int * int\ntype R = { Pair: IntPair }\nlet r = { Pair = (1, 2) }`
    — `r : R`, field `Pair : int * int`.
14. **Implicit free typar in abbreviation diagnoses.**
    `type Bad = 'a` — "Free type parameter 'a is not declared".

**`CoverageTests.fs`:**

15. **TAST shape for an abbreviation-typed binding.**
    `type Name = string\nlet n : Name = \"x\"` — `declType` is
    `TyConst "string"` (abbreviation erased).
16. **TAST shape for a generic-abbreviation literal.**
    `type Pair<'a> = 'a * 'a\nlet p : Pair<int> = (1, 2)` —
    `declType` is `TyTuple [TyConst "int"; TyConst "int"]`.

**`RegionsTests.fs`:** (sanity)

17. **Abbreviation to a record at module top is LocalStack.**
    `type IntBox = Box<int>\n…\nlet b : IntBox = { Value = 1 }` —
    same shape as a direct `Box<int>` binding.

Each test follows the existing `analyse` → `declType` /
`Expect.equal` / `Expect.isEmpty Diagnostics` pattern.

## Open questions

- **Preserving the user's abbreviation name in diagnostics.** Eager
  expansion drops `IntPair`'s name once it becomes `int * int`. Real
  F# preserves it via a side-channel that prints abbreviations until
  unification forces expansion. Defer; revisit if Validation /
  Freeze diagnostics get materially worse.
- **Recursive `and`-groups of abbreviations.** `type A = B and type B = A *
  int` (illegal — true recursion) and `type A = int and type B = A *
  bool` (legal — sequential abbreviations using `and` for ergonomic
  grouping). v1 treats each `TypeDefn.Abbrev` independently; `and`-
  groups need their own forward-reference rules. Likely lands with
  mutually recursive records / unions.
- **Multi-segment qualifiers.** `type X = Module.Foo` falls through
  the multi-segment skip arm. Land with module / namespace resolution.
- **Measure-type abbreviations.** `[<Measure>] type velocity = m/s`
  abbreviates a `MeasureTerm`, not a `SemType`. Different registry,
  different unification path; covered in the measures roadmap.
- **Cross-file abbreviations.** Same registry-overlay scheme records /
  unions / now-generics established. Lands with module / namespace
  resolution; the `AbbreviationInfo` shape transports unchanged.

## Out of scope for this plan

- **`and`-grouped mutually recursive types** (records, unions, or
  abbreviations declared in one `and`-chain). v1 abbreviations are
  single-decl; in-module order independence is enough for the common
  case.
- **Measure abbreviations** (`[<Measure>] type kg = …`). They live
  on the measure side of the pipeline, not the SemType side.
- **Type-parameter constraints on abbreviations**
  (`type Cmp<'a when 'a : comparison> = 'a -> 'a -> int`). Lands with
  constraints generally.
- **Static / inline / private modifiers on abbreviations.** Parsed but
  ignored.
- **Display-name preservation in TAST and diagnostics.** Deferred; eager
  expansion is the v1 behaviour.
- **Cross-file abbreviation lookup.** Lands with the namespace /
  cross-file resolution work.
- **Phantom-type idioms** (`type Unchecked<'a> = string`) — they work
  by accident under eager expansion (the typar simply has nowhere to
  flow); we don't claim to support them as a feature.
