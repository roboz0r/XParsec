# Discriminated unions plan

The build plan for **monomorphic F# discriminated unions** in the
semantic-analysis pipeline. Same overall shape as
[`records-plan.md`](records-plan.md): a new named-type registry entry,
constructor-name-driven inference, and pattern-match support that
threads through every pass. The records work already paid for the
named-type infrastructure (`ctx.RecordTypes`, `TyRecord`,
`hasPendingFieldAccess`), so a lot of the moving pieces are already in
place — this plan extends them rather than inventing parallel
machinery.

The status quo is silent on DUs. `TypeDefn.Union` declarations are
parsed but never read by any pass. Constructor *uses* (`Circle 1.0`,
`Point`) are parsed as plain `Expr.App` / `Expr.LongIdentOrOp` — the
former routes through NameResolution's unresolved-name diagnostic, the
latter through the unresolved-qualified-name diagnostic. Constructor
*patterns* (`| Circle r ->`, `| Point ->`) route through the wildcard
arm in `Unification.inferPat` and emit no constraints, so any code
that matches on a union explodes downstream.

The canonical examples we want to handle after this lands:

```fsharp
// Definition with three cases of varying arity.
type Shape =
    | Circle of float
    | Rectangle of float * float
    | Point

// Nullary constructor — types as the declaring union.
let p = Point
//    p : Shape

// Single-arg constructor — argument types pin per field.
let c = Circle 1.0
//    c : Shape

// Multi-arg constructor — F# DU args are a tuple, application
// passes the tuple positionally.
let r = Rectangle(2.0, 3.0)
//    r : Shape

// Pattern match — receiver-driven, must reach a TyUnion.
let area s =
    match s with
    | Circle r -> 3.14 * r * r
    | Rectangle(w, h) -> w * h
    | Point -> 0.0
//    area : Shape -> float

// Constructor as a value — types as `arg -> TyUnion`.
let mkCircle = Circle
//    mkCircle : float -> Shape

// Disambiguation when two DUs share a case name.
type Result1 = | Ok of int | Err of string
type Result2 = | Ok of float | Err of string
let r1 : Result1 = Ok 1                          // annotation
let r2 = Result2.Ok 1.0                          // qualified prefix
```

## Goal

After the pipeline finishes:

- **`ctx.UnionTypes`** holds one `UnionTypeInfo` per `TypeDefn.Union`
  in the file, name-keyed. Each carries the case list (name, fields,
  declaration order) — mirrors the records registry.
- **`SemType` gains `TyUnion of name: string`** — a named-type wrapper
  in the same family as `TyRecord`. Two unions unify iff their names
  match; case lookup is a side-channel on `ctx.UnionTypes`, not a
  structural part of `TyUnion`.
- **Constructor references** infer their type from the case-name
  registry. `Circle` (the bare identifier) resolves to a value of type
  `float -> Shape`; `Point` (nullary) resolves to `Shape`. Ambiguity
  (two unions share a case name) requires a `Result2.Ok` qualifier or
  a binding-level annotation.
- **Constructor applications** type-check the argument against the
  case's declared fields. `Rectangle(2.0, 3.0)` unifies the tuple
  `(2.0, 3.0)` with the case's declared `float * float` field set.
- **Constructor patterns** (`| Circle r -> …`, `| Point -> …`)
  destructure by case name. The scrutinee unifies with `TyUnion T` for
  the case's declaring type; sub-patterns unify with declared field
  types.
- **TAST** grows `TExpr.UnionCons` and `TPat.Union`. The frozen tree
  carries the case name plus already-typed argument expressions.

What this plan does **not** deliver (carried in [§Out of scope](#out-of-scope)):
generic DUs (`Option<'a>`, `Result<'a,'b>`, `list<'a>`), match-arm
exhaustiveness, struct DUs, and cross-file DU resolution.

## What we have to build on

| Piece                                  | Where                                           | Status |
|----------------------------------------|-------------------------------------------------|--------|
| `TypeDefn.Union`                       | `XParsec.FSharp/Expr.fs:691`                    | Done — typeName + `UnionTypeCases` array. |
| `UnionTypeCase`                        | `Expr.fs:640`                                   | Done — attributes + `UnionTypeCaseData`. |
| `UnionTypeCaseData.Nullary`            | `Expr.fs:633`                                   | Done — `| Point`. |
| `UnionTypeCaseData.Nary`               | `Expr.fs:634`                                   | Done — `| Rectangle of float * float`. |
| `UnionTypeField.Unnamed`               | `Expr.fs:627`                                   | Done — positional field type. |
| `UnionTypeField.Named`                 | `Expr.fs:628`                                   | Done — `| Case of name: type` (named field). |
| `Pat.Named`                            | `Expr.fs:416`                                   | Done — `LongIdent * argumentPats` — used for ctor patterns with args. |
| `Pat.NamedFieldPats`                   | `Expr.fs:418`                                   | Done — `Case(name = pat; …)` for named-field unpacks. |
| `UnionArgPat.Named` / `.Positional`    | `Expr.fs:404`                                   | Done — single arg form for `NamedFieldPats`. |
| `Expr.LongIdentOrOp` (single-seg)      | already wired                                   | Done — used for nullary ctor references / qualifier-less ctor names. |
| `Expr.App`                             | already wired                                   | Done — used for ctor applications (`Circle 1.0`, `Rectangle(2.0, 3.0)`). |
| Named-type registry pattern            | `SideTables.fs:25` (`RecordTypeInfo`)            | Done — copy the shape: name-keyed registry on `PassContext`, side index by member name. |
| `TyRecord` precedent                   | `SemanticInfo.fs`                               | Done — `TyUnion` follows the same shape (name-only carrier, side-channel lookup). |
| `translateType` named-type recognition | `Unification.fs:502`                            | Done for records — extends with a `TyUnion` branch trivially. |
| `Validation.checkAssignment`           | `Passes/Validation.fs`                          | Unchanged — DUs aren't mutable LHSes in v1. |
| `bindingsOfPat` / `bindersOfPat`       | `NameResolution.fs:75`, `Regions.fs:153`        | Need new arms for `Pat.Named` (ctor) and `Pat.NamedFieldPats`. |
| `walkModuleElem` (Validation)          | `Passes/Validation.fs:118`                      | Already accepts `ModuleElem.Type _` after records — no change needed. |

The pieces missing are:

1. **`UnionTypeInfo` / `UnionCaseInfo` registry**. A new
   `ctx.UnionTypes : Dictionary<string, UnionTypeInfo>` plus a
   `CtorIndex : Dictionary<string, UnionCaseInfo list>` reverse index
   for ctor-name inference. Built by NameResolution from
   `TypeDefn.Union`s, populated with placeholder TyVars whose Links
   Unification fills in later (same dance as record fields).
2. **`SemType.TyUnion`** — new variant. Every `SemType`-consumer
   (zonk, occurs, unify, isAllocation, hasFreeTyVar, generalise,
   instantiate, hasPendingFieldAccess) gains an arm — same set of
   sites we updated for `TyRecord`.
3. **Ctor reference / application inference**: a new `inferExpr` path
   that recognises ctor identifiers (single-segment `Expr.LongIdentOrOp`
   whose name is in `ctx.CtorIndex`) and types them as
   `arg-tuple -> TyUnion` (or just `TyUnion` for nullary). `Expr.App`
   on a ctor unifies the argument with the ctor's declared shape.
4. **Pattern inference**: a `Pat.Named` / `Pat.NamedFieldPats` arm
   that drives off the ctor name. Sub-patterns unify with the
   ctor's declared field types.
5. **Nullary ctor pattern disambiguation**: a `Pat.NamedSimple`
   whose token text matches a known ctor *and* starts with an
   uppercase letter (F# convention) is reinterpreted as a nullary ctor
   pattern, not a binder. This is the only piece that needs new
   syntactic ambiguity handling.
6. **Freeze + TAST**: new `TExpr.UnionCons` and `TPat.Union` cases.
7. **Tests across the five passes**.

## Why DUs need a registry

Records already taught us: the pipeline resolves every name through
`ctx.Binding` (let-bound) or `IExternalSymbolProvider.TryLookup`
(external). Constructor names fit neither shape — they're declared in
the same compilation unit as their uses but they're not let-bound, and
the provider only knows external symbols. So DUs need the same kind of
locally-scoped registry that records got.

The two-table layout copies records:

- **`ctx.UnionTypes : Dictionary<string, UnionTypeInfo>`** — by type
  name, looked up when we see a `Type.NamedType` that refers to a DU,
  or when an annotation pins a value to a known union.
- **`ctx.CtorIndex : Dictionary<string, UnionCaseInfo list>`** — by
  ctor name, looked up when we see a ctor reference / application /
  pattern and need to find which union(s) declare it. The same
  ambiguity-resolution story records use applies: zero matches →
  unresolved, multiple matches → `Type.Case` qualifier or annotation
  required.

Cross-file resolution rides on the same hooks records will use when
modules / namespaces land — the registry becomes a per-file overlay
on the provider's named-type catalogue, and `IExternalSymbolProvider`
gains a `TryLookupType` channel that returns the same `UnionTypeInfo`
shape.

## The algorithm: ctor-name dispatch + receiver-driven pattern matching

### Constructor references

For `Expr.LongIdentOrOp(LongIdent [name])` whose `name` is *not* in
`ctx.Binding` but *is* in `ctx.CtorIndex`:

1. Look up `ctx.CtorIndex[name]`. If the list has exactly one entry,
   that's the ctor; otherwise emit "ambiguous constructor name" with
   the candidate types listed.
2. Build the ctor's function-shaped type from its declared fields:
   - **Nullary** (`| Point`) → `TyUnion T`.
   - **Single-field** (`| Circle of float`) → `TyFun(float, TyUnion T)`.
   - **Multi-field** (`| Rectangle of float * float`) →
     `TyFun(TyTuple [float; float], TyUnion T)` — F# DUs take a tuple
     as their single argument; multi-field application is just
     tuple-application.
3. Return that type. `Expr.App` of the ctor then unifies its argument
   against the tuple, same as any other function application.

For `Expr.LongIdentOrOp(LongIdent [typeName; caseName])` — qualified
ctor reference (`Result2.Ok`): look up `typeName` in
`ctx.UnionTypes`, find `caseName` in the case list, build the
function-shaped type as above. Disambiguates between unions that
share a case name.

### Constructor patterns

For `Pat.Named(LongIdent [name]; argumentPats = [argPat])`:

1. Resolve `name` via `ctx.CtorIndex` (same lookup as references).
2. Unify the surrounding scrutinee's type with `TyUnion T`.
3. Recursively infer `argPat`'s type, unify against the ctor's
   declared field shape:
   - Single field → unify directly with that field's type.
   - Multi-field → the parser delivers a single `argPat` whose shape
     is `Pat.EnclosedBlock(Pat.Tuple [...])` (parens + comma list) or
     `Pat.Tuple [...]`. Unify with `TyTuple [field1; field2; …]`.

For `Pat.NamedSimple t` where `ctx.NameOf t` is a known nullary ctor
and starts with an uppercase letter: reinterpret as a ctor pattern.
Unify scrutinee with `TyUnion T`, bind nothing. The uppercase check is
the F# spec convention for disambiguating ctors from binders in
patterns — same rule the compiler uses.

For `Pat.NamedFieldPats(LongIdent [name]; args = …)` — named-field
ctor pattern (`Foo(x = pat; y = pat)`): out of scope for v1. Emit a
TODO diagnostic; lands when we model named fields end-to-end.

### Qualified ctor patterns

`Pat.Named(LongIdent [typeName; caseName]; …)` resolves through
`ctx.UnionTypes[typeName]` directly — bypasses the `CtorIndex`
ambiguity check. Drives the same field-unification logic as the
unqualified path.

### Nullary ctors as values

`let f = Circle` types `f` as `float -> Shape` (the ctor function
itself). This falls out of the rule above without special-casing —
the inferer just returns the function-shaped type for the ctor
reference, and binding-time unification carries it.

A nullary ctor as a value is just `Point`, which types as `Shape`.
Same path as any other ident reference.

## Data-model changes

### `SemanticInfo.fs`

A new `SemType` variant:

```fsharp
type SemType =
    | TyVar of TypeVar
    | TyConst of name: string
    | TyFun of arg: SemType * result: SemType
    | TyTuple of items: SemType list
    | TyRecord of name: string
    /// Named union type. Case data isn't stored inline — look up
    /// `ctx.UnionTypes[name]` for the case list. Two TyUnions unify
    /// iff their names match.
    | TyUnion of name: string
```

Every consumer adds an arm:

- `Unification.zonk` — `TyUnion` zonks to itself.
- `Unification.unify` — `TyUnion n1, TyUnion n2` → name equality;
  otherwise mismatch.
- `Unification.occursAndAdjust` — no inner TyVars, nothing to do.
- `Unification.translateType` — `Type.NamedType li when
  ctx.UnionTypes.ContainsKey name` → `TyUnion name`. Sits inside the
  existing records-vs-other-named-types fall-through.
- `Unification.generalise` / `instantiate` — `TyUnion _` is ground.
- `Unification.hasPendingFieldAccess` — `TyUnion _ -> false`.
- `Regions.isAllocation` — `TyUnion _ -> true`.
- `Validation.hasFreeTyVar` — `TyUnion _ -> false`.
- `Freeze` — preserves `TyUnion` as the inferred type on union-shaped
  TAST nodes.

### Registry

A new `UnionTypeInfo` plus the `PassContext` storage:

```fsharp
[<Sealed>]
type UnionCaseInfo
    (name: string, fields: SemType[], fieldNames: string voption[],
     declKey: NodeKey) =
    member val Name = name
    /// Declaration-order field types. Length 0 for nullary cases.
    member val Fields = fields
    /// Per-field names where the source uses `Named` fields
    /// (`| Case of x: int * y: int`); ValueNone for positional fields.
    /// Stored even when unused so a follow-up `Pat.NamedFieldPats` pass
    /// can drive off it.
    member val FieldNames = fieldNames
    member val DeclKey = declKey

[<Sealed>]
type UnionTypeInfo
    (name: string, cases: UnionCaseInfo[], declKey: NodeKey) =
    member val Name = name
    member val Cases = cases
    member val DeclKey = declKey
```

Storage on `PassContext`:

```fsharp
/// Written by NameResolution from `TypeDefn.Union`s; case field types
/// are filled in by Unification after the registry is fully populated
/// (so a case can reference another DU declared elsewhere in the same
/// file). Name-keyed (single-segment v1).
member val UnionTypes = Dictionary<string, UnionTypeInfo>() with get
/// Reverse index: ctor name → list of union types that declare it.
/// Used by ctor-reference / ctor-pattern resolution.
member val CtorIndex = Dictionary<string, UnionCaseInfo list>() with get
```

The placeholder-then-fill-in dance for field types mirrors what
records do today: NameResolution stamps a fresh TyVar per field with
`Level = 0`, Unification's pre-pass walks the file's `TypeDefn.Union`s
and `Link`s each placeholder to the translated CST type.

### `TypeVar`

**No new axis needed.** Unlike records' field access, ctor inference
doesn't need a deferred constraint — every ctor reference resolves at
its use site (the name is either in `ctx.CtorIndex` or it isn't). The
`hasPendingFieldAccess` monomorphization gate already handles the
records-side case; DUs don't introduce a new gate.

## Pass-by-pass changes

### `NameResolution`

Add an arm to the existing `registerRecordTypes` pre-pass that also
handles `TypeDefn.Union`:

```fsharp
let private registerUnionTypeDefn (ctx: PassContext) (td: TypeDefn<SyntaxToken>) : unit =
    match td with
    | TypeDefn.Union(typeName = TypeName(ident = nameLi); cases = cases) when
        nameLi.Idents.Length = 1
        ->
        let name = ctx.NameOf nameLi.Idents.[0]
        if ctx.UnionTypes.ContainsKey name then
            diag "Duplicate union type"
        else
            let caseInfos =
                [|
                    for UnionTypeCase(data = data) in cases ->
                        let caseName, fieldCount, fieldNames = inspect data
                        // Per-field placeholder TyVars (see records).
                        let fieldTys =
                            Array.init fieldCount (fun _ ->
                                let tv = TypeVar()
                                tv.Level <- 0
                                TyVar tv)
                        UnionCaseInfo(caseName, fieldTys, fieldNames, …)
                |]
            let info = UnionTypeInfo(name, caseInfos, …)
            ctx.UnionTypes.[name] <- info
            for c in caseInfos do
                match ctx.CtorIndex.TryGetValue c.Name with
                | true, infos -> ctx.CtorIndex.[c.Name] <- c :: infos
                | false, _ -> ctx.CtorIndex.[c.Name] <- [c]
    | _ -> ()
```

`inspect` extracts the case's name + arity + per-field names from
`UnionTypeCaseData.Nullary` / `.Nary`. The two GADT-style variants
(`GadtNary`, `GadtNullary`) emit a "not yet supported" diagnostic and
register the case with zero fields so subsequent resolution doesn't
cascade — same posture as records' un-modelled CST cases.

Run order in `walkElems`: register records, then unions, then walk
expressions. Records can't reference unions in their field types and
vice versa in v1 (no module/namespace types), but the two registry
populations are mutually independent so the order between them
doesn't matter.

**`bindingsOfPat`** grows new arms:

```fsharp
| Pat.Named(longIdent = li; argumentPats = args) when
    li.Idents.Length = 1 && ctx.CtorIndex.ContainsKey(ctx.NameOf li.Idents.[0])
    ->
    // Ctor pattern — the head name binds nothing. Recurse into args.
    [ for sub in args do yield! bindingsOfPat ctx sub ]
| Pat.NamedSimple t when
    let n = ctx.NameOf t
    n.Length > 0 && System.Char.IsUpper n.[0] && ctx.CtorIndex.ContainsKey n
    ->
    // Nullary ctor pattern in disguise — binds nothing.
    []
```

The order matters: the `NamedSimple` arm must check `ctx.CtorIndex`
before falling through to the existing "this name binds itself" arm.
The uppercase guard keeps the legitimate lowercase-binder case
(`| x ->`) from accidentally claiming to be a ctor when an unrelated
DU happens to declare a single-letter ctor of the same shape.

### `Unification`

**Field-type fill-in.** Extend the existing `fillRecordFieldTypes`
walk to also fill `UnionTypeInfo` case fields. Same shape: for each
`TypeDefn.Union`, translate every field's CST type and `Link` the
placeholder TyVar.

**Ctor reference / application.** Extend `inferIdent`'s
single-segment LongIdent arm to recognise ctor names:

```fsharp
| Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
    let name = ctx.NameOf li.Idents.[0]
    match ctx.Binding.TryGetValue (CstKeys.ofExpr e), ctx.CtorIndex.TryGetValue name with
    | ValueSome rb, _ -> /* existing local-binding path */
    | _, (true, [info]) -> ctorType info
    | _, (true, infos) ->
        diag (sprintf "Ambiguous constructor '%s'; declared in %s"
                       name (String.concat ", " [for i in infos -> i.…]))
        ctorType infos.[0]   // best-effort to avoid cascades
    | _, (false, _) -> /* existing external / unresolved path */
```

For two-segment LongIdents (`Result2.Ok`), the existing field-chain
arm needs a sibling check: if the head is a known union *type* name
(not a local binding), the second segment is a ctor of that union.
This adds a new precedence rule to the existing arm.

`ctorType info` builds:

```fsharp
let ctorType (info: UnionCaseInfo) (unionName: string) : SemType =
    match info.Fields.Length with
    | 0 -> TyUnion unionName
    | 1 -> TyFun(info.Fields.[0], TyUnion unionName)
    | _ -> TyFun(TyTuple (List.ofArray info.Fields), TyUnion unionName)
```

**Constructor pattern.** Add to `inferPat`:

```fsharp
| Pat.Named(longIdent = li; argumentPats = args) when
    li.Idents.Length = 1 && ctx.CtorIndex.ContainsKey(ctx.NameOf li.Idents.[0])
    ->
    let caseName = ctx.NameOf li.Idents.[0]
    let info, unionName = resolveCtor ctx key caseName
    unifyArgPats ctx key info args
    let nodeTv = freshTv ctx key
    nodeTv.Link <- ValueSome(TyUnion unionName)
    TyUnion unionName
```

`unifyArgPats` mirrors the records `Pat.Record` arm: walk `args` in
order, recursively `inferPat` each, unify against the corresponding
`info.Fields` element. For multi-field ctors the parser delivers a
single arg pattern that's either `Pat.EnclosedBlock(Pat.Tuple [...])`
or `Pat.Tuple [...]` — strip the EnclosedBlock and treat the tuple
sub-patterns as the per-field bindings.

**Nullary ctor pattern.** Override `Pat.NamedSimple` ahead of the
existing arm:

```fsharp
| Pat.NamedSimple t when
    (let n = ctx.NameOf t in
     n.Length > 0 && System.Char.IsUpper n.[0]
     && (match ctx.CtorIndex.TryGetValue n with
         | true, [info] when info.Fields.Length = 0 -> true
         | _ -> false))
    ->
    let n = ctx.NameOf t
    let info = ctx.CtorIndex.[n] |> List.head
    let unionName = unionOfCase ctx info
    let nodeTv = freshTv ctx key
    nodeTv.Link <- ValueSome(TyUnion unionName)
    TyUnion unionName
```

Multi-candidate nullary ctors still need a qualifier; the
`Pat.NamedSimple` arm can't carry one (it's a single ident), so
they always emit an ambiguity diagnostic and route through the
unique-match path. The qualifier form lives on `Pat.Named` via
`LongIdent ["Type"; "Case"]`.

### `Regions`

DUs are allocations. Each `Expr.App` on a ctor mints one region with
one outgoing edge per ctor argument. Nullary ctors don't allocate
(in real F# they're singletons), but for v1 we treat them as
allocations to keep the rules uniform — the conservative direction.

The existing `appRegion` already handles this once `exprIsAllocation`
returns true for `TyUnion _` (covered by the `isAllocation` arm
addition above). No new arm needed for ctor application — it's just
`Expr.App` whose result types as `TyUnion T`.

`bindersOfPat` mirrors NameResolution's change: ctor patterns and
nullary-ctor `NamedSimple` reinterpretations bind nothing at their
head; only sub-patterns introduce binders.

Pattern matching on a union doesn't add new region rules — the
existing `matchRegion` walks all arms, mints a result region if the
match expression allocates, and edges from result back to each arm.

### `Validation`

Nothing new. DUs aren't mutable LHSes (v1), so no
assignment-to-immutable-ctor diagnostic. The `Pat.Named` /
`Pat.NamedFieldPats` arms in `bindingsOfPat` propagate to
Validation's binding-walks via the side tables; no per-arm Validation
logic is needed.

**Match exhaustiveness is explicitly out of scope** — it lands with
the broader pattern-match completeness work in Validation. The
existing test suite checks "no spurious diagnostics" on incomplete
matches, which is the current contract.

### `Freeze`

Two new TAST cases:

```fsharp
type TPat =
    // … existing cases …
    /// Constructor pattern. `fields` is empty for nullary cases.
    /// `ty` is always a `TyUnion`. The case's declaring union is
    /// recoverable via `ctx.CtorIndex[caseName]` at consumption time.
    | Union of caseName: string * fields: TPat list * ty: SemType

type TExpr =
    // … existing cases …
    /// Constructor application. `args` length matches the ctor's
    /// declared arity (0 for nullary, n for n-field). `ty` is a
    /// `TyUnion`.
    | UnionCons of caseName: string * args: TExpr list * ty: SemType
```

Freeze translation:

- **Ctor reference as a value** (`let f = Circle`): translates as
  `TExpr.External("Circle", ctorType)`. Downstream consumers see it
  as a regular external function; the codegen layer (when it lands)
  recognises ctor-shaped externals via the same registry.

  *Alternative considered:* eta-expand to
  `TExpr.Lambda("x", UnionCons("Circle", [Var "x"], …))`. Pushed to
  v1.5 — codegen can do this lowering when it cares. The External
  representation keeps Freeze simple.

- **Ctor application** (`Circle 1.0`): folds the `Expr.App` chain
  into `TExpr.UnionCons("Circle", [translatedArg], TyUnion "Shape")`.
  The translation peels application from the outside-in, recognising
  ctor heads via `ctx.CtorIndex`.

- **Nullary ctor** (`Point`): translates as
  `TExpr.UnionCons("Point", [], TyUnion "Shape")` — no `App` to peel.

- **Ctor pattern** (`Pat.Named` or reinterpreted `Pat.NamedSimple`):
  translates as `TPat.Union(caseName, translatedSubPats, TyUnion T)`.

`prettyDecl` / `TastShape` grow render arms — `UnionCons("Circle", [TConst 1.0], _)`
renders as `Circle 1` (constructor + arg); nullary renders as just
`Point`; patterns render as `| Circle r ->` etc.

## Pipeline integration

No new pass. DUs are handled by extending NameResolution (registry
pre-pass), Unification (field fill-in, ctor reference / application,
ctor patterns), Regions (ctor allocation falls out of
`isAllocation`), and Freeze (new TAST cases). Same skeleton records
used.

The order inside NameResolution's `run` extends the records walk:

```fsharp
let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
    // Existing records pre-pass.
    for m in elems do registerRecordTypes ctx m
    // New: register unions before walking expressions.
    for m in elems do registerUnionTypes ctx m
    // Walk expression bodies.
    for m in elems do walkModuleElem walker [] m
```

A DU can reference a record in its field types and vice versa, so
both registries need to be populated before the field fill-in pass
in Unification runs.

## TAST representation

The frozen TAST gains two new cases (above). The `ty` slot on each is
`TyUnion unionName`; downstream consumers look up
`ctx.UnionTypes[unionName]` for the case list — same posture as
records' field list.

Cross-file: `UnionTypes` is currently a per-file `PassContext` field.
When namespaces / cross-file resolution land, the registry layers
over a provider-backed equivalent — at TAST consumption time, the
consumer needs both the local registry and the provider's named-type
catalogue (same situation records left us with).

## Test strategy

Mirrors the records test split:

**`NameResolutionTests.fs`:**

1. **Union type registered.** `type S = | Circle of float | Point` —
   `ctx.UnionTypes["S"].Cases.Length = 2`.
2. **Duplicate type name diagnoses.** Two `type S = …` in one file →
   diagnostic.
3. **CtorIndex built.** Each ctor name reverse-maps to its declaring
   union.
4. **Nullary ctor in a pattern binds nothing.** `| Point -> ()` — no
   binding-site entry for `Point`.
5. **N-ary ctor pattern binds its sub-patterns.** `| Circle r -> r`
   — `r` resolves through the ctor pattern's sub.

**`UnificationTests.fs`:**

6. **Nullary ctor reference types as the union.**
   `type S = | Point\nlet p = Point` — `p : TyUnion "S"`.
7. **Single-arg ctor application.**
   `type S = | Circle of float\nlet c = Circle 1.0` — `c : TyUnion "S"`.
8. **Multi-arg ctor application takes a tuple.**
   `type S = | Rect of float * float\nlet r = Rect(2.0, 3.0)` —
   `r : TyUnion "S"`; arg-type mismatch diagnoses.
9. **Ctor as value types as a function.**
   `let f = Circle` — `f : float -> S`.
10. **Pattern unifies scrutinee with TyUnion.**
    `match s with | Circle r -> r` — `s : TyUnion "S"`.
11. **Ambiguous ctor name requires qualifier.** Two unions share
    `Ok`; bare `Ok 1` diagnoses ambiguity; `Result1.Ok 1` resolves.
12. **Wrong arity diagnoses.** `Circle(1.0, 2.0)` against
    `| Circle of float` — arity mismatch.

**`RegionsTests.fs`:**

13. **Module-level ctor application is LocalStack.**
    `let c = Circle 1.0` at module top → `LocalStack`.
14. **Ctor returned from a function is CallerStack.**
    `let mk () = Circle 1.0` → `CallerStack`.

**`ValidationTests.fs`:**

15. **No diagnostics on a well-formed DU pipeline.** Catches any
    spurious "unresolved" or "value restriction" firings.

**`CoverageTests.fs`:** golden TAST shapes for ctor application,
nullary ctor, ctor pattern, and match-on-union end-to-end.

Each test follows the existing `analyse` → `declType` → `Expect.equal`
/ `Expect.isEmpty Diagnostics` pattern.

## Open questions

- **Constructor-name resolution ordering with locals.** F# allows
  `let Point = 5` to shadow the ctor (lowercase-named cases would
  shadow normally; uppercase needs special-case behavior). v1
  resolves ctors *only when no local binding exists* for the name —
  the existing `inferIdent` check (`ctx.Binding.TryGetValue` first,
  then `ctx.CtorIndex`) gets this right. Verify with a test.

- **Constructor as a value: External vs eta-expanded Lambda in TAST.**
  Current plan: emit `TExpr.External`. The trade-off is that target
  plugins need to recognise external names that match ctor entries
  in the registry; the alternative (eta-expand at Freeze) keeps the
  TAST self-contained at the cost of larger trees. Defer the final
  call until codegen exists; for v1 either path works.

- **Named-field ctor patterns** (`| Circle(radius = r)`). `Pat.NamedFieldPats`
  carries the shape; v1 emits a "not yet supported" diagnostic. The
  registry already stores `FieldNames` so a follow-up is a per-arm
  unification against the named field's type rather than the
  positional index.

- **Single-field paren-wrapped ctor application.** `Circle(1.0)` vs
  `Circle 1.0` — the parser may emit `Expr.HighPrecedenceApp`
  (already wired) vs `Expr.App`. Both should route through the same
  ctor-recognition logic. The existing `inferHighPrecApp` arm
  already calls `infer` on the function expression, which produces
  the ctor's `TyFun` type — should fall out naturally. Verify with a
  test.

- **GADT-style cases** (`UnionTypeCaseData.GadtNary` / `.GadtNullary`).
  Out of scope; emit a "not yet supported" diagnostic on registration
  so the rest of the file still type-checks.

- **Cross-file ctor resolution.** Same story as records: lands with
  namespaces / modules. Per-file registry stays as the foundation;
  cross-file becomes a provider lookup overlay.

- **Pattern exhaustiveness on unions.** `match s with | Circle _ -> …`
  is incomplete (`Rectangle` / `Point` not covered). Part of the
  broader pattern-match exhaustiveness work in Validation — defer
  to that plan.

- **`let mutable c = Circle 1.0`**. The value-restriction check
  (`hasFreeTyVar` on the binding's TyVar) trivially passes since
  `TyUnion` is ground. Worth a regression test.

- **Constructor name overlap with provider symbols.** If `Ok` is both
  a local DU ctor *and* a `FSharp.Core` external symbol, the local
  ctor wins (consistent with F# shadowing rules). The lookup order
  in `inferIdent` (`ctx.Binding` → `ctx.CtorIndex` → provider)
  enforces this.

## Out of scope for this plan

- **Generic DUs** (`type Option<'a> = | Some of 'a | None`). Lands
  with typar scoping — same generics work that records' open question
  flagged.
- **`list`** and **`Option`** as concrete types. They're generic DUs
  in FSharp.Core; both land with generics + provider-side named-type
  catalogue.
- **Match-arm exhaustiveness.** Part of the broader pattern-match
  exhaustiveness work in Validation.
- **Cross-file DU resolution.** Lands with namespaces / modules.
- **External (FSharp.Core / BCL-defined) DUs.** `IExternalSymbolProvider`
  has no notion of a named-type catalogue yet — same gap records left.
- **Struct DUs** (`[<Struct>] type R = …`). Same ctor-resolution
  story; different region treatment.
- **Named-field ctor patterns** (`Pat.NamedFieldPats`).
- **GADT-style cases** (`GadtNary` / `GadtNullary`).
- **Active patterns** (`| MyActive p ->`). Resolution looks
  superficially like ctor patterns but routes through a different
  declaration form (`let (|Foo|_|) =`) and needs its own plan.
- **Constructor function value (lowering)** — TAST emits
  `TExpr.External` for ctor-as-value; codegen does eta-expansion if
  it needs to.
- **`[<RequireQualifiedAccess>]` attribute** on DU types. Lands with
  attributes in general.
