# Records plan

The build plan for **F# records** in the semantic-analysis pipeline.
Larger scope than [`mutable-plan.md`](mutable-plan.md) and
[`measures-plan.md`](measures-plan.md): records touch every pass, add a
new named-type registry the pipeline doesn't currently have, and push us
to think about field-name-driven type inference for the first time.

The status quo is silent on records. `TypeDefn.Record` declarations are
parsed but never read by any pass — they fall off the side of
`walkModuleElem` in NameResolution, Unification, and now Validation
(which `failwith`s on `ModuleElem.Type` after the [`mutable-plan.md`]
follow-up). `Expr.Record` / `Expr.RecordClone` / `Expr.DotLookup` /
`Pat.Record` route through the conservative-`HeapShared` fallback in
Regions and emit nothing in Unification, so any code that touches a
record explodes downstream.

The canonical examples we want to handle after this lands:

```fsharp
// Definition with one mutable field.
type Point = { X: int; mutable Y: int }

// Record literal — field set uniquely identifies the type.
let p = { X = 1; Y = 2 }
//    p : Point

// Field access — driven by the receiver's type.
let xCoord = p.X        // xCoord : int

// Field assignment — works because Y is mutable.
p.Y <- 5
// p.X <- 5             // would diagnose: X is not mutable

// Record clone — copies p, overrides Y.
let p2 = { p with Y = 10 }
//    p2 : Point

// Record pattern — destructures by field name.
let { X = x; Y = y } = p
//    x : int, y : int

// Disambiguation when two records share field names.
type Vec = { X: float; Y: float }
let v : Vec = { X = 1.0; Y = 2.0 }       // annotation
let v2 = { Vec.X = 1.0; Y = 2.0 }        // qualified prefix
```

## Goal

After the pipeline finishes:

- **`ctx.RecordTypes`** holds one `RecordTypeInfo` per `TypeDefn.Record`
  in the file, name-keyed. Each carries the field list (name, type,
  IsMutable) in declaration order.
- **`SemType` gains `TyRecord of name: string`** — a named-type wrapper.
  Two records unify iff their names match; field lookup is a side-channel
  on `ctx.RecordTypes`, not a structural part of `TyRecord`.
- **Record literals** infer their type from the field-name set: every
  `Expr.Record` with field set `{F1, …, Fn}` resolves to the unique
  record whose declared field set is exactly `{F1, …, Fn}`. Ambiguity
  (two records share a field set) requires a `Vec.X` qualifier or a
  binding-level annotation.
- **Field access** `r.X` unifies `r`'s type with `TyRecord "T"` for the
  type T that declares X, then emits `TExpr.FieldGet(receiver, fieldIdx,
  fieldTy)`. If `r`'s type stays free past end-of-let, emit a Validation
  diagnostic.
- **Field assignment** `r.X <- v` is allowed iff X's `IsMutable = true`;
  Validation diagnoses otherwise. The cell-region machinery from
  [`mutable-plan.md`](mutable-plan.md) handles the escape state of each
  mutable field.
- **Record patterns** `{ X = px; Y = py }` destructure a value of the
  matching record type. Same field-set / qualifier rules as record
  literals.
- **Record cloning** `{ r with X = v }` types as the same record as `r`,
  validates that the listed fields exist, and validates field types.
- **TAST** grows `TExpr.RecordCons`, `TExpr.RecordClone`,
  `TExpr.FieldGet`, `TExpr.FieldSet`, `TPat.Record`.

## What we have to build on

| Piece                                  | Where                                           | Status |
|----------------------------------------|-------------------------------------------------|--------|
| `TypeDefn.Record`                      | `XParsec.FSharp/Expr.fs:684`                    | Done — typeName + brace-enclosed `RecordField` array. |
| `RecordField`                          | `Expr.fs:645`                                   | Done — attributes / mutable / access / ident / colon / type. |
| `Expr.Record`                          | `Expr.fs:285`                                   | Done — brace-enclosed `FieldInitializer` array. |
| `Expr.RecordClone`                     | `Expr.fs:290`                                   | Done — `{ expr with init1; … }`. |
| `Expr.DotLookup`                       | `Expr.fs:260`                                   | Done — `r.X`, `r.X.Y` (chained). |
| `Pat.Record`                           | `Expr.fs:432`                                   | Done — brace-enclosed `FieldPat` array. |
| `FieldInitializer`                     | `Expr.fs:203`                                   | `LongIdent * '=' * Expr`. Long ident gives optional qualifier `Vec.X`. |
| `FieldPat`                             | `Expr.fs:400`                                   | `LongIdent * '=' * Pat`. Same qualifier story. |
| `SemType`                              | `SemanticInfo.fs:89`                            | 4 cases (`TyVar`, `TyConst`, `TyFun`, `TyTuple`). Adding `TyRecord name` is the minimum surface for v1. |
| `ResolvedBinding.IsMutable`            | `SemanticInfo.fs:231`                           | Already plumbed through record-field assignment via the cell-region path. |
| `Validation.checkAssignment`           | `Passes/Validation.fs`                          | Handles `Expr.Ident`/`Expr.LongIdentOrOp` LHSes. Needs `Expr.DotLookup` LHS arm — see §Validation. |
| Cell-region machinery for `mutable`    | `Passes/Regions.fs:531`                         | Threshold-of-1 rule + cell-vs-rhs separation. Extends to mutable fields naturally. |
| `ImplementationFile` walker            | `Passes/Validation.fs`, `Unification.fs`, etc.  | `walkModuleElem` ignores `ModuleElem.Type` today; needs a real arm. |
| Conservative `HeapShared` fallback     | `Passes/Regions.fs:300`                         | Today catches `Expr.Record` etc. Stays as the safe net but precise rules replace it for records. |

The pieces missing are:

1. **`RecordTypeInfo` registry**. A new `ctx.RecordTypes : Dictionary<string, RecordTypeInfo>` plus a `FieldIndex : Dictionary<string, RecordTypeInfo list>` reverse index for field-set inference. Built by NameResolution from `TypeDefn.Record`s.
2. **`SemType.TyRecord`** — new variant. Every `SemType`-consumer (zonk, occurs, unify, isAllocation, hasFreeTyVar) gains an arm. Field-content is a side-channel on `ctx.RecordTypes`; not stored inline on `TyRecord`.
3. **Unification rules**: literal inference (field-set → record name → unify), field access (receiver-type-driven), record clone, record patterns. Each adds an `inferExpr` / `inferPat` arm.
4. **Regions rules**: a record is an allocation with one outgoing edge per field initialiser; mutable fields mint a cell region each. Record clone shares regions with the cloned-from record where fields are reused.
5. **Validation**: assignment-to-immutable-field diagnostic; deferred-field-resolution diagnostic.
6. **Freeze + TAST**: new `TExpr` / `TPat` cases.
7. **Tests across all five passes**.

## Why records need a registry

The pipeline's current shape resolves *every* name through one of two
places: `ctx.Binding` (let-bound names, including locals and module
values) or `IExternalSymbolProvider.TryLookup` (compiled names from
FSharp.Core / .NET). Neither answers "what fields does the record type
`Point` have?" or "which record types contain a field `X`?".

Record type definitions live in the same compilation unit as their uses,
which means they aren't reached through `IExternalSymbolProvider` (that
interface is for *external* assemblies). A field name is not a binding
either — `r.X` doesn't resolve `X` through `ctx.Binding`, it resolves
through r's type. So records need their own resolution channel.

The cleanest shape is a name-keyed registry on `PassContext`, populated
by NameResolution from the file's `TypeDefn.Record` declarations.
Subsequent passes read it but don't modify it. Cross-file resolution
lands when modules / namespaces do — at which point the registry's
write-once contract makes it trivial to merge across files (the provider
becomes the cross-file equivalent, and the local registry is a
per-file overlay).

## The algorithm: field-set inference + receiver-driven dispatch

### Literals

For `{ F1 = e1; …; Fn = en }`:

1. If any `Fi` carries a qualifier (`Vec.X` — i.e. `LongIdent.Idents.Length > 1`), the qualifier names the record type. All other fields must belong to that type.
2. Otherwise, intersect `ctx.RecordTypes.FieldIndex[F1] ∩ … ∩ FieldIndex[Fn]` and check the result has exactly one entry whose declared field set equals `{F1, …, Fn}`.
3. If zero matches: "no record type has fields F1, …, Fn" diagnostic.
4. If more than one: "field-name set is ambiguous between R1, R2; add a qualifier or annotation" diagnostic.
5. If exactly one type T: unify the enclosing expression's TyVar with `TyRecord T.Name`, and unify each `inferExpr ei`'s type with the declared field type for `Fi`.

### Field access

For `r.X` (`Expr.DotLookup(expr = r; longIdentOrOp = LongIdent { Idents = [X] })`):

1. Type r as usual. Read `resolveStep (typeOf r)`.
2. If it's `TyRecord T`: look up `T`'s field X in the registry. Unify the enclosing expression's TyVar with the field's declared type. Emit `TExpr.FieldGet`.
3. If it's a `TyVar` that's still free: defer. Mark the access with a "needs receiver-record" constraint stored on the TyVar (similar to SRTPs but mechanically simpler — one record name, no overload set). On every subsequent unification that pins the TyVar, run the pending constraints. If still unresolved at end of analysis, emit a Validation diagnostic.
4. If it's something else (`TyConst`, `TyFun`, `TyTuple`, `TyRecord T` with no field X): emit "type `…` has no field `X`."

For v1 the deferred-constraint store is keyed off the TyVar; we just
attach a `PendingFieldAccess: (fieldName, useKey, resultTyVar) list`
to each TypeVar. When `unify` sets a `Link` from a TyVar to a
`TyRecord T`, it drains the pending list and resolves each access. This
is the same shape as the deferred-bound mechanism `migrateBounds` already
implements for SRTPs.

### Multi-segment `Expr.DotLookup`

`r.X.Y` parses as `DotLookup(DotLookup(r, ., X), ., Y)`. Each
`.X`-step folds into the rule above by typing the inner DotLookup first
and recursing. No special multi-segment path needed.

### Record clone

For `{ r with F1 = e1; …; Fn = en }`:

1. Type r. Resolve to `TyRecord T`. (Free TyVar → deferred constraint, same as field access — the field set must match T's set when T is pinned.)
2. Validate each `Fi` belongs to T. Diagnose unknown fields.
3. Unify each `inferExpr ei`'s type with T's declared field type for `Fi`.
4. Result type is `TyRecord T`.

For v1, record clone requires r's type to resolve before this expression
is typed — if r is a let-bound local with an annotated type, that's fine.
Deferred resolution lands later.

### Record patterns

For `{ F1 = p1; …; Fn = pn }`:

1. Same field-set lookup as the literal — find the unique T (or use the qualifier / scrutinee type if available).
2. Unify the scrutinee with `TyRecord T`.
3. Recursively check each sub-pattern `pi` against T's declared field type for `Fi`.

Unlike literals, record patterns may omit fields. A pattern with a strict
subset of T's field set still matches — the unlisted fields are simply
not destructured.

## Data-model changes

### `SemanticInfo.fs`

A new `SemType` variant:

```fsharp
type SemType =
    | TyVar of TypeVar
    | TyConst of name: string
    | TyFun of arg: SemType * result: SemType
    | TyTuple of items: SemType list
    /// Named record type. Field types are not stored inline — look up
    /// `ctx.RecordTypes[name]` for the field list. Two TyRecords unify
    /// iff their names match. The name comes from the type-name
    /// declaration (single segment for v1; qualified names land with
    /// namespaces).
    | TyRecord of name: string
```

Every consumer of `SemType` adds an arm:

- `Unification.zonk` — `TyRecord` zonks to itself (no inner type variables to chase).
- `Unification.unify` — `TyRecord n1, TyRecord n2` → name equality; otherwise mismatch.
- `Unification.occursAndAdjust` — `TyRecord _` → no inner TyVars, nothing to do.
- `Unification.translateType` — `Type.NamedType li when ctx.RecordTypes.ContainsKey name` → `TyRecord name`. This sits inside the existing wildcard arm at `Unification.fs:439` (the `_ -> TyConst name` fall-through); records win over the catch-all.
- `Regions.isAllocation` — `TyRecord _` → `true`.
- `Validation.hasFreeTyVar` — `TyRecord _` → `false` (no inner TyVars).
- `Freeze` — preserves `TyRecord` as the inferred type on record-shaped TAST nodes.

### Registry

A new `RecordTypeInfo` plus the `PassContext` storage:

```fsharp
[<Sealed>]
type RecordTypeInfo =
    {
        /// Single-segment name (v1). Multi-segment names land with
        /// namespaces.
        Name: string
        /// Declaration-order field list. Lookup-by-name uses the same
        /// list scanned linearly — typically 2-10 fields, not worth a
        /// Dictionary.
        Fields: RecordFieldInfo[]
        /// NodeKey of the TypeDefn.Record's typeName ident. For Freeze /
        /// diagnostics that need to point back at the declaration.
        DeclKey: NodeKey
    }

and [<Sealed>] RecordFieldInfo =
    {
        Name: string
        Type: SemType
        IsMutable: bool
        /// NodeKey of the RecordField ident — diagnostics point here.
        DeclKey: NodeKey
    }
```

Storage on `PassContext`:

```fsharp
[<Sealed>]
type PassContext(provider, input, lexed) =
    // … existing fields …
    /// Written by NameResolution from `TypeDefn.Record`s. Name-keyed
    /// (single-segment v1). Field types start as fresh TyVars and get
    /// linked by Unification when the type's `Type` annotations resolve
    /// — same handling as let-binding type annotations.
    member val RecordTypes = Dictionary<string, RecordTypeInfo>() with get
    /// Reverse index: field name → list of record types that declare it.
    /// Used by the literal / pattern field-set inference. Built once by
    /// NameResolution after RecordTypes is populated.
    member val FieldIndex = Dictionary<string, RecordTypeInfo list>() with get
```

`RecordTypes` is name-keyed (not NodeKey-keyed like the other side
tables) because both the consumer pattern (`{F1, F2, …}` looks up by
name) and the registry's identity (the *type's name*, not its
declaration position) are name-based. Cross-file resolution will overlay
the local table with a provider-backed one when modules land.

### `TypeVar`

One new axis on `TypeVar` for deferred field accesses:

```fsharp
and [<Sealed>] TypeVar() =
    // … existing Link / Level / Region / Units / Bounds fields …
    /// Pending field-access constraints accumulated while this TyVar was
    /// free. Drained by `unify` when the TyVar's `Link` becomes a
    /// `TyRecord _`. Empty for the overwhelming majority of TyVars —
    /// only allocated when the first `.Field` access hits a free TyVar.
    member val PendingFieldAccess: (string * NodeKey * TypeVar) list = [] with get, set
```

`(fieldName, useKey, resultTyVar)` — the use-site NodeKey is for
diagnostics, and `resultTyVar` is the expression's own TyVar that needs
to be unified with the field type once the receiver resolves.

Mechanically this is one new axis to merge in `migrateBounds` (list
append on union; drain on `Link` set).

## Pass-by-pass changes

### `NameResolution`

Two new responsibilities — both module-level passes over `TypeDefn.Record`s:

```fsharp
// Passes/NameResolution.fs walkModuleElem, new arm for ModuleElem.Type:
| ModuleElem.Type typeDefns ->
    for td in typeDefns do
        match td with
        | TypeDefn.Record(typeName = TypeName(ident = nameLi); fields = fields) ->
            registerRecordType ctx nameLi fields
        | _ -> ()    // Union/Abbrev/Anon/etc. handled in their own plans
```

`registerRecordType` translates the field types via the existing
(Unification-private) `translateType` — except `translateType` lives in
Unification. Move it to a shared helper module, or have NameResolution
stamp a placeholder TyVar per field and let Unification fill in the
type when it sees the same `TypeDefn.Record` again. The placeholder
approach matches how Binding entries work today: NameResolution writes
shape information, Unification writes types.

```fsharp
let private registerRecordType (ctx: PassContext) (nameLi: LongIdent) (fields: RecordFields) =
    let name = ctx.NameOf nameLi.Idents.[0]
    if ctx.RecordTypes.ContainsKey name then
        ctx.Diagnostics.Add { Key = …; Message = $"Duplicate record type: {name}"; Severity = Error }
    else
        let fieldInfos =
            [|
                for f in fields do
                    let (RecordField(mutableToken = mt; ident = id; …)) = f
                    let name = ctx.NameOf id
                    yield {
                        Name = name
                        Type = TyVar(freshTyVar ctx)   // filled by Unification
                        IsMutable = mt.IsSome
                        DeclKey = CstKeys.ofRecordField f
                    }
            |]
        let info = { Name = name; Fields = fieldInfos; DeclKey = CstKeys.ofTypeDefn td }
        ctx.RecordTypes.[name] <- info
        for fi in fieldInfos do
            match ctx.FieldIndex.TryGetValue fi.Name with
            | true, infos -> ctx.FieldIndex.[fi.Name] <- info :: infos
            | false, _ -> ctx.FieldIndex.[fi.Name] <- [info]
```

The order matters: NameResolution must populate `RecordTypes` *before*
walking expressions, so a record literal in the same module can resolve
back to a record defined later in the source. This is the same shape as
let-bindings inside a `let rec` group — collect declarations first,
infer bodies second. For modules with mixed type defs and expressions,
process all `ModuleElem.Type`s before any `ModuleElem.FunctionOrValue` /
`ModuleElem.Expression`.

No change to scope handling: record field names are not part of
lexical scope. `r.X` doesn't introduce X anywhere.

### `Unification`

Five new `inferExpr` / `inferPat` arms plus the deferred-constraint
plumbing.

**Field-type fill-in.** Right after NameResolution writes the registry,
Unification walks `ctx.RecordTypes` and runs `translateType` over each
field's CST type. The placeholder TyVar gets `Link` set to the real
type. Module field types may reference other records declared in the
same file — `RecordTypes` is fully populated by this point so the
lookup works.

**Record literal.** `inferExpr` on `Expr.Record(fieldInitializers = inits)`:

```fsharp
| Expr.Record(fieldInitializers = inits) ->
    let names = [ for FieldInitializer(longIdent = li; …) in inits -> qualifiedFieldName ctx li ]
    let qualifier, plainNames = splitQualifier names
    let candidate =
        match qualifier with
        | ValueSome typeName ->
            match ctx.RecordTypes.TryGetValue typeName with
            | true, info -> ValueSome info
            | false, _ ->
                diag "Unknown record type qualifier"
                ValueNone
        | ValueNone -> findUniqueRecordByFieldSet ctx plainNames
    match candidate with
    | ValueNone -> // diagnostic emitted; type as a fresh TyVar to avoid cascades
        TyVar(freshTyVar ctx)
    | ValueSome info ->
        validateFieldSetMatches ctx info plainNames
        for FieldInitializer(longIdent = li; expr = e) in inits do
            let fieldName = ctx.NameOf (lastSegment li)
            match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
            | Some field ->
                let eTy = inferExpr ctx e
                unify ctx (CstKeys.ofExpr e) eTy field.Type
            | None -> diag (sprintf "Unknown field: %s" fieldName)
        TyRecord info.Name
```

**Field access.** `inferExpr` on `Expr.DotLookup(expr = r; longIdentOrOp = LongIdent li)` when `li.Idents.Length = 1`:

```fsharp
| Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
    let fieldName = ctx.NameOf li.Idents.[0]
    let rTy = inferExpr ctx r
    match resolveStep rTy with
    | TyRecord recName ->
        let info = ctx.RecordTypes.[recName]
        match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
        | Some field -> field.Type
        | None ->
            diag $"Type '{recName}' has no field '{fieldName}'"
            TyVar(freshTyVar ctx)
    | TyVar tv ->
        // Defer: park the constraint on the TyVar. When unify links it
        // to a TyRecord, the constraint drains.
        let root = UnionFind.find tv
        let resultTv = freshTyVar ctx
        root.PendingFieldAccess <- (fieldName, CstKeys.ofExpr e, resultTv) :: root.PendingFieldAccess
        TyVar resultTv
    | other ->
        diag $"Cannot read field '{fieldName}' from non-record type"
        TyVar(freshTyVar ctx)
```

Multi-segment `DotLookup` falls through to the existing path (which today
emits a diagnostic for unresolved `.A.B.C` chains).

**Field assignment.** `inferAssignment` already handles `Expr.Assignment`
by typing LHS / RHS and unifying. The only new piece is that
`Expr.DotLookup` is a valid LHS — and `inferExpr` on the LHS just
produces the field type, which then unifies with the RHS as usual.
**Validation** owns the mutability check (see below).

**Record clone.** `inferExpr` on `Expr.RecordClone`:

```fsharp
| Expr.RecordClone(expr = r; fieldInitializers = inits) ->
    let rTy = inferExpr ctx r
    match resolveStep rTy with
    | TyRecord recName ->
        let info = ctx.RecordTypes.[recName]
        for FieldInitializer(longIdent = li; expr = e) in inits do
            let fieldName = ctx.NameOf (lastSegment li)
            match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
            | Some field ->
                let eTy = inferExpr ctx e
                unify ctx (CstKeys.ofExpr e) eTy field.Type
            | None -> diag $"Type '{recName}' has no field '{fieldName}'"
        TyRecord info.Name
    | _ ->
        diag "Record clone requires the source expression to be a record"
        TyVar(freshTyVar ctx)
```

Note: record clone with a free receiver TyVar is deferred to v1.5 — for
v1, r's type must already resolve.

**Record pattern.** `inferPat` on `Pat.Record`:

Mirror the literal arm. The scrutinee's type (unified with `TyRecord T`)
drives, plus the same field-set lookup when no qualifier and no
scrutinee constraint is known.

**Constraint draining in `unify`.** When unify links a `TyVar tv` to
`TyRecord recName`, walk `tv.PendingFieldAccess` and resolve each:

```fsharp
// Inside the TyVar-to-TyRecord arm of unify:
let root = UnionFind.find tv
root.Link <- ValueSome (TyRecord recName)
let info = ctx.RecordTypes.[recName]
for (fieldName, useKey, resultTv) in root.PendingFieldAccess do
    match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
    | Some field ->
        unify ctx useKey (TyVar resultTv) field.Type
    | None ->
        ctx.Diagnostics.Add { Key = useKey; Message = sprintf "Type '%s' has no field '%s'" recName fieldName; Severity = Error }
root.PendingFieldAccess <- []
```

`migrateBounds` (called during TyVar–TyVar union) needs a list-append
for `PendingFieldAccess` too — same shape as the existing
`IfaceBounds` migration.

### `Regions`

Records are allocations. Each `Expr.Record` mints one region for the
record itself, with one outgoing edge per field initialiser (record
holds field → record outlives field — same direction as the
tuple-holds-item rule):

```fsharp
| Expr.Record(fieldInitializers = inits) ->
    let recR = s.Graph.Fresh(level = s.EnclosingLet, mintFn = functionStackTop s,
                              isLambda = false, isMutableCell = false, seed = ValueNone)
    for FieldInitializer(expr = e) in inits do
        let eR = inferRegion s ctx e
        s.Graph.AddEdge(recR, eR)         // record ≥ each field's value
    recR
```

**Mutable fields**: each `mutable` field needs its own cell region —
same machinery as `let mutable`. The cell is minted at the record's
mint level, not the access level (the cell lives wherever the record
does). For `r.X <- v` where X is mutable:

```fsharp
// In Regions' Expr.Assignment arm, when LHS is Expr.DotLookup on a
// mutable field, AddEdge from rhs region to the field's cell region:
| Expr.Assignment(leftExpr = (Expr.DotLookup(...) as lhs); rightExpr = r) ->
    let fieldCellR = inferRegion s ctx lhs    // returns the field's cell region
    let rhsR = inferRegion s ctx r
    s.Graph.AddEdge(rhsR, fieldCellR)         // rhs ≥ cell (same direction as let mutable)
    RegionId.Unknown
```

The field's cell region is computed once per record-literal site
(stored on the field's NodeKey via `s.BindingRegions`) and looked up at
every `r.X` access. For records that aren't allocation sites in the
current file (`fun (r : Point) -> r.Y <- 5` — r is a parameter), the
parameter's region serves as the receiver and field cells share it
(same as how tuples treat their elements — coarser-than-precise but
sound).

**Record clone**: the cloned-from record's region is shared as the
clone-result region for fields that are reused; overridden fields get
new edges from their override RHSes. For v1, model this conservatively:
new region for the clone, edge to the source record's region, edges
from each override field's RHS.

`exprIsAllocation` already returns `true` for `TyRecord _` once it's
added to `isAllocation`'s arm list — see §Data-model.

**Conservative fallback retreat**: today's `Expr.Record` route through
the catch-all `HeapShared` fallback. Once the precise rules above land,
record literals are subject to the same level / lambda-reach
classification as tuples and closures.

### `Validation`

Two new diagnostics:

**1. Assignment to an immutable field.** Extend `checkAssignment` to
handle `Expr.DotLookup` LHSes:

```fsharp
let private checkAssignment (ctx: PassContext) (l: Expr<SyntaxToken>) : unit =
    let rec unwrap e =
        match e with
        | Expr.EnclosedBlock(expr = inner)
        | Expr.TypeAnnotation(expr = inner) -> unwrap inner
        | _ -> e
    let core = unwrap l
    match core with
    | Expr.Ident _
    | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent _) ->
        // … existing immutable-binding check …
    | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
        // Look up r's resolved type → ctx.RecordTypes → field's IsMutable.
        let rKey = CstKeys.ofExpr r
        match ctx.TypeVar.TryGetValue rKey with
        | ValueSome tv ->
            match Unification.zonk (TyVar tv) with
            | TyRecord recName ->
                let fieldName = ctx.NameOf li.Idents.[0]
                match ctx.RecordTypes.TryGetValue recName with
                | true, info ->
                    match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
                    | Some field when not field.IsMutable ->
                        ctx.Diagnostics.Add { Key = CstKeys.ofExpr core
                                              Message = sprintf "Cannot assign to immutable field '%s'" fieldName
                                              Severity = Error }
                    | _ -> ()
                | false, _ -> ()
            | _ -> ()
        | ValueNone -> ()
    | _ -> ()
```

**2. Deferred field access never resolved.** After `Unification` runs,
walk `ctx.TypeVar.AsDictionary()` once. For each TyVar whose root has
`PendingFieldAccess` non-empty (i.e. the receiver type never pinned to
a `TyRecord`), emit a diagnostic per pending access:

```fsharp
let private checkUnresolvedFieldAccesses (ctx: PassContext) =
    for kv in ctx.TypeVar.AsDictionary() do
        let root = UnionFind.find kv.Value
        for (fieldName, useKey, _) in root.PendingFieldAccess do
            ctx.Diagnostics.Add { Key = useKey
                                  Message = sprintf "Cannot resolve field '%s': receiver type was never constrained to a record type" fieldName
                                  Severity = Error }
```

Wire this into `Validation.run` after the walker pass, before
`checkValueRestriction`.

### `Freeze`

Add `TPat.Record` and `TExpr.RecordCons` / `TExpr.RecordClone` /
`TExpr.FieldGet` / `TExpr.FieldSet`:

```fsharp
| RecordCons of fields: (string * TExpr) list * ty: SemType         // ty : TyRecord
| RecordClone of source: TExpr * overrides: (string * TExpr) list * ty: SemType
| FieldGet of receiver: TExpr * fieldName: string * ty: SemType    // field's type
| FieldSet of receiver: TExpr * fieldName: string * value: TExpr * ty: SemType   // always unit
```

Record patterns:

```fsharp
| TPat.Record of fields: (string * TPat) list * ty: SemType        // ty : TyRecord
```

Field reads via `Expr.DotLookup` fold to `FieldGet`; field-LHS
assignments via `Expr.Assignment(Expr.DotLookup _, …)` fold to
`FieldSet` (which is unit-typed, like the existing `Assignment`).

`prettyDecl` / `TastShape` grow render arms — `"{ X = …; Y = … }"`,
`"r.X"`, `"r.X <- …"`.

## Pipeline integration

No new pass. Records are handled by extending NameResolution,
Unification, Regions, Validation, and Freeze — same shape as
[`mutable-plan.md`](mutable-plan.md). The order within
`walkModuleElem` matters: types must be registered before expressions
walk (so a literal can resolve to a record declared later in source).
The fix lives entirely inside NameResolution's `run`:

```fsharp
let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
    let elems = … // same as today
    // First pass: register every record type so subsequent expression
    // walks can resolve literals / field accesses against the registry.
    for m in elems do
        match m with
        | ModuleElem.Type defs -> registerTypeDefns ctx defs
        | _ -> ()
    // Second pass: name resolution proper, including expression bodies
    // that may reference any registered record.
    for m in elems do
        walkModuleElem walker [] m
```

Validation extends `walkModuleElem` to walk `ModuleElem.Type` (currently
`failwith`ed after mutable-plan) — there's nothing to validate at type-
decl level for v1, so this is one line that becomes `| ModuleElem.Type
_ -> ()`. The `failwith` was a "surface unhandled cases" sentinel; this
plan answers `Type`.

## TAST representation

The frozen TAST gains four new `TExpr` cases and one new `TPat`
case (above). The `ty` slot on each is `TyRecord recName`; downstream
consumers (codegen, target plugins) look up `ctx.RecordTypes[recName]`
for the field list, same as Unification did.

Cross-file: `RecordTypes` is currently a per-file `PassContext` field.
When namespaces / cross-file resolution land, the registry layers over
a provider-backed equivalent — at TAST consumption time, the consumer
needs both the local registry and the provider's named-type catalogue.
The TAST itself doesn't carry the field list inline — same posture as
[`measures-plan.md`](measures-plan.md#tast-representation): the measure
rides on the type, full info is reachable from the type.

## Test strategy

Tests split across the five pass-test files. Each pass owns its own
half of the contract:

**`NameResolutionTests.fs`:**

1. **Record type registered.** `type R = { X: int }` — `ctx.RecordTypes["R"].Fields.Length = 1`.
2. **Duplicate type name diagnoses.** Two `type R = …` in one file → diagnostic.
3. **Field index built.** `type R = { X: int; Y: int }` — `ctx.FieldIndex["X"]` and `ctx.FieldIndex["Y"]` both contain R.

**`UnificationTests.fs` (likely a new file, since `CoverageTests.fs`
currently bundles general type-checking):**

4. **Record literal infers from field set.** `type R = { X: int; Y: int } in let r = { X = 1; Y = 2 }` — `r : TyRecord "R"`.
5. **Ambiguous field set requires qualifier.** Two types with `{X, Y}` field set — bare `{ X = 1; Y = 2 }` diagnoses "ambiguous"; `{ R.X = 1; Y = 2 }` resolves.
6. **Field-set mismatch.** `{ X = 1; Z = 3 }` against `type R = { X: int; Y: int }` — diagnostic.
7. **Field access on annotated parameter.** `fun (r : Point) -> r.X` — types as `Point -> int`.
8. **Field access on free TyVar pinned later.** `let f r = r.X in let _ = f { X = 1; Y = 2 }` — `f : R -> int` after the call constrains r.
9. **Field access on free TyVar never pinned.** `let f r = r.X` standalone — Validation diagnostic.
10. **Record clone types as the source.** `let p2 = { p with Y = 5 }` — `p2 : TyRecord "Point"`.
11. **Record clone validates field names.** `{ p with Z = 5 }` against R with no Z — diagnostic.

**`RegionsTests.fs`:**

12. **Record literal at module top is LocalStack.** `let r = { X = 1; Y = 2 }` at module level — `LocalStack`.
13. **Record returned from a function is CallerStack.** `let mk () = { X = 1; Y = 2 }` — `CallerStack`.
14. **Mutable field captured by escaping closure is HeapShared.** `let mk () = let r = { X = 0; mutable Y = 0 } in fun () -> r.Y <- r.Y + 1` — r's cell for Y is HeapShared.
15. **Record clone shares region with source.** Lower priority — defer to v1.5.

**`ValidationTests.fs`:**

16. **Assignment to immutable field diagnoses.** `let p = { X = 1; Y = 2 } in p.X <- 5` — diagnostic ("X is not mutable").
17. **Assignment to mutable field is clean.** `let p = { X = 1; mutable Y = 2 } in p.Y <- 5` — no diagnostic.
18. **Unresolved field-access diagnoses.** `let f r = r.X` (no use of f, r stays free) — diagnostic.
19. **Resolved-by-use field access is clean.** `let f r = r.X in f { X = 1; Y = 2 }` — no field-access diagnostic.

**`CoverageTests.fs`:** golden TAST shapes for record cons, clone, field
get, field set, and record pattern.

Each test follows the existing pattern — `analyse` → `declType` →
`Expect.equal` / `Expect.isEmpty Diagnostics`.

## Open questions

- **Field-name registry scope when modules land.** Today
  `ctx.RecordTypes` is per-file. Across modules (`module A; type R = …`
  used from `module B`), B needs A's record types. When namespaces /
  modules land in the parser → semantic-analysis surface, the registry
  becomes hierarchical (per-module overlays) and the lookup goes
  through scope. Defer the design to that work — for v1 a flat
  single-file table is enough.
- **Record types from external assemblies.** `IExternalSymbolProvider`
  has no notion of a named-type catalogue today. When .NET integration
  brings in `System.DateTime` etc., the provider needs a parallel
  `TryLookupType` channel. Records lands first with same-file types
  only; the provider extension lands with the .NET provider's first
  named-type entries.
- **Generic records.** `type Box<'a> = { Value: 'a }`. Requires typar
  scoping (which `translateType` punts on today — see the wildcard arm
  at `Unification.fs:480`). The registry shape extends naturally
  (`RecordTypeInfo.TypeParams: TypeVar list`); instantiation at literal
  / access sites mints fresh TypeVars per use. Out of scope for v1.
- **Anonymous records (`{| X = 1; Y = 2 |}`).** Different runtime
  shape — no declaration to register; the type *is* its field set.
  Effectively `TyAnonRecord of (string * SemType) list`. Out of scope
  for v1; lands as a follow-up.
- **Record field shadowing of let bindings.** `let X = 1 in r.X` — X
  the let-binding has nothing to do with X the field. Verify that the
  field-access path never looks at `ctx.Binding`; this is the case
  today because field resolution goes through r's type, not lexical
  scope. Worth a test (a let-bound X in scope at the access site).
- **Pattern exhaustiveness on records.** `match r with { X = 1 } -> …`
  is incomplete (X = 2, X = 3, … all missing). Record-pattern
  exhaustiveness is part of the broader pattern-match exhaustiveness
  work — defer to that plan.
- **Field-init `with` over a function call.** `{ f () with X = 5 }`
  evaluates `f ()` exactly once (it's a value, not a record-clone
  template). Today's CST handles the syntax; the only twist is that
  Regions needs to treat the cloned-from expression as having a
  region (not just an Ident lookup). Captured by the precise-region
  rule for `RecordClone` above.

## Out of scope for this plan

- **Generic records** (`type Box<'a> = …`). Lands with typar scoping.
- **Anonymous records** (`{| … |}`). Different runtime shape.
- **Record-pattern exhaustiveness.** Part of the broader pattern-match
  exhaustiveness work in Validation.
- **Cross-file record resolution.** Lands with namespaces / modules.
- **External (BCL-defined) named types as records.** Lands with the .NET
  provider's named-type catalogue.
- **Struct records** (`[<Struct>] type R = …`). Same field-resolution
  story; different region treatment (struct allocation is inline, not
  heap). Lands with structs.
- **Field-level access modifiers** (`private mutable X: int`). v1
  ignores `access` token; lands when module visibility does.
- **Reference-equality on records.** F# records are value-equal by
  default; `[<ReferenceEquality>]` flips it. Validation diagnostic for
  attribute-disallowed shapes lands with attributes in general.
- **`{| r with X = 5 |}` anonymous-record clone.** With anonymous
  records.
