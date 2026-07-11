# Discriminated unions — architecture

How the semantic-analysis pipeline handles F# discriminated unions, as
built. Supersedes `du-plan.md` (the original monomorphic-DU landing
plan): the implementation has since grown generics, arity-overloading,
augmentation members, equality/comparison postures, and external
(referenced-package) union resolution, so the plan's data model and
"out of scope" list no longer describe the code. File:line references
are anchors, not contracts — they drift.

DUs share their machinery with records and classes: one named-type
registry entry per declaration, constructor-name-driven inference, and
pattern support threaded through every pass. The three nominal kinds
(`TyRecord` / `TyUnion` / `TyClass`) carry the same shape — a resolved
`SymbolKey` plus a type-argument vector — and most unification sites
treat them with one combined arm.

The canonical surface handled today:

```fsharp
// Definition — cases of varying arity, optionally generic.
type Shape =
    | Circle of float
    | Rectangle of float * float
    | Point

type Lst<'T> =
    | Cons of 'T * Lst<'T>
    | Nil

// Nullary ctor → the declaring union.
let p = Point                       //  p : Shape

// Single- and multi-arg ctor applications.
let c = Circle 1.0                  //  c : Shape
let r = Rectangle(2.0, 3.0)         //  r : Shape

// Ctor as a value → arg -> union.
let mkCircle = Circle               //  mkCircle : float -> Shape

// Pattern match — receiver-driven, reaches a TyUnion.
let area s =
    match s with
    | Circle r -> 3.14 * r * r
    | Rectangle(w, h) -> w * h
    | Point -> 0.0                  //  area : Shape -> float

// Disambiguation when two unions share a case name.
type Result1 = | Ok of int | Err of string
type Result2 = | Ok of float | Err of string
let r1 : Result1 = Ok 1            // pinned by annotation
let r2 = Result2.Ok 1.0           // qualified prefix

// External (referenced-package) unions resolve through the provider.
let o = Some 1                     //  o : Option<int>
```

## Data model

### `SemType.TyUnion` (`SemanticInfo.fs:314`)

```fsharp
| TyUnion of key: SymbolKey * args: EqArray<SemType>
```

Identical shape to `TyRecord` (`:311`) and `TyClass` (`:318`). Case
data is **not** stored inline — it is reachable from the union registry
by `key` (`TypeRegistry.tryUnionByKey`). The `key` is an arity-qualified
`SymbolKey` (`Choice\`2` vs `Choice\`7`); `args` is the type-argument
vector for generic unions. Two `TyUnion`s unify iff their keys are equal
**and** their args unify pairwise (`Engine.fs:559`).

`FrozenType` has the post-freeze counterpart `FTUnion`; surviving open
typars are rewritten to `TyTypar` at freeze time.

### Registry (`PassContext.fs`)

`UnionCaseInfo` (`:145`) — one per case:

- `Name`, `UnionName`, `UnionArity` (the declaring union's generic
  arity; pairs with `UnionName` so arity-overloaded short names resolve
  to the right union — `TypeRegistry.unionOfCase` keys on
  `(UnionName, UnionArity)`).
- `Fields : SemType[]` — declaration-order field types. Length 0 for
  nullary. Start as placeholder `TyVar`s, linked later (see fill-in).
- `FieldNames : string voption[]` — per-field names for named fields
  (`| Case of x: int`); `ValueNone` for positional. Stored even when
  unused so a future named-field-pattern pass can drive off it.
- `DeclKey`.

`UnionTypeInfo` (`:162`) — one per declaration:

- `Name`, `Key` (arity-qualified `SymbolKey`, minted by
  `stampLocalTypeKey` to match emitted metadata), `TypeParams`
  (`EqArray<string * TypeVar>` — generic params; case field types may
  reference these directly), `Cases`, `DeclKey`.
- `TyparConstraints` — `when 'a : …` clauses, attached to prototype
  TyVars during fill-in.
- `Members : TypeMemberInfo[]` — augmentation members (`with member …`
  / `static member …`); types linked by `fillUnionMembers`. Empty for a
  plain union.
- `ThisName` / `ThisKey` — `this`-binder for instance member bodies.
- `EqualitySupport` (default `Structural`) / `ComparisonSupport`
  (default `NoComparison`) — postures decoded from attributes at
  registration, read by `checkConstraint`, projected onto the TAST
  type-decl for codegen.

Storage lives on `PassContextTypes` (`PassContext.fs`):

- `Union : Dictionary<string, UnionTypeInfo>` (`:458`) — by union name,
  arity-overloaded via `TypeRegistry` keying.
- `CtorIndex : Dictionary<string, EqArray<UnionCaseInfo>>` (`:469`) —
  reverse index, ctor name → bucket of declaring cases. A bucket with
  more than one entry means the name is ambiguous and needs a qualifier
  or annotation.

No new `TypeVar` axis: ctor references resolve at their use site (the
name is in `CtorIndex` or it isn't), so there is no deferred constraint
like records' pending-field-access gate.

## Pass-by-pass

### NameResolution — registration (`Passes/NameResolution/TypeRegistration.fs`)

`registerUnionTypeDefn` (`:265`) walks each `TypeDefn.Union`:

- Single-segment type names only; multi-segment is skipped.
- `inspectCaseData` (`:224`) pulls each case's name + arity + per-field
  names. It handles plain `Nullary`/`Nary`, operator-named cases
  (`[]` / `::`), and the explicit-return **GADT-syntax** forms FSharp
  uses for `list` (`GadtNullary` / `GadtNary`) — their return type is
  treated as the declaring union. True GADTs remain out of scope.
- Arity-qualified duplicate check (`Choice\`2` and `Choice\`7` coexist;
  a name+arity collision, or a clash with a record of the same name,
  diagnoses).
- Stamps one placeholder `TyVar` (Level 0) per case field.
- Mints the arity-qualified `Key` via `stampLocalTypeKey`, decodes
  equality/comparison attributes, registers into `ctx.Types.Union`,
  records the decl-site `ResolvedType` mapping, and appends each case to
  `CtorIndex`.

Run order (`NameResolution.fs:354`): records, then unions, then classes,
then expression walks. The three registry populations are mutually
independent.

Pattern binders: `bindingsOfPat` recognises ctor patterns (head name
binds nothing; recurse into sub-patterns) and uppercase nullary-ctor
`NamedSimple` reinterpretations (bind nothing) ahead of the
"name-binds-itself" arm.

### Unification

**Field-type fill-in** (`Passes/Unification.fs`). After the registry is
fully populated, `walkElems` (`:809`) runs the fill passes in order:
`fillRecordFieldTypes` → `fillUnionFieldTypes` (`:98`) → `fillClassMembers`
→ `fillUnionMembers` (`:760`) → expression walk. `fillUnionFieldTypes`
walks each `TypeDefn.Union`, sets the union's typar scope, translates
every field's CST type index-aligned with the registered cases, and
`Link`s each placeholder field `TyVar` to the translated type — so a
case may reference another type declared elsewhere in the file.
`fillUnionMembers` fills augmentation member signatures via the shared
`fillTypeMembers` driver with `MkSelfType = fun args -> TyUnion(Key, args)`.

**Constructor references** (`Passes/Unification/Infer.fs`,
`InferResolve.fs`):

- `ctorType` (`InferResolve.fs:77`) builds the ctor's type from a
  `UnionCaseInfo`: instantiates the declaring union's typars fresh,
  substitutes them into the field types, and returns `TyUnion` for
  nullary, `TyFun(field, TyUnion)` for single-field, or
  `TyFun(TyTuple fields, TyUnion)` for multi-field (F# DUs take a tuple).
- Bare single-segment names resolve in `inferIdentDefault`
  (`Infer.fs:168`): local binding → provider → `resolveCtorName`
  (`CtorIndex`) → external ctor (`tryExternalCtorType`) → class-ctor-as-
  function. A let-bound name shadows a ctor (it has a `Binding` entry and
  never reaches the ctor path). `resolveCtorName` (`InferResolve.fs:134`)
  returns the unique case, or `count >= 2` to drive the ambiguity
  diagnostic.
- Two-segment qualified names (`Infer.fs:124`) cascade class static
  member → union static member → qualified ctor (`resolveQualifiedCtor`,
  bypassing the `CtorIndex` ambiguity check) → external qualified case.
- Application is ordinary function application: the ctor's `TyFun` type
  flows through the normal `App` arm, unifying the argument (a tuple for
  multi-field) against the declared field shape. Arity / type mismatch
  diagnoses there.

**Constructor patterns** (`Passes/Unification/InferPat.fs`):

- `Pat.NamedSimple` with an uppercase-leading name in `CtorIndex`
  (`:37`) is reinterpreted as a nullary ctor pattern: resolve the case,
  instantiate the union fresh, link the node to `TyUnion`. Non-nullary
  used nullary, or an ambiguous name, diagnoses. An external-union
  variant (`:86`) resolves through `ctx.Provider.TryLookupUnionCase`.
- `Pat.Named` (ctor pattern with args) resolves the case (bare via
  `CtorIndex`, qualified via the union registry, or external), unwraps
  the parser's single tuple argument, and unifies each sub-pattern
  against the corresponding field type.
- A lowercase or unknown `NamedSimple` falls through to the ordinary
  binder arm (`:117`), reusing any TyVar a forward reference pre-minted.

**Unification engine arms** (`Passes/Unification/Engine.fs`): `zonk`
(`:42`), `occursAndAdjust` (`:131`), `substituteWith` (`:184`), `unify`
(`:559`, key + arg-vector equality), the nominal-kind classifier
(`:419`, `:512`), and the constraint checker — equality/comparison on a
`TyUnion` reads `EqualitySupport`/`ComparisonSupport` and recurses into
case field types (`:781`); `Struct`/`ReferenceType` constraints reject /
accept structurally (`:804`, `:809`).

`translateType` (`Passes/Unification/Translate.fs`) maps a
`Type.NamedType` to `TyUnion` for both local (`:179`, `:367`) and
external (`:441`) unions, threading translated type arguments.

### Regions (`Passes/Regions.fs`)

`isAllocation` returns `true` for `TyUnion _` (`:173`) — ctor
applications mint a region with one outgoing edge per argument; nullary
ctors are treated as allocations for uniformity (conservative). No new
ctor-specific region arm: a ctor application is just an `Expr.App` whose
result types as `TyUnion`. `bindersOfPat` extracts binders from
`TPat.Union` sub-patterns (`:254`, `:609`).

### Freeze / TAST (`Tast.fs`, `ElaborateExpr.fs`)

Two TAST cases:

```fsharp
// Tast.fs:50
| Union of caseName: string * fields: EqArray<TPatG<'ty>> * ty: 'ty
// Tast.fs:159
| UnionCons of caseName: string * args: EqArray<TExprG<'ty>> * ty: 'ty
```

`ty` is always the `TyUnion`; the declaring union is recovered via the
registry / `CtorIndex` at consumption time. Freeze translation
(`ElaborateExpr.fs`):

- Ctor reference outside an `App` (`:830`): a `TyUnion`-typed reference
  is a nullary ctor → `UnionCons(name, [], ty)`; a `TyFun`-typed one is
  ctor-as-value → `TExpr.External(name, …)` (codegen eta-expands when it
  cares).
- Ctor application (`:846`) / high-precedence application (`:848`):
  peel the argument(s) into a per-field list → `UnionCons`.
- `h :: t` and `[…]` literals lower to `UnionCons` against the resolved
  list union (`:1481`, `:1557`) — the local cons-list and the external
  FSharp.Core / Vesper list share this shape.
- Patterns translate to `TPat.Union` (nullary, list nil/cons, and
  multi-arg ctor forms).

Local and external ctors emit identical `UnionCons` nodes — the external
path is distinguished only by where the case was resolved, not by the
TAST shape.

## Tests

- `NameResolutionTests.fs` — union registration, arity-qualified keys,
  duplicate diagnostics, `CtorIndex` reverse-mapping, nullary-ctor
  pattern binding, generic union `TypeParams`, decl-site / use-site
  `ResolvedType` stamping.
- `UnificationTests.fs` — ctor reference / application inference, pattern
  inference, ambiguity, arity/type mismatch.
- `RegionsTests.fs` — ctor allocation classification.
- `CoverageTests.fs` — golden TAST shapes end-to-end.
- Codegen `UnionTests.fs` / `ChoiceTests.fs` — monomorphic and generic
  construction + match, arity-overloaded `Choice\`2`…`Choice\`7`,
  recursive `Lst<'T>` with `foldl`.

## Known gaps

- **Named-field constructor patterns** (`Pat.NamedFieldPats`,
  `| Circle(radius = r)`). `UnionCaseInfo.FieldNames` already stores the
  data; there is no `inferPat` arm yet. A follow-up unifies each named
  sub-pattern against the named field's type rather than by position.
- **Match-arm exhaustiveness.** `match s with | Circle _ -> …` is
  accepted even though `Rectangle` / `Point` are uncovered. Belongs to
  the broader pattern-completeness work in Validation; the current
  contract is "no spurious diagnostics" on incomplete matches.
- **Struct DUs** (`[<Struct>] type …`). Same ctor-resolution story but a
  different region/codegen treatment; not modelled.
- **True GADTs.** The GADT *syntax* forms are accepted and treated as
  ordinary cases (their return type names the declaring union — what
  FSharp.Core's `list` needs); genuine generalized-ADT typing is out of
  scope.
- **Active patterns** (`| MyActive p ->`). Superficially ctor-shaped but
  declared via `let (|Foo|_|) =`; needs its own resolution path.
- **`[<RequireQualifiedAccess>]`** enforcement on union ctor references
  lands with attribute handling in general.
- **Multi-segment qualifiers** (`A.B.Case`). Resolution handles
  single-segment and two-segment forms; deeper qualifiers fall through.
