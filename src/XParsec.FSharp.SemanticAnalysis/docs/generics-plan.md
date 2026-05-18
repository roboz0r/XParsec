# Generics plan

The build plan for **user-written type parameters** in the
semantic-analysis pipeline — `'a` typars in let-bindings, in record /
DU type definitions, and in type annotations. Builds directly on
[`generalisation-plan.md`](generalisation-plan.md): generalisation
already produces `TypeScheme`s with quantified TyVars, and `instantiate`
already mints fresh ones per use site. What's missing is the *surface
syntax* that lets users name their own typars and the *named-type*
machinery that lets `Option<int>` mean a specific instantiation of
`type Option<'a>`.

The status quo is silent on typars. `Type.VarType` (the `'a` in
`let f (x : 'a) = x`) falls through `translateType`'s wildcard arm to a
**fresh, unscoped** TyVar — so two `'a`s in the same signature don't
share identity. `Type.GenericType` lands in the same wildcard arm
(except for the measure-shaped numeric cases). `TypeDefn.Record` /
`TypeDefn.Union` ignore their `prefixTypars` / `typarDefns` slots
entirely: `type Box<'a> = { Value: 'a }` registers as the zero-arity
record `Box` whose `Value` field types as a free TyVar. `TyparDefns` and
`TyparConstraints` are parsed but never consulted.

The canonical examples we want to handle after this lands:

```fsharp
// Generic let-binding with annotated typar.
let id<'a> (x : 'a) : 'a = x
//    id : 'a -> 'a       (already works in v1 via let-generalisation;
//                          the typar annotation just names it)

// Two annotations sharing the same typar identity.
let pair (x : 'a) (y : 'a) : 'a * 'a = x, y
//    pair : 'a -> 'a -> ('a * 'a)
let _ = pair 1 2          // fine
let _ = pair 1 "hello"    // mismatch — both args share 'a

// Generic record.
type Box<'a> = { Value: 'a }
let b1 = { Value = 1 }            // b1 : Box<int>
let b2 : Box<string> = { Value = "x" }

// Generic DU.
type Option<'a> = | Some of 'a | None
let s = Some 1                     // s : Option<int>
let n : Option<string> = None      // None instantiates to Option<string>

// Generic function that destructures a generic value.
let unwrap (b : Box<'a>) : 'a = b.Value
//    unwrap : Box<'a> -> 'a
```

## Goal

After the pipeline finishes:

- **Type-parameter scope.** Every `'a` token inside a single signature
  (binding-level annotation, record/DU declaration, type annotation
  inside an expression body) resolves to **one** `TypeVar` for the
  duration of that signature. Two `'a` tokens in the same binding's
  arg types share identity; `'a` in one binding is independent from
  `'a` in another.
- **Generic registry entries.** `RecordTypeInfo` and `UnionTypeInfo`
  grow a `TypeParams: TypeVar list` field — the quantified typars of
  the declared type. Each `TyRecord` / `TyUnion` carries its arg list
  inline so two instantiations with different args don't unify
  (`Box<int> ≠ Box<string>`).
- **`Type.VarType` translation** produces a TyVar drawn from the
  surrounding signature's typar scope, not a fresh one per occurrence.
- **`Type.GenericType` translation** looks up the named type, mints
  fresh TyVars to play the role of the type's parameters, unifies them
  against the translated type-arg list, and returns the instantiated
  `TyRecord` / `TyUnion`.
- **Record literal / ctor reference** at a generic record / DU
  instantiates the type's scheme — fresh TyVars for the params, body
  is the field / ctor-arg type with params substituted.
- **TAST** carries the instantiated arg list on `TyRecord` / `TyUnion`
  so downstream consumers see `Box<int>` rather than the bare `Box`.

What this plan does **not** deliver (carried in [§Out of scope](#out-of-scope)):
constraints (`when 'a :> IComparable`, SRTPs), variance, higher-kinded
types (`'f<'a>` where `'f` is itself a typar), type abbreviations of
generic types, statically-resolved `^a` typars, `'a` typars in
class-member signatures.

## What we have to build on

| Piece                                  | Where                                           | Status |
|----------------------------------------|-------------------------------------------------|--------|
| `Type.VarType of typar: Typar<'T>`     | `XParsec.FSharp/Expr.fs:71`                     | Done — `Typar.Named/Anon/Static`. |
| `Type.GenericType`                     | `Expr.fs:73`                                    | Done — `longIdent * lAngle * typeArgs * commas * rAngle`. |
| `Typar.Named` (`'a`)                   | `Expr.fs:106`                                   | Done — `quote * ident`. |
| `Typar.Anon` (`_`)                     | `Expr.fs:105`                                   | Done — wildcard typar. |
| `Typar.Static` (`^a`)                  | `Expr.fs:107`                                   | Done — SRTP typar. |
| `TypeArg.Type` / `.Measure`            | `Expr.fs:100`                                   | Done — already used by measure path. |
| `TyparDefns` / `TyparDefn`             | `Expr.fs:109-112`                               | Done — `<'a, 'b when …>` shape on let / type. |
| `TyparConstraints`                     | `Expr.fs:113`                                   | Done — `when` clause, lands later. |
| `PrefixTypars` (ML-style)              | `Expr.fs:479`                                   | Done — `'a Box` form, currently unused. |
| `TypeName.typarDefns` / `prefixTypars` | `Expr.fs:487-489`                               | Done — typar-defns slots on every TypeDefn. |
| `Binding.typarDefns`                   | (binding shape) wherever bindings live          | Done — let-level typar defns. |
| `TypeScheme` + `instantiate`           | `SemanticInfo.fs`, `Unification.fs`             | Done — the polymorphism plumbing is already in place. |
| `translateType` wildcard for VarType   | `Unification.fs:591`                            | Returns a fresh TyVar — needs scope-driven lookup. |
| `RecordTypeInfo` / `UnionTypeInfo`     | `SideTables.fs:24,38`                           | Name-only carrier — needs `TypeParams` field. |
| `TyRecord of name: string`             | `SemanticInfo.fs:99`                            | Name-only — needs `args: SemType list`. |
| `TyUnion of name: string`              | `SemanticInfo.fs:103`                           | Name-only — needs `args: SemType list`. |
| `ctorType` (DU instantiation)          | `Unification.fs` (post-DU plan)                 | Returns the raw declared field types — needs to instantiate the union's scheme. |
| Record literal / clone / field-access  | `Unification.fs` (post-records plan)            | Read declared field types verbatim — need to substitute the receiver's args. |
| Generalise / instantiate machinery     | `Unification.fs`                                | Done — same shape covers user-declared typars. |

The pieces missing are:

1. **Typar scope on `PassContext`** (or a threaded reader) — a per-signature
   `Dictionary<string, TypeVar>` so every `'a` in the same signature maps
   to the same root.
2. **`TyRecord` / `TyUnion` carry args** — `TyRecord of name: string *
   args: SemType list`. Every consumer (zonk, unify, instantiate,
   generalise, translateType, isAllocation, hasFreeTyVar,
   hasPendingFieldAccess) gains an arm walking `args`.
3. **`RecordTypeInfo.TypeParams` / `UnionTypeInfo.TypeParams`** — the
   quantified TyVars from the declaration; stored so literal /
   ctor-reference inference can substitute fresh copies at use sites.
4. **`translateType` rewrites** for `Type.VarType`, `Type.GenericType`,
   and `Type.NamedType` with no args (back-fill with fresh TyVars
   sealed by the registry's `TypeParams.Length`).
5. **Definition-side typar collection** — when registering
   `TypeDefn.Record` / `TypeDefn.Union` / a `Binding`, snapshot the
   typar scope so the declared field / case / RHS types share typars
   with one another.
6. **Use-side instantiation** for record literals, ctor references, and
   field accesses — same substitution scheme `instantiate` does for
   `TypeScheme`, lifted out so the named-type registry can reuse it.
7. **TAST update** — `TyRecord` / `TyUnion` propagate their args; no
   new TAST cases.
8. **Tests across every pass** covering the canonical examples.

## Why typars need a scope

The existing pipeline resolves *value-level* names through `ctx.Binding`
and falls back to `IExternalSymbolProvider`. Type-level names go
through a different channel: `translateType` consults `ctx.RecordTypes`
/ `ctx.UnionTypes` for named types, and falls through to `TyConst name`
for unknowns. Typars (`'a`) belong to a third channel: they're
*lexically scoped to a single signature*, never declared at the module
level, and shadow nothing outside their signature.

The simplest implementation is a per-signature scope passed alongside
`translateType`'s recursion. Two `'a`s in the same signature look up
the same TyVar; entering a new signature (a fresh `let` binding, a
fresh `type` declaration) starts a fresh empty scope.

This matches how F# itself handles typars: a typar is introduced either
by an explicit `<'a>` defn or by **implicit generalisation** of an
unbound `'a` mention. Same scope rule either way.

## The algorithm: per-signature typar scope + arg-carrying named types

### Typar scope

A signature begins one of:

- A `let f<'a, 'b> (x: 'a) (y: 'b) = …` binding (explicit defn).
- A `let f (x: 'a) (y: 'a) = …` binding (implicit — first `'a`
  introduces the typar; second `'a` resolves to it).
- A `type Foo<'a> = …` definition.
- An expression-level annotation `(e : 'a)` *inside* an already-open
  signature — uses the surrounding scope.

The scope is a `Dictionary<string, TypeVar>`, fresh per signature, populated
lazily as `translateType` encounters `Type.VarType`. A `'a` not yet in
the scope **defines** a new typar there; subsequent occurrences resolve
to it.

`Typar.Anon` (`_`) is **always** a fresh TyVar — wildcard typars are
distinct per occurrence, same as `Pat.Wildcard`.

`Typar.Static` (`^a`) follows the same scope rule as `'a` for v1 (SRTPs
themselves are out of scope; the static marker becomes meaningful when
the bound machinery lands).

### Named types carry args

A `TyRecord` / `TyUnion` is no longer name-only:

```fsharp
type SemType =
    | TyVar of TypeVar
    | TyConst of name: string
    | TyFun of arg: SemType * result: SemType
    | TyTuple of items: SemType list
    | TyRecord of name: string * args: SemType list
    | TyUnion of name: string * args: SemType list
```

Two `TyRecord(n1, args1)` / `TyRecord(n2, args2)` unify iff `n1 = n2`
*and* their arg lists unify pairwise (same arity rule as `TyTuple`).
Empty arg list means a non-generic type — `TyRecord("Point", [])`
behaves exactly like today's `TyRecord("Point")`. Same for `TyUnion`.

### Type-definition registration

When NameResolution stamps a record / union registry entry, it also
fills `TypeParams`. The defn's typar list (from `prefixTypars` or
`typarDefns`) opens a fresh scope; each `Typar.Named` becomes a fresh
TyVar at `Level = 0` whose root is stored both in the scope dictionary
and pushed onto `TypeParams`. Field / case-arg types then translate
under that scope, so `type Box<'a> = { Value: 'a }` has `Value`'s
placeholder TyVar share identity with `TypeParams.[0]` after
Unification's field fill-in runs.

`type Foo = { X: 'a }` with no `<'a>` defn (implicit free typar) v1
treats as an error — implicit typars on a *type definition* aren't
standard F#; on a *let binding* they are. The diagnostic message points
at the typar token: "Free type parameter `'a` is not declared in
`Foo`'s type-parameter list."

### Type-translation rewrites

`translateType` becomes scope-aware. Signature:

```fsharp
type TyparScope = Dictionary<string, TypeVar>

let rec private translateType (ctx: PassContext) (scope: TyparScope) (t: Type<SyntaxToken>) : SemType =
    match t with
    | Type.VarType(Typar.Named(ident = id)) ->
        let name = ctx.NameOf id
        match scope.TryGetValue name with
        | true, tv -> TyVar tv
        | false, _ ->
            // Implicit typar introduction. Mint a fresh TyVar at level
            // 0 (it'll be generalised at binding-group exit) and
            // memoise.
            let tv = TypeVar()
            tv.Level <- 0
            scope.[name] <- tv
            TyVar tv
    | Type.VarType(Typar.Anon _) ->
        // `_` typar — fresh every time, never stored.
        TyVar(TypeVar())
    | Type.NamedType li when li.Idents.Length = 1 ->
        let name = ctx.NameOf li.Idents.[0]
        match ctx.RecordTypes.TryGetValue name with
        | true, info when info.TypeParams.IsEmpty -> TyRecord(name, [])
        | true, info ->
            // Bare reference to a generic record — back-fill with
            // fresh TyVars at the current level (the type carrier is
            // generic at this site, will be pinned by surrounding
            // unification).
            let args = [ for _ in info.TypeParams -> TyVar(freshTyVar ctx) ]
            TyRecord(name, args)
        | false, _ ->
            // (similar for UnionTypes)
            // (similar primitive fall-through)
            ...
    | Type.GenericType(longIdent = li; typeArgs = args) when li.Idents.Length = 1 ->
        let name = ctx.NameOf li.Idents.[0]
        let translatedArgs = [ for a in args -> translateTypeArg ctx scope a ]
        match ctx.RecordTypes.TryGetValue name with
        | true, info when info.TypeParams.Length = translatedArgs.Length ->
            TyRecord(name, translatedArgs)
        | true, info ->
            diag $"Type '{name}' expects {info.TypeParams.Length} arg(s), got {translatedArgs.Length}"
            TyRecord(name, translatedArgs)        // best-effort to avoid cascades
        | false, _ ->
            // (similar for UnionTypes)
            // (fall through to TyConst with arity diagnostic)
            ...
    | ... // other cases unchanged
```

`translateTypeArg` translates `TypeArg.Type` via `translateType`;
`TypeArg.Measure` continues to live on the measure path.

Existing call sites:

- `translateType ctx t` becomes `translateType ctx scope t`.
- Records / DUs' field-fill-in passes open one scope per type defn.
- A `Binding`'s RHS / arg types open one scope per binding.
- Expression-level annotations (`(e : 'a)`) read the surrounding
  binding's scope — they don't open a new one.

### Record literal at a generic type

`{ X = 1 }` against `type Box<'a> = { Value: 'a }` finds `Box` via the
field-set lookup, then **instantiates**: fresh TyVars for the type's
`TypeParams`, substituted into each field's declared type. The
resulting `TyRecord("Box", [TyVar fresh_a])` unifies with the
expression's TyVar; the field initialiser's type unifies with the
field's instantiated type. If `1` flows in, `fresh_a` pins to `int`.

```fsharp
let instantiateRecordType (ctx: PassContext) (info: RecordTypeInfo) : SemType * (string -> SemType) =
    let subst = Dictionary<TypeVar, TypeVar>(HashIdentity.Reference)
    for tp in info.TypeParams do
        let fresh = TypeVar()
        fresh.Level <- ctx.CurrentLevel
        subst.[UnionFind.find tp] <- fresh
    let walk = substituteWith subst   // same helper instantiate uses
    let recTy = TyRecord(info.Name, [ for tp in info.TypeParams -> TyVar subst.[UnionFind.find tp] ])
    let fieldTypeOf name =
        info.Fields |> Array.find (fun f -> f.Name = name) |> fun f -> walk f.Type
    recTy, fieldTypeOf
```

(`substituteWith` factors out the substitution loop currently inlined
in `instantiate` — same body, parameterised on the substitution map.)

### Ctor reference / application at a generic union

Same shape as records. `ctorType` becomes parametric:

```fsharp
let ctorType (ctx: PassContext) (info: UnionCaseInfo) : SemType =
    let unionInfo = ctx.UnionTypes.[info.UnionName]
    let subst = freshSubst ctx unionInfo.TypeParams
    let unionTy = TyUnion(info.UnionName, [ for tp in unionInfo.TypeParams -> TyVar subst.[UnionFind.find tp] ])
    let walkedFields = info.Fields |> Array.map (substituteWith subst)
    match walkedFields.Length with
    | 0 -> unionTy
    | 1 -> TyFun(walkedFields.[0], unionTy)
    | _ -> TyFun(TyTuple(List.ofArray walkedFields), unionTy)
```

`Some 1` then types as `TyFun(int, TyUnion("Option", [TyVar fresh_a]))`
applied to `int`, pinning `fresh_a = int`, yielding
`TyUnion("Option", [int])`.

### Field access at a generic record

`r.X` where `r : Box<int>` reads `Box`'s field `X` (declared type
`'a`), substitutes `'a → int` (from the receiver's args), and returns
`int`. The substitution is keyed off the receiver's `TyRecord("Box",
args)`: `subst[TypeParams.[i]] = args.[i]`.

Free-TyVar receivers still defer via `PendingFieldAccess`; the drain
step learns to substitute typars from the eventual `TyRecord`'s
arg list.

### Generic let-binding RHS

`let pair (x : 'a) (y : 'a) = x, y` opens one typar scope at binding
entry. Both annotations resolve `'a` to the same TyVar; the inferred
type is `'a -> 'a -> 'a * 'a`. Generalisation closes over `'a`
(level > outer level) and writes a scheme — same as today, just
deterministic in which TyVar appears in the scheme. Use sites
instantiate exactly as they do for inferred polymorphism.

Explicit `let f<'a> (x: 'a) = x` works the same way: the explicit defn
seeds the scope with named typars before the RHS is typed.

## Data-model changes

### `SemanticInfo.fs`

```fsharp
type SemType =
    | TyVar of TypeVar
    | TyConst of name: string
    | TyFun of arg: SemType * result: SemType
    | TyTuple of items: SemType list
    /// Named record type with instantiated args. `Box<int>` is
    /// `TyRecord("Box", [TyConst "int"])`. The declaring type's
    /// arity is fixed by `ctx.RecordTypes[name].TypeParams.Length`;
    /// `args.Length` must match.
    | TyRecord of name: string * args: SemType list
    /// Named union type with instantiated args. Same shape as TyRecord.
    | TyUnion of name: string * args: SemType list
```

Every `SemType`-consumer gains an arm walking `args`:

- `zonk` — `TyRecord(n, args) -> TyRecord(n, List.map zonk args)`.
- `unify` — `TyRecord(n1, a1), TyRecord(n2, a2) when n1=n2 && a1.Length=a2.Length` → pairwise unify args.
- `occursAndAdjust` — recurses into args.
- `instantiate` walker — recurses into args.
- `generalise` walker — recurses into args.
- `translateType` — backs records / unions with arg lists.
- `hasPendingFieldAccess` — recurses into args.
- `Regions.isAllocation` — still `true` (args are immaterial).
- `Validation.hasFreeTyVar` — recurses into args (a `Box<'a>`
  whose `'a` is free is still free).

### `SideTables.fs`

```fsharp
[<Sealed>]
type RecordTypeInfo(name, typeParams: TypeVar list, fields, declKey) =
    member val Name = name
    /// Quantified type parameters in declaration order. Empty for
    /// non-generic types. Each TyVar is a *prototype* — substituted
    /// out by `instantiateRecordType` at every use site so two
    /// instantiations get independent variables.
    member val TypeParams = typeParams
    member val Fields = fields
    member val DeclKey = declKey

[<Sealed>]
type UnionTypeInfo(name, typeParams: TypeVar list, cases, declKey) =
    member val Name = name
    member val TypeParams = typeParams
    member val Cases = cases
    member val DeclKey = declKey
```

`UnionCaseInfo` is unchanged — case-arg types reference the union's
typars by sharing their TyVar roots. Substitution operates at the
union level; case-arg types ride along.

### `TypeVar`

No change. The typar-vs-fresh-vs-quantified distinction is encoded by
`Level` + `Link` already.

## Pass-by-pass changes

### `NameResolution`

`registerRecordTypeDefn` and `registerUnionTypeDefn` learn to read
typar lists. Algorithm:

1. Open a fresh `TyparScope` for this type defn.
2. Walk `TypeName.typarDefns` and `prefixTypars`. For each
   `Typar.Named`, mint a fresh `TypeVar` (level 0), insert into the
   scope, push onto `typeParams`.
3. Stash the scope on the registry entry — *not* directly, but indirectly:
   the field / case-data CST is held by the registry entry already, and
   the placeholder TyVars NameResolution stamps need to *be* the typars
   when the field/case type is just `'a`.

That third bullet is the tricky one: today NameResolution stamps a
**fresh** placeholder TyVar per field and Unification's fill-in
replaces its `Link`. To preserve typar identity (`field.Value : 'a`
should *be* `TypeParams.[0]`, not a fresh TyVar that gets linked to
`TypeParams.[0]` later), we have two options:

- **Option A** (preserve placeholder pattern): NameResolution still
  stamps fresh placeholders. Unification's fill-in walks the CST under
  the scope and sets the placeholder's `Link` to the translated type
  (which may itself be `TyVar TypeParams.[i]`). Equivalent to today's
  records flow, just with the typar scope passed in.
- **Option B** (direct identity): NameResolution stamps fields' TyVars
  *as* the typar TyVar when the field type is bare `'a`. Cheaper for
  the common case but special-cases the CST shape in NameRes.

Option A is the cleaner extension. The placeholder/link indirection
costs one extra `find` per field read, which the pipeline already pays
elsewhere.

A new helper exposes the scope to Unification:

```fsharp
// Stored on the registry entry alongside fields. Reconstructed on
// every fill-in walk — not persisted past `Unification.run`.
type private TypeDefnScope = Dictionary<string, TypeVar>
```

For let-bindings, NameResolution doesn't currently see typars (it walks
expressions and binding heads, not the binding's typar-defns). The
binding's `Binding.typarDefns` slot is read by Unification at
`inferBinding` time — same flow as records, just per-let.

### `Unification`

The major rewrite. Every `translateType` call site threads a scope:

```fsharp
let private freshTyparScope () = TyparScope(StringComparer.Ordinal)
```

**Binding-level scope.** `inferBinding` opens a scope for the binding,
seeds explicit `<'a, 'b>` typars from `Binding.typarDefns`, then types
the arg patterns and body under that scope. The scope is dropped at
`exitLevel` — generalisation hoists the scope's typars into the scheme,
so dropping the dictionary doesn't lose information.

**Pattern-level annotation reads.** `Pat.Typed(pat, _, t)` calls
`translateType` with the surrounding binding's scope.

**Expression-level annotation reads.** `Expr.TypeAnnotation(e, _, t)`
calls `translateType` with the surrounding binding's scope. Implicit
typars in expression annotations work the same as in pattern
annotations: first occurrence defines, subsequent uses lookup.

**Generic record fill-in.** `fillRecordFieldTypes` opens the type
defn's scope (rebuilt from the CST's `typarDefns`), translates each
field type under it, and links the field's placeholder TyVar to the
translated type.

**Generic union fill-in.** Same for `fillUnionFieldTypes`.

**Record literal inference.** `inferRecord` finds the unique
`RecordTypeInfo`, calls `instantiateRecordType` (above) for the fresh
substitution, and unifies each `inferExpr ei`'s type with the
instantiated field type. The result type is the instantiated
`TyRecord("R", args)`.

**Record clone inference.** Same as literal, but the substitution is
keyed off the *source record's* arg list, not a fresh one. `{ b with X
= 5 }` against `b : Box<int>` walks `Box`'s field declared type with
`'a → int`.

**Field-access inference.** When the receiver resolves to
`TyRecord("R", args)`, look up the field's declared type and substitute
`R.TypeParams[i] → args[i]`. The new helper:

```fsharp
let substituteRecordField (info: RecordTypeInfo) (args: SemType list) (field: RecordFieldInfo) : SemType =
    let subst = Dictionary<TypeVar, SemType>(HashIdentity.Reference)
    List.iter2 (fun tp arg -> subst.[UnionFind.find tp] <- arg) info.TypeParams args
    substituteWithSemType subst field.Type
```

(`substituteWithSemType` walks a SemType and replaces TyVar roots
present in the map; same shape as `instantiate`'s inner walker but
mapping to a *type*, not another TyVar — instantiation creates fresh
variables, substitution can plug in concrete types.)

**Ctor-reference inference.** Same shape as record literal: substitute
the union's typars with fresh TyVars at the current level, build the
function-shaped type from the substituted case args.

**Ctor-pattern inference.** Same shape. Sub-patterns unify against
substituted field types.

### `Regions`

No new arms. `isAllocation` already treats `TyRecord _` / `TyUnion _`
as allocations; the args don't influence allocation status. The argbut
walk in `zonk` / `unify` (called via `resolveLink`) ensures arg-level
TyVars participate in region propagation through their parents.

### `Validation`

`hasFreeTyVar` recurses into `args` so a `Box<'a>` with unresolved `'a`
still fires the value-restriction diagnostic on a mutable binding.

### `Freeze`

`TPat.Union` / `TExpr.UnionCons` / `TExpr.RecordCons` / etc. keep
their existing shape — `ty: SemType` already carries the instantiated
arg list inline because `TyRecord` / `TyUnion` grew the field. No new
TAST cases; the `ty`-querying paths in Freeze (`Unification.zonk`)
return arg-carrying named types automatically.

`prettyDecl` / `TastShape` learn to render generic types as
`Box<int>`, `Option<string>` rather than `Box`, `Option`. One arm
update per TAST case that renders a SemType inline.

## Pipeline integration

No new pass. Generics extend NameResolution (typar-defn capture in
type registries), Unification (typar-scoped translateType, named-type
instantiation, substitution), Validation (recursing into args), and
Freeze (rendering). Same skeleton records and DUs used.

The order of operations inside `Unification.run` is unchanged:

```fsharp
// Walk records / unions to fill in field / case-arg types — each
// type defn opens its own typar scope, populates TypeParams on its
// registry entry, and translates fields under that scope.
for m in elems do fillRecordFieldTypes ctx m
for m in elems do fillUnionFieldTypes ctx m
// Walk expression bodies. Each let-binding opens its own typar scope
// inside `inferBinding`.
for m in elems do walkModuleElem ctx m
```

## TAST representation

The frozen TAST carries arg-bearing `TyRecord` / `TyUnion` inline. A
target plugin reading the TAST sees `TyRecord("Box", [TyConst "int"])`
and looks up `RecordTypeInfo.TypeParams` if it needs to recover the
declaration's parameter list. Cross-file: when modules / namespaces
land, the registry layers over a provider-backed equivalent; the arg
list on `TyRecord` doesn't change shape.

Monomorphisation (a later pass) walks the TAST collecting every
instantiated arg list per named type and emits one specialised variant
per distinct arg list. The args on `TyRecord` / `TyUnion` are the keys
the monomorphiser groups by.

## Test strategy

Mirrors the records / DU test splits:

**`NameResolutionTests.fs`:**

1. **`type Box<'a>` registers TypeParams.** One-element `TypeParams`,
   `Value` field's placeholder TyVar links to the typar TyVar.
2. **`type Pair<'a, 'b>` keeps declaration order.** `TypeParams.[0]`
   is `'a`, `TypeParams.[1]` is `'b`.
3. **Implicit free typar in type-def diagnoses.** `type Bad = { X:
   'a }` (no `<'a>`) → "Free type parameter 'a" diagnostic.

**`UnificationTests.fs`:**

4. **Generic record literal pins typar.** `type Box<'a> = { Value:
   'a }\nlet b = { Value = 1 }` — `b : Box<int>` (i.e. `TyRecord("Box",
   [TyConst "int"])`).
5. **Generic record annotation forces typar.** `let b : Box<string> =
   { Value = "x" }` — typechecks; mismatched annotation `Box<int>`
   diagnoses.
6. **Generic ctor pins typar.** `type Option<'a> = | Some of 'a |
   None\nlet s = Some 1` — `s : Option<int>`.
7. **`None` requires annotation or use.** `let n = None` standalone
   stays generic (`Option<'a>` after generalisation, instantiated per
   use); `let n : Option<string> = None` pins it.
8. **Two annotations share `'a` identity.** `let pair (x: 'a) (y: 'a)
   : 'a * 'a = x, y\nlet _ = pair 1 "hello"` — mismatch.
9. **Two bindings have independent `'a`s.** `let id1 (x: 'a) = x\nlet
   id2 (y: 'a) = y\nlet _ = id1 1 + (if id2 true then 0 else 1)` —
   typechecks.
10. **Wrong-arity generic type diagnoses.** `let b : Box<int, string>
    = …` — arity-mismatch diagnostic.
11. **Field access on generic record substitutes typar.** `let f (b :
    Box<int>) = b.Value` — `f : Box<int> -> int`.
12. **Generic record clone preserves args.** `let b2 = { b with Value
    = "x" }` against `b : Box<int>` — diagnoses (clone preserves arg
    list).

**`RegionsTests.fs`:**

13. **Generic record at module top is LocalStack.** Same shape as the
    monomorphic case.
14. **Generic ctor at module top is LocalStack.** Same.

**`ValidationTests.fs`:**

15. **`let mutable b = { Value = 1 }` is clean.** Mutable + generic
    record + literal use — value-restriction passes because the
    binding's resolved type is `Box<int>`, not `Box<'a>`.
16. **`let mutable id = fun x -> x` still diagnoses.** Pre-existing
    value-restriction test continues to fire.

**`CoverageTests.fs`:** golden TAST shapes:

17. **Generic record literal renders args.** `let b = { Value = 1 }`
    → `let v0 = { Value = 1 }`, but the `b`'s TAST type is `Box<int>`.
18. **Generic ctor renders args.** `Some 1`'s TAST node's `ty` is
    `Option<int>`.

Each test follows the existing `analyse` → `declType` / `Expect.equal`
/ `Expect.isEmpty Diagnostics` pattern.

## Open questions

- **Generalisation of types that name unresolved typars.** A binding
  whose RHS produces `TyRecord("Box", [TyVar tv])` where `tv` is at
  level > outer must generalise `tv` — `tv` is reachable through the
  arg list, the existing `generalise` walker handles it once the
  arg-recursion arm is added.
- **Wildcard typars inside generic annotations.** `let f (b : Box<_>)
  = b.Value` — `_` mints a fresh TyVar, scope ignores it. Behaviour
  matches F#: the wildcard typar is independent and gets pinned by
  context or generalised.
- **Anonymous record types** (`{| X: 'a |}`). Different runtime shape,
  but the typar story is identical: the type *is* its field set, with
  typars participating. Defer with anonymous records themselves.
- **Cross-file generic types.** `Box<'a>` defined in one module, used
  in another. Lands with the namespace / cross-file resolution work;
  the registry overlay scheme already established for records / DUs
  extends naturally — the provider's `TryLookupType` would return a
  `RecordTypeInfo` whose `TypeParams` are prototype TyVars from the
  defining module.
- **External generic types.** `System.Collections.Generic.List<'a>`,
  `Microsoft.FSharp.Core.Option<'a>`. The provider needs the same
  `TryLookupType` channel + a way to construct `RecordTypeInfo` /
  `UnionTypeInfo`-shaped descriptors for BCL types. Lands with the
  .NET provider's named-type catalogue.
- **Higher-kinded typars.** `'f<'a>` where `'f` is itself a parameter.
  Hindley–Milner with kinds; out of scope.
- **Implicit typar generalisation on type definitions.** F# rejects
  `type Bad = { X: 'a }` (must be `type Bad<'a>`). v1 follows F#:
  implicit typars on type definitions are an error. Implicit typars
  in let bindings are fine — `let id x = x` and `let id (x: 'a) = x`
  both generalise the same way.
- **Type-parameter constraints** (`when 'a :> IComparable`, `when 'a
  : equality`). The parser captures these in `TyparConstraints`; v1
  reads them as a no-op (lands with constraints). Storage on
  `TypeVar.IfaceBounds` / `SrtpBounds` is already plumbed — the
  resolution callback is what's missing.

## Out of scope for this plan

- **Type-parameter constraints** (`when 'a : equality`, IWSAMs).
  Plumbed but unread; lands with the SRTP/IWSAM resolution work.
- **Statically-resolved typars** (`^a`). Same SRTP work.
- **Variance markers** (`'+a`, `'-a`). F# uses these only for
  delegates / interfaces and is fairly limited; defer until interfaces
  land.
- **Higher-kinded typars** (`'f<'a>`). HM with kinds; out of scope for
  any near-term plan.
- **Member-level typars** (`member this.Foo<'a> (x: 'a) = …`). Lands
  with class members.
- **Type abbreviations of generic types** (`type Pair<'a> = 'a * 'a`).
  Lands with abbreviations generally.
- **Generic active patterns.** Lands with active patterns themselves.
- **Higher-rank polymorphism** (`forall a. (a -> a) -> int`). HM-style
  generalisation handles let-polymorphism only.
- **Monomorphisation pass.** The TAST will carry enough information
  (arg lists on named types, schemes on bindings) for a monomorphiser
  to specialise; that pass is its own design exercise.
- **Cross-file generic types** and **external generic types**. Land
  with the namespace / .NET-provider work, sharing the registry-overlay
  story records / DUs already established.
