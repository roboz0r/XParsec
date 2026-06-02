# Inheritance plan

The build plan for **single inheritance between classes** in the
semantic-analysis pipeline, picking up where
[`classes-plan.md`](classes-plan.md) left off. Same overall posture: a
small extension to the existing class registry plus a handful of new
arms in Unification and Freeze. The interesting design surface is
*subtyping* — F# classes participate in a relation that's wider than
unification, and that relation has to live somewhere coherent so v2
(interfaces, IWSAMs) can hang off the same spine.

The status quo is silent on inheritance. `ClassInheritsDecl` is parsed
(`Expr.fs:604`) and rides on `ObjectModelBody.inherits`, but neither
NameResolution nor Unification looks at it: a class that says
`inherit Base(...)` registers exactly like a class that doesn't, and
`base.M(...)` / `:>` / `:?>` / `:?` all reach the conservative
`freshTyVar` fallbacks. Any program that touches inheritance explodes
downstream.

The canonical examples we want to handle after this lands:

```fsharp
// Base class with one ctor arg and one read-only property.
type Shape(name: string) =
    member this.Name = name
    member this.Describe() = name

// Single inheritance with an explicit base-ctor call.
type Circle(radius: float, n: string) =
    inherit Shape(n)
    // Member declared on the derived class.
    member this.Radius = radius
    // Override of an inherited member.
    override this.Describe() = sprintf "%s (r = %f)" this.Name radius

// Constructed in the usual way.
let c = Circle(1.0, "unit")
//    c : Circle

// Inherited members reachable through the derived instance.
let n = c.Name                     // n : string  — Shape.Name on a Circle
let d = c.Describe()               // d : string  — Circle.Describe (override)

// Upcast to the base type.
let s : Shape = c :> Shape         // s : Shape   — refers to the same value
let s2 = c :> Shape                // s2 : Shape  — annotation-driven inference

// Type test.
let isCircle = (s :? Circle)       // isCircle : bool

// Downcast (succeeds at runtime when s really is a Circle).
let back = s :?> Circle            // back : Circle

// Pass-through via annotation: any Shape works here, including a Circle.
let nameOf (sh : Shape) = sh.Name
let n2 = nameOf c                  // implicit upcast through annotation
//    n2 : string

// `base.M(...)` reaches the parent's implementation.
type Loud(name: string) =
    inherit Shape(name)
    override this.Describe() = "LOUD: " + base.Describe()
```

What this plan does **not** deliver (see [§Out of scope](#out-of-scope)):
interfaces (`InterfaceImpl` / `InterfaceSpec`), object expressions
(`Expr.Object`), abstract-without-default members (the slot side of
`abstract`), implicit subsumption at function-argument positions,
multi-segment base type names, IWSAMs, and equality/comparison
through the inheritance chain. v1 covers single class-to-class
inheritance, explicit `:>` / `:?` / `:?>`, `base.M(...)`, and the
`override`/`default` member keywords against an inherited member.

## Goal

After the pipeline finishes:

- **`ClassTypeInfo.BaseType`** carries the parent class's resolved
  `SemType` (`TyClass _`), translated under the derived class's typar
  scope so the base's typars are bound to whatever the `inherit
  Base<…>(args)` clause supplied. `ValueNone` when the class has no
  `inherit` clause.
- **`ClassTypeInfo.BaseCtorArgs`** is the CST expression handed to
  the base constructor (`expr` on `ClassInheritsDecl`). Typed once
  by Unification's `fillClassMembers` pre-pass — the same way the
  derived class's own ctor params get their fresh-instance check —
  and discarded after Freeze; the TAST carries only the base-class
  call already canonicalised into `TExpr.New`.
- **`ClassMemberInfo.IsOverride`** distinguishes `override`/`default`
  members from plain `member` declarations. Used by Validation when
  the override-against-an-abstract-slot check lands; in v1 the flag
  is recorded but no diagnostic fires.
- **`Unification.subsumes`** is a new relation distinct from `unify`:
  it succeeds when the source's runtime type is a subtype of the
  target's static type. Invoked at `:>` (must succeed), at the
  argument of `:?>` (must succeed in the *reverse* direction — the
  source must be an ancestor of the target), at `:?` (any non-final
  source / target pair), and at the TAST-side fold for
  base-constructor invocation. **Unification stays nominal**: two
  `TyClass`es unify iff their names match exactly, regardless of
  whether one inherits from the other.
- **`base` binding** inside a member body resolves to a TyVar
  pre-linked to the *parent's* `TyClass`. Member lookup on `base`
  walks the parent chain starting from the immediate parent — never
  re-dispatching through any override the current class might have
  installed.
- **Member lookup walks the chain.** `resolveFieldStep` against a
  `TyClass(D, args)` tries `D`'s members first; if no match,
  substitutes `D`'s `BaseType` through `mkNamedTypeSubst` and recurses
  against the parent. Overrides shadow inherited members of the same
  name.
- **TAST** grows `TExpr.Upcast`, `TExpr.TypeTest`, and
  `TExpr.Downcast`. `TExpr.MethodCall` and `TExpr.PropertyGet` gain
  a `via: ViaBase | ViaSelf` discriminator on the receiver
  expression's resolved type so codegen can emit a non-virtual call
  for `base.M(...)`. The base-ctor call (`inherit Base(args)`) is
  represented in the TAST as a `TExpr.New(baseClassName, args, …)`
  attached to the derived class's `ClassTypeInfo` (see
  [§Freeze](#freeze)).

## What we have to build on

| Piece                                  | Where                                           | Status |
|----------------------------------------|-------------------------------------------------|--------|
| `ClassInheritsDecl`                    | `XParsec.FSharp/Expr.fs:604`                    | Done — `inheritToken * typ * expr voption`. |
| `ObjectModelBody.inherits`             | `Expr.fs:620`                                   | Done — `ClassInheritsDecl<'T> voption`. |
| `MemberKeyword.Override` / `.Default`  | `Expr.fs:547`-`548`                             | Done — single-token keywords; parser tags every `member`. |
| `Expr.StaticUpcast`                    | `Expr.fs:302`                                   | Done — `expr * ':>' * typ`. |
| `Expr.DynamicTypeTest`                 | `Expr.fs:303`                                   | Done — `expr * ':?' * typ`. |
| `Expr.DynamicDowncast`                 | `Expr.fs:304`                                   | Done — `expr * ':?>' * typ`. |
| `ClassTypeInfo` registry               | `SideTables.fs`                                 | Done — extend with `BaseType` + `BaseCtorArgs`. |
| `ClassMemberInfo`                      | `SideTables.fs`                                 | Done — extend with `IsOverride`. |
| `freshNamedInstance`                   | `Unification.fs`                                | Done — re-used for parent-typar substitution. |
| `resolveFieldStep`                     | `Unification.fs`                                | Done — extend the `TyClass` arm to recurse into `BaseType` on miss. |
| `tryClassMember`                       | `Freeze.fs`                                     | Done — same recursion shape lands here. |
| `TExpr.MethodCall` / `PropertyGet`     | `Tast.fs`                                       | Done — receiver TyVar already carries the actual runtime class; only the `via` flag is new. |
| `TExpr.New`                            | `Tast.fs`                                       | Done — re-used verbatim for the base-ctor invocation. |
| `IExternalSymbolProvider`              | `ExternalSymbols.fs`                            | Done — BCL base classes (`System.Object`, eventually `System.Exception`) need to land here as `TyClass` so a same-file class can `inherit System.Object`. v1 deliberately routes the lookup *through* the provider but defers populating it (see [§Open questions](#open-questions)). |

The pieces missing are:

1. **`BaseType` + `BaseCtorArgs` fields on `ClassTypeInfo`.** The
   parent type's resolved `SemType` plus the (un-translated) CST
   expression for the base ctor args.
2. **`IsOverride` flag on `ClassMemberInfo`.** Stamped at registration
   time from `MemberKeyword.Override` / `MemberKeyword.Default`.
3. **NameResolution: `registerInheritedSlots`.** A post-pass after
   `registerClassTypes` that resolves each class's `inherit` clause
   to a known class name, validates that the parent exists in
   `ctx.ClassTypes` (or in the provider), and stamps `BaseType`. Runs
   after every class is registered so a derived class can name a
   parent declared later in the file.
4. **Unification: `fillBaseCtorCall`.** Type the `inherit Base(args)`
   call against the parent's primary-constructor signature. Wired
   into `fillClassMembers` so it runs before any member body is
   walked — `base` needs `BaseType` available.
5. **Unification: `subsumes`.** Three-way relation:
   `Subtype | Equal | Unrelated`. Compositional over `TyClass` arg
   lists (covariant in v1 — see [§Open questions](#open-questions)),
   recursive over the parent chain.
6. **Unification: `:>` / `:?` / `:?>` arms in `infer`.** Each calls
   `subsumes` with the right direction and emits a targeted diagnostic
   when the relation fails.
7. **Unification: `base` binding inside member bodies.** Mirrors
   `this`: synthetic NodeKey on the class, TyVar pre-linked to
   `BaseType`. Visible only when the class has an `inherit` clause.
8. **Unification: member-lookup chain walk.** `resolveFieldStep`'s
   `TyClass` arm recurses into `BaseType` on miss. Override shadowing
   is implicit — derived's `Members` table takes priority.
9. **Freeze: `TExpr.Upcast` / `TypeTest` / `Downcast`.** Plus the
   `via` flag on `MethodCall` / `PropertyGet` so codegen can emit
   the non-virtual base call.
10. **Validation: override-against-known-member check.** A v1 check
    that an `override` member's name matches *some* member on the
    parent chain — full slot-matching against `abstract` lands when
    abstract members do.

## Why inheritance needs subtyping (and not just member lookup)

Records, DUs, abbreviations, and classes-without-`inherit` are all
*nominal*: two types unify iff their names match. F# inheritance
breaks that — `let s : Shape = c` where `c : Circle` is legal even
though `Shape` and `Circle` are different nominal types. The unifier
*cannot* be relaxed to allow this directly: doing so would let
`unify` succeed on assignments where the user expected a strict
match, hiding bugs.

The standard fix (and what every nominal-with-inheritance system uses)
is to keep `unify` strict and add a *separate* coercion relation
invoked only at syntactic positions where the language guarantees a
subtype is acceptable. In F# those positions are:

- **`expr :> T`** — explicit upcast. Source must be a subtype of `T`.
- **`expr :? T`** — type test. Source's static type must be related
  to `T` (either direction), else the test is statically void.
- **`expr :?> T`** — explicit downcast. `T` must be a subtype of the
  source's static type. Runtime check at the use site.
- **Function-argument flexibility (`'T :> Base`)** — *out of scope
  for v1*. F#'s `#Base` flex-type annotation and the implicit
  upcast it triggers at App sites is a substantial design surface;
  v1 requires the user to write `:>` explicitly when needed.
- **Annotation on a `let` or return.** A `let r : Base = derived`
  also wants subsumption. v1 handles this by detecting the shape in
  `inferTypeAnnotation` and routing through `subsumes` when both
  sides are `TyClass`. Other shapes (function returns, tuple
  positions) stay nominal — same rationale as deferring flex types.

Mechanically: `subsumes ctx key src tgt` returns `Subtype` /
`Equal` / `Unrelated`. The caller decides what to do with each:
`Equal` is identical to a successful `unify`, `Subtype` succeeds
without changing TyVar links (no nominal change), and `Unrelated`
emits a diagnostic. The relation is *not* an on-unified callback —
it has no deferred state because it only fires at positions where the
target type is already known.

## The algorithm

### Registration (NameResolution)

Inheritance picks up where the classes-plan left off. After the
existing `registerClassTypes` pre-pass runs, every class in the file
is in `ctx.ClassTypes` but without parent info. A second pass
(`registerInheritedSlots`) walks each class again:

1. If `body.inherits = ValueSome (ClassInheritsDecl(typ = t; expr = exprOpt))`:
   a. Translate `t` under the class's typar scope via
      `translateType`. The result must be a `TyClass _` (records /
      unions / abbreviations are not inheritable in F# — those produce
      a "cannot inherit from this type" diagnostic).
   b. Verify the parent class exists in `ctx.ClassTypes` (same-file)
      or via the provider. v1 only routes single-segment names; the
      multi-segment / fully-qualified cases land with module/namespace
      resolution.
   c. Stamp `info.BaseType <- ValueSome parentTy` and
      `info.BaseCtorArgs <- exprOpt` (the un-translated expression).
   d. Diagnose a cycle: if walking `BaseType` chains back to the
      current class, emit a "cyclic inheritance" diagnostic and clear
      `BaseType` so subsequent passes treat the class as parent-less.

The walk is shallow — it only resolves direct parents. The transitive
chain is consulted at lookup time via repeated `BaseType` follows.

`MemberKeyword.Override` and `MemberKeyword.Default` set
`ClassMemberInfo.IsOverride <- true`. `MemberKeyword.Abstract` is
**not in v1**: a member declared with no body that the registry
records as a "slot" is a different shape (`MethodOrPropDefn.AbstractSignature`)
than a member with a body, and slot-matching has its own validation
story. v1 surfaces an "abstract members not yet supported" diagnostic
when `MethodOrPropDefn.AbstractSignature` is encountered.

### Member-chain lookup

`resolveFieldStep ctx key rTy memberName` currently has this `TyClass`
arm:

```fsharp
| TyClass(clsName, args) ->
    match ctx.ClassTypes.TryGetValue clsName with
    | true, info ->
        match info.Members |> Array.tryFind (fun m -> m.Name = memberName && not m.IsStatic) with
        | Some m ->
            let subst = mkNamedTypeSubst info.TypeParams args
            substituteWith subst m.Type
        | None -> /* diagnostic */
    | _ -> /* diagnostic */
```

The v1 extension keeps the structure but recurses into `BaseType` on
miss:

```fsharp
let rec walk (rTy: SemType) : SemType voption =
    match resolveStep rTy with
    | TyClass(clsName, args) ->
        match ctx.ClassTypes.TryGetValue clsName with
        | true, info ->
            // Derived takes priority over inherited (override shadowing).
            match info.Members |> Array.tryFind (fun m -> m.Name = memberName && not m.IsStatic) with
            | Some m ->
                let subst = mkNamedTypeSubst info.TypeParams args
                ValueSome (substituteWith subst m.Type)
            | None ->
                match info.BaseType with
                | ValueSome parentTy ->
                    let subst = mkNamedTypeSubst info.TypeParams args
                    walk (substituteWith subst parentTy)
                | ValueNone -> ValueNone
        | false, _ -> ValueNone
    | _ -> ValueNone
```

The `substituteWith subst parentTy` step is what makes generic
inheritance work: if `Circle<'a>` inherits from `Container<'a>` and
we're resolving against `TyClass("Circle", [int])`, the parent type
gets walked with `'a ↦ int`, so `Container`'s typar binds to the
derived class's instantiation.

The same recursion shape lands in `Freeze.tryClassMember` (instance
lookup) and in the `drainPendingDotAccess` arm so deferred receivers
see the full chain too.

### `base` binding

`base` is a binding like any other. NameResolution writes a scope
entry for `base` inside any class member body whose enclosing class
has an `inherit` clause. Same shape as the `this` binding (synthetic
NodeKey, binding-site self-entry) — except the entry is keyed off a
*different* synthetic NodeKey
(`NodeKey.ofSynthetic info.DeclKey.Offset NodeKind.SynthBaseBinding`,
a new `NodeKind`).

Unification's `fillClassMembers` mints `baseTv` at the same level as
`thisTv`, pre-linked to the derived class's `BaseType` (with typars
substituted from the derived class's prototype TypeParams):

```fsharp
match info.BaseType with
| ValueSome parentTy ->
    let baseTv = TypeVar()
    baseTv.Level <- ctx.CurrentLevel
    let parentSubst = ... // from info.TypeParams onto parentTy's args
    baseTv.Link <- ValueSome (substituteWith parentSubst parentTy)
    ctx.TypeVar.Set(info.BaseKey, baseTv)
| ValueNone -> ()
```

Looking up `base.M` then goes through the same multi-segment
`LongIdent` chain that `this.M` does — head segment resolved as a
local binding, member-chain lookup against the head's resolved type.

**Key difference from `this.M`:** `base.M` skips the derived class's
overrides. The TAST flags this with a `via = ViaBase` discriminator
on the MethodCall / PropertyGet so codegen emits a non-virtual call.
v1 implements this by branching on the head segment's binding-site
NodeKey at TAST-translation time: if the receiver's binding site is
the class's `BaseKey`, emit `ViaBase`; otherwise `ViaSelf`.

### Constructor invocation walks the chain

When the user writes `inherit Base(args)`, Unification's
`fillClassMembers` types the call against `Base`'s primary
constructor — same path `Expr.New` uses, just with the receiver type
already fixed to the parent and the args explicitly supplied. The
diagnostic key is the `inherit` token. Errors (wrong arity, type
mismatch) attach there and don't cascade into member-body inference.

The base-ctor call lands in the TAST as a `TExpr.New(parentName,
typedArgs, parentTy)` value associated with the derived class — not
emitted directly into the member bodies. Codegen consults the
`ClassTypeInfo` after Freeze to retrieve the base-ctor expression
for the synthesized constructor.

### `:>` upcast

`Expr.StaticUpcast(expr; colonGreaterThan; typ)`:

1. Type `expr` to get `srcTy`.
2. Translate `typ` under the current typar scope to get `tgtTy`.
3. `match subsumes ctx (CstKeys.ofExpr e) srcTy tgtTy with`:
   - `Equal` → succeed; the upcast is a no-op at runtime but the
     expression's type is `tgtTy`.
   - `Subtype` → succeed; result type is `tgtTy`.
   - `Unrelated` → diagnose "Cannot upcast type 'X' to 'Y' — no
     inheritance relationship".
4. Return `tgtTy`.

### `:?` type test

`Expr.DynamicTypeTest(expr; colonQuestionMark; typ)`:

1. Type `expr` to get `srcTy`.
2. Translate `typ` to get `tgtTy`.
3. Require srcTy and tgtTy to be *related*: either `subsumes src tgt`
   or `subsumes tgt src` is non-`Unrelated`. F# allows the test even
   when the source's static type is unrelated to the target only if
   one of them is a sealed final type — v1 simplifies to "must be
   related"; the corner cases land later.
4. Return `TyConst "bool"`.

### `:?>` downcast

`Expr.DynamicDowncast(expr; colonQuestionMarkGreaterThan; typ)`:

1. Type `expr` to get `srcTy`.
2. Translate `typ` to get `tgtTy`.
3. Require `subsumes tgtTy srcTy` to be `Subtype` (the target is a
   *more specific* type than the source). `Equal` is also accepted
   but emits a Warning ("downcast is redundant — the static type
   already matches"). `Unrelated` emits an Error.
4. Return `tgtTy`.

### The `subsumes` relation

```fsharp
type private SubsumeOutcome =
    | Equal
    | Subtype
    | Unrelated

let rec private subsumes
    (ctx: PassContext)
    (src: SemType)
    (tgt: SemType)
    : SubsumeOutcome =
    match resolveStep src, resolveStep tgt with
    // Same nominal class with same args → Equal.
    | TyClass(s, sa), TyClass(t, ta) when s = t && List.length sa = List.length ta ->
        // Same name; args must coincide pairwise. v1 treats args as
        // *invariant* (Equal) — wider variance lands with the IWSAM /
        // variance plan.
        if List.forall2 (fun a b -> subsumes ctx a b = Equal) sa ta then Equal
        else Unrelated
    // Same name, mismatched arity → Unrelated (parser shouldn't emit, but stay total).
    | TyClass _, TyClass _ when (match resolveStep src, resolveStep tgt with TyClass(s, _), TyClass(t, _) -> s = t | _ -> false) ->
        Unrelated
    // Different class names → walk src's parent chain.
    | TyClass(s, sa), TyClass(t, ta) ->
        match ctx.ClassTypes.TryGetValue s with
        | true, info ->
            match info.BaseType with
            | ValueSome parentTy ->
                let subst = mkNamedTypeSubst info.TypeParams sa
                let parentInstance = substituteWith subst parentTy
                match subsumes ctx parentInstance tgt with
                | Equal -> Subtype
                | Subtype -> Subtype
                | Unrelated -> Unrelated
            | ValueNone -> Unrelated
        | false, _ -> Unrelated
    // TyClass vs anything-else → only Equal if structurally equal
    // (TyConst names match, both are the same TyVar, etc.).
    | a, b when a = b -> Equal
    | _ -> Unrelated
```

A few invariants the implementation must preserve:

- **`subsumes` never mutates `Link` or `Constraints`.** It's a pure
  query. Failure paths emit diagnostics from the caller, not from
  inside the relation.
- **Args are invariant in v1.** `List<Circle>` does not subsume
  `List<Shape>`, even though `Circle <: Shape`. Covariant / contravariant
  arms land with the variance plan.
- **Reflexivity is `Equal`, not `Subtype`.** Callers that distinguish
  "redundant upcast" from "actual upcast" rely on this.
- **`Unrelated` doesn't recurse into TyFun / TyTuple.** Function
  types only relate by full structural equality in v1; tuples likewise.

**Why `subsumes` being read-only is load-bearing, not just tidy.** Keeping
the relation a pure query is what lets the *destructive* unifier
(`UnionFind.union` + `TyVar.Link` mutation, with **no undo trace**) carry
inheritance at all. `subsumes` only fires at syntactic positions where the
*target* type is already known (`:>` / `:?` / `:?>` targets, a `let`
annotation), so it never has to speculatively bind an unsolved typar and
then discover the guess was wrong — every v1 coercion site is
decide-when-ground, and nothing needs rollback. The moment a coercion must
be resolved *before* its types are ground, this shortcut stops being enough;
see [§Speculative unification](#open-questions) below.

### TAST shape

Three new `TExpr` cases:

```fsharp
| Upcast of source: TExpr * ty: SemType
| Downcast of source: TExpr * ty: SemType
| TypeTest of source: TExpr * ty: SemType    // ty is always TyConst "bool"
```

Plus a discriminator on the existing `MethodCall` / `PropertyGet`:

```fsharp
[<RequireQualifiedAccess>]
type CallVia =
    | Self        // Normal virtual dispatch
    | Base        // `base.M(...)` — non-virtual; targets the parent slot

| MethodCall of receiver: TExpr * methodName: string * via: CallVia * args: TExpr list * ty: SemType
| PropertyGet of receiver: TExpr * propertyName: string * via: CallVia * ty: SemType
```

Adding a field rather than a new TExpr case keeps consumers that
don't care about virtual dispatch (most analyses) unchanged — a wild
match against `MethodCall (_, _, _, _, _)` works as before once the
new field lands.

## Data-model changes

### `SideTables.fs`

```fsharp
[<Sealed>]
type ClassMemberInfo
    (
        name: string,
        kind: ClassMemberKind,
        isStatic: bool,
        isOverride: bool,
        ty: SemType,
        declKey: NodeKey
    ) =
    new(name, kind, isStatic, ty, declKey) =
        ClassMemberInfo(name, kind, isStatic, false, ty, declKey)
    new(name, kind, ty, declKey) =
        ClassMemberInfo(name, kind, false, false, ty, declKey)
    member val Name = name
    member val Kind = kind
    member val IsStatic = isStatic
    /// `true` for `override`/`default`-keyword members. Used by
    /// Validation's override-target check and by Freeze's `via`
    /// discriminator (members the derived class explicitly overrode
    /// dispatch through `ViaSelf`; inherited members through the
    /// parent chain dispatch via the parent's declaration).
    member val IsOverride = isOverride
    member val Type = ty
    member val DeclKey = declKey

[<Sealed>]
type ClassTypeInfo
    (
        name: string,
        typeParams: (string * TypeVar) list,
        ctorParams: ClassCtorParamInfo[],
        members: ClassMemberInfo[],
        declKey: NodeKey,
        thisName: string,
        thisKey: NodeKey,
        baseKey: NodeKey
    ) =
    member val Name = name
    member val TypeParams = typeParams
    member val CtorParams = ctorParams
    member val Members = members
    member val DeclKey = declKey
    member val ThisName = thisName
    member val ThisKey = thisKey
    /// Synthetic NodeKey for the `base` binder. Shared across every
    /// member body in this class. Only meaningful when `BaseType` is
    /// `ValueSome _`; otherwise `base` is not in scope.
    member val BaseKey = baseKey
    /// Parent class's instantiated `TyClass`, translated under this
    /// class's typar scope so the parent's typars are bound to
    /// whatever `inherit Base<…>(args)` supplied. `ValueNone` when
    /// the class has no `inherit` clause.
    member val BaseType: SemType voption = ValueNone with get, set
    /// `expr` from `ClassInheritsDecl`: the un-translated CST
    /// expression handed to the parent's primary constructor. Typed
    /// by Unification once during `fillClassMembers`; the resulting
    /// `TExpr.New` is attached to the derived class via Freeze.
    member val BaseCtorArgs: Expr<SyntaxToken> voption = ValueNone with get, set
```

`BaseType` / `BaseCtorArgs` start as `ValueNone` and get filled in by
NameResolution's `registerInheritedSlots`.

### `NodeKey.fs`

```fsharp
type NodeKind =
    // … existing entries …
    | SynthBaseBinding = 1004us
```

### `Tast.fs`

```fsharp
[<RequireQualifiedAccess>]
type CallVia =
    | Self
    | Base

[<RequireQualifiedAccess>]
type TExpr =
    // … existing …
    /// `e :> T`. `ty` is the target type. `source`'s type is a
    /// subtype of `ty` (validated in Unification).
    | Upcast of source: TExpr * ty: SemType
    /// `e :?> T`. `ty` is the target type; runtime check.
    | Downcast of source: TExpr * ty: SemType
    /// `e :? T`. `ty` is always `TyConst "bool"`.
    | TypeTest of source: TExpr * ty: SemType
```

`MethodCall` and `PropertyGet` grow a `CallVia` field — see
[§Subtle migrations](#subtle-migrations) for the diff posture.

### Subtle migrations

The `via` field on `TExpr.MethodCall` / `TExpr.PropertyGet` is a
*breaking* change to consumers that pattern-match on the existing
arity. Two coping strategies:

- **Add a new TExpr arm per `via` value.** Doubles the case count
  forever; rejected.
- **Add the field with a default `ViaSelf` for in-tree consumers.**
  F# discriminated-union cases don't have parameter defaults at the
  pattern-match site, so every existing match grows an underscore
  or named-`via=ViaSelf` arm. v1 adopts this; the diff is mechanical
  and the new field is essential.

A migration check: after applying the diff, search for
`TExpr.MethodCall` and `TExpr.PropertyGet` everywhere and confirm
each match site either uses the new field or `_`. The `TastShape`
renderer is the only known consumer that needs the value:
`base.M(...)` should render distinctly from `this.M(...)` so test
strings stay readable (`b.M(...)` for ViaBase, `r.M(...)` for
ViaSelf, where `b` and `r` are the receiver's source name).

## Pass-by-pass changes

### `NameResolution`

Three touch-points:

**1. `registerInheritedSlots` post-pass.** Runs after
`registerClassTypes` in `walkElems`. For each class with an
`inherit` clause:

```fsharp
let private registerInheritedSlots (ctx: PassContext) (m: ModuleElem<SyntaxToken>) : unit =
    match m with
    | ModuleElem.Type defs ->
        for td in defs do
            let info, body =
                match td with
                | TypeDefn.Class(typeName = TypeName(ident = nameLi); body = body)
                | TypeDefn.Anon(typeName = TypeName(ident = nameLi); body = body) when nameLi.Idents.Length = 1 ->
                    match ctx.ClassTypes.TryGetValue (ctx.NameOf nameLi.Idents.[0]) with
                    | true, info -> ValueSome (info, body)
                    | false, _ -> ValueNone
                | _ -> ValueNone
                |> ValueOption.defaultValue (Unchecked.defaultof<_>, Unchecked.defaultof<_>)
            // … walk body.inherits, translateType into BaseType, store BaseCtorArgs.
    | _ -> ()
```

Cycle detection is shallow at this stage — `registerInheritedSlots`
walks each class exactly once. Cycles surface during the first
`subsumes` or `walkChain` call, which detect a re-entry and emit a
"cyclic inheritance" diagnostic on the class's `DeclKey`. Setting
`BaseType <- ValueNone` after the diagnostic short-circuits further
recursion.

**2. `walkClassBodies` extension.** When the class has `BaseType`,
add `base` to the *instance* scope (statics still see neither). The
binding site for `base` is `info.BaseKey`:

```fsharp
match info.BaseType with
| ValueSome _ ->
    instanceScopeMap <- Map.add "base" (info.BaseKey, false) instanceScopeMap
    ctx.Binding.Set(
        info.BaseKey,
        { BindingSite = info.BaseKey; IsInline = false; IsMutable = false }
    )
| ValueNone -> ()
```

**3. Member-keyword extraction.** `MemberKeyword.Override` /
`MemberKeyword.Default` set `isOverride = true`; `MemberKeyword.Member`
stays `false`. `MemberKeyword.Abstract` reaches an "abstract members
not yet supported" diagnostic and skips registration. The
`MemberDefn.Member` arm of `registerClassTypeDefn` now reads
`keyword` (currently ignored) to drive the flag.

### `Unification`

**1. `fillBaseCtorCall` inside `fillClassMembers`.** Before any member
body is walked, type the base-ctor invocation under the class's typar
scope. The `expr` from `ClassInheritsDecl` is wrapped synthetically:

```fsharp
match info.BaseType, info.BaseCtorArgs with
| ValueSome (TyClass(baseName, baseArgs)), ValueSome argExpr ->
    match ctx.ClassTypes.TryGetValue baseName with
    | true, baseInfo ->
        let subst = mkNamedTypeSubst baseInfo.TypeParams baseArgs
        let paramTys =
            baseInfo.CtorParams
            |> Array.map (fun p -> substituteWith subst p.Type)
            |> Array.toList
        let expected =
            match paramTys with
            | [] -> MockBuiltins.tyUnit
            | [t] -> t
            | many -> TyTuple many
        let argTy = infer ctx argExpr
        unify ctx (CstKeys.ofExpr argExpr) argTy expected
    | false, _ -> ()    // parent unknown; registerInheritedSlots already diagnosed
| _ -> ()
```

**2. `base` TyVar mint.** Inside `fillClassMembers`, right after the
`this` TyVar is set up:

```fsharp
match info.BaseType with
| ValueSome parentTy ->
    let baseTv = TypeVar()
    baseTv.Level <- ctx.CurrentLevel
    // parentTy is already substituted from registerInheritedSlots, so
    // we can use it directly.
    baseTv.Link <- ValueSome parentTy
    ctx.TypeVar.Set(info.BaseKey, baseTv)
| ValueNone -> ()
```

**3. `resolveFieldStep` chain walk.** As outlined in
[§Member-chain lookup](#member-chain-lookup). The walk also stamps a
re-entry guard (`HashSet<string>` of class names already visited)
to short-circuit cycles.

**4. `inferIdent` static / qualified-static lookup.** Statics declared
in a parent class are accessible through the derived class's name in
F# (`Derived.SomeStatic`). v1 implements this by walking the chain
inside the static-member lookup arm — same recursion as instance
members.

**5. New `Expr.StaticUpcast` / `Expr.DynamicTypeTest` /
`Expr.DynamicDowncast` arms in `infer`.** Each delegates to a small
helper that types the inner expression, translates the target type,
calls `subsumes`, and emits the appropriate diagnostic on failure.

**6. `subsumes` itself.** New private function alongside `unify`.
Pure read of `ctx.ClassTypes`; no mutation.

### `Regions`

`Expr.StaticUpcast`, `Expr.DynamicTypeTest`, `Expr.DynamicDowncast` are
all *non-allocating* — they're just a static-type adjustment over the
same runtime value. Region of the result expression is the region of
the source. Three small arms in `inferRegionImpl` cover this.

`inherit Base(args)` allocates as part of the derived class's
constructor; v1 attributes the allocation to the derived class's
region (already the case via `Expr.New`) and adds an edge to the
base-ctor args' region. No new region rule needed beyond the existing
`newRegion`.

### `Validation`

Two new diagnostics:

- **Override of a missing member.** If `m.IsOverride && member name
  is not found anywhere in the parent chain`, emit "no member to
  override". Surfaces typos like `override this.NaMe = …`.
- **Cyclic inheritance.** If `walkChain` re-enters the starting
  class, emit "Type 'X' inherits from itself (cycle)". Falls out of
  the lookup-loop guard already used in `resolveFieldStep`.

Both diagnostics live in `Validation.fs` and run as standalone
walks over `ctx.ClassTypes` after every other pass has finished,
so the inheritance graph is fully populated.

### `Freeze`

Three new `TExpr` arms, plus the `via` discriminator on
`MethodCall` / `PropertyGet`:

- **`Expr.StaticUpcast`** → `TExpr.Upcast(translateExpr ctx inner, translateType ctx t)`.
- **`Expr.DynamicTypeTest`** → `TExpr.TypeTest(translateExpr ctx inner, tyBool)`.
- **`Expr.DynamicDowncast`** → `TExpr.Downcast(translateExpr ctx inner, translateType ctx t)`.

The `via` discriminator is set in the existing folding arms:

```fsharp
| Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
    li.Idents.Length = 2 && ctx.NameOf li.Idents.[0] = "base" ->
    // … same chain as `this.X` / `r.X` but with via = ViaBase
```

Actually the right check is on the resolved binding site:

```fsharp
let viaOfHead (headTok: SyntaxToken) =
    match ctx.Binding.TryGetValue (NodeKey.ofToken headTok NodeKind.ExprIdent) with
    | ValueSome rb when ctx.ClassTypes |> Seq.exists (fun kv -> kv.Value.BaseKey = rb.BindingSite) ->
        CallVia.Base
    | _ -> CallVia.Self
```

That's O(classes) per access; production code will want a reverse
index. For v1, the index is cheap enough — most files have a handful
of classes.

The base-ctor invocation translates inside `Freeze.run` after each
class's members are translated: produce a `TExpr.New(baseName,
typedArgs, baseTy)` and attach it to the class's `ClassTypeInfo` (a
new mutable field `BaseCtorCallTast: TExpr voption`). The TAST itself
doesn't grow a "class declaration" node in v1 — codegen consumes the
side table directly. (Adding a class-declaration TAST shape is a
follow-up alongside the existing TODO for type declarations in the
TAST.)

## Pipeline integration

No new pass. Inheritance extends NameResolution (one new post-pass,
plus member-keyword reading), Unification (chain walk, `subsumes`,
new expression arms, `base` binding), Regions (three arms), Validation
(two checks), and Freeze (three TExpr cases + `via` field).

Pass order:

1. `Desugar.run` — unchanged.
2. `NameResolution.run`:
   1. `registerRecordTypes`
   2. `registerUnionTypes`
   3. `registerAbbreviationTypes`
   4. `registerClassTypes`            (existing)
   5. `registerInheritedSlots`        (new)
   6. `walkClassBodies`               (now also adds `base` to instance scope)
   7. `walkModuleElems`               (existing)
3. `Unification.run`:
   1. `fillAbbreviationBodies`
   2. `fillRecordFieldTypes`
   3. `fillUnionFieldTypes`
   4. `fillClassMembers`              (now: type base-ctor calls + mint `base` TyVar before walking members)
   5. `walkModuleElems`
4. `Regions.run` — three new arms.
5. `Validation.run` — two new checks (override-target + cycle).
6. `Freeze.run` — three new arms; `via` discriminator on existing.

`registerInheritedSlots` is sequenced after `registerClassTypes` so
forward / out-of-order references work (`type Derived = inherit Base`
where `Base` is declared later in the file).

## Test strategy

Across the four existing test modules. Same posture as the
records / DU / abbreviations / classes splits.

**`NameResolutionTests.fs`:**

1. **Inheritance recorded on registry.** `type D(x: int) =
   inherit B(x)` — `ctx.ClassTypes["D"].BaseType` is
   `ValueSome (TyClass("B", []))`.
2. **`base` in scope inside derived member body.** A member that
   references `base` gets a Binding entry whose `BindingSite` is the
   class's `BaseKey`.
3. **`base` not in scope when no `inherit` clause.** A class without
   `inherit` that mentions `base` in a member body diagnoses
   "Unresolved identifier: base".
4. **Cyclic inheritance diagnoses.** `type A() = inherit B()` and
   `type B() = inherit A()` — at least one of the two emits "cyclic
   inheritance".
5. **`override` flag set on registry.** A `override this.M (…)` arm
   gets `ClassMemberInfo.IsOverride = true`.
6. **Inheriting from a non-class diagnoses.** `type D() = inherit R`
   where `R` is a record / union / abbreviation → "Cannot inherit
   from this type".

**`UnificationTests.fs`:**

7. **Inherited member access.** `type B() = member this.X = 1`,
   `type D() = inherit B()`, `let d = new D() in d.X` — `d.X : int`.
8. **Override shadows inherited member.** `type B() = member this.M
   () = 1`, `type D() = inherit B() override this.M () = 2`, `(new
   D()).M()` — types as `int`; override is selected.
9. **`base.M(…)` types through parent.** `override this.M () =
   base.M()` types correctly with `base.M : unit -> int`.
10. **Generic inheritance.** `type Box<'a>(v:'a) = member this.V = v`,
    `type IntBox(v: int) = inherit Box<int>(v)`, `(new IntBox(1)).V`
    — types as `int` (the `'a` substitution from inheriting class to
    parent's typar).
11. **`:>` upcast succeeds for declared subtype.**
    `type B() = …`, `type D() = inherit B()`, `let s = new D() :>
    B` — `s : B`.
12. **`:>` upcast fails for unrelated types.**
    `type A() = …`, `type B() = …`, `(new A()) :> B` —
    diagnostic.
13. **`:?>` downcast types as target.** `(s : B) :?> D` — typed as
    `D` (with appropriate `subsumes` check).
14. **`:?>` downcast to unrelated diagnoses.** `(new B()) :?> A` —
    "Cannot downcast" diagnostic.
15. **`:?` type test types as bool.** `(s : B) :? D` — typed as
    `bool`.
16. **`:?` type test on unrelated types diagnoses.** Optional, depends
    on the variance rules; v1 emits a Warning rather than an Error.
17. **Implicit subsumption at annotation.** `let s : B = (new D())`
    — succeeds (LHS annotation triggers subsumption via `inferTypeAnnotation`).
18. **Base ctor arg type mismatch diagnoses.** `type B(x: int) =` /
    `type D(s: string) = inherit B(s)` — type-mismatch diagnostic at
    the `inherit` token.

**`CoverageTests.fs`:**

19. **TAST: `s :> B` shapes as `TExpr.Upcast`.** Args / ty correct.
20. **TAST: `s :?> D` shapes as `TExpr.Downcast`.**
21. **TAST: `s :? D` shapes as `TExpr.TypeTest`.**
22. **TAST: `base.M ()` carries `CallVia.Base`.** Discriminator
    distinguishes from `this.M ()`.
23. **TAST: inherited member access still emits `MethodCall` /
    `PropertyGet`.** No new TAST shape for inherited members —
    chain-walking is transparent to consumers.

**`ValidationTests.fs`:**

24. **Override of missing member diagnoses.** `override this.NotThere
    () = 1` without an ancestor declaring `NotThere`.
25. **Cyclic inheritance surfaced once.** Multiple-class cycle emits
    exactly one diagnostic per class involved, not one per `subsumes`
    call.

Each test follows the existing `analyse` → `declType` /
`Expect.equal` / `Expect.isEmpty Diagnostics` pattern.

## Open questions

- **`#Base` flex-type / implicit subsumption at App.** F# permits
  `let f (s: Shape) = …; f c` for `c : Circle` *without* an explicit
  `:>`. The mechanism is the `'T :> Base` constraint added implicitly
  on the parameter typar. v1 requires explicit `:>`; landing this
  involves new constraint-kind plumbing (`SemanticConstraintKind.Coercion`,
  which is currently a no-op stub).
- **Speculative unification / an undo trace.** Everything in v1 is arranged
  so a *choice* is only made once its inputs are ground: `subsumes` is
  read-only and fires only where the target type is known, `checkConstraint`
  returns `Defer` for a free `TyVar` rather than guessing
  (`Unification.fs:513`), and static-opt clause selection waits until
  codegen. That arrangement is exactly what makes the destructive unifier
  (no undo) sufficient — see [§Why `subsumes` being read-only is
  load-bearing](#the-subsumes-relation). Several features parked in these
  open questions break it, because they must *speculatively* constrain
  unsolved variables and feed the result back: **`#Base` flex types /
  implicit App-arg subsumption** (pick a coercion while the arg type is
  still open), **return-type-directed overload resolution** (fsc's
  `alwaysCheckReturn` path — `op_Implicit` / `op_Explicit`, out-args), and
  **variance** (trial a covariant / contravariant match). fsc handles all of
  these with a pervasive undo trace
  (`D:\roboz0r\fsharp\src\Compiler\Checking\ConstraintSolver.fs`
  `FilterEachThenUndo`, `IsSpeculativeForMethodOverloading`). We will **not**
  need that full machinery: because all type-state mutation funnels through
  union-find, a **scoped checkpoint** — an append-only log of
  `(root, oldParent, oldLink, oldConstraints)` with `mark()` / `rewind(mark)`
  around a speculative region — is enough, and is a localized add rather than
  a refactor. Budget it at this milestone; until then the
  decide-when-ground / defer / fall-back-or-error posture stands. The
  same conclusion is reached from the overload-resolution side.
- **Variance on `TyClass` args.** v1 treats args as invariant.
  Covariance (`List<Circle>` as `List<Shape>`) requires an annotation
  on the typar (`<+'a>` / `<-'a>`). Wider treatment lands with the
  variance plan; v1 deliberately rejects covariant uses with a
  diagnostic so the user isn't silently mis-typed.
- **Interfaces.** `interface IFoo with member this.M = …` and the
  `InterfaceImpl` CST piece. Mechanically similar to inheritance —
  every interface is just a virtual base — but interfaces are *not*
  classes (no ctor, no fields, member dispatch only), so they need
  their own registry (`ctx.InterfaceTypes`) and their own `TyInterface`
  variant. Defer; v1 surfaces "interfaces not yet supported" on any
  `InterfaceImpl` / `InterfaceSpec` element.
- **Object expressions.** `{ new IFoo with member this.M = … }`
  (`Expr.Object`) is a single-use anonymous class. Lands after
  interfaces and shares much of the same machinery.
- **Abstract members.** `abstract M : int -> int` declares a slot
  without a body. Slot-matching against overrides is the validation
  story. v1 defers; `MethodOrPropDefn.AbstractSignature` surfaces a
  "not yet supported" diagnostic.
- **`System.Object` as the implicit base.** Every F# class without an
  explicit `inherit` clause silently inherits from `System.Object`.
  v1 treats those as having `BaseType = ValueNone`, which is correct
  for member-lookup (Object's members like `ToString` / `Equals`
  aren't visible without the BCL provider populated) but means the
  `:>` subsumption from `Foo` to `obj` doesn't fire. Lands with the
  provider-backed BCL class catalogue.
- **Equality / comparison through inheritance.** F# uses
  `Object.Equals` / `Object.GetHashCode` by default — reference
  equality, modulo `[<CustomEquality>]` attributes. The
  `checkConstraint` arm for `TyClass` currently returns `Defer`;
  inheritance doesn't change that posture, but it does mean
  `Set<Circle>` (Set demands `: comparison`) needs the constraint
  walker to chase the parent chain too. Same v2 work.
- **Sealed types.** `[<Sealed>]` on a class forbids inheriting from
  it. The attribute walker hasn't landed yet; once it does,
  `registerInheritedSlots` will read `info.IsSealed` and diagnose.
- **`base` aliased via `as`.** F# allows
  `type D() as self = inherit B() member self.M = base.X` — `self`
  is `this`, `base` is still `base`. v1 honours both; the
  `MemberKeyword.Override` story doesn't change.
- **Multiple base-ctor candidates** (additional constructors on the
  parent). `type B = new(x) = …` plus `new(x, y) = …`; deriving
  classes pick which to call via overload resolution. v1 only
  supports a single primary ctor (matching the class-plan v1).
  Defer to the overloading plan.
- **`inherit` clause with type args on the parent.** `type D() =
   inherit Box<int>()` — handled via `translateType`'s existing
  `GenericType` arm, which already produces `TyClass("Box", [int])`.
  Worth one test case to confirm.
- **Multi-segment base type names.** `inherit System.Object()` —
  currently diagnoses "Unknown class type". Lands with the provider's
  BCL class catalogue, same as the cross-file resolution gap noted
  in classes-plan.md.

## Out of scope for this plan

- **Interfaces** (`InterfaceImpl`, `InterfaceSpec`). Mechanically
  related but architecturally distinct — separate registry, separate
  TAST shape. Lands as `interfaces-plan.md`.
- **Object expressions** (`Expr.Object`). Composes interfaces +
  inheritance + anonymous types.
- **Abstract members without default.** Slot-matching, IWSAMs.
- **Additional constructors** (`MemberDefn.AdditionalConstructor`).
  Bundled with the overloading plan.
- **`#Base` flex types and implicit App-arg subsumption.**
- **Variance annotations on typars** (`<+'a>` / `<-'a>`).
- **Sealed / interface inheritance restrictions.** Attribute walker
  work.
- **External (BCL) base classes as `TyClass`.** Same provider gap as
  classes-plan.md §Open questions.
- **`as base`-style aliased base binders.** F# accepts a custom name
  for `base` in some forms; v1 hard-codes `"base"`.
- **`obj` as implicit top of the hierarchy.** Requires the BCL
  provider story.
- **Equality / comparison rule propagation through the parent
  chain.** v1 keeps `Defer` for `TyClass` constraints regardless of
  inheritance depth.
- **Generic-parameter constraints inherited from the parent class.**
  `type Box<'a when 'a : comparison>(...)` plus `type IntBox(...) =
  inherit Box<int>(...)`: the derived class's typar list doesn't
  re-state the parent's constraints, and v1 doesn't propagate them.
  Worth a follow-up alongside the constraints plan.
