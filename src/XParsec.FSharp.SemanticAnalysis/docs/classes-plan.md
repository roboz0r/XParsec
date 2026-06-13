# Classes plan

The build plan for a **subset of F# classes** in the semantic-analysis
pipeline. Same overall shape as [`records-architecture.md`](records-architecture.md)
and [`du-architecture.md`](du-architecture.md): a new named-type registry entry, a
constructor that mints values, members that are looked up on the
receiver's resolved type. Records and DUs already paid for the
named-type infrastructure (`ctx.RecordTypes`, `TyRecord`,
`hasPendingFieldAccess`), so most of the moving pieces are
re-used here — this plan adds a new registry, two new TAST cases,
and one new on-unified callback path.

The status quo is silent on classes. `TypeDefn.Class` is parsed
(`Expr.fs:705`) but never inspected by any pass: NameResolution's
`registerTypeDefns` only recognises `Record` / `Union` / `Abbrev`,
so `TypeDefn.Class` falls off the side. `Expr.New` (the
`new Point(1, 2)` form) and instance-method / property invocations
via `Expr.DotLookup` both reach the conservative `freshTyVar`
fallbacks in Unification and emit nothing useful — any code that
touches a class explodes downstream.

The canonical examples we want to handle after this lands:

```fsharp
// Primary constructor with two parameters; both visible to the class body.
type Point(x: int, y: int) =
    // Instance method — `this` is bound by the `member this.M` form.
    member this.Magnitude() = x * x + y * y
    // Read-only property — `member this.X = …` (no `()` argument list).
    member this.X = x
    member this.Y = y
    // Auto-property — same surface, with explicit initialiser.
    member val Origin = (0, 0)

// Instantiation via `new`.
let p = new Point(3, 4)
//    p : Point

// Instantiation via constructor-as-function (no `new`).
let q = Point(5, 12)
//    q : Point

// Property read.
let xCoord = p.X                   // xCoord : int

// Method invocation.
let sq = p.Magnitude()             // sq : int

// Pass-through via annotation.
let mag (pt: Point) = pt.Magnitude()
//    mag : Point -> int

// Generic class.
type Box<'a>(value: 'a) =
    member this.Value = value
let b = Box(1)                     // b : Box<int>
let v = b.Value                    // v : int
```

What this plan does **not** deliver (see [§Out of scope](#out-of-scope)):
inheritance (`inherit Base(...)`), interfaces (`InterfaceImpl` /
`InterfaceSpec`), abstract / virtual / override members, additional
constructors (`new(...) = ...`), static members, `member val` with
`get, set`, `let`-bindings inside the class preamble, `do`-blocks,
mutable instance fields, and the `:>`/`:?` coercion operators.

## Goal

After the pipeline finishes:

- **`ctx.ClassTypes`** holds one `ClassTypeInfo` per `TypeDefn.Class`
  in the file, name-keyed. Each carries: the declared `TypeParams`,
  the primary constructor's parameter list (name / type), and a
  member list (name, kind: `Method | Property`, type, declKey).
  Mirrors `RecordTypeInfo` / `UnionTypeInfo`.
- **`SemType` gains `TyClass of name: string * args: SemType list`**
  — a named-type wrapper, same family as `TyRecord` / `TyUnion`. Two
  classes unify iff their names match AND their args unify pairwise.
  Member lookup is a side-channel on `ctx.ClassTypes`.
- **`Expr.New`** (`new T(args)`) and **constructor-as-function calls**
  (`T(args)`, an `Expr.App` whose head resolves to a class name) both
  type-check the argument tuple against the primary-constructor signature
  and yield a `TyClass T`.
- **Member access** `r.M` (`Expr.DotLookup`) drives off the receiver's
  resolved type: if `r : TyClass T`, look up `M` in `T`'s member list.
  Properties type as the member's declared type; methods type as
  `TyFun(argTuple, returnTy)` (or curried, matching the binding shape).
  Free receivers defer through the existing `PendingFieldAccess`
  channel, repurposed as `PendingMemberAccess`.
- **`this` binding** inside method / property bodies resolves to a
  TyVar pre-linked to `TyClass T<args>`. Each member opens its own
  binding-group level; `this` is one of the binders.
- **TAST** grows `TExpr.New` (constructor call), `TExpr.MethodCall`
  (instance method invocation), and `TExpr.PropertyGet` (instance
  property read). The frozen tree carries the class name + member
  name + already-typed arguments.

## What we have to build on

| Piece                                  | Where                                           | Status |
|----------------------------------------|-------------------------------------------------|--------|
| `TypeDefn.Class`                       | `XParsec.FSharp/Expr.fs:705`                    | Done — `typeName * primaryConstr * asDefn * body`. |
| `PrimaryConstrArgs`                    | `Expr.fs:467`                                   | Done — `attributes * access * '(' * Pat voption * ')'`. |
| `ObjectModelBody`                      | `Expr.fs:618`                                   | Done — `inherits * classPreamble * elements`. |
| `TypeDefnElement.Member`               | `Expr.fs:494`                                   | Done — wraps `MemberDefn`. |
| `MemberDefn.Member`                    | `Expr.fs:578`                                   | Done — `attributes * static? * keyword * inline? * access * MethodOrPropDefn`. |
| `MethodOrPropDefn.Method`              | `Expr.fs:527`                                   | Done — `(identPrefix * dot)? * Binding`. |
| `MethodOrPropDefn.Property`            | `Expr.fs:528`                                   | Done — same shape as Method, no arg list. |
| `MethodOrPropDefn.AutoProperty`        | `Expr.fs:535`                                   | Done — `val ident (: returnType)? = expr (with get/set)?`. |
| `Expr.New`                             | `Expr.fs:277`                                   | Done — `newToken * typ * expr` (expr = arg tuple). |
| `Expr.DotLookup`                       | `Expr.fs:260`                                   | Done — receiver-driven dispatch, the same shape `r.X` (record field) and `r.M` (class member) share. |
| `Expr.App`                             | `Expr.fs:262`                                   | Done — constructor-as-function (`Point(3, 4)`) folds through here. |
| `TypeVar.PendingFieldAccess`           | `SemanticInfo.fs`                               | Done — single deferred-receiver channel; rename / extend to cover member access. |
| `TyRecord` / `TyUnion` named-type story | `SemanticInfo.fs:89`, `Unification.fs`         | Done — `TyClass` slots in as a peer. |
| `RecordTypeInfo` / `UnionTypeInfo` registries | `SideTables.fs`                          | Done — `ClassTypeInfo` follows the same shape. |
| `freshNamedInstance`                   | `Unification.fs`                                | Done — generic-class instantiation reuses this directly. |
| `typarNamesOfTypeName` / `mkTypeParams` | `NameResolution.fs`                            | Done — generic-class typars piggy-back on the existing helper. |
| `Validation` walks `ModuleElem.Type`   | `Passes/Validation.fs`                          | Currently `()`. No new arm needed for v1 (member bodies are walked through their `Binding.expr`). |

The pieces missing are:

1. **`ClassTypeInfo` + `ClassMemberInfo`** — registry shape: name,
   `TypeParams`, constructor-arg list (name + type), member list. Same
   declaration-keyed layout as `RecordFieldInfo`. Two member kinds
   (`Method`, `Property`) share the same record shape; the kind
   discriminates how `Expr.DotLookup` types the result.
2. **`ctx.ClassTypes` + `ctx.ClassMemberIndex`** — primary registry
   plus a reverse member-name index that the inference path uses for
   ambiguity diagnostics. Same posture as `FieldIndex` / `CtorIndex`.
3. **`SemType.TyClass name args`** — new variant. Every consumer
   (`zonk`, `occursAndAdjust`, `unify`, `substituteWith`,
   `checkConstraint` structural arms, Validation walkers, Freeze)
   gains an arm. Mostly mechanical — the existing `TyRecord` /
   `TyUnion` arms are the template.
4. **NameResolution: `registerClassTypeDefn`** — walks
   `TypeDefn.Class`, mints `TypeParams`, captures constructor-arg
   names (binding sites for the parameter scope inside the class
   body), and stamps placeholder TyVars for each member. Adds each
   constructor-arg name and `this` to the class-body scope.
5. **Unification: `fillClassMembers`** — runs after the registry is
   populated. For each class, opens a fresh typar scope seeded from
   `TypeParams`, opens a binding-group scope with the constructor
   args + `this` (pre-linked to `TyClass name args`), and types every
   member's body. The placeholder member TyVars get linked to the
   resulting member types.
6. **Unification: `Expr.New` + ctor-as-function** —
   `translateType` resolves `T` to `ctx.ClassTypes[T]` and produces
   `TyClass(T, freshArgs)`; the call site unifies the argument tuple
   against the (substituted) constructor signature.
7. **Unification: member dispatch** — `Expr.DotLookup` on a class
   receiver looks up the member in `ctx.ClassTypes[T].Members`,
   substitutes the receiver's arg list against the type params, and
   returns the member's type. Free receivers defer through
   `PendingMemberAccess`.
8. **Freeze: `TExpr.New` / `TExpr.MethodCall` / `TExpr.PropertyGet`**.
9. **Tests** across NameResolution, Unification, Coverage, Validation.

## Why classes need a registry (and not just `TyConst`)

Mechanically the answer is the same as records and DUs: class
definitions live in the same compilation unit as their uses, so they
can't reach through `IExternalSymbolProvider` (that interface is for
*external* assemblies, which classes do extend — see
[§Open questions](#open-questions)). And member resolution is
receiver-driven, not lexical — `p.X` doesn't look up `X` in
`ctx.Binding`, it looks up `X` on whatever type `p` resolves to.

What's new versus records / DUs is that classes have **values that
the type itself constructs**. Records and unions are passive
descriptions: a `TyRecord` shape never produces a value by itself,
only via `Expr.Record` literals; a `TyUnion` only via its ctors. A
class, by contrast, has a primary constructor — a *function value*
keyed off the class name. The registry needs to expose enough to
let `inferApp` / `inferTypeAnnotation` build that function value at
every `new Point(...)` or `Point(...)` site.

Mechanically: each `ClassTypeInfo` stores the constructor parameter
list (the same shape as a function's argument pattern list).
Constructor-as-function calls fold through `Expr.App`'s existing arm
with the looked-up signature; `Expr.New` is a thin wrapper that
takes the class name from a `Type` rather than an expression head.

## The algorithm: register, fill members, dispatch on receiver

### Registration (NameResolution)

For each `TypeDefn.Class` in source order:

1. Pull the single-segment type name; duplicate-name diagnostic if
   already in any of `ctx.RecordTypes` / `ctx.UnionTypes` /
   `ctx.AbbreviationTypes` / `ctx.ClassTypes`.
2. Build `TypeParams` via `mkTypeParams (typarNamesOfTypeName ctx tn)`
   — same as records / unions.
3. Capture the primary constructor's parameter list. The CST is
   `PrimaryConstrArgs.pat: Pat voption`; the contents are F#-spec
   "simple patterns" only (identifiers with optional type
   annotations), but the parser stores a full `Pat`. v1 supports
   `Pat.NamedSimple` and `Pat.Tuple [Pat.NamedSimple | Pat.Typed _ ; …]` —
   anything else diagnoses as "constructor argument patterns must
   be simple identifiers in v1" and is skipped (binding-site empty).
4. For each member in `body.elements`, walk a `MemberDefn.Member`:
   - For `MethodOrPropDefn.Method (defn = b)`: `b.headPat`'s name is
     the member name. Stamp a placeholder `ClassMemberInfo` with
     `Kind = Method`, `Type = TyVar(freshTyVar)`.
   - For `MethodOrPropDefn.Property (defn = b)`: same, `Kind = Property`.
   - For `MethodOrPropDefn.AutoProperty (ident = id; …)`: name is `id`'s
     text, `Kind = Property`. (Always read-only in v1.)
   - For `MethodOrPropDefn.PropertyWithGetSet`, `AbstractSignature`,
     and `MemberDefn.Value` / `MemberDefn.AdditionalConstructor`:
     skip with a "not yet supported" diagnostic.
   - Skip members carrying `staticToken.IsSome` with a "static
     members not yet supported" diagnostic; v1 covers instance only.
5. Add a reverse index entry per member: `ctx.ClassMemberIndex[memberName]`
   gets `(classInfo, memberInfo)` appended. (Used only for the
   ambiguity diagnostic — member access is receiver-driven.)
6. **Scope inside the class body** is supplied by Unification's
   `fillClassMembers`, not by NameResolution. The parameter `this`
   token (from the optional `asDefn`, default name `"this"`) and
   every constructor argument name need to be in scope for member
   body walks; rather than pre-stamping scope entries here,
   Unification opens a synthetic scope before walking each member —
   the same trick `inferBindingGroup` uses for binding-group typars.

The registration runs *before* expression walks, so an `Expr.New`
or `Expr.DotLookup` against a class declared later in the file can
still resolve.

### Member-body fill-in (Unification)

After NameResolution writes `ctx.ClassTypes`, Unification's
pre-pass walks every class and types each member's body:

```fsharp
let private fillClassMembers (ctx: PassContext) (m: ModuleElem<SyntaxToken>) =
    match m with
    | ModuleElem.Type defs ->
        for td in defs do
            match td with
            | TypeDefn.Class(typeName = TypeName(ident = nameLi)
                             primaryConstr = pc
                             asDefn = asD
                             body = body) when nameLi.Idents.Length = 1 ->
                let name = ctx.NameOf nameLi.Idents.[0]
                match ctx.ClassTypes.TryGetValue name with
                | true, info ->
                    let savedScope = ctx.TyparScope
                    ctx.TyparScope <- scopeOfTypeParams info.TypeParams
                    // Pre-link the constructor parameters to their declared
                    // types (or fresh TyVars when un-annotated).
                    seedCtorParamScope ctx info pc
                    // `this` is keyed on a synthetic NodeKey derived from
                    // the class's DeclKey + a marker offset, so multiple
                    // members in one class share the same TyVar.
                    let thisName =
                        match asD with
                        | ValueSome (AsDefn(ident = id)) -> ctx.NameOf id
                        | ValueNone -> "this"
                    let thisTv =
                        let tv = TypeVar()
                        tv.Level <- ctx.CurrentLevel
                        tv.Link <- ValueSome (TyClass(info.Name, ctorArgsAsSelfArgs info))
                        tv
                    ctx.Binding.Set(thisKey, { BindingSite = thisKey; … })
                    // Walk each member: for Method / Property, the member
                    // body is its `Binding.expr`; inferBinding handles the
                    // rest. The member's stamped TyVar gets Link'd to the
                    // resulting type.
                    for el in body.elements do
                        match el with
                        | TypeDefnElement.Member (MemberDefn.Member(defn = d)) ->
                            fillOneMember ctx info d
                        | _ -> ()    // Inherit/Interface skipped
                    ctx.TyparScope <- savedScope
                | false, _ -> ()
            | _ -> ()
    | _ -> ()
```

`fillOneMember` handles each `MethodOrPropDefn` case:

- **Method**: type the `Binding` exactly like a top-level let — head
  pat names the member, `argumentPats` is the method's argument
  list, body types as usual. The member's TyVar gets linked to the
  curried function type `TyFun(arg1, TyFun(arg2, …, retTy))`. The
  `this` binding is in scope, plus the constructor params.
- **Property** (without `get/set`): no `argumentPats` — type the
  body, link the member's TyVar directly to the body type.
- **AutoProperty**: type the initialiser expression; link the
  member's TyVar to its type. The (optional) `returnType` annotation
  unifies with the initialiser.

The member's body must be able to mention any other member of the
same class — `this.OtherMember` is just an `Expr.DotLookup` whose
receiver type is `TyClass T<args>`. Because all members are
pre-stamped with their placeholder TyVars *before* any member body
walks, those references resolve to the placeholder, which gets
linked as soon as the referenced member's body finishes. No special
let-rec discipline needed beyond what records / DU mutually-
recursive field references already get for free.

### Constructor invocation

`Expr.New(typ = T(args); expr = argExpr)`:

1. Translate `T(args)` through `translateType` — same path that
   records / DUs / abbreviations take. Produces `TyClass(name, args')`
   where `args'` is either the user-provided args (generic) or fresh
   TyVars (bare reference).
2. Build the constructor's expected signature from `ClassTypeInfo`:
   the constructor's parameters carry their declared types, which
   are walked through the same `info.TypeParams` substitution that
   `freshNamedInstance` produces. (Mechanically: call
   `freshNamedInstance` to mint the per-use TyVars, then substitute
   the constructor-param types through `subst`.)
3. Type `argExpr`. F# parses `new Point(3, 4)` as
   `Expr.New(_, Type, Expr.EnclosedBlock(_, Expr.Tuple [3; 4], _))`;
   the tuple unifies pairwise with the param list. A single-arg
   ctor (`Box(1)`) parses as `Expr.New(_, _, EnclosedBlock(1))`.
4. The result type is `TyClass(name, args')`.

Constructor-as-function (`Point(3, 4)`, no `new` keyword) reaches
`inferApp`. The function head is `Expr.LongIdentOrOp` (or `Expr.Ident`)
that resolves to neither a local binding nor a provider entry. The
existing fall-through (single-segment, uppercase, ctor-or-class)
extends with a new arm: if the name is in `ctx.ClassTypes`, mint a
fresh-instance constructor signature and type the App as if the
class name were a function value. The TAST emits the same
`TExpr.New` either way.

### Member access

`Expr.DotLookup(expr = r; longIdentOrOp = LongIdent { Idents = [M] })`:

1. Type `r`. Read `resolveStep (typeOf r)`.
2. If it's `TyClass(T, args)`: look up `M` in
   `ctx.ClassTypes[T].Members`. Substitute the member's declared
   type through `mkNamedTypeSubst info.TypeParams args`. The
   resulting `SemType` is the access expression's type.
   - For `Property`: that *is* the access expression's type.
   - For `Method`: the access expression's type is the curried
     function type; the surrounding `Expr.App` (or
     `Expr.HighPrecedenceApp`) consumes it.
3. If it's a free `TyVar`: defer via `PendingFieldAccess` (already
   the deferred-receiver channel for record fields — rename the
   field to `PendingDotAccess` and reuse). When `unify` links the
   TyVar to a `TyClass`, drain the pending list against the class's
   member registry; against a `TyRecord`, against the record's
   field registry. The drain path branches on the link target's
   shape.
4. If it's something else (`TyConst`, `TyFun`, `TyTuple`,
   `TyRecord T` with no field `M`, `TyClass T` with no member `M`):
   diagnose "type `…` has no member `M`."

The "looks like a field access but the receiver is actually a
class" path is uniform because both go through `Expr.DotLookup`.
The single deferred-access channel covers both — the resolver
inspects the link target's shape and dispatches.

### Method calls

`p.M(args)` parses as `Expr.App(Expr.DotLookup(p, ., M), [args])`
(or `Expr.HighPrecedenceApp` for parenthesised single-arg form).
After `Expr.DotLookup` types the access as `TyFun(argTy, retTy)`,
`inferApp` unifies the application args against the function type
in the existing arm — no new code path needed.

`p.M()` (nullary method call) parses as
`Expr.App(DotLookup, [EmptyBlock])` whose argument types as `unit`;
the method's declared type is `TyFun(unit, retTy)`. Pure
mechanical fall-through.

## Data-model changes

### `SemanticInfo.fs`

A new `SemType` variant:

```fsharp
type SemType =
    | TyVar of TypeVar
    | TyConst of name: string
    | TyFun of arg: SemType * result: SemType
    | TyTuple of items: SemType list
    | TyRecord of name: string * args: SemType list
    | TyUnion of name: string * args: SemType list
    /// Named class type with instantiated arg list. Same shape as
    /// `TyRecord` / `TyUnion`; member lookup is a side-channel on
    /// `ctx.ClassTypes`. Two `TyClass` unify iff their names match
    /// AND their args unify pairwise.
    | TyClass of name: string * args: SemType list
```

Every consumer of `SemType` adds an arm — the diff is mechanically
identical to the `TyRecord` / `TyUnion` rollouts.

### `SideTables.fs`

```fsharp
[<RequireQualifiedAccess>]
type ClassMemberKind =
    | Method
    | Property

[<Sealed>]
type ClassMemberInfo
    (
        name: string,
        kind: ClassMemberKind,
        ty: SemType,
        declKey: NodeKey
    ) =
    member val Name = name
    member val Kind = kind
    /// Filled by Unification once the member body has been typed.
    /// Placeholder TyVar at NameResolution time so forward references
    /// (`this.Other` in one member's body where `Other` is declared
    /// further down) resolve.
    member val Type = ty
    member val DeclKey = declKey

[<Sealed>]
type ClassCtorParamInfo
    (
        name: string,
        ty: SemType,
        declKey: NodeKey
    ) =
    member val Name = name
    member val Type = ty
    member val DeclKey = declKey

[<Sealed>]
type ClassTypeInfo
    (
        name: string,
        typeParams: (string * TypeVar) list,
        ctorParams: ClassCtorParamInfo[],
        members: ClassMemberInfo[],
        declKey: NodeKey
    ) =
    member val Name = name
    member val TypeParams = typeParams
    member val CtorParams = ctorParams
    member val Members = members
    member val DeclKey = declKey
```

Storage on `PassContext`:

```fsharp
/// Written by NameResolution from `TypeDefn.Class`s. Member types
/// start as placeholder TyVars and get linked by Unification's
/// fill-in pass.
member val ClassTypes = Dictionary<string, ClassTypeInfo>() with get
/// Reverse index: member name → list of (class, member) pairs.
/// Used only for the ambiguity-disambiguation diagnostic when a
/// receiver's type is free and the member name occurs in multiple
/// classes — same shape as `FieldIndex` / `CtorIndex`.
member val ClassMemberIndex = Dictionary<string, (ClassTypeInfo * ClassMemberInfo) list>() with get
```

### `TypeVar`

The existing `PendingFieldAccess: (fieldName, useKey, resultTyVar) list`
already carries the right shape. Rename to **`PendingDotAccess`** and
let the resolver inspect the link target:

```fsharp
and [<Sealed>] TypeVar() =
    // … existing fields …
    /// Deferred receiver-dispatch constraints. Drained by `unify`
    /// when the TyVar's `Link` becomes a `TyRecord _`, `TyUnion _`,
    /// or `TyClass _`; the drain code branches on the link target
    /// to resolve against fields vs members.
    member val PendingDotAccess: (string * NodeKey * TypeVar) list = [] with get, set
```

The existing record-field resolver becomes one arm in
`drainPendingDotAccess`:

```fsharp
match linkTarget with
| TyRecord(name, args) -> drainAsRecordField ctx … // existing path
| TyClass(name, args)  -> drainAsClassMember ctx …
| _ -> ()                                           // not a dispatch target
```

### Member body scope

`this` is a binding like any other. NameResolution doesn't see the
class body directly (the registration pre-pass doesn't walk member
bodies); Unification's `fillClassMembers` opens the scope when it
walks each member's `Binding.expr`.

Synthetic NodeKey for `this`: derived from the class's `DeclKey` via
`NodeKey.ofSynthetic (info.DeclKey.Offset) NodeKind.SynthThisBinding`
(new `NodeKind`). One synthetic key per class — all members in the
class share the same `this` TyVar (linked to `TyClass(name, args)`).

## Pass-by-pass changes

### `NameResolution`

A new `registerClassTypeDefn` parallel to the existing
`registerRecordTypeDefn` / `registerUnionTypeDefn`:

```fsharp
let private registerClassTypeDefn (ctx: PassContext) (td: TypeDefn<SyntaxToken>) =
    match td with
    | TypeDefn.Class(typeName = tn; primaryConstr = pc; body = body) ->
        let (TypeName(ident = nameLi)) = tn
        if nameLi.Idents.Length <> 1 then () else
        let nameTok = nameLi.Idents.[0]
        let name = ctx.NameOf nameTok
        let declKey = NodeKey.ofToken nameTok NodeKind.DeclType

        if duplicateInAnyRegistry ctx name then
            ctx.Diagnostics.Add { Key = declKey; Message = … }
        else
            let typeParams = mkTypeParams (typarNamesOfTypeName ctx tn)
            let ctorParams = extractCtorParams ctx pc
            let memberInfos =
                [|
                    for el in body.elements do
                        match el with
                        | TypeDefnElement.Member (MemberDefn.Member(staticToken = s; defn = d)) when s.IsNone ->
                            match d with
                            | MethodOrPropDefn.Method(defn = b) ->
                                yield mkPlaceholderMember ctx ClassMemberKind.Method b
                            | MethodOrPropDefn.Property(defn = b) ->
                                yield mkPlaceholderMember ctx ClassMemberKind.Property b
                            | MethodOrPropDefn.AutoProperty(ident = id) ->
                                yield mkPlaceholderAutoProp ctx id
                            | _ -> ()    // diagnose "not yet supported"
                        | _ -> ()        // Inherit/Interface/Static deferred
                |]
            let info = ClassTypeInfo(name, typeParams, ctorParams, memberInfos, declKey)
            ctx.ClassTypes.[name] <- info
            for m in memberInfos do
                let key = m.Name
                match ctx.ClassMemberIndex.TryGetValue key with
                | true, lst -> ctx.ClassMemberIndex.[key] <- (info, m) :: lst
                | false, _  -> ctx.ClassMemberIndex.[key] <- [(info, m)]
    | _ -> ()
```

`extractCtorParams` translates `PrimaryConstrArgs.pat`:

- `Pat.NamedSimple t` → `[ClassCtorParamInfo(name=t, ty=fresh TyVar, declKey)]`.
- `Pat.Typed(pat = Pat.NamedSimple t; typ = ty)` → `[…(ty = translateType ty)]`.
- `Pat.Tuple pats` → recurse over each element.
- Anything else → diagnostic + empty list.

Both `name` resolution within the class body and Unification's
fill-in walk read the same `ClassTypeInfo.CtorParams`.

Order matters: classes must be registered before any expression
walk so a `new Point(...)` literal in the same module can resolve
back to a class defined further down. NameResolution's `walkElems`
already sequences `registerRecord*` / `registerUnion*` /
`registerAbbreviation*` before the expression walk — add
`registerClassTypeDefn` to that list.

No scope changes for the module-level walk. Class member names are
not in lexical scope; `p.X` looks up `X` through `p`'s type.

### `Unification`

Three new touch-points, plus extending the deferred-access drain
to recognise `TyClass`:

**1. `fillClassMembers` pre-pass.** Runs after
`fillAbbreviationBodies` / `fillRecordFieldTypes` /
`fillUnionFieldTypes`. For each class, walk the CST `body.elements`
under a typar scope seeded from `info.TypeParams`, then walk each
member:

```fsharp
let private fillOneMember
    (ctx: PassContext)
    (info: ClassTypeInfo)
    (m: ClassMemberInfo)
    (defn: MethodOrPropDefn<SyntaxToken>)
    : unit =
    // Open a binding-group level so any TyVars in the member's body
    // generalise at the right scope.
    let outerLevel = ctx.CurrentLevel
    enterLevel ctx
    try
        let memberTy =
            match defn with
            | MethodOrPropDefn.Method(defn = b)
            | MethodOrPropDefn.Property(defn = b) ->
                // Re-uses inferBinding's body walker. The
                // headPat's TyVar is `m.Type` (already in
                // `info.Members`); body type unifies with it.
                inferMemberBinding ctx info m b
            | MethodOrPropDefn.AutoProperty(ident = id; expr = e
                                            returnType = rt) ->
                let bodyTy = infer ctx e
                let annTy =
                    match rt with
                    | ValueSome (ReturnType(typ = t)) ->
                        let t = translateType ctx t
                        unify ctx (CstKeys.ofExpr e) bodyTy t
                        t
                    | ValueNone -> bodyTy
                annTy
            | _ -> TyVar(freshTyVar ctx)
        // Link the placeholder.
        match m.Type with
        | TyVar tv -> (UnionFind.find tv).Link <- ValueSome memberTy
        | _ -> ()
    finally
        exitLevel ctx
```

The `this` binding gets minted once per class fill-in (synthetic
NodeKey, see §Member body scope) and Bound to `TyClass(info.Name,
ctorArgsAsSelfArgs info)`; constructor-arg names get fresh
binding-site entries with their (already-translated or fresh-TyVar)
types. Both go into `ctx.Binding` so `Expr.Ident`/`Expr.LongIdentOrOp`
inside member bodies resolve through the regular path.

**2. `Expr.New` arm.** New arm in `infer`:

```fsharp
| Expr.New(typ = t; expr = argExpr) ->
    let receiverTy = translateType ctx t
    match resolveStep receiverTy with
    | TyClass(name, args) ->
        match ctx.ClassTypes.TryGetValue name with
        | true, info ->
            // Mint per-use ctor signature.
            let subst = mkNamedTypeSubst info.TypeParams args
            let paramTys =
                info.CtorParams
                |> Array.map (fun p -> substituteWith subst p.Type)
                |> Array.toList
            let expected =
                match paramTys with
                | []     -> MockBuiltins.tyUnit
                | [t]    -> t
                | many   -> TyTuple many
            let argTy = infer ctx argExpr
            unify ctx (CstKeys.ofExpr argExpr) argTy expected
            receiverTy
        | false, _ ->
            ctx.Diagnostics.Add { Key = …; Message = sprintf "Unknown class type '%s'" name; Severity = Error }
            TyVar(freshTyVar ctx)
    | _ ->
        ctx.Diagnostics.Add { Key = …; Message = "'new' requires a class type"; Severity = Error }
        TyVar(freshTyVar ctx)
```

**3. Ctor-as-function in `inferIdent`.** When a single-segment
ident resolves to neither a local binding nor a provider entry nor
the existing ctor-index (DU), check `ctx.ClassTypes`:

```fsharp
match ctx.ClassTypes.TryGetValue name with
| true, info ->
    // Mint a fresh instance, return the ctor as a function value.
    let args, subst = freshNamedInstance ctx info.TypeParams
    let receiverTy = TyClass(info.Name, args)
    let paramTys =
        info.CtorParams
        |> Array.map (fun p -> substituteWith subst p.Type)
        |> Array.toList
    match paramTys with
    | [] -> TyFun(MockBuiltins.tyUnit, receiverTy)
    | [t] -> TyFun(t, receiverTy)
    | many -> TyFun(TyTuple many, receiverTy)
| false, _ -> …
```

This mirrors `ctorType` for DU ctors. The "function value" shape
means `Expr.App` types `Point(3, 4)` correctly without a separate
`Expr.New` arm — the tuple argument unifies with the tuple param.
The TAST canonicalises both forms to `TExpr.New` so downstream
consumers don't have to branch.

**4. `Expr.DotLookup` extension.** The existing arm matches
single-segment `r.X`; extend `resolveFieldStep`:

```fsharp
and private resolveDotStep (ctx: PassContext) (diagKey: NodeKey) (rTy: SemType) (memberName: string) : SemType =
    match resolveStep rTy with
    | TyRecord(recName, args) -> resolveRecordField ctx … // existing
    | TyClass(clsName, args) ->
        match ctx.ClassTypes.TryGetValue clsName with
        | true, info ->
            match info.Members |> Array.tryFind (fun m -> m.Name = memberName) with
            | Some m ->
                let subst = mkNamedTypeSubst info.TypeParams args
                substituteWith subst m.Type
            | None ->
                diag $"Type '{clsName}' has no member '{memberName}'"
                TyVar(freshTyVar ctx)
        | false, _ ->
            diag $"Unknown class type '{clsName}'"
            TyVar(freshTyVar ctx)
    | TyVar tv ->
        // Defer through PendingDotAccess (renamed
        // PendingFieldAccess); the drain resolver dispatches on
        // link-target shape.
        let root = UnionFind.find tv
        let resultTv = freshTyVar ctx
        root.PendingDotAccess <- (memberName, diagKey, resultTv) :: root.PendingDotAccess
        TyVar resultTv
    | _ -> diag $"Cannot access member '{memberName}' on non-class non-record type"; …
```

**5. Drain extension for `TyClass`.** `drainPendingFieldAccess`
(renamed `drainPendingDotAccess`) gains an arm:

```fsharp
match resolveStep linkTarget with
| TyRecord(name, args) -> …  // existing
| TyClass(name, args) ->
    let pending = root.PendingDotAccess
    root.PendingDotAccess <- []
    match ctx.ClassTypes.TryGetValue name with
    | true, info ->
        let subst = mkNamedTypeSubst info.TypeParams args
        for (memberName, useKey, resultTv) in pending do
            match info.Members |> Array.tryFind (fun m -> m.Name = memberName) with
            | Some m -> unify ctx useKey (TyVar resultTv) (substituteWith subst m.Type)
            | None ->
                ctx.Diagnostics.Add { Key = useKey
                                      Message = $"Type '{name}' has no member '{memberName}'"
                                      Severity = Error }
    | false, _ -> …
```

**6. `migrateBounds` extension.** Already handles
`PendingFieldAccess` migration on TyVar union — works unchanged
after the rename.

### `Regions`

A new arm in `isAllocation` for `TyClass _ -> true`: every class
instance is a heap allocation. The escape rules mirror records —
`Expr.New` mints a region with outgoing edges to each constructor-
argument's region (the constructed object holds its constructor
arguments). Method calls don't allocate (return value's region is
inferred from the method's body); property reads return existing
state.

Conservative fallback: today's `Expr.New` routes through the
catch-all. Once the precise rule above lands, classes get the same
level / lambda-reach treatment as records.

`exprIsAllocation` is the only Regions edit, plus the new `Expr.New`
arm in the region walker.

### `Validation`

No new diagnostics in v1. The `PendingDotAccess` "receiver never
resolved" check already covers the unresolved-member-access case,
via the existing `checkUnresolvedFieldAccesses` walker (rename to
`checkUnresolvedDotAccesses` and broaden the message).

Future Validation checks (not in v1):

- **Assignment to immutable property**: members declared without
  `with set` are read-only; `p.X <- v` against a get-only property
  diagnoses. Lands with `member val … with get, set`.
- **Accessing a member of the wrong static-ness**: instance member
  via type name. Lands with statics.

### `Freeze`

Three new TAST cases:

```fsharp
| New of className: string * args: TExpr list * ty: SemType
| MethodCall of receiver: TExpr * methodName: string * args: TExpr list * ty: SemType
| PropertyGet of receiver: TExpr * propertyName: string * ty: SemType
```

`ty` is the constructed `TyClass` / member's return type /
property type respectively.

- `Expr.New` and constructor-as-function `Expr.App` both fold to
  `TExpr.New` — the canonical form is class-name plus the (possibly
  empty) argument list.
- `Expr.DotLookup` on a class with `Kind = Method` followed by an
  `Expr.App` folds to `TExpr.MethodCall`. The Freeze pattern:
  detect `App(DotLookup(r, ., M), args)` where `M`'s `ClassMemberInfo`
  is `Method`; emit the fused node.
- `Expr.DotLookup` on a class with `Kind = Property` (no following
  `App`) folds to `TExpr.PropertyGet`.

`TastShape.prettyDecl` grows render arms — `"new Point(3, 4)"`,
`"p.M(a, b)"`, `"p.X"`.

## Pipeline integration

No new pass. Classes are handled by extending NameResolution
(register), Unification (fill members + dispatch), Regions
(`Expr.New` allocation rule), and Freeze (TAST nodes) — same shape
as records / DUs / abbreviations.

The pass order from [`passes.md`](passes.md) is unchanged.
Registration runs in `NameResolution.walkElems` after the existing
type-registry passes; member-body fill-in runs in
`Unification.walkElems` after `fillAbbreviationBodies` and the
record / union field fill-ins (so a class member's body can
reference any other named type in the same file).

## Test strategy

Tests split across the four existing test modules. Each pass owns
its own half of the contract — same posture as the records / DU /
abbreviations splits.

**`NameResolutionTests.fs`:**

1. **Class type registered.** `type C(x: int) = member this.X = x` —
   `ctx.ClassTypes["C"].Members.Length = 1`,
   `ctx.ClassTypes["C"].CtorParams.Length = 1`.
2. **Duplicate type name diagnoses.** `type C = { X: int }` followed
   by `type C() = …` → "Duplicate type definition".
3. **Member-index built.** `type C() = member this.M () = 1` —
   `ctx.ClassMemberIndex["M"]` contains C.
4. **Static / additional ctor / interface impl members diagnose.**
   `type C() = static member M () = 1` → "static members not yet
   supported".

**`UnificationTests.fs`:**

5. **Constructor invocation via `new`.**
   `type Point(x: int, y: int) = member this.X = x\nlet p = new Point(3, 4)` —
   `p : TyClass("Point", [])`.
6. **Constructor-as-function call.**
   `type Point(x: int, y: int) = member this.X = x\nlet p = Point(3, 4)` —
   same type as above; both forms canonicalise.
7. **Constructor argument-type mismatch.**
   `new Point(3, "x")` against `Point(x: int, y: int)` — diagnostic.
8. **Property read pins receiver.**
   `let f (p : Point) = p.X` — `f : Point -> int`.
9. **Method invocation.**
   `let m (p : Point) = p.Magnitude()` — `m : Point -> int`.
10. **Member on free TyVar pinned by use.**
    `let f p = p.X in let _ = f (new Point(3, 4))` — `f` types as
    `Point -> int`; no unresolved-member diagnostic.
11. **Member on free TyVar never pinned.**
    `let f p = p.X` standalone — `Cannot resolve member` diagnostic.
12. **Generic class instantiation.**
    `type Box<'a>(value: 'a) = member this.Value = value\nlet b = Box(1)` —
    `b : TyClass("Box", [int])`, `b.Value : int`.
13. **Generic class annotation pins typar.**
    `let b : Box<string> = Box("hi")` — clean; `b.Value : string`.
14. **Method body sees constructor parameter.**
    `type C(x: int) = member this.Get () = x` — `C.Get`'s body
    types as `unit -> int` and references `x` cleanly.
15. **Method body sees other members via `this`.**
    `type C(x: int) = member this.Inner () = x\nmember this.Outer () = this.Inner ()` —
    `Outer : unit -> int`, `Inner : unit -> int`.
16. **Auto-property types from initialiser.**
    `type C() = member val Origin = (0, 0)` — `Origin : int * int`.
17. **Auto-property with annotation.**
    `type C() = member val Tag : string = ""` — `Tag : string`.

**`CoverageTests.fs`:**

18. **TAST shape for `new Point(3, 4)`** — `TExpr.New("Point", [TConst 3; TConst 4], TyClass("Point", []))`.
19. **TAST shape for `p.X`** — `TExpr.PropertyGet(TExpr.Var "p", "X", TyConst "int")`.
20. **TAST shape for `p.M(1)`** — `TExpr.MethodCall(TExpr.Var "p", "M", [TConst 1], …)`.

**`ValidationTests.fs`:**

21. **Unresolved member diagnoses.** `let f p = p.NotAField` (no use)
    — diagnostic.
22. **Resolved-by-use member access is clean.**
    `type C() = member this.M () = 1\nlet f p = p.M()\nlet _ = f (new C())` —
    no diagnostic.

Each test follows the existing `analyse` → `declType` / `Expect.equal`
/ `Expect.isEmpty Diagnostics` pattern.

## Open questions

- **`this` token under explicit `asDefn`.** F# lets a class re-name
  `this`: `type C() as self = member self.M () = 1`. The `asDefn`
  field in `TypeDefn.Class` carries the alias token. v1 honours it
  for the parameter scope; downstream code (TAST) renders as the
  user-supplied name.
- **Constructor-param `let`-bindings.** F# desugars
  `type C(x: int) = let y = x + 1 in member this.Y = y` —
  constructor-param-derived locals visible to every member. Lands
  later (the `classPreamble` field carries
  `ClassFunctionOrValueDefn.LetBindings`).
- **Constructor-side-effecting `do`-blocks.** `type C() = do printfn
  "hi"`. Same `classPreamble` channel; lands with `let`-preamble
  support.
- **Member overloading by arity.** Two methods named `M` taking
  different argument tuples. F# allows it; the registry shape needs
  a list-per-name (`Members: ClassMemberInfo[] but lookup-by-name`
  becomes "find the arity-matching candidate"). v1 takes the first
  match and diagnoses subsequent ones; full overload resolution
  lands with a dedicated overloading plan.
- **Speculative unification for overload resolution.** The
  overloading-by-arity case above — and external-method overload
  resolution generally —
  ultimately needs to *try* a candidate signature against the argument
  types and back out if it doesn't fit. v1 sidesteps this (first match
  wins, diagnose the rest), which is only viable because the current
  unifier is *destructive* (`UnionFind.union` + `TyVar.Link`, **no undo**)
  and so cannot trial-and-rollback. Real resolution either restricts itself
  to **ground** argument types — a read-only `feasiblySubsumes` /
  compatibility check, committing only the winner, which covers the common
  BCL call site — or, for non-ground cases, needs a **scoped union-find
  checkpoint** (`mark()` / `rewind(mark)` over an append-only mutation log).
  The dedicated overloading plan owns this; the fsc reference is
  `ConstraintSolver.fs` `FilterEachThenUndo`. The same
  requirement surfaces from the subtyping side in
  [`inheritance-plan.md`](inheritance-plan.md) §Open questions.
- **`base.M(...)` calls.** Requires inheritance; deferred. The
  current `BaseCall` CST piece (used in `Expr.Object`) is unaffected
  by v1.
- **Member that mentions a generic typar in arg position.**
  `member this.Map (f: 'a -> 'b) = …` — works via the typar scope
  already opened for the class, plus implicit-typar introduction
  on the member's `Binding.typarDefns`. Covered by the existing
  generics plumbing; explicit test case worth adding.
- **`member val` with `with get, set` (mutable auto-property).**
  Requires a per-member `IsMutable` flag plus the cell-region
  machinery from `mutable-plan.md` extended to class fields.
  Deferred to v2 alongside mutable instance fields.
- **`Expr.HighPrecedenceApp` parens around the arg tuple.**
  `Point(3, 4)` parses as `Expr.App(Expr.Ident "Point",
  [Expr.EnclosedBlock(Expr.Tuple [3; 4])])` *or*
  `Expr.HighPrecedenceApp(Expr.Ident "Point", _, Expr.Tuple [3; 4], _)`
  depending on the lex of the opening `(`. Both forms reach
  `inferApp`'s tuple-arg path naturally; verify with a coverage
  test.
- **Cross-file class resolution.** `ctx.ClassTypes` is per-file.
  When modules / namespaces land, the registry layers over a
  provider-backed catalogue (same posture as records / DUs).
  Currently the .NET provider does deliver classes via
  `MetadataLoadContext` for BCL types, but routing them through a
  named-type channel is a separate plumbing job.
- **Generic-class constraint propagation.** `type Set<'a when 'a : comparison>(items: 'a list) = …`.
  The constraint mechanism from `constraints-plan.md` works
  unchanged here — `freshNamedInstance` copies prototype constraints
  onto every use site's fresh TyVars. Worth one test alongside the
  rest.
- **Member-access vs field-access naming conflict.** Both records
  and classes go through `Expr.DotLookup`. The receiver's resolved
  type discriminates: `TyRecord` → field-lookup, `TyClass` →
  member-lookup. The shared `PendingDotAccess` channel routes
  correctly because the drain inspects the link-target shape.
  Test case: a record `R` and a class `C` both expose `X`; `r.X`
  and `c.X` type independently without cross-talk.
- **Equality on classes.** F# classes are reference-equal by default
  (`Object.Equals` / `Object.GetHashCode`). The `constraints-plan.md`
  trait table has no arm for `TyClass _` under `Equality` / `Comparison`
  — `Defer` is the right v1 answer, with a follow-up when the
  attribute walker lands (`[<CustomEquality>]` etc.).

## Out of scope for this plan

- **Inheritance** (`type Derived(...) inherit Base(...)`). Requires
  a `BaseType` field on `ClassTypeInfo` plus an upcast-aware
  unification path; subsumption rules are a larger design. Lands
  with the IWSAM / coercion track.
- **Interfaces and `InterfaceImpl`.** Same family as inheritance —
  needs subtyping. The `InterfaceSpec` / `InterfaceImpl` CST pieces
  already exist; v1 simply skips them with a "not yet supported"
  diagnostic.
- **Abstract / virtual / override members.** Members marked
  `abstract`, `default`, or `override` in the `MemberKeyword`
  field; v1 only honours plain `member`. Each abstract member
  becomes a slot that subclasses fill; needs the inheritance work
  first.
- **Additional constructors.** `MemberDefn.AdditionalConstructor` —
  `new(arg) = primary(...)`. Requires the `AdditionalConstrExpr`
  walker; defer.
- **Static members.** `static member M (...) = …`. Adds a separate
  member-lookup channel (the class name, not an instance, is the
  receiver). Mechanically straightforward but adds a parallel
  dispatch path; defer.
- **`member val … with get, set` and mutable instance fields.**
  Cell-region machinery from `mutable-plan.md` extends naturally;
  lands with mutable fields generally.
- **Constructor-preamble `let`-bindings and `do`-blocks.** Visible
  to every member; lands with the `classPreamble` walk.
- **Property-with-explicit-get/set** (`member this.X with get () = … and set v = …`).
  Two-binding member definition; requires the `PropertyWithGetSet`
  arm of `MethodOrPropDefn`. Defer.
- **Indexed properties** (`member this.Item with get i = …`).
  Same machinery as `PropertyWithGetSet`. Defer.
- **Struct classes** (`[<Struct>] type C(...) = …`). Same surface
  shape; different region treatment (struct allocation is inline).
  Lands with structs generally.
- **Operator members** (`static member (+) (a, b) = …`). Same
  channel as operator-resolution; lands with the operator-overload
  story.
- **Optional / named arguments** in constructors and methods. v1
  takes positional only.
- **`Expr.Object` (object expressions).** `{ new IInterface with member this.M () = … }`.
  Different shape from a `TypeDefn.Class`; lands with interfaces.
- **`:>` upcast / `:?` test / `:?>` downcast.** Lands with
  inheritance.
- **External (BCL-defined) classes as `TyClass`.** Today the .NET
  provider returns `ExternalSymbol`s that look like function values;
  routing them through `ctx.ClassTypes` as proper named types is a
  separate plumbing job in the provider's named-type catalogue.
