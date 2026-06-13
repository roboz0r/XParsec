# Module-value representation & lowering plan

**Status: v1 landed (2026-06-09).** The §7 steps are implemented for the
**ground-typed, named-module-holder** case (the `set.fs` `SetTree.empty` shape):
module values emit as `public static` fields on their module holder, initialised
by the holder's `.cctor`, referenced via `ldsfld` (`EmitContext.ModuleValues`),
and excluded from capture analysis. **Generic values landed
(2026-06-09)** — see §9 below. **Deferred** (each keeps its current treatment, no
regression): function-values stored as values, anonymous top-level
"Program"-holder values (§2.5 `do`/`Main` ordering), and values whose type never
grounded (`FTUnknown` — skipped). Gates: `ClassTests.fs` `ClassStatic` "… module
value …" rows + "a generic module value lowers to a generic method …".

---

## 9. Generic module values — generic static methods (2026-06-09)

A **generic** module value (`let empty: SetTree<'T> = null`) cannot become a
static *field*: a module holder is **non-generic**, so it has no type parameter
to type a `SetTree<'T>` field, and the handoff's "generic static field on the
open holder TypeSpec" idea (which assumed a generic holder, the G13 class shape)
does not apply. Instead — exactly as **real F#** represents a generic value — it
lowers to a **zero-arg generic static method** on its holder, returning the
initialiser; every reference is a `call` to its `MethodSpec`.

- **Classification** (`EmitClosures.collectGenericModuleValues`): a non-`inline`,
  non-`Lambda` `let` on a *named* holder whose frozen type is **non-ground**
  (carries an `FTTypar`, freeze-quantified to the method axis) and **`FTUnknown`-
  free** (a leaked metavar can't be encoded — kept skipped, as ground values
  already are). A *function*-typed generic value (a stored closure) stays
  deferred. Returned as ordinary 0-param `StaticFn`s.
- **Emission**: merged into `HolderPlan.StaticFns`, so the whole static-method
  machinery (signature via `GenericStaticFnSignature`, body via
  `buildStaticMethod`, `MethodDef` prediction, the holder method group) handles
  them uniformly. No `.cctor` involvement (a method is computed on demand). Their
  keys also join `StaticFnKeys`, so `collectStaticFns` / `discoverClosures` treat
  a reference to one as resolved storage (never a capture) — a function over a
  generic value stays a static method, not a closure (§4, extended to both key
  sets).
- **Reference lowering** (`EmitExpr.buildExpr`, the `Var` arm guarded by
  `env.StaticMethods.ContainsKey`): a module value is never applied, so — unlike a
  static *function*, which `collectStaticFns` proves is always saturated and thus
  only appears as an `App` head — it reaches codegen as a bare `Var`. Emit a 0-arg
  `call` to its `MethodSpec`, the instantiation recovered by `matchInstantiation`
  matching the method's declared result template against the reference's own type
  (e.g. `SetTree<'T_class>` ⇒ `[FTTypar(Declaring,0)]` inside a generic class's
  cctor; `SetTree<'T_method>` ⇒ `[FTTypar(Method,0)]` inside a sibling fn).
- **`TExprG.Null`**: this was the first bare `null` *value* ever emitted (the
  ground path never reached one, since `SetTree<'T>` is generic) — added an
  `Ldnull` arm to `buildExpr`. `ldnull` returning the class instantiation
  `SetTree<!!0>` verifies; a *bare* typar return (`!!0`) would not, which is why
  the front-end gate (next bullet) excludes bare-typar values.
- **Front-end prerequisite (the actual blocker).** The handoff's "pure codegen
  gap" framing was wrong: `let empty = null` froze to `FTUnknown` because the
  value's type came entirely from later uses and was never grounded; even
  annotated (`let empty: SetTree<'T> = null`) the **type argument `'T`** leaked as
  `FTUnknown`. Root cause: `Elaborate` built the method-typar `quantEnv` (the
  `TyVar → TyTypar(Method,i)` markers that survive `freezeTypars`) **only for
  `TyFun` bindings** — a generalised *value* binding got `quantEnv = []`, so its
  free typars never became `FTTypar` and froze to `FTUnknown`. Fix: also quantify
  a value binding **when it was generalised into a non-empty scheme AND its free
  typars sit inside a type constructor** (not a bare `TyVar`/`TyTypar` — that stays
  a value-restriction metavar, so `let n = null` is unchanged). The `set.fs`
  source gains the `: SetTree<'T>` annotation (real FSharp.Core types `empty`
  explicitly too). Gate: `SemanticAnalysis` "`null` types as a free TypeVar"
  (unchanged) + the codegen end-to-end row.

This solved the case where a generic class's
`static let` initialiser (and, it turns out, *any* type-member body or `.cctor`)
that references a module-level value (`SetTree.empty`) fails codegen with
`Emit: no binding for variable src@<off>:PatIdent`.

This document is the single record of how module-level **values** (`let x = e`
at module scope, as opposed to module *functions*) are represented in the
emitted assembly, the lowering semantics, and the invariants codegen guarantees.

---

## 1. The problem — module values have no storage today

The handoff's diagnosis (a stale `static let` assumption at `Elaborate.fs:918`)
was **wrong**. Reproduced three ways (generic `static let`, mono `static let`,
and a plain instance-member body) — *all three* fail identically on the module
value, while a module **function** referencing a module value succeeds. The real
cause:

- A module-level value `let empty = null` is emitted as a **`Main` local**
  (`Emit.buildMain`: each top-level `TDecl.Let(NamedSimple …)` allocates an IL
  local and `stloc`s the init). It has no storage outside `Main`.
- A reference to it is a `TExpr.Var(bindingKey)`. `EmitExpr.buildVarLoad`
  resolves only method args, the closure self, capture fields, and method-local
  slots — **no module-value arm**. So a `Var` to a module value resolves *only*
  when it happens to be a live `Main` local (i.e. the reference is lexically
  inside `Main`).
- A module **function** that references a module value is therefore excluded
  from static-method eligibility (`collectStaticFns` rule 2 — "captures a
  module-level local") and is emitted as a **closure capturing the `Main`
  local**. That is the F# "init then capture" pattern — exactly what we want to
  avoid — and it only works because the capture and the use both live under
  `Main`.
- A **Library** (`Vesper.Set`) emits **no `Main` at all** (`EmitMain false`), so
  its module values are never allocated *any* storage. Every cross-context
  reference (class member, `.cctor`, sibling module fn that is *not* itself
  Main-reachable) is unresolvable.

So this is not a `static let` bug. Module-level values are simply not first-class
in the backend.

---

## 2. Target representation

> A module value is a **public static field on its module's holder type**,
> initialised once by that holder's **`.cctor`**, read everywhere as `ldsfld`.
> No init function, no re-check on access.

### 2.1 What is a "module value"

A top-level `TDecl.Let(NamedSimple(k, _), value, isInline, ty)` is a **module
value** iff:

1. it is **not** `inline` (inline bindings are already spliced away pre-freeze
   by `Passes.InlineExpansion` and removed by `Emit.lower`), and
2. its bound value is **not a function emitted as a static method** — i.e. its
   key is **not** in `staticFnKeys`.

Concretely this is the set of top-level `let`s that bind a **plain value**
(`peelLambda` yields zero parameters: `let empty = null`, `let pi = compute ()`)
**plus** any function-value that escapes or captures and so could not become a
static method (a closure stored as a value — `let add = fun … -> …` used as a
value). Both need a single piece of storage initialised once; both become static
fields.

For the v1 slice the dominant case is **plain values**; the plain-value subset is
identifiable up front (no lambda parameters) without first resolving
`staticFnKeys`, which matters for breaking the analysis cycle in §4.

### 2.2 Holder placement (F#-correct)

Each module value lives on the **holder type of its declaring module**, the same
holder its sibling module *functions* already emit onto:

- A binding inside a named `module Foo` (recorded in `TastFile.ModuleMembers`)
  → a `public static` field `Foo::x` on the `Foo` holder (namespace + holder
  name from `ModuleMemberInfo`).
- A top-level binding (no enclosing named module) → a `public static` field on
  the anonymous **"Program"** holder (`project.ModuleName`).

This matches what a consumer (and F#) expects: `SetTree.empty` resolves as a
static field on the `SetTree` type. It is the representation a future
cross-assembly consumer (G8) reads directly — no rework.

### 2.3 Field shape

- Attributes: `Public ||| Static`. `InitOnly` (readonly) when the source binding
  is immutable (the common case); a `mutable` module value drops `InitOnly`.
- Name: the **source name** verbatim (`empty`, `seed`). No mangling — module
  values do not arity-overload.
- Signature: `provider.FieldSignature ty` over the binding's frozen type. Module
  values are **never generic** (a module `let` has no type parameters of its
  own; any free type variable would have been a generalisation error at the
  module level), so the field is always a plain `Def` token — no self-`TypeSpec`
  `MemberRef` dance (contrast the generic-class `static let` of G13).

### 2.4 Initialiser — the holder `.cctor`

Each holder that owns ≥1 module value gets one synthesised `.cctor`:

- It evaluates each owned module value's init expression **in source
  declaration order** and `stsfld`s the result into the field — structurally the
  same recipe as the class `static let` cctor (`Emit.buildStaticCctor`).
- Init expressions resolve their own references through the normal emit
  environment: a forward/back reference to *another* module value is an
  `ldsfld` (via the module-value table, §3), a call to a sibling static fn is a
  `call`, a closure construction goes through `closureByNode`.
- The holder type **drops `BeforeFieldInit`** when its `.cctor` is present, so
  the runtime runs the initialiser deterministically before the first access to
  any of the holder's members (F# module-init semantics). A holder with no
  module values keeps `BeforeFieldInit` and emits no `.cctor` (unchanged).

### 2.5 Top-level `do` expressions & `Main` (scope boundary for v1)

F# folds module `do`-statements into the module `.cctor`, interleaved with value
inits in source order; for an executable the *last* module's do-statements form
the entry point.

**v1 keeps the current `Main` behaviour for `do`-expressions** (an executable
runs its top-level `TDecl.Expression`s in `Main`, in source order). Module-value
`.cctor`s run before `Main` (on first holder access), so for the common shape —
pure value inits, then `do` code that reads them — order is preserved. The
**known limitation**: a side-effecting top-level `do` that must run *before* a
module value's initialiser observes its effect would be reordered (the value
init is hoisted into the `.cctor`). This is rare and is called out here rather
than solved; revisit if a real case appears. A Library still drops top-level
`do`-expressions (unchanged) — only value **bindings** gain storage.

---

## 3. Reference lowering

All module-value references stay `TExpr.Var(bindingKey)` through the front end
(no new TAST node, no holder `SymbolKey` threaded into the frozen tree). They are
resolved **uniformly at emit** through a new codegen table:

```
EmitContext.ModuleValues : Dictionary<NodeKey, EntityHandle>   // binding key → static field
```

- Threaded into every `EmitEnv` (Main, static methods, closures, type-member
  bodies, secondary ctors, class `.cctor`s, the new holder `.cctor`s) via
  `EmitContext`, so it is shared by *all* body builders — one table fixes every
  context at once.
- `EmitExpr.buildVarLoad` gains a final arm: after args / self / captures /
  slots, if the key is in `env.ModuleValues`, emit `ldsfld <handle>`. (A module
  value is never assigned after init from user code in v1; a `mutable` module
  value's `<-` would lower through the existing field-set path — out of scope
  until needed.)

Choosing the codegen table over a front-end `Var → StaticFieldGet` rewrite keeps
the change localised to the backend and avoids inventing a holder `SymbolKey`
surface for `resolveStaticField` (holders are not in `env.Classes`).

---

## 4. Interaction with closure / static-fn analysis

Because a module value is now real static storage, it must be **invisible to the
free-variable/capture machinery**:

- `collectStaticFns` rule 2 must **not** count a module-value reference as a
  captured module-level local. A function whose only "captures" are module
  values is now a genuine **static method** (`get () = seed` → `static get()`
  with `ldsfld seed`), not a closure.
- `discoverClosures` must **not** capture module values into closure fields — a
  reference is an `ldsfld`, available without a `this`.

Mechanism: compute the **plain module-value key set** up front (top-level
NamedSimple non-inline lets with no lambda parameters — identifiable without
`staticFnKeys`) and pass it as "externally resolved" into `freeVarKeys` /
`collectStaticFns` / `discoverClosures` so those keys are treated as bound (never
free). This breaks the cycle: plain values are classified first, which lets
functions over them promote to static methods, which finalises `staticFnKeys`,
which (with the inline filter) defines the full module-value set for field
emission. Escaping function-values (the rare second case) are handled after
`staticFnKeys` is known.

---

## 5. Metadata row ordering

Holders and their fields/methods are emitted **last** (after interfaces, unions,
records, classes, closures), so module-value rows are the trailing field/method
rows — consistent with today's `holderFirstField = fieldCount + 1`. The
delicate parts:

1. **Field handles are *predicted*** (like type-defs and static-method handles
   already are). The module-value field base = (all type fields) + (all closure
   capture fields), both known in the constructor. Each module value gets a
   predicted `FieldDefinitionHandle`, the `ModuleValues` table is populated with
   predicted handles **before any body is built** (member bodies in
   `NominalEmit` run before holder emission and may reference module values), and
   the actual `FieldDefinition` rows are added — in the predicted order, grouped
   by holder — during holder emission.

2. **A holder's `.cctor` lives inside that holder's contiguous method range.**
   Method handles are predicted from counts (`staticBase + 1 + i`). The
   prediction is extended so each module-value-bearing holder contributes **one
   `.cctor` method** immediately before its static-fn group (and the Program
   holder's `.cctor` before its holderless fns). The emission order becomes:

   ```
   methods: interface → union/record/class → closure
            → [ per named holder: (.cctor?) ++ static-fns ]
            → [ Program holder: (.cctor?) ++ holderless-fns ++ Main ]
   fields:  type fields → closure captures
            → [ per named holder: module-value fields ] → [ Program: module-value fields ]
   ```

   A holder's `FirstField`/`FirstMethod` point at its first module-value field /
   its `.cctor` (or first static fn, when it has no values). Building the full
   ordered method-emission plan up front (cctor vs static-fn entries) makes
   handle prediction a positional lookup, avoiding ad-hoc offset arithmetic.

---

## 6. Guarantees (invariant summary)

- **G-MV-1.** Every module value is a `public static` field on its declaring
  module's holder type, named exactly as in source.
- **G-MV-2.** Each module value is initialised **exactly once**, in source order,
  by its holder's `.cctor`; no per-access init check is emitted.
- **G-MV-3.** Every reference — from any method/ctor/cctor in the assembly —
  lowers to `ldsfld` of that field. References are never closure captures or
  `Main` locals.
- **G-MV-4.** Module values are non-generic: always a plain field `Def` token.
- **G-MV-5.** A holder owning module values is **not** `BeforeFieldInit`; its
  `.cctor` runs before first member access.
- **G-MV-6 (Exe).** Top-level `do`-expressions still run in `Main`, after the
  holder `.cctor`s. (Known reordering limitation, §2.5.)

---

## 7. Implementation steps

1. `EmitContext` / `EmitEnv`: add `ModuleValues`; thread through every env
   constructor (`Emit.fs`, `NominalEmit`, `EmitClosures`).
2. `EmitExpr.buildVarLoad`: add the `ModuleValues` → `ldsfld` arm.
3. Exclude plain module-value keys from `freeVarKeys` / `collectStaticFns` /
   `discoverClosures` (§4).
4. `Emit.buildMain`: stop allocating a `Main` local for a module value (it is a
   static field now); its reference resolves via `ModuleValues`.
5. Assembler constructor: classify module values, predict field handles, populate
   `ModuleValues`; build the holder method-emission plan (cctor + fns) and extend
   handle prediction.
6. Holder emission (`EmitStaticMethods` / a dedicated pass + `Finalise`): add the
   module-value `FieldDefinition` rows per holder, build each holder `.cctor`
   (`buildStaticCctor`-style over the module values), set holder `FirstField` /
   `FirstMethod`, and drop `BeforeFieldInit` where a `.cctor` exists.
7. Gates: a Library and an Exe each (a) a class member, (b) a `static let`, and
   (c) a sibling module fn reading a module value; the generic `static let`
   `set.fs` shape; a module value reading an earlier module value. Then flip the
   `PackageBuildTriage` `ptest "Vesper.Set builds BCL-only"` and chase the next
   wall.

---

## 8. Cross-references

- `Emit.buildMain` / `Emit.buildStaticCctor` — the existing local-binding and
  class-`static let` cctor recipes this mirrors.
- `EmitClosures.collectStaticFns` / `discoverClosures` — the capture analysis
  §4 modifies.
- `Assembler` constructor + `Finalise` — the row-prediction / holder-emission
  machinery §5 extends.
