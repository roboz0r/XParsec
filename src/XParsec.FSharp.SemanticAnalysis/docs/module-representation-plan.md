# Module-value representation & lowering plan

**Status: v1 landed (2026-06-09).** The §7 steps are implemented for the
**ground-typed, named-module-holder** case (the `set.fs` `SetTree.empty` shape):
module values emit as `public static` fields on their module holder, initialised
by the holder's `.cctor`, referenced via `ldsfld` (`EmitContext.ModuleValues`),
and excluded from capture analysis. **Generic values landed
(2026-06-09)** — see §9 below. **Deferred** (each keeps its current treatment, no
regression): function-values stored as values and
values whose type never grounded (`FTUnknown` — skipped). Anonymous top-level
"Program"-holder values (§2.5 `do`/`Main` ordering) **landed in §10 (Stages 1–3,
2026-06-16)** — ground leading/trailing values and generic values all have real
storage now. Gates: `ClassTests.fs` `ClassStatic` "… module
value …" rows + "a generic module value lowers to a generic method …" + the
"§10…" rows.

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

---

## 10. Top-level ("Program"-module) values — full self-host (planned)

**Status: Stages 1–3 landed (2026-06-16).** Closes the §intro "Deferred: anonymous top-level 'Program'-holder
values" item and the PP7c-5 route-around
("a member referencing a *top-level* module `let` VALUE fails — `Emit: no binding for variable`"). The §7 (v1) work
gave **named-module-holder** values real storage; this section gives the same to
**top-level** values, preferring the §7 `.cctor` + `static initonly` shape (which
the JIT folds far better than fsc's mutable-static + property-getter shape, §10.2)
**where ordering permits**, and falling back to a `Main`-written `static` field
where it does not.

**What landed (Stage 1, gated by `ClassTests` `ClassStatic` "§10: …" rows):**
a **leading** top-level value (declared before any top-level statement) is a
`public static initonly` field on the Program holder, initialised by its `.cctor`
in declaration order; a member / sibling reads it as `ldsfld`. This removes the
PP7c-5 route-around. Three facts the implementation surfaced, each refining the
plan below:

- **The trailing case now lands (Stage 2).** A value *after* a top-level statement
  is **not** a standalone decl — the parser folds consecutive top-level
  statements/lets into one `TDecl.Expression` *sequential*, so the value was a
  nested `TExpr.Let` the collector never saw (it stayed a `Main` local). Stage 2
  added a **top-level statement-spine flattener** in `EmitLower.lower`
  (`flattenTopLevel`): a `TDecl.Expression` whose expr is a `Sequential` / `let … in
  …` chain is peeled into standalone `TDecl.Let` + `TDecl.Expression` decls in
  source order (only the outermost spine — sub-expressions are left intact, so a
  genuinely-local nested `let` is untouched). A flattened trailing value then flows
  into the previously-dormant `ProgramMainValues` / `MainInitValues` / mutable-field
  machinery (§10.5–6) and is written by `Main` via `stsfld`. Gate:
  `ClassTests` `ClassStatic` "§10.3: a top-level value after a statement is a
  Main-written mutable static field (trailing)".
- **The §4 promotion is now in effect for top-level values too.** Because a leading
  value is a static *field*, a function whose only "capture" is that value captures
  nothing and lowers to a **static method**, not a closure (consistent with §7's
  named-module values). Closure-synthesis tests therefore must capture a genuine
  local (a curried function's parameter), not a top-level value.
- **Ref-struct / byref values are excluded.** A `[<Struct; IsByRefLike>]` (or `&`)
  top-level value cannot be a static field (the CLR confines it to the stack) and
  never needs to be (a ref struct can't be read from a member / cctor), so it stays
  a `Main` local. Detected by the `(ns, name)` of every assembly-declared ref-struct
  type (computed from `tast.Decls`, since `lower` strips type decls).

### 10.1 Where a top-level value can legally occur (FS0222)

A "top-level" binding is a `let` with **no** enclosing `namespace` / `module`
declaration. F# permits these in **exactly one place**: the **last source file of
an executable** (the implicit-`Main` "Program" module). Verified empirically
(`tmp/`, `dotnet build`):

- **Library** (or *any* non-last file of a multi-file compile) led by a top-level
  `let` → **`error FS0222`**: *"Files in libraries or multiple-file applications
  must begin with a namespace or module declaration … Only the last source file of
  an application may omit such a declaration."*
- **Last file of an exe** with `let a = …` / `do …` / `let b = …` / a `type` whose
  member reads `a` → **compiles**.

So there is **no library case** — an earlier draft's "library top-level value"
branch was invalid F# and is removed. The only case to support is the
implicit-`Program` module of an executable; a non-last exe file (and every library
file) carries a `module`/`namespace` header, so its values already take the §7
named-holder path.

### 10.2 Why `.cctor` + `static initonly`, not fsc's shape

fsc lowers a top-level value to a **mutable** `assembly static` field on a
`<StartupCode$asm>.$Program` storage class, written by the entry point `main@`
(`stsfld`), and read through a `get_a()` **property getter** on a public `Program`
class (verified via `ilspycmd` in `tmp/`). That shape is deliberately
library-consumption-friendly (an `init@` guard runs module init on demand) but is
the **less optimizable** one: a mutable static defeats the JIT's readonly-static
constant-folding / hoisting, and every read pays a (potentially un-inlined) getter
call.

We instead reuse §7's named-holder shape **where ordering permits**: a
`public static initonly` field on the Program holder, written **once** in the
holder `.cctor`, read as a direct `ldsfld`. The CLR treats an `initonly` static as
a hoistable constant after the cctor runs. The cost is the ordering constraint in
§10.3.

### 10.3 The ordering constraint and the partition

A Program `.cctor` runs at first access to the Program type; the exe entry point
`Main` **is** a Program member, so the `.cctor` runs immediately **before** `Main`'s
first instruction. Hoisting a value init into the `.cctor` therefore moves it ahead
of every top-level `do` statement (which runs in `Main`) — a reorder iff a `do`
**precedes** that value in source.

**Partition the top-level decls at the first top-level statement**
(`TDecl.Expression`, i.e. a `do`):

- A ground value in the **leading prefix** (no `do` before it) → `.cctor` +
  `static initonly`. Its source position is already before every `do`, so running
  it in the pre-`Main` `.cctor` is **observationally identical** to source order.
- A ground value **after** the first `do` → written in `Main` via `stsfld`, a plain
  **mutable** `static` field (an `initonly` field cannot be written outside a
  `.cctor`). `Main` runs it in source order, interleaved with the `do`s — no
  reorder.
- All `do`s and residue (`FTUnknown` / destructuring `let`s) stay in `Main`,
  unchanged.

The partition is monotonic (a value after a `do` can only be followed by more
`do`s / trailing values), needs **no purity analysis**, and is always
source-correct. In the dominant "definitions first, execution last" shape *every*
value is leading → *every* field is `initonly` in the `.cctor`.

**Placement is a per-value *policy*, not a structural assumption.** "Leading
prefix" is v1's **cheap, sound** policy — but it is conservative: a value *after*
a `do` whose initialiser provably neither observes nor perturbs the intervening
statements' effects could *also* be lifted to the `.cctor` (gaining `initonly`),
and a side-effecting *leading* value could in principle be ordering-sensitive in
ways a richer model would keep in `Main`. Establishing that needs **side-effect /
data-dependence analysis over the program's semantic graph** — explicitly **not a
v1 target**. The representation is therefore designed so the leading/trailing call
is an **independent per-value decision** (`Placement = Cctor | MainInit`), not a
single syntactic cut index: `ProgramCctorValues` / `ProgramMainValues` (§10.6) are
two free lists into which *any* sound assignment can drop a value. v1 computes that
assignment by the first-`do` partition; a future pass may compute it from the
effect graph **without changing the lowering, field, or `Main`/`.cctor` machinery
below** — only the classifier that fills the two lists changes. The one hard
invariant a future policy must preserve: a `Cctor`-placed value's initialiser must
be safe to run before *every* `Main` statement (that is what `initonly` + the
pre-`Main` cctor encode).

### 10.4 Concrete lowering

```fsharp
// (implicit Program module = project.ModuleName; last file of an exe)
let provider = System.Globalization.CultureInfo.InvariantCulture   // leading  → .cctor, initonly
let tag      = "v"                                                 // leading  → .cctor, initonly
do System.Console.WriteLine "starting"                             // statement → Main
let stamp    = System.DateTime.UtcNow.Ticks                        // after a do → Main, mutable static

type Printer() =
    member _.Render(x: int) : string =
        tag + x.ToString(provider)                                 // member reads leading values
```

Lowered — the Program holder owns all three value fields (two `initonly` from the
`.cctor`, one mutable written by `Main`); references are `ldsfld`:

```
.class Program
  .field public static initonly class …CultureInfo provider     // leading  → .cctor
  .field public static initonly string             tag          // leading  → .cctor
  .field public static          int64              stamp        // trailing → Main (mutable)

  .method private specialname rtspecialname static void .cctor() {      // leading prefix only
    call …CultureInfo::get_InvariantCulture();  stsfld Program::provider
    ldstr "v";                                   stsfld Program::tag
    ret
  }
  .method static int32 Main(string[] args) {     // do + trailing value, in source order
    ldstr "starting"; call …Console::WriteLine(string)
    call …DateTime::get_UtcNow(); … get_Ticks;   stsfld Program::stamp   // trailing → stsfld
    ldc.i4.0; ret
  }
.class Printer
  .method instance string Render(int32 x) {
    ldsfld Program::tag … ldsfld Program::provider …             // ModuleValues table → ldsfld
    ret
  }
```

The Program holder now owns a `.cctor`, so it **drops `beforefieldinit`** (G-MV-5):
the cctor runs before first access to any Program member — including `Main`. A
Program holder with *no* leading values keeps `beforefieldinit` and emits no
`.cctor` (its values, if any, are all `Main`-written).

### 10.5 Generic top-level values

**Status: landed (2026-06-16, Stage 3).** A *generic* top-level value
(`let empty : 'T list = []`) cannot be a static *field* (a non-generic holder has
no type parameter to type it), so — exactly as §9 — it lowers to a **zero-arg
generic static method** on the Program holder (a "generic property" on the module's
static class), `call`ed at the use-site instantiation. Position-independent (a
method computes on demand, so the §10.3 partition does not apply). The *alternative*
outcome — a **non-generalisable** generic value — never reaches codegen: the front
end's value restriction (`InferGeneralize.shouldGeneralise` keeps an expansive
parameterless binding monomorphic; `Validation.checkValueRestriction` errors a
mutable one) leaves such a binding either ground or with an `FTUnknown` the collector
rejects.

`collectGenericModuleValues` already classified these for a *named* holder (§9); Stage
3 made it also classify a **holderless** (top-level) generic value, giving it
`Holder = None` (the `StaticFn.Holder` option — `None` ⇒ the Program holder) and a
name from `TopLevelNames` (synthetic `value@<offset>` for a flattened nested trailing
value), so it joins `HolderlessFns` and emits as a generic static method on Program.
Its key joins `StaticFnKeys` (the value-position `call` at the reference site, and the
§4 capture exclusion, already handle it uniformly). Gate: `ClassTests` `ClassStatic`
"§10.5: a generic top-level value is a generic static method on Program (not a
field)".

### 10.6 Mechanism — the change set

1. **Front-end name (`Elaborate.translateModuleElem`).** A top-level binding records
   no `ModuleMemberInfo`, so the backend has no field *name*. Add an additive
   `TastFile.TopLevelNames : Map<NodeKey, string>` (binding-key → source name via the
   existing `memberNameOfBinding`), recorded for every top-level binding; consulted
   **only** by the value collector, so top-level **functions** keep their
   `fn$<off>` holderless path untouched. *(References are by `NodeKey`, so the name
   is cosmetic; a synthetic `<name>@<off>` à la fsc would also work and skip this
   touch — the table is the nicer-metadata option.)*
2. **Collectors (`EmitClosures`).** Thread the program holder key
   `(None, project.ModuleName)` + `TopLevelNames`. On the `moduleMembers` **miss**,
   a non-`inline` non-`Lambda` ground top-level value becomes a Program value, and a
   **placement classifier** stamps it `Cctor` or `MainInit`. v1's classifier is the
   §10.3 first-`do` partition (a single index over the decl list); it is the **only**
   place the leading/trailing decision is made, so a future effect-graph policy
   (the §10.3 forward-compat note) is a **drop-in replacement of this function**
   alone — everything downstream keys off the resulting `Placement`, never re-derives
   it from source position. `collectGenericModuleValues`' holderless miss → a
   Program-holder `StaticFn` (§10.5, placement-independent).
3. **`HolderPlan`.** Add `ProgramCctorValues` (leading → `.cctor`, `initonly`) and
   `ProgramMainValues` (trailing → `Main`, mutable). Both are **out** of
   `OrderedNamedHolders` / `ValuesByHolder` but **appended to** `ModuleValueFieldOrder`
   in declaration order (trailing rows — the Program slot is the last type, §5), so
   their field handles are predicted and `moduleValueFields` is populated. When
   `ProgramCctorValues` is non-empty, insert a single `MethodSlot.HolderCctor`
   for the program key into `MethodPlan` immediately **before** `HolderlessFns`
   (reuses `Assembler.prepareHolderCctor` / `Emit.buildStaticCctor` verbatim — a
   `.cctor` is just a `(field, init)`-store sequence in declaration order, exactly
   what a named holder's cctor already is; this holds for *any* `Cctor`-placed set,
   not only a contiguous leading prefix, so a future policy reuses it unchanged).
4. **`Layout.build`.** The `programSlots` slot gains
   `FieldCount = ProgramCctorValues + ProgramMainValues`; field rows appended to
   `layout.Fields` (trailing), each `Public ||| Static`, **plus `InitOnly`** for a
   leading value only. `MethodCount += (ProgramCctorValues non-empty ? 1 : 0)` for
   the `.cctor`; `TypeSlotKind.Program` carries `hasCctor` so `Assembler` passes
   `not hasCctor` for `beforefieldinit`.
5. **`Emit.buildMain` + `EmitContext`.** `ModuleValues` (the `ldsfld` table) covers
   **all** field-backed values (named-holder + both Program kinds). Add a new
   `MainInitValues : Dictionary<NodeKey, EntityHandle>` = the **trailing** Program
   values. `buildMain`'s top-level `TDecl.Let` arm: key in `MainInitValues` →
   `buildExpr value; stsfld handle`; key in `ModuleValues` (cctor-initialised:
   named-holder or leading-Program) → skip; else → `Main` local. `buildVarLoad`
   resolves every field key to `ldsfld`, unchanged.
6. **`Assembler`.** `moduleValueFields` already populates from
   `plan.ModuleValueFieldOrder`; populate `MainInitValues` from the same handles for
   the trailing subset. The program `.cctor` prepares through the existing
   `prepareHolderCctor` (now also keyed by the program holder). No bespoke body
   builder.

The prefix-sum handle prediction (`Layout.deriveHandles`) is untouched — the
Program slot is last, so its fields are the trailing field rows and its `.cctor`
(when present) sits at a fixed offset just before the holderless fns.

### 10.7 Guarantees (extends §6)

- **G-MV-7 (landed).** A top-level ground value in the leading prefix (no top-level
  statement before it) is a `public static initonly` field on the Program holder
  (`project.ModuleName`), initialised once by the Program `.cctor` in declaration
  order; every reference is an `ldsfld`. Symmetric with §7's named-holder values.
  Excludes a `[<Struct; IsByRefLike>]` / `&` value (stays a `Main` local — a
  byref-like type cannot be a static field).
- **G-MV-8 (landed — Stage 2).** A top-level ground value following a top-level
  statement is a `public static` (mutable, NOT `initonly`) field, written by `Main`
  via `stsfld` in source order; every reference an `ldsfld`, no reorder. The parser
  folds it into the preceding statement's sequential; `EmitLower.flattenTopLevel`
  peels the top-level statement spine back into standalone decls so the collector
  sees it (the init side effect runs in `Main`, not the pre-`Main` `.cctor`).
- **G-MV-9 (landed — Stage 3).** A top-level *generic* value is a zero-arg generic
  static method on the Program holder (§9 / §10.5), `call`ed at the use-site
  instantiation; never a field, never a `Main` local. A non-generalisable generic
  value is a front-end value-restriction case, so it never reaches this path.
- **G-MV-10.** The G-MV-6 source-order property holds verbatim: leading values run
  in the `.cctor` (before `Main`, matching their pre-`do` source position); every
  other top-level decl runs in `Main` in source order. Top-level values exist only
  in an exe's last file (FS0222), so there is no library case and the Program
  `.cctor` runs at most once, before `Main`.

### 10.8 Staging

1. **Name + collector partition + leading-prefix `.cctor` + trailing `stsfld`.**
   **Landed.** Gate (an exe, `ClassTests` `ClassStatic`): (a) a member reads a
   leading top-level value via a Program-holder `initonly` field (the PP7c-5
   closure), and (b) two leading values where the second reads the first, both
   `initonly`, `.cctor`-initialised in order. The probe's inline
   `CultureInfo.InvariantCulture` can now become a leading `let provider`.
   *Scope as built:* leading values only; the byref/ref-struct exclusion; the §4
   promotion of top-level-value-capturing functions to static methods (closure
   tests rewritten to capture a genuine local).
2. **Flatten top-level sequentials → decls**, so a value after a statement is a
   standalone `TDecl.Let` the collector sees. **Landed.** `EmitLower.flattenTopLevel`
   peels the top-level statement spine (`Sequential` items + `let … in` continuations,
   outermost only) into standalone decls, *activating* the already-built trailing path
   (`ProgramMainValues` → a `Main` `stsfld` to a mutable `static` field). Gate
   (`ClassTests` `ClassStatic` "§10.3: …trailing…"): the leading `initonly` vs
   trailing mutable partition with source-order side effects (`r`'s init runs in
   `Main` after the leading value prints, not in the pre-`Main` `.cctor`). A nested
   trailing value carries no recorded source name → field-named `value@<offset>`
   (fsc-consistent; references are by `NodeKey`). **Not yet covered:** a *member*
   reading a trailing value needs `type`-after-statement, which the parser currently
   folds into the sequential (`CstKeys.firstTokenOfExpr: TODO SkipsTokens` on the
   `type` token) — an orthogonal parser gap, not §10.
3. **Generic top-level values** (the §10.5 holderless fallback). **Landed.**
   `collectGenericModuleValues` now classifies a holderless (top-level) generic value
   as a `Holder = None` `StaticFn` (name from `TopLevelNames`), so it emits as a
   zero-arg generic static method on the Program holder, `call`ed at the use-site
   instantiation. A non-generalisable generic value is a front-end value-restriction
   case and never reaches codegen. Gate: `ClassTests` `ClassStatic` "§10.5: a generic
   top-level value is a generic static method on Program (not a field)". (Before this,
   such a value became a `Main` local with a method-axis typar → `BadImageFormatException`.)

---
