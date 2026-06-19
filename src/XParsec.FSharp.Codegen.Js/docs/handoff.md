# Handoff: JS classes, interfaces, custom equality & comparison

**Branch:** `eq-comp-attributes` (off `main`/`codegen-js` lineage). **Status at handoff:**
the attribute feature is COMPLETE through the CLR backend (Phases 1–5, committed);
the JS backend cannot yet dispatch custom equality/comparison because it emits no
classes at all and its provider lacks the relevant interfaces. This document is the
plan to finish JS.

> NOTE: this is an EPHEMERAL plan doc (per repo convention `src/**/docs/*-plan`-style
> files are deleted once the work lands). Delete it — and any comment references to
> it — when JS classes/interfaces/custom-eq are done.

---

## 1. What is already done (do NOT redo)

Committed on `eq-comp-attributes` (5 commits, `git log d1798fb..`):

- **Recognition** — `[<CustomEquality>]`/`[<CustomComparison>]` decode to
  `EqualityVerdict.Custom`/`ComparisonVerdict.Custom`
  (`SemanticInfo.fs`, `Passes/Attributes.fs`). Classes carry
  `ClassTypeInfo.EqualitySupport`/`ComparisonSupport` (struct⇒Structural default,
  reference class⇒Reference).
- **Validation** — `validateEqCompAttributes` (`Passes/Attributes.fs`) emits
  **FS0382** (type-kind legality: structural family only on
  record/union/exception/struct; ReferenceEquality bars struct; Custom\* bar
  interface; No\* legal anywhere) and **FS0377** (invalid mix). Wired at every
  registration site. `validateCustomEqCompImpls` (`Passes/Unification.fs:~1244`)
  emits **FS0378** (a `Custom` class must implement `IEquatable<Self>`/
  `IComparable<Self>`) and **FS0379** (CustomComparison requires CustomEquality);
  records/unions with `Custom*` get a scope error (wrap in a class).
- **Front-end constraint gate** — `Engine.fs checkConstraint` (~1195–1245):
  record/union/class arms treat `Custom` equality/comparison use sites as
  `Satisfied`; the class arm honours the verdict via `TypeRegistry.tryClassByKey`
  (NoEquality⇒Violated, Reference/Custom⇒Satisfied, struct Structural⇒field-walk
  `InstanceFields`).
- **CLR backend** — NO code change was needed and custom dispatch is **verified by
  executing IL**. The SRTP lowering of `=`/`<` on a nominal `^T` already calls
  `EqualityComparer<T>.Default.Equals` / `Comparer<T>.Default.Compare`
  (`Vesper.Core/ops-platform.fs:114`, `Vesper.Comparison/comparison.fs:40`), which
  at runtime dispatch to the type's `IEquatable<T>`/`IComparable<T>`. A `Custom`
  class emits no synthesized triple/pair (gated on `Structural` in
  `Codegen.Clr/NominalEmit.fs:804/846`) and its interface impls land as
  InterfaceImpl rows. **Test oracle to mirror on JS:**
  `test/XParsec.FSharp.Codegen.Clr.Tests/CustomEqualityComparisonDispatchTests.fs`
  (id-only `Equals`, inverted `CompareTo`, distinguishable from
  reference/structural).

All 1000 `XParsec.FSharp.Codegen.Clr.Tests` pass.

---

## 2. Goal of the remaining (JS) work

Make `a = b` / `a < b` / `hash a` on a `[<CustomEquality>]`/`[<CustomComparison>]`
**class** dispatch to the USER's implementation on the JS backend — at parity with
the CLR backend. This requires building **general class emission on the JS
backend**, of which the custom-eq dispatch slot is the small final consumer.

---

## 3. JS architecture facts (verified, with anchors)

**The runtime dispatch hooks ALREADY EXIST** — no `.mjs` regen needed:
- `src/Vesper.Core/Vesper.Core.mjs`: `eq(a,b)` → `if (typeof a.Equals === "function"
  && typeof b.Equals === "function") return a.Equals(b);` (~L56); `hashOf(x)` →
  `if (typeof x.GetHashCode === "function") return x.GetHashCode();` (~L106).
- `src/Vesper.Comparison/Vesper.Comparison.mjs`: `cmp(a,b)` → `if (typeof
  a.CompareTo === "function" && typeof b.CompareTo === "function") return
  cmpSign(a.CompareTo(b));` (~L46).
- Saturated `=`/`hash`/`compare` collapse to `structuralEquals`/`structuralHash`/
  `structuralCompare`, which call `eq`/`hashOf`/`cmp`. **INVARIANT:** structural
  types must stay slot-less (no `.Equals`/`.CompareTo`/`.GetHashCode` instance
  method) so they stay on the structural walk. Only `Custom` types get a slot.

**The TAST carries everything needed:**
- `TTypeKindG.Class of TClassG` (`Tast.fs:549`, payload `:553`). Members split:
  - `TClassG.Members : EqArray<TTypeMember>` — regular members AND
    `override this.Equals(o)` / `override this.GetHashCode()`.
  - `TClassG.Interfaces : EqArray<'ty * EqArray<TTypeMember>>` (`Tast.fs:559`) —
    one entry per `interface IFace with …` block; `IEquatable<Self>.Equals` /
    `IComparable<Self>.CompareTo` bodies live HERE.

**The two GAPS:**

1. **JS provider lacks the interfaces** (`src/XParsec.FSharp.Codegen.Js/JsNativeSymbols.fs`).
   It is BCL-free; `types : Map<string, ExternalTypeShape>` holds only `"Error"`
   (`errorShape`, an `ExternalTypeShape.Class` with `IsInterface=false`, L67–80).
   With no `System.IEquatable<'T>`/`System.IComparable<'T>`, the user's
   `interface System.IEquatable<Self>` resolves to an unbound `TyVar`, the
   "is not an interface" check fires (`Unification.fs:860`), and the Phase-3
   FS0378 gate then reports "must implement IEquatable" (`Unification.fs:~1285`).
   These interfaces are ERASED at runtime on JS (the runtime duck-types
   `.Equals`/`.CompareTo` presence), so they need to exist as PROVIDER METADATA
   only — likely no `.mjs` runtime artifact.

2. **The JS backend emits NO classes** (`src/XParsec.FSharp.Codegen.Js/EmitJs.fs`).
   `collectTypes` (~L1260–1288) matches only `TTypeKindG.Record`/`Union`;
   `TTypeKindG.Class` falls to `| _ -> ()` (L1287) — no class decl, members never
   gathered. `JsAst.Class` (`JsAst.fs:103`) is record-shaped:
   `Class of name: string * fields: string list * export: bool` — no
   members/methods/base/interfaces. Its printer is
   `JsPrint.fs:254` (`classDecl export name None [ ctorDecl fields [] fields ]`).

**Member emission convention (USER-STATED DESIGN — follow it):**
- **Regular methods → free, receiver-first curried functions** (`<Type>__<member>`),
  for tree-shaking. This is the existing `emitMemberFn` (`EmitJs.fs:1162`),
  `Members.mangledName` (`EmitJs.fs:237`: `Type__member` instance,
  `Type__get_Prop` getter, `Type_member` static). Call shape:
  `<Type>__<member>(receiver)(args…)`.
- **Interface-implementation members → ATTACHED instance members** (prototype/class
  methods on the emitted object), because the runtime dispatches by method
  presence on the instance. This is the NEW behaviour to add.

---

## 4. Plan

### Step A — JS provider interfaces (unblocks the front-end)
1. Study `JsNativeSymbols.fs` (`errorShape`/`types` pattern) and the
   `ExternalTypeShape` DU — how an interface shape is represented (`IsInterface=true`,
   generic arity, `FrozenInterfaces`). Compare with how the CLR
   `MetadataLoadContext` provider surfaces `System.IEquatable\`1`/`System.IComparable\`1`
   so the resolved nominal head matches what `validateCustomEqCompImpls` checks
   (`SymbolKeyOps.qualifiedName = "System.IEquatable\`1"` / `"System.IComparable\`1"`).
2. Add `System.IEquatable<'T>` and `System.IComparable<'T>` as interface shapes to
   the JS provider so `interface System.IEquatable<Self>` resolves and the
   "is not an interface" (`Unification.fs:860`) + FS0378 gate pass for a JS-target
   custom-eq class. They carry the single `Equals`/`CompareTo` member signature.
3. CHECKPOINT: a `[<CustomEquality>]` class should now reach codegen (it will emit
   nothing yet — that's Step B). Probe with `emitJs` over a tiny source and assert
   no front-end diagnostics.

### Step B — General JS class emission
1. Extend the JS AST: give `JsAst.Class` (or a new `JsStatement.ClassDecl`) the
   ability to carry attached instance methods (and a base/`extends` slot if you take
   inheritance — see §5). Minimal shape: `name`, ctor `fields`, `methods`
   (each: name + params + body), `export`. Keep the existing record `Class` working
   (it's just a class with no methods).
2. Update `JsPrint.fs` (the `classDecl`/`ctorDecl` helpers at/around L254) to print
   the constructor + attached method definitions.
3. Update `EmitJs.fs collectTypes` to add a `TTypeKindG.Class` arm:
   - Emit the class decl (positional ctor storing each field, like records).
   - Add `TClassG.Members` regular members to `memberDefs` as FREE functions
     (existing `addMembers`/`emitMemberFn` path) — tree-shaking convention.
   - Emit `TClassG.Interfaces` members as ATTACHED instance methods on the class,
     named by the member name (`Equals`, `CompareTo`, …).
   - Decide whether `override Equals`/`override GetHashCode` (which live in
     `Members`, not `Interfaces`) ALSO attach — see §5.
4. Wire member-call lowering so call SITES still resolve correctly (the walker's
   `Members` module ~L232–300): regular member calls keep resolving to the free
   `<Type>__<member>` function; attached interface methods are primarily for runtime
   dispatch. Make sure a direct `obj.Equals(x)` call (if expressible) doesn't
   double-resolve. Add class instantiation lowering (`new T(args)` → `new T(args)`)
   if not already covered by the union/record ctor path.

### Step C — Custom dispatch slot (mostly falls out of Step B)
Because interface-impl members attach as instance methods named `Equals`/`CompareTo`,
and the runtime hooks call `a.Equals(b)`/`a.CompareTo(b)`, **dispatch works
automatically once Step B attaches them** — for a `Custom` class the `IEquatable`
impl IS the `.Equals` slot. Remaining specifics:
- `GetHashCode`: attach the user's `override GetHashCode` so `hashOf` finds it. See
  §5 for the no-user-hash case.
- GATE the attachment so ONLY `Custom` classes get the slot (structural/reference
  classes must stay slot-less — runtime invariant). Equivalently: only attach the
  equality/comparison-relevant methods when `EqualitySupport=Custom` /
  `ComparisonSupport=Custom`. (Other interface impls on a non-custom class are a
  separate question — they can attach too, they just mustn't be named
  `Equals`/`GetHashCode`/`CompareTo` on a structural type, which the validator
  already prevents since those would require Custom.)

### Step D — Round-trip tests (prove by EXECUTION)
- Find the JS execution harness in `test/XParsec.FSharp.Codegen.Js.Tests`
  (`Step6Tests.fs` compiles to `.mjs` and runs node, capturing output).
- Mirror the CLR oracle `CustomEqualityComparisonDispatchTests.fs`:
  - `[<CustomEquality; NoComparison>]` class whose `Equals` compares ONE of two
    fields → custom-equal ≠ structural ≠ reference; assert `a = b` is the custom
    answer through emitted JS.
  - `[<CustomEquality; CustomComparison>]` class whose `CompareTo` inverts order;
    assert `a < b`/`>`/`<=`/`>=` give the inverted answers.
- Also add a basic "class emits and runs on JS" test (plain class with a method,
  instantiation, field access, a free-function member call) since general class
  emission is new and otherwise untested.

---

## 5. Design decisions to settle (call them out in the PR)

1. **Do `override Equals`/`override GetHashCode` (in `Members`) attach?** They must,
   for the runtime hook to find `GetHashCode` and for an `override Equals(obj)` to
   be reachable. Recommended: members that are overrides of `Object` methods AND
   interface-impl members attach; everything else stays free. Confirm `IsOverride`
   is available on `TTypeMember`/the frozen member (it is stamped on
   `TypeMemberInfo.IsOverride`).
2. **GetHashCode default for a custom-eq class with NO user `GetHashCode`.** The
   runtime falls to `hashStructural` only when no `.GetHashCode` method exists. A
   non-structural custom `Equals` + structural hash violates equal⇒same-hash.
   Options: (a) require a user `GetHashCode` (extend the Phase-3 validator), or
   (b) document the limitation. Recommend (a) for soundness, but it widens the
   validator — get user sign-off.
3. **Inheritance / `extends`.** Custom-eq classes in the tests don't need a base.
   Decide whether Step B does general `inherit Base(...)` now or defers it (the
   `TClassG` carries base info). Deferring is fine for custom-eq; note it.
4. **Generic classes on JS.** Decide scope; custom-eq tests can be monomorphic.
5. **`export`/library mode** for emitted classes (records/unions already handle the
   `export` flag — mirror it).

---

## 6. Commands / verification

- Build: `./claude_tools.cmd -Action Build`
- JS tests: `./claude_tools.cmd -Action Test -TestProject "XParsec.FSharp.Codegen.Js.Tests"`
- CLR regression (must stay green): `... -TestProject "XParsec.FSharp.Codegen.Clr.Tests"`
- Semantic-analysis tests: `... -TestProject "XParsec.FSharp.SemanticAnalysis.Tests"`
- If output is truncated, read `claude_tools_output.log`.
- Memory caveat: some `runsSet`/`-Filter` drivers fail spuriously (Printf ALC) —
  prefer `ftest` + whole-project runs over `-Filter`.

## 7. Related memory
- `project_js_interface_impls_attached_members` — the attached-members principle +
  this prerequisite stack.
- `project_js_step8_exn_root`, `project_js_step7_type_members`,
  `project_js_union_layout_compositional` — JS backend layout/provider context.
- `project_js_step6_equality_hashing` — the generic structural runtimes these hooks
  live in.
