# Vesper.Set — gap inventory

The state of the world as of 2026-05-26, the day `src/Vesper.Set/{set.fsi,set.fs}`
landed as **verbatim copies of FSharp.Core's `set.fsi` / `set.fs`** (only the namespace and
`Microsoft.FSharp.*` opens patched). `set.fsi` and `set.fs` parse with zero
recovery diagnostics; nothing compiles end-to-end yet. This doc walks the gaps,
splitting them into (A) edits the `set.fs` source needs to match Vesper idioms
and (B) compiler / codegen enhancements the build pipeline needs.

> **Audience.** Someone planning the next slice toward `Vesper.Set.dll`. The
> ordering inside each section is rough — each entry is independently
> actionable, and most of (A) is gated on (B).

## TL;DR

- `Vesper.Set.dll` exists today only as parse-tested target source. Front-end and
  codegen both have hard stops on **class type declarations** (the whole
  `Set<'T>` plus the internal `SetTree<'T>` / `SetTreeNode<'T>` AVL pair),
  **interface implementation** (`IComparable` / `IEnumerable<'T>` / `ICollection<'T>` /
  `IReadOnlyCollection<'T>` / `IStructuralEquatable`), and **object expressions**
  (the `{ new IEnumerator<_> with … }` shape `SetTree.mkIEnumerator` returns).
  These three are the *structural* blockers — without them no slice of `set.fs`
  compiles.
- Several FSharp.Core idioms (`OptimizedClosures.FSharpFunc<…>.Adapt`,
  `LanguagePrimitives.FastGenericComparer`, `LanguagePrimitives.anyToStringShowingNull`,
  `SR.GetString`, `[<DebuggerTypeProxy>]` / `[<DebuggerDisplay>]` /
  `[<NonSerialized>]` / `[<System.Runtime.Serialization.*>]` attributes) must
  be rewritten or dropped before the file is self-host-compilable, regardless
  of compiler progress. These are **source-side** rewrites; (A) below.
- A handful of *library surfaces* are needed by `set.fs`'s body (and by the
  `set.fsi` contract) before the file links: `Seq.fold` / `Seq.reduce` /
  `Seq.truncate` / `Seq.toArray`, `Array.fold` / `Array.zeroCreate`,
  `List.ofSeq` / `List.toSeq`, `Choice<'T1, 'T2>` + `Choice1Of2` / `Choice2Of2`,
  `IComparer<'T>` use sites, `StringBuilder` (or a `Vesper`-side stringifier
  to replace the four `ToString()` arms). These are **library-side**; (C) below.

## A — `set.fs` source rewrites (Vesper idioms)

Each item is "edit `set.fs` to drop a FSharp.Core dependency it cannot have
inside a self-hosted Vesper.* package." Most are mechanical once the matching
(B)/(C) entry lands.

| Idiom in upstream `set.fs` | Lines | Rewrite |
|---|---|---|
| `OptimizedClosures.FSharpFunc<_,_,_>.Adapt f` + `f.Invoke(a, b)` | 302, 312, 314, 326, 857 | Drop the Adapt layer; call `f a b` directly. Vesper closures don't carry the curried/tupled split FSharp.Core's `OptimizedClosures` patches, so the tupled-invoke optimisation is moot — `Fun<_,_>.Invoke` is the only call. |
| `LanguagePrimitives.FastGenericComparer<'T>` | 800, 816, 870, 871, 882, 1053, 1060, 1110 | Replace with `Comparer<'T>.Default` (the BCL surface `Vesper.Comparison` already routes through — see `project_operator_emission_equality`). The `set.fsi` `when 'T: comparison` constraint stays. |
| `LanguagePrimitives.anyToStringShowingNull h` | 1067, 1070, 1071, 1075, 1076, 1077, 1089, 1090, 1091 | Replace with the Vesper printf surface (`sprintf "%O" h` once printf grows `%O` — already done per `project_printf_p2_implemented`) or a Vesper-side `Object.toString` shim in `Vesper.Core`. |
| `SR.GetString(SR.setContainsNoElements)` / `SR.enumerationNotStarted` / `SR.enumerationAlreadyFinished` | 551, 556, 588, 591 | Inline the English string literally (`"The set contains no elements"`, etc.). The `SR` resource-table indirection is FSharp.Core-specific and not worth porting. |
| `Seq.fold (+) Set<'T>.Empty sets` | 920 | Either depend on a Vesper `Seq.fold` (see C) or rewrite to a struct enumerator loop. |
| `Seq.reduce (fun s1 s2 -> Set.Intersection(s1, s2)) sets` | 923 | Same — replace with explicit enumerator loop. |
| `Seq.truncate 4 x` / `Seq.truncate 1000` | 1064, 1122 | `Seq.truncate` isn't on the planned `Vesper.Seq` surface yet (`brainstorm-seq-module.md`); inline the truncate as a counted enumerator loop. |
| `Seq.toArray` | 1122 | Same as truncate — counted loop into a `'T array`. |
| `Array.fold` / `Array.zeroCreate` | 772, 757 | Need Vesper `Array` module bindings or open-coded loops. |
| `List.ofSeq` / `List.toSeq` | 1064, 1209 | Need Vesper `List.ofSeq` / `List.toSeq` (today's `Vesper.List.fold` is the only module member). Until then, inline. |
| `StringBuilder().Append(...).Append(...).ToString()` | 1068, 1073, 1079, 1093 | Vesper has no `Vesper.Text.StringBuilder` and no `System.Text` shim. Either depend on `System.Text.StringBuilder` (BCL ref — same as `Vesper.List` depending on `System.Exception`), or compose via string concatenation / printf. |
| `invalidArg "s" message` | 551, 556 | `invalidArg` lowers in FSharp.Core to a typed `ArgumentException`; Vesper has only `failwith` (lowered to `System.Exception`, per `Emit.fs:1351`'s hard-coded recipe). Replace with `failwith` until `raise (ArgumentException …)` is supported (B-9). |
| `raise (InvalidOperationException(…))` | 588, 591 | Same gap: `raise` for arbitrary exception types isn't wired (only `failwith` is). Either replace with `failwith` for now, or land (B-9). |
| `[<DebuggerTypeProxy(typedefof<SetDebugView<_>>)>]` / `[<DebuggerDisplay("Count = {Count}")>]` / `[<DebuggerBrowsable(…)>]` | 779, 780, 817, 822, 857, 928, 931, 934 | Drop unless the front-end's attribute decoder recognises them as no-ops. They are .NET debugger hints; cosmetic, not load-bearing. |
| `[<NonSerialized>]` + `[<OnSerializingAttribute>]` + `[<OnDeserializedAttribute>]` + `serializedData` plumbing | 783, 787, 801, 811 | Drop the whole `System.Runtime.Serialization` machinery in v1 — Vesper has no serialization story and `BinaryFormatter`-style serialization is deprecated. The `SetDebugView` proxy class goes with this. |
| `[<CompilerMessage(…, 1204, IsHidden=true)>]` on the `Set` helper type | 1101–1109 | Drop along with the static `Create(ReadOnlySpan<'T>)` block (`#if NETSTANDARD2_1_OR_GREATER`) — it's spec-positively the compiler-only collection-builder dance, parse-included in the snapshot but irrelevant to Vesper's `[1; 2]`-without-builders posture (D6 mirrors). |
| `#if TRACE_SETS_AND_MAPS` instrumentation | 54–95, 826–847 | Drop unconditionally — diagnostic-only and the symbol isn't defined. |
| `[<EqualityConditionalOn>] 'T when 'T: comparison` on `Set<'T>` | 23 (`set.fsi`), 778 (`set.fs`) | Keep — the `comparison` constraint is wired (`constraints-plan.md`) and works against `set.fsi`. `[<EqualityConditionalOn>]` decodes to an `EqualityVerdict.Conditional` posture (C-Attr extension, deferred but the plan exists). |

These edits alone leave `set.fs` with no FSharp.Core references but still
broken because the **class / interface / object-expression / `:?>` machinery is
unsupported in the compiler**. (B) covers that.

## B — Compiler / codegen enhancements

The big-ticket items, in the order they need to land. Each entry is sized
roughly: **S** = one-slice / few-day, **M** = multi-slice, **L** = its own
plan-doc-sized effort. Citations are file:line into the current pipeline.

### B-1. Class type emission (`TTypeKind.Class`) — **L**

**What's there.** `ClassTypeInfo` exists in `SideTables.fs:198`, gets populated
by `NameResolution.registerClassTypeDefn` (`NameResolution.fs:911`), and is
zonked through Unification (`Unification.fs:63`, 130, 185, 215). `SemType.TyClass`
flows end-to-end. The classes-plan
([docs/classes-plan.md](classes-plan.md)) is the design.

**What's missing.**
- `Tast.TTypeKind` has only `Interface` / `Union` / `Record` (`Tast.fs:241–253`).
  Needs a `Class of fields: TRecordField list * members: TTypeMember list *
  ctorParams: TRecordField list` (or similar — borrow `Record`'s shape, add
  ctor params + mutable-field markers).
- `Freeze.tryTypeDecl` (`Freeze.fs:1812`) handles only `Anon` (via `classify`),
  `Interface`, `Union`, `Record`. Needs a `TypeDefn.Class` arm + a class-body
  member walker.
- `Codegen.fs:297–319` handles only the three current `TTypeKind`s. Needs a
  fourth arm emitting a `TypeDefinition` (class, not interface, not value-type)
  with the right `BaseType` (default `System.Object`), the primary constructor
  as a `.ctor` method, and each member as an instance method.
- TAST nodes for class semantics that don't exist yet:
  - `TExpr.New` (constructor call) — already in `Tast.fs:115` as `New of
    className: string * args: TExpr list * ty: SemType`. **Done.**
  - `TExpr.MethodCall` / `TExpr.PropertyGet` — `Tast.fs:119–120`. **Done.**
  - `TExpr.StaticMethodCall` / `TExpr.StaticPropertyGet` — `Tast.fs:122–123`. **Done.**
  - `TExpr.FieldGet` / `TExpr.FieldSet` — `Tast.fs:101–104`. **Done** (used
    today for records).
- A `this`-binding lowering path in codegen for class methods (the union-member
  precedent in `p3d.3` is the template).

**Scope cut.** The classes-plan v1 says **no** secondary `new(...)`,
**no** `static let`, **no** `member val`, **no** mutable instance fields. Each
of those is a separate enhancement (B-1.a / .b / .c / .d) — `set.fs` needs
the first three of them. Mutable fields specifically are required by `SetTree`
(no mutables, but `Set` mutates `comparer` / `tree` for deserialization —
which we drop with `[<NonSerialized>]` anyway), and by `SetIterator<'T>`
(`mutable stack` / `mutable started`). The `SetIterator` mutables go away if
the iterator becomes a struct enumerator (see B-3).

### B-2. Interface implementation — **M**

**What's there.** Parser emits `TypeDefnElement.InterfaceImpl` /
`InterfaceSpec` (`Expr.fs:215`, walked in `CstWalk.fs`). NameResolution
rejects them today: `"Inheritance / interfaces are not yet supported"`
(`NameResolution.fs:900`).

**What's missing.**
- The `set.fs` `Set<'T>` declares six interfaces: `IComparable`,
  `IStructuralEquatable`, `ICollection<'T>`, `IReadOnlyCollection<'T>`,
  `IEnumerable<'T>`, `IEnumerable`. Each has a `with member …` body that calls
  through to the type's own methods.
- Front-end: lift the diagnostic, register each `InterfaceImpl` against the
  class's `ClassTypeInfo`, type-check each member body against the interface's
  method signature (resolved via the external provider, as today's
  `BCL-interface-as-external-type` plumbing — set.fsi's `interface IComparable`
  declaration already lands at the front-end as an open type spec).
- TAST: `TTypeKind.Class` (B-1) carries an `Interfaces: (string * TTypeMember list) list`
  alongside its own members.
- Codegen: emit each interface in the `TypeDefinition.Interfaces` list +
  emit each member with `.override`-style explicit interface dispatch where
  the source named it explicitly (`(this :> seq<_>).GetEnumerator()` syntax).
- The classes-plan §"Out of scope" line on interfaces (line 962 — "v1 simply
  skips them with a 'not yet supported' diagnostic") is the same wall, now
  load-bearing.

### B-3. Object expressions — **L**

**What's there.** `Expr.Object` (`Expr.fs:278`) parses but isn't visited in
`Unification.fs` (falls through the `| _ -> TyVar (freshTyVar ctx)` catch-all
at `Unification.fs:2311`). `CstWalk.fs:270` explicitly leaves it as a no-op
(`| Expr.Object _ -> ()`).

**What's needed.**
- Front-end: `inferObject` arm that types each member body against the named
  interface (or class) and binds the synthetic `this`. Object expressions
  with multiple `interface IFoo with …` blocks (the `mkIEnumerator` shape in
  `set.fs:624` covers `IEnumerator<_>` + `IEnumerator` + `IDisposable` in one
  expression) need the same multi-interface support that B-2 builds.
- TAST: a new node, e.g.
  `TExpr.ObjExpr of baseType: string * interfaces: (string * TTypeMember list) list *
   capturedEnv: NodeKey list * ty: SemType`. The `capturedEnv` is the body's
  free variables (same shape `discoverClosures` already produces — reuse the
  closure-synthesis pipeline).
- Codegen: synthesise a sealed nested type per object expression, the way
  closure subclassing is done today (`project_codegen_clr_slice5`). Each
  interface in the source becomes an emitted `.interface` row; each member
  becomes a method.

**Cut option.** If `mkIEnumerator` is the *only* object expression `set.fs`
uses, a less-general path is to hand-write its replacement as a top-level
struct enumerator (B-3-alt). That makes B-3 a future-Vesper item rather than
a `Vesper.Set.dll` blocker. **Recommended** — duck-typed struct enumerators
are where `brainstorm-seq-module.md` is heading anyway.

### B-4. `:?>` / `:?` / `:>` outside inheritance — **M**

**What's there.** `inheritance-plan.md` §Subsumption lands `:>` / `:?` / `:?>`
**only** in the inheritance context (derived class → base class). `set.fs`
uses `:?>` in `SetTree.asNode` (line 43, `value :?> SetTreeNode<'T>`) which
*is* inheritance (`SetTreeNode` `inherit SetTree`), so that case is in scope
for the inheritance work. But it also uses `:?>` on the `IComparable.CompareTo`
arm (line 988, `(that :?> Set<'T>).Tree`) — a downcast from `obj` to the
type implementing the interface. That's not inheritance-pattern downcasting;
it's the standard `obj`-to-derived-type test that interface-method bodies need.

**What's needed.**
- Extend the inference / freeze / codegen arms added by the inheritance work
  to accept `obj`-to-`TyClass T` downcasts. The IL emission (`unbox.any`
  for value types, `castclass` for ref types) is identical.
- `:?` in a match pattern (the pattern-match form) is *also* needed for the
  `Equals(that, comparer)` arm at `set.fs:992`. The inheritance plan covers
  patterns; verify it covers `:? Set<'T>` against an `obj` receiver.

### B-5. `use` (IDisposable) — **S**

**What's there.** Parser has `LetOrUseKeyword.Use` (`Expr.fs:199`) and routes
it through `Expr.LetOrUse`. Unification handles `LetOrUse` generically
(`Unification.fs:2256`) — the `use` vs. `let` distinction is currently dropped
at Freeze. The CST captures it but the TAST has no `try…finally` wrapping yet.

**What's needed.**
- Freeze: desugar `use x = e in body` as `let x = e in try body finally (x :> IDisposable).Dispose()`,
  with a synthesised `try…finally` and an explicit `IDisposable` upcast.
- Or, equivalently, a TAST node `TExpr.Use of binding: TPat * value: TExpr *
  body: TExpr * ty: SemType` that codegen lowers directly to the same IL
  `try…finally` + `callvirt IDisposable::Dispose` pair.
- `set.fs` uses `use` at lines 768, 975, 976, 991, 992 — five sites, all
  enumerator disposal. Each is rewritable to an explicit `try…finally` once
  exception handling is in good shape (it is — `TryFinally` is wired:
  `Unification.fs:2284`, `Tast.fs:80`).

### B-6. `for x in IEnumerable<'T> do` (over a real enumerable) — **S–M**

**What's there.** `inferForIn` (`Unification.fs:2871`) only accepts `Expr.Range`
sources today; everything else trips
`"for-in: enumerable / element-type checking not yet implemented"`
(`Unification.fs:2895`). Codegen has no path either.

**What's needed.**
- Front-end: when `src`'s zonked type implements `IEnumerable<'T>` (or, the
  duck-typed `GetEnumerator()`-returning-struct shape per
  `brainstorm-seq-module.md`), unify `patTy` with `'T`.
- Codegen: lower to the standard `IEnumerator<'T>` loop pattern
  (`GetEnumerator` → `try { while MoveNext do { body(Current) } } finally
  { Dispose() }`). Once B-5 lands the `try…finally` half is reusable.
- `set.fs` uses `for x in this do …` at lines 964, 1011, and `for item in items do …`
  at line 1113 — each iterates the receiver (a `Set<'T>`, an
  `ReadOnlySpan<'T>`). The span case in `Create` we drop (it's under
  `#if NETSTANDARD2_1_OR_GREATER`); the two `for x in this` sites can be
  rewritten as explicit `IEnumerator<'T>` loops once B-2 (interface
  implementation) and B-5 (use / try-finally) land.

### B-7. Struct tuples — **S**

**What's there.** Parser emits `Expr.StructTuple` (`Expr.fs:275`); CstWalk
descends into it (`CstWalk.fs:175`); Unification's expression dispatch has
*no* arm for it, so it falls through the catch-all and produces a free TyVar
(`Unification.fs:2311`). No TAST node, no codegen.

**What's needed.**
- `TExpr.StructTuple of items: TExpr list * ty: SemType` (or just allow the
  existing `TExpr.Tuple` to mark `IsStruct: bool` — same shape, different
  emit).
- Front-end arm types it as `struct ('a * 'b)` (a `ValueTuple<'a, 'b>` from
  the BCL — `unit` already aliases `ValueTuple` per the core README).
- Codegen: emit as `System.ValueTuple<…>` instantiation, the same surface F#
  uses. Field reads are `.Item1` / `.Item2`.
- `set.fs` uses struct tuples in `partition1With` / `partitionWith` (lines
  481–509) — four call sites. They can also be rewritten to reference
  tuples (`'a * 'b`) at a small allocation cost, scope-cutting the struct
  tuple work to a later slice.

### B-8. `[<Sealed>]`, `[<AllowNullLiteral>]`, `[<Literal>]` attributes — **S**

**What's there.** `[<Sealed>]` and `[<AllowNullLiteral>]` are recognised as
attribute tokens by the parser but not consumed by NameResolution / Freeze
beyond the structural-equality verdict (C-Attr already decodes
`[<NoEquality>]` / `[<NoComparison>]` / `[<ReferenceEquality>]` —
`project_c_attr_pr_a`).

**What's needed.**
- Wire `[<Sealed>]` through to the codegen `TypeDefinition.IsSealed` flag
  (mechanical once B-1 lands).
- `[<AllowNullLiteral>]` toggles whether `null` is unifiable with a `TyClass`
  type. Today `Expr.Null` types as a free TyVar (`Unification.fs:2288`).
  `set.fs` line 37 (`let empty = null`) needs this. A small registry bit on
  `ClassTypeInfo.AllowNullLiteral` does it.
- `[<Literal>] let private tolerance = 2` (set.fs:103) — check whether the
  attribute makes its way through Freeze as a compile-time constant.
  Probably free already (the binding's `inlineToken` would do the same job),
  but worth a slice.

### B-9. `raise` of an arbitrary exception type — **S**

**What's there.** `failwith` is the *only* throwing primitive supported
(`Emit.fs:1351`'s hard-coded recipe lowers it to `throw new System.Exception(msg)`).
The `raise (FooException …)` form parses fine and goes through the regular
`Expr.App` machinery, but the front-end has no special case for the
`raise` symbol, so its call site types as a free TyVar.

**What's needed.**
- Front-end: recognise `raise` (resolved via the `Vesper.Core` open scope) as
  a polymorphic `'e :> exn -> 'a` value. The argument is constructor-applied
  to an exception type, which lands as an `Expr.New` (B-1's machinery).
- TAST: a `TExpr.Raise of exn: TExpr * ty: SemType`. Codegen lowers to
  `throw`.
- `invalidArg "name" "message"` is just `raise (ArgumentException("message", "name"))`
  with a known compiled name; once `raise` works, `invalidArg` is a sugar.
- `set.fs` uses `raise (InvalidOperationException …)` at lines 588, 591 and
  `invalidArg "s" …` at 551, 556 — four sites total. All are
  rewriteable to `failwith` in a first cut, deferring B-9.

### B-10. Static class members + `static let` — **M**

**What's there.** NameResolution recognises `isStatic` on class members
(`NameResolution.fs:830`) and stamps them on `ClassMemberInfo`. The
classes-plan explicitly defers them (line 962, "v1 covers instance only").
`set.fs` has many statics: `Set<'T>.Empty` (static property),
`Set.Singleton` / `Set.Union` / `Set.Intersection` (static methods),
`Set.Create` (static factory), plus `static let empty` (the per-instantiation
empty-set cache).

**What's needed.**
- Codegen path for static instance and static `let` (class-static field +
  static-constructor initialiser).
- TAST: nothing new — `TExpr.StaticMethodCall` / `TExpr.StaticPropertyGet`
  exist (`Tast.fs:122–123`).
- The lift of NameResolution's silent-static path to a real codegen-targeted
  arm.

### B-11. Secondary constructors (`new(elements: seq<'T>) = …`) — **S**

**What's there.** NameResolution rejects `MemberDefn.AdditionalConstructor`
with `"This member kind is not yet supported"` (`NameResolution.fs:887`).
classes-plan defers them.

**What's needed.**
- Lift the diagnostic, type the body against the class's primary-constructor
  signature, emit as a `.ctor` overload that calls the primary `.ctor` via
  `call instance void`.
- `set.fs` line 1049 (`new(elements: seq<'T>) = …`) is the only secondary
  ctor; trivial to drop in v1 in favour of a static factory.

### B-12. Generic methods on classes — **M**

**What's there.** Generic *type* parameters on class declarations work
(`ClassTypeInfo.TypeParams`). Generic *method* parameters on class members
have `ClassMemberInfo.MethodTypeParams` (`NameResolution.fs:875`), populated
for abstract member signatures (interfaces). Whether classes emit them
correctly is unclear — confirm via a small spike before relying on it.

**Why it matters here.** `Set<'T>` has `member s.PartitionWith<'T1, 'T2>(…)`
and `member s.Map<'U>(…)` — generic methods on a non-generic *scope* (the
method introduces fresh typars). Vesper's generic-DU-member backend (R2,
`project_self_host_r2_generic_members`) is the template; reuse it.

## C — Library / contract additions

These are the pieces `set.fs` consumes that need a contract in `src/Vesper.*`
before linking succeeds. None are blocking the *backend* work (B); they are
parallelisable.

| Need | Source | Likely package |
|---|---|---|
| `Seq.fold`, `Seq.reduce`, `Seq.truncate`, `Seq.toArray` | `set.fs:920, 923, 1064, 1122` | New `Vesper.Seq` package, per `brainstorm-seq-module.md`. The brainstorm targets the struct-enumerator deforestation story; for `set.fs` even a non-fused reference impl suffices to start. |
| `Array.fold`, `Array.zeroCreate` | `set.fs:757, 772` | New `Vesper.Array` package or a `Array` module hanging off the BCL `'T array` type (the simpler path). |
| `List.ofSeq`, `List.toSeq` | `set.fs:1064, 1209` | Add to `src/Vesper.List/list.fsi` + `list-min.fs` (or `List.fs` once it lands). |
| `Choice<'T1, 'T2>` (DU + `Choice1Of2` / `Choice2Of2`) | `set.fs:484, 485`; `set.fsi:766, 778` | New `Vesper.Choice` package, or add to `src/Vesper.Core/core-types.fsi` next to `Result`. Sibling of `Result` — simpler than `Option` (no struct optimisation), shipped as a regular DU. |
| `IComparer<'T>` (BCL surface) | `set.fs:140`, throughout | Already routes through `Comparer<'T>.Default` per `Vesper.Comparison`; no new contract needed — the front-end's external resolution sees `System.Collections.Generic.IComparer<'T>` via the BCL provider. Confirmed by `project_operator_emission_equality`. |
| `IEnumerator<'T>` / `IEnumerable<'T>` / `ICollection<'T>` / `IReadOnlyCollection<'T>` / `IStructuralEquatable` / `IDisposable` | `set.fs:621–646, 1013–1046` | BCL types, no contract to ship — they're available via the `MetadataLoadContext` provider as long as the *implementation* (B-2) can consume them. |
| `System.Text.StringBuilder` | `set.fs:1068–1097` | BCL; same as above. Or replace four arms with `sprintf`-style printf composition. |
| `Argument`/`InvalidOperationException` (BCL types) | `set.fs:551, 588, 591` | BCL; gated on B-9. |

## D — Recommended slice order

The shortest path to a *minimally-useful* `Vesper.Set.dll` — say, the
operations `Set.empty<int>`, `Set.add`, `Set.contains`, `Set.toList`, with
no `Set.union` / `Set.partition` / `Set.fold` / iteration:

1. **D-1.** Drop the `[<Debugger*>]`, `[<NonSerialized>]`,
   `[<OnSerializing/Deserialized>]`, `#if TRACE_SETS_AND_MAPS`,
   `#if NETSTANDARD2_1_OR_GREATER` blocks, `SetDebugView` proxy class,
   `Set.Create` factory. **(A)** edits only; no compiler dependency.
2. **D-2.** Land `[<AllowNullLiteral>]` for class types + null-as-empty
   tree representation, **(B-8)**. The `SetTree<'T>` AVL inheritance pair
   builds on this.
3. **D-3.** Land **B-1** (class type emission) without statics, without
   interfaces, without mutable fields. Just primary constructor + instance
   members. Replace `static let empty` with a parameterless static factory
   method (deferred to step 5) — for now have callers pass a comparer.
4. **D-4.** Land **B-4** (subset of inheritance work): the `inherit
   SetTree(...)` form, the `:?> SetTreeNode<'T>` downcast in `asNode`. The
   rest of `:?>` / `:?` patterns waits.
5. **D-5.** Land **B-10** (statics + `static let`) so `Set<'T>.Empty` works.
6. **D-6.** Add **Choice DU** to `Vesper.Core` or as a new `Vesper.Choice`
   package (C), enabling `partitionWith` (which we might also defer).
7. **D-7.** Land **B-7** (struct tuples) — or rewrite the four
   `partitionWith` arms to use reference tuples and defer B-7.
8. **D-8.** Rewrite enumerator construction (`mkIEnumerator`) as a top-level
   struct enumerator (**B-3-alt**, the cut option). This avoids the deep
   object-expression work.
9. **D-9.** Land **B-2** (interface implementation) for the *minimum* set
   `Vesper.Set.dll` needs — probably just `IEnumerable<'T>` / `IEnumerable`
   so `for x in set do …` works. Defer `IComparable` / `IStructuralEquatable`
   / `ICollection<'T>` / `IReadOnlyCollection<'T>`.
10. **D-10.** Land **B-5** (`use`) and **B-6** (`for x in IEnumerable`) so
    the iteration sites compile.
11. **D-11.** Land minimal **Seq** + **Array** module surfaces (C) for
    `Set.union` / `Set.intersect` / `Set.ofSeq`. Or write them as explicit
    enumerator loops.
12. **D-12.** Restore the full `set.fs` surface: `Set.fold` / `Set.foldBack`
    / `Set.partition` / `Set.partitionWith` / `Set.minElement` /
    `Set.maxElement` / `Set.toArray` / `Set.ofArray`.

Steps 1–8 are roughly the boundary between "parse-only" (today) and
"compiles a useful subset". 9–12 close the rest.

## Cross-references

- [`classes-plan.md`](classes-plan.md) — the design B-1 implements.
- [`inheritance-plan.md`](inheritance-plan.md) — the design B-4 implements; covers `:>` / `:?` / `:?>` and `inherit` syntax.
- [`brainstorm-seq-module.md`](brainstorm-seq-module.md) — the design behind C's `Vesper.Seq` package and the deforestation story.
- [`brainstorm-structural-equality.md`](brainstorm-structural-equality.md) — C-Attr posture for `[<EqualityConditionalOn>]` etc.
- [`function-representation-plan.md`](function-representation-plan.md) — the `Fun<_,_>` story B-3's object expression lowering builds on.
- [`operators-plan.md`](operators-plan.md) — Vesper.Comparison wiring, the precedent for the `Comparer<'T>.Default` replacement in A.
- [`../../../src/Vesper.Set/README.md`](../../../src/Vesper.Set/README.md) — the package README pointing at this doc.
- [`../../../src/Vesper.Set/set.fs`](../../../src/Vesper.Set/set.fs) — the verbatim impl this doc inventories against.
