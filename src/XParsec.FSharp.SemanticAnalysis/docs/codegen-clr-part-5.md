# XParsec.FSharp.Codegen.Clr — slice 5 plan

Follow-on to [codegen-clr-part-4](codegen-clr-part-4.md), whose thin-slice #4
(`printfn "%A" [1; 2; 3]`) is implemented and passing. The on-disk runnable-app
milestone also landed (`Codegen.materialiseApp` writes the PE +
`runtimeconfig.json` + an `FSharp.Core.dll` copy, so `dotnet <app>.dll` runs
standalone — `RunnableAppTests.fs`), which this slice's capstone uses to prove
the full sample runs as a real assembly. This doc plans
[il-emission-roadmap](il-emission-roadmap.md)'s **thin-slice #5 — the full
canonical sample**:

```fsharp
let inline sum xs = List.fold (+) 0 xs
let nums = [1; 2; 3; 4; 5]
printfn "%d" (sum nums)
```

It assumes the part-2/3/4 file layout (`Types`, `Cil`, `CilBuilder`, `Metadata`,
`ICodegenProvider`, `ClrProvider`, `Emit`, `Codegen`) and that every prior
deviation carries forward (host-derived assembly identities; the depth-tracked
walker; `Main`-local slots keyed by binding `NodeKey`; the `CallRecipe.Emit`
closure; the `FSharpFunc.Invoke` consumption path; the `EmitEnv` record +
inline-expansion machinery; the `TryEmitUnionCons` list-construction hook).

## The headline: slice 5 is closure *synthesis*

Slices 1–4 emitted exactly one type with one method (`Program` + `Main`). Every
value lived on the stack or in a `Main` local; every call was to an existing
external member. **Slice 5 is the first slice that emits new types** — closure
classes — because the sample passes `(+)` as a *value* to `List.fold`, and a
function value at runtime is an `FSharpFunc<_,_>` instance.

Three facts pin the design:

- **`List.fold` is not inline.** The lib signature is
  `val fold<'T,'State> : ('State -> 'T -> 'State) -> 'State -> 'T list -> 'State`
  (`Common/list.fsi`) — a genuine cross-assembly generic function. So the folder
  argument must be a real `FSharpFunc` value handed across the call boundary; it
  cannot be inline-spliced away (contrast slice 3's `succ`).
- **`(+)` is inline-SRTP but used as a value.** The freeze leaves an operator
  used as a value as a bare `TExpr.External("op_Addition", int->int->int)`
  ("codegen can eta-expand if needed", `Freeze.fs:798`). An *inline* function
  used as a first-class value can't be expanded — it must be **eta-reified**
  into a closure that, when invoked, runs the operator.
- **`sum` is monomorphic.** With int-defaulting (`default ^T1 : int` in the
  lib's `(+)`), `sum : int list -> int`. So slice 3's monomorphic inline
  expansion (`inlineExpand decl [||]`) already handles `sum nums`; **no
  polymorphic inline and no generic closures are needed for this sample**
  (despite the roadmap's "polymorphic inline body" phrasing — the default
  collapses it to `int`).

So the genuinely new machinery is: **synthesising `FSharpFunc<_,_>` subclasses**
(the roadmap's stated v1 representation — [function-representation-plan](function-representation-plan.md)
§"First-pass shape") and the **metadata infrastructure to emit more than one
type**. The `Fun<_,_>`-constraint / value-struct-closure form in
function-representation-plan is the *destination*, explicitly a follow-up;
v1 ships heap `FSharpFunc` closures with `callvirt Invoke` (the slice-2
consumption path, unchanged).

## What the sample demands that slice 4 lacks

| Need | New mechanic |
|---|---|
| `(+)` / a lambda as a runtime function value | **Closure synthesis**: emit an `FSharpFunc\`2<a,b>` subclass with a virtual `Invoke` override carrying the lowered body; `newobj` it at the use site. |
| A lambda that reads an enclosing variable | **Capture analysis + fields**: free vars become instance fields; the ctor stores them; `Invoke` reads them via `ldfld`. First use of `FieldDefinition` rows. |
| Curried `(+)` (`int -> int -> int`) | **Nested closures**: outer `FSharpFunc\`2<int, FSharpFunc\`2<int,int>>` whose `Invoke(x)` `newobj`s an inner `FSharpFunc\`2<int,int>` capturing `x`. |
| More than one emitted type | **Multi-type assembly layout**: contiguous per-type method/field ranges, types added leaves-first so every cross-reference points backward. Restructures `Codegen.assembleWith`. |
| Variable means different IL in different method bodies | **Per-method variable resolver**: in `Main` a `Var` is a local slot; in a closure `Invoke` it is `ldarg.1` (the param) or `ldarg.0; ldfld` (a capture). |
| `List.fold<int,int>(folder, 0, xs)` | **Generic 2-typar cross-assembly call**: a `TryEmitCall` recipe minting a `MethodSpec` over `ListModule::Fold` (the `emitPrintfn` pattern, but 2 typars / 3 args). Needs the head's full type, not just the result. |

## Scope: monomorphic, heap `FSharpFunc`, this sample

- **`FSharpFunc<_,_>` heap closures only.** No `Fun` interface, no value-type
  closures, no `OptimizedClosures` 2-arg `Invoke` — curried chains with one-arg
  `Invoke` and `callvirt`. (function-representation-plan §"Out of scope".)
- **Monomorphic closures only.** Every closure here is a *closed* type
  (`FSharpFunc\`2<int, …>`); its `Invoke` signature uses concrete types, not
  type parameters. Generic closures (a closure inside a polymorphic generic
  method) are deferred.
- **Tree-shaped closure nesting.** The sample's closures form a tree (outer →
  inner). Mutually-recursive closures (a cycle in the construct-graph) need
  forward method-handle references and are out of scope — see Open questions.
- **`List.fold` only, via `MockBuiltins`.** Add `ListModule.Fold` to the test
  provider (consistent with slices 1–4 keeping the suite on `MockBuiltins`); the
  lib-backed provider already extracts it but is heavier. Other combinators
  (`map`, `filter`, …) are later.

## TAST shapes (what `Emit` will walk)

The three decls, with `list<int>` ≡
`TyRecord("Microsoft.FSharp.Collections.list", [TyConst "int"])` and
`Fold = "Microsoft.FSharp.Collections.ListModule.Fold"`:

```
// 1. the inline binding (collected into the Inlines side table, emits nothing)
TDecl.Let(
  TPat.NamedSimple(kSum, TyFun(list<int>, int)),
  Lambda(TPat.NamedSimple(kXs, list<int>),
         App(App(App(External(Fold, (int->int->int) -> int -> list<int> -> int),
                     External("op_Addition", int->int->int)),          // (+) AS A VALUE
                 Const(Int 0)),
             Var(kXs, list<int>)),
         TyFun(list<int>, int)),
  isInline = true,
  TyFun(list<int>, int))

// 2. the list literal (slice 4 construction)
TDecl.Let(TPat.NamedSimple(kNums, list<int>), <Cons/Nil chain over [1..5]>, false, list<int>)

// 3. the effectful print (slices 1/2 printfn + an inline call site)
TDecl.Expression(
  App(App(External "printfn", New("…PrintfFormat", [Const(String "%d")], …)),
      App(Var(kSum, _), Var(kNums, list<int>), int)),               // sum nums — inline call
  unit)
```

Read off the nodes:
- Decl 1 is `inline` → collected into `Inlines`, emits no value (slice 3).
- The call site `sum nums` is `App(Var kSum, Var kNums)` with `kSum ∈ Inlines`,
  so slice 3 expands it: `inlineExpand decl [||] |> freshen |> betaReduce
  [(Var kNums, _)]` → `Let(xs' := nums, <body[kXs := xs']>)`. The body is then
  the `List.fold` spine.
- Inside that body, **`External "op_Addition"` appears as an argument, not an
  application head** — the eta-reification trigger. **`External Fold` is the
  spine head** — the generic-call trigger.
- `nums` is built by slice-4 list construction; `Var kNums` reloads the `Main`
  local.

## Design changes, file by file

This slice touches every file except `CilBuilder.fs` and `Types.fs`. The two
load-bearing additions are **multi-type metadata layout** (`Metadata.fs` +
`Codegen.fs`) and **closure synthesis** (`Emit.fs` + `ClrProvider.fs`).

### `Metadata.fs` — emit fields, classes, and virtual methods

New surface, none of which existed (slices 1–4 used only `AddMethod` /
`AddModuleType` / `AddProgramType`, all single-type):

- `AddField(attrs, name, signature) : FieldDefinitionHandle` — a
  `FieldDefinition` row (`BlobEncoder.FieldSignature()` for the type).
- `AddClass(name, baseType: EntityHandle, firstField, firstMethod, attrs) :
  TypeDefinitionHandle` — a `TypeDefinition` whose **base is a `TypeSpec`**
  (the instantiated `FSharpFunc\`2<a,b>`), not just `Object`. Generalises
  `AddProgramType`, which hard-codes `abstract sealed` + `Object`.
- The method-range model is unchanged but now exercised for real: a
  `TypeDefinition` owns method/field rows from its first handle until the next
  type's. **Callers must add each type's fields then methods, in type order,
  before adding the `TypeDefinition` rows** (also in that order). The serializer
  needs the ranges contiguous.

`Serialize` is unchanged; the entry-point token still points at `Main`.

### `Codegen.fs` — multi-type assembly layout

`assembleWith` today emits `Main`'s body, then `<Module>`, then `Program`. Slice
5 restructures it into a **two-pass, leaves-first** build so closures and
`Program` coexist with contiguous ranges and only backward references:

1. **Discover.** Walk all decls (the same walk `Emit` does, but emitting no IL)
   to enumerate the closures the program needs — each `Lambda`-as-value and each
   `External`-of-function-type-as-value — with its capture set and its nesting.
   Order them **leaves-first** (an inner closure before the outer that
   constructs it); a tree for this sample. Assign each a synthetic type name
   (`<closure>$0`, `$1`, …) and `NodeKey`.
2. **Emit.** For each closure in leaves-first order: build its `Invoke` body and
   `.ctor` body (the `Invoke`-body walk resolves any inner-closure `newobj` to an
   already-emitted ctor handle, since inners came first), then add its field rows
   and its `.ctor`/`Invoke` method rows. Then build `Main` (its walk resolves the
   top-level closures' ctor handles — already emitted) and add `Program`'s
   method row. Finally add the `TypeDefinition` rows in order `<Module>`,
   leaves…, root, `Program`.

Because every closure is fully emitted (fields + both methods) before the next,
and because leaves precede the closures that reference them and `Program` is
last, **no forward method-handle reference arises** — no handle prediction
needed (see Open questions for the recursive-closure case that would need it).
The discover/emit split mirrors the slice-3 `Inlines` pre-pass: a cheap first
walk that makes the second walk's references resolvable.

`materialise` / `materialiseApp` are unchanged — the runnable-app bundle already
works for whatever `Program` + types `compile` produces.

### `Emit.fs` — closure collection, capture analysis, the new arms

The walker grows the function-*value* half of the function-representation
problem (slice 2 did the consumption half, `Invoke`). New pieces:

- **Variable resolution becomes context-dependent.** Today `TExpr.Var` always
  reads a `Main` local (`Slots`). Introduce a `VarLoc = Local of int | Arg of
  int | Capture of FieldRef` and thread a resolver on `EmitEnv` (the closure
  `Invoke` body maps the param key → `Arg 1` and each capture key →
  `Capture field`; `Main` keeps `Local slot`). The `TExpr.Var` arm dispatches on
  the resolver instead of going straight to `Slots`.
- **`TExpr.Lambda` as a value** (currently fails loudly): synthesize a closure.
  Compute free vars (body `Var` keys minus the param and inner-bound keys), emit
  each captured value at the construction site (via the *current* resolver), then
  `newobj <closure>$n::.ctor(captureTypes)`. The closure's `Invoke` body is the
  lambda body lowered under a fresh resolver.
- **`TExpr.External` of function type as a value** (currently fails loudly):
  **eta-reify**. Treat `External(name, a -> b -> … -> r)` as
  `fun p0 -> fun p1 -> … -> name p0 p1 …` — synthesize the nested-lambda chain
  with body `App(App(External name, p0), p1)…`, then run the lambda path. The
  innermost body emits through the *existing* recipe for `name` (for
  `op_Addition`, the `add` intrinsic). One mechanism covers operators-as-values
  and any function-name-as-value.
- **Capture analysis** (`freeVars : TExpr -> NodeKey list`): a pure walk
  collecting referenced `Var` keys minus locally-bound ones, in stable order
  (drives field order = ctor-arg order = construction-site push order). Lives in
  `Emit` (CLR-specific layout) but is mechanical.
- The `App`-head dispatch gains the **generic external call** path: when the
  head `External`'s recipe has `ArgCount = 3` (Fold), the leading three args are
  emitted (the folder arg hits the new `External`-as-value arm → a closure
  `newobj`) and `applyRecipe` runs the `call`. This already falls out of the
  existing `External` head handling once `TryEmitCall` returns the Fold recipe.

The slice-3 inline path is unchanged: `sum nums` expands to a `Let` chain whose
body is the `List.fold` spine, walked exactly as above. Freshening keeps the
expanded `xs'` distinct.

### `ICodegenProvider.fs` / `ClrProvider.fs` — the Fold recipe + closure bases

- **`TryEmitCall` needs the head's full type, not just the result.** A 2-typar
  generic call can't recover `'T` and `'State` from the application result
  (`int`) alone. Change the hook to receive the head `External`'s declared
  curried type (`fnTy`), from which both new and existing recipes read what they
  need: `emitPrintfn` takes the printer = *result of* `fnTy` (unchanged
  behaviour, different source); `emitFold` reads `'State`/`'T` from the folder
  parameter `(’State -> ’T -> ’State)`. `Emit` passes `typeOfExpr head`.
  (Additive-friendly: keep `resultTy` too if cleaner, but `fnTy` subsumes it.)
- **`emitFold (fnTy)`** → `call ListModule::Fold<'T,'State>(folder, state, list)`:
  a member ref to the generic method def
  `'State Fold<'T,'State>('State->'T->'State, 'State, 'T list)` against a new
  `eListModule` (`ctx.TypeRef(fsCoreRef, "Microsoft.FSharp.Collections",
  "ListModule")`), parameters written with `GenericMethodTypeParameter` (folder
  = `FSharpFunc\`2<!!1, FSharpFunc\`2<!!0,!!1>>`, state = `!!1`, list =
  `FSharpList\`1<!!0>`, ret = `!!1`), then a `MethodSpec` instantiating
  `<int, int>` via `encodeType`. `Emit = call`, `ArgCount = 3`, `Pushes = 1`.
  Keyed on the full compiled name `…ListModule.Fold` (not last-segment `Fold`,
  which collides with `Array`/`Seq`).
- **Closure base + member helpers** (consumed by `Emit`/`Codegen`, so exposed on
  the provider or a shared module):
  - `closureBaseSpec (a, b) : EntityHandle` — the `FSharpFunc\`2<a,b>`
    `TypeSpec` for a closure's base type (reuses `encodeType`'s `TyFun`
    encoding).
  - `fsharpFuncCtorRef (a, b) : EntityHandle` — member ref to the protected
    parameterless `FSharpFunc\`2<a,b>::.ctor()` the closure ctor chains to.
  - The closure's own `Invoke` (an override) and `.ctor` (concrete, with capture
    params) are `MethodDefinition`s built in `Codegen`/`Emit`, signatures
    concrete (the closure type is closed) — no `TypeSpec`-relative member refs
    needed for *its own* members.

`encodeType` already covers every type these signatures need (`int`, `TyFun` →
`FSharpFunc\`2`, list → `FSharpList\`1`).

### `ExternalSymbols.fs` — register `List.fold` in `MockBuiltins`

Add `Microsoft.FSharp.Collections.ListModule.Fold` as a `poly` symbol typed
`('State -> 'T -> 'State) -> 'State -> 'T list -> 'State` (2 typars), so the
sample type-checks through the test provider and freezes the `External Fold`
head. (`op_Addition` is already registered.)

### unchanged

`Types.fs`, `Cil.fs`, `CilBuilder.fs`. The closure method bodies drive the same
untyped `Il` helpers (`ldarg`/`ldfld`/`stfld` are the only genuinely new
opcodes — add `emitLdarg`/`emitLdfld`/`emitStfld` to `Cil.fs` alongside the
existing `emit*`; trivial). The `add` intrinsic and `callvirt Invoke` are reused
verbatim.

## Build order and testable staging

Each milestone ends with a runnable assertion — in-process via `runEntryPoint`
and (for the capstone) on-disk via `runOnDisk` (`dotnet <dll>`). The progression
isolates the closure mechanics before composing them with `List.fold`.

1. **Non-capturing closure (end-to-end).** `let f = fun x -> x + 1` /
   `printfn "%d" (f 41)` → `42`. The minimal closure: one `FSharpFunc\`2<int,int>`
   subclass, no fields, `Invoke` = `ldarg.1; ldc.i4.1; add; ret`, `newobj` at
   the binding, applied through the existing `Invoke` path. Proves a *second*
   emitted type, a generic-instantiation base, a virtual override, `newobj` of a
   synth type, and that both runners load a multi-type assembly. **This is the
   single biggest infra step** (Metadata `AddField`/`AddClass`, the Codegen
   layout restructure, the per-method resolver) — everything after composes onto
   it.
2. **Capturing closure (end-to-end).** `let n = 10` /
   `let g = fun x -> x + n` / `printfn "%d" (g 41)` → `51`. Adds capture
   analysis, a `FieldDefinition`, a ctor that stores the capture, `ldfld` in
   `Invoke`, and the capture push at the `newobj` site.
3. **Curried / eta-reified closure (end-to-end).** `let add = (+)` /
   `printfn "%d" (add 40 2)` → `42`. Adds nested-closure synthesis and the
   `External`-as-value eta-reification — the outer closure `newobj`s the inner;
   the application goes through `Invoke().Invoke()`. This is exactly the folder
   value the sample needs, proven in isolation.
4. **The full sample (end-to-end, on disk).** `let inline sum xs = List.fold
   (+) 0 xs` / `let nums = [1; 2; 3; 4; 5]` / `printfn "%d" (sum nums)` → `15`.
   Adds the `ListModule.Fold` provider entry + recipe and ties together inline
   expansion (slice 3), list construction (slice 4), the eta-reified `(+)`
   closure (milestone 3), and the generic cross-assembly call. Assert via
   `dotnet <dll>` to close the roadmap: a real assembly that prints `15`.

(1) proves emitting types at all; (2) captures; (3) currying + eta; (4) the
generic call and the composition. After (4), `il-emission-roadmap`'s canonical
sample runs end-to-end.

## Verified facts

Grounded in the lib, the freeze, and the existing slices:

- **`List.fold` is non-inline, 2-typar**: `val fold<'T,'State> : ('State -> 'T
  -> 'State) -> 'State -> 'T list -> 'State` (`Common/list.fsi:849`). `(+)` is
  `val inline (+)` SRTP with `default … : int` (`Clr/prim-types.fsi:2795`), so
  the sample's `(+)` resolves to `int -> int -> int` and `sum : int list -> int`
  (monomorphic).
- **Operators-as-values freeze to a bare `External`** of function type — the
  freeze does not eta-expand (`translateApp` / the LongIdentOrOp path;
  `Freeze.fs:798` "codegen can eta-expand if needed"). So the folder arg reaches
  `Emit` as `External("op_Addition", int->int->int)`.
- **`FSharpFunc\`2<T,R>`** exposes the abstract `R Invoke(T)` and a protected
  parameterless `.ctor()` a subclass chains to — the standard closure base the
  F# compiler itself derives from. `encodeType`'s `TyFun` case already mints
  `FSharpFunc\`2<a,b>` (slice 2).
- **The consumption path is done**: applying an `FSharpFunc` value via
  `callvirt Invoke` (slice 2, `emitInvoke`) is exactly how the synthesized
  closures get *used*; slice 5 only adds their *creation*.
- **List construction + inline + printfn are done** (slices 1–4): `nums`, the
  `sum` expansion, and the `%d` print need no new work beyond wiring.

## Out of scope (still)

- **`Fun<_,_>` constraint form and value-type closures**
  ([function-representation-plan](function-representation-plan.md)) — the
  zero-alloc/devirt destination; v1 is heap `FSharpFunc` + `callvirt`.
- **Generic closures** — a closure captured inside a polymorphic generic method
  (needs `GenericParameter` on the closure type). The sample is monomorphic.
- **Returned function values that escape** (factory functions) — covered later
  by inline (1), escape analysis (2), or the `FSharpFunc` fallback (3) per
  function-representation-plan §"Return positions".
- **Mutually-recursive / cyclic closures** — would need predicted
  `MethodDefinitionHandle`s for forward references; the leaves-first layout
  assumes a closure tree.
- **`OptimizedClosures` 2-arg `Invoke`**, currying flattening, and other
  combinators (`List.map`, etc.) — perf/coverage follow-ups.

## Open questions / decisions

- **Closure synthesis lives in the CLR backend, not a universal pass.**
  backend-design-plan §"Lowering split" lists closure/eta lowering as a
  universal canonicalisation — but that targets the *`Fun` form*, the
  follow-up. `FSharpFunc`-subclass synthesis is irreducibly CLR-specific
  (it emits CLR types deriving from a CLR base), so v1 does it in `Emit` +
  `Metadata`, consistent with slices 1–4 keeping `SemanticAnalysis` untouched
  apart from the `MockBuiltins` fixture. When the `Fun` universal pass lands,
  it replaces the front of this path additively.
- **Leaves-first layout vs. handle prediction.** Chosen: emit closures
  leaves-first with `Program` last, so all method-handle references point
  backward and can use real handles — no `MetadataTokens.*Handle(i)` prediction.
  Prediction is the general technique (and the only option for cyclic closures),
  but it's avoidable for the tree this sample produces and the simpler layout is
  worth it.
- **`TryEmitCall` carries the head's full type.** Decided over a bespoke
  generic-call hook: one signature change (`resultTy` → `fnTy`) serves printfn,
  arithmetic, *and* Fold, and any future multi-typar external. `Emit` already
  has `typeOfExpr head` to hand.
- **Eta-reify `External`-as-value rather than teach the freeze to do it.** Keeps
  the freeze target-agnostic (the freeze comment already defers this to codegen)
  and means one synthesis path handles both lambdas and named functions used as
  values.
- **Closure type naming.** Synthetic `<closure>$N` names keyed by a build-wide
  counter (like slice 3's `Mint`), so nested/repeated closures don't collide.
  Cosmetic — the runtime finds them by token, not name.
- **`Cil` gains `ldarg`/`ldfld`/`stfld` untyped helpers.** Minor; the typed
  `Op` surface already has `ldarg` for hand-written bodies — add the depth-tracked
  `emit*` twins for the walker.

## Deviations discovered during implementation

- **Operators-as-values were *not* already supported by the front-end.** The
  "Verified facts" claim that `(+)` freezes to `External("op_Addition", …)` was
  false against the current pipeline + `MockBuiltins`: `(+)` parses as
  `Expr.LongIdentOrOp(LongIdentOrOp.Op(ParenOp(_, SymbolicOp _, _)))`, which
  NameResolution rejected ("Operator-form qualified names not yet resolved")
  and froze to `External("(", TyVar)`. Slice 5 therefore *added* minimal
  operator-as-value support — contrary to the "SemanticAnalysis unchanged apart
  from `MockBuiltins`" posture — across three passes plus one shared helper:
  - `Desugar.symbolicOpCompiledName : Token -> string voption` (exposes the
    existing infix op→name map for value position).
  - NameResolution skips the unresolved-name error for a symbolic `(op)` that
    maps to a known name (it resolves through the provider in Unification).
  - Unification's `inferIdent` types the `(op)` form via `Provider.TryLookup`.
  - Freeze's `translateIdent` emits `External(compiledName, ty)` for it.

  This is the eta-reify trigger the rest of the slice was designed around; with
  it, the closure backend is otherwise exactly as planned.
- **`List.fold` is keyed by its *source* name `"List.fold"`, not the compiled
  `…ListModule.Fold`.** NameResolution / Unification look the provider up with
  the source-form qualified name (`qualName = "List.fold"`), so that is what the
  `External` carries and what `MockBuiltins` registers. `ClrProvider.TryEmitCall`
  matches `compiledName = "List.fold"` exactly (avoiding the `Array`/`Seq.fold`
  collision the plan worried about) before emitting the `ListModule::Fold`
  member ref. When the lib-backed provider lands with real compiled names this
  becomes a 1-line key change.
- **`MethodBodyStreamEncoder` must be created once.** Its ctor requires a
  4-byte-aligned IL builder; a fresh encoder per method body (the old
  `ctx.BodyStream` property pattern) throws on the second body once a tiny body
  leaves the builder unaligned. `assembleProgram` creates one encoder and reuses
  it (`AddMethodBody` realigns per body).

## Cross-references

- [codegen-clr-part-4](codegen-clr-part-4.md) — slice 4 (list construction).
  The on-disk runnable-app bundle (`materialiseApp` / `runOnDisk`) the capstone
  reuses landed alongside it (no separate doc; see `RunnableAppTests.fs`).
- [codegen-clr-part-3](codegen-clr-part-3.md) — the inline expansion (`sum`)
  and `EmitEnv` this slice composes with.
- [codegen-clr-part-2](codegen-clr-part-2.md) — the `FSharpFunc.Invoke`
  consumption path (`emitInvoke`); slice 5 supplies the values it consumes.
- [function-representation-plan](function-representation-plan.md) — the closure
  representation: v1's `FSharpFunc` subclass vs. the `Fun` destination.
- [il-emission-roadmap](il-emission-roadmap.md) §Thin-slice ordering — slice 5
  is the roadmap's final slice; after it the canonical sample runs.
- [backend-design-plan](backend-design-plan.md) §"Lowering split" — why closure
  *synthesis* stays in the CLR backend while the `Fun` canonicalisation is
  universal.
- [front-end-gaps-plan](front-end-gaps-plan.md) §A/§C — the list freeze and the
  inline retention this slice consumes.
