# XParsec.FSharp.Codegen.Clr — slice 4 plan

Follow-on to [codegen-clr-part-3](codegen-clr-part-3.md), whose thin-slice #3
(`let inline succ x = x + 1` / `printfn "%d" (succ 41)`) is implemented and
passing. This doc plans [il-emission-roadmap](il-emission-roadmap.md)'s
**thin-slice #4**:

```fsharp
printfn "%A" [1; 2; 3]
```

It assumes the part-2/3 file layout (`Types`, `Cil`, `CilBuilder`, `Metadata`,
`ICodegenProvider`, `ClrProvider`, `Emit`, `Codegen`) and all the part-2/3
deviations carry forward (host-derived assembly identities; the untyped
depth-tracked walker; `Main`-local slots keyed by binding `NodeKey`; the
`Emit`-closure `CallRecipe`; the `FSharpFunc.Invoke` consumption path; the
`EmitEnv` record + inline-expansion machinery).

## The headline: slice 4 is list *construction*

The new thing is building an `FSharpList<int>` value at runtime from the frozen
`UnionCons` chain — emitting the FSharp.Core list constructors as IL. The list
literal `[1; 2; 3]` already freezes (front-end §A) to a nested
`UnionCons("Cons", [head; tail])` chain terminated by `UnionCons("Nil", [])`
over `TyRecord("Microsoft.FSharp.Collections.list", [int])`. Slice 4 lowers each
node:

```
UnionCons("Nil", [], list<int>)        ──►  call  FSharpList`1<int>::get_Empty()
UnionCons("Cons", [h; t], list<int>)   ──►  <emit h>; <emit t>; call FSharpList`1<int>::Cons(!0, FSharpList`1<int>)
```

so `[1; 2; 3]` emits as `ldc 1; ldc 2; ldc 3; call get_Empty; call Cons; call
Cons; call Cons` (innermost-first, the natural post-order of the chain) leaving
one `FSharpList<int>` on the stack.

> **The `printfn "%A" (…)` wrapper is unchanged from slices 2–3.** `%A` is just
> another printer: the format types as
> `PrintfFormat<(list<int> -> unit), TextWriter, unit, unit>`, the
> `PrintFormatLine<T>` call instantiates `T = FSharpFunc<list<int>, unit>`, and
> the constructed list is applied through `FSharpFunc::Invoke` exactly as slice 2
> applied an `int`. The structured formatter that turns the list into the text
> `[1; 2; 3]` lives entirely inside FSharp.Core at runtime — codegen never sees
> it. So the only thing `%A` adds over `%d` is a *list-typed* generic argument,
> which is covered by the one `encodeType` case below.

The consequence, stated up front:

> **Slice 4 touches no `Cil`, `Metadata`, or `Codegen` code.** The static
> `call` is expressed through the existing `CallRecipe.Emit` closure (the same
> shape `printfn`/arithmetic already use), and `Metadata`'s `TypeRef` /
> `MemberRef` / `TypeSpec` helpers already cover everything. The work is one new
> provider hook (`TryEmitUnionCons`), one `encodeType` case + two recipe
> builders in `ClrProvider`, and one `TExpr.UnionCons` arm in `Emit`.

## What the sample demands that slice 3 lacks

| Need | New mechanic |
|---|---|
| `list<int>` in a signature slot (the `%A` printer's arg, the format's type-arg, a list-typed local) | `encodeType` case: `TyRecord("…list", [e])` → `FSharpList\`1<e>`. |
| `[…]` constructed at runtime | `TExpr.UnionCons` arm in `Emit` → a new `TryEmitUnionCons` provider hook. |
| `Cons` cell | `call FSharpList\`1<e>::Cons(!0, FSharpList\`1<e>)` (public static method) — member ref against the instantiated `TypeSpec`. |
| `[]` / `Nil` | `call FSharpList\`1<e>::get_Empty()` (the empty singleton; the parameterless ctor is `internal`, so `get_Empty` is the only public empty path). |

## Scope: lists only, monomorphic element

The sample's element type is `int` (forced by the literal). The element type is
read off the frozen `TyRecord("…list", [elemTy])` and threaded into both the
`TypeSpec` parent and the member signatures — no inference, no typar derivation
(an empty `[]` whose element type isn't pinned by context would ship a free
`TyVar` and is out of scope; `printfn "%A" []` is *not* codegen-clean for that
reason). **Arrays are deferred.** `[|1; 2; 3|]` freezes to
`App(External "Microsoft.FSharp.Collections.ArrayModule.OfList", <list-chain>)`
([front-end-gaps-plan](front-end-gaps-plan.md) §A), so it reuses this slice's
list construction but additionally needs a generic-method recipe for
`ArrayModule.OfList<T>` — a small follow-on once the list path is proven.
User-defined DU constructors also flow through `TExpr.UnionCons`; the provider
returns `ValueNone` for any non-list type and `Emit` fails loudly (a later
slice).

## TAST shapes (what `Emit` will walk)

Grounded in `FreezeTests.fs` (`let xs = [1; 2; 3]`) and verified by analysing
the full sample (diagnostic-clean):

```
TDecl.Expression(
  App(App(External "printfn",
          New("Microsoft.FSharp.Core.PrintfFormat", [Const(String "%A")],     // : PrintfFormat<(list<int>->unit),TextWriter,unit,unit>
              TyClass("…PrintfFormat", [TyFun(list<int>, unit); TextWriter; unit; unit]))),
      UnionCons("Cons",
                [ Const(Int 1)
                  UnionCons("Cons",
                            [ Const(Int 2)
                              UnionCons("Cons",
                                        [ Const(Int 3); UnionCons("Nil", [], list<int>) ],
                                        list<int>) ],
                            list<int>) ],
                list<int>),                                                    // : list<int>
      unit),
  unit)
```

where `list<int>` ≡ `TyRecord("Microsoft.FSharp.Collections.list", [TyConst "int"])`.

Read off the nodes:
- The whole expression is a `TDecl.Expression` (a top-level `do`), lowered by
  `emitStatement` exactly as slice 2's `printfn` was.
- `printfn` is an `External` (slice-2 recipe path, unchanged); the format `New`
  and the trailing-arg `Invoke` are unchanged — the trailing arg just happens to
  be the `UnionCons` chain instead of a `Var`.
- Each `Cons`/`Nil` carries its `list<int>` type inline; `encodeType` and the
  recipe builders read the element type from its single `TyRecord` arg.

## Design changes, file by file

### `ICodegenProvider.fs`
- Add one hook, mirroring the symbol provider's union knowledge:

  ```fsharp
  /// Resolve a union-case constructor (as carried by `TExpr.UnionCons`) to a
  /// call recipe. `tyArgs` are the union type's instantiation arguments (for
  /// `list<int>`, `[int]`); the field values are already on the stack in
  /// declaration order beneath the call.
  abstract TryEmitUnionCons: typeName: string * caseName: string * tyArgs: SemType list -> CallRecipe voption
  ```

  A `CallRecipe` (not a new shape): `Cons` is `ArgCount = 2, Pushes = 1`; `Nil`
  is `ArgCount = 0, Pushes = 1`. Both `Emit` a static `call` — the existing
  closure shape covers it.

### `ClrProvider.fs`
- A cached `TypeRef`: `eFSharpList1 = ctx.TypeRef(fsCoreRef,
  "Microsoft.FSharp.Collections", "FSharpList\`1")`, and a `listTypeName`
  constant `"Microsoft.FSharp.Collections.list"` (the abbreviation name the
  freeze hard-codes — [front-end-gaps-plan](front-end-gaps-plan.md) §A; minting
  the type properly from the provider is a separate extract-symbols follow-up).
- `encodeType`: add
  `TyRecord(name, [elem]) when name = listTypeName` →
  `GenericInstantiation(eFSharpList1, 1)` with `elem`. This single case serves
  every list-typed slot: the `%A` printer's `FSharpFunc` arg, the
  `PrintfFormat` ctor `TypeSpec`, the `PrintFormatLine`/`Invoke` instantiations,
  and any list-typed local signature.
- Two recipe builders, both minting a member ref against the
  `FSharpList\`1<elem>` `TypeSpec` (the exact `emitInvoke` / `emitPrintfFormatCtor`
  pattern), with signatures written in terms of the *declaring type's* generic
  parameter `!0` (`GenericTypeParameter(0)`):
  - `emitListCons elem` → static method `Cons`, signature
    `FSharpList\`1<!0> (!0, FSharpList\`1<!0>)`; `Emit = call`, `ArgCount = 2`,
    `Pushes = 1`.
  - `emitListNil elem` → static method `get_Empty`, signature
    `FSharpList\`1<!0> ()`; `Emit = call`, `ArgCount = 0`, `Pushes = 1`.
- Wire `TryEmitUnionCons`: when `typeName = listTypeName`, dispatch
  `"Cons" → emitListCons`, `"Nil" → emitListNil` (`zonk`-ing the element type
  first); otherwise `ValueNone`.

### `Emit.fs`
- Add the `TExpr.UnionCons` arm — emit each field, then run the recipe:

  ```fsharp
  | TExpr.UnionCons(caseName, args, ty) ->
      let typeName, tyArgs =
          match ty with
          | TyRecord(n, xs)
          | TyUnion(n, xs) -> n, xs
          | other -> failwithf "Emit: UnionCons with non-union type %A" other

      for a in args do
          emitExpr env il a

      match env.Provider.TryEmitUnionCons(typeName, caseName, tyArgs) with
      | ValueSome recipe -> applyRecipe il recipe
      | ValueNone -> failwithf "Emit: no union-cons recipe for %s.%s" typeName caseName
  ```

  Recursion through the `tail` field handles the nested chain; `applyRecipe`
  settles depth by `Pushes - ArgCount` exactly as the `External`/`Invoke` paths
  do. (List is a `TyRecord` in the current freeze; the `TyUnion` arm is there
  for the user-DU generalisation, which still hits `ValueNone` in v1.)

### unchanged
`Types.fs`, `Cil.fs`, `CilBuilder.fs`, `Metadata.fs`, `Codegen.fs` — no edits.
The static `call` rides the existing `CallRecipe.Emit` closure; `Metadata`'s
`TypeRef`/`MemberRef`/`TypeSpec` already cover the generic member ref.

## Build order and testable staging

Each milestone ends with a runnable assertion, per the repo's thin-slice
discipline.

1. **Single-element list (end-to-end).** Add the `encodeType` case, the two
   recipe builders, `TryEmitUnionCons`, and the `Emit` arm. Test
   `printfn "%A" [1]` → stdout `[1]`. The smallest construction that exercises
   one `Cons` over a `Nil` plus the list-typed `Invoke`.
2. **Multi-element chain (end-to-end).** Test the full sample
   `printfn "%A" [1; 2; 3]` → stdout `[1; 2; 3]`. Proves the recursive `tail`
   emission and the post-order `call` sequencing.
3. **List-typed local (end-to-end).** Test `let nums = [1; 2; 3]` /
   `printfn "%A" nums` → `[1; 2; 3]`. Proves `encodeLocalSignature` encodes a
   reference-typed (`FSharpList\`1<int>`) local and that `stloc`/`ldloc`
   round-trips it — the `nums` binding of the canonical sample.

(1) proves the construction + the list-as-`Invoke`-arg path; (2) proves the
chain; (3) proves a list-typed slot.

## Verified facts

Confirmed against the loaded FSharp.Core and by analysing the sample with
`MockBuiltins`:

- `Microsoft.FSharp.Collections.FSharpList\`1` exposes a **public static**
  `FSharpList<T> Cons(T, FSharpList<T>)` and a **static** `FSharpList<T>
  get_Empty()` (the `Empty` property getter). The parameterless `.ctor()` is
  `internal` (`Assembly` visibility); the public `.ctor(T, FSharpList<T>)` is an
  alternative cons path (see Open questions). So `Cons` + `get_Empty` are the
  chosen public surface.
- `printfn "%A" [1; 2; 3]` analyses **diagnostic-clean**: `printfn` is an
  `External` typed
  `PrintfFormat<(list<int>->unit),TextWriter,unit,unit> -> (list<int> -> unit)`;
  the format `New`'s type carries `TyFun(list<int>, unit)` as its printer slot;
  the trailing arg is the `Cons`/`Nil` chain typed `list<int>`; the result is
  `unit`. The `%A` (`Structured`) typar resolves to `list<int>` at the call, so
  `ResolvedTypes` is satisfied.
- `[1; 2; 3]` freezes as the nested `UnionCons("Cons", …)` /
  `UnionCons("Nil", [])` chain over
  `TyRecord("Microsoft.FSharp.Collections.list", [TyConst "int"])`
  (`FreezeTests.fs`).

## Out of scope (still)

- **Arrays.** `[|…|]`'s extra `ArrayModule.OfList<T>` generic-method call —
  next, on top of this slice's list path.
- **Empty/free-element list literals.** `[]` with no pinning context ships a
  free element `TyVar`; not codegen-clean.
- **User-defined DU construction.** Other `TExpr.UnionCons` types (`Circle 1.0`,
  …) — the hook returns `ValueNone` and `Emit` fails loudly; a later slice mints
  the DU type + ctors from the provider.
- **List `match` / deconing** (`List.head`, pattern matching a `::`) — slice 4 is
  construction-only; consumption beyond `%A`'s reflection-driven printer is
  later.
- **`List.fold` + `(+)`-as-value** (slice 5) and the closure-synthesis path it
  needs.

## Open questions / decisions

- **`Cons` static method, decided.** Use the public static `Cons` method +
  `Empty` property — exactly what the F# compiler itself emits (`[1; 2]`
  decompiles to `FSharpList<int>.Cons(1, FSharpList<int>.Cons(2,
  FSharpList<int>.Empty))`). It is symmetric with the empty case (both static
  `call` — `Empty`'s getter is `call get_Empty()` — under one
  `TryEmitUnionCons` hook returning `CallRecipe`). The public
  `.ctor(T, FSharpList<T>)` via `newobj` is runtime-equivalent but the compiler
  doesn't use it, and it would split the empty case (a `call`) from the cons
  case (a `newobj`) across two recipe shapes for no gain.
- **`encodeType` keys on the abbreviation name.** Matching `listTypeName`
  string-wise mirrors how the freeze refers to the type. When the extractor
  mints `FSharpList` properly (the §A follow-up), this becomes a registered
  `TyUnion` and the case can key on that instead — additive, one line.
- **Where union-cons emission lives.** A dedicated `TryEmitUnionCons` hook, not
  an overload of `TryEmitCtor` (which is `newobj`-shaped via `CtorRecipe`) — the
  list constructors are static `call`s, so the `CallRecipe` shape fits and the
  hook stays union-semantic.
- **Local slot reuse / list-typed locals.** Still deferred; a list-typed local
  is just another `DeclareLocal ty` whose signature `encodeType` now handles.

## Cross-references

- [codegen-clr-part-3](codegen-clr-part-3.md) — slice 3; the `EmitEnv` +
  inline + `printfn`/`Invoke` mechanics this slice reuses unchanged.
- [codegen-clr-part-2](codegen-clr-part-2.md) — slice 2; the recipe / `Invoke` /
  `encodeType` machinery the list construction plugs into.
- [il-emission-roadmap](il-emission-roadmap.md) §Thin-slice ordering — slice 4
  in the progression.
- [front-end-gaps-plan](front-end-gaps-plan.md) §A — the list-literal freeze
  (`UnionCons` chain over `list<int>`) this slice consumes.
- [backend-design-plan](backend-design-plan.md) — provider posture for the
  union-cons hook.
