# Key-threaded nominal resolution plan

Close the `simpleName`-for-lookup trap **structurally**: make the
project-local nominal-resolution surface accept a `SymbolKey` (the type's
entity identity) instead of a bare `(name: string, args)` pair, so the
generic arity rides the key and can never be dropped-then-reconstituted.

This is the follow-through on the arity-overloading fixes that renamed
`Fun2 → Fun` and routed the by-key lookups (`tryClassByKey` /
`tryUnionByKey` / `tryRecordByKey` / `tryInterfaceImplHostByKey`,
`tryClassMemberByKey`, `tryOwnStaticOp`). Those fixed the call sites that
resolved *directly* by bare name. This plan removes the remaining
**indirect** form: helpers that take a bare name plus `args`, recover the
arity as `args.Length`, and re-key internally.

## The smell

A `SymbolKey` carries `(asm, ns, name`arity)` — arity is part of type
identity, exactly like the compiler's `TyconRef` entity. The chain
walkers throw that identity away and rebuild it:

```fsharp
// EngineCore.fs — tryClassChainMemberDecl
let rec walk (clsName: string) (args: EqArray<SemType>) =
    let arityKey = SymbolKeyOps.arityName clsName args.Length   // rebuild the key…
    match TypeRegistry.tryClassArity ctx.Types clsName args.Length with
    | ValueSome info ->
        …
        | TyClass(parentKey, parentArgs) ->
            walk (SymbolKeyOps.simpleName parentKey) parentArgs  // …strip it again on recursion
```

Even inside the walker, which *holds* `parentKey`, the code strips it to
`simpleName` only to re-key by `arityName + parentArgs.Length` on the next
iteration. It is correct **only** while the unchecked invariant
`key.arity = args.Length` holds. Every caller currently satisfies it, so
this is a latent-fragility cleanup, not a live bug — hence a plan doc
rather than an inline fix.

The forcing functions are the two chain walkers plus the `DotSource`
label, all of which take `(name, args)`:

- `tryClassChainMemberDecl` / `tryClassChainMember` — `EngineCore.fs:305,344`
- `DotSource.ClassChain of name: string * args` — `Engine.fs:82`

Because they take a bare name, every caller is *forced* to produce one via
`SymbolKeyOps.simpleName`, which is what keeps that projection wired into
lookup paths.

## Goal

After this lands:

- `tryClassChainMemberDecl` / `tryClassChainMember` take
  `(ctx) (clsKey: SymbolKey) (args) (memberName)`. Internally they resolve
  via `tryClassByKey` (not `tryClassArity ctx name args.Length`), and the
  base-type recursion passes `parentKey` straight through — no
  `simpleName`, no `arityName` round-trip.
- `DotSource.ClassChain` carries `key: SymbolKey` instead of
  `name: string`. The membership gate becomes `tryClassByKey` /
  `containsClassKey` rather than `containsClass ctx name args.Length`.
- The only remaining `SymbolKeyOps.simpleName` uses are the ones where
  dropping arity is semantically correct: **diagnostics** and the
  **JS/CLR name emitters**. `simpleName` fed into any registry lookup is
  gone, matching the docstring warning it now carries.

Non-goal: touching `simpleName`'s legitimate display/emit callers, the
external-provider paths (they key by `qualifiedName`, which retains the
suffix), or the abbreviation registry (bare-keyed, never overloaded).

## Call-site inventory (what moves)

Chain-walker callers — today pass `simpleName`/label + args, would pass the key:

| Site | Today | After |
|------|-------|-------|
| `Freeze/Resolve.fs:792` | `tryClassChainMemberDecl ctx (simpleName clsKey) args segName` | `… ctx clsKey args segName` |
| `InferControlFlow.fs:549,550` | `tryClassChainMember ctx ifaceName ifaceArgs "MoveNext"/"Current"` | `… ctx ifaceKey ifaceArgs …` |
| `InferControlFlow.fs:592` | `tryClassChainMember ctx ifaceName ifaceArgs "GetEnumerator"` | `… ctx ifaceKey ifaceArgs …` |
| `InferRecordAccess.fs:217` | `tryClassChainMember ctx ifaceName ifaceArgs memberName` | `… ctx ifaceKey ifaceArgs …` |
| `InferRecordAccess.fs:266` | `tryClassChainMember ctx clsSimple args memberName` | `… ctx clsKey args memberName` |
| `Engine.fs:448` | `tryClassChainMember ctx name args d.MemberName` | `… ctx key args d.MemberName` (from `ClassChain`) |

At each of `InferControlFlow.fs:541/585` and `InferRecordAccess.fs:209`
the `let ifaceName = SymbolKeyOps.simpleName ifaceKey` line survives only
if `ifaceName` is still needed for a diagnostic; otherwise it deletes. The
key (`ifaceKey`) is already in scope at all of them.

`DotSource` producer/consumer — `Engine.fs:93–109` and `Engine.fs:440–448`:
the `NominalKind.Class` arm already has `key` in hand and computes
`let name = simpleName key` purely to build `ClassChain(name, …)`; after
the change it stores `key` and the `name` local drops (or stays only for
the `UnknownType`/diagnostic arms).

## Steps

1. **Walkers first.** Change `tryClassChainMemberDecl` to take `clsKey`,
   resolve via `tryClassByKey`, and recurse with `parentKey` directly.
   `tryClassChainMember` forwards unchanged. This is the linchpin: once the
   walker takes a key, every caller *must* hand it one, so the compiler
   drives the rest.
2. **Fix the fallout** at the six caller sites above, deleting each now-dead
   `simpleName` local (keep the ones a diagnostic still reads).
3. **`DotSource.ClassChain`** → carry `SymbolKey`; update the producer
   (`Engine.fs:108`) and consumer (`Engine.fs:448`). Consider a
   `containsClassKey` helper (or reuse `tryClassByKey … |> ValueOption.isSome`)
   for the membership gate so no bare-name `containsClass` remains on this path.
4. **Sweep** `grep 'SymbolKeyOps.simpleName'` and confirm every survivor is a
   diagnostic string or a JS/CLR emit. Anything feeding a `TypeRegistry.*`
   lookup is a regression.
5. **Soft follow-up (optional):** the base-type cycle check at
   `MemberRegistration.fs:855` compares `simpleName parentKey = start.Name`;
   an arity-overloaded self-inheritance (`Foo`2` : `Foo`3`) would
   false-positive. Compare arity-qualified names (or keys) instead.

## Invariants / risks

- **`tryClassArity ctx name args.Length` vs `tryClassByKey key`** are
  equivalent *iff* `key.arity = args.Length`. The whole point is to stop
  relying on that coincidence; while migrating, keep both only long enough
  to diff behavior, then delete the arity-string path from these walkers.
- **`seen`-set dedup** in `walk` is keyed on the `arityName` string today;
  switch it to the `SymbolKey` (or its `qualifiedName`) so cycle detection
  still terminates on the key identity, not a reconstructed string.
- **External receivers** never reach these walkers (they branch on
  `qualifiedName` earlier), so no external path changes.
- Verification: `XParsec.FSharp.SemanticAnalysis.Tests` and
  `XParsec.FSharp.Codegen.Clr.Tests` (the chain walk feeds
  `ClassChainMethod` lowering). Add a focused test with an
  arity-overloaded local class carrying instance members on both arities
  (`type Box<'a> = …` + `type Box<'a,'b> = …`, both with a method of the
  same name) to lock in that the walk resolves per-arity — the scenario the
  current `args.Length` reconstruction happens to get right but doesn't
  *guarantee*.

## Definition of done

`SymbolKeyOps.simpleName` has zero call sites that flow into a
`TypeRegistry` lookup; the chain walkers and `DotSource.ClassChain` speak
`SymbolKey`; the arity round-trip at `EngineCore.fs:337` is gone. Delete
this doc when it lands.
