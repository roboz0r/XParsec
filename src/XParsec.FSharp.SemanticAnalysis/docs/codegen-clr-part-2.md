# XParsec.FSharp.Codegen.Clr — slice 2 plan

Follow-on to [codegen-clr-plan](codegen-clr-plan.md), whose thin-slice #1
(`printfn "hi"`) is implemented and passing. This doc plans
[il-emission-roadmap](il-emission-roadmap.md)'s **thin-slice #2**:

```fsharp
let x = 1 + 2
printfn "%d" x
```

It assumes the part-1 file layout (`Cil`, `Metadata`, `ICodegenProvider`,
`ClrProvider`, `Emit`, `Codegen`) and grows node coverage rather than
re-architecting. Read part 1's *Implementation status* section first — its
deviations (host-derived assembly identities; untyped depth-tracked walker;
`abstract sealed` holder class) all carry forward.

## Implementation status

**Thin-slice #2 is implemented and passing end-to-end.** All three build
milestones below are green: the `Cil` arithmetic/locals unit tests, the
`printfn "%d" (1 + 2)` function-consumption test, and the full
`let x = 1 + 2` / `printfn "%d" x` sample (`test/XParsec.FSharp.Codegen.Clr.Tests/Slice2Tests.fs`,
`CilTests.fs`). Decisions taken where the plan left a choice:

- **`CallRecipe` carries `Emit: Il -> unit`** (replacing `Handle`), uniformly
  expressing a metadata `call`/`callvirt` or a bare intrinsic opcode. `ArgCount`
  is the count of stack values *consumed* and `Pushes` the count produced; the
  walker settles depth by `Pushes - ArgCount` via one `applyRecipe` helper.
- **`Invoke` arg accounting** (the doc's open question): the recipe counts the
  receiver `FSharpFunc` *in* `ArgCount` (= 2), so the single `applyRecipe`
  helper needs no `Invoke` special-case — chosen over the "ArgCount = 1, walker
  tracks the receiver" alternative for a uniform depth rule.
- **Top-level `let`s are `Main` locals** (script-style), keyed by binding
  `NodeKey` in a `Dictionary` local to the `Main` build; the local-variable
  signature is built by the provider's `EncodeLocalSignature` and threaded
  through `Cil.buildBody`.
- **Test isolation:** the in-process `runEntryPoint` capture is `lock`-guarded,
  since `Console.Out` is process-global and Expecto runs tests in parallel.

## What the sample demands that slice 1 lacks

| Need | New mechanic |
|---|---|
| `1 + 2` | `op_Addition` as a CIL `add` **intrinsic** (no metadata ref). |
| `let x = …` used later | local-slot allocation + a `NodeKey → slot` side table. |
| `x` (a `Var`) | `ldloc` from the slot table. |
| `printfn "%d" x` | the `%d` printer is a **function value** (`int -> unit`): the printf call returns an `FSharpFunc<int,unit>` that the trailing arg is applied to via `callvirt Invoke`. |
| curried `App(App(f, a), b)` | spine peeling — slice 1 only handled `App(External, arg)` with one arg. |

## The function-value subtlety (read this first)

Slice 1 got away with `printfn "hi"` because, with no format args, the printer
type is `unit`: `PrintFormatLine<unit>(format)` returns a `Unit` we just pop.

`printfn "%d" x` is different. `printfn "%d"` has type `int -> unit`, so:

```
ldstr "%d"
newobj   PrintfFormat<FSharpFunc<int,unit>, TextWriter, Unit, Unit>::.ctor(string)
call     !!0 PrintfModule::PrintFormatLine<FSharpFunc<int,unit>>(PrintfFormat<!!0,…>)
         ; ^ leaves an FSharpFunc<int,unit> on the stack
ldloc    x
callvirt instance !1 FSharpFunc`2<int,unit>::Invoke(!0)   ; → Unit
pop                                                        ; discard Unit
```

This is the project's **first encounter with the function-representation
problem** ([function-representation-plan](function-representation-plan.md)) —
but only the *consumption* half: we `callvirt Invoke` on an `FSharpFunc` that
FSharp.Core handed us. We **do not synthesise** a closure; passing `(+)` as a
value to `List.fold` (slice 5) is the synthesis half and stays out of scope.
Keeping this boundary sharp is the main design point of slice 2.

Concretely, the only `FSharpFunc` machinery slice 2 needs is:
- encode `SemType.TyFun(a, b)` → `Microsoft.FSharp.Core.FSharpFunc\`2<a, b>`;
- a `callvirt` to `FSharpFunc\`2::Invoke` (member ref against a `TypeSpec`
  parent, signature `instance !1 (!0)` — the same TypeSpec-parent pattern the
  `PrintfFormat` ctor already uses in `ClrProvider`).

## TAST shapes (what `Emit` will walk)

`let x = 1 + 2` freezes to its own decl, and the body to a second decl (a
module-level `let … in body` does **not** fold into one `TExpr.Let` —
[[reference_module_let_in_splits]]):

```
TDecl.Let(
  TPat.NamedSimple(kx, int),
  App(App(External "op_Addition", Const(Int 1)), Const(Int 2)),   // : int
  isInline=false, int)

TDecl.Expression(
  App(App(External "printfn", New("…PrintfFormat", [Const(String "%d")])),  // : int -> unit
      Var(kx, int)),                                                        // : unit
  unit)
```

Key types to read off the nodes (the walker needs them, no re-inference):
- `App(External "printfn", New …)` has type `TyFun(int, unit)` — the
  `FSharpFunc` instantiation for both `PrintFormatLine<T>` and the `Invoke`.
- The `New PrintfFormat`'s `ty` is `TyClass(printfFormatName, [TyFun(int,unit); TextWriter; unit; unit])` — so the ctor TypeSpec already exercises the new `TyFun` encoding.
- `Var(kx, int)` carries the **binding** NodeKey `kx`; the matching
  `TPat.NamedSimple(kx, _)` is where the slot is allocated.

## Design changes, file by file

### `Cil.fs`
- Typed + untyped ops: `sub`, `mul` (alongside the existing `add`); `stloc` /
  `ldloc` (`InstructionEncoder.StoreLocal` / `LoadLocal`); `callvirt1`
  (`il.Encoder.OpCode(ILOpCode.Callvirt); il.Encoder.Token h` — note SRM has
  no `Callvirt` helper, unlike `Call`). Depth deltas as today.
- Local variables: `Il` grows a `Locals: ResizeArray<SemType>` and
  `DeclareLocal(ty) : int` returning the slot index. `buildBody` finalisation
  must build the local-variable signature and pass it to `AddMethodBody`
  (currently it passes only `maxStack`). Because encoding a `SemType` needs
  the provider's type refs, thread an `encodeLocals: SemType list ->
  StandaloneSignatureHandle` callback into `buildBody` rather than pulling BCL
  knowledge into `Cil`.

### `Metadata.fs`
- `AddStandaloneSignature(blob) : StandaloneSignatureHandle` for the locals
  sig. (`BlobEncoder.LocalVariableSignature` builds the blob.)
- No other rows are new — `FSharpFunc\`2` is just another cached `TypeRef`,
  and `Invoke` another `MemberRef` against a `TypeSpec` parent.

### `ICodegenProvider.fs`
- Generalise `CallRecipe` so it can express an **intrinsic** (emit an opcode,
  no handle) as well as a metadata call. Replace `Handle: EntityHandle` with
  `Emit: Il -> unit` — the recipe performs the call/intrinsic with args
  already pushed; the walker keeps using `ArgCount` / `Pushes` for depth. This
  is the `EmitRecipe` shape the original plan sketched, now actually needed.
- Add `EncodeLocalSignature: SemType list -> StandaloneSignatureHandle` (for
  `Cil.buildBody`'s callback) and `TryEmitInvoke: funcTy: SemType ->
  CallRecipe voption` (the `FSharpFunc.Invoke` for a `TyFun(a,b)`).

### `ClrProvider.fs`
- `encodeType`: add the `TyFun(a, b)` case → `GenericInstantiation` of the new
  `FSharpFunc\`2` ref. (This alone makes the slice-1 `printfn` paths handle a
  non-`unit` printer, since they already pipe types through `encodeType`.)
- `TryEmitCall`: add `op_Addition` → `{ Emit = fun il -> il.Encoder.OpCode
  ILOpCode.Add; ArgCount = 2; Pushes = 1 }`; likewise `op_Subtraction` (`Sub`)
  and `op_Multiply` (`Mul`). Migrate the existing `printfn` recipe to the
  `Emit`-closure form.
- `TryEmitInvoke`: member ref `instance !1 FSharpFunc\`2<!0,!1>::Invoke(!0)`
  against the `TypeSpec` for `FSharpFunc<a,b>`; `Emit = callvirt`,
  `ArgCount = 1` (the applied arg; the receiver func is already on the stack
  beneath it — model it as the receiver not counted, see Open questions),
  `Pushes = 1`.

### `Emit.fs`
- `collectSpine : TExpr -> TExpr * (TExpr * SemType) list` — peel an `App`
  chain into its head and the args paired with each `App` node's result type
  (the result types drive `Invoke` instantiation).
- Rewrite the `App` arm:
  1. If the head is `External name` and `TryEmitCall name resultTy` resolves
     (`resultTy` = the result type after the recipe's `ArgCount` args), emit
     those leading args, run the recipe, then **fold each remaining arg**
     through `TryEmitInvoke` using the running function type.
  2. Otherwise the head is itself a function value: emit it, then fold every
     arg via `TryEmitInvoke`.
- `TExpr.Var(kx, _)` → `ldloc (slots.[kx])`.
- `TDecl.Let(NamedSimple(kx,_), value, _, ty)` in `emitMain`: `DeclareLocal
  ty`, record `slots.[kx]`, emit `value`, `stloc`. Keep the `Dictionary<NodeKey,int>`
  slot table local to the `Main` build (side-table posture,
  [backend-design-plan](backend-design-plan.md)).
- Top-level lets become **`Main` locals** in slice 2 (script-style), not
  static fields — defer the static-field / `.cctor` split until a slice needs
  cross-method visibility, exactly as part 1's entry-point lowering noted.

### `Codegen.fs`
- `assembleWith` already centralises the body build — thread the provider's
  `EncodeLocalSignature` into `Cil.buildBody`. No structural change.

## Build order and testable staging

Each milestone ends with a runnable assertion, mirroring the repo's thin-slice
discipline.

1. **`Cil` arithmetic + locals (unit-level).** Add the ops + local-sig
   plumbing. Extend `CilTests` with a hand-written typed-`Op` body
   (`ldcI4; ldcI4; add; stloc; ldloc; ret`) returning the sum as the process
   exit code. No TAST, no provider.
2. **Arithmetic + function consumption (end-to-end).** Intrinsics + spine
   peeling + `TyFun` encoding + `Invoke`. Test `printfn "%d" (1 + 2)` →
   stdout `3` (exercises both spines and `callvirt Invoke`, **no locals**).
3. **Locals (end-to-end).** `let`/`Var` lowering + the local signature. Test
   the full sample `let x = 1 + 2` / `printfn "%d" x` → stdout `3`.

(2) is the milestone that proves the function-value consumption path; (3) is a
small addition on top.

## Verified FSharp.Core facts

Confirmed by reflection against the loaded FSharp.Core (same method part 1
used for `printfn`):

- `Microsoft.FSharp.Core.FSharpFunc\`2` — abstract; `abstract virtual Invoke(T)
  : TResult`. So `callvirt instance !1 FSharpFunc\`2<!0,!1>::Invoke(!0)`.
- `OptimizedClosures.FSharpFunc\`N` exist for tupled multi-arg printers but are
  **not** needed: a single-`%d` printer is a plain `FSharpFunc\`2`.
- `PrintfModule.PrintFormatLine<T>(PrintfFormat\`4)` and `PrintfFormat\`4::.ctor(string)`
  are unchanged from slice 1 — only `T` / the first type-arg differ
  (`FSharpFunc<int,unit>` instead of `unit`).

## Out of scope (still)

- Closure **synthesis** — passing a function as a value
  ([function-representation-plan](function-representation-plan.md)). Slice 2 is
  consumption-only.
- Multi-arg printers via `OptimizedClosures` (`printfn "%d %d" a b` curries one
  `FSharpFunc` per arg in v1; the optimised tupled path is a later perf item).
- `inline` expansion (slice 3), list literals (slice 4), `List.fold` (slice 5).
- Static fields / `.cctor` for top-level lets — deferred while `Main`-local
  lowering suffices.
- Float / int64 arithmetic beyond what the same `add`/`sub`/`mul` opcodes give
  for free (the intrinsics are operand-type-agnostic at the IL level; what is
  gated is `MockBuiltins` only typing `op_Addition` at `int`).

## Open questions / decisions

- **`Invoke` arg accounting.** The receiver `FSharpFunc` sits on the stack
  beneath the applied arg. Model the recipe as `ArgCount = 1` (the applied
  value) with the walker responsible for the receiver already being present, or
  fold the receiver into the recipe. Prefer the former — it matches how the
  walker already emits a receiver-then-call for `printfn`.
- **Where the spine result types come from.** Reading each `App` node's `ty`
  off the TAST avoids any re-inference; confirm `Freeze` populates intermediate
  `App` types (it does for slice 1's `printfn`). Zonk them through the existing
  `ClrProvider.zonk` before encoding.
- **One `add` op vs typed `add` per numeric type.** CIL `add` is
  operand-typed by what's on the stack, so a single intrinsic suffices for
  `int`; revisit only if a numeric type needs `add.ovf` or conversion.
- **Local reuse.** Slice 2 allocates one slot per `let`; a slot-recycling pass
  (LicenseToCIL's `tmplocal`) is unnecessary until bodies get large.

## Cross-references

- [codegen-clr-plan](codegen-clr-plan.md) — part 1; file layout + slice-1
  status this plan extends.
- [il-emission-roadmap](il-emission-roadmap.md) §Thin-slice ordering — slice 2
  in the overall progression.
- [function-representation-plan](function-representation-plan.md) — the
  function-value problem slice 2 first touches (consumption side only).
- [backend-design-plan](backend-design-plan.md) — side-table posture for the
  local-slot map.
