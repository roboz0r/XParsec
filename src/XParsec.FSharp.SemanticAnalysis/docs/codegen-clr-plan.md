# XParsec.FSharp.Codegen.Clr — plan

The concrete project plan for the first backend. Sister to two existing
docs, which this one assumes:

- [backend-design-plan](backend-design-plan.md) — the *posture* (no
  observable input mutation, side tables keyed by `NodeKey`, the
  `compile` / `materialise` pair, the universal-vs-target lowering split,
  "don't speculate on a second backend").
- [il-emission-roadmap](il-emission-roadmap.md) — *what* must be emitted
  and the thin-slice order to build it in.

This doc fills the gap between them: the project's internal structure, the
two emission layers, what to borrow from LicenseToCIL, and the component
checklist for the first runnable artifact.

## Implementation status

**Thin-slice #1 (`printfn "hi"`) is implemented and passing end-to-end.**
The `src/XParsec.FSharp.Codegen.Clr` project exists with the planned files
(`ProjectInfo`, `Metadata`, `ICodegenProvider`, `ClrProvider`, `Emit`,
`Codegen`); the body DSL is split into `Types` (the `E`/`S`/`Il`/`Op`
fundamentals), `Cil` (the opcode catalogue + `buildBody`), and `CilBuilder`
(the `cil { }` CE), mirroring LicenseToCIL's `Stack`/`Ops`/`CILBuilder`
layout. `test/XParsec.FSharp.Codegen.Clr.Tests` compiles `printfn "hi"`
to an in-memory PE, loads it in-process, invokes `Main`, and asserts stdout
`"hi"` + exit code `0`, plus a standalone `Cil` body-DSL test and an
on-disk `materialise` round-trip. Notes that may shift later expectations:

- **Assembly identities are read from the loaded host assemblies**
  (`typeof<unit>.Assembly`, `typeof<obj>.Assembly`) rather than from a
  configured `FSharp.Core.dll` path, so the emitted references match exactly
  what the in-process loader binds. `ProjectInfo.FSharpCorePath` is carried
  but not yet consumed.
- **The TAST walker emits via untyped, depth-tracked `Cil` helpers**, not the
  phantom-typed `Op` CE: the `compiledName → recipe` dispatch can't preserve
  the stack phantom across the provider boundary. The typed `Op`/`cil { }`
  surface is real and exercised by the `Cil` unit test (hand-written bodies).
- **The holder class is emitted `abstract sealed`** (a static type) so slice 1
  needs no `System.Object::.ctor` member ref.

**Thin-slice #2 (`let x = 1 + 2` / `printfn "%d" x`) is also implemented and
passing** — see [codegen-clr-part-2](codegen-clr-part-2.md). It added arithmetic
intrinsics, `Main`-local slots for top-level `let`s, and the *consumption* half
of the function-representation problem (`callvirt FSharpFunc\`2::Invoke` on the
printer FSharp.Core hands back). **Slice #3 (`inline` expansion) is planned** in
[codegen-clr-part-3](codegen-clr-part-3.md) but not yet implemented. Slices 3–5
(`inline`, list literals, the full `List.fold` sample with closure synthesis)
are the remaining work and grow `Emit`/`ClrProvider` node coverage.

## Status of the prerequisite

Front-end gap closure (`A → D → C → B`) is **done**, so the TAST is
codegen-clean for thin-slices 1–4. The only remaining universal
canonicalisation is closure / eta lowering
([function-representation-plan](function-representation-plan.md)), needed
only for the full sample (slice 5); it can land while the earlier slices
are built.

## Scope (v1)

Emit a runnable .NET assembly for the
[il-emission-roadmap](il-emission-roadmap.md) thin slices, in order, ending
at the canonical sample. v1 references a real `FSharp.Core.dll` at runtime
([[project_dotnet_provider_stack]]). Everything in the roadmap's *Out of
scope* stays out (classes, DUs/records beyond lib-internal, async, CEs,
measures, PDBs, optimisation beyond `inline` + JIT devirt).

## Project layout

New sibling project `src/XParsec.FSharp.Codegen.Clr`, referencing
`XParsec.FSharp.SemanticAnalysis` (for `TastFile` / `TExpr` / `SemType` /
`NodeKey`) and `XParsec.FSharp` (lexer types only if needed). Proposed
files, bottom-up so each layer is independently testable:

| File | Concern |
|---|---|
| `ProjectInfo.fs` | Per-build config record (TFM, output path, signing, FSharp.Core path) — `option`-typed slots per [backend-design-plan](backend-design-plan.md). |
| `Cil.fs` | The phantom-stack-typed method-body DSL (LicenseToCIL-shaped, over `System.Reflection.Metadata`). No TAST dependency. |
| `Metadata.fs` | Assembly / module / type / method / field / signature construction over `MetadataBuilder` + `BlobBuilder`. No TAST dependency. |
| `ICodegenProvider.fs` | Target codegen provider — mirror of `IExternalSymbolProvider`; resolves a compiled name to the handles + emit recipe for a call. |
| `ClrProvider.fs` | The .NET implementation of `ICodegenProvider` (BCL + `FSharp.Core.dll`). |
| `Emit.fs` | The TAST walker: `compile` proper. Owns target-specific lowerings. |
| `Codegen.fs` | `compile` / `materialise` entry points + `ProjectInfo` wiring. |

`Cil.fs` and `Metadata.fs` are independent and parallelisable; `Emit.fs`
needs both plus a provider.

## Public surface

Per [backend-design-plan](backend-design-plan.md), a pair, not an interface:

```fsharp
val compile : IExternalSymbolProvider -> ProjectInfo -> TastFile -> ClrArtifact
val materialise : ClrArtifact -> unit
```

`ClrArtifact` is the in-memory assembled PE (a `BlobBuilder` holding the
serialized image, plus enough to re-serialise / inspect in tests).
`compile` is pure-ish (deterministic given the same inputs); `materialise`
is the only side effect (write PE to `ProjectInfo` output path, or hand the
bytes to an in-process loader for end-to-end tests).

## Two emission layers

LicenseToCIL teaches the body layer and is silent on the metadata layer —
which is exactly the split this project needs.

### A. Method-body DSL (`Cil.fs`) — borrow LicenseToCIL's shape

LicenseToCIL's core is `Op<'stackin,'stackout> = S<'stackin> -> S<'stackout>
-> IL -> unit`, where `S<>` nesting (`E S`, `E S S`) tracks stack *depth*
at the type level, composed by a `cil { }` CE (`Yield` an op, `Combine`
sequences, `let! l = deflabel` / `let! v = deflocal ty` bind, `mark`,
branches). Depth — not element types — is checked; the README's rationale
(runtime-unknown types) applies to us too, and the TAST already carries
`SemType`s for the type-level checking we actually care about.

**Borrow:** the `Op<'in,'out>` phantom-depth encoding, the `cil` CE
(`Yield`/`Combine`/`Delay`/`Run`/`Bind`/`While`/`For`), the one-function-
per-opcode surface (`ldc'i4`, `add`, `ret`, `ldarg`, `stloc`, `call`,
`newobj`, `mark`, `br`, …), and `deflabel` / `deflocal`.

**Differs — the emitter.** LicenseToCIL's `IL` wraps a
`System.Reflection.Emit.ILGenerator` (runtime-only, non-deterministic, no
PE-to-disk). We wrap `System.Reflection.Metadata`'s `InstructionEncoder`
(+ `ControlFlowBuilder` for labels, + a `MetadataBuilder` handle source for
call/newobj tokens, + a local-signature accumulator for `deflocal`):

```fsharp
type Op<'stackin, 'stackout> = S<'stackin> -> S<'stackout> -> Il -> unit
// Il wraps: InstructionEncoder, ControlFlowBuilder, local-sig builder,
// and a handle resolver (delegates to ICodegenProvider / Metadata).
```

Op bodies call `enc.OpCode(ILOpCode.Add)`, `enc.LoadConstantI4 n`,
`enc.LoadString handle`, `enc.Call entityHandle`, `enc.Branch(ILOpCode.Br,
label)`, etc. Finalisation hands the encoded body to
`MethodBodyStreamEncoder.AddMethodBody`, yielding the body offset for the
`MethodDefinition` row.

**Bonus the depth-typing buys us:** `AddMethodBody` needs `maxStack`. The
`cil` runner can track current/peak depth as it threads ops and hand the
peak to `AddMethodBody` — no separate stack-analysis pass. (LicenseToCIL
doesn't need this; `ILGenerator` computes maxstack itself.)

### B. Metadata construction (`Metadata.fs`) — the "different constructs"

No LicenseToCIL analog and no stack typing — just ordered construction of
the metadata graph via `MetadataBuilder`:

- Assembly + module definitions; the `<Module>` type; the `<ModuleName>`
  static class for top-level `let`s.
- `TypeDefinition` / `MethodDefinition` / `FieldDefinition` rows, in the
  order SRM requires (methods/fields contiguous per type, etc.).
- Signatures encoded into `BlobBuilder` (`MethodSignatureEncoder`,
  `LocalSignatureEncoder`, `BlobEncoder`).
- `AssemblyReference` (FSharp.Core, System.Runtime, System.Console),
  `TypeReference`, `MemberReference` rows — cached so repeated refs reuse
  one handle.
- PE assembly: `ManagedPEBuilder` + `MetadataRootBuilder` →
  `Serialize` into the artifact `BlobBuilder`.

Namespaces are a metadata concern (the namespace string on a
`TypeDefinition`), distinct from the body DSL — which is the user-flagged
"types and namespaces deserve different constructs."

## The codegen (target) provider

Mirror of `IExternalSymbolProvider` ([extract-symbols-plan](extract-symbols-plan.md)):
the symbol provider knows a compiled name's *shape*; the codegen provider
knows how to *emit a call* to it.

```fsharp
type ICodegenProvider =
    /// Resolve a compiled name to a handle + emit recipe. Returns the
    /// `Op` that, given already-pushed arguments, performs the call
    /// (`call` / `callvirt` / `newobj` / intrinsic like `add`).
    abstract TryEmit : compiledName: string -> arity: int -> EmitRecipe voption
```

`op_Addition` → an intrinsic `add` op (no metadata ref);
`Microsoft.FSharp.Collections.ListModule.Fold` → a `call` to a member ref
minted against `FSharp.Core.dll`; `printfn` → the BCL `PrintfModule` entry
(see slice 1). The compiled-name table from
[extract-symbols-plan](extract-symbols-plan.md) keys the dispatch, same as
the analysis side. `TExpr.External compiledName` is the only thing the
walker consults the provider for.

## FSharp.Core resolution

v1: an `AssemblyReference` to a real `FSharp.Core.dll` (path in
`ProjectInfo`), and member/type refs minted against it on demand and
cached. The lib's `.fsi` files are signatures only and don't ship IL; the
per-target `.fs` impls ([[project_fsharpcore_clr_needs_prim_types]]) are a
long-term replacement, out of scope for v1.

## Entry-point lowering

Per [il-emission-roadmap](il-emission-roadmap.md) §Entry-point lowering,
option (1): synthesise `[<EntryPoint>] static member Main(argv: string[])
: int`. Effect-free top-level `let`s (`let nums = …`) initialise static
fields; effectful `TDecl.Expression`s (`printfn …`) emit calls in body
order inside `Main`; `Main` ends `ldc.i4.0; ret`. Field init ordering vs.
`Main` is the one subtlety — v1 emits field initialisers as leading
statements in `Main` (script-style), deferring a real `.cctor` split until
ordering-sensitive cases appear.

## The TAST walker (`Emit.fs`)

Walks `TDecl`s in source order, each producing a static member, a field
initialiser, or a contribution to `Main` (per Entry-point lowering). `TExpr`
→ `Op` recursively, writing against the `cil` CE so stack-balance/control-
flow errors are compile-time in *our* code. Per-node emission state (local
slots, captured-var layout, generated refs) lives in `Dictionary<NodeKey,_>`
side tables ([backend-design-plan](backend-design-plan.md)). Node coverage
grows with the slices — start with the slice-1 subset, not the whole
`TExpr`.

## Thin-slice #1 checklist — `printfn "hi"`

The first end-to-end milestone. `printfn "hi"` freezes today (verified) to
`App(External "printfn", New "…PrintfFormat"(Const(String "hi")), unit)` as
a single `TDecl.Expression`. Minimum to run it:

1. `Metadata.fs` enough for: assembly/module, `<ModuleName>` class, a
   `Main` method, refs to `FSharp.Core.dll`.
2. `Cil.fs` enough for: `ldstr`, `newobj`, `call`, `pop`/`ret` — i.e. the
   `Op` core + finalisation + maxStack.
3. `ClrProvider` entry for `printfn`: `newobj PrintfFormat<unit,
   TextWriter, unit, unit>::.ctor(string)` then `call
   PrintfModule.PrintFormatLine<…>(format)` (returns `unit` for a no-arg
   format), then discard.
4. Entry-point lowering: wrap the one expression in `Main`; `ldc.i4.0; ret`.
5. `materialise`: write the PE; an end-to-end test runs it and asserts
   stdout / exit code.

This proves the assembly writer, FSharp.Core resolution, entry point, the
metadata layer, the body DSL, and the simplest provider call — with no
closure, list, inline, or arithmetic machinery.

## Order of work

1. **`Cil.fs`** (body DSL) and **`Metadata.fs`** (metadata layer) in
   parallel. Each tested standalone — `Cil` by emitting a hand-written
   `add(int,int):int` body and asserting bytes + behaviour; `Metadata` by
   producing a minimal "hello" assembly with a hand-written `Main`.
2. **`ICodegenProvider` + `ClrProvider`** for the slice-1 names.
3. **`Emit.fs` + `Codegen.fs`** for slice 1, then grow node coverage
   through slices 2–4 (arithmetic intrinsics → `inline` expansion via
   `Inline.inlineExpand` → list-literal `Cons`/`Nil` ctors).
4. **Closure lowering** lands (universal canonicalisation pass) ahead of
   slice 5; then `List.fold` + `(+)`-as-value complete the sample.

## Open questions / decisions

- **TFM.** Match `XParsec.FSharp.SemanticAnalysis` (`net8.0`) for the
  *codegen project*; the *emitted* assembly's TFM and FSharp.Core version
  are `ProjectInfo` inputs (a v1 default plus an override).
- **Stack typing depth-only vs typed.** Start depth-only (LicenseToCIL's
  choice); revisit only if untyped stack slots cause real bugs the TAST
  `SemType`s can't pre-empt.
- **Reuse LicenseToCIL as a dependency vs. reimplement over SRM.** It's
  S.R.Emit-bound, so the emitter can't be reused; the `Op`/`S<>`/`cil`
  design is ~80 lines and gets reimplemented over SRM. Vendor the *shape*,
  not the package.
- **`maxStack` computation** in the `cil` runner (tracked depth) vs. a
  separate pass — prefer tracked depth.
- **In-memory run for tests** (load + invoke the emitted bytes) vs.
  shelling out to a written PE — prefer in-memory for speed, fall back to
  on-disk + `dotnet exec` if loader isolation bites.

## Cross-references

- [backend-design-plan](backend-design-plan.md) — posture + lowering split
  this plan instantiates.
- [il-emission-roadmap](il-emission-roadmap.md) — emission targets + the
  thin-slice order.
- [function-representation-plan](function-representation-plan.md) — closure
  lowering for slice 5.
- [extract-symbols-plan](extract-symbols-plan.md) — the symbol provider the
  codegen provider mirrors.
- LicenseToCIL (`D:\roboz0r\LicenseToCIL`) — the body-DSL shape to vendor
  (`Stack.fs`, `Types.fs`, `Ops.fs`, `CILBuilder.fs`); note it targets
  `S.R.Emit`, not `S.R.Metadata`.
