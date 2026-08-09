# FSharp.Core compatibility plan — `--fsharp-compat`

How a Vesper-compiled assembly interops with assemblies compiled by `fsc`
(which speak `Microsoft.FSharp.Core.FSharpFunc\`2`, `FSharpList\`1`, etc.) without
giving up the lightweight, FSharp.Core-free default the self-host ladder is built
around. This is the deferred `--fsharp-compat` flag, sharpened after R1 landed the
`Vesper.Fun` representation.

## Why

The default Vesper build emits no `Microsoft.FSharp.*` references — function values
are `Vesper.Fun`, lists will be `Vesper.Collections.List`, etc. That is the point of
the cut. But a real program may want to reference an existing **F#-compiled DLL**,
whose public surface is typed in FSharp.Core shapes (`'a -> 'b` ≡ `FSharpFunc`,
`'a list` ≡ `FSharpList`). For those builds Vesper should convert core-type
representations transparently across the boundary, at the cost of pulling in
`FSharp.Core.dll`.

**The default stays lightweight.** Compat is opt-in (a `ProjectInfo` flag, off by
default), so a program that references no F# DLL ships the smaller, FSharp.Core-free
bundle. Compat is what you turn on when you knowingly take the FSharp.Core
dependency for interop.

## Two directions — they are not symmetric

"Transparent interop" is two problems, and dual-implementing closures only solves
the first.

### Vesper → F# (a Vesper function value is passed to an F# API)

This is the easy half, and R1 already built most of it. A **compat closure**:

- derives from `FSharpFunc\`2<a,b>` (the pre-R1 base — that emission path still
  exists in `Codegen`/`ClrProvider` history) **and**
- carries an `InterfaceImpl` row to `Vesper.Fun\`2<a,b>` (the row R1 added).

A single `Invoke(a):b` serves both: it is a reuse-slot virtual that *overrides* the
abstract `FSharpFunc\`2::Invoke`, and the same virtual *implicitly implements* the
`Vesper.Fun\`2::Invoke` interface slot (name + signature match, with the
`InterfaceImpl` row present). The ctor chains to the protected
`FSharpFunc\`2::.ctor()`. The closure references **both** `FSharp.Core` and
`Vesper.Core`.

So a compat closure value is *both* an `FSharpFunc` and a `Vesper.Fun`. It can be
stored in an `FSharpFunc`-typed slot (an F# API parameter) or a `Vesper.Fun`-typed
slot (native) — the value is universally acceptable. Delta from R1: restore the
`FSharpFunc` base + ctor chain and the reuse-slot `invokeAttrs` (no `NewSlot|Final`),
*keep* the `InterfaceImpl` row, all gated on the flag.

### F# → Vesper (an `FSharpFunc` from an F# DLL used where `Vesper.Fun` is expected)

This is the half dual-impl does **not** cover. An `FSharpFunc` produced by an F#
DLL does not implement `Vesper.Fun`, so it cannot be passed to a native
`Vesper.Fun`-typed slot directly. It needs an **adapter** — a small wrapper
(`struct` or class) that holds the `FSharpFunc` and forwards `Vesper.Fun::Invoke`
to `FSharpFunc::Invoke` (and the reverse wrapper for the other direction). One
adapter per arrow arity in use; effectively free, but it has to be emitted and
inserted at the boundary. (This is the "wrapper struct in user assemblies" bridge
sketched in [function-representation-plan](function-representation-plan.md)
§First-pass shape, made concrete for the F#→Vesper direction.)

## The crux: per-boundary type-slot selection

The closure *value* being dual is necessary but not sufficient. Every
variable / parameter / field / return has **one** static type in metadata, and it
must match what the callee expects:

- a slot facing an FSharp.Core API → `FSharpFunc\`2`,
- a native Vesper slot → `Vesper.Fun\`2`.

So `ClrProvider.encodeType`'s `TyFun` arm cannot be globally "always `Vesper.Fun`"
(R1) or "always `FSharpFunc`" in compat mode — it must choose **per boundary**.
R1 already hit a miniature of this and handled it by hand: the cold-printf island
encodes its arrows with `encodeFSharpFunc` (→ `FSharpFunc`) while everything else
uses `encodeType` (→ `Vesper.Fun`). Generalising that hand-split into a principled
per-boundary decision — driven by whether the symbol on the far side of the call is
an FSharp.Core symbol or a Vesper symbol — is the real design work of this plan, and
is why it is more than "implement both interfaces."

The signal is available in principle: the external-symbol provider knows whether a
resolved name lives in `Microsoft.FSharp.*` or `Vesper.*`. Threading that origin to
the encoder at each call/field boundary is the mechanism to design.

## Side benefit: the `List.fold` sample without R2

A compat closure *is* an `FSharpFunc`, so the eta-reified `(+)` folder could be
passed straight to FSharp.Core's `ListModule.Fold` — which would un-break the
canonical `List.fold` run-tests (`Slice5Tests.fs`, currently `ptest`-pending) under
`--fsharp-compat` **without** waiting for R2's `Vesper.Fun`-folder fold. This is a
genuine interim, but it ships FSharp.Core; the lightweight default still needs R2.

## Why deferred (not built with R1)

- **The value side is cheap and R1-ready; the type side and the F#→Vesper adapter
  are the actual work.** Building only the easy half would be a half-feature.
- **It wants a concrete consumer to validate against** — a test that references a
  real `fsc`-compiled DLL and round-trips a function (and a list) both ways. Without
  that, the per-boundary encoder decision and the adapter shapes are guesses.
- **It must not regress the default.** The lightweight, FSharp.Core-free path is the
  default path; compat is a bolt-on behind a flag, validated to not perturb the default
  bundle.

Recommended sequencing: land it after R2/R3 give us real `Vesper.*` core symbols on
both sides, so the per-boundary "is this an FSharp.Core or a Vesper symbol" decision
has both populations to distinguish. Until then this is the documented seam.

## Sketch of the flag surface

- `ProjectInfo.FSharpCompat: bool` (default `false`). When set, the build may
  reference `FSharp.Core.dll` and emits dual closures + boundary adapters.
- `ClrProvider` closure emission branches on it: object-base + `Vesper.Fun`
  interface (default) vs `FSharpFunc`-base + `Vesper.Fun` interface (compat).
- `encodeType`'s `TyFun` arm consults the per-boundary origin (above) instead of a
  single global representation.
- `materialiseApp` copies `FSharp.Core.dll` when compat pulled it in (the existing
  `FSharpCoreDependencies` signal already covers this).

## Cross-references

- [function-representation-plan](function-representation-plan.md) — §First-pass shape
  (FSharpFunc-implements-Fun / wrapper-struct bridge) and §Out of scope
  (cross-assembly Fun shapes); the adapter direction here is that bridge.
