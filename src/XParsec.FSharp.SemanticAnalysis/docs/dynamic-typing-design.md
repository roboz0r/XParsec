# Dynamic typing (`dynamic`) — a disciplined F# `any`

**Status (2026-08-11).** IMPLEMENTED, the implicit-escape warning included
(`DynamicEscape.fs`, `DynamicTypeTests`). Superseded the `TyDynamic` front-end
DU-case approach spiked under "Wall 2" of
[`ts-provider-implementation-plan.md`](ts-provider-implementation-plan.md); that spike
was unwound, and `TyDynamic` survives only as a `TyConst` recognizer in
`RuntimeNames.fs`. One deferral remains, flagged **[DEFERRED]** at the foot.

## Motivation

TS `any` must land somewhere on a Vesper value. TS treats `any` as fast-and-loose —
it flows implicitly into and out of every concrete type, and `.member` access is
unchecked. **Vesper does not.** F# is not that loose with types, and we respect that in
spirit: `dynamic` is a real, *opaque* type you must explicitly enter and explicitly
leave. The escape hatch is the F# dynamic-access operator `?` (with target typing), or
an explicit cast — never silent flow.

## The type: `dynamic` is a plain opaque JS intrinsic

```fsharp
// prim-types-dynamic.js.fsi   (JS-only — no CLR analog)
type dynamic = extern
// prim-types-dynamic.js.fs
type dynamic = (# "any" #)
```

Modelled exactly like `undefined`: a JS-only intrinsic, so the pair is named by
`Vesper.Core/manifest.js.toml` (`files` + `impl`) and by no other manifest — which is the
whole statement that the CLR has no `dynamic`. The `.fs` is extracted for both marker and
platform name, ahead of the `.fsi`. Canon identity `dynamic`; JS platform tag
`"any"`. It carries **no special unifier behaviour** — it unifies with itself by name,
like `int` or `string`, and with nothing else. There is deliberately **no `TyDynamic`
SemType/`FTDynamic` FrozenType DU case**: `dynamic` is `TyConst("dynamic")` /
`FTConst("dynamic")` everywhere, so it needs no threading through the child-walk
skeletons and mints no new absorb/subtype rules.

### No free interconversion (the F#-discipline premise)

- `let d : dynamic = someInt` — **rejected** (an `int` is not a `dynamic`). You enter
  `dynamic` from a TS-`any`-typed value, or explicitly via the `dynamic` conversion
  function (below).
- `let n : int = d` — **rejected**. You leave `dynamic` through `?` (target-typed), or
  whole-value through `Unsafe.retype`.
- `d.foo` (dotted) — **rejected**. `.` is for statically-known members; `dynamic` has
  none. This is the load-bearing F#-fidelity point.

`dynamic` therefore has *zero* assignability edges in `subsumes`. All of its behaviour
is in the `?` operator and the `dynamic`/`?` intrinsics.

### `retype`, and entering/leaving `dynamic`

The underlying primitive is FSharp.Core's general erasing reinterpret, declared in
`Vesper.Core/ops-dynamic.js.fsi`'s non-`[<AutoOpen>]` `module Unsafe`:

```fsharp
val inline retype: x: ^T -> ^U          // body: (# "" x : ^U #)
```

The empty-string intrinsic `(# "" x : 'U #)` is the identity cast: it emits the value
unchanged and re-types it `'U` (the explicit `: 'U` annotation *is* the reinterpret
target). This is inherently unsafe (no runtime check) — the escape valve, not the
common path.

`dynamic` (entry) and whole-value exit are `retype` at a fixed result type:

```fsharp
// ops-dynamic.js.fsi — entry convenience, shares the type's name like int/string/box
val inline dynamic: value: ^T -> dynamic
let inline dynamic (value: ^T) : dynamic = Unsafe.retype value
```

- **Enter:** `dynamic x` — same JS value, retyped to `dynamic`. `x` alone never flows in
  silently.
- **Member exit:** `x?foo` (target-typed) — the principled, *checked-by-you* path.
- **Whole-value exit:** `retype d : int` — the explicit, unchecked escape (resolves the
  earlier open question; `retype` is the general cast, so no bespoke `undynamic` needed).

The `(# "" x : ^U #)` identity lowering emits the operand verbatim on JS, which is what
makes `dynamic someInt` a no-op at run time. `retype` is PUBLIC but never ambient:
`module Vesper.Unsafe` carries no `[<AutoOpen>]`, so an explicit `open Vesper.Unsafe` is
the marker that a cast here is unchecked, and the FFI escape hatch stays out of every
program's default scope.

## The operators: `?` and `?<-`, SRTP with a `dynamic` default

```fsharp
// ops-dynamic.js.fsi   ([<AutoOpen>] module DynamicOperators)
val inline (?)   : target: dynamic -> name: string -> ^TResult
                       when default ^TResult : dynamic
val inline (?<-) : target: dynamic -> name: string -> value: ^TValue -> unit
```

`x ? ident` means `(?) x "ident"` (F# spec 6.4.5) — the member name is a compile-time
**string** literal, never a value reference. The parser keeps it as one
`Expr.DynamicLookup` node; the operator call is minted downstream (below), so the CST
carries the surface form and the elaborated tree carries the application. Then ordinary
inference + the existing SRTP `default`-constraint machinery
(`InferGeneralize.applyDefaults`, a real fixpoint defaulting pass — general, not
int-hardcoded) does the rest:

- **Unconstrained** context → `^TResult` defaults to `dynamic`. So `x?a?b` chains stay
  dynamic — "infectious", but *only through `?`*, never through assignability.
- **Pinned** context (`let n : int = x?foo`, `x?foo + 1`, an `int` parameter position)
  → `^TResult` unifies to the pinned type *before* generalisation, the default never
  fires, and `?` is the **principled escape back to static**.

`?<-` is the setter (`x?foo <- v`).

### Implicit escape warns; explicit escape is silent

Target-typing lets `dynamic` escape to a concrete type through *context* — `d?foo + 1`
forces `^TResult = int` via the arithmetic. That is an **unchecked assertion** (the
compiler cannot verify `d.foo` is really an `int`), so it warns, mirroring F#'s posture
on `op_Implicit`.

What suppresses is **syntactic**: an ascription directly on the `?` expression,
`(d?foo : int) + 1`. An annotation on the *binding*, `let n : int = d?foo`, still warns,
nudging `let n = (d?foo : int)`. One teachable rule — name the type at the escape point.
The looser alternative (any expected type reaching the expression suppresses, so only
inference-derived escapes warn) was rejected: it needs provenance tracking on the
expected type, which is much harder to get right for a permissiveness nobody asked for.

Detection is a post-settle sweep over the `?`-result vars `inferDynamicLookup` recorded,
NOT an `applyDefaults` hook: a var that `zonk`s to a concrete non-`dynamic` shape is one
whose `default : dynamic` was skipped, which is exactly the warn case.

### The `?` operand is `dynamic`, not `obj`

FSharp.Interop.Dynamic types `(?)` as `obj -> string -> 'TResult` (permissive: `?`
works on anything). Vesper takes the **stricter** `dynamic -> …`: `?` is valid only on a
`dynamic` operand, so you cannot `?`-probe a statically-typed value by accident — you
must first *be* in `dynamic`. This matches "you must go through `d?foo` / a cast."

### JS emission — bracket form

`(?)` inline body emits computed member access `$0[$1]` → `x["foo"]` (safe for any
name). `(?<-)` emits `$0[$1] = $2` → `x["foo"] = v`.
**TODO (prettify only):** when `name` is a valid JS identifier, emit dotted `x.foo`
instead of `x["foo"]`. Purely cosmetic on the output JS; not semantic.

### No FID reflection (JS-only for now)

FSharp.Interop.Dynamic inspects `typeof<'TResult>` at runtime to choose property-get vs
method-invoke via the DLR. On JS we need none of that: `x?foo` is a property get
(`x["foo"]`), and `x?foo(args)` is just `App` over that get (`x["foo"](args)`) — the JS
runtime *is* the dynamic dispatch. **FID-style reflection is the CLR story and is
deferred** (a CLR `dynamic` would need a `Dynamitey`-like runtime call-site dispatch
layer); our first and only target here is JS.

## Extractor / provider

The extractor already maps TS `any → Schema.TypeRef.Dynamic` (serialised
`{"k":"dynamic"}`, committed golden). The F#-side deserialize lands it as
**`FTConst("dynamic")`** (it was the `FTUnknown "any"` TODO, and briefly `FTDynamic` in
the reverted spike). So a TS-`any`-typed member/param/return
arrives as the opaque `dynamic` intrinsic, and the only thing you can do with it is `?`.

## Where it lives

- **The type** — `Vesper.Core/prim-types-dynamic.js.{fsi,fs}`, named by
  `manifest.js.toml` and by no other manifest.
- **`retype` + `dynamic` + `(?)` / `(?<-)`** — `Vesper.Core/ops-dynamic.js.{fsi,fs}`,
  likewise JS-only. The bodies are `$0[$1]` and `$0[$1] = $2`; `dynamic value` is
  `retype value`.
- **`x?name`** — the parser keeps `Expr.DynamicLookup`. NameResolution stamps the node
  with `OperatorData.OpDynamic`, `inferDynamicLookup` unifies the resolved operator's
  scheme against `objArg -> string -> ^TResult` and records the result var for the
  escape sweep, and `ElaborateAccess.translateDynamicLookup` mints the curried
  `External` application whose second argument is the ident as a string constant.
  `x?name <- v` runs the `(?<-)` counterpart off `Assignment(DynamicLookup …)`.
  This is an infer/elaborate route rather than a Desugar rewrite, but it is still the
  *operator* — which is what unlocks SRTP target-typing; a bespoke arm returning
  `dynamic` would not.
- **The escape warning** — `DynamicEscape.fs`, a post-settle sweep of
  `ctx.DynamicEscapes` skipping `ctx.DynamicEscapeSuppressed`.
- **TS ingress** — `TsManifestTypes.fs` maps `Schema.TypeRef.Dynamic` to
  `FTConst(RuntimeNames.dynamicKey)`.

`DynamicTypeTests` (in the JS codegen suite) holds the behaviour: the default firing and
not firing, `d?a?b` staying dynamic, dotted `.foo` and `let n : int = d` erroring, the
`?`-setter round-tripping under Node, each warning case, and `retype` being reachable
only through `open Vesper.Unsafe`.

## Settled forks

1. `(?)` operand — **`dynamic` (strict)**, not FSharp.Interop.Dynamic's permissive
   `obj`. You cannot `?`-probe a statically-typed value; you must first *be* in
   `dynamic`.
2. `retype` surface — PUBLIC but in a NON-`[<AutoOpen>]` `module Vesper.Unsafe`, the
   middle ground between ambient `[<AutoOpen>]` and FSharp.Core's internal-only.
   Whole-value `dynamic -> 'T` exit is `Unsafe.retype d : 'T`.
3. Escape suppression — the **syntactic** fork: an ascription directly on the `?`
   expression suppresses, an annotation on the binding does not.

**[DEFERRED]** CLR `dynamic`, which would need a `Dynamitey`-like runtime call-site
dispatch layer. Out of scope; JS-only. `#nowarn`-number suppression of the escape
warning is also still descoped — no warning-number plumbing reaches semantic
diagnostics.
