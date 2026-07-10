# Idiomatic `Unchecked.defaultof<'T>` plan

The build plan for making `Unchecked.defaultof<'T>` a **first-class**,
callable representation instead of the current bare-`defaultof` +
`[<AutoOpen>]` workaround. The same machinery generalises to every
`inline` intrinsic value whose IL body is a valid standalone method
(`Unchecked.unbox`, a future `Unchecked.compare`, …).

## Status quo

`Vesper.Core/ops-platform.fs:247` defines the primitive as a nullary
generic `inline` value in an `[<AutoOpen>] module Unchecked`:

```fsharp
let inline defaultof<'T> : 'T = (# "ilzero" type ('T) : 'T #)
```

Its only legal use is the **bare, un-type-applied** form under a type
annotation (`seq.fs:52`: `let mutable acc: 'T = defaultof`). That is a
workaround for two front-end gaps, both of which the idiomatic spelling
`Unchecked.defaultof<'T>` hits:

1. **The qualifier crashes codegen.** A generic value whose typar lives
   only in the result cannot be a field (value restriction), so — like
   real F# — it is represented as a zero-arg generic static *method*
   `DefaultOf<T>()` and referenced with value syntax. But `defaultof`
   is `inline`, and `classifyModuleValues` (`EmitClosures.fs:164`) gates
   the generic-value→method materialiser on **`not isInline`**, so no
   method is ever emitted. Bare `defaultof` survives only because it
   lowers to a `TExpr.External` leaf that `InlineExpansion.fs:674`
   splices to `ilzero` *before* codegen. The qualified spelling lowers
   instead to a member read (`StaticPropertyGet`/`StaticMethodCall`,
   `FreezeExpr.fs:189,307`), which the splice arm (matches only
   `TExpr.External`) skips; it reaches `EmitExpr.fs:111/113` and emits a
   `call` into the never-materialised `Unchecked` class →
   **TypeLoadException / InvalidProgramException** at runtime. This is
   why `[<AutoOpen>]` + bare form is load-bearing, not cosmetic.

2. **The `<'T>` has no lowering.** `Expr.TypeApp(value, types)` parses
   (`Expr.fs:281`); `inferTypeApp` treats explicit args on a
   non-nominal result (a bare typar `'T`) as a documented no-op
   (`InferTypeOps.fs:30-58`); and Freeze has no arm for a *value*
   `TypeApp` — only the `TypeAppStaticMember` class-receiver arms
   (`FreezeExpr.fs:183,307`) — so `defaultof<'T>` hits the `failwith`
   fallthrough.

The canonical example we want to compile — byte-identically to today's
bare form for in-language uses, and additionally callable from
C#/reflection:

```fsharp
let reduce (reduction: 'T -> 'T -> 'T) (source: seq<'T>) : 'T =
    let mutable acc = Unchecked.defaultof<'T>   // splices to `ilzero`; no call
    ...
```

## Design — "a bit of both"

Mirror FSharp.Core exactly (`Unchecked` module + `nativeptr.fs`): an
`inline` intrinsic value is **both** spliced at every in-language use
**and** materialised as a real method for external callers. Materialise
by **default**; opt out with `[<NoDynamicInvocation>]`.

### Half 1 — inline every reference during semantic analysis

Every reference to an `inline` binding must splice, regardless of
spelling (bare / `Unchecked.`-qualified / `<'T>`-applied). Today only
the bare `External` leaf splices.

- **Freeze:** when a `DotLookup` / `TypeApp` resolves to an `inline`
  binding, lower it to the **`External` head** (symbol key + `refTy`)
  rather than `StaticPropertyGet` / `StaticMethodCall`, so it lands on
  the existing `InlineExpansion.fs:674` splice arm. `ilzero` grounds to
  `refTy` there, unchanged.
- **`<'T>`:** add a value-`TypeApp` freeze arm that forwards to the
  inner reference (the type-app already did its job — or is a no-op —
  in inference), leaving the `External` leaf for the splice. Optionally
  extend `inferTypeApp` to bind the value's scheme typar to the
  explicit arg so the annotation is not required; not needed for
  correctness (`refTy` pins `'T`).

This is the **correctness fix**: after it, a qualified reference never
reaches codegen as a call, so it cannot hit the phantom-method crash —
this holds even for `[<NoDynamicInvocation>]` intrinsics that emit no
fallback method (e.g. a future `stackalloc`).

### Half 2 — materialise a real method (default)

Lift the blanket `not isInline` exclusion in `classifyModuleValues`
(`EmitClosures.fs:164`): an `inline` generic value is *also* fed to
`collectGenericModuleValues`, emitting `Unchecked.DefaultOf<T>()` with
the `ilzero` body (`EmitIntrinsic.fs:87` already lowers `ilzero`;
`initobj` on an unconstrained typar is the valid universal-generic
recipe). In-language uses still splice via Half 1; the method is purely
the dynamic-invocation fallback — the FSharp.Core shape (`inline` *and*
a compiled `DefaultOf`).

- **Name:** `[<CompiledName("DefaultOf")>]` is **already threaded**
  (`Elaborate.fs:1710` → `VesperLibTypeTranslate.tryCompiledName`), so
  the materialised method takes its IL name for free.
- Once materialised, `[<AutoOpen>]` on `module Unchecked` can be
  removed and `seq.fs:52` rewritten to the idiomatic spelling.

### Discriminator — `[<NoDynamicInvocation>]` (opt-out)

Some `inline` intrinsics have no valid standalone method: a future
`stackalloc` (`localloc`) allocates in the *caller's* frame, so a
materialised `StackAllocate<T>()` would return a pointer into a freed
frame. Whether an opcode is frame-sensitive is CIL knowledge that must
not leak into Freeze; FSharp.Core encodes the verdict at the source with
`[<NoDynamicInvocation>]` (`nativeptr.fs`: `stackalloc`).

- **New (small) work:** `[<NoDynamicInvocation>]` is present in library
  sources but consumed **nowhere** today — inert decoration. Recognise
  it (alongside the `isInline` check in `classifyModuleValues`) as
  "skip materialisation." `defaultof` has no such attribute → it
  materialises. Everything unmarked materialises; nothing is
  materialised by accident.
- **Failure mode is safe:** an unmarked frame-sensitive intrinsic that
  slipped through would fail fast (corrupt stack / verify error), not
  silently miscompile; and the only such intrinsic (`stackalloc`) is
  deferred anyway. F#'s throw-stub body for `NoDynamicInvocation`
  methods is *not* replicated now — a marked intrinsic simply emits no
  fallback method (Half 1 keeps every in-language ref spliced). Revisit
  when `stackalloc` lands.

## Change-sites

| # | File | Change |
|---|------|--------|
| 1 | `Freeze/Idents.fs` (`translateIdent`) | A qualified read of an *external* module value already froze to `TExpr.External` — the defect was that it carried the **dotted name + holder-scoped key**, which the cross-package inline-body index (keyed by *simple* name / bare-reference key) does not contain, so `InlineExpansion.fs:674` missed it and codegen emitted the phantom `call`. Fix: for a nullary zero-operand intrinsic value only (`Inline.nullaryIntrinsicValueBody`), re-point the `External` head to the simple name so it hits the same splice as the bare form. (The member-read path at `FreezeExpr.fs:189/307` fires only for a *project-local* holder, not this case.) |
| 2 | `FreezeExpr.fs` | New arm: value `Expr.TypeApp(inner, types)` forwards to `inner`'s frozen `External` leaf. |
| 3 | `InferTypeOps.fs:36` (optional) | `inferTypeApp` binds a generic value's scheme typar to the explicit arg. |
| 4 | `EmitClosures.fs:164` | `classifyModuleValues`: admit `isInline` values that lack `[<NoDynamicInvocation>]`. |
| 5a | `Vesper.Core/compiler-attributes.fsi` | Declare `NoDynamicInvocationAttribute`; and add `CompiledNameAttribute`, which is recognised by short name (`Elaborate.fs:1710` → `tryCompiledName`) but currently has **no source representation** — a latent gap to close while we are here. |
| 5b | attribute plumbing (near `Elaborate.fs:1710`, sibling of `tryCompiledName`) | Derive a `NoDynamicInvocation` flag off `b.attributes` and thread it to site #4. |
| 6 | `ops-platform.fs:247` / `.fsi:464` | Drop `[<AutoOpen>]`; keep `inline`; the `.fsi` remarks documenting the old workaround are deleted. |
| 7 | `seq.fs:52` | Rewrite bare `defaultof` → `Unchecked.defaultof<'T>`. |

Sites 1–3 are **Half 1** (correctness, independently shippable); 4–7 are
**Half 2** (materialisation, lets `[<AutoOpen>]` go away).

## Deferred (cross another day)

- `stackalloc` / `localloc` itself, and with it F#'s throw-stub body for
  `[<NoDynamicInvocation>]` methods.
- Materialising the JS-only / contract-only intrinsics (`undefined`,
  `GetIndex`/`SetIndex` `failwith` bodies) — they have no meaningful CLR
  method and are left inline-only; Half 1 already keeps their references
  spliced.

## Test plan

- **Regression:** `seq.fs` `reduce` still compiles and its emitted IL
  for the seed is unchanged (`ilzero` splice, no call).
- **Qualified in-language:** a fixture using `Unchecked.defaultof<'T>`
  (bare and type-applied) splices identically — assert on frozen TAST
  and on emitted IL (no `call Unchecked::*`).
- **Materialisation:** assert the `Unchecked` holder emits a generic
  `DefaultOf<T>()` method with an `ilzero` body, and that it loads and
  invokes via reflection returning `default(T)` for a value and `null`
  for a reference type.
- **Opt-out:** a synthetic `inline` value marked
  `[<NoDynamicInvocation>]` emits **no** fallback method, yet a
  reference to it still compiles (spliced).
