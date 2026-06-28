# Free-function overloads → synthetic erased per-module type (Tier 2 item 9b)

**Status:** design, **decided**, not started. Ephemeral — delete once 9b lands.
Scope: the consumer/emit half of item 9 in [`ts-extraction-plan.md`](ts-extraction-plan.md)
(the member-overload half, 9a, already landed). The producer already emits every
free-function call signature; this doc is provider + seam + JS-emit only.

## The problem

F# has no free-function overloading, so the provider's `funcs = Map.ofList`
(name-keyed) is last-wins: a TS `export function format(x: string): string;
export function format(x: number): string;` loses all but one signature. `singleSignature`
in `toFunctionSymbol` currently throws on this (deliberately loud).

## The decision (user-confirmed)

**Per-module synthetic erased type.** All overloaded free functions of a module
`M` are grouped as **static members of one synthetic type named deterministically
after `M`** (e.g. module `util` → type `Util`). User code calls `Util.format(x)`;
it **erases at JS emit** to the bare `format(x)`. Non-overloaded free functions
stay bare (`shout(x)` → `shout(x)`) via the existing `funcs`/`TryLookup` path.
This mirrors the Fable `[<Erase>] type JS` prior art (one grouping type, members
are the real export names, the grouping erases).

Deterministic name rule: capitalize/normalise the last segment of the module
specifier (the `moduleSpec`/nsPath the provider already threads). Document the
exact rule in code; it must round-trip stably for the golden.

## Why erase is load-bearing (not cosmetic)

Reconnaissance (JS backend map): an external **static-member** call today lowers
via `JsImports.addMemberRef` to a **mangled `Type_member` runtime import**
(`EmitJs.fs:579-591`, `Members.mangledName`). If the synthetic `Util.format`
went through that path it would emit `import { Util_format } from …` — **no such
export exists**; the real export is bare `format`. So the call MUST erase to the
bare export name (the free-function `addRef` path, `EmitJs.fs:377-384` /
`JsImports.addRef`, `JsRuntime.fs:65-72`). Erase is required for correctness.

There is **no existing erase facility** for member/static calls to reuse (the only
emit-time erase is the `ILIntrinsic` raw-splice for operators, `EmitJs.fs:639-675`;
the `TryLookupInlineBody` seam is unwired on JS). A new marker + emit branch is
needed.

## v1 scope bound: named imports only

`ImportShape` is consumed **nowhere** at JS emit today (the provider drops `_import`
at `TsManifestProvider.fs`; the seam carries no import-shape field; the JS AST has
only one named-import form, `JsAst.fs:86` / `JsPrint.fs:225-226`). Full
Default/Namespace/CommonJS import forms are a separate, pre-existing gap. **v1
restricts the synthetic-type erase to NAMED-imported overloaded functions** (the
common case — the existing named-import `addRef` path already works). A
non-Named overloaded free function throws a clear "not yet supported" error,
gating the deferred import-form work to exactly the fixture that needs it. Do NOT
build the Default/Namespace/CommonJS import AST forms here.

## Implementation — two reviewable phases

### Phase 1 — provider synthesis + seam erase marker (manifest/provider level)
- **Seam:** add a way to mark a synthetic class (or its static members) as
  **erase-to-bare-export**. Cleanest candidate: an `ExternalClassFlags` bit (it
  currently has `Default`) — e.g. `Erased`/`ErasedImportGrouping` — set on the
  synthetic type; the JS emit branch (Phase 2) keys off it. Confirm the flag type
  in `src/XParsec.FSharp.SemanticAnalysis` / `Codegen.Common` before adding.
- **Provider (`TsManifestProvider.providerOfManifest`):** partition free functions
  by signature count. N=1 → the existing `toFunctionSymbol`/`funcs` path. N>1 →
  group by module, synthesize one `ExternalTypeShape.Class` per module (flagged
  erased), static members = the overloads expanded with argSig (reuse 9a's
  `expandMethod`/`overloadArgSigs`; the member name stays the REAL export name so
  it erases correctly). Register under the synthetic type name in `types`.
  - Throw on a non-Named-import overloaded free function (the v1 gate above).
- **Resolution reuses 9a:** `Util.format(x)` resolves through the existing external
  static-method overload path (`InferExternalCall.tryInferExternalStaticMethodCall`
  → `TryLookupMembers` → `pickBestOverload`), which 9a's `expandMethod` already
  feeds. No new resolution machinery expected — verify.
- **Test:** extractor fixture `specs/freefnoverloads/` with two `export function f`
  overloads; assert the synthetic type resolves via `TryLookupType` and both
  overloads via `TryLookupMembers` with distinct keys. (Manifest/provider level —
  no JS emit yet.)

### Phase 2 — JS emit erase branch (JS codegen harness)
- In `EmitJs.fs` `TExprG.ExternalMember` arm (`:579-591`): when the member's
  declaring type carries the erased flag, lower the static call like a free-function
  `External` — `JsImports.addRef` of the **bare export name** (the member name) from
  the member's home module — instead of `addMemberRef` of the mangled `Type_member`.
  The receiver is already `ValueNone` for statics, so the call shape is `format(x)`.
- **Test:** a `XParsec.FSharp.Codegen.Js.Tests` case emitting `Util.format(x)` and
  asserting the output is `import { format … }` + `format(x)`, NOT `Util_format`.
- This phase needs the JS codegen test harness, distinct from the extractor suite.

## Out of scope (deferred, gated)
- Default/Namespace/CommonJS import forms (the broader ImportShape-at-emit work).
- Grouping NON-overloaded free functions under the module type (they stay bare).
- CLR backend (free-function overloads are a TS/JS-interop concern; CLR has real
  overloading natively — revisit only if a CLR consumer needs it).
