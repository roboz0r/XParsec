# Codegen.Js plan — a second backend emitting JavaScript

**Status:** plan, not started. No `Codegen.Js` exists yet; the only backend is
`XParsec.FSharp.Codegen.Clr`. This is the load-bearing step toward the stated
multiplatform goal. The anonymous-union front-end
([anon-unions-plan](anon-unions-plan.md)) and the TS-consumer symbol provider
([codegen-js-symbol-provider-plan](codegen-js-symbol-provider-plan.md)) are
separable workstreams that plug into this one.

## Why a *new* emitter, not a fork of the CLR backend

The CLR backend's TAST walker is **stack-machine-shaped to the bone**, so it
cannot be parameterised into a shared two-backend emitter. `EmitExpr.buildExpr`
([`EmitExpr.fs:20`](../../XParsec.FSharp.Codegen.Clr/EmitExpr.fs)) emits CLR
instructions inline — every arm calls `b.Add(ILInstr.X …)` (`LdcI4`, `Ldfld`,
`Box`, `Newobj`, `Call(EntityHandle, …)`), and `EmitEnv`
([`EmitTypes.fs:235`](../../XParsec.FSharp.Codegen.Clr/EmitTypes.fs)) is saturated
with `EntityHandle` and `MetadataContext`. Only the *match scrutinee* (the
`TExprG` cases) is platform-neutral; every arm *body* is CLR IL. And JS is **not
a stack machine** — it is an expression-tree target with native closures,
objects, GC, and dynamic dispatch. Forcing a stack-machine abstraction onto it
would be the wrong fit.

So `Codegen.Js` is a fresh `TAST → JS-AST` consumer. The good news: it is
**smaller**, not a 9,500-LOC parallel, because most of what makes the CLR backend
large is impedance that *evaporates* on JS.

## The shared contract (what both backends consume)

The clean seam sits *above* emission:

- **TAST** (`Tast.fs`, `Frozen.TExpr`) — the platform-neutral input.
- **FrozenType** ([`SemanticInfo.fs:263`](../SemanticInfo.fs)) — `SymbolKey`-keyed,
  platform-neutral type model. Gains `FTOr` from
  [anon-unions-plan](anon-unions-plan.md).
- **`IExternalSymbolProvider`**
  ([`ExternalSymbols.fs:399`](../ExternalSymbols.fs)) — the symbol oracle; where
  the TS-consumer provider plugs in.
- **`EmitLower.lower`**
  ([`EmitLower.fs:384`](../../XParsec.FSharp.Codegen.Clr/EmitLower.fs)) — TAST→TAST
  normalisation (eta-reification, operator expansion). Mostly reusable; some
  choices are CLR-motivated and the JS lowering may differ (e.g. operator
  expansion to `ILIntrinsic` is CLR-flavoured).

Everything below `EmitLower` in the CLR backend (`Emit*`, `IlIr`, `Cil`,
`ClrEncoder`, the metadata stack, the `EntityHandle`-shaped `ICodegenProvider`)
is CLR and is **not** shared.

## What evaporates on JS

| CLR backend pays for… | On JS… |
|---|---|
| ValueTuple encoding, metadata signature blobs | gone — arrays/objects, no type encoding |
| `EntityHandle` / MemberRef / MethodSpec minting, AssemblyRef tables | gone — names are strings |
| Boxing, `constrained.callvirt`, value-type dispatch | gone — values are already boxed |
| Stack-balance analysis, `maxStack`, exception regions | gone — structured AST + `try/catch` |
| Closure-as-TypeDef + Invoke interface | native `function`/arrow closures |

What *remains* to pay for is F# semantics JS lacks — the representation work
below.

## Output IR: an ESTree-shaped JS AST, printed in F#

- **ESTree**, not Babel AST. ESTree is the de-facto standard (Acorn / Espree /
  ESLint), and it is the dialect of the one AST-level seam a modern toolchain
  exposes — Rollup parses with Acorn (ESTree). A modern Vite/esbuild/Rollup
  pipeline consumes **source text + a V3 source map**, never a foreign AST, so a
  Babel AST would buy nothing and re-introduce the transform step the
  esbuild/swc generation deliberately dropped.
- **Model only the node subset we emit**, growing as codegen grows (Fable does
  this), with `type` discriminators and field names spec-exact so JSON
  serialisation to the standard stays a free future option.
- **Carry `loc` on every node**, sourced from the TAST/CST tokens the AST already
  preserves ([[feedback_ast_preserve_tokens]]) — that makes source maps
  mechanical.
- **Print in F#** with a writer that tracks generated line/column and emits V3
  VLQ mappings. **No Node in the build loop** (matches the repo's no-live-bridge
  discipline — see [codegen-js-symbol-provider-plan](codegen-js-symbol-provider-plan.md)).
- **Emit ESM** (`import`/`export`).

## Representation conventions — Fable-shaped, validated empirically

These were chosen by compiling probe F# through `dotnet fable` 5.0.0 and reading
the emitted JS (see `tmp/fable-repr/`). Verdicts:

| Construct | Decision | Notes |
|---|---|---|
| **Currying** | uncurried; re-curry only at partial-application sites | `add x y` → flat `add(x,y)`; `add 5` → `(y)=>add(5,y)`; saturated calls direct. Zero per-arg alloc in the common case. Arity analysis feeds from the existing saturation analysis. |
| **DU** | named props + integer `tag` + `cases()` | *Diverge from Fable's positional `fields[]`* — named fields are debuggable and sourcemap-friendly at no runtime cost. Match → `switch(tag)`. |
| **Records** | named fields; copy-update → reconstruction | `{r with F=v}` → `new R(r.A, v)`. |
| **Equality / compare / hash / toString** | runtime dispatch + base-class methods | The per-type code is tiny; structural logic lives in the runtime. **This is the Vesper runtime port** (below). |
| **Self tail calls** | `while(true)` + labeled `continue` | param-shadow mutation; zero cost. |
| **Mutual tail calls** | punt (may overflow) | Fable punts too; trampoline is a later option. |
| **int32** | `\| 0` truncation on every int32-typed result | *Be principled* — Fable is inconsistent (`100000*100000` lost its `\|0`). Truncate by type, not site. |
| **int64** | BigInt + runtime ops | correct (vs `number` precision loss past 2⁵³); heavier. |
| **float32** | collapse to `number` | no real single precision in JS. |
| **char** | length-1 string | |
| **Tuples** | arrays; tupled fn params flattened to positional | `(1,"two")` → `[1,"two"]`. |
| **Option** | **honest nominal DU, never erased** | `Some x` real, `None` singleton; nests correctly. Nullability is expressed via anonymous unions ([anon-unions-plan](anon-unions-plan.md)), not option erasure. |
| **`X \| Y \| null`** | native anonymous union, erased; `typeof`/`instanceof`/`=== null` narrowing | front-end owned by [anon-unions-plan](anon-unions-plan.md). |
| **List** | runtime cons list | `[1;2;3]` → `ofArray([…])`; this is `Vesper.List`. |
| **Reflection metadata** | **omit** | Fable always emits `_$reflection`; pure overhead unless reflection is needed. |

## The runtime library: self-hosted Vesper, not `fable-library-js`

Rather than depend on `fable-library-js`, the runtime primitives
(`equals`/`compare`/`structuralHash`/`toString`, the cons `List`, `Option`,
`Map`/`Set`) are the F#/Vesper code **already being ported** as part of the
structural-format work ([[project_percentA_structural_format]]) and the per-type
packages ([package-split-plan](package-split-plan.md)). That Vesper code,
compiled by `Codegen.Js` itself, *is* the runtime — a self-hosted runtime
consistent with [[project_vesper_core_lib]]. The temporary CLR `%A`
`IStructuralFormattable` synthesis retires; the Vesper port serves both backends
(CLR via normal codegen, JS as the imported runtime).

### Bootstrapping subtlety

`Codegen.Js` needs *some* of these primitives to compile the very module that
defines them (e.g. the structural-equality base class is referenced by every DU
the compiler emits, including the DUs *inside* the runtime). Sequence: identify
the irreducible core (the base-class shapes + `equals`/`compare`/`hash`
dispatch), hand-author or emit those first as a fixed prelude, then compile the
rest of the runtime against it. This mirrors the CLR side's `--compiling-fslib`
discipline ([[feedback_fsharpcore_one_assembly]]).

## Project shape

A new `XParsec.FSharp.Codegen.Js` project paralleling `…Codegen.Clr`:

- `JsAst.fs` — the ESTree node subset (DU).
- `JsPrint.fs` — AST → source text + V3 source map writer.
- `EmitJs*.fs` — the `TAST → JsAst` walker (the JS analogue of `Emit*`), reusing
  the *logic* of pattern destructuring / closure discovery / free-variable
  analysis where it transfers, but emitting AST nodes, not stack ops.
- A `Codegen.Js.compile` entry mirroring
  `Codegen.compile` ([`Codegen.fs`](../../XParsec.FSharp.Codegen.Clr/Codegen.fs))
  — same `(IExternalSymbolProvider, ProjectInfo, Frozen.TastFile)` inputs.

The CLR `ICodegenProvider` is **not** reused (its members traffic in
`EntityHandle`). If a JS-side provider interface is needed at all, it is far
thinner — most CLR provider methods (signature encoding, MethodSpec minting) have
no JS analogue.

## Scope

**MVP (the non-union subset):** functions/currying, records, DUs + match, tuples,
options, closures, self-recursion, int32/int64/float, lists; ESM output + source
maps; the bootstrapped Vesper runtime core. This is independently shippable
before the anonymous-union codegen rows.

**Deferred:** anonymous-union narrowing codegen (gated on
[anon-unions-plan](anon-unions-plan.md)); mutual-tail-call trampoline; reflection
metadata; full `Map`/`Set`/`Seq` runtime; async; the TS-consumer provider
([codegen-js-symbol-provider-plan](codegen-js-symbol-provider-plan.md)).

## Seed milestone

Mirror the CLR backend's first vertical slice: **`printfn "hi"` → runnable JS**,
end-to-end — TAST → `JsAst` → printed `.js` (ESM) + `.js.map`, executing under
Node and producing `hi`. One slice, not breadth; it proves the seam (walker,
printer, source map, runtime-import resolution) before any hard semantics.
