# Codegen.Js external symbol provider plan — consuming TS/JS via the TS toolchain

**Status:** plan, not started. **Supersedes**
[brainstorm-codegen-js-symbol-provider](brainstorm-codegen-js-symbol-provider.md)
(kept for the original design discussion; this plan is the up-to-date,
seam-accurate version). Downstream of [codegen-js-plan](codegen-js-plan.md): this
provider resolves TS-sourced types into TAST, but nothing it resolves is runnable
until the JS backend can emit. It *can* be prototyped against the CLR pipeline
(types resolve fine; they just can't codegen yet), making it a low-risk parallel
workstream.

## The core idea (unchanged from the brainstorm)

Back a future `Codegen.Js`'s `IExternalSymbolProvider` with TypeScript's own type
machinery instead of hand-writing Fable-style bindings. Feed the TS Compiler API
(`ts.createProgram` + `TypeChecker`) the `.ts`/`.d.ts` surface of a library, emit
a serialised **manifest**, and have an F# provider deserialise it. This is **not
a new architecture** — it is `MetadataSymbols.fs` re-pointed at a different
platform: use the platform's native type oracle and map its native type
representation into `SemType`, following "skip rather than fake."

## What the brainstorm got right and what is now stale

The brainstorm's *file:line anchors are out of date*; the current seam:

- **The provider interface.** `IExternalSymbolProvider`
  ([`ExternalSymbols.fs:399`](../ExternalSymbols.fs)) — 8 members. Lookups return
  closures (`Instantiate: int -> SemType` minting fresh `TypeVar`s at a given
  let-depth) plus `ExternalConstraint` lists — the data a manifest must rehydrate.
- **Overloads are already solved.** The brainstorm's gating decision #3
  ("`TryLookupMember` is singular → seam change") is **obsolete**: the seam
  already has `TryLookupMembers : typeName * memberName -> ExternalMember[]`
  ([`MetadataSymbols.fs:480`](../../XParsec.FSharp.Codegen.Clr/MetadataSymbols.fs)),
  returning the full overload set with arg-aware resolution. TS call-signature
  overloads map straight onto it; no seam change needed.
- **The composite stack is real and is the `retype` mechanism.**
  `ExternalSymbols.stack` / `composite`
  ([`ExternalSymbols.fs:657`](../ExternalSymbols.fs)) is first-hit-wins with
  optional origin re-stamping. Hand-written overrides are *just a higher-priority
  layer* in front of the TS-derived provider — same as referenced-project beats
  referenced-assembly today
  ([`SymbolProviders.fs:57`](../../XParsec.FSharp.Codegen.Clr/SymbolProviders.fs)).
  No new machinery for overrides.
- **The rehydration shape to copy is real.** `ReferencedProject.fs` reads
  `manifest.toml` + `.fsi` contracts, kinds cross-package nominals at bake time,
  and rehydrates `Instantiate` closures from serialised data. The TS manifest
  loader copies this shape (a different on-disk schema, the same closure
  rehydration).
- **`TyOr` is now a real plan.** The brainstorm's biggest open fork (erased `U2`
  DU vs native anonymous union) is **decided: native**, scoped in
  [anon-unions-plan](anon-unions-plan.md). `T | null | undefined` and general
  `number | string` map to `TyOr`, not a synthetic `Core.Js` library type. This
  is the faithful TS match (TS unions *are* anonymous structural unions) and
  reuses the parser surface the CST already accepts.

## Mechanism: extractor + manifest, not a live tsc bridge

A TS-side **extractor** (full `ts.TypeChecker`, where the hard mapping is easy)
emits a serialised manifest; an **F# provider deserialises** it, rehydrating
builder closures. Not a per-lookup RPC into a live Node checker, because:

- **Thread-safety.** `TryLookup*` must be concurrency-safe (parallel per-file
  `PassContext`s); a single Node checker is single-threaded.
- **Closures don't cross the boundary.** `Instantiate: int -> SemType` and the
  `SemType[] -> SemType` builders mint fresh `TypeVar`s at a let-depth; only a
  *data description* can be serialised, which the F# loader rehydrates.
- **Build coupling.** A live bridge makes every compile depend on a running Node
  toolchain.

## Mapping table (updated for the `TyOr` decision)

| TS construct | Policy | SemType / seam |
|---|---|---|
| declared class/interface/alias (named) | nominal, direct | `TyClass`/`TyRecord`(name, args) |
| generics `<T>`, `<T extends Foo>`, `<T = string>` | typars + args; bound → constraint; default → `ExternalConstraint.Default` | `TyClass(name, args)` + builder closures |
| anonymous object `{x:number}` | content-hash → synthetic nominal name | `TyRecord(hash, …)` + side-table for readable name |
| `any` | dynamic/top, infectious | **new `TyDynamic`** + unifier absorb rule |
| `unknown` | top, forbids access until narrowed | map to `obj` in v1 (per [anon-unions-plan](anon-unions-plan.md)) |
| `never` | bottom | `TyOr []` |
| `T \| null \| undefined`, general `number \| string` | **native union** | `TyOr [members]` ([anon-unions-plan](anon-unions-plan.md)) |
| intersection `A & B` | erase | `TyDynamic` (or one side) |
| literal `"GET" \| "POST"` | erase to base (v1) | `string`/`number` |
| conditional/mapped (`Partial<T>`, `ReturnType<F>`) | query the *evaluated* type | concrete `SemType` snapshot; generic form lost |
| overloaded functions | overload set | `TryLookupMembers` (already exists) |
| `(a, b?, ...rest)` | curry per existing convention; optional/rest → policy | `TyFun` chain |
| module specifier / `namespace` / `declare global` | re-role `SymbolOrigin` | asm→specifier, ns→namespace, declType→class |

### `any` — the one genuinely new SemType surface

`TyDynamic` for `any`: absorbs on unification (`unify(TyDynamic, T) = TyDynamic`,
succeed, propagate no constraints — TS's infectious `any`) and permits any member
access. This is the only new front-end type the provider *requires* beyond
`TyOr`. (On a JS backend, `x.foo` is native property access — no DLR, no call
sites; `TyDynamic` is purely a checker concern that erases at codegen.)

### Structural → content-hash nominal name

Hashing the canonicalised shape recovers structural identity inside a nominal
system: two `{x:number,y:number}` hash equal and unify (correct TS semantics).
Traps: **cycles** (`interface Node { children: Node[] }`) need SCC +
canonical numbering before hashing (see `brainstorm-tarjan-scc.md` if it covers
cycle canonicalisation); **equality not subtyping** — a content hash is
exact-shape, so inflow (Vesper values into TS APIs) must match shape exactly or
coerce; keep the declared name in a side table for diagnostics.

## Manifest schema — a type-description IR

Not `SemType` (closures don't serialise). A small versioned JSON grammar:
`{typarRef:i}`, `{named:name,args:[…]}`, `{curriedFn:[args],ret}`, `tuple`,
`dynamic`, `union:[…]`, `structural:{hash,fields}`. The F# loader recursively
rehydrates into `SemType[] -> SemType` builders — same shape as
`ReferencedProject`. Version-stamp it (Node extractor and F# loader drift
independently). Extraction unit: batch-per-package first (cacheable), lazy-per-
symbol later (big for `@types/node` / DOM).

## The inverse problem

F#'s inference asks constraint questions TS metadata never answered — `when 'T :
equality`, comparison, SRTP `(+)`. The provider needs a default policy (assume
structural/`===` equality; or refuse generic-constrained use), since it won't
fall out of the manifest. For `TyOr` members this composes with
[anon-unions-plan](anon-unions-plan.md)'s all-members-or-defer constraint rule.

## Scope

**MVP / seed milestone:** mirror the CLR "printfn hi" discipline — one vertical
slice. A single non-generic interface with primitive-typed members + one free
function, round-tripping `.d.ts` → JSON manifest → F# provider → resolves in a
Vesper program (resolution verifiable against the CLR pipeline before
`Codegen.Js` can emit).

**Deferred:** `any`/`TyDynamic`; structural content-hash + SCC cycle handling;
conditional/mapped type evaluation; lazy-per-symbol extraction; literal-type
precision; the `retype` override file (lands as a composite layer when the first
lossy mapping bites).

## Prior-art reality check

This is ts2fable / Glutinum re-aimed at emitting `SemType` manifests instead of
F# binding source, consumed directly by the compiler. Their need for manual
cleanup is the evidence that "transparent" oversells it: the honest payoff is
auto-generating most of the surface under a fixed erasure policy + a small
`retype` override layer. Genuine wins over ts2fable: no intermediate compile
step, lazy on-demand resolution, and types stay live rather than frozen into a
binding lib.
