# Codegen.Js external symbol provider plan — consuming TS/JS via the TS toolchain

**Status:** plan, not started. **Supersedes**
[brainstorm-codegen-js-symbol-provider](brainstorm-codegen-js-symbol-provider.md)
(kept for the original design discussion; this plan is the up-to-date,
seam-accurate version). This provider resolves TS-sourced types into TAST, but
nothing it resolves is runnable until the JS backend can emit. It *can* be
prototyped against the CLR pipeline
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

### API durability: Strada now, Corsa-isolated

The extractor runs on the **classic `typescript` package** (the "Strada" API:
`ts.createProgram` + `TypeChecker`). Two caveats that shape the boundary:

- That API has **never carried a semver stability guarantee** — Microsoft
  documents it as subject to breaking changes between any releases.
- **TypeScript 7 ("Corsa", the Go native port)** drops the Strada programmatic
  API entirely, and no stable replacement is expected before ~7.1.

Neither caveat reaches us *if the extractor is isolated behind the serialised
manifest schema* — which this design already enforces (the F# side never sees a
`ts.*` object, only the JSON IR). So the discipline is: pin the extractor to a
known `typescript` version, treat the manifest schema (not the TS API) as the
stable contract, and swap the extractor backend at the Corsa transition without
touching the F# loader. A working extractor skeleton is sketched in
[`ts-bridge-extractor.sketch.ts`](ts-bridge-extractor.sketch.ts); its mechanics
are detailed under *Manifest schema* below.

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
  the anonymous-union front-end (`TyOr`). `T | null | undefined` and general
  `number | string` map to `TyOr`, not a synthetic `Core.Js` library type (with
  `null`/`undefined` ↦ `unit` — see the resolved subsection below). This
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
| `unknown` | top, forbids access until narrowed | map to `obj` in v1 |
| `never` | bottom | `TyOr []` |
| general `number \| string` | **native union** | `TyOr [members]` |
| `T \| null \| undefined` | **native union**, not `option` (resolved — see below) | `TyOr [T; unit]` |
| intersection `A & B` | **v1 erase; flatten later** (resolved — see below) | `obj` → later `TyRecord(hash,…)` |
| literal `"GET" \| "POST"` | erase to base (v1); nominal enum once literal precision lands | `string`/`number` |
| conditional/mapped (`Partial<T>`, `ReturnType<F>`) | query the *evaluated* type | concrete `SemType` snapshot; generic form lost |
| overloaded functions | overload set | `TryLookupMembers` (already exists) |
| `(a, b?, ...rest)` | curry per existing convention; optional/rest → policy | `TyFun` chain |
| module specifier / `namespace` / `declare global` | re-role `SymbolOrigin` | asm→specifier, ns→namespace, declType→class |

### `T | null | undefined` → `TyOr`, **not** `option` (resolved)

The naive ML instinct is to strip null/undefined and wrap the remainder in
`option`. Rejected, because on the JS backend `option` is a **nominal heap DU**
(`Some`/`None` are tagged data objects — `EmitJs.fs`, imported external union),
whereas `TyOr` is **erased over `obj`**. Mapping `T | undefined` → `T option`
therefore reintroduces **runtime marshalling at every FFI boundary** (box into
`Some`, turn `undefined` into `None`) — destroying the zero-marshalling property
that motivated `TyOr`, and losing the fact that the surface was a JS nullable.

So: `T | null | undefined` → `TyOr [T; unit]`, erased, zero-cost. Mechanics that
already exist: `undefined` ↦ the `unit` repr (`unit` *is* `undefined` on JS —
`JsEmitHelpers.fs`), and `null` collapses to that same absence member, matching
`TPatG.Null`'s `== null` (which deliberately matches both `null` and `undefined`
— `EmitJs.fs`). Option-promotion stays available as an **opt-in `retype`
override / backend sugar**, never baked into the neutral extractor.

### intersection `A & B` → erase in v1, flatten later (resolved)

Erase to `obj` in v1 (consistent with `unknown → obj`; `TyDynamic` is also
deferred). This is the same answer as "flatten into one record" at two
milestones, not a different one: flatten-via-`getPropertiesOfType` (the checker
pre-merges the members for you) only has somewhere to land **once the content-
hash structural-record machinery exists** — which is deferred below. So once
structural records land, an intersection *of object types* flows through the
**same** content-hash record path for free; non-object intersections
(function/branded) stay erased. Gate the flatten on the structural-records
milestone, not before.

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

Load-context mechanics (from the [`ts-bridge-extractor.sketch.ts`](ts-bridge-extractor.sketch.ts) sketch — the concrete
"`Assembly.Load`"): build a `ts.Program` over a **synthetic entry file** that
`import * as`-es each requested package, so the module graph pulls in their
`.d.ts` closure; resolve each package's module symbol via `resolveModuleName` +
`getSymbolAtLocation`/`getMergedSymbol`; enumerate with
`getExportsOfModule`; dispatch on `SymbolFlags`
(Function/Class/Interface/TypeAlias/Enum/Module/Variable). Non-obvious
`CompilerOptions`: `skipLibCheck: true` (read the world, don't validate it),
`types: []` (don't auto-pull ambient `@types`), `moduleResolution:
Bundler`/`NodeNext`, `noEmit`. The per-package **version** for the manifest
stamp / cache key is free from `resolveModuleName(...).resolvedModule.packageId.version`.
(Glutinum builds its program with `@ts-morph/bootstrap`'s `createProjectSync`
instead of hand-rolling the `CompilerHost` — a friendlier alternative worth
considering for the real extractor.)

## Operational metadata the manifest must carry (FFI lowering)

Because the lowering target is a first-class FFI **intrinsic expression**
(`(# … #)` — `Expr.ILIntrinsic`/`TExprG.ILIntrinsic`), richer than Fable's
`[<Emit>]` string templates, the neutral IR must carry **operational** semantics,
not just types — that's what selects the right intrinsic. The seam mostly already
models this (`ExternalMember`: `IsStatic`, `IsProperty`, ctor encoded as
`Name = ".ctor"`, `MemberKind`, two-axis arity, `OptionalDefaults`). What the
extractor must therefore emit, and what is *not* yet covered:

- **Construct signatures.** The sketch captures `getCallSignatures()` but **not
  `getConstructSignatures()`** — yet our model encodes constructors as `.ctor`.
  Emit construct signatures too, or every `new`-intrinsic target goes missing.
- **Accessor kind / optional / rest.** `SymbolFlags.GetAccessor`/`SetAccessor` vs
  `Method` vs `Property`; `questionToken`/`initializer` → optional;
  `dotDotDotToken` → rest. (Sketch already covers optional/rest/static/readonly.)
- **ImportShape — the genuinely missing field.** `ExternalMember` records *what* a
  symbol is but not *how* it is exported: default export, named, namespace, or
  CommonJS `export =`. That distinction picks the import intrinsic (`new`-on-
  default-import vs named-binding, etc.), so the manifest needs an explicit
  `ImportShape` per top-level symbol. Follow re-exports with `getAliasedSymbol`;
  detect `export =` explicitly. The plan's "re-role `SymbolOrigin`
  (asm→specifier)" is coarser than this and does not subsume it.

## Authored form: alias view + AST backlink

Checker-driven extraction loses the *authored* shape (conditional/mapped types
resolve to a concrete snapshot — "generic form lost" above). Recover it cheaply
without re-deriving TS semantics: the checker keeps the authorial alias on the
resolved type for free — `type.aliasSymbol` + `type.aliasTypeArguments` (e.g.
`Partial<Config>` still carries `aliasSymbol → Partial`, `aliasTypeArguments →
[Config]`), already resolved to symbols so it **serialises cleanly** (store the
alias *name*). For everything the checker drops (exact text, union-member
ordering, JSDoc), keep an optional **AST backlink** — but nodes don't serialise,
so it is a `{file, span, text}` coordinate, not a node pointer. This authored
view feeds diagnostics and the future `retype` override layer; it is enrichment,
never the primary type channel.

## The inverse problem

F#'s inference asks constraint questions TS metadata never answered — `when 'T :
equality`, comparison, SRTP `(+)`. The provider needs a default policy (assume
structural/`===` equality; or refuse generic-constrained use), since it won't
fall out of the manifest. For `TyOr` members this composes with the all-members-or-defer constraint rule.

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

Verified against the Glutinum source (`reference/cli`), three of its choices
corroborate decisions here:

- **The two-IR pipeline is real.** `GlueAST` (`GlueType`, TS-shaped,
  attribute-free) → `Transform.fs` → `FSharpAST` (`FSharpType`, carries Fable
  attrs) → printer. That is exactly the neutral-manifest / target-lowering split
  this plan draws — our manifest is the GlueAST analog, our intrinsic lowering
  the FSharpAST analog.
- **It is checker-driven, not AST-only.** Despite the folklore, Glutinum's reader
  walks the declaration AST *and* leans on the TypeChecker
  (`getTypeFromTypeNode`, `getPropertiesOfType`, `getConstantValue`, keyof
  expansion), so the checker choice here has direct precedent.
- **Its union story matches our classifier.** Glutinum emits Fable `U{n}` erased
  unions for general unions (no arity cap in *its* code — the `U2..U9` limit is
  Fable.Core's), diverts string-literal unions to `StringEnum`, and numeric-
  literal unions to a real `enum`. Same three-way split the mapping table adopts
  (`TyOr` / literal→enum), reached independently. (Two transcript claims the
  source *refutes*: there is no `Glutinum.X`-vs-`.Extensions` regeneration-safety
  convention, and "union arg → multiple F# overloads" is an unimplemented
  aspiration — union args become `U{n}`/`StringEnum`.)

Test model worth copying: Glutinum's golden specs are per-feature
`tests/specs/references/<feature>/name.d.ts` + expected-output pairs. The MVP's
`.d.ts → manifest → resolves` slice can adopt the same isolated-fixture
discipline.
