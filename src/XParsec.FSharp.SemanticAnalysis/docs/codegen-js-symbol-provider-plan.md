# Codegen.Js external symbol provider plan — consuming TS/JS via the TS toolchain

**Status:** MVP + per-feature breadth LANDED; the hard semantic pieces below are
deferred-by-design (each guarded by a loud `failwith`, never a silent gap). The
original design brainstorm has been deleted per the ephemeral-doc convention —
its open forks (erased `U2` DU vs native union, the overload "seam change", the
five gating decisions) are all resolved here and in code. This provider resolves
TS-sourced types into TAST and emits on the JS backend.

**This doc holds the DECISIONS (the why).** The R5 tranche that made cross-package
nominals real (refs-table identity → ambient-global extraction → `lib.es2015` ref
pack → `Js.*` consumption) has LANDED; its ephemeral step-by-step sequence doc is
deleted per the ephemeral-doc convention.

## The core idea

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

## Seam grounding (the decisions that are now load-bearing)

The current seam these decisions depend on:

- **The provider interface.** `IExternalSymbolProvider`
  ([`ExternalSymbols.fs:399`](../ExternalSymbols.fs)) — 8 members. Lookups return
  closures (`Instantiate: int -> SemType` minting fresh `TypeVar`s at a given
  let-depth) plus `ExternalConstraint` lists — the data a manifest must rehydrate.
- **Overloads are already solved.** The early "`TryLookupMember` is singular →
  seam change" worry is **obsolete**: the seam
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
- **`TyOr` is now a real plan.** The biggest early open fork (erased `U2`
  DU vs native anonymous union) is **decided: native**, scoped in
  the anonymous-union front-end (`TyOr`). `T | null | undefined` and general
  `number | string` map to `TyOr`, not a synthetic `Core.Js` library type (with
  `null`/`undefined` as distinct JS intrinsics — see the resolved subsection below). This
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
| `T \| null \| undefined` | **native union**, not `option`; `null`/`undefined` distinct intrinsics, **not** `unit` (resolved — see below) | `TyOr [T; null; undefined]` |
| intersection `A & B` | **v1 erase; flatten later** (resolved — see below) | `obj` → later `TyRecord(hash,…)` |
| literal `"GET" \| "POST"` | **faithful structural literal union**, external vocabulary only (resolved — see below); Vesper-source companion = string enums (landed) | `FTOr [FTLiteral …]` → `TyOr [TyLiteral …]` |
| conditional `A extends B ? X : Y` | **faithful node, ground-evaluated** (R4a; resolved — see below) | `FTConditional`, evaluated when both sides ground |
| mapped (`Partial<T>`, `ReturnType<F>`) | query the *evaluated* type | concrete `SemType` snapshot; generic form lost |
| overloaded functions | overload set | `TryLookupMembers` (already exists) |
| `(a, b?, ...rest)` | curry per existing convention; optional/rest → policy | `TyFun` chain |
| module specifier / `namespace` / `declare global` | re-role `SymbolOrigin` (resolved — see the cross-package/ref-pack section) | asm→specifier, ns→namespace, declType→class |
| cross-package named ref (`Map`, `Buffer`) | identity+kind BAKED at extraction (refs table); shape resolves via provider stack (resolved — see below) | `FTClass(TypeKey(home,…))`, members via `TryLookupMember` |

### Literal types stay structural, in the external vocabulary only (resolved — R4a)

**Decision (2026-07-01).** A TS string/number literal type maps to a new `FTLiteral`
(constant) arm composing with the EXISTING `FTOr` — `("ping" | "pong")` becomes
`FTOr [FTLiteral "ping"; FTLiteral "pong"]`, structurally, with **no anonymous nominal
name and no registry entry**. `TypeRef.Union → FTOr` already set the precedent that the
external vocabulary tolerates structural unions; literals reuse the same seam.

**Union member identity: order-preserving storage, order-INSENSITIVE equality.** Union
members live in a new `EqSet<'T>` (sibling of `EqArray`): insertion-ordered storage so
the DECLARED `.d.ts` order survives into diagnostics and the manifest golden, but
`Equals`/`GetHashCode` are set-semantic (member-set equality; commutative hash combine).
Canonical-sort normalisation was REJECTED: it requires a total order on `FrozenType`
that does not exist (and the tempting `%A`-sort shortcut is the known EqArray-in-`%A`
collision trap), whereas set equality reuses the member equality/hash that already
exists. The smart constructor also owns TS's SEMANTIC union rules — flatten nested
unions, dedupe, collapse a singleton to its member — because duplicates arise
POST-construction too (`TyOr [typar a; typar b]` instantiated at `a = b`): every
rebuild site (`instantiateWith`, freshen/map walks) must rebuild through the
constructor, never raw-copy the member list. Scope: `FTOr`/`TyOr` members ONLY —
tuples/args/typars stay positional `EqArray`. No `IComparable` until something truly
needs ordered keying. Do NOT key any cache on `%A` of these (the EqArray
`%A`-collision trap).

**The nominalism invariant (what keeps F# F#).** Vesper's own inference NEVER mints a
literal type: `"ping"` types as `string`, always; no Vesper binding generalises over a
literal; `TyLiteral` arises **only** by instantiating an external signature. The literal
matters solely *directionally, at the external-arg seam* — the same layer where `TyOr`
already builds on obj's directional `subsumes` — so the unifier learns no subtyping:

- a syntactic string/number CONSTANT argument checks against a literal union by set
  membership (`"ping"` admits into `TyOr [TyLiteral "ping"; TyLiteral "pong"]`);
- a Vesper string ENUM (landed: `type PingPong = | Ping = "ping" | Pong = "pong"`,
  JS repr already the bare string) admits when its case-VALUE set ⊆ the union — the
  nominal companion for code that wants to name/abstract the type;
- plain `string` is NOT admitted (directional, exactly like `T <: T|null` holding and
  its converse not);
- outward, a literal (union) WIDENS to its base primitive (`string`/`number`), so
  reading a literal-typed value back into Vesper needs nothing new.

**Why not the alternatives.** (a) *Synthesised anonymous nominal enums* (Glutinum's
`StringEnum` move) were rejected: case-name derivation from values (mangling,
collisions), content-hash naming for cross-signature/cross-package identity anyway, and
call-site ergonomics (`emit(Anon3f2a.Ping, 7)`) that end up needing the literal-admission
rule regardless — at which point the nominal wrapper adds only ceremony. This is a
deliberate deviation from Glutinum's three-way union split. (b) *General refinement
types* (singletons as refinements of `string` with real subtyping/flow-sensitivity) are
research-grade and would re-litigate the repo's architecture of quarantining directional
assignability at specific seams; the decided model IS the refinement idea restricted to
the one seam where it pays. TS's type system is undecidable in general — fidelity means
consuming what real packages export (via the real `tsc` in the extractor), not
re-implementing TS.

**`keyof` / indexed access / conditional build on top** (the mitt R4a constructs, each a
faithful schema arm + `FrozenType` node, ground-EVALUATED rather than degraded):

- `keyof T`, `T` ground to a record/interface → `FTOr` of `FTLiteral` member names;
  unground, the node is carried.
- `T[K]` → when `T` is ground and `K` is a KNOWN literal, fold to that member's type.
  The key becomes known through **call-site constant propagation**: a literal argument
  instantiating the method typar grounds `K` — the printf machinery is the in-repo
  precedent (a literal format string already drives external-call typing). A NON-literal
  key (read from a variable) degrades, documented: payload = union of member value
  types (or use the enum companion, which grounds `K` to its case set).
- `A extends B ? X : Y` → evaluate when ground; mitt's
  `undefined extends Events[Key] ? Key : never` folds once `Events[Key]` does (the
  `extends` test on a ground union is `TyOr` membership).

**Emission.** `FTLiteral` erases to its base primitive on BOTH backends (the runtime
value already IS the literal); `EmitJs.validatePlatformTypes` must admit it. No new
runtime representation anywhere.

### `T | null | undefined` → `TyOr`, **not** `option` (resolved)

The naive ML instinct is to strip null/undefined and wrap the remainder in
`option`. Rejected, because on the JS backend `option` is a **nominal heap DU**
(`Some`/`None` are tagged data objects — `EmitJs.fs`, imported external union),
whereas `TyOr` is **erased over `obj`**. Mapping `T | undefined` → `T option`
therefore reintroduces **runtime marshalling at every FFI boundary** (box into
`Some`, turn `undefined` into `None`) — destroying the zero-marshalling property
that motivated `TyOr`, and losing the fact that the surface was a JS nullable.

So: `T | null | undefined → TyOr [T; null; undefined]`, including exactly the
absence members TS wrote (`T | undefined → TyOr [T; undefined]`, etc.). `null` and
`undefined` are intrinsic types **distinct from `unit`** — but they sit at
**different layers** (see the step-0 subsection): `null` is **cross-backend** (it is
the canonical nullable on *both* targets — JS `null` and the CLR's F# 9 `T | null`
nullable-reference interop), so it is a *core* intrinsic with a per-target repr;
`undefined` is **JS-only** (no CLR analog), a JS-platform intrinsic with a value
literal.

Earlier this folded `undefined ↦ unit` because both emit JS `undefined` — that was
wrong. A codegen repr coincidence is **not** type identity (the freeze /
backend-knowledge separation): `unit` is inhabited (`()`); `null`/`undefined`
are absence sentinels. Folding them would make `string | undefined` interoperate
with `string | unit` and erase the nullability the checker must reason about.
(`TPatG.Null`'s `== null` conflation is a codegen-level pattern convenience, not
the type-level model.)

Nullability then needs **no separate machinery**: it is union membership, and
soundness is handled by the directional `subsumes` layer where `TyOr` already lives —
`T <: T|null` holds, `T|null <: T` does not, and narrowing (`!= null`,
`!== undefined`) is union-member removal, the same operation general TS-union
narrowing needs (deferred to that milestone). Collapsing to one `nullish`, or
promoting to `option`, stays an **opt-in `retype`/backend transform**, never baked
into the neutral extractor.

### `null` / `undefined` as JS-intrinsic types — DECIDED, step 0 (OUTSTANDING)

This is the next task and almost certainly **gates any real-API target**: every
non-trivial `.d.ts` is saturated with `T | null | undefined`, so it bites on the
first nullable member of the first real package (`mitt`, the seed target below).

**The decision** has two halves at two layers, because `null` is cross-backend and
`undefined` is JS-only:

- **`null` — a *core* (cross-backend) intrinsic type.** `TyOr [T; null]` is the
  canonical nullable on *both* targets: it is the CLR's F# 9 `T | null`
  nullable-reference interop (value-level `ldnull`) **and** JS `null`. So `null`
  registers as ONE intrinsic with a **per-target repr** (`IntrinsicRepr` already
  carries the per-backend platform repr — the same mechanism the numeric prims use),
  not as a JS-only entry in the `prim-types` companion. The payoff: the nullable
  machinery is shared infrastructure — doing it for the TS provider *also* lights up
  CLR nullable-reference interop, it is not a JS tax. NOTE: `null` already exists as a
  *value*/keyword, but that does **not** imply the *type* `null` is a registered
  intrinsic — the type side still needs doing for union membership +
  `validatePlatformTypes`.
- **`undefined` — a JS-only intrinsic type AND value, modeled like `unit` but never
  unified with it.** No CLR analog; it only ever enters from TS sources. It becomes a
  JS-platform intrinsic type with a single-inhabitant value literal — the *same shape*
  as `unit`/`()`, but a *distinct type*. "Like `unit`" is about the modeling shape, not
  identity: `unit` is inhabited (`()` is a real value you compute with); `null` /
  `undefined` are *absence sentinels*. Unifying `undefined` with `unit` would let
  `string | undefined` interoperate with `string | unit` and erase the nullability the
  checker must reason about. *Minor open fork (decide when implementing):* the value
  literal as a dedicated keyword (like `null`) vs an intrinsic binding. Low stakes.

**Repr coincidence stays in the backend.** `unit` and `undefined` are repr-coincident
on JS — both emit `undefined` at the value level (the unit→JS-`undefined` ABI). That
coincidence is exactly what made the earlier `undefined ↦ unit` fold tempting and
**wrong**: the "both emit `undefined`" knowledge lives in the JS backend repr, the
type identity stays distinct upstream (the freeze / backend-knowledge separation).
(`TPatG.Null`'s `== null` conflation is a codegen-level pattern convenience, not the
type-level model.)

**Verification entry point.** The provider today maps a TS union to `FTOr` with
`null`/`undefined` as **bare named members that are not registered intrinsics**
(`TsManifestProvider.fs`, the `TypeRef.Union` arm) — so a `string | null` reaching JS
emit is a candidate to trip the `validatePlatformTypes` BCL-fallback error. Check
whether a `unions.d.ts`-derived program survives JS emit *today*; that is the actual
starting point.

**One CLR caveat, flagged not solved:** `T | null` as `TyOr` is the *reference-type*
nullable story. CLR *value-type* nullability is `System.Nullable<T>` (`T?`), a
structurally different repr; TS/JS has no such distinction, so it only bites on the
CLR side. Don't let `T | null → TyOr` imply it covers CLR value types — that mapping
is separate and deferred.

Once `null`/`undefined` resolve, nullability needs no further machinery: it is union
membership on the directional `subsumes` layer where `TyOr` already lives
(`T <: T|null`, not the reverse), and narrowing (`!= null` / `!== undefined`) is
union-member removal.

### intersection `A & B` → erase in v1, flatten later (resolved)

Erase to `obj` in v1 (consistent with `unknown → obj`; `TyDynamic` is also
deferred). This is the same decision as "flatten into one record" at two
milestones, not a different one: flatten-via-`getPropertiesOfType` (the checker
pre-merges the members for you) only has somewhere to land **once the content-
hash structural-record machinery exists** — which is deferred below. So once
structural records land, an intersection *of object types* takes the
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

## Cross-package nominals, globals, and the JS ref pack (RESOLVED 2026-07-02)

**The problem (SOLVED in R5).** A foreign named type in a manifest (`mitt`'s
`all: Map<…>`) used to carry no home and no kind: `TsManifestProvider.toFrozen`
consulted only the manifest's OWN registry, so anything cross-package degraded to an
opaque `FTConst` — it survived as a carried nominal but its members could never
resolve. R5 closed this: mitt's `all` now resolves as a homed `Js.Map` and member
calls on it (`e.all.has("pong")`) type and run. Real packages make this a hard
requirement — `@types/node` and DOM surfaces are *built from* cross-package and global
references.

### Identity + kind are BAKED at extraction (decided)

The extractor records, for every foreign named reference, its **home** and its
**kind** — tsc already knows both: a symbol's `declarations[0].getSourceFile()` names
the declaring file, `program.isSourceFileDefaultLibrary` /
`isSourceFileFromExternalLibrary` (vendored binding `TypeScript.fs:4308`) classify it
(default lib vs `node_modules` package vs local), the package specifier/version comes
from `packageId`/nearest `package.json` (machinery the package entry mode already
has), and `SymbolFlags` give the kind (interface/class/alias/enum). Schema shape: a
manifest-level **refs table** — the ECMA-335 `TypeRef`/`AssemblyRef` analog — mapping
each foreign name to `{ home specifier; kind; arity }` (additive; exports stay the
"TypeDef" side). Reserved home specifiers for the default lib mirror TS's own lib
grouping (`es2015`, `dom`, …).

What is baked is **identity, not shape**: the referencing manifest never inlines the
foreign type's members (that is the staleness trap). `toFrozen` mints
`FTClass(TypeKey(home, …))` directly from the baked ref entry — no composite-resolver
two-phase load needed — and member access resolves through the ordinary provider
STACK at inference time (`TryLookupMember` with the home-qualified key), exactly as a
CLR TypeRef resolves against a loaded referenced assembly. A ref whose home manifest
is absent from the compilation fails at member access with a "package not referenced"
diagnostic, the moral equivalent of a missing assembly reference.

### The JS ref pack: extractor-generated, vendored, NEVER hand-authored (decided)

The standard library ships as committed, extractor-generated manifests over TS's own
`lib.es*.d.ts` — the analog of the CLR's per-compilation BCL ref set — bundled with
the compiler (or a `Vesper.Js` std-lib package; distribution point open). **No
hand-authored stubs**, even where that forces extractor complexity up front (the
ambient-global `declare global` entry mode, declaration MERGING across lib files, the
interface+constructor-var global pair): hand-authored surface drifts from real TS and
has no fidelity oracle. `JsNativeSymbols` shrinks to the intrinsic-repr seam
(`exn→Error`) as the pack absorbs the rest — its header always said it was the seam a
tsc-derived format plugs into.

**First target: full `lib.es2015`, run as a diagnostics burndown** (the failure
contract's coverage-golden machinery, below) — LANDED as the vendored
`test/ts-fixtures/es2015` pack (mounted under `Js`, driven end-to-end by the `Js.Map`
gate); the committed diagnostics report ranks the extractor gaps by frequency so the
high-value wins surface empirically instead of by curation.

### Vesper surface: the `Js` namespace, mirroring TS's lib structure (decided)

Ref-pack types mount under `Js` — `Js.Map<string, int>`, `Js.Promise<'T>` (the
`ns→namespace` re-role in the mapping table). Non-optional, not just tractability:
Vesper's own core claims `Map`/`Set`, so unqualified globals would collide.
Packaging/namespacing mirrors TS's own lib structure: the ES language core flattens
into `Js` (the version-suffixed `lib.es5`/`lib.es2015.*` files are TS's
compile-target mechanism, not semantic namespaces), host environments split
(`Js.Dom`, later). `Js.*` is JS-target-only by design — the `Vesper.Exceptions`
JS-only-contract precedent; nothing pretends `Js.Map` is `Dictionary`. (A future
portability layer — a `Vesper.Platform.Map` forwarding to `SCG.Dictionary`/`Js.Map`
per target — is RECORDED as a possibility, explicitly out of scope.)

### Emission: globals import nothing

A ref-pack type is ambient in every JS runtime. Fable's precedent is the
`[<Global>]` attribute (bypasses import emission); ours is the same fact as data: a
`Global` marking on the external class shape/origin (sibling of `AttachMembers`,
which is likewise Fable-named) that `JsImports` consults to emit NO import. Without
it the pack would emit `import { Map } from "es2015"` — wrong the moment it runs.

## Failure contract: resilient extraction with diagnostics (DECIDED)

The extractor behaves like a **compiler front-end**, not an all-or-nothing
transform: it always emits a **best-effort manifest + a structured diagnostics
list**, never aborting a whole package because one type didn't map. This is the
production shape — in the end state, `npm install` drops packages into
`node_modules`, the Vesper LSP batches an extraction over them, caches the
manifests, and serves design-time intellisense/type-checking *plus* surfaces the
extraction diagnostics in the editor. An extractor that threw on the first awkward
type would make the LSP useless against any real library; a partially-typed
`querySelector` beats a missing one.

This does **not** retreat from "skip rather than fake" — you still never invent a
wrong type. It moves the loud failure from a `failwith` (aborts) into a *diagnostic*
(records + continues), and moves the CI rigor from "extraction throws" to "the
**diagnostics set drifts**" (a golden fixture asserts its expected diagnostics; a
clean package asserts *none*).

### Two tiers of failure

- **Fatal — extraction aborts.** I/O and resolution plumbing only: package not
  found / not a module / no exports (the ambient-global DOM case), source file
  unreadable, specifier unresolvable (`Extractor.fs` lines ~708/731/808/825). There
  is nothing to be resilient *about* — the run cannot proceed. These stay throws.
- **Per-symbol diagnostic — extraction continues.** Every *type-mapping* failure
  (method-axis typar `:166`, structural object `:241`, asymmetric accessor `:324`,
  …) converts to a diagnostic attached to the symbol, and the walk proceeds.

### Severity = fidelity, with a low bar to "degrade"

The rule (decided): **if we can name that a type/symbol exists, we degrade it; we
omit only when it cannot be named at all.**

- **Warning — low-fidelity (the common case).** Extracted but lossy, the type
  *resolves* (intellisense works) and the checker is *told* precision was lost:
  `any → TyDynamic`; structural object → content-hash stub (until that machinery
  lands); intersection → `obj`; literal → base type; and **method-axis typar →
  erased to `obj` + warning** (rather than the current abort — a degraded generic
  method is far more useful at design time than an absent one).
- **Error — unmappable (rare).** Only when the symbol can't even be named → omitted
  from the manifest + diagnostic. With the "can we name it?" bar this is a small
  residual.

Crucially the warning must be **load-bearing in the checker**, not cosmetic: a
low-fidelity type *is* the `TyDynamic`/`FTUnknown` stub that infects inference so
downstream type-checking doesn't over-promise. The diagnostic is the *report*; the
dynamic/stub type is the *checker-level consequence* — one mechanism, two ends. This
is why "degrade to `obj`/dynamic" and "emit a warning" are the same act.

### Shape of a diagnostic

- **Diagnostics live INSIDE the manifest** (decided): a top-level `diagnostics: [ …
  ]` channel, serialised with the type IR, so the LSP reads them from one cached
  artifact without re-running Node. (One cache entry, coherent with the manifest it
  describes.)
- **Stable codes**, compiler-style (`FS0064`/`TS2304` analog) — e.g.
  `method-axis-typar-erased`, `structural-object-stubbed`, `intersection-erased`,
  `any-dynamic`, `literal-widened`. The test oracle asserts on **codes + counts**,
  not message strings (robust to wording); the LSP groups/filters/suppresses by code.
- **Spans reuse the AST-backlink** the *Authored form* section already specs (a
  `{file, span, text}` coordinate, since `ts.Node`s don't serialise). Diagnostic
  spans *are* that coordinate — one machinery, not two — so the LSP can place a
  squiggle in the `.d.ts`.

### What this makes the test harness

One resilient extractor, two views over its `(manifest, diagnostics)` output:

- **Golden fixture (`mitt`):** assert manifest matches golden AND diagnostics ==
  expected (empty for a clean curated package). Any new diagnostic fails the test —
  either the package wasn't as clean as assumed, or a regression degraded fidelity.
- **Coverage golden (`@types/node`, DOM):** commit the diagnostics report; ranked by
  code frequency it is a **burndown chart** — codes vanish as features land, no
  special extractor mode required.

### Test ownership: three tiers, coupled only through a committed contract

The producer (Extractor tests) and consumer (Codegen.Js tests) **are** coupled — but
healthily, the way two code projects couple through a `.fsi`: the manifest is the
published contract. The discipline that keeps it healthy is that the coupling is a
**committed artifact, never a live extractor run at consume-time**. So three tiers
with distinct jobs:

| Tier | Owned by | Coupling | Catches |
|---|---|---|---|
| Extractor golden (`testExtractorMatchesGolden[Package]`) | producer | — | output stability / canonical form / schema drift |
| Synthetic consumer (`emitWithCalc` + inline manifest) | consumer | none (hand-written manifest) | provider/emit behaviour in isolation |
| Real-package e2e (`.fs` → emit → run under Node vs real runtime) | consumer | committed contract | the manifest is *semantically* correct end-to-end |

The tiers are **complementary, and that is why the coupling pays**: the Extractor
golden only proves the output is *stable* (it memorialises whatever the extractor
emits — a wrong-but-self-consistent manifest sails through); the real-package e2e is
the *semantic* oracle the golden cannot be, because a typed Vesper program built from
the manifest must actually emit JS that **runs against the real vendored runtime** and
returns the right result. The consumer validates the producer's contract behaviourally.

Hard rules:
- **Codegen.Js tests read committed files only** (`.manifest.json` + the vendored
  runtime `.mjs` + the `.fs` program); they never invoke the Node extractor. No
  `ProjectReference` from `Codegen.Js.Tests` → `Extractor.Tests` ever appears.
- **The shared package fixture lives in a NEUTRAL, test-level location** — not inside
  either test project (today `pkgs/` sits under `Extractor.Tests`, which would force
  `Codegen.Js.Tests` to reach across with a `../Extractor.Tests/pkgs/` path — a false
  ownership + fragile path). Hoist to e.g. `test/ts-fixtures/<pkg>/` holding the
  vendored `.d.ts` + `package.json` + runtime `.mjs` + golden `.manifest.json`; both
  projects reference it by path. (Avoid a name the repo `.gitignore`
  `**/[Pp]ackages/*` rule swallows — the reason the current dir is `pkgs/`.) Do this
  with `mitt` as the first tenant, before the layout calcifies across many packages.
- **CI must run the Node golden-diff** for the contract to mean anything. A
  checked-in generated artifact is only as honest as the environment that regenerates
  it: with Node absent, `testExtractorMatchesGolden` *skips* and a stale manifest can
  pass the consumer e2e against an outdated contract. The pure-F# canonical
  round-trip catches *schema-shape* drift without Node, but not *semantic* drift — so
  the producer side is silently unverified unless CI has Node.

### Note: no schema versioning during prototyping

We are prototyping; all manifests live only in this repo and are regenerated with
`UPDATE_SNAPSHOTS=1`. So the `diagnostics` channel (and any other manifest change)
goes in **without** a `SchemaVersion` bump or back-compat shim — just regenerate the
goldens. Revisit version-stamping only if/when a manifest is ever published outside
the repo.

## Manifest schema — a type-description IR

Not `SemType` (closures don't serialise). A small versioned JSON grammar:
`{typarRef:i}`, `{named:name,args:[…]}`, `{curriedFn:[args],ret}`, `tuple`,
`dynamic`, `union:[…]`, `structural:{hash,fields}`, plus the top-level
`diagnostics:[…]` channel (see *Failure contract* above). The F# loader recursively
rehydrates into `SemType[] -> SemType` builders — same shape as
`ReferencedProject`. (No `SchemaVersion` while prototyping — manifests are
repo-only and regenerated; version-stamp only if one is ever published externally.)
Extraction unit: batch-per-package first (cacheable, keyed on
`packageId.version`), lazy-per-symbol later (big for `@types/node` / DOM — and the
LSP wants it so an `npm install` of a DOM-sized package doesn't pay full extraction
up front).

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

F#'s inference asks constraint questions TS metadata cannot settle — `when 'T :
equality`, comparison, SRTP `(+)`. The provider needs a default policy (assume
structural/`===` equality; or refuse generic-constrained use), since it won't
fall out of the manifest. For `TyOr` members this composes with the all-members-or-defer constraint rule.

## Scope

**MVP / seed milestone — LANDED.** Mirrored the CLR "printfn hi" discipline: a
single vertical slice plus per-feature breadth, all on hand-authored single-feature
`.d.ts` fixtures (`test/Vesper.Ts.Extractor.Tests/specs/*`).

**First real-package target — `mitt`.** The fixtures are synthetic feature-isolation
files; nothing has been run against a real npm package. `mitt` (a tiny, stable event
emitter — one `.d.ts`, real module exports, generics + function-typed members +
nullable returns) is the seed real-API target: it confirms the
`node_modules → manifest → provider → JS emit` path survives a real package without
trying to eat an elephant. Climb from there toward overload/generics stress
(`date-fns` / `@types/lodash`) and breadth (`@types/node`). The DOM is the eventual
*destination* (browser is the only place a JS target earns its keep over CLR/native)
but the *capstone*, not an early bite: it is ambient/global (`declare global`, no
module exports), so it needs a second **ambient-global extraction entry mode** the
module-based `extractPackage` lacks, on top of every deferred feature below firing at
once. Prerequisite for any of these: the `null`/`undefined`-intrinsic step above.

**Deferred — but now *degraded*, not *blocking* (see *Failure contract*).** Each of
these emits a low-fidelity result + a warning diagnostic rather than aborting:
`any → TyDynamic`; structural object → content-hash stub (SCC cycle handling still
deferred for the *faithful* form); conditional/mapped → concrete snapshot;
literal → base type; method-axis typar → erased to `obj`. Genuinely deferred (no
degraded form yet): the `retype` override layer (lands as a composite layer when the
first lossy mapping bites), lazy-per-symbol extraction, and the ambient-global
(`declare global`) extraction *entry* mode (a fatal "not a module" today; gated on
the DOM/browser target). Prerequisite for any real-package run: the
`null`/`undefined`-intrinsic step above.

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
