# Porting the Vesper library to the JS target — scope

Working document. Ephemeral: delete it when the work lands.

Written against `semantic-analysis` @ `82fe318e`. Every number below came from actually
running `ConformancePass.checkManifest "js"` + `ConformancePass.enforce` over all eleven
`src/Vesper.*` manifests. Where I could not establish something I say so rather than
estimating.

**T1, T2, T3's Kind B half, T4, T5 and T6 have landed.** The run is at **5** errors, down from 24. The
error set is now pinned by a committed test rather than a throwaway one —
`ConformanceTests.fs`'s *"js: the hard-error set is exactly the un-ported library surface"*
— so every tranche below has to shrink that list to be believed.

---

## Decisions taken

**`Vesper.Set` is out of scope.** The port stops at the coherent small core: Core,
Array, List, Seq, Comparison, Choice, Option, Result, Exceptions. Deferring costs
1 of 24 errors and nothing waits on it.

**Computation expressions, including `seq { }`, are deferred.** They are greenfield
— no front-end node, no backend support, on either target — and nothing in the port
needs them. `Seq.truncate` gets the hand-rolled enumerator class, which is not a
workaround: it is what a compiler would generate anyway.

When they are picked up they are their own project, and one thing is worth knowing
in advance because it inverts the usual intuition. On JS a sequence expression
lowers to a generator (`function*` / `yield`), close to 1:1. On CLR it needs a
compiled state machine. So the JS half is the cheap half, and a CE project scoped
CLR-first would hit the expensive end before proving the design.

---

## 0. The number

**5 hard conformance errors on `js`, all in Vesper.Core. 0 on `clr`.**
(Was 24 across 7 before T1 + T2 + T3's Kind B half + T4 + T5 + T6.)

Conformance is necessary but no longer sufficient — it pairs contracts with impls and does
not compile them. Two blockers it cannot see are recorded at the end of §7.

| package | js errors | was | clr errors |
|---|---|---|---|
| Vesper.Core | 5 | 14 | 0 |
| Vesper.Seq | 0 | 2 | 0 |
| Vesper.Array | 0 | 1 | 0 |
| Vesper.List | 0 | 2 | 0 |
| Vesper.Printf | 0 | 3 | 0 |
| Vesper.Comparison | 0 | 1 | 0 |
| Vesper.Set | 0 | 1 | 0 |
| Vesper.Choice / Exceptions / Option / Result | 0 | 0 | 0 |

Accepted with no error and no exemption list, derived from file content:
`PairOutcome.Unrepresentable` — `prim-types-decimal.fsi`, `prim-types-nativeint.fsi`,
`prim-types-nd-array.fsi`, `prim-types-attr.fsi`, `capabilities.fsi`,
`capabilities-compat.js.fsi`; `PairOutcome.RuntimeServed` — `ops-platform-runtime.js.fsi`,
`comparison-runtime.js.fsi`. `array-index.js.fsi` is no longer among them: it is now
`Paired` with its body and conforms.

`capabilities.fsi` landing there is *correct and load-bearing*, not a gap — see §4.

---

## 1. Verified inventory, per package, split three ways

### Kind A — deliberately absent and correct; the conformance pass was wrong about them (6 errors) — ✅ RETIRED (T2)

These needed **no library source at all**. Each was a machinery fix in `ConformancePass` /
the manifest. What the pass now concludes, and why:

| # | file | verdict now | the rule that produces it |
|---|---|---|---|
| A1 | `Vesper.Core/capabilities-compat.js.fsi` | `Unrepresentable(f, [])` | the rule is now stated once: *a contract owes a `.fs` unless every declaration in it is satisfied without one*. An `extern` is (its whole body would be a repr this target does not bind) and so is a transparent abbreviation (it resolves through). Five abbreviations and nothing else ⇒ nothing owed. The old guard additionally demanded a non-empty `extern` list, which its own doc-comment never claimed |
| A2 | `Vesper.Core/ops-platform-runtime.js.fsi` | `RuntimeServed(f, "Vesper.Core.mjs", …)` | same rule, third clause: a `val` is satisfied without a `.fs` **exactly when the target's committed `runtime` asset exports it**. Derived from the manifest's own `runtime` key and *checked against the asset*, so it is not an assertion — rename the export and the contract is a V240 hard error again. This is also §4's asked-for export-presence check, arriving as the verdict rather than beside it |
| A3 | `Vesper.Comparison/comparison-runtime.js.fsi` | `RuntimeServed(f, "Vesper.Comparison.mjs", …)` | same |
| A4 | `Vesper.Core/array-index.js.fs` (was `array-index-body.js.fs`) | `Paired` with `array-index.js.fsi`, no errors | the file is renamed to its contract's stem. The manifest's stated reason for the odd name was **unfounded**: `extractIntrinsicReprsInto` runs over *every* body the manifest names, with no reference to `pairingStem`, so the bare `` `[]` `` ⇒ `"!0[]"` repr was already bound from this file and still is. Both halves of the two-way bug close at once — the body is no longer contract-less, and the contract is no longer waved through as owing nothing |
| A5–A6 | `Vesper.Printf/structural-printer.fsi` ← `structural-printer.js.fs` | not compared at all | two statements, not a suppression. (1) `structural-printer.fsi` is the **CLR** `%A` engine's surface, so it moves to `[targets.clr] files` alongside `formatter.fsi` — a contract not in the JS file set is not checked on JS. (2) `structural-printer.js.fs` implements no contract, which the manifest can now say: a new `impl-only` key, the mirror of `sig-only`. A body named there is withheld from the pairing candidate set, so the stem rule cannot marry it to a `.fsi` it does not implement; an `impl-only` entry naming nothing the target compiles is a V243 hygiene error |

**What did NOT change, deliberately.** `ImplWithoutContract` (V242) is still a hard error
for an *undeclared* contract-less body, and the `Unrepresentable` counterweight still
refuses anything body-bearing: `core-types.fsi`, `structural-format.fsi` and
`compiler-attributes.fsi` remain V240.

`int-comparison.fsi` used to be A2/A3's negative control — all `val`s, in a package that
ships an asset, a hard error purely because `Vesper.Core.mjs` exports no `<`/`>`/`<=`/`>=`.
B6 gave it a body, so that control is gone. It is replaced by a stronger one:
`ConformanceTests.fs`'s *"js: rename the asset's export and the contract owes a `.fs`
again"* materialises a one-contract package twice, changing only the exported identifier in
the `.mjs`, and asserts `RuntimeServed` then V240. Both polarities, on ground no library
port can retire.

### Kind B — genuinely missing library source, writable today (1 error left of 10)

No compiler feature needed. This is F#-writing work.

| # | file to write / extend | missing declarations | size |
|---|---|---|---|
| B1 ✅ | `Vesper.Core/prim-types-min.js.fs` | *done.* `type 'T ``[]`` = (# "!0[]" #)` + `type 'T array = 'T[]`, mirroring `prim-types-min.clr.fs:26-27`. The caveat below resolved: inert |
| B2–B5 ✅ | `Vesper.Core/ops-platform.js.fs` | *done.* `ignore` = `(# "void $0" #)` (JS's own discard, so the operand is still evaluated — the CLR's bare `()` says nothing about that); `isNull` = `(# "$0 === null" #)`, STRICT, `undefined` being a separate type here with its own value; `box` = the erasing `(# "" #)`, JS having nothing unboxed to move; `invalidArg` = `failwith` over the BCL's own `"{message} (Parameter '{name}')"` text, every exception erasing to `Error` |
| B6 ✅ | `Vesper.Core/int-comparison.js.fs` | *done.* `IntComparison.(<) (>) (<=) (>=)` as the native relational operators |
| B7–B8 ✅ | `Vesper.List/list.fs` | *done,* subsumed by the merge (§3). `toSeq` is a `:> seq<'T>` upcast, which erases on JS (`export const toSeq = (list) => list` — the cons-list IS iterable); `ofSeq` is `for x in source`. Both node-tested on the generated asset (`ListTests.fs`) |
| B9 ✅ | `Vesper.Array/array.js.fs` (new) | *done, but not as a port* — see §3's third consequence. `newarr` is a named IR node the JS backend already implements, so the CLR body compiled as written on JS. Split instead into `array-prelude.{clr,js}.fs` (the intrinsics) + a neutral `array.fs` (all 16 functions, no inline IL) | ~10 lines of prelude; the module itself unchanged |
| B10 ✅ | `Vesper.Seq/seq.js.fs` (new) | *done, and no `seq.js.fs`.* `seq.clr.fs` → one neutral `seq.fs` carrying all four: `toArray` on a doubling buffer over `Array.zeroCreate`, `truncate` a hand-rolled lazy cursor. Both BCL reaches (`ResizeArray`, `Enumerable.Take`) were convenience. Took the CLR repr-hop fix to share the cursor | ~45 lines of cursor + ~20 of `toArray` |

**B1 caveat — settled, it is inert.** `PlatformTypes.fs:26` states the rule: *"a target that
CAN represent a structural constructor (`'T []`) says so by binding its repr"*. There is no
collision to have: `IntrinsicMarkers`/`IntrinsicReprs` are keyed by the declaration's SHORT
name (`ReferencedProject.fs:629-633`, read at `VesperLib.fs:1409`), so the bare/global
`array-index.js.fs` entry and the `Vesper`-namespaced `prim-types-min.js.fs` one are the
same key with the same value — which is also why arrays were never `Unsupported` on JS
before B1 landed. The `.fs` addition registers no type either; both `.fsi` files already
declare `` `[]` ``.

### Kind C — blocked on a compiler/codegen feature, or on a scope decision (8 errors)

| # | contract | error | the actual blocker |
|---|---|---|---|
| C1 | `prim-types-min.fsi` → `Fun` | `Fun<'A,'B>` … `Fun<..,'E>` (4 arities) absent from `prim-types-min.js.fs` | interface **declarations** emit nothing on JS (`EmitJs.fs:1096` only records the key in `LocalInterfaces`), so this is *probably* a pure source addition. Unverified: I did not compile it. Low risk, but it is the one Kind C item that may turn out to be Kind B |
| C2 | `prim-types-object.fsi` → `obj` | HeritabilityMismatch | the contract says `type obj = extern class` (heritable); `prim-types-object.js.fs:5` binds the untagged `(# "unknown" #)`. JS has no heritable base to point at, and JS class inheritance does not exist (C3). Either the contract splits per target, or `(# class "Object" #)` becomes meaningful on JS. **Design call, not a port task** |
| C3 | `compiler-attributes.fsi` | V240 | every one of the 8 types is `inherit Attribute`, and (a) `Attribute` is `Unrepresentable` on JS (`prim-types-attr.fsi`), (b) **class inheritance hard-fails on JS**: `EmitJsTypes.fs:409-412`, `"class inheritance is not yet supported on the JS target"`. Confirmed independently by `test/Codegen.Conformance/manifest.toml`: *"`inherit` … has no program here: the JS backend has no `extends`/`super(…)` and REJECTS it"* |
| C4 | `core-types.fsi` | V240 | `core-types.fs` is target-neutral in content: a `[<ReferenceEquality>]` record with a mutable field, two classes with primary ctors implementing `Fun<…>`, and a module of upcasts. **Probably just a missing entry in `[targets.js] impl`**, but it depends on C1 first (it references `Fun`) and it stacks two things with *zero* JS test coverage: (a) a record with a `mutable` field — `IsMutable` (`TastDecl.fs:325`) is read by the CLR backend (`LayoutNodes.fs:443`) and **never** by the JS one; a record is a plain class with assignable properties and `FieldSet` emits `r.X = v` unconditionally (`EmitJs.fs:256-266`), so it should work, but `RecordTests.fs` has no `mutable` case; (b) a **class** implementing a plain non-capability interface — the only golden/E2E for that is on a *record* (`ClassEmitTests.fs:224,248`). Both go through the same code path, so this is "very likely fine, no evidence" |
| C5 | `structural-format.fsi` | V240 | `structural-format.fs` is two pure interface declarations. Same story as C4 — probably a missing `[targets.js] impl` entry. Untested |
| C6 ✅ | `Vesper.Printf/formatter.fsi` | *retired (T1)* | the contract itself is CLR: `open System.IO` / `System.Text` / `System.Runtime.CompilerServices`, `new: … * writer: TextWriter`, `… * builder: StringBuilder`. Also 3 constructor overloads, and JS rejects secondary ctors (`EmitJsTypes.fs:414-417`). **printf on JS never routes through `Formatter`** — the manifest says printf is front-end special-cased into `Format` nodes. This contract has no JS role at all |
| C7 ✅ | `Vesper.Seq/struct-seq.fsi` | *retired (T1)* | the *contract* names `System.Collections.IEnumerable` (non-generic — **not** in `capabilities-compat.js.fsi`), `IEnumerator`, `IDisposable`, and every type is `[<Struct>]`. The whole point of the file is zero-allocation by-value struct chaining, which is a CLR performance construct. This is CLR-only by nature |
| C8 ✅ | `Vesper.Set/set.fsi` | *retired (T1)* | contract names `System.Collections.IEnumerable`, `System.Collections.IStructuralEquatable`, `ReadOnlySpan<'T>`, `[<CollectionBuilder>]`, `[<ScopedRef>]`. 954 lines of contract / 1145 of impl. See §6 |

**C6, C7, C8 were not port work — they were scope work, and they are done.** Each `.fsi`
moved out of `[core] files` into `[targets.clr] files`, which the manifest schema already
supported (`Vesper.Core` uses `[targets.js] files` today) and which the conformance pass
already honoured (`ReferencedProject.resolveFiles`). A contract not in the JS file set is
not checked on JS. That is *three errors deleted by a correct statement of fact*, not by an
exemption — and it does not violate the list-free property, which `ConformanceTests.fs`
pins only against `[targets.<t>] sig-only`. `structural-printer.fsi` moved with them, for
the same reason (A5–A6). Each move carries a comment saying what a JS port would have to
decide first.

---

## 2. Blocked vs writable — the summary

| | count | what it is |
|---|---|---|
| ~~**Writable now** (Kind B)~~ | ~~1~~ ✅ | B1–B10 all done. What remains for `Vesper.Seq` is a BUILD capability (multi-file JS modules), not library source |
| ~~Machinery, no library source~~ (Kind A) | ~~6~~ ✅ | done — `ConformancePass` + the manifest schema's `impl-only` key |
| ~~Manifest scope statement~~ (C6, C7, C8) | ~~3~~ ✅ | done — 4 `.fsi` moved to `[targets.clr] files` |
| **Probably writable, unverified** (C1, C4, C5) | 3 | likely just `[targets.js] impl` entries + the 4 `Fun` interfaces. Must be compiled to know |
| **Genuinely blocked on a codegen feature** | 2 | **C3** — JS class inheritance (`EmitJsTypes.fs:409`). **C2** — heritable `obj` on JS |

So: **exactly one missing compiler feature is load-bearing — class inheritance
(`extends` / `super`) on the JS target** — and it blocks exactly one contract
(`compiler-attributes.fsi`), whose runtime value on JS is close to zero (these are
compile-time marker attributes the front end reads from the `.fsi`; their `.fs` bodies
exist to emit CLR TypeDefs). This is not a compiler project. It is a library-writing job
with one architectural question attached.

---

## 3. `list` / `seq` / the `.clr.fs` renames — corrections owed

`seq` is a **capability**, not a BCL binding: an interface on CLR, `Symbol.iterator` on JS.
I verified the JS lowering is real and complete, not aspirational:

- `EmitJsCapabilities.fs:49-210` — the whole protocol table. Iteration → `Symbol.iterator`,
  cursor → attached `MoveNext`/`Current`, disposal → `Symbol.dispose`,
  equality/comparison → `Symbol.for("vesper.…")`.
- Implementer side: `interface seq<'T>` on a class / record / **union** emits
  `*[Symbol.iterator]()` (`EmitJsMembers.fs:66`, base-class placement at `EmitJsTypes.fs:182`).
- Consumer side: `src.GetEnumerator()` → `enumeratorOf(src)` from `Vesper.Core.mjs:189`;
  the manual `MoveNext`/`Current` pull protocol; `use e = …` → `[Symbol.dispose]()`.
- Node end-to-end, not just emit assertions: `ForInTests.fs:154/178/196/210` (class, union,
  record, and a **bare cons-list** `for x in [1;2;3]` driving the real `Vesper.List`),
  `ManualEnumerationTests.fs:90/106`, `IterableForInTests.fs:149/168` (native `Js.Set` /
  `Js.Map`).
- `capabilities.fsi` needs **no** `capabilities.js.fs`: `RuntimeNames.CapabilityIds` is
  resolved from the *provider* (`RuntimeNames.fs:320-341`), i.e. from the contract, and the
  JS anchors are symbols living in the backend. Its `Unrepresentable` verdict is exactly
  right.

### Consequence 1 — `list.clr.fs` went back to `list.fs` ✅ (T4)

`7e994a93` renamed it on the grounds that *"`interface seq<'T>` and `toSeq`/`ofSeq` bind it
to the BCL `seq` contract, which is exactly what `list.js.fs` exists to strip"*. Both halves
were false:

- `list.clr.fs` contained **no `System.` text and no inline IL** — it passed the commit's
  own neutrality test.
- `list.js.fs` did **not** strip the `seq` leg. It had `interface seq<'T>` with a full
  `ListEnumerator`. The two files' seq legs were the same leg.

`list.js.fs` is deleted and `src/Vesper.List/manifest.toml` carries one shared
`[core] impl = ["list.fs"]`. The four deltas were all non-issues:

| delta | status |
|---|---|
| `member this.IsEmpty` / `.Head` / `.Tail` on `List<'T>` (CLR only) | **not a gap.** A union's augmentation members emit as *lifted receiver-first free functions*, so the merged body exports `List__get_Length` / `_IsEmpty` / `_Head` / `_Tail` — node-tested on the generated asset (`ListTests.fs`) |
| `[<NoEquality; NoComparison; Struct>]` on `ListEnumerator` (CLR only) | `[<Struct>]` is **erased** on JS (`EmitJsTypes.fs:319-321`; `ClassValueKind` is read nowhere in the JS backend), so the attribute is harmless. `[<Struct>]` *records* are conformance-tested on both targets; a `[<Struct>]` `val`-form class has no test but reduces to the same reference object |
| explicit `interface Vesper.disposable with member Dispose() = ()` (CLR only) | supported on JS (`[Symbol.dispose]`, `ManualEnumerationTests.fs:100/114`) |
| `toSeq` / `ofSeq` (CLR only) | B7/B8 above; both constructs node-tested on JS |

**Two live emitter bugs found while auditing — both fixed with the merge:**

- **The explicit `new(…) = { … }` body was discarded** — the emitter always wrote a
  *positional* ctor over the declared fields, so `new(s) = { cursor = s; started = false }`
  emitted `constructor(cursor, started)` against the call site `new ListEnumerator(_s0)` and
  `started` arrived `undefined`, working only by falsiness. **Fixed in the emitter, not in
  the source:** a `val`-form class's single `new` is lowered as written (`JsCtor`,
  `PendingCtor.Explicit`), so its parameter list and its stores are the ones the source
  names. More than one constructor arity on a class is now a loud failure — a JS class has
  exactly one constructor, so the second was already unreachable.
- **`list.fsi` declares `member Length / IsEmpty / Head / Tail / Item` on `List<'T>` and no
  `.fs` implemented them.** Conformance does not catch it (the pass checks type and `val`
  presence, not members). The merged `list.fs` implements `Length` / `IsEmpty` / `Head` /
  `Tail`, and they now reach `Vesper.List.mjs` as lifted free functions. **`Item` is still
  absent, and is not library work:** it is an INDEXED property, which nothing in the front
  end can define — `get_Item` is resolved only on EXTERNAL receivers
  (`InferRecordAccess.fs:573,680`), there is no `IsIndexed` on the member node, and no
  Vesper source anywhere declares one. `Empty` / `Cons` / `GetSlice` / `GetReverseIndex` are
  likewise contract-only on both targets.

**Member-level conformance — assessed, NOT taken.** It is not a few lines. The pass reads
the raw CST and extracts only module-level types + `val`s (`Conformance.fs:398-400` says so);
a member rung needs a `MemberDecl` extractor over the member lists of all twelve
`TypeSignature` shapes and all twelve `TypeDefn` shapes, a per-TYPE pairing (member names
only collide within their type), a new `ConformanceError` case, and a decision about the
kinds no `.fs` can satisfy — `static member Empty`, indexed properties, `[<Experimental>]`
slots. It would light up on both targets at once, `set.fsi` alone contributing hundreds. It
is its own tranche.

### Consequence 2 — `seq.clr.fs` and `struct-seq.clr.fs`: the renames were *right*, for a different reason

- `seq.clr.fs` **is** BCL-bound, but only in 2 of its 4 functions: `truncate` calls
  `System.Linq.Enumerable.Take`, `toArray` uses `ResizeArray`. `fold` and `reduce` are
  neutral. So `.clr.fs` is a fair name, and a `seq.js.fs` is writable (B10). Long term
  `fold`/`reduce` could live in a shared `seq.fs` with only `truncate`/`toArray` split —
  but that is a refinement, not a blocker.
- `struct-seq.clr.fs` **is** genuinely CLR: its *contract* names the non-generic
  `System.Collections.IEnumerable` / `IEnumerator` (which `capabilities-compat.js.fsi` does
  **not** map — it maps only the generic ones), and its raison d'être is `[<Struct>]`
  by-value chaining. C7 above: move the `.fsi` to `[targets.clr] files`.
- `array.clr.fs` — **misnamed**, on the same false premise as `list.clr.fs`. See
  consequence 3.
- `comparison.clr.fs` (32 inline-IL sites), `capabilities.clr.fs` (5 BCL reprs),
  `int-comparison.clr.fs`, the `prim-types-*.clr.fs` — all correctly named.

### Consequence 3 — `array.clr.fs` split into a prelude + a neutral `array.fs` ✅ (T5)

Inline IL is not by itself a CLR binding — but the reason is **not** that CIL mnemonics are
neutral. It is that four of them are this codebase's **named array IR**, and the JS backend
hardcodes an interpretation of each: `newarr` → `Array(n).fill(null)` (dense, so no slot is
a hole), `ldelem` → `a[i]`, `stelem` → `a[i] = v`, `ldlen` → `a.length` (`EmitJs.fs:560-594`).
CLR's names were adopted for that IR, which is a CLR-flavoured choice; portability here is
manufactured by the backend, not inherent to the spelling.

So a single body does compile on both targets, and B9's "write `array.js.fs`, 16 functions"
was work that did not exist — a duplicate would have carried the identical `newarr` line.
But "it is target-neutral" was the wrong reason to reach for, and a straight merge left a
wart. A census of `(# "` across `src/Vesper.*` puts inline IL in 27 files, of which 26 are
`.clr.fs` or `.js.fs`: **inline IL lives in per-target files** is an invariant the tree
otherwise holds perfectly, and a merged `array.fs` would have been the sole exception.

**The layout that keeps both properties.** `array-prelude.fsi` declares the primitives whose
bodies can only be an intrinsic — today just `val inline NewArray: count: int -> 'T[]` — and
`array-prelude.clr.fs` / `array-prelude.js.fs` carry them per target. `array.fs` is the
`Array` module proper, all sixteen functions, with **no inline IL at all**: the fifteen loops
reach the array ops through `ops-platform`'s per-target bodies exactly as `list.fs` reaches
`failwith`, and `zeroCreate` is now `NewArray count`.

Two things this deliberately is not. It does **not** widen `Vesper.Core`: `GetArray` /
`SetArray` / `GetArrayLength` live in `ops-platform` because the front end desugars `arr.[i]`
and `arr.Length` to them through `CoreAccess`, and nothing resolves an allocation primitive by
name — so it belongs to the package that owns arrays. And it does **not** remove the JS
backend's IL knowledge; the JS prelude body is the same `newarr`, as the two `ops-platform`
bodies are already identical for the other three. Identical is not shared: separate files are
what let them stop being identical.

The manifest mirrors `Vesper.Core` throughout — a neutral `.fsi` over two per-target bodies,
and `array.fs` named in *both* target `impl` lists rather than in `[core]`, for the same
declaration-order reason Core repeats `ops-std.fs` (inherited lists append after shared ones,
which would put `array.fs` ahead of the prelude it splices).

**The split is provably inert.** Regenerating `Vesper.Array.mjs` across it changes only the
generated temp-name counters (`_lim317` → `_lim320`, +3 from the extra splice); every emitted
statement is byte-identical, `export const zeroCreate = (count) => Array(count).fill(null);`
included. On CLR the reflection-invoke suite over the built `Vesper.Array.dll` is unchanged.

**One seam it moved.** A package that splices its own prelude needs its own manifest in the
contract that compiles it, so `ArrayTests` takes `arrayDepsJsContract` rather than
`coreDepsJsContract`. Safe here, and for the reason the exclusion exists: it guards a
collision over in-file *types*, and `Vesper.Array` declares none. A package that does declare
types (`Vesper.List`) still takes the deps-only contract.

**T6 inherits this shape.** `seq.clr.fs` is BCL-bound in 2 of 4 functions, which §3 above
already flagged as a merge candidate. The prelude split is the answer: `fold`/`reduce` into a
neutral `seq.fs`, `truncate`/`toArray` into per-target preludes.

`ArrayTests.fs` pins it the way `ListTests.fs` pins the list: the module is compiled in
library mode, byte-compared against a committed `Vesper.Array.mjs`, and all sixteen
functions are executed under Node. `Vesper.Array` also joins `TestHelpers.jsManifests`, so
a use site resolves `Array.map` and imports it from the asset.

---

## 4. The six committed `.mjs` runtime assets

Verified against the tests, not the manifest comments.

| asset | actually | generated from | drift detected? | load-bearing for the port? |
|---|---|---|---|---|
| `Vesper.List.mjs` | **generated** (claim true) | `list.fs` | **yes** — full byte compare every run, `ListTests.fs` | yes, but it regenerates itself |
| `Vesper.Array.mjs` | **generated** (new, T5) | `array.fs` | **yes** — full byte compare every run, `ArrayTests.fs` | yes, self-maintaining |
| `Vesper.Option.mjs` | **generated** (claim true) | `option.fs` | **yes** — `OptionTests.fs:117-127` | yes, self-maintaining |
| `Vesper.Printf.mjs` | **generated** (claim true) | `structural-printer.js.fs` | **yes** — `StructuralPrinterTests.fs:39-49` | yes, self-maintaining |
| `Vesper.Core.mjs` | **hand-authored** | — | **no** | **critically** — 4 exports: `checkedDivisor` (every integral `/` and `%`), `structuralEquals` / `structuralHash` (every `=` / `<>` / `hash`), `enumeratorOf` (every `for … in`) |
| `Vesper.Comparison.mjs` | **hand-authored** | — | **no** | yes — `structuralCompare`, behind every aggregate `< > <= >=` |

None of the six is referenced from `package.json` or any npm script; they are executed only
by the dotnet suite via `Codegen.materialise` + `node`.

**The gap worth naming — ✅ closed (T2).** Nothing used to check that `Vesper.Core.mjs` /
`Vesper.Comparison.mjs` actually export the symbols their `.fsi` declares; a renamed export
surfaced as an ESM link error under Node, *and only if node was on PATH*, since every such
test `skiptest`s otherwise. That check is now the `RuntimeServed` verdict itself: the
conformance pass reads the asset's ESM exports and accepts the contract's absent `.fs` only
if every declared `val` is among them. Node-free, and it runs on every conformance run.

**Should they become generated?** `Vesper.Comparison.mjs` — yes, and it is nearly free: it
is one function of the same shape as `Vesper.Printf.mjs`'s walker. `Vesper.Core.mjs` — the
manifest's own note is right that this is the `--compiling-fslib` bootstrap ("Route B"), and
it is *not* on the critical path for this port: `structuralEquals`/`structuralHash`/
`enumeratorOf` are self-referential (the equality runtime cannot be written in a language
whose `=` it implements) and want a real bootstrap story. **Recommendation: keep both
hand-authored for this port.** The rename-rot risk is what the export-presence check above
now covers.

Legacy / not load-bearing: none of the six is dead. `Vesper.Printf.mjs` exports ~60
symbols of which the backend imports 2 (`structuralFormat`, `float32ToString`) — the rest
are incidental library-mode exports, harmless.

---

## 5. Dependency order

From `depends-on` plus what the front end needs to resolve anything at all:

```
Vesper.Core  ──┬─► Vesper.Array ────┐
               ├─► Vesper.List ─────┼─► Vesper.Set   (CLR-only, see §6)
               ├─► Vesper.Comparison┤
               ├─► Vesper.Choice ───┤
               ├─► Vesper.Option ───┤
               ├─► Vesper.Result    │
               └─► Vesper.Exceptions│
                                    │
Core + List + Comparison ─► Vesper.Seq ─┘
Core + List ─────────────► Vesper.Printf
```

Within Vesper.Core the `[core] files` order is itself a dependency chain and the manifest
already documents it: `prim-types-min` (defines `int`/`bool`/`` `[]` ``/`Fun`/`unit`) →
`prim-types-*` → `compiler-attributes` (needs `Attribute` from `prim-types-attr`) →
`core-types` (needs `Fun` **and** `[<ReferenceEquality>]`) → `capabilities` →
`structural-format` → `ops-platform` → `ops-std` → `int-comparison`.

So inside Core: **C1 (`Fun`) gates C4 (`core-types`)**, and B1 (`` `[]` ``) gates anything
that mentions an array. Everything else in Core is independent.

Nothing outside Core is blocked by anything except Core. `Vesper.Seq` nominally needs
`Vesper.List`, but `seq.js.fs`'s four functions don't touch the cons-list.

---

## 6. The open decision: where does the port stop?

**Recommendation: stop before `Vesper.Set`, and say so in the manifest rather than leaving
it as a red error.**

Reasoning:

- `set.fsi` is 954 lines of contract over 1145 lines of AVL tree ported verbatim from
  FSharp.Core. It is the single largest artifact in the tree by a wide margin.
- Its *contract* — not merely its impl — names `System.Collections.IEnumerable`,
  `System.Collections.IStructuralEquatable`, `ReadOnlySpan<'T>`,
  `[<CollectionBuilder(typeof<Set>, "Create")>]` and `[<ScopedRef>]`. Porting it means first
  deciding what all of those mean on JS, which is a much bigger question than "does the AVL
  tree compile".
- It sits at the bottom of the dependency graph (Core + List + Array + Seq + Choice +
  Option + Comparison + Printf), so it is the *last* thing that could land anyway. Nothing
  else in the tree is waiting on it.
- Deferring it costs exactly **1 of the 24 errors**.

Concretely: move `set.fsi` from `[core] files` to `[targets.clr] files` in
`src/Vesper.Set/manifest.toml`, with a comment saying what a JS port would first have to
decide. Same treatment for `struct-seq.fsi` (C7) and `formatter.fsi` (C6), for the same
reason and with better justification — those two are CLR constructs by *design*
(by-value struct chaining; a `TextWriter`/`StringBuilder`-backed interpolation handler),
not merely unported.

That leaves a first-JS-port target of: **Core, Array, List, Seq, Comparison, Choice,
Option, Result, Exceptions** — i.e. everything except Set, `struct-seq`, and the printf
`Formatter`. That is a coherent "small core": primitives, operators, arrays, cons-lists,
sequences, and the three data unions.

**This is your call.** The alternative — including Set — roughly doubles the port and pulls
in the BCL-interface question early, and I see no forcing reason for it.

---

## 7. Tranches

Each leaves the tree green and moves the JS error count down monotonically.

| # | tranche | closes | errors | rough size |
|---|---|---|---|---|
| **T1** ✅ | **Statement of scope.** `formatter.fsi`, `struct-seq.fsi`, `set.fsi` (and `structural-printer.fsi`, for A5–A6) moved to `[targets.clr] files`, each with a comment saying what a JS port would have to decide first | C6, C7, C8 | −3 (→21) | done |
| **T2** ✅ | **Conformance machinery.** One rule replaces three guards: *a contract owes a `.fs` unless every declaration in it is satisfied without one* — `extern`/abbreviation always, a `val` exactly when the `runtime` asset exports it (`RuntimeServed`, which IS §4's export-presence check). `array-index-body.js.fs` renamed to pair with its contract; a new `impl-only` manifest key withholds a contract-less body from pairing | A1–A6 | −6 (→15) | done |
| **T3a** ✅ | **Vesper.Core, the writable half.** `` `[]` `` repr in `prim-types-min.js.fs`; `ignore`/`isNull`/`box`/`invalidArg` in `ops-platform.js.fs`; new `int-comparison.js.fs`. Each body node-tested (`CoreOpBodiesJsTests.fs`), not merely conformed | B1–B6 | −6 (→9) | done |
| **T4** ✅ | **`list.fs` merge.** Folded `list.js.fs` back into the neutral `list.fs` (`Length`/`IsEmpty`/`Head`/`Tail`, `toSeq`, `ofSeq`, the explicit `disposable` impl, and `[<Struct>]`, which holds — it is erased). `list.js.fs` deleted; one shared `[core] impl`. `Vesper.List.mjs` regenerated. Plus the two emitter bugs above | B7, B8, + the `7e994a93` revert | −2 (→7) | done |
| **T3b** | **The compile-and-see remainder.** The 4 `Fun` interfaces in `prim-types-min.js.fs`; add `core-types.fs` and `structural-format.fs` to `[targets.js] impl` and see whether they compile | C1, C4, C5 | −3 (→3) | half a day if C4/C5 just work; a day if `Curried`/`Flattened` surface a codegen gap |
| **T5** ✅ | **`Vesper.Array`, split not ported.** The CLR body compiled as written on JS, `newarr` included, so the work was a layout call: per-target `array-prelude.{clr,js}.fs` for the intrinsics, a neutral `array.fs` for the module. Plus a generated + byte-compared `Vesper.Array.mjs` and all 16 functions node-tested (`ArrayTests.fs`) | B9 | −1 (→6) | done |
| **T6** ✅ | **Vesper.Seq, fully merged.** All four functions in one neutral `seq.fs`: `toArray` on a doubling buffer (no `ResizeArray`), `truncate` a hand-rolled lazy cursor (no `Enumerable.Take`, no `seq { }`). Needed a compiler fix — the CLR repr hop — which also deleted the prelude the split had called for. Generated + byte-compared `Vesper.Seq.mjs`, node-tested including laziness over an infinite generator | B10 | −1 (→5) | done |
| **T7** | **The two remaining architectural items.** `obj`'s heritability on JS (C2); `compiler-attributes.fsi` (C3) — which is either "implement `extends`/`super` on JS" or "these are CLR-only marker types". I recommend the latter and deferring the former | C2, C3 | −2 (→0) | unbounded; scope it separately once T1–T6 are in |

After T6 the conformance run is at **5**, and after T3b it would be **2 — both named
architectural questions** rather than missing work. That is a good place to stop and
re-decide.

**But conformance is no longer the whole count.** T6 surfaced two things it does not
measure, because the pass checks contract/impl pairing and not whether a body compiles:

1. ~~**CLR member resolution does not make the repr hop** for a capability-typed
   receiver.~~ ✅ **Fixed in T6** — `EngineCore.capabilityPlatformKey`. This is what would
   have split `truncate`; instead `seq.fs` is one shared body and no prelude exists.
2. **A JS runtime module is compiled from ONE source file** (`JsProjectInfo.Source` is a
   single `JsSource`). This did not bite in the end — fixing (1) left `Vesper.Seq` a
   single-file package — but the limitation is real and unaddressed. `Vesper.Array` avoids
   it only because its prelude is `inline` and splices. **`Vesper.Core` cannot have a JS
   asset until this lands**, so it is still on the critical path for the rest of the port.

The general lesson worth carrying: a per-target split is a claim that the TARGETS differ.
Twice now — `array.fs`, then `seq.fs` — the real cause was something else (a shared IR
spelled in CIL; a front-end gap), and the honest fix was upstream rather than a second copy
of the file.

T1 + T2 landed together, then T3a, then T4, then T5 (taken out of order — once it turned out
to be a rename rather than a port, there was nothing to sequence it behind). **T3b is the next
move**, and every tranche after it must shrink the pinned error list in `ConformanceTests.fs`
— that list is the count.

### T6's decision — SETTLED (option 2), and `seq.fs` is fully shared

Option 2 was taken. The forecast was that a hand-rolled `truncate` would keep `seq.fs`
target-split, because CLR would keep `Enumerable.Take`. It does not: **all four functions
are one shared body.**

Two things had to be true for that. `toArray` stopped needing `ResizeArray` — a doubling
buffer over `Array.zeroCreate` is neutral and is what `ResizeArray.ToArray` does underneath.
And `truncate`'s cursor briefly did force a split, for a reason that turned out to be a
**front-end gap rather than a runtime difference**: driving a cursor reads `MoveNext` /
`Current` off a value typed as the `enumerator<'T>` CAPABILITY, whose canonical shape is an
`IntrinsicInterface` — it names its platform type but carries no member table — so CLR
member lookup found nothing and reported `Unknown class type
'Vesper.Collections.enumerator`1'`. The identical source compiled and ran on JS, where a
capability is a plain `Class` carrying its own members.

**That gap is now fixed** (see below), so the cursor compiles on both targets and
`seq-prelude` was deleted before it was ever committed. `Enumerable.Take` and `ResizeArray`
were BCL convenience, not necessity.

### The CLR repr hop — fixed

`EngineCore.capabilityPlatformKey`, the mirror of the existing `capabilityCanonKey`: it
folds a capability's two nominal keys to its PLATFORM key, and returns a non-capability key
unchanged. Member lookup retries under it at both `DotSource.ExternalClass` sites —
`InferRecordAccess.fs` (direct) and `Engine.fs` (deferred).

The direction is the whole safety argument. `EngineCore` already warns that rewriting keys
inside a lookup path breaks base/interface chains — but that warning is about rewriting
*canon-ward*, which erases the platform type's own bases. Rewriting *platform-ward* restores
them, which is exactly what lets `enumerator<'T>.Dispose` reach `IDisposable` through the
BCL interface chain.

Confirmed by negative control: with the retry disabled the original error returns and 39 CLR
tests fail; with it enabled, 1451 pass. `CapabilityMemberAccessTests.fs` pins it
systematically — method, property (`Current` → `get_Current`, the case that could have made
this expensive and did not), inherited member through a second capability, a
capability-typed result, the deferred field path, and a non-capability negative control.

### The original framing, for reference

**`seq { }` / `yield` does not exist** — not in the backend, not even in the front end.
The parser has the syntax (`Expr.fs:252-253,269`) but elaboration has no arm for it, so it
falls to `ElaborateExpr.fs:375-378`'s `failwithf "…TODO %A"`, and there is no
`SeqExpr`/`Yield` node in `ExprShape` at all. The only `yield` the backend emits is the
internal `[Symbol.iterator]` adapter. So a lazy `truncate` cannot be written the obvious way.

Three options, none of them free:

1. **Eager** — materialise into an array and return it as a `seq<'T>`. Wrong semantics for
   an infinite or side-effecting source, but zero new machinery.
2. **Hand-rolled lazy** — a `TruncateSeq<'T>` class implementing `interface seq<'T>` with a
   counting `TruncateEnumerator` cursor, exactly as `list.js.fs` already does for the
   cons-list. Correct, ~30 extra lines, and uses only constructs proven end-to-end on JS.
3. **Implement sequence expressions.** A real front-end feature (there is already
   `docs/sequence-expressions-plan.md`), out of scope here.

Recommend (2) for the port and leave (3) to its own plan. Note that on CLR `truncate` is
`Enumerable.Take` — so choosing (2) means `seq.fs` genuinely stays target-split, which is
the honest answer rather than a merge candidate like `list.fs`.

---

## 8. What the JS backend actually refuses — reference

Every one of these is a `failwith` in the emitter, **not** a graceful diagnostic. Worth
having beside you while writing any of the tranches: a library body that trips one of them
takes the whole compile down with a raw exception.

| construct | site |
|---|---|
| `try … with` (the catch-side arm is unwritten; `try … finally` **is** supported) | `EmitJs.fs:624` |
| `null` literal in expression position | `EmitJs.fs:623` |
| `x :? T` type test, `:? T as y` pattern | `EmitJs.fs:625`, `EmitJsContext.fs:581` |
| range in value position (`let r = 1..10`) | `EmitJs.fs:634` |
| class `inherit` | `EmitJsTypes.fs:409-412` |
| secondary ctor alongside a primary one | `EmitJsTypes.fs:414-417` |
| duck-typed (`Pattern`) `for … in` enumerator | `EmitJs.fs:900-902` |
| refutable / non-tuple `for … in` binder | `EmitJs.fs:898-899` |
| `nativeint` / `decimal` literal | `JsEmitHelpers.fs:142`, `:155` |
| an `override` whose name clashes with an interface impl | `EmitJsTypes.fs:256-258` |
| a format sink other than `sprintf`/`printfn`/`eprintfn` | `EmitJs.fs:615` |

Adjacent facts that shape the port:

- **Sequence expressions do not exist at all** (front end included) — see T6's decision box.
- **Every exception type erases to a JS `Error`** — `ExceptionTests.fs:103` asserts the
  emitted source does *not* contain `ArgumentException`. So `invalidArg` (B5) cannot preserve
  its type, only its message. Custom exception types are unimplemented (no elaboration arm
  for `ExceptionDefn`, and catching by type would need the refused `TypeTest`).
- **Only the nativeint family has `UnsupportedOnTarget` test coverage**
  (`UnsupportedOnTargetTests.fs:51-62`: `nativeint`, `unativeint`, `nativeptr<'T>`,
  `voidptr`, `ilsigptr<'T>`). `Attribute`, `decimal`, and the nd-arrays are unsupported on JS
  by the same derivation but nothing asserts it — so C3's *first* wall (`Attribute` refused
  before the emitter is even reached) is inferred from `VesperLib.fs:1407-1411` +
  `PlatformTypes.fs:144`, not observed.
- **`PlatformTypes` deliberately does not walk class member bodies**
  (`PlatformTypes.fs:166-174`): *"a backend may not emit them yet (the JS back end does
  not), so flagging a type they reference would be a premature reject."* Record/union
  augmentation bodies and all interface-impl bodies **are** walked. So a class member
  mentioning an unrepresentable type is silently unchecked.
- **Library mode is proven end-to-end for four packages**: List, Option, Printf and — since
  T5 — Array (`ListTests.fs`; `OptionTests.fs`; `StructuralPrinterTests.fs`; `ArrayTests.fs`,
  all via `TestHelpers.compileLibrary`). Extending it to Seq / Core is the mechanism every
  tranche after T2 rides on; it is real, not aspirational.

---

## 9. Corrections to the inventory I was handed

- ✅ `prim-types-min.js.fs` binds no `` `[]` `` or `Fun` — confirmed; `` `[]` `` now bound (B1), `Fun` still open (C1).
- ✅ `obj`'s JS repr is untagged where the contract says `extern class` — confirmed.
- ✅ `ops-platform.js.fs` lacks `ignore`/`isNull`/`box`/`invalidArg` — confirmed, exactly those four; all four now written (B2–B5).
- ✅ `compiler-attributes.fsi`, `core-types.fsi`, `structural-format.fsi`, `int-comparison.fsi` have no JS body — confirmed; `int-comparison` now has one (B6).
- ❌ `capabilities-compat.js.fsi` and `ops-platform-runtime.js.fsi` "are JS-only contracts served by the `.mjs` runtime" — the *characterisation* was right, but they were **not accepted**: both were live V240 hard errors. And they are not the same case as each other — one is pure abbreviations, the other is genuinely runtime-served. Both now have their own verdict (A1, A2).
- ❌ `array-index-body.js.fs` "deliberately has no `.fsi`" — it deliberately had a *differently named* `.fsi` (`array-index.js.fsi`), for a reason that did not hold. Renamed; they pair (A4).
- ❌ `comparison-runtime.js.fsi` was listed alongside `array.fsi`/`seq.fsi`/`set.fsi` as "no JS implementation". It is not missing work — it is the `Vesper.Comparison.mjs`-served twin of `ops-platform-runtime.js.fsi` (A3).
- ❌ "`structural-printer.fsi`'s `RuntimeFormatState`/`StructuralPrinter` are absent from `structural-printer.js.fs`" — true as stated but misleading. `structural-printer.js.fs` is a complete, generated, byte-checked, node-tested JS `%A` engine that implements a *different* surface (`structuralFormat`). It was not an incomplete port of that contract; the two were mispaired by the stem rule (A5–A6).
- ❌ `formatter.fsi` was omitted from the Printf line — it was a third live error there (C6).
- ❌ (already known) `ops-platform.fsi` mentions `nativeint`: it does not. Zero occurrences.
- ❌ `array.clr.fs` "correctly named" (§3, my own line) — it was not, though not because the
  file is target-free. `newarr` / `ldelem` / `stelem` / `ldlen` are a named array IR both
  backends implement, so one body compiles on both; T5's "write `array.js.fs`, 16 functions"
  was work that did not exist. The one genuinely CLR-only thing in it was the `newarr`
  spelling, which is now in a per-target prelude — see consequence 3.
- ➕ Not in the handed-over inventory at all: `list.js.fs` **already implemented
  `interface seq<'T>`**, which falsified the stated reason both for its existence and for
  the `list.fs` → `list.clr.fs` rename. Both are undone (§3).

---

## 10. Reproducing the numbers

`ConformanceTests.fs`, test *"js: the hard-error set is exactly the un-ported library
surface"* — it walks `Directory.GetDirectories(src, "Vesper.*")`, runs
`ConformancePass.checkManifest "js"` + `enforce` on each, and asserts the messages equal a
declared list. Run `XParsec.FSharp.SemanticAnalysis.Tests`; a failure prints both sides.

The list shrinks tranche by tranche, and it is what keeps the count honest: an entry that
vanishes without the corresponding source appearing means the pass stopped asking.
