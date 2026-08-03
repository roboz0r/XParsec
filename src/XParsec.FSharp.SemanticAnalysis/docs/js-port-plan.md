# Porting the Vesper library to the JS target — scope

Working document. Ephemeral: delete it when the work lands.

Written against `semantic-analysis` @ `82fe318e`. Every number below came from actually
running `ConformancePass.checkManifest "js"` + `ConformancePass.enforce` over all eleven
`src/Vesper.*` manifests. Where I could not establish something I say so rather than
estimating.

**T1 and T2 have landed.** The run is at **15** errors, down from 24. The error set is now
pinned by a committed test rather than a throwaway one — `ConformanceTests.fs`'s
*"js: the hard-error set is exactly the un-ported library surface"* — so every tranche
below has to shrink that list to be believed.

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

**15 hard conformance errors on `js`, across 4 of the 11 packages. 0 on `clr`.**
(Was 24 across 7 before T1 + T2.)

| package | js errors | was | clr errors |
|---|---|---|---|
| Vesper.Core | 11 | 14 | 0 |
| Vesper.List | 2 | 2 | 0 |
| Vesper.Array | 1 | 1 | 0 |
| Vesper.Seq | 1 | 2 | 0 |
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
`compiler-attributes.fsi` remain V240, and `int-comparison.fsi` — all `val`s, in a package
that *does* ship a runtime asset — stays V240 purely because `Vesper.Core.mjs` exports no
`<`/`>`/`<=`/`>=`. That is the negative control for A2/A3 and is pinned as one.

### Kind B — genuinely missing library source, writable today (10 errors)

No compiler feature needed. This is F#-writing work.

| # | file to write / extend | missing declarations | size |
|---|---|---|---|
| B1 | `Vesper.Core/prim-types-min.js.fs` | `type 'T ``[]`` = (# … #)` + `type 'T array = 'T[]` (CLR does it at `prim-types-min.clr.fs:26-27`) | ~2 lines, but see caveat below |
| B2–B5 | `Vesper.Core/ops-platform.js.fs` | `ignore`, `isNull`, `box`, `invalidArg` (CLR bodies at `ops-platform.clr.fs:81,84,88,151`). JS: `()`, `(# "$0 === null" #)`, identity, a `throw new Error` template like the existing `failwith` at `ops-platform.js.fs:125` | ~10 lines |
| B6 | `Vesper.Core/int-comparison.js.fs` (new) | `IntComparison.(<) (>) (<=) (>=)` — `(# "$0 < $1" x y : bool #)` etc. Direct analogue of `comparison.js.fs` | ~11 lines |
| B7–B8 | `Vesper.List/list.js.fs` | `ofSeq`, `toSeq`. Both already work as written in `list.clr.fs:103-111` — `toSeq` is a `:> seq<'T>` upcast (node-tested on JS, `ForInTests.fs:159/183`), `ofSeq` is `for x in source` over a `seq<'T>` (`ForInTests.fs:130`) | ~8 lines — and see §3, this is subsumed by the `list.fs` merge |
| B9 | `Vesper.Array/array.js.fs` (new) | the whole `Array` module: `zeroCreate length isEmpty get set create init copy append rev map mapi iter iteri fold foldBack` (16 `val`s, `array.fsi`). Only `zeroCreate` is intrinsic on CLR (`newarr`); on JS it is `new Array($0).fill(…)`. The rest are ordinary loops | ~100 lines |
| B10 | `Vesper.Seq/seq.js.fs` (new) | `Seq.fold reduce truncate toArray` (4 `val`s). `fold`/`reduce` in `seq.clr.fs` are already target-neutral (`for … in`, `Unchecked.defaultof`, `invalidArg`). Only `truncate` (`Enumerable.Take`) and `toArray` (`ResizeArray`) are BCL. **But `truncate` returns a `seq<'T>` and `seq { }` does not exist** — see the sizing note in T6 | ~40 lines + a design call |

**B1 caveat.** `PlatformTypes.fs:26` states the rule: *"a target that CAN represent a
structural constructor (`'T []`) says so by binding its repr"*. Arrays plainly work on JS
today, so a repr is already bound — but under the **bare/global** key (`array-index.js.fsi`
+ `array-index-body.js.fs`, deliberately in `namespace global`), not under
`Vesper.``[]```. Binding `Vesper.``[]``` in `prim-types-min.js.fs` mirrors the CLR exactly
and should be inert, but I did not verify that it doesn't collide with the global entry in
the intrinsic-repr extraction. Treat B1 as "2 lines plus one experiment".

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
| **Writable now** (Kind B) | 10 | ~170 lines of F#. An afternoon or two |
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

### Consequence 1 — `list.clr.fs` should go back to `list.fs`

`7e994a93` renamed it on the grounds that *"`interface seq<'T>` and `toSeq`/`ofSeq` bind it
to the BCL `seq` contract, which is exactly what `list.js.fs` exists to strip"*. Both halves
are false:

- `list.clr.fs` contains **no `System.` text and no inline IL** (verified by grep) — it
  passes the commit's own neutrality test.
- `list.js.fs` does **not** strip the `seq` leg. It has `interface seq<'T>` with a full
  `ListEnumerator` (`list.js.fs:13-46`). The two files' seq legs are the same leg.

The actual deltas between `list.clr.fs` and `list.js.fs` are four, and three are already
non-issues:

| delta | status |
|---|---|
| `member this.IsEmpty` / `.Head` / `.Tail` on `List<'T>` (CLR only) | **not a gap.** A union's augmentation members emit as *lifted receiver-first free functions* (`EmitJsTypes.fs:372,384` → `EmitJsMembers.fs:138-167`), pinned for a union at `MemberEmissionTests.fs:47` (`const Lst__get_IsEmpty = (`, `const Lst__AddHead = (`) with Node E2E at `:73,89,104,114`. Real instance: `Vesper.Option.mjs:24,34,43` exports `Option__get_Value` / `Option__get_IsSome` / `Option__get_IsNone` from `option.fs`'s members. The comment at `list.js.fs:11-12` is **stale** — the accessor isn't a `.Head` property, but a use site still resolves |
| `[<NoEquality; NoComparison; Struct>]` on `ListEnumerator` (CLR only) | `[<Struct>]` is **erased** on JS (`EmitJsTypes.fs:319-321`; `ClassValueKind` is read nowhere in the JS backend), so the attribute is harmless. `[<Struct>]` *records* are conformance-tested on both targets; a `[<Struct>]` `val`-form class has no test but reduces to the same reference object |
| explicit `interface Vesper.disposable with member Dispose() = ()` (CLR only) | supported on JS (`[Symbol.dispose]`, `ManualEnumerationTests.fs:100/114`) |
| `toSeq` / `ofSeq` (CLR only) | B7/B8 above; both constructs node-tested on JS |

**Two things the merge must fix, found while auditing the emitter — both live today:**

- **The explicit `new(…) = { … }` body is discarded.** The emitter always writes a
  *positional* ctor over the declared fields in declaration order (`EmitJsTypes.fs:419-428`,
  comment at `:406-408`). `list.js.fs:22-26` declares two `val mutable` fields and
  `new(s) = { cursor = s; started = false }`; `Vesper.List.mjs:2-6` emits
  `constructor(cursor, started)` and the call site at `:51` is `new ListEnumerator(_s0)`, so
  **`started` is `undefined`**. It happens to work only because `undefined` is falsy. Any
  `val`-form type whose `new` arity ≠ field count miscompiles silently. Either the merged
  `list.fs` keeps the arities aligned, or the emitter learns the `new` body — pick
  deliberately, don't inherit the accident.
- **`list.fsi:28-40` declares `member Length / IsEmpty / Head / Tail / Item` on `List<'T>`
  and `list.js.fs` implements none of them.** Conformance doesn't catch it (the pass checks
  type and `val` presence, not members), and `Vesper.List.mjs` exports no such function, so a
  JS program writing `xs.Head` fails at codegen or emits an import of an export that does not
  exist. The manifest says so out loud: *"the cons-list type's cases **without member
  methods**"*. Restoring the members as part of the merge closes this too.

**Plan item:** merge `list.js.fs` back into a single neutral `list.fs`, delete
`list.js.fs`, and drop `[targets.clr] impl` / `[targets.js] impl` from
`src/Vesper.List/manifest.toml` in favour of a shared `impl = ["list.fs"]`. This closes
B7/B8 for free. Regenerate `Vesper.List.mjs` (see §4 — it is byte-checked, so the suite
will tell you).

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
- `array.clr.fs` — correctly named: `(# "newarr !0" … #)` at `array.clr.fs:7`.
- `comparison.clr.fs` (32 inline-IL sites), `capabilities.clr.fs` (5 BCL reprs),
  `int-comparison.clr.fs`, the `prim-types-*.clr.fs` — all correctly named.

---

## 4. The five committed `.mjs` runtime assets

Verified against the tests, not the manifest comments.

| asset | actually | generated from | drift detected? | load-bearing for the port? |
|---|---|---|---|---|
| `Vesper.List.mjs` | **generated** (claim true) | `list.js.fs` | **yes** — full byte compare every run, `ListTests.fs:176-186` | yes, but it regenerates itself |
| `Vesper.Option.mjs` | **generated** (claim true) | `option.fs` | **yes** — `OptionTests.fs:117-127` | yes, self-maintaining |
| `Vesper.Printf.mjs` | **generated** (claim true) | `structural-printer.js.fs` | **yes** — `StructuralPrinterTests.fs:39-49` | yes, self-maintaining |
| `Vesper.Core.mjs` | **hand-authored** | — | **no** | **critically** — 4 exports: `checkedDivisor` (every integral `/` and `%`), `structuralEquals` / `structuralHash` (every `=` / `<>` / `hash`), `enumeratorOf` (every `for … in`) |
| `Vesper.Comparison.mjs` | **hand-authored** | — | **no** | yes — `structuralCompare`, behind every aggregate `< > <= >=` |

None of the five is referenced from `package.json` or any npm script; they are executed only
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

Legacy / not load-bearing: none of the five is dead. `Vesper.Printf.mjs` exports ~60
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
| **T3** | **Vesper.Core, the writable half.** `` `[]` `` repr + the 4 `Fun` interfaces in `prim-types-min.js.fs`; `ignore`/`isNull`/`box`/`invalidArg` in `ops-platform.js.fs`; new `int-comparison.js.fs`; add `core-types.fs` and `structural-format.fs` to `[targets.js] impl` and see whether they compile | B1–B6, C1, C4, C5 | −8 (→7) | ~30 lines of new F#, plus the compile-and-see on the two neutral bodies. Half a day if C4/C5 just work; a day if `Curried`/`Flattened` surface a codegen gap |
| **T4** | **`list.fs` merge.** Fold `list.js.fs` back into a neutral `list.fs` (restoring `IsEmpty`/`Head`/`Tail`, `toSeq`, `ofSeq`, the explicit `disposable` impl, and — if `[<Struct>]` holds on a `val`-form class — the struct attribute). Delete `list.js.fs`; one shared `impl`. Regenerate `Vesper.List.mjs` (the byte-identity test will insist) | B7, B8, + the `7e994a93` revert | −2 (→5) | ~1 day; the risk is entirely in `[<Struct>]` on a `val`-form class |
| **T5** | **Vesper.Array.** New `array.js.fs`: 16 functions, only `zeroCreate` non-obvious | B9 | −1 (→4) | ~100 lines, half a day |
| **T6** | **Vesper.Seq.** New `seq.js.fs`: 4 functions. `fold`/`reduce`/`toArray` are straightforward. **`truncate` is the one real decision** — see below | B10 | −1 (→3) | ~40 lines + a design call; ~1 day |
| **T7** | **The two remaining architectural items.** `obj`'s heritability on JS (C2); `compiler-attributes.fsi` (C3) — which is either "implement `extends`/`super` on JS" or "these are CLR-only marker types". I recommend the latter and deferring the former | C2, C3 | −3 (→0) | unbounded; scope it separately once T1–T6 are in |

After T6 the JS run is at **3 errors, all of them named architectural questions** rather
than missing work. That is a good place to stop and re-decide.

T1 + T2 landed together. **T3 is the next move**, and every tranche after it must shrink
the pinned error list in `ConformanceTests.fs` — that list is now the count.

### T6's decision: `Seq.truncate` with no sequence expressions

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
- **Library mode is proven end-to-end for exactly three packages**: List, Option, Printf
  (`ListTests.fs:154,176,188`; `OptionTests.fs`; `StructuralPrinterTests.fs`, all via
  `TestHelpers.compileLibrary` at `TestHelpers.fs:213-231`). Extending it to Array / Seq /
  Core is the mechanism every tranche after T2 rides on; it is real, not aspirational.

---

## 9. Corrections to the inventory I was handed

- ✅ `prim-types-min.js.fs` binds no `` `[]` `` or `Fun` — confirmed.
- ✅ `obj`'s JS repr is untagged where the contract says `extern class` — confirmed.
- ✅ `ops-platform.js.fs` lacks `ignore`/`isNull`/`box`/`invalidArg` — confirmed, exactly those four.
- ✅ `compiler-attributes.fsi`, `core-types.fsi`, `structural-format.fsi`, `int-comparison.fsi` have no JS body — confirmed.
- ❌ `capabilities-compat.js.fsi` and `ops-platform-runtime.js.fsi` "are JS-only contracts served by the `.mjs` runtime" — the *characterisation* was right, but they were **not accepted**: both were live V240 hard errors. And they are not the same case as each other — one is pure abbreviations, the other is genuinely runtime-served. Both now have their own verdict (A1, A2).
- ❌ `array-index-body.js.fs` "deliberately has no `.fsi`" — it deliberately had a *differently named* `.fsi` (`array-index.js.fsi`), for a reason that did not hold. Renamed; they pair (A4).
- ❌ `comparison-runtime.js.fsi` was listed alongside `array.fsi`/`seq.fsi`/`set.fsi` as "no JS implementation". It is not missing work — it is the `Vesper.Comparison.mjs`-served twin of `ops-platform-runtime.js.fsi` (A3).
- ❌ "`structural-printer.fsi`'s `RuntimeFormatState`/`StructuralPrinter` are absent from `structural-printer.js.fs`" — true as stated but misleading. `structural-printer.js.fs` is a complete, generated, byte-checked, node-tested JS `%A` engine that implements a *different* surface (`structuralFormat`). It was not an incomplete port of that contract; the two were mispaired by the stem rule (A5–A6).
- ❌ `formatter.fsi` was omitted from the Printf line — it was a third live error there (C6).
- ❌ (already known) `ops-platform.fsi` mentions `nativeint`: it does not. Zero occurrences.
- ➕ Not in the handed-over inventory at all: `list.js.fs` **already implements
  `interface seq<'T>`**, which falsifies the stated reason both for its existence and for
  the `list.fs` → `list.clr.fs` rename (§3).

---

## 10. Reproducing the numbers

`ConformanceTests.fs`, test *"js: the hard-error set is exactly the un-ported library
surface"* — it walks `Directory.GetDirectories(src, "Vesper.*")`, runs
`ConformancePass.checkManifest "js"` + `enforce` on each, and asserts the messages equal a
declared list. Run `XParsec.FSharp.SemanticAnalysis.Tests`; a failure prints both sides.

The list shrinks tranche by tranche, and it is what keeps the count honest: an entry that
vanishes without the corresponding source appearing means the pass stopped asking.
