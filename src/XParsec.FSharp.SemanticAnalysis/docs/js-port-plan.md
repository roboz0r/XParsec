# Porting the Vesper library to the JS target — scope

Working document. Ephemeral: delete it when the work lands.

Written against `semantic-analysis` @ `82fe318e`. Every number below came from actually
running `ConformancePass.checkManifest "js"` + `ConformancePass.enforce` over all eleven
`src/Vesper.*` manifests (a throwaway test, since reverted). Where I could not establish
something I say so rather than estimating.

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

**24 hard conformance errors on `js`, across 7 of the 11 packages. 0 on `clr`.**

| package | js errors | clr errors |
|---|---|---|
| Vesper.Core | 14 | 0 |
| Vesper.Printf | 3 | 0 |
| Vesper.List | 2 | 0 |
| Vesper.Seq | 2 | 0 |
| Vesper.Array | 1 | 0 |
| Vesper.Comparison | 1 | 0 |
| Vesper.Set | 1 | 0 |
| Vesper.Choice / Exceptions / Option / Result | 0 | 0 |

Accepted with no error and no exemption list (`PairOutcome.Unrepresentable`, derived from
file content): `prim-types-decimal.fsi`, `prim-types-nativeint.fsi`, `prim-types-nd-array.fsi`,
`prim-types-attr.fsi`, `capabilities.fsi`, `array-index.js.fsi`.

`capabilities.fsi` landing there is *correct and load-bearing*, not a gap — see §4.

---

## 1. Verified inventory, per package, split three ways

### Kind A — deliberately absent and correct; the conformance pass is wrong about them (6 errors)

These need **no library source at all**. Each is a machinery fix in `ConformancePass` /
the manifest, and each is small. Doing Kind A first removes a quarter of the errors and
stops them masking real gaps.

| # | file | error | why it is correct | what it needs |
|---|---|---|---|---|
| A1 | `Vesper.Core/capabilities-compat.js.fsi` | V240 SigWithoutImpl | 5 pure type **abbreviations** (`type IDisposable = Vesper.disposable`, …). F# needs no `.fs` for a transparent abbreviation — `ConformanceTests.fs:171` pins exactly that at kernel level | `ConformancePass.unpaired` (`ConformancePass.fs:213`) demands `not (List.isEmpty externs)` before it will say `Unrepresentable`. A contract that is `bodiless` with no `val`s and *zero* externs falls through to `SigOnly`. Relax the guard to `bodiless && vals.IsEmpty`; `Unrepresentable(f, [])` |
| A2 | `Vesper.Core/ops-platform-runtime.js.fsi` | V240 | 3 `val`s (`structuralEquals`, `structuralHash`, `checkedDivisor`) whose bodies are `Vesper.Core.mjs` exports (`Vesper.Core.mjs:136,183,184`) | no verdict exists for "body lives in the committed `runtime` asset" — see §4 open question |
| A3 | `Vesper.Comparison/comparison-runtime.js.fsi` | V240 | 1 `val` (`structuralCompare`), body at `Vesper.Comparison.mjs:77` | same as A2 |
| A4 | `Vesper.Core/array-index-body.js.fs` | V242 ImplWithoutContract | it *is* the body of `array-index.js.fsi`; the two just don't share a `pairingStem` (`array-index-body` vs `array-index`). The name difference is deliberate — the manifest says a matching stem would make the intrinsic-repr extraction treat it as the array's platform repr | either teach the pass this pair, or split "pairing stem" from "repr-extraction stem". Note the *other* half of this bug: `array-index.js.fsi` is currently accepted as `Unrepresentable` — i.e. accepted for the wrong reason, since its body exists |
| A5–A6 | `Vesper.Printf/structural-printer.fsi` ← `structural-printer.js.fs` | V240 ×2 (`RuntimeFormatState`, `StructuralPrinter` missing) | **mispaired.** `structural-printer.js.fs` is not an implementation of that contract at all. It is a standalone JS `%A` engine exposing `module StructuralPrinter` with `structuralFormat` / `float32ToString`, imported by the backend at `EmitJsContext.fs:444-460`. It implements *no* `.fsi`, like `array-index-body.js.fs` | the manifest must be able to say "this `.fs` implements no contract". Today the stem rule pairs them because both stem to `structural-printer` |

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
| C6 | `Vesper.Printf/formatter.fsi` | V240 | the contract itself is CLR: `open System.IO` / `System.Text` / `System.Runtime.CompilerServices`, `new: … * writer: TextWriter`, `… * builder: StringBuilder`. Also 3 constructor overloads, and JS rejects secondary ctors (`EmitJsTypes.fs:414-417`). **printf on JS never routes through `Formatter`** — the manifest says printf is front-end special-cased into `Format` nodes. This contract has no JS role at all |
| C7 | `Vesper.Seq/struct-seq.fsi` | V240 | the *contract* names `System.Collections.IEnumerable` (non-generic — **not** in `capabilities-compat.js.fsi`), `IEnumerator`, `IDisposable`, and every type is `[<Struct>]`. The whole point of the file is zero-allocation by-value struct chaining, which is a CLR performance construct. This is CLR-only by nature |
| C8 | `Vesper.Set/set.fsi` | V240 | contract names `System.Collections.IEnumerable`, `System.Collections.IStructuralEquatable`, `ReadOnlySpan<'T>`, `[<CollectionBuilder>]`, `[<ScopedRef>]`. 954 lines of contract / 1145 of impl. See §6 |

**C6, C7, C8 are not port work — they are scope work.** The right move for each is to
move the `.fsi` out of `[core] files` into `[targets.clr] files`, which the manifest schema
already supports (`Vesper.Core` uses `[targets.js] files` today) and which the conformance
pass already honours (`ReferencedProject.resolveFiles`). A contract not in the JS file set
is not checked on JS. That is *three errors deleted by a correct statement of fact*, not by
an exemption — and it does not violate the list-free property, which
`ConformanceTests.fs:418-429` pins only against `[targets.<t>] sig-only`.

---

## 2. Blocked vs writable — the summary

| | count | what it is |
|---|---|---|
| **Writable now** (Kind B) | 10 | ~170 lines of F#. An afternoon or two |
| **Machinery, no library source** (Kind A) | 6 | 4 small edits to `ConformancePass` / the manifest schema. One (A2/A3) needs a design decision first |
| **Manifest scope statement** (C6, C7, C8) | 3 | move 3 `.fsi` to `[targets.clr] files` |
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

**The gap worth naming:** nothing checks that `Vesper.Core.mjs` / `Vesper.Comparison.mjs`
actually export the symbols their `.fsi` declares. A renamed export surfaces as an ESM link
error under Node — *and only if node is on PATH*, since every such test `skiptest`s
otherwise. On a node-less CI leg those two assets get zero validation.

**Should they become generated?** `Vesper.Comparison.mjs` — yes, and it is nearly free: it
is one function of the same shape as `Vesper.Printf.mjs`'s walker. `Vesper.Core.mjs` — the
manifest's own note is right that this is the `--compiling-fslib` bootstrap ("Route B"), and
it is *not* on the critical path for this port: `structuralEquals`/`structuralHash`/
`enumeratorOf` are self-referential (the equality runtime cannot be written in a language
whose `=` it implements) and want a real bootstrap story. **Recommendation: keep both
hand-authored for this port, but add an export-presence check** (assert the `.mjs` exports
every name its runtime `.fsi` declares) so a rename cannot silently rot. That is a
cheap, node-free guard and it also converts A2/A3 from "unchecked" to "checked
differently".

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
| **T1** | **Statement of scope.** Move `formatter.fsi`, `struct-seq.fsi`, `set.fsi` to `[targets.clr] files`, each with a comment saying what a JS port would have to decide first | C6, C7, C8 | −3 (→21) | manifest only, ~30 min |
| **T2** | **Conformance machinery.** (a) relax `ConformancePass.unpaired` so a bodiless, `val`-free contract is `Unrepresentable` even with zero externs → fixes `capabilities-compat.js.fsi`; (b) make "this `.fs` implements no contract" expressible, covering `array-index-body.js.fs` **and** un-pairing `structural-printer.js.fs` from `structural-printer.fsi`; (c) a verdict (or a `runtime`-derived derivation) for a contract whose bodies live in a committed `.mjs` → `ops-platform-runtime.js.fsi`, `comparison-runtime.js.fsi`. Add the `.mjs` export-presence check from §4 | A1–A6 | −6 (→15) | small, but (b) and (c) each need a design call. Half a day plus discussion |
| **T3** | **Vesper.Core, the writable half.** `` `[]` `` repr + the 4 `Fun` interfaces in `prim-types-min.js.fs`; `ignore`/`isNull`/`box`/`invalidArg` in `ops-platform.js.fs`; new `int-comparison.js.fs`; add `core-types.fs` and `structural-format.fs` to `[targets.js] impl` and see whether they compile | B1–B6, C1, C4, C5 | −8 (→7) | ~30 lines of new F#, plus the compile-and-see on the two neutral bodies. Half a day if C4/C5 just work; a day if `Curried`/`Flattened` surface a codegen gap |
| **T4** | **`list.fs` merge.** Fold `list.js.fs` back into a neutral `list.fs` (restoring `IsEmpty`/`Head`/`Tail`, `toSeq`, `ofSeq`, the explicit `disposable` impl, and — if `[<Struct>]` holds on a `val`-form class — the struct attribute). Delete `list.js.fs`; one shared `impl`. Regenerate `Vesper.List.mjs` (the byte-identity test will insist) | B7, B8, + the `7e994a93` revert | −2 (→5) | ~1 day; the risk is entirely in `[<Struct>]` on a `val`-form class |
| **T5** | **Vesper.Array.** New `array.js.fs`: 16 functions, only `zeroCreate` non-obvious | B9 | −1 (→4) | ~100 lines, half a day |
| **T6** | **Vesper.Seq.** New `seq.js.fs`: 4 functions. `fold`/`reduce`/`toArray` are straightforward. **`truncate` is the one real decision** — see below | B10 | −1 (→3) | ~40 lines + a design call; ~1 day |
| **T7** | **The two remaining architectural items.** `obj`'s heritability on JS (C2); `compiler-attributes.fsi` (C3) — which is either "implement `extends`/`super` on JS" or "these are CLR-only marker types". I recommend the latter and deferring the former | C2, C3 | −3 (→0) | unbounded; scope it separately once T1–T6 are in |

After T6 the JS run is at **3 errors, all of them named architectural questions** rather
than missing work. That is a good place to stop and re-decide.

Suggested first move: **T1 + T2 together.** They are the cheapest, they are the ones that
make the remaining count *mean* something, and T2(b)/(c) want a conversation before code.

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
- ❌ `capabilities-compat.js.fsi` and `ops-platform-runtime.js.fsi` "are JS-only contracts served by the `.mjs` runtime" — the *characterisation* is right, but they are **not accepted**: both are live V240 hard errors today. And they are not the same case as each other — one is pure abbreviations (a conformance-pass hole), the other is genuinely runtime-served.
- ❌ `array-index-body.js.fs` "deliberately has no `.fsi`" — it deliberately has a *differently named* `.fsi` (`array-index.js.fsi`). It is a live V242 error, and its contract is simultaneously being accepted for the wrong reason.
- ❌ `comparison-runtime.js.fsi` was listed alongside `array.fsi`/`seq.fsi`/`set.fsi` as "no JS implementation". It is not missing work — it is the `Vesper.Comparison.mjs`-served twin of `ops-platform-runtime.js.fsi`.
- ❌ "`structural-printer.fsi`'s `RuntimeFormatState`/`StructuralPrinter` are absent from `structural-printer.js.fs`" — true as stated but misleading. `structural-printer.js.fs` is a complete, generated, byte-checked, node-tested JS `%A` engine that implements a *different* surface (`structuralFormat`). It is not an incomplete port of that contract; the two are mispaired by the stem rule.
- ❌ `formatter.fsi` was omitted from the Printf line — it is a third live error there.
- ❌ (already known) `ops-platform.fsi` mentions `nativeint`: it does not. Zero occurrences.
- ➕ Not in the handed-over inventory at all: `list.js.fs` **already implements
  `interface seq<'T>`**, which falsifies the stated reason both for its existence and for
  the `list.fs` → `list.clr.fs` rename (§3).

---

## 10. Reproducing the numbers

There is no committed way to do this. I used a throwaway Expecto test in
`XParsec.FSharp.SemanticAnalysis.Tests` that walks `Directory.GetDirectories(src, "Vesper.*")`,
calls `ConformancePass.checkManifest "js"` then `ConformancePass.enforce` on each, and dumps
every `PairOutcome` plus every diagnostic. It has been reverted.

If this port proceeds past T2, that dump is worth committing as a real test — an assertion
that the JS error set is *exactly* a declared list, shrinking tranche by tranche, is the
thing that keeps the count honest.
