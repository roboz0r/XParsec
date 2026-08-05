# Porting the Vesper library to the JS target — what is left

Working document. Ephemeral: delete it when the work lands.

T1–T6 are done and their reasoning is **not** repeated here — it lives where it is
enforceable: the per-package `manifest.toml` comments, `EngineCore.capabilityPlatformKey`'s
doc comment, and the test suites named below. This file now holds only what is outstanding
and the questions a future session has to answer.

Line numbers are deliberately absent: the previous revision's had already rotted
(`EmitJsTypes.fs:409` → `:419` within one tranche). Constructs and file names only.

---

## 1. Where it stands

**5 hard conformance errors on `js`, all in `Vesper.Core`. 0 on `clr`.** Down from 24
across 7 packages. Every other package — Array, List, Seq, Printf, Comparison, Choice,
Option, Result, Exceptions — is clean, and Set is deliberately out of scope (§4.4).

The five, verbatim from `ConformanceTests.fs`'s *"js: the hard-error set is exactly the
un-ported library surface"*:

| # | error | § |
|---|---|---|
| C2 | `prim-types-object.fsi`: type `obj` disagrees on heritability | §2.1 |
| C3 | `compiler-attributes.fsi` has no implementation file | §2.1 |
| C5 | `structural-format.fsi` has no implementation file | §2.1 |
| C1 | `prim-types-min.fsi`: type `Fun` declared but not defined | §2.2 |
| C4 | `core-types.fsi` has no implementation file | §2.2 |

**Conformance is necessary but not sufficient.** The pass pairs contracts with impls; it
does not compile them. Two things it cannot see are live: the multi-file build limit
(§4.3) and the fact that `Vesper.Seq` is absent from the default JS program contract
(§3.2). Neither shows up as an error.

---

## 2. The tranches

All five errors are in `Vesper.Core`, and the design questions behind C2 / C3 are now
**decided** (§4.1, §4.2) — so what is left splits into three tranches by cost, not by
question. Tranche 3 (§4.3) is the only one that involves backend work. §3.2 is independent
of all three and can land at any point, but wants its own commit.

### 2.1 One-liners — takes the count 5 → 2

None of these writes a body, and none needs a codegen change.

| item | change | why it is a one-liner |
|---|---|---|
| **C2** `obj` | `prim-types-object.js.fs`: `(# "unknown" #)` → `(# class "Object" #)` | `exn` is the same `extern class` shape and is already bound `(# class "Error" #)` on JS. `(# class … #)` needs no emitter support for `inherit` — heritability is a claim about the declaration, and nothing inherits `obj` |
| **C3** attributes | `[targets.js] sig-only = ["compiler-attributes.fsi"]` | Per-target `sig-only` is a supported key (the resolver unions `[core]`'s with the target's). The attributes are fully erased on JS — see §4.2 |
| **C5** `structural-format` | add `structural-format.fs` to `[targets.js] impl` | No new file: the existing CLR body is two pure interface declarations with nothing target-specific in it |

C2 is inert at runtime (JS emission is type-erased), but it touches every `obj`-typed JS
body — `structural-printer.js.fs` is 434 lines of `(# … #)` helpers typed `obj`, so
`StructuralPrinterTests` plus the `Vesper.Printf.mjs` byte-compare is the canary. Run all
four suites (§6).

### 2.2 Writing source — DONE, count 2 → 0

C1 was a verbatim copy of the four `Fun` arities from the CLR body; C4 needed no codegen
change; the two coverage gaps (a `mutable` record field, a *class* implementing a plain
interface) both passed first try. What the tranche actually found:

- **The package-own-contract collision is an artefact of the test helper, not a real
  limit.** `core-types.fs` compiles against a contract containing `Vesper.Core`'s own
  manifest, once the compile NAMES its home assembly — the external-claim check is
  `shapeHomeAssembly <> ctx.AssemblyName`, and the old helper passed `""`. So the
  `arrayDepsJsContract` comment's "safe only where the package declares no in-file types"
  was wrong, and is corrected in place.
- **It still does not RUN, for an unrelated reason.** `f.Invoke(a, b)` on a receiver typed
  as an interface the compiling file does not itself declare lowers to a receiver-first
  free function imported from the package's asset, which exports no such name. Attached-
  method dispatch is chosen only for interfaces in the file's own declaration set. That
  matters well beyond this file: under §4.3's per-file model, EVERY cross-file interface
  takes this path, and an interface-only file emits no module for the import to name.

  **DECIDED: interface dispatch on JS is always an attached-method call** — same file,
  another file, another library, no difference. An interface has no runtime existence on
  JS (no module, no export, no free-function form); its implementations are attached
  methods on the implementing class, so a member reached through an interface-typed
  receiver is a member access on whatever object is there, and where the interface was
  DECLARED is not part of the question. The local/external split is the wrong axis.
  The capability mapping keeps precedence over it — that one renames a member to its
  JS-native spelling rather than dispatching by name. Expected to fix TS interop in the
  same stroke: the path this replaces is a loud `failwith` for any real npm package.

### 2.3 Per-file JS output (§4.3)

The backend tranche. It is what finally COMPILES `Vesper.Core` on JS, and therefore what
turns the two tranches above from a green paper check into a running artifact.

---

## 3. Loose ends that are not conformance errors

### 3.1 The `.mjs` runtime assets

Five generated and self-maintaining (byte-compared against a regeneration on every run), two
hand-authored:

| asset | source | gate |
|---|---|---|
| `Vesper.List.mjs` | `list.fs` | `ListTests.fs` |
| `Vesper.Array.mjs` | `array.fs` | `ArrayTests.fs` |
| `Vesper.Seq.mjs` | `seq.fs` | `SeqTests.fs` |
| `Vesper.Option.mjs` | `option.fs` | `OptionTests.fs` |
| `Vesper.Printf.mjs` | `structural-printer.js.fs` | `StructuralPrinterTests.fs` |
| `Vesper.Core.mjs` | **hand-authored** | export-presence, via `RuntimeServed` |
| `Vesper.Comparison.mjs` | **hand-authored** | export-presence, via `RuntimeServed` |

The two hand-authored ones are load-bearing: `Vesper.Core.mjs` carries `checkedDivisor`
(every integral `/` and `%`), `structuralEquals` / `structuralHash` (every `=` / `<>` /
`hash`) and `enumeratorOf` (every `for … in`); `Vesper.Comparison.mjs` carries
`structuralCompare` (every aggregate `<`/`>`/`<=`/`>=`). Rename an export and the conformance
pass turns the contract back into a hard error — that check is node-free and runs every time.

`Vesper.Core` itself still has **no** generated JS output of its own, and cannot until §4.3
lands. Under that model the table above changes shape: a generated entry becomes one `.mjs`
per emitting `.fs` inside the package directory, and the two hand-authored ones become
referenced assets copied to the output directory rather than "the package's module".

### 3.2 `Vesper.Seq` is not in the default JS program contract

`TestHelpers.jsManifests` lists Core, Comparison, Printf, Exceptions, Option, List, Array —
not Seq. So `SeqTests` resolves the package through its own contract, but a *consumer*
program calling `Seq.fold` would not resolve at all. Adding it is probably one line; it was
left alone because it widens the fixture every JS test resolves against, which deserves its
own check rather than a drive-by.

### 3.3 Contract-only members with no way to implement them

`list.fsi` declares `Item`, `Empty`, `Cons`, `GetSlice`, `GetReverseIndex`, and none has a
body on *either* target. `Item` is the interesting one: it is an INDEXED property, and
nothing in the front end can define one — `get_Item` is resolved only on external receivers,
there is no `IsIndexed` on the member node, and no Vesper source anywhere declares one. This
is a front-end gap, not library work, and conformance does not see it (§4.5).

---

## 4. The design questions

4.1 – 4.3 are **decided** (2026-08-03) and are kept here with their reasoning, because the
reasoning is what a reviewer of the tranches in §2 will want and half of it corrects a
premise the previous revision argued from. 4.4 – 4.7 are still open; each is stated with what
is already known, so a session starts from evidence rather than re-derivation.

### 4.1 What is `obj` on JS? — DECIDED: the real `Object` *(C2)*

`prim-types-object.fsi` says `type obj = extern class` — heritable. `prim-types-object.js.fs`
binds the untagged `(# "unknown" #)`, and conformance flags the disagreement.

**Bind `type obj = (# class "Object" #)`.** The premise that made this look like a backend
project was wrong: `(# class … #)` already means something on JS — `exn` is the identical
`extern class` shape and is bound `(# class "Error" #)` today. Heritability in the contract
is a claim about the DECLARATION, not a demand that the emitter support `inherit`; the
emitter's inheritance wall fires on a source `inherit` clause, and nothing writes one against
`obj`. So this is decoupled from §4.2 entirely, and it is not a per-target contract split.

What it does NOT settle is the `top` / `obj` conflation — JS `obj` becomes the class root
where some sites mean the value ⊤ (`unknown`). That is deferred, with the reasoning and the
naming direction recorded in `contract-sourced-intrinsic-identity-plan.md`. It has no
observable consequence until `.d.ts` emission (§4.3), which is what will force it.

### 4.2 Are the compiler attributes CLR-only? — DECIDED: erased on JS *(C3)*

**`[targets.js] sig-only = ["compiler-attributes.fsi"]`.** The contract stays in the JS
build — the declarations are still extracted — and the manifest states that JS owes no body.

Not the alternative the previous revision recommended (move the `.fsi` to
`[targets.clr] files`), for a reason that will outlive this tranche: attribute decoding is
due to stop being a syntactic short-name match against hardcoded string lists and start
resolving through name resolution like any other type, with the `Attribute`-suffix rule.
Once it does, `compiler-attributes.fsi` is load-bearing IN THE JS CONTRACT, because
`core-types.fs` — added to `[targets.js] impl` by C4 — carries `[<ReferenceEquality>]` and
`[<NoComparison>]`. Deleting the declarations from the JS contract would break that build.

The two "walls" the previous revision cited are not reached, because no JS `.fs` is written.
More importantly, one of them was the wrong question: **whether `inherit Attribute` has any
representation is a BACKEND decision.** Attributes being declared as classes inheriting
`Attribute` is an F#/CLR source-compatibility decision, not a codegen contract — so "is
`Attribute` representable on JS?" must not gate attribute resolution, and a resolution rule
that demands a representable base would reject every attribute on JS. The declared base in
the contract is what resolution reads.

On what an attribute MEANS on JS, by purpose:

- **Compiler signal** (all 8 in this file — `ReferenceEquality`, `StructuralEquality`,
  `StructuralComparison`, `NoEquality`, `NoComparison`, `CallAtMostOnce`, …): fully erased,
  no runtime residue. There is no JS idiom because it is not a language feature there; the
  CLR emits TypeDefs only because CLR metadata cannot reference a type that does not exist.
  `sig-only` is the correct semantics here, not a concession.
- **Static data retrievable at runtime**: TS 5 standard decorators plus decorator metadata
  (`context.metadata`, reachable as `Klass[Symbol.metadata]`) is the target if and when this
  is needed. Nothing needs it today — there is no reflection surface in the language at all
  — and it would mean a real `.mjs` body per attribute class, so it is a separate design.

### 4.3 Multi-file JS library builds — DECIDED: a package is a DIRECTORY

A JS runtime module is compiled from **one** source file (`JsProjectInfo.Source` is a single
`JsSource`). Every package that has shipped an asset so far is single-file, and `Vesper.Array`
only manages it because its prelude is `inline` and splices away. It cannot be dodged for
**`Vesper.Core`**, which is eighteen files. So: no JS asset for Core — and therefore no Route
B bootstrap — until this lands.

**The model is per-file output, not a merged module.** A package (manifest `name`) becomes a
directory; each `.fs` that emits anything becomes an `.mjs` beside its siblings, and a
generated barrel re-exports them.

This is *smaller* than merging N files into one module, not larger, because it deletes that
plan's hard half rather than solving it. Pools are never merged, so the hazard the CLR's
`Layout` states — two files' pools both number from 0, so a bare id does not miss across
files, it silently names a different node — never arises. Each file keeps its own 1:1 source
map, preserving the anchor locality that is separately tested. `JsProjectInfo.Source` stays a
single `JsSource` and `buildProgram` stays per-file.

The front-end half already exists and is target-neutral: `AssemblyFiles.analyseAssemblyWith`
analyses an ordered `(path, source)` list, composing each file's projected view nearest-first
ahead of the external provider, and takes the front end as a parameter
(`Pipeline.analyseForSelfHost` for a BCL-free package). `ClrDriver.compileAssemblyWith` is
the driver template and `CrossFileTests` runs it end to end.

**The one real cost: import specifiers are per-file, but symbol identity is per-assembly.**
`SymbolOrigin` carries `{ Home: Origin; Namespace }` where `Origin = Unstamped | InAssembly`,
and `JsImports` keys both its runtime map and its entries by assembly name (failing with
"has no JS runtime module"). A cross-file reference therefore resolves to "somewhere in
`Vesper.Core`" with no way to name `core-types.mjs`. The fact EXISTS at analysis time —
`AssemblyFiles` mints a per-file `OriginSource` and hands it to the view projection — it just
never reaches the symbol. So the spine of the tranche is: carry the declaring file onto the
symbol origin (additive, with the usual central alias), then key imports on (assembly, file)
and render `./<Package>/<name>.mjs`. Cross-file TYPE references ride the same path, so
records and unions come along with it. Nothing changes for CLR — one PE per assembly stays,
and the new field is simply unread there.

Decided alongside it:

- **A barrel** (`index.mjs`, re-exporting) — the accepted JS idiom, and it keeps today's
  package-level import specifier working for consumers.
- **A `.fs` that emits nothing gets no `.mjs`.** Most of Core's eighteen files are
  intrinsic-repr-only and lower to no JS. The regeneration test then asserts ABSENCE, which
  says more than an empty file would.
- **The hand-authored `Vesper.Core.mjs`** (possibly renamed) is not generated from any `.fs`,
  so it is a referenced asset COPIED to the output directory — the analogue of referencing a
  `.dll` directly from an `.fsproj`. The `runtime` manifest key already names it; what
  changes is that its semantics are "reference and copy", not "the package's one module".
- **`.d.ts` emission is a separate feature.** Nothing emits `.d.ts` today: `Vesper.Ts.Extractor`
  runs the other direction (`.d.ts` → Vesper manifest, `TypeMap` doing `boolean`→`bool`,
  `void`→`unit`). Emitting it means the inverse map plus decisions on how unions,
  capabilities, `dynamic`, `undefined` and `obj`-as-⊤ (§4.1) surface in TS. Sequence it after
  per-file `.mjs` and derive it from `TypeMap`'s existing pairings, so the two directions
  cannot drift.

### 4.4 Does the port stop before `Vesper.Set`? *(standing decision — confirm or revisit)*

Currently **yes**, stated in `src/Vesper.Set/manifest.toml` rather than left as a red error.
Reasons unchanged: `set.fsi` is 954 lines of contract over 1145 of AVL tree; its *contract*
(not just its impl) names `System.Collections.IEnumerable`, `IStructuralEquatable`,
`ReadOnlySpan<'T>`, `[<CollectionBuilder>]` and `[<ScopedRef>]`, so porting it means first
deciding what all of those mean on JS; and it sits at the bottom of the dependency graph, so
nothing waits on it.

Including it roughly doubles the port and pulls the BCL-interface question in early. Revisit
only if something concrete starts needing sets on JS.

### 4.5 Member-level conformance — assessed, not taken

The pass reads the raw CST and extracts only module-level types and `val`s. A member rung
needs a `MemberDecl` extractor over the member lists of all twelve `TypeSignature` shapes and
all twelve `TypeDefn` shapes, a per-TYPE pairing (member names only collide within their
type), a new `ConformanceError` case, and a decision about the kinds no `.fs` can satisfy —
`static member Empty`, indexed properties (§3.3), `[<Experimental>]` slots.

It would light up on **both** targets at once, `set.fsi` alone contributing hundreds. Its own
tranche, and worth doing: it is what would have caught §3.3 automatically.

### 4.6 Should the hand-authored assets become generated?

`Vesper.Comparison.mjs` — yes, and nearly free: one function of the same shape as
`Vesper.Printf.mjs`'s walker.

`Vesper.Core.mjs` — this is the `--compiling-fslib` bootstrap ("Route B"), and it is
genuinely hard: `structuralEquals` / `structuralHash` / `enumeratorOf` are self-referential
(the equality runtime cannot be written in a language whose `=` it implements). It also needs
§4.3 first. Keep it hand-authored until the bootstrap has a real design.

### 4.7 Computation expressions, including `seq { }` *(deferred — own project)*

`seq { }` / `yield` does not exist anywhere: the parser has the syntax but elaboration has no
arm for it, and there is no `SeqExpr`/`Yield` node in `ExprShape` at all. Nothing in the port
needs them — `Seq.truncate`'s hand-rolled cursor is not a workaround, it is the state machine
a compiler would generate.

One thing worth knowing before scoping it, because it inverts the usual intuition: on JS a
sequence expression lowers to a generator (`function*` / `yield`), close to 1:1; on CLR it
needs a compiled state machine. **The JS half is the cheap half**, so a CE project scoped
CLR-first would hit the expensive end before proving the design. There is already a
`docs/sequence-expressions-plan.md`.

---

## 5. Reference — what the JS backend refuses

Every one is a `failwith` in the emitter, **not** a graceful diagnostic: a library body that
trips one takes the whole compile down with a raw exception. Re-verify before relying on it;
this list is derived, not enforced.

| construct | where |
|---|---|
| `null` literal in expression position, `try … with`, `x :? T` type test | `EmitJs`, the `Null`/`TryWith`/`TypeTest` arm (`try … finally` **is** supported) |
| `:? T as y` pattern | `EmitJsContext` |
| range in value position (`let r = 1..10`) | `EmitJs`, the `Range` arm — already reported at Elaborate, so reaching it means emitting a program known bad |
| class `inherit` | `EmitJsTypes` — a SOURCE `inherit` clause only; an `extern class` intrinsic repr (`exn`, and `obj` after §4.1) does not touch it |
| more than one constructor on a class | `EmitJsTypes` — a JS class has exactly one |
| duck-typed (`Pattern`) `for … in` enumerator; refutable / non-tuple `for … in` binder | `EmitJs` |
| `nativeint` / `decimal` literal | `JsEmitHelpers` |
| an `override` whose name clashes with an interface impl | `EmitJsTypes` |
| a format sink other than `sprintf`/`printfn`/`eprintfn` | `EmitJs` |

Adjacent facts that shape any remaining work:

- **Every exception type erases to a JS `Error`**, so `invalidArg` preserves its message and
  not its type. Custom exception types are unimplemented — no elaboration arm for
  `ExceptionDefn`, and catching by type would need the refused `TypeTest`.
- **`PlatformTypes` deliberately does not walk class member bodies** ("a backend may not emit
  them yet … flagging a type they reference would be a premature reject"). Record/union
  augmentation bodies and all interface-impl bodies **are** walked. So a class member
  mentioning an unrepresentable type is silently unchecked.
- **Library mode is proven end-to-end for five packages**: List, Option, Printf, Array, Seq —
  all via `TestHelpers.compileLibrary`, all byte-compared and executed under Node. Extending
  it to Core is §4.3.
- **A package that splices its own prelude** needs its own manifest in the contract that
  compiles it (`ArrayTests` takes `arrayDepsJsContract`, not `coreDepsJsContract`). Safe only
  where the package declares no in-file types — that collision is what the exclusion guards.

---

## 6. Reproducing the numbers

`ConformanceTests.fs`, test *"js: the hard-error set is exactly the un-ported library
surface"* — it walks `Directory.GetDirectories(src, "Vesper.*")`, runs
`ConformancePass.checkManifest "js"` + `enforce` on each, and asserts the messages equal a
declared list. Run `XParsec.FSharp.SemanticAnalysis.Tests`; a failure prints both sides.

That list is the count, and it is pinned rather than counted for a reason: an entry that
vanishes *without* the corresponding source appearing means the pass stopped asking.

Full verification for any tranche here is four suites —
`XParsec.FSharp.SemanticAnalysis.Tests`, `XParsec.FSharp.Codegen.Js.Tests`,
`XParsec.FSharp.Codegen.Clr.Tests`, `Vesper.Tests` — because library-source changes reach the
CLR backend and the parse goldens too, not just the JS one.
