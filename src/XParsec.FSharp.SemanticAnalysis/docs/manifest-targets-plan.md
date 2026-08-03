# Manifest redesign: one `impl` list, targets as peers

Two defects in the `manifest.toml` schema, and the single change that removes both.

## Defect 1 — `impl` and `inline-bodies` are one list wearing two hats

The split claims to separate "files compiled into the DLL" from "files whose
`let inline` bodies are spliced across the package boundary". Nothing in the tree
actually needs that separation:

- `Vesper.Core` re-lists seven `prim-types-*.fs` under `inline-bodies` that are
  already in `impl`, with a comment apologising for it ("Also in `impl`, and
  necessarily in both").
- `Vesper.List`'s `impl-js` and `inline-bodies-js` are the identical one-element
  list.
- `Vesper.Comparison` has `impl = []` with `inline-bodies = ["comparison.fs"]` —
  a hand-written restatement of a fact about `comparison.fs`'s contents (every
  declaration is `inline`, so nothing is emitted).
- `ConformancePass` computes `resolveImpl target ∪ resolveInlineBodies target` as
  *the* impl set. The one consumer that needs the real concept unions them back.

Emission is decided per declaration and is already known from the declaration.
So: **one `impl` list.** A package whose declarations are all `inline` emits no
DLL — which is what `impl = []` says today by hand. `sig-only` stays; a contract
with deliberately no body anywhere is a genuine manifest fact, not derivable.

No escape hatch. Nothing currently needs "compile this but do not publish it as a
splice source", and building the hatch before the case exists is what re-creates
the defect. If such a case appears, it is its own design effort.

Cost accepted: a consumer today parses only its dependencies' `inline-bodies`;
merged, it parses their whole `impl`. The frozen inline cache amortises it, and
`Vesper.Printf`'s BCL-heavy `formatter.fs` — the worst case, and the reason
`inline-bodies-js = []` exists — is not in any JS list under the new scheme
anyway.

## Defect 2 — the base tier is secretly the CLR

`[core] impl` means both "target-neutral" and "CLR", and every piece of awkward
machinery in `ReferencedProject` traces back to that conflation:

| Symptom | Where |
| --- | --- |
| `target: string option`, `None` = CLR | `resolveImpl` and every call site |
| `Target` has a `Js` literal and no `Clr` — CLR is not a value | `Target.fs` |
| `impl-<t>` REPLACES the base, `files-<t>` APPENDS | `resolveImpl` / `resolveExtraFiles` |
| ...which forces neutral `ops-std.fs` to be repeated verbatim in `inline-bodies-js` | `Vesper.Core/manifest.toml` |
| `runtime-<t>` has no base key at all | `resolveRuntime` |
| `companionFs None` = `.fs` — the default probe shape is CLR | `ReferencedProject.companionFs` |
| ...which needs `declaredTarget` to stop the probe set and `sourceInputs` diverging | `ReferencedProject.declaredTarget` |
| `prim-types-int.fs` binds `(# "System.Int32" #)` under a neutral name | `src/Vesper.Core` |

### Target state

Three tiers, CLR an ordinary target. The TOML parser already handles nested
table headers (`TomlParser.fs`).

```toml
[core]
description = "..."
depends-on  = [...]
files    = ["core-types.fsi", ...]   # target-neutral contract
impl     = ["ops-std.fs", ...]       # target-neutral bodies
sig-only = [...]

[targets.clr]
impl = ["prim-types-int.clr.fs", "ops-platform.clr.fs", ...]

[targets.js]
files   = ["capabilities-compat.js.fsi", ...]
impl    = ["prim-types-int.js.fs", "ops-platform.js.fs", ...]
runtime = ["Vesper.Core.mjs"]
```

Every list is **inherit + append**, uniformly. There is no REPLACE anywhere,
because nothing needs to override a base that was secretly another target's.
`runtime` is simply a key some targets use (CLR has no runtime asset — its DLL
is built, not committed), so its "missing base" stops being an asymmetry.

### What this deletes

`companionFs`, `targetOverrideFs`, `declaredTarget`, and the coverage argument
they exist to protect. `sourceInputs` becomes "every path any list names".

That also fixes a claim the manifests currently make falsely — "Every `.fs` in
the package is listed here (one source of truth, no orphaned `.fs`)". Four files
are reachable today *only* by the name probe: `prim-types-{dynamic,exn,object}.js.fs`
and `structural-printer.js.fs`.

## File classification

Rule: a `.fs` is **platform-bound** iff it contains an inline-IL / intrinsic-repr
`(# … #)` body, or opens/names a BCL namespace. Otherwise it is neutral and keeps
its bare name.

**Neutral — stay `foo.fs`, listed in `[core] impl`**

- `Vesper.Core`: `compiler-attributes.fs`, `core-types.fs`, `ops-std.fs`, `structural-format.fs`
- `Vesper.Option/option.fs`, `Vesper.Result/result.fs`, `Vesper.Choice/choice.fs`

**CLR-bound — rename to `foo.clr.fs`, listed in `[targets.clr] impl`**

- `Vesper.Core`: `prim-types-{min,int,float,string,object,exn,decimal,bigint,nativeint,nd-array,attr}.fs`,
  `capabilities.fs`, `int-comparison.fs`, `ops-platform.fs` — inline IL and/or BCL reprs
- `Vesper.Comparison/comparison.fs` — 32 IL sites
- `Vesper.Array/array.fs` — one site, `zeroCreate`'s `newarr`
- `Vesper.Seq/{seq,struct-seq}.fs` — `IEnumerable`/`IEnumerator`/`System.Linq`
- `Vesper.Set/set.fs` — `System.Collections{,.Generic}`, `System.Text`
- `Vesper.Printf/{formatter,structural-printer}.fs` — `Span`/`ArrayPool`/`Globalization`

**`Vesper.List/list.fs` — the one that contradicts the intuition.** It has no
`System.` text, but `interface seq<'T>` and `toSeq`/`ofSeq` bind it to the BCL
`seq` contract, which is exactly what `list.js.fs` exists to strip. It is
CLR-bound today and gets renamed `list.clr.fs`. Making list genuinely neutral
means factoring the `seq` leg out of the contract; that is separate work, and
when it lands the package reverts to a single neutral `list.fs` and both target
lists drop it — a local change.

The BCL-mentioning-but-algorithmic files (`set.fs`, `seq.fs`, the printf pair)
are named honestly for what they are now. Their `.clr` suffix is a standing
marker of a dependency wart, which is the point.

## Unsupported-on-target diagnostics

A contract may declare a type that a given target cannot represent at all —
`nativeint`, `unativeint`, `nativeptr`, `voidptr`, `ilsigptr` on JS. That is a
third verdict, distinct from `sig-only`:

- `sig-only` — no body on any target, legitimately (`printf.fsi`, lowered by the
  front end). Consumed when the package is checked.
- **unsupported** — the contract exists, the target binds no representation, and
  naming the type in user code is an error. Consumed at the USE site.

Loading `Vesper.Core` on JS stays silent. The diagnostic belongs to the program
that writes `nativeint`, not to the library that declares it, and it names the
target: `nativeint is not supported on the js target`.

**Derived, never listed.** An intrinsic type with no representation bound for
target T is unsupported on T. Absence is the statement, so adding a target never
requires re-listing what it lacks. A manifest key would re-encode what the file
lists already say — the defect removed from `inline-bodies` above — and could not
express the granularity anyway: `prim-types-nativeint.fsi` happens to declare
five types that are all unsupported, but `capabilities.fsi` is partly supported
through the JS compat shim, which no file-level key can say.

**Any mention errors**, not just a use that demands the representation. A JS
program naming `nativeint` in a signature it never instantiates is confused
regardless, and the permissive rule costs more to build and more to explain.

The cost: a *forgotten* `foo.js.fs` reports as "not supported" rather than "you
did not write the file". Split on content to keep that honest — an
`extern`/intrinsic type whose whole body is a repr binding earns the unsupported
diagnostic; a contract with declarations that need a real body and has none stays
an FS0240-style hard error. Only the former is ever polite about absence.

Consequence, and the reason this is worth building: `ConformancePass` gains a
third `PairOutcome` (declared, unrepresentable, accepted), so a JS conformance run
becomes possible with no hand-maintained exemption list. Today conformance runs
only for CLR precisely because that list would have to be guessed.

## Migration steps

Each step builds and tests before the next.

1. **Schema.** `ReferencedProject.Manifest` becomes
   `{ Name; DependsOn; Shared: SharedLists; Targets: Map<string, TargetLists> }`
   with `SharedLists = { Files; Impl; SigOnly }` and
   `TargetLists = { Files; Impl; SigOnly; Runtime }` — two records so that a
   shared `runtime`, which is meaningless, is not merely conventionally empty
   but unspellable. Resolvers take `target: string`; `Target.fs` gains `Clr`.
   Delete `companionFs` / `targetOverrideFs` / `declaredTarget`.
2. **Stem pairing.** With explicit lists there is no companion derivation, so the
   `.fsi`→`.fs` pairing that `ConformancePass` and the intrinsic-repr extraction
   both need becomes one shared helper: strip `.fs`, then strip a trailing
   `.<target>` segment for any declared target; pair on what remains. (`prim-types-int.clr.fs`
   pairs with `prim-types-int.fsi`.)
3. **Renames.** `git mv` the CLR-bound files above, plus their `.parsed` goldens.
   Purely mechanical; the manifests written in step 4 name the new paths.
4. **Manifests.** Rewrite all 11 `src/Vesper.*/manifest.toml` and the
   `codegen-js` `widget` fixture; list the four currently-unlisted `.js.fs`.
5. **Consumers.** `ConformancePass`, `SymbolProviders.inlineBodies` (now reads
   `impl`), `Codegen.Js`, `Codegen.Clr`, `Hashing`, `TestHelpers`, bench fixtures.
6. **Tests.** `ReferencedProjectTests`, `HashingTests`, `ConformanceTests`, then
   a full no-filter run.
7. **Unsupported-on-target diagnostics** (see the section above): the derived
   unsupported rule, the use-site diagnostic, the third `PairOutcome`, and a JS
   conformance run that needs no exemption list.

Commit boundaries: steps 1–2 and 4–6 are one green-able unit (the schema change
breaks every consumer and manifest at once). Step 3 is the second commit, step 7
the third.
