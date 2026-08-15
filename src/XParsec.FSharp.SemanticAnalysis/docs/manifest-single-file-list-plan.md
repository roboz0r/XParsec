# Collapsing `files` + `impl` into one ordered list

**Question (user, 2026-08-12):** should `manifest.<target>.toml` become just
`files = [every .fs and .fsi, in compile order]`, dropping the other groups of files that
get compiled?

**Answer: yes for `files` + `impl`. No for `sig-only` / `impl-only` / `runtime`** — those
are not "groups of files that get compiled", they are declarations of intent and an asset
list, and none of them is derivable from the merged order. The win is real but it is not
the one it looks like from outside: it is not "fewer keys", it is **one ordering instead of
two that can silently disagree**.

**Not ready to start (2026-08-15).** The prerequisite below has to land first; until it does,
justification 3 is an argument nothing has tested.

## What the five lists mean today

| key | contents | who reads it |
| --- | --- | --- |
| `files` | `.fsi` contracts, compile order | contract extraction, in order, into the package provider |
| `impl` | `.fs` bodies, compile order | intrinsic-repr pre-scan (order-free); cross-package inline bodies (**order matters** — a later body wins a clash); conformance pairing (order-free, keyed by name); hashing |
| `sig-only` | `.fsi` deliberately without a `.fs` | suppresses the content-based unpaired split |
| `impl-only` | `.fs` deliberately without a `.fsi` | removes it from pairing candidacy |
| `runtime` | JS assets | never parsed; deliberately excluded from `sourceInputs` |

The compiled `SourceFile list` does **not** come from the manifest — a driver is handed it.
So merging is a change to contract extraction and conformance, not to codegen input.

## Why merge

1. **The two orders can disagree, and in `Vesper.Core` they already do.** `fun-adapters` is
   last in `files` (after `int-comparison`) but sixth-from-last in `impl` (before
   `structural-format` and the operator files). Either one of those is wrong or the position
   does not matter — the manifest cannot say which, and nothing checks. One list makes the
   question unaskable.
2. **Signature-ness is already encoded twice.** `parseFileFull` picks `parseSignature` vs
   `parse` by `.EndsWith ".fsi"`, while ordering and pre-scan go by which list the entry is
   in. Nothing stops `impl = ["foo.fsi"]`; the two encodings would then contradict, and the
   extension wins in one place and the list in another.
3. **The merged order is the order F# actually compiles in.** `a.fsi, a.fs, b.fsi, b.fs` is
   expressible in one list and is not expressible in two. The current split can only say
   "all contracts, then all bodies", which is not how the compiler under test orders a real
   project. The assembly pipeline now TAKES a `.fsi` (`AssemblyFiles.SourceUnit`), so this
   stopped being hypothetical — but nothing passes one yet, which is the prerequisite below.

## Prerequisite — pair the `.fsi` halves into a real package build

**Do this first, and decide the merge afterwards.** Every caller hands the pipeline
`SourceUnit.ofImplementation`: `TestHelpers.buildPackage` and `TestHelpers.vesperCoreDll` in
`Codegen.Clr.Tests`, `SelfPackageIntrinsicsTests`, and the `JsPackageTests` corpus run. No
real package has ever been analysed alongside its own contract.

### What that costs today

`AssemblyFiles.conformanceDiagnostics` is the only place the CST check (`Conformance.checkUnit`)
and the semantic typar checks (`ConformanceTypars.checkFile` / `checkMembers`) run inside the
assembly pipeline, and it is reached only from a unit that HAS a signature. So on the real
packages it has never run at all.

An unrun check decays. On 2026-08-15 `checkFile` turned out to be doing nothing on the real
corpus twice over: it exempted `let inline` by construction, and its lookup names carried no
namespace (`ArithmeticOperators.op_Addition` against a contract publishing
`Vesper.ArithmeticOperators.op_Addition`), so every binding in a namespaced module — which is
all of `Vesper.Core`'s operators — was skipped. Both are fixed, with a mutation guard in
`ConformanceTyparsTests` that reverses `(+)`'s typars and requires the sweep to report it,
because a conforming corpus and a skipped one are otherwise indistinguishable. The CST half
has had no such exposure and should be assumed to be in the same state.

### Why it is cheap

The pairing needs no new mechanism and no manifest change. `ConformancePass.checkManifest`
already computes it, and `buildPackage` already CALLS it (for `enforce`) and then throws the
result away: `PackageOutcome.Pairs` carries `PairOutcome.Paired { SigFile; ImplFile }` per
contract, with `sig-only` and `impl-only` already honoured — a `sig-only` `.fsi` owes no `.fs`,
an `impl-only` `.fs` is kept out of pairing candidacy. Build the `SourceUnit` list off those
outcomes instead of mapping `manifest.Impl`, keeping each pair at its `.fs`'s position in
`impl` order.

### Expect it to fail, in two different ways

- **Real drift**, reported through `Kind.Conformance` and therefore fatal: `analyseGated`
  refuses an assembly carrying an error-severity diagnostic, so `buildPackage` fails rather
  than emitting a degraded DLL. This is the point of the exercise.
- **A publication change that is not drift.** A unit with a `.fsi` publishes the SIGNATURE's
  surface — the `.fsi`-derived signatures REPLACE the `.fs`-derived ones — so anything the
  contract hides stops being visible to LATER files in the same package. A package that
  compiles today can legitimately stop compiling, and that is a source fix (or a contract
  fix), not a reason to back the pairing out.

### What it settles for this doc

Justification 3 is untested until a `.fsi` sits in a package build. Once the unit list is
being constructed, the interleaved order is either load-bearing or it is not, and the
`Vesper.Core` `files`/`impl` disagreement in justification 1 is either a live bug or inert.
Merging first means rewriting every manifest — and every package hash — on an argument, and
then discovering which.

## Why NOT also merge the rest

- **`sig-only` is not derivable.** With one list, "`foo.fsi` present and `foo.fs` absent" is
  visible — but that is exactly what the content-based split already computes. `sig-only`'s
  job is to *outrank* that split: it says "these `val`s have no bodies here and that is
  intended" for a case the content check would otherwise report. That intent has no
  spelling in the file list.
- **`impl-only` is not derivable** for the same reason, plus it actively removes a file from
  pairing candidacy so it is not married to a same-named `.fsi` it does not implement.
- **`runtime` is not a source.** It is never parsed and is excluded from the hash's source
  inputs on purpose. Folding it in would make an asset edit look like a source edit.

The natural follow-on — moving `sig-only` / `impl-only` from separate lists to a per-entry
marker, so the exemption sits next to the file it exempts — is a **separate** change and
needs the TOML reader to accept inline tables in the array. Not bundled here.

## Design

Parse ONCE into a typed ordered list. The thing to avoid is handing five consumers a bare
`string list` that each re-derives the role by sniffing the extension — that trades two
honest lists for one over-wide one.

```fsharp
[<RequireQualifiedAccess>]
type ManifestRole =
    | Contract        // .fsi
    | Implementation  // .fs

type ManifestFile = { Relative: string; Role: ManifestRole }
```

- `Manifest.Files : ManifestFile list` — the single ordered list.
- `Manifest.Contracts` / `Manifest.Impls` — derived views, so each of today's consumers
  changes by one line and the migration carries no behaviour.
- An entry whose extension is neither `.fs` nor `.fsi` is a parse ERROR, matching how an
  unknown `[core]` key is already treated: read as silence it would resolve a stale manifest
  to a plausible wrong file set.
- `impl` stays a recognised key for one release, parsed as `Implementation` entries appended
  after `files`, so migration is not a flag day. Or drop it in one commit — there are only
  10 packages and they are all in this repo. **Recommend the second**: a compatibility path
  for an in-repo format nobody else consumes is the kind of moving piece worth not having.

## Cost

- 19 manifests across 10 packages (`Vesper.Set` is CLR-only), plus
  `test/Codegen.Conformance/manifest.toml`, the JS `widget` fixture, and the
  `tmp/buildClosure-tests` fixtures.
- `ReferencedProject.parseManifest` / `sourceInputs` / `pairingKey`; `Hashing` (the manifest
  bytes are hashed, so every package hash changes once → one full rebuild);
  `ConformancePass` (`m.Impl` → `m.Impls`); `ReferencedProject.buildProviderWith` (two
  loops, both now filtered views); `SymbolProviders.inlineBodies`.
- `ReferencedProjectTests` pins the parsed lists directly and will need updating.

## Ordering caveat to preserve in the migration

`SymbolProviders.inlineBodies` resolves a clash by "later body wins", over `manifest.Impl`
order. A mechanical migration that keeps the relative order of `.fs` entries preserves this.
Interleaving `.fsi` between them does not change any `.fs`-to-`.fs` relative order, so the
rule is safe — but it is the one place where the merged order is load-bearing beyond
readability, and it should get a test before the change, not after.
