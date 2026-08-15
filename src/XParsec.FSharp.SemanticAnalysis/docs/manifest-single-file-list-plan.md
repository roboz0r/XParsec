# Collapsing `files` + `impl` into one ordered list

**Question (user, 2026-08-12):** should `manifest.<target>.toml` become just
`files = [every .fs and .fsi, in compile order]`, dropping the other groups of files that
get compiled?

**Answer: yes for `files` + `impl`. No for `sig-only` / `impl-only` / `runtime`** — those
are not "groups of files that get compiled", they are declarations of intent and an asset
list, and none of them is derivable from the merged order. The win is real but it is not
the one it looks like from outside: it is not "fewer keys", it is **one ordering instead of
two that can silently disagree**.

**The prerequisite has landed (2026-08-15); the merge itself is still a decision.** What the
pairing found is recorded below — read it before merging, because it changes what
justifications 1 and 3 are claiming.

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
   project. **Superseded by the prerequisite:** the package build compiles that interleave
   today, off the pairing, with the two lists unchanged.

## Prerequisite — pair the `.fsi` halves into a real package build — **DONE**

`PackageUnits.ofOutcome` / `ofManifest` build the unit list off `ConformancePass.checkManifest`'s
pairing, each `.fsi` riding its `.fs`'s unit at that `.fs`'s position in `impl` order.
`TestHelpers.buildPackage`, `TestHelpers.vesperCoreDll`, `SelfPackageIntrinsicsTests` and the
`JsPackageTests` corpus run all take it; `ConformanceTests`'s `PackageUnits` list pins the
pairing per package per target, so a revert to `SourceUnit.ofImplementation` fails loudly.

Two failures came out of it, one of each predicted kind:

- **A publication gap, not drift.** The `.fsi` extractor published a member-less
  `type X = extern class` as a bare scalar, dropping the heritability the declaration states,
  so `inherit Attribute()` in a later `Vesper.Core` file stopped resolving. Fixed at the
  extractor (`IntrinsicShape.HeritableClass`); it was never specific to the paired build — a
  consumer package inheriting a contract's `extern class` hit the same wall.
- **Real drift.** `Vesper.Set`'s `SetModule.intersectMany` was inferring
  `(Set<'T> * Set<'T>) -> Set<'T>` against a contract declaring `seq<Set<'T>> -> Set<'T>`,
  because a project-local static member overloaded on arity resolves to its FIRST declaration:
  `resolveMember`'s ranking is reached only for instance calls. The source is fixed; the
  resolution gap is not, and is worth its own plan.

### What it settles — and it weakens the case for merging

- **Justification 3 no longer argues for one list.** `a.fsi, a.fs, b.fsi, b.fs` IS what the
  pipeline now compiles, and a `SourceUnit` carries both halves, so the interleave is a
  property of the unit rather than of the file list. A merged list would only be spelling out
  an order the pairing already derives.
- **Justification 1's disagreement is inert for the build.** The package build now reads
  `impl` order alone — every `.fsi` sits at its `.fs`'s position — so `fun-adapters.fsi` being
  last in `files` and sixth-from-last in `impl` costs the build nothing. `files` order still
  sequences the CONSUMER-facing contract extraction in `ReferencedProject.buildProviderWith`,
  which is a different list serving a different pass; that is the question left, and merging
  would answer it by fiat rather than by test.
- The CST check (`Conformance.checkUnit`) and the typar sweeps in
  `AssemblyFiles.conformanceDiagnostics` now run on every package build, both targets, and are
  clean on the corpus. `ConformanceTyparsTests`'s reversed-`(+)` mutation guard is still the
  only thing separating "conforms" from "skipped" for the typar half; the CST half has no
  equivalent guard.

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
