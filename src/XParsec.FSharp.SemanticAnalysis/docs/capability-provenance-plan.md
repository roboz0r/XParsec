# Where a capability comes from: `.fsi` shadowing and backend satisfaction

Working document. Ephemeral: delete it when the work lands.

Two changes, one defect. Neither is coded yet; the semantic premises marked **CONFIRM** have
to be settled before any of it is.

## The defect

`'T[]` declares `interface Vesper.Collections.seq<'T>` in `prim-types-array.fsi`, and the
unifier reads it off `IntrinsicClassSurface.Interfaces`. That surface exists ONLY on the
`.fsi` extraction route.

The other route publishes an intrinsic from `Residue.IntrinsicReprKeys` — a table the `.fs`
implementation pass fills from `(# "!0[]" #)`. An impl file binds a REPRESENTATION and
nothing else: there is no interface list to carry, so the shape comes back with
`Interfaces = [||]` and the capability is silently absent. `Heritable` is the same story one
step earlier: a bool squeezed through the repr table because the parent nominal could not be.

So the answer to "is `'T[]` a `seq<'T>`?" depends on which provider the question reaches.
That is the defect. Two independent changes remove it.

## Change A — a file is published as SIGNATURES + BODIES, and a `.fsi` hides

A NEW DESIGN, decided but deliberately not attempted alongside the rest. It is bigger than
the defect below it and should land on its own.

Today `analyseAssemblyWith` takes `(path, source)` pairs, all implementations, and pushes
each file's `FrozenSignature.toProvider` view onto `priorViews`. A later file therefore sees
the previous file's IMPLEMENTATION projection, which is the lossy one.

**The rule: `.fsi` HIDES.** A declaration the signature does not publish is not visible
outside its file, as in F#. Not an overlay.

### The shape

When a `.fs` finishes analysis it is converted into TWO objects, not one view:

- **signatures** — the published surface,
- **inline bodies** — the splice templates.

The `IExternalSymbolProvider` the next file sees is built from those two, never from the
`.fs` directly. Separating them is what lets the signature hide without taking the splice
templates with it: a `member inline` stays reachable as a BODY keyed independently of
whether its declaration is published as a SIGNATURE.

When a corresponding `.fsi` exists, the `.fs`-derived signatures are checked against the
`.fsi`-derived signatures for conformance, and then the **`.fsi` signatures REPLACE the
`.fs` ones** before the provider is built. So the provider a later file resolves through is
signatures-from-`.fsi` + bodies-from-`.fs`, and the `.fs` signatures exist only to be
checked and discarded.

### What already exists, and what does not

- **The two-object split is not new — it is how the PACKAGE route already works.**
  `SymbolProviders.collectInlineBodies` keys inline bodies by `SymbolKey` independently of
  any shape, and `ExternalSymbolProviders.withInlineBodies` layers them onto a provider built
  from signatures. The assembly route is the one that publishes a single fused `.fs` view.
  Change A brings the assembly route to the shape the contract route is already in.
- **File pairing exists.** `ConformancePass` pairs a manifest's `.fsi` with its companion
  `.fs` on the manifest's own pairing key (`prim-types-int.js.fs` ↔ `prim-types-int.fsi`),
  and reports a leading-`module`/`namespace` mismatch as a pairing error.
- **Conformance is PARTLY there.** `Conformance.check` is CST-level and compares names and
  shapes only — presence, `extern`↔intrinsic pairing, heritability — but `ConformanceTypars`
  is already SEMANTIC: `checkFile` and `checkMembers` compare typar ORDER over the frozen
  surfaces, and the whole seam is hard-gated into the package build (`V240`–`V243`). So
  "check the `.fs` signatures against the `.fsi` signatures" extends an existing semantic
  check rather than inventing one. What is missing is comparing the signatures THEMSELVES,
  not just their typar axes.
- **`ConformanceTypars` exempts `let inline` by construction, and that exemption is this
  change's problem.** `typar-fsi-fs-faithfulness-plan.md` names it "the last thing standing
  between this class of bug and the compiler", having already had one miscompile from it
  (`InvalidProgramException`, a heterogeneous operator folded into one typar). Making inline
  bodies a first-class PUBLISHED object is exactly where that gap lives, so this change
  should close it rather than inherit it.
- **No driver passes a `.fsi` to the assembly pipeline at all** — see below. Both drivers
  gain that plumbing.

### Consequences to design through

- Hiding is a NEW diagnostic class: a later file naming a declaration its `.fsi` does not
  publish must be told so, and today it silently resolves.
- A `.fs` with no `.fsi` publishes its own signatures unchanged, so the common case is the
  existing behaviour with the fused view split in two.
- `AssemblyFilesTests` is where the rule gets pinned: a type declared in `file1.fs` and
  published by `file1.fsi` resolves from `file2.fs` through the SIGNATURE; one the `.fsi`
  omits does not resolve at all.

### Verified, so not a question

`Vesper.Core` DOES go through `analyseAssemblyWith`, and it passes its manifest `impl` list
only — no `.fsi` reaches the assembly pipeline anywhere today. The signatures arrive by the
separate contract route, which sits at the TAIL of the composition while the per-file `.fs`
views sit at the head. So a later `Vesper.Core` file asking whether `'T[]` is a `seq<'T>`
gets the array's own `.fs` view, whose surface is empty, in preference to the contract's,
which is populated.

Nothing trips it today, because no file in any Vesper package writes `for x in arr` or passes
an array where a `seq` is asked for. The first one to do so fails with no diagnostic pointing
here. So this defect is a LIVE LATENT HOLE that Change B closes; Change A is the general
rule, and stands on its own merits whether or not it is what closes this.

## Change B — SUPERSEDED: this is `intrinsic-capability-representation-plan.md`

**Do not implement this section. It is the same work as an existing plan, which already
decided it — and decided it differently from the sketch below.** Kept only so the reasoning
that led here is not repeated by the next reader.

`intrinsic-capability-representation-plan.md` splits constraint sourcing on two axes and puts
`seq` explicitly on the first:

- **Capability axis** — `Equality`, `Comparison`, and the interface capabilities `disposable`
  / `seq`. Behavioural and TARGET-AGNOSTIC: `int` is comparable on CLR, JS and a GPU alike.
  Declared on the type in the shared `.fsi`, resolved through `FrozenInterfaces`.
- **Representation axis** — value-ness, the null model. Genuinely per-target, and the subject
  of `platform-facts-plan.md`.

So "is `'T[]` a `seq<'T>`?" is a CONTRACT fact under the decided design, not a backend query.
`prim-types-array.fsi`'s `interface seq<'T>` is the answer, and the fix for the hole above is
to make the contract route reach every consumer — which is Change A — not to add a second
source of truth. The JS backend already works this way: `JsNativeSymbols`'s fabricated
`IEnumerable`/`IEnumerator` stubs are gone, and JS "keys iteration by the `seq` capability
alone".

Two further points from those plans that the sketch below got wrong:

- **The query DOES go on `IExternalSymbolProvider`.** `platform-facts-plan.md` originally
  ruled the other way; that bullet is reversed there, with the reasoning. In short: the
  provider being the sole outside-world view of a file under analysis is worth keeping, the
  test-double cost it was traded for does not exist (one double on the whole tree, and
  `KeyIndexedLeaf` is already the shared data-driven one), and the interface already carries
  platform facts in `IntrinsicForwardRepr`.
- **`TypeKey` is the primary key, the repr is secondary.** `IntrinsicIdentity.Platform` is
  many-to-one (JS maps `float` and `float32` both to `number`), so a repr-keyed map — the
  tempting implementation — conflates them.

The work this plan contributes to that one is already done: `IntrinsicClassSurface` now
carries `Interfaces`, and the republish fills it from `FrozenInterfaces`. That is the
down-payment the capability axis needs.

### The superseded sketch

Change A makes the `.fsi` signatures the published surface. It does NOT state the fact that
matters here, which is not a Vesper declaration at all: **the target supplies
`IEnumerable<T>` for `T[]`.** No Vesper code implements it. The `.fsi`'s `interface seq<'T>` is a CLAIM about the platform, and the
component that can substantiate it is the backend.

So `IExternalSymbolProvider` gains the question directly: given an intrinsic identity and a
capability identity, is the capability satisfied, and at what instantiation? The CLR provider
answers from metadata — `T[]` really does implement `IEnumerable<T>`, and reflection says so
without any contract file. The JS provider answers from its own knowledge that an array
carries `Symbol.iterator`. `subtypeInterfacesOf` and `tryForInEnumerator` then ask the
provider rather than reading a frozen field, and the answer no longer depends on which route
published the shape.

This is the direction `feedback_freeze_no_backend_knowledge` already points: freezing must
not know a backend, so the target dialect stays in the backend and the front end asks.

### CONFIRM before coding

- **Does the `.fsi` declaration stay?** Two readings. Either it stays as the TYPE-CHECKED
  claim and the provider is the witness — a mismatch between them being a diagnostic worth
  having — or the provider becomes the only source and the declaration goes, at the cost of
  the capability no longer being visible in the source at all. The first is better: a
  capability a reader cannot see in `prim-types-array.fsi` is a capability nobody knows about.
- **What is the identity a backend is asked about?** The array reaches this code as
  `TyConst(arrayKey 1, [elem])`, and the array is exactly the type whose key has two spellings
  (see `array-key-spelling.md`). That wart should be fixed BEFORE a new provider member is
  keyed on it, or the new member inherits it.

## Order, against the other plans in flight

1. `extern-is-self-evident-plan.md` — already the declared prerequisite for both
   `per-target-manifest-plan.md` and `platform-facts-plan.md`. Small and behaviour-preserving.
2. `array-key-spelling.md` — independent of all of it, and a prerequisite for anything that
   keys a capability query on the array's identity.
3. `per-target-manifest-plan.md` — **before Change A, not after.** It shrinks `pairingKey`
   from "strip a suffix for any target this manifest declares" to "strip `.<myTarget>`", and
   the `.fsi`↔`.fs` pairing is precisely what Change A needs the manifest to hand it. It also
   threads `target` out of `SymbolProviders.inlineBodies`, the function Change A generalises
   to the assembly route. Change A first would mean writing that plumbing against a rule that
   is about to be deleted, then writing it again.
4. `intrinsic-capability-representation-plan.md` (which absorbs Change B) and
   `platform-facts-plan.md` — both gated on step 1, independent of the manifest split.
5. **Change A**, last. Biggest, newest, and the one that benefits from every step above:
   simpler pairing, a target already threaded out, and the capability axis already sourced
   from the contract so the signatures object has something correct to carry.
