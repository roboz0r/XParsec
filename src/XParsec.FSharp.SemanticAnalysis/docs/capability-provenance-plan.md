# Where a capability comes from: `.fsi` shadowing and backend satisfaction

Working document. Ephemeral: delete it when the work lands.

Two changes, one defect. Neither is coded yet.

**Change B is the one that removes the defect, and it is the decided direction** (user,
confirmed 2026-08-12): the provider answers the capability query. Change A is a separate,
larger rule about `.fsi` hiding that stands on its own merits. An earlier revision had these
the other way round — Change B marked "SUPERSEDED, do not implement" and Change A named as the
fix — which is why the section below is written as a rebuttal of its own former status.

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
That is the defect, and it is a defect of asking a FROZEN FIELD a question the PLATFORM owns
the answer to. Change B removes it at that root.

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
here.

**CLOSED (2026-08-12), independently of Change B.** `ExternalSymbolProviders.stack` now FOLDS
`IntrinsicClassSurface` across sources rather than taking the nearest: an impl `.fs` view
publishes an empty surface because it binds a representation and knows nothing else, which is
abstention, not an answer. Every reader of the surface — `subtypeInterfacesOf`,
`tryForInEnumerator`, `tryUpcastWitness`, the constraint solver — gets the contract's answer
through the ordinary shape lookup, so `'T[]` is a `seq<'T>` whatever the route. Change B is
still the end goal for provenance generally; it no longer has this hole to close.

## Change B — THE END GOAL (relitigated and reinstated 2026-08-12)

**The route-dependence this section was going to fix is already gone** — see "Verified, so not
a question" above: the composite folds intrinsic surfaces, so no reader of
`IntrinsicClassSurface.Interfaces` depends on which leaf answers first. What remains here is
provenance proper: which FILE a declaration came from, and hiding.

**Status: this is the direction (user, confirmed 2026-08-12). Do not add features that move
away from it.** This section carried a "SUPERSEDED — do not implement" marker for two days,
deferring to `intrinsic-capability-representation-plan.md`. That deference was never earned:
the doc it deferred to was three weeks OLDER, and when its premises were checked (2026-08-12)
three of them were stale — it has since been absorbed into `platform-facts-plan.md`. The
sketch below is reinstated as the target design.

**The provider answers the capability query** — given an intrinsic identity and a capability
identity, is the capability satisfied and at what instantiation. `subtypeInterfacesOf` and
`tryForInEnumerator` ask that question instead of reading a frozen field, so the answer stops
depending on which route published the shape.

**The contract PRESCRIBES the answer** (user, 2026-08-12). This is the correction to an earlier
revision of this section, which had the CLR provider deriving an intrinsic's capabilities from
reflection and the JS provider from hardcoded dialect knowledge. That is wrong, and the reason
is portability, not layering.

### The contract is a LOWER BOUND, and the platform may widen it

`int` is equatable and comparable because **the language prescribes it**, not because
`System.Int32` happens to implement `IEquatable<Int32>`. Those facts belong in the shared
`Vesper.Core` `.fsi`. Likewise `prim-types-array.fsi` declares `interface seq<'T>` and is
silent on the rest.

**That silence is a floor, not a ceiling** (user, 2026-08-12). `Int32[].GetInterfaces()`
returns ten interfaces — `IList\`1`, `ICollection\`1`, `IReadOnlyList\`1`,
`IStructuralComparable`, `ICloneable`, … — and on a CLR build a Vesper array SHOULD be passable
to a BCL method expecting `IList<'T>`. Refusing that to protect portability would be a bad
trade: it makes the common case inconvenient to buy a guarantee the build system already
provides. **Target incompatibility is discovered by building against the target**, exactly as
it is for `when 'T : struct` — legal to write, diagnosed on the target that cannot honour it.

So the two sources are ADDITIVE:

- **contract** — the portable floor every target must supply (`seq<'T>` on arrays; equatable /
  comparable on `int`). Prescribed, target-agnostic, visible in the `.fsi`.
- **platform** — whatever this target actually provides on top. Genuinely reflection-derived on
  CLR, and correctly so.

### So what is the backend for? Widening, and witnessing the floor

The platform leaf has two jobs, and only the first is new capability CONTENT:

1. **Widen.** Surface the target's own capabilities for an intrinsic, so CLR code can use a
   Vesper array as an `IList<'T>`. This is the reflection answer, and it is legitimate.
2. **Witness the floor.** `contract ⊆ platform` — a target that cannot deliver a PRESCRIBED
   capability is a broken target, which is a diagnostic worth having. Checkable on CLR;
   on JS there is nothing to check against, which is fine.

This keeps `feedback_freeze_no_backend_knowledge` satisfied: freezing knows no backend, the
backend owns its own dialect, and the language owns the floor.

### The mechanism — a dedicated member so the lossy leaf ABSTAINS

If the contract prescribes the answer, why does the defect exist at all? Because of HOW the
question is currently asked, and this is the whole of the fix.

`TryLookupType` is **first-hit at whole-shape granularity** — `stack`'s `firstHit` scans leaves
from index 0 and stops at the first `ValueSome`, returning that leaf's `ExternalTypeShape`
entire (`ExternalSymbolProviders.fs:239-253`, `:330-331`). Composition puts per-file `.fs`
views at the HEAD and the contract + metaTail at the TAIL (`AssemblyFiles.fs:108-109`,
`ClrDriver.fs:141-142`, `ReferencedProject.fs:563-564`). So for `'T[]` the array's own
`prim-types-array.fs` view wins, and its `Interfaces = EqArray.empty` is read as **"no
interfaces"** when the truth is **"this leaf does not know"**. The contract's populated shape,
sitting further down, is never consulted.

**A dedicated ADDITIVE member fixes this, because abstention becomes the default.**
`FrozenSignature.toProvider` builds its leaf as a record update over `KeyIndexedLeaf.empty`
(`FrozenSignature.fs:496-522`). A new field defaulting to "no opinion" in `KeyIndexedLeaf.empty`
is inherited there **without editing that call site** — F#'s `with`-syntax makes silence the
default. The per-file view then says nothing rather than lying, and the contract leaf's answer
survives to the caller.

**The lower-bound semantics and the abstention fix are the SAME requirement.** Contract-floor
plus platform-widening means the query must UNION across leaves rather than stop at the first —
the contract leaf contributes `seq<'T>`, the CLR leaf contributes `IList<'T>` and the rest, and
a caller asking "is this assignable to `IList<int>`?" needs both to have been consulted. An
additive merge delivers that AND makes an abstaining leaf contribute the identity element. One
mechanism, both problems; there is no version of this design where first-hit is right.

**Precedent, already load-bearing in this tree: `AmbientOpenPrefixes`.** The per-file view
abstains explicitly — `AmbientOpenPrefixes = []` at `FrozenSignature.fs:517-519`, with a sited
comment saying a frozen impl file publishes no `[<AutoOpen>]` surface — and `collectAmbient`
(`ExternalSymbolProviders.fs:375-380`) concatenates over ALL sources rather than taking the
first, so the contract's prelude prefixes reach the front of a self-host compile through N
abstaining views. That is exactly the shape to copy. `TryRecordsWithField` is the second
precedent, and its sited comment states the principle outright: *"UNION, not first-hit-wins …
so a later source's records add rather than being shadowed"* (`:317-319`).

**Consequence: this needs neither Change A nor a new frozen fact channel.** The route-dependence
was never inherent to contract-sourcing; it was an artefact of asking a whole-shape first-hit
question.

#### Two traps to avoid when building it

1. **Plumb the member THROUGH the leaf record; do not derive it in `ofKeyedLeaf`.** That
   function already derives channels from others — `TryLookupMemberByKey` from
   `TypeMembersByKey` (`ExternalSymbolProviders.fs:178-180`), `TryLookupIndexSignature` /
   `TryLookupByKey` by re-rendering keys onto the name index (`:182-189`). Implementing the
   capability query there as "look up `ShapesByKey`, read `Intrinsic.Class.Interfaces`, answer"
   would force the per-file view to answer, and it would answer empty — reproducing the exact
   defect through a new door. This is the live trap; the interface's current shape invites it.
2. **A single `voption` cannot carry a union.** Under additive merge the natural return is the
   set of satisfied capabilities with their instantiations, not one hit. Note also that
   `voption` conflates "absent" with "no opinion": decide whether a leaf ever needs to refute a
   capability (a target contradicting a stale contract claim) before fixing the shape. Fold or
   first-hit, `voption` has that conflation.

### Feasibility of the platform half

None of this gates the prescribed floor, which the contract supplies:

- **CLR widening and witnessing.** `MetadataLoadContext` gives `Int32[].GetInterfaces()` the
  full set, and does so for an OPEN element (`T.MakeArrayType()`), so the surface can be built
  over a typar — no concrete element needed. But `TryLookupType` projects a key to a name
  (`MetadataSymbols.fs:681-683`) and `Vesper.[]` resolves to null, so `buildClassInterfaces`
  never runs for an array today; this is a net-new entry point. `computeType` also reports
  arity 0 for `Int32[]` (`IsGenericType` is false for arrays), so it cannot serve as-is. For
  primitives the repr→metadata bridge already exists and is production-proven for MEMBERS
  (`EngineCore.fs:450-456`, `:469-478` → `InferRecordAccess.fs:35,64`, how `"hello".TryCopyTo`
  reaches `System.String`).
- **Reconciling names.** Metadata returns `FTClass(System.Collections.Generic.IEnumerable\`1,
  …)` while the capability anchor is `Vesper.Collections.seq`, so the CLR contribution goes
  through `sameNominalKey` / `capabilityCanonKey` (`EngineCore.fs:403-428`) — otherwise the
  platform would double-report the floor under a different name instead of widening past it.
- **JS needs nothing for the floor.** `JsNativeSymbols.fs:63` is one `Error` class with no
  interfaces, and that is fine: JS gets `seq` from the shared contract plus
  `capabilities-compat.js.fsi:11`, as it does today. JS-side widening can stay empty until
  there is a JS capability worth surfacing.

### Do NOT build these — they move away from the goal

- **A stamped capability verdict frozen onto the intrinsic identity.** Tempting, because
  nominals already work this way (`IInterfaceImplHost.EqualitySupport`, stamped at
  registration, `TypeRegistration.fs:531-553`) and `IntrinsicAbbrevInfo` already has the slot
  as a hardcoded constant (`TypeInfos.fs:253-254`). It is rejected: it is a NEW frozen fact
  channel, which entrenches exactly the field-reading path the provider query removes. It does
  not even dodge Change A — the producer (`TypeRegistration.fs:812-820`) fires on the `.fs`,
  which declares no interfaces, so it would need Change A anyway or a second declaration.
- **Widening `subtypeInterfacesOf` to read more frozen fields.** Same reason.

### Known cost — `subtypeInterfacesOf` is the hard half

`tryForInEnumerator`'s `TyConst` arm (`InferControlFlow.fs:592-600`) is an exact fit: it
already reduces the interface list to "is Enumerable satisfied, at what instantiation" via
`pickEnumerableElem`. ~8 lines, `ctx.Provider` already in hand.

`subtypeInterfacesOf` (`EngineCore.fs:532-561`) is not. It returns the whole outgoing edge
set, and both callers need that: `tryUpcastWitness` (`:590`) recurses through each surfaced
interface to reach transitive bases, and `tryExternalInheritedMember` (`:615`) searches
members across all supertypes. A capability-keyed yes/no cannot serve either. So the provider
needs an ENUMERATE-interfaces-of-an-intrinsic query as well as the capability query — still
provider-answered, but it is a second member, and rerouting `tryUpcastWitness` is a rewrite of
the walk rather than a swap.

### Prerequisite — the zero-leaf case, and where those tests go — **DONE (2026-08-12)**

`noMetaTail` returns `[]` (`ReferencedProject.fs:490-494`) and
`SemanticAnalysis.Tests/TestHelpers.fs:27` uses it, so the entire SA front-end suite runs with
NO platform provider.

**Decided (user, 2026-08-12): SA having no platform is CORRECT, and stays.** Do not give SA a
synthetic platform leaf to keep those tests running — a test-only stand-in for the real
provider is the shape of thing this tree keeps deleting
(`feedback_mockbuiltins_is_a_trap`). As the hardcoded types move out of SA, some tests become
difficult or impossible to construct there. Each one goes one of two ways:

1. **Restated** so it does not require platform types at all — most SA tests are about
   resolution, scoping, inference structure, and can be written over project-local types.
2. **Moved to `XParsec.FSharp.Codegen.Common.Tests`**, where BOTH backends run it against
   their REAL providers.

The second is a fit rather than a workaround. That project's `Conformance` harness is already
parameterised over the one thing that differs between targets — a `Backend` record of
`{ Name; CompileAndRun; Diagnostics }` (`Conformance.fs:37-49`) — with `clrBackend` and
`jsBackend` supplying real compilations
(`Codegen.Clr.Tests/ConformanceTests.fs:13`, `Codegen.Js.Tests/ConformanceTests.fs:10`).
A constraint verdict that differs by target is exactly `Obligation.Diagnose` (`:60`), a
compile-time rejection matched on a diagnostic fragment, and the per-backend `Obligations` map
lets one program say CLR runs it and JS rejects it. So `when 'T : struct` — legal to declare,
erroring at the use site on JS — is expressible as one corpus program with two obligations,
judged against goldens no backend can influence.

That also means the capability work should not try to keep SA green by weakening what it
asserts. If an SA test can only pass with a platform, it was never an SA test.

#### What the sweep actually found — the set is ONE axis, not the whole constraint table

The relocation is done, and it is much smaller than "every SA test at once" above. That
sentence was written when the platform was to DERIVE the capability verdicts; the "contract
PRESCRIBES the floor" correction retired it, and the two claims were never reconciled. SA
composes the real `src/Vesper.*` contract (`TestHelpers.fs:23-27`), so SA HAS the floor. What
it lacks is only what no contract can state.

- **Equality / comparison — NOT relocated.** Contract-prescribed, so SA answers them the moment
  step 1 authors `interface equatable<int>` and friends. Nine `ConstraintsTests` sites and
  `UnificationUnionsTests`'s `equality on (int | string) is Satisfied` currently reach the
  verdict through the primitive table and would go silent without it, but they come back
  through the contract rather than moving. Do not touch them in step 1 beyond watching them
  stay green.

  The sweep did add `ops/equality-primitives.fs` and `ops/comparison-primitives.fs`, which is
  NET-NEW coverage and not part of the relocation: nothing in the corpus ran `=` or `<` at a
  primitive before, and the two backends do not lower them alike (JS emits `===` at int but a
  `structuralEquals` call at string). That is operator behaviour, so it lives in `ops/` with
  the other operator programs, and carries no `width` — the support matrix is about
  `+ - * / %`.
- **The array's `seq<'T>` — NOT relocated**, for the same reason.
  `SignatureExtractorTests`'s intrinsic-surface assertion and `UnificationClassesTests`'s
  `an int[] argument subsumes to a seq<int> parameter` read a declaration the contract
  carries, which SA composes.
- **`struct` / `not struct` — RELOCATED.** This is the whole of it. Value-ness is the one axis
  a shared `.fsi` cannot state, because the decided semantics are that the two targets
  DISAGREE about it. Four `ConstraintsTests` cases over `int` / `string` became four programs
  in `test/Codegen.Conformance/constraints/`, and SA kept the target-invariant pair over a
  function shape.
- **Nullness / not-null — nothing to relocate.** Those arms of the primitive table have zero
  test coverage anywhere, so step 2 writes new corpus programs rather than moving old ones.

The corpus rows record TODAY's verdicts, so steps 2–3 must flip them in the open:
`typar-struct.fs` goes `accept = ["clr", "js"]` → `accept = ["clr"]` + a `js` rejection, and
`typar-not-struct-violated.fs` flips the other way. That prediction lives HERE and nowhere
else — a manifest row or a program header that forecast its own next edit would have to be
found and rewritten alongside the row itself.

**A polarity asymmetry the corpus cannot fix, and step 1 should not pretend otherwise.** A
REFUSAL is a compile error, so `Obligation.Diagnose` pins it exactly. SATISFACTION is not
pinnable that way: nothing reports a leftover deferred constraint, so a front end that simply
stopped answering compiles as clean as one that answered yes. Exercising the capability is the
usual escape — print a result the verdict is a precondition for — and it is available for
equality and comparison but NOT for value-ness, which no program can observe. So the two
`struct` / `not struct` rows pin only "the front end did not refuse this", and are worth keeping
for that alone: they are what forces steps 2–3 to change a recorded verdict rather than land
silently. That is what `Obligation.Accept` is — the corpus obligation for exactly this state,
carrying no `.expected`, so the row does not have to borrow `run` and invent an output contract
to say it. The same gap is why the nine equality/comparison SA sites listed above are weak
assertions TODAY, not only after step 1.

**Still outstanding, and NOT part of this: `Regions.isNonAllocatingPrimitive` (step 4).**
`RegionsTests`'s `pure arithmetic has no escape entry` reads that predicate, and when step 4
makes it backend-answered SA will have no answer. It cannot go to this corpus — a region is
not observable in a program's stdout — so it goes to the per-backend suites instead.

### Settled points carried forward

- **The query goes ON `IExternalSymbolProvider`**, not a second interface. The provider being
  the sole outside-world view of a file under analysis is worth keeping; the test-double cost
  it was traded for does not exist (one double on the whole tree — `MemoizeTests.fs:53` — and
  `KeyIndexedLeaf` is the shared data-driven one); and the interface already carries platform
  facts in `IntrinsicForwardRepr`. Cost is 7 forwarding arms, mechanical and compiler-checked.
- **`TypeKey` is the primary key, the repr is secondary.** `IntrinsicIdentity.Platform` is
  many-to-one (JS maps `float` and `float32` both to `number`), so a repr-keyed map — the
  tempting implementation — conflates them.
- **What identity is a backend asked about?** The array reaches this code as
  `TyConst(arrayKey 1, [elem])`, now its only spelling, so a provider member may key on it
  directly. Reconciling the CLR answer needs `sameNominalKey` / `capabilityCanonKey`
  (`EngineCore.fs:403-428`), since metadata returns
  `FTClass(System.Collections.Generic.IEnumerable\`1, …)` while the capability anchor is
  `Vesper.Collections.seq`.

*(The two CONFIRM questions this section once carried are settled and folded in above: the
declaration stays as the claim with the provider as witness, and the array is keyed on
`arrayKey 1` directly.)*

## Order, against the other plans in flight

The manifest split has since landed, so its step is struck. The rest stands as written —
Change A last.

*(A 2026-08-12 revision briefly moved Change A FIRST, on the grounds that the frozen route
drops `Interfaces` and so gates the capability axis. That gate is an artefact of reading the
verdict off a frozen field, and it disappears under the provider query Change B reinstates:
the platform leaf is injected per target, not derived from the file under analysis. The
reversal is withdrawn.)*

0. ~~**Relocate the SA tests that need a platform.**~~ **DONE (2026-08-12)** — and it was the
   `struct` / `not struct` axis alone, not "the larger half of step 1" this entry predicted.
   See the sub-section above for why the equality/comparison and array-`seq` tests stay in SA.
1. `platform-facts-plan.md` step 1 (which absorbs Change B) — the capability query on the
   provider, then eq/cmp routed to it. Independent of Change A.
2. `platform-facts-plan.md` steps 2–4 — the representation axis and `Regions`. Step 4 is
   independent of everything and is the reasonable place to prototype the query shape.
3. **Change A**, last. Biggest, newest, and it no longer has anything waiting on it: with the
   capability verdict provider-answered, Change A is about `.fsi` HIDING and the
   signatures/bodies split on their own merits, not about plugging the capability hole.
