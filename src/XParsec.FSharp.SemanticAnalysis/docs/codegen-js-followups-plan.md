# Codegen.Js follow-ups — plan

Working document. Ephemeral: delete it when the work lands.

Line numbers are deliberately absent — they rot. Constructs and file names only.

Raised by the comment overhaul of `XParsec.FSharp.Codegen.Js` (all 20 files, 2544 → 1033
comment lines). Reading every comment against the code it claimed to describe turned up two
kinds of work that the sweep itself could not do, because both change code:

- **Part A — defects.** Two, both now fixed. Neither was a miscompile; A1 was a missed
  optimisation with a stack-overflow consequence, A2 emitted a dangling import.
- **Part B — prose that should be a type.** Eight. Each is a comment that was genuinely
  load-bearing and long, where the durable fix is a type that makes the sentence
  unnecessary. This is the half that stops the regrowth: a fact the compiler enforces cannot
  rot, and there is nothing left to narrate.

Two pairs overlap. **A1 and B6 are the same confusion**, and landing A1 closed B6's JS half.
**B3 and B7 are one type** — `MemberSlot` is a field of B7's capability row — and landed
together.

Nothing here is urgent. Nothing here is blocked on anything else, and no item touches
another's files except those two pairs.

---

# Part A — code defects

## A1. A tupled parameter group forgoes tail-call optimisation — DONE

The predicted symptom (a dead `while (true) { return f(…); }`) turned out not to be
reachable: `emitFlatModuleFn` declined the self-key unless `TastLower.allSimpleGroups
cf.Groups`, so the two counts never met. That gate was the defect — a `let rec` over a tuple
group, or a lone `()` group, got no trampoline at all and recursed on the JS stack, which is
the overflow this entry was after.

Fixed by lowering the write-back through `JsFlatFns.flattenGroupArgs`, which already opens a
saturated call's source arguments onto the flat parameter vector, spilling an impure tuple to
a `_tg` temporary. The gate is gone, and `TastLower.allSimpleGroups` with it — its only
caller.

**Also does B6's work, JS-side.** `TailParams` (`Unary of names | Flat of groups * names`,
with `Arity` and `Names` members) replaces the arity/names pair through `hasTailSelfCall`,
`(|TailSelfCall|_|)`, `trampolineOrExpr` and `buildTailBody`, so there is one arity in the
system and no recomputation. It does NOT cover Part C's `Codegen.Clr` `EmitCall` finding,
which needs a shared value below both backends.

Two tests in `FunctionEmissionTests`: the emitted body for a tupled group (both flat params
written back, then `continue`), and a 60000-deep tupled tail recursion under Node.

## A2. `JsRuntime.assets` takes no transitive closure — DONE

Assets import each other — `Vesper.Seq.mjs` imports `Vesper.Array.mjs` and `Vesper.Core.mjs`
— but `assets` returned only the DIRECTLY referenced entries, and `JsDriver.checkResolvable`
validated only the EMITTED modules' `ImportedModules` against the written set. Neither closed
over asset→asset edges, so a program that reached `Vesper.Seq` without independently reaching
`Vesper.Core` wrote a dangling ESM specifier that failed when Node loaded it.

Fixed by carrying the edges on the asset: `JsRuntimeModule` gains `Imports: JsModulePath list`,
which `JsRuntimeModule.ofSource` (now the only constructor, production and test alike) reads
off the asset's own text. `JsModulePath.tryOfRootSpecifier` — the inverse of
`specifierFrom ValueNone` — maps each `./X.mjs` back to the module it names and drops a bare
specifier, which is the host's to resolve. `assets` walks that graph and returns the closure;
an edge naming a module no referenced package ships throws there rather than at load time.
`checkResolvable` validates the written assets' imports alongside the emitted modules'.

`RuntimeAssetTests`: the scan against the committed `Vesper.Seq.mjs`, a bare specifier and a
specifier-shaped string value excluded, the closure and its fault over a synthetic asset
graph, and a program that names only `Vesper.Seq.mjs` yet ships all three and runs under Node.

Worth noting for calibration: a comment asserting each asset was "a self-contained leaf … no
transitive closure needed" dated to `0f8e2f84` (2026-08-04), the cross-asset imports it denies
to `2994a340` (2026-08-03). It was false the day it was written, and it is the sentence that
had been reassuring readers this hole did not exist.

---

# Part B — prose that should be a type

Each entry names the comment it deletes. That is the acceptance test: if the refactor lands
and the sentence would still need writing, the refactor was the wrong shape.

## B1. `EmitJsContext.WalkCtx`'s six `Dictionary<SymbolKey, _>` fields — DONE

`LocalThenExternal<'Info>` (`Local` / `External` / `Resolve`) replaced the `Records`+
`ExternalRecords` and `Unions`+`ExternalUnions` pairs, so six fields are four and the
fall-back is `tryFind` rather than a sentence. `resolveExternalRecord` / `resolveExternalUnion`
became the private `externalRecord` / `externalUnion`, provider-only functions that no longer
cache — the table does that. `create` takes the collected local half, so the table is whole
when it is minted; the `importedAs` both external resolvers share mints the imported name and
home once.

`tryLocal` is the second operation: `Members.typeName` mangles off a name only a locally
emitted declaration has, and must not resolve an import to answer.

All four tables are now keyed by `TypeKey`, and so is everything upstream of them: `FTConst` /
`TyConst` carry a `TypeKey` like their `FTClass` / `FTRecord` siblings, the `RuntimeNames`
primitive identities are `TypeKey`s, `TryLookupType` / `TryLookupMembers` /
`TryLookupIndexSignature` take one, and the `StaticFieldGet` / `StaticFieldSet` / `EnumCase`
TAST payloads carry one. That took `SymbolKey.Type` wrappings across `src` from 165 to 24 and
left no narrowing cast anywhere: the codec writes the narrow key through `writeTypeKeyRef`.

## B2. `EmitJs`'s `ExprShape.New` arm — DONE

`NewTarget` (`LocalClass | GlobalClass | ExnRepr`) and `EmitJsContext.tryNewTarget` replaced the
two independent `voption`s and the precedence `match` that joined them. Precedence is now the
classifier's top-down probe order — local class, then ambient, then `exn` repr — and the emit arm
is a total match on the result, so both comments are gone: each names a case instead.
`TastLower.objArgShape` is called once rather than twice.

The three-line header comment above the arm went with them; it described only the `exn` strategy,
which is now the `ExnRepr` case's own doc. What survives in the arm is one line on why `Error`
takes just the leading argument — a fact about the emission, not about the classification.

## B3. `PartitionedMembers` ↔ `emitCapabilityMethods` state one mapping twice — DONE

Landed with B7 as one type, since `MemberSlot` is a field of B7's row.
`MemberSlot = Named | Iterator | Protocol of registryKey | Dispose` makes `PartitionedMembers`
`{ Slotted: (MemberSlot * TypeMember) list; Free: TypeMember list }`; `emitCapabilityMethods`
became `emitClassMethods`, one comprehension matching the slot. `emitAttachedMethod`,
`emitDisposeMethod` and `emitProtocolMethod` — the three wrappers that attracted both
fabricated claims — are gone, and with them the prose that restated the partition.

Every case of `MemberSlot` IS a class dispatch slot, so `JsCapability.Slot` cannot spell a
non-slot, and neither consumer filters: `emitClassMethods` reads `Slotted`, `collectTypes`
reads `Free`.

Emitted method ORDER changed as a fall-out: a class body is now in source order, not grouped
attached-then-iterator-then-protocol-then-disposer. Inert in JS. The committed
`Vesper.List.mjs` was regenerated; its method bodies are byte-identical, only their order moved.

## B4. `TsManifestTypes.structuralKey (hash: string)` — DONE

`StructuralHash = private StructuralHash of string`, minted only by `structuralHash` and
opaque outside the module, replaced the `string`. `structuralKey` and `structuralIndexSigsIn`
take/return it, so the "MUST be the interning string" sentence is unstatable and the
field-order-permuted twin is unconstructible. A private `hashText` unwraps it at the three
in-module sites that want the characters (`shapeHash`'s recursion, `FTUnknown`, `mint`).

## B5. `TsManifestTypes.mint : … -> string * TypeKey` — DONE

`MintedType = { QualifiedName: string; Key: TypeKey }` replaced the bare pair, returned by
`mint`, `declaredIdentity` and `structuralKey` alike. `TypeIdentity` now holds the whole
`MintedType` rather than the `Key` alone, so `declaredIdentity` hands back the value
`buildCtx` minted instead of re-pairing a freshly computed name with a stored key — the
pairing is the same object, not an asserted invariant. Both "equals the map key by
construction" blocks are gone.

## B6. `EmitJs.trampolineOrExpr`'s `arity` — DONE JS-side by A1

Three lines existed only to say `arity` is the SOURCE-group count and NOT `names.Length`,
because both are bare (`int`, `string list`) and nothing ties them. `TailParams` now carries
the groups and the flat names together; the note is gone.

What remains is Part C's move: `Codegen.Clr`'s `EmitCall` indexes the flat parameter list
with a source-group index, and `TailParams` is trampoline-shaped, so it does not serve that
call site. A shared value in `Codegen.Common` is still wanted.

## B7. `EmitJsCapabilities`'s capability table — DONE

The 42-line module header's centre was an ASCII table mapping capability → JS anchor →
implemented shape → call lowering: four fields per row, in prose, that two code tables read
off and restate.

The `JsCapability` DU is gone. A `JsCapability` RECORD replaced it —
`{ Anchor: CapabilityIds -> CapabilityIdentity voption; Slot: MemberSlot }` — and the five
values are one `capabilities` array. Three matches over the old DU (the anchor if-chain, the
partition's slot routing, the call lowering) became a scan of the rows and one field read.
`tryCapabilitySlot` is `tryCapabilityLowering`: "slot" now names `MemberSlot`.

A call's lowering is NOT a second column: it must reach the slot an impl was emitted into, so
`loweringFor : MemberSlot -> CapabilityLowering voption` derives it and an incoherent pairing
is unspellable. `Anchor` is a selector, so the table is a static value; `capabilityOf` scans it
with an index rather than a predicate closure, allocating nothing per member access.

Adding a capability is one row. There is nowhere for the ASCII table to come back to.

## B8. `ClrDriver.compileCachedWith` — DONE

The same item as `codegen-clr-followups-plan.md` B19, landed with it. `PreparedCompilation`
(private representation, minted only by `ClrDriver.prepare`) carries the digest and the
provider built from one `ClrCompilation`, and `compileCachedWith` takes it instead of a digest
plus the inputs — so a digest folded from other inputs is unspellable and the clause saying to
fold it from THESE `inputs` is gone. The triplicated `buildContractWithRefs` call became a
private `contractFor`, so a compilation resolves its contract one way.

`compileCached` prepares inline, as it folded the digest inline before, and is `prepare`'s only
caller today: the multi-file driver that would prepare once and share it across files does not
exist yet. The type earns its place on the invariant alone — the mismatch it made unspellable —
not on a throughput win anything currently collects.

---

# Part C — carried into the remaining comment sweeps

The overhaul runs `Codegen.Js` → `Codegen.Common` → `Codegen.Clr` → `SemanticAnalysis`. These
were spotted from `Codegen.Js` and belong to a later pass; recorded here so deleting the
`Codegen.Js` plan does not lose them.

**The `Codegen.Clr` sweep is under way** — 14 of 47 files done — and its findings are tracked
in `codegen-clr-followups-plan.md`. Every item this list carried forward has been cleared by
that sweep: the `flattenGroupArgs` clone twin, the `RefCellPromotion` clone, and the `Step B`
/ `Step C` milestone labels are all gone from `Codegen.Clr`, none of them pointed at.

One finding there belongs to THIS list rather than that one. `EmitCall` indexes the flat
parameter list with a source-group index — **the same confusion as A1's trampoline, arrived
at independently in the other backend**, and also defended by a comment claiming the two
counts cannot diverge. A1's `TailParams` is trampoline-shaped and JS-local, so it does not
reach that call site: the CLR half still wants a value in **`Codegen.Common`** pairing the
groups with the flat vector. See `codegen-clr-followups-plan.md` A1/B1.

## Diagnostic STRINGS carry the H19 causal hedge — one decision, three projects

The H19 pass over `Codegen.Js` (an em-dash standing in for the connective the code
determines: `because`, `so`, `but`, `namely`) stopped at the comment/string boundary by
design — a punctuation sweep must not silently edit user-facing text. Seventeen
`failwithf`/`failwith` messages across the three codegen projects carry the same defect:

- `Codegen.Js` — 2: `EmitJs`'s `use`-with-no-resolved-disposal, `EmitJsContext`'s
  unpublished-origin-file.
- `Codegen.Common` — 2, both in `InlineExpand`.
- `Codegen.Clr` — 13: `ClrEncoder` ×5, `Layout` ×4, and one each in `ClrEnv`,
  `ClosureVerdictRewrite`, `EmitBindings`, `NominalEmit`.

**Do not fix the `Codegen.Js` two on their own.** `EmitJs`'s and
`Codegen.Clr/EmitBindings`'s `use`-disposal messages are a deliberately parallel pair, the
same sentence in the two backends down to the trailing clause; editing one desynchronises
them. That is what makes this one decision rather than a per-project tidy-up.

No test asserts on any of the seventeen (checked across `test/`), so the edit is mechanically
safe. The open question is whether user-facing diagnostic prose is held to the comment rule
at all — a message is read by someone who has just hit a compiler failure and is the one
place where an unnamed relation costs the most. Decide once, then apply across the three
projects during the `Codegen.Common` / `Codegen.Clr` sweeps.

`Codegen.Clr/ClrRecipes.fs` still re-narrates `CallArity`'s flat-vs-grouped divergence in
seven lines, a near-verbatim clone of an essay already cut from `ICodegenProvider.fs`, where
the fact now lives sited on the `Grouped` case and the `FlatArgCount` member. It goes when
that file is swept.

The method is in `.claude/skills/comment-hygiene`. Read it before starting the next project;
the sweep procedure, the two-stage verification gate and the batching that made
parallelisation safe are all there rather than here.

---

## Done when

- Part A: each defect has a fix and, where noted, a test that would have caught it.
- Part B: each entry's named comment is gone because it has become unstatable.
- This file is deleted.
