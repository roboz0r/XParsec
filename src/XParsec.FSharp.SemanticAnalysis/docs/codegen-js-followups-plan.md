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
cache — the table does that — and are baked into the value at `WalkCtx.create`; `buildProgram`
supplies the collected local half through `withLocal`.

`tryLocal` is the second operation: `Members.typeName` mangles off a name only a locally
emitted declaration has, and must not resolve an import to answer.

All four tables are now keyed by `TypeKey`. `nominalKey` returns one instead of widening;
`collectTypes` writes `td.TypeKey`; `enumCaseAccess` and `staticFieldRef` take one. The two
`SymbolKey.Type key` wrappings at the table reads are gone. Three call sites narrow instead,
through a new `SymbolKeyOps.asTypeKey` beside `asMemberKey`, because the TAST's
`StaticFieldGetView.Key` / `StaticFieldSetView.Key` / `EnumCasePatView.EnumKey` are still
`SymbolKey` — always `SymbolKey.Type`, and narrowing them is a `SemanticAnalysis` +
`Codegen.Clr` change out of scope here. Worth a Part C entry: the CLR backend wraps the same
keys at its own `env.Enums` / `env.Classes` reads.

## B2. `EmitJs`'s `ExprShape.New` arm

Three construction strategies are probed as two independent `voption`s and joined by a
precedence `match` that the surviving comments exist to state. One classifier returning a
`NewTarget` DU — the shape `MemberDispatch` already has in this project — makes precedence a
total match and deletes both comments.

## B3. `PartitionedMembers` ↔ `emitCapabilityMethods` state one mapping twice — DONE

Landed with B7 as one type, since `MemberSlot` is a field of B7's row.
`MemberSlot = Named | Iterator | Protocol of registryKey | Dispose | Free` makes
`PartitionedMembers` a single `(MemberSlot * TypeMember) list`; `emitCapabilityMethods` became
`emitClassMethods`, one comprehension matching the slot. `emitAttachedMethod`,
`emitDisposeMethod` and `emitProtocolMethod` — the three wrappers that attracted both
fabricated claims — are gone, and with them the prose that restated the partition.

`Free` is a slot rather than a list beside the slots: it is the only case with no class method,
so `emitClassMethods` skips it and `collectTypes` enrols it as a top-level function.

Emitted method ORDER changed as a fall-out: a class body is now in source order, not grouped
attached-then-iterator-then-protocol-then-disposer. Inert in JS. The committed
`Vesper.List.mjs` was regenerated; its method bodies are byte-identical, only their order moved.

## B4. `TsManifestTypes.structuralKey (hash: string)`

An unenforced contract on a `string` parameter: it MUST be the `structuralHash` interning
string, or a field-order-permuted twin resolves to a second type.

A single-case `StructuralHash` produced only by `structuralHash` deletes the sentence and the
bug class. Both callers already thread exactly that string.

## B5. `TsManifestTypes.mint : … -> string * TypeKey`

Every consumer must independently know the `string` is the qualified name OF that `TypeKey`;
the prose asserted "equals the map key by construction" twice, in two blocks. A record, or
keying by `TypeKey` and rendering at the edge, makes the pairing structural.
`declaredIdentity` and `structuralKey` return the same bare pair and want the same treatment.

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
`{ Anchor: CapabilityIds -> CapabilityIdentity voption; Slot: MemberSlot; Lowering }` — and the
five values are one `capabilities` list. Three matches over the old DU (the anchor if-chain,
the partition's slot routing, the call lowering) became a `List.tryFind` over the rows and two
field reads. `tryCapabilitySlot` is `tryCapabilityLowering`: "slot" now names `MemberSlot`.

`Anchor` is a selector rather than an identity, so the table is a static value and reading it
allocates nothing per member access.

Adding a capability is one row. There is nowhere for the ASCII table to come back to.

## B8. `ClrDriver.compileCachedWith` — `Codegen.Clr`, out of scope here

Recorded so it is not lost. Eleven lines argue that the `digest` parameter covers every input
the `provider` is built from, because nothing enforces it. Folding the digest inside, or
making the provider a projection of the digest, deletes the note. Found during the earlier
code-quality sweep; belongs to the `Codegen.Clr` pass.

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

## The static-field / enum-case TAST views are over-wide — SemanticAnalysis + both backends

Fallen out of B1. `StaticFieldGetView.Key`, `StaticFieldSetView.Key` and
`EnumCasePatView.EnumKey` are `SymbolKey`, and every producer builds `SymbolKey.Type`. The JS
backend now narrows at three call sites; `Codegen.Clr` wraps at each `env.Enums` /
`env.Classes` read instead. Narrowing the three views to `TypeKey` deletes both, but touches
`TastNodeViews`, `FrozenCodec` and the CLR backend, so it belongs to a pass that owns all three.

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
