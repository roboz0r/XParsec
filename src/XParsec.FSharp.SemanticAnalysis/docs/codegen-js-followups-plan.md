# Codegen.Js follow-ups — plan

Working document. Ephemeral: delete it when the work lands.

Line numbers are deliberately absent — they rot. Constructs and file names only.

Raised by the comment overhaul of `XParsec.FSharp.Codegen.Js` (all 20 files, 2544 → 1033
comment lines). Reading every comment against the code it claimed to describe turned up two
kinds of work that the sweep itself could not do, because both change code:

- **Part A — defects.** Four. One is a real miscompilation-adjacent bug; the rest are traps
  and dead weight.
- **Part B — prose that should be a type.** Eight. Each is a comment that was genuinely
  load-bearing and long, where the durable fix is a type that makes the sentence
  unnecessary. This is the half that stops the regrowth: a fact the compiler enforces cannot
  rot, and there is nothing left to narrate.

The two overlap once: **A3 is fixed by B6**, not by prose.

Nothing here is urgent. Nothing here is blocked on anything else, and no item touches
another's files except A3/B6.

---

# Part A — code defects

## A1. The self-tail-call trampoline is entered on one arity and rewritten on another

`EmitJs.trampolineOrExpr` decides whether to open a trampoline with `hasTailSelfCall k arity
body`, where `arity` is the SOURCE-group count — `emitFlatModuleFn` passes `List.length
cf.Groups`. It then hands the rewrite to `buildTailBody ctx k names body`, which recomputes
`let arity = List.length paramNames` from the FLAT names.

For a flat module function with a tuple group the two disagree. `let rec f (a, b) c` is 2
groups and 3 flat params, so the detector opens a `while (true)` trampoline that the rewriter
then never matches, and the body emits:

```js
while (true) { return f(…); }
```

Not a miscompile — it returns on the first iteration, so the result is correct — but the tail
call is NOT optimised, and a dead loop wraps every such function. Anything that recurses
deeply through a tupled first group will still blow the JS stack despite being written for
TCO.

Fixed by **B6**: one `FlatParams` value carrying both counts removes the possibility of
passing one where the other is meant. A direct patch (thread the same `arity` into
`buildTailBody`) works too and is smaller, but leaves the two bare `int`s free to diverge
again.

Wants a test: a `let rec` with a tupled group, asserting the emitted body contains a
`continue`, not a `return` inside a `while`.

## A2. `JsRuntime.assets` takes no transitive closure

Assets import each other — `Vesper.Seq.mjs` imports `Vesper.Array.mjs` and `Vesper.Core.mjs`
— but `assets` returns only the DIRECTLY referenced entries, and `JsDriver.checkResolvable`
validates only the EMITTED modules' `ImportedModules` against the written set. Neither closes
over asset→asset edges.

So a program that reaches `Vesper.Seq` without independently reaching `Vesper.Core` writes a
dangling ESM specifier. It fails when Node loads it, not at compile time.

Two fixes, and they are complementary rather than alternative: close `assets` over the
imports of the assets it selects, and extend `checkResolvable` to validate written assets'
imports as well as emitted modules'. The second turns any future gap of this shape into a
compile-time error.

A comment asserting each asset is "a self-contained leaf … no transitive closure needed" was
FALSE and has already been corrected in the sweep to state the limitation instead — the code
is unchanged.

Worth noting for calibration: `git log -S` dates that false claim to `0f8e2f84`
(2026-08-04), and the cross-asset imports it denies to `2994a340` (2026-08-03). It was false
the day it was written, and it is the sentence that had been reassuring readers this hole did
not exist.

## A3. `JsEscape.quoted` is one escape set serving two grammars, and nothing says so

`JsPrint` uses it for JS string literals; `JsSourceMap.build` uses it for the `.map` JSON.

It is correct TODAY: every control character below `0x20` goes to `\uXXXX`, and only `"` and
`\` need escaping above it, which both grammars share. The hazard is a future edit. `\v`
(0x0B) and `\0` (0x00) are valid JS escapes and INVALID JSON, so adding either as a JS
convenience silently produces a malformed source map — noticed only by a debugger, and only
by whoever is using one.

Either a JS-only and a JSON-only entry point over a shared core, or a test asserting the
emitted `.map` parses as JSON. The test is cheaper and catches the whole class.

Two lesser gaps in the same function, neither currently reachable in a way that matters:

- a lone surrogate passes through raw, which is invalid UTF-8 in `sourcesContent`;
- U+2028 / U+2029 pass through raw — fine on ES2019+, a syntax error in older engines.

---

# Part B — prose that should be a type

Each entry names the comment it deletes. That is the acceptance test: if the refactor lands
and the sentence would still need writing, the refactor was the wrong shape.

## B1. `EmitJsContext.WalkCtx`'s six `Dictionary<SymbolKey, _>` fields

Two structural facts were narrated in prose across several blocks: which key kind each table
takes, and that each `External*` table is the fall-back consulted on a miss in its local
twin.

A `LocalThenExternal<'Info>` — the two dictionaries plus the resolver, with miss-then-provider
as its one operation — makes the pairing visible instead of asserted, for both records and
unions.

Narrower half of the same finding, and independently worth doing: `Classes` / `Enums` /
`Records` / `Unions` are keyed by a `SymbolKey` that is ALWAYS `SymbolKey.Type`, so every
call site writes `SymbolKey.Type key` to get in. Keying them by `TypeKey` removes the wrapping
AND the need to document the key at all.

## B2. `EmitJs`'s `ExprShape.New` arm

Three construction strategies are probed as two independent `voption`s and joined by a
precedence `match` that the surviving comments exist to state. One classifier returning a
`NewTarget` DU — the shape `MemberDispatch` already has in this project — makes precedence a
total match and deletes both comments.

## B3. `PartitionedMembers` ↔ `emitCapabilityMethods` state one mapping twice

`EmitJsTypes`'s four record fields and `EmitJsMembers`'s four-line comprehension are the same
partition→emitter table, and three of the emitters are one-line wrappers over
`emitPlainMethod` differing only in the `JsMethodKey` they pass.

A `MemberSlot = Named | Iterator | Protocol of registryKey: string | Dispose` carried per
member makes the partition one list, `emitCapabilityMethods` one `List.map`, and deletes all
three wrappers.

Why this one matters beyond tidiness: **a wrapper with nothing to say attracts a summary.**
Both fabricated claims found in `EmitJsMembers.fs` sat on exactly those wrappers, and one of
them contradicted a correct doc five declarations lower in the same file.

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

## B6. `EmitJs.trampolineOrExpr`'s `arity`

Three lines existed only to say `arity` is the SOURCE-group count and NOT `names.Length`,
because both are bare (`int`, `string list`) and nothing ties them. One `FlatParams` value
carrying the groups and the flat names together deletes the note — **and fixes A1**, which is
that exact confusion happening one function away.

## B7. `EmitJsCapabilities`'s capability table

The 42-line module header's centre was an ASCII table mapping capability → JS anchor →
implemented shape → call lowering: four fields per row, in prose, that two code tables read
off and restate.

A record per capability would BE the table, and the two routing tables would read off it
rather than duplicate it in a form that can drift from either.

The header is already gone; this entry is what would keep it from coming back.

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
counts cannot diverge. That moves **B6's `FlatParams` below both backends, into
`Codegen.Common`**, rather than being a JS-local fix. See `codegen-clr-followups-plan.md`
A1/B1.

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
