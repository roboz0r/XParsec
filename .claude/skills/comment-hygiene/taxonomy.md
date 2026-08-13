# Failure-mode taxonomy

Nineteen named modes, each with the shape, a verbatim example found in the wild, and a
disposition. Examples are real — H1–H16 come from the `Codegen.Js` overhaul, H17–H19 from
later repo-wide passes (see the sections on those three, which have their own evidence base
and are the only modes whose disposition is rephrase rather than delete/verify/relocate).

Modes marked **greppable** can be spotted without reading any code; run those first, they are
most of the deletable bulk.

| # | mode | shape | disposition |
| --- | --- | --- | --- |
| H1 | Self-congratulation | any claim the code meets the ordinary baseline: "the ONE place X happens", "spelled ONCE so the three sites cannot disagree", "its dispatch is TOTAL over `ExprShape` with no `_`" | delete outright, no verification |
| H2 | Unverified failure claim | "falls through and fails loudly", "cannot happen", "rejected earlier" | trace to the `failwith` or delete |
| H3 | Signature restatement | doc repeats the name and types — `/// The heap ref-cell record Ref<'T>.` on `vesperRefKey` | delete outright |
| H4 | Process / history | "renamed from `Freeze`", "nothing reads them yet", milestone labels | delete outright |
| H5 | Argument for a stated invariant | one sentence states it, three paragraphs defend it | keep sentence 1, delete the rest |
| H6 | Displaced fact | a real invariant documented on the declaration ABOVE the one it describes, or duplicated where it is already correctly sited elsewhere | relocate — or delete, if the destination already says it |
| H7 | ASCII table / essay header | a multi-field table in prose that two code tables read off | becomes a type; record it, don't shorten it |
| H8 | Rejected-alternative defence | argues against a design the code does NOT have: "modelling absence a second time, as a `voption` around it, bought nothing and cost correctness…" | delete outright |
| H9 | Redundant sub-heading | an identical boilerplate prefix repeated across inline comments, categorising rather than informing: `// Backend name emission:` ×4 | strip the prefix, judge the remainder |
| H10 | Doc-by-negation | a lookup table documented by enumerating what does NOT hit it, re-narrated at the field, the accessor AND the builder, each cross-referencing the others | state once, at the accessor |
| H11 | Citation of an unreadable artefact | points the reader at a document they cannot open — a plan-table row reproduced verbatim (`Design-table row:`, `MVP scope:`, `(W1)`), or worse, a citation of a file that is not in the repo at all: `(feedback_walkelems_order_ctor_params)`, a machine-local agent memory | delete; if load-bearing, restate the FACT as one sited line |
| H12 | Producer protocol on a field | a field's doc asserts who fills it, in what order, and what they guarantee — none of it enforced: "Built by `buildCtx` over ALL flat exports before any per-export walk" | trace it, then keep the consequence and drop the mechanism |
| H13 | Cloned cross-file essay | the same multi-line block byte-identical in a sibling backend, sometimes with a line congratulating the duplication ("Shared with the JS backend so the two say one thing") | delete BOTH copies; the invariant lives at the shared helper |
| H14 | Routing-order narration | prose re-listing the order of match arms or of a probe chain that the code below reads in that order: "Resolved after the local-class path and before the external `exn`-repr fallback" | delete outright |
| H15 | Known-limitation hedge | a parenthetical pre-empting an objection: "(Not collision-proof against a source param literally named `_tc0` …)" | delete; the honest form is a failing test name |
| H16 | Jargon without an instance | states a what AND a why, but only in vocabulary defined in other files | replace with the concrete emitted shape, or delete |
| H17 | Collapsed verdict | one negation phrase standing for several distinct verdicts: "`name` names no type" covers *structurally has none*, *lookup found nothing*, and *found the wrong kind* | rephrase — pick the verdict, then negate the VERB (`does not resolve to a type`), never the object |
| H18 | Overloaded term | one noun or verb naming several independent concepts across the tree: `head`, `receiver`, `binder`, `holder`, `spine`, `drain`, `face`, `harvest`, `flow`, `leaf`, `tail` | rename per concept, reusing the word the codebase already has; the number of distinct replacements measures the damage |
| H19 | Causal hedge | an em-dash standing in for a connective the code DETERMINES: "constructs by its BARE export name with NO import — the JS runtime provides it intrinsically" (*because*), "produces NO symbol — only its members do" (*but*), "are LEAVES — never expanded" (*namely*) | rephrase — name the relation; keep the dash only as `literal — gloss`, literal LEFT |

## Grep signatures — the part that becomes a lint

Found to catch essentially every instance in a file, without reading any code:

- **H1** — `cannot drift|cannot diverge|ONE source|THE ONE|single source of truth|spelled ONCE`
  and the totality family: `TOTAL over|no wildcard|exhaustive|breaks THIS build`
- **H2** — `fails loudly|cannot happen|rejected earlier|must not reach`
- **H4** — dated futures, found in every single file: `not yet|nothing reads them yet|a later
  change will|the arm disappears once`. These are facts about a submission date.
- **H8** — `rather than a|not a .* whose|never a|which would be`, plus past-tense verdicts
  about the code itself: `bought`, `cost`, `had to fake`, `was forced`
- **H9** — an identical `//` prefix at 3+ sites
- **H10** — the same enumeration appearing twice in one file; `all MISS`, `deliberately`,
  `the mirror image of`
- **H11** — `Design-table row:`, `MVP scope:`, `Decision recorded here:`, a bare `(W1)` label,
  and `feedback_|project_|reference_` — a snake_case citation with no extension is usually an
  agent's private memory file, which no reader of the repo can ever open. Grep the whole tree
  for these: they leak from the agent's own context into code and are invisible to review.
- **H12** — a FIELD doc naming another function plus an ordering word (`before`, `AFTER`,
  `first`, `pre-pass`)
- **H13** — hash every n-line comment block and look for cross-FILE collisions, especially
  between a file and the files behind its `open` list
- **H14** — `resolved before|resolved after|falls to|anything else|else a`
- **H15** — a parenthetical containing `not .*-proof|does not handle|would need`
- **H16** — not greppable. The review question is: does this contain a concrete instance
  (`x` → `y`), or only nouns from the design? Highest-value manual check in codegen code.
- **H17** — `names no |names none|names nothing|resolves to no |maps to no |projects to no `.
  Measured 63/65 precision over the tree: the only false positive is `names` as a plural NOUN
  ("short names no longer resolve"). The positive verb (`key names the declaring type`) is
  fine and must not be matched — the negation is the target, not the word.
- **H18** — `\b(holder|receiver|binder|spine|drain|harvest)\b` and `\bface`, case-insensitive,
  over comments AND identifiers (`interface` and `surface` do NOT match `\bface`, verified;
  `head` needs manual filtering against legitimate cons use). Re-run after the renames landed
  it returned 98 hits: 3 genuine residue, all in COMMENTS, and the rest sanctioned survivors —
  monadic `binder` in the option/result/parser combinators, `cons spine` in list tests, a
  `Holder<'T>` fixture type in test source. Triage is one pass. The general test needs no word
  list: **a term appearing across three unrelated subsystems is either genuinely universal or
  overloaded, and there are very few genuinely universal terms.**
  For `flow`, `\bflow(s|ing|ed)?\b` then subtract the legitimate compounds:
  `control.flow|flow-sensitiv|flow-typing|flow-narrow|guard-flow|flow environment|data flow`.
  Measured over this tree — 72 occurrences, 27 of them on a compound line, 45 residue of which
  30 were defects. Two in three, so read the residue; do not treat it as a hit list.
- **H19** — three signatures, in yield order. The parenthetical pair has the best precision:
  7 hits over the worked files, every one a genuine defect. That precision is about DETECTION
  only, and the fix is NOT positional, so read the entry before acting on a hit. Match it
  with a window rather than per line:
  `rg -U --multiline-dotall -o '—[^—]{0,240}—' <path>`. The obvious `—.*—` is a trap: it sees
  one line only, and 3 of the first 6 wrapped, the closing dash landing mid-line where
  neither of the other two signatures reaches it either. Over `Codegen.Js` the per-line form
  hits 6 files against the window's 20 matches in 12, so it was missing about half. The
  window can pair dashes across two adjacent comment blocks, so eyeball the hits; the 3-line
  ceiling keeps that rare. Then `^\s*(///|//)\s*—` for a wrapped aside whose dash is the
  first glyph on the line. **For everything else, grep the bare `—` and classify by reading**
  — do not try to filter the good form out mechanically. `[^`]\s—`, intended to spare
  `literal — gloss`, fails in BOTH directions: on `EmitJsFormat.fs` it spared 4 dashes of
  which only 1 was a real gloss, because a sentence that merely ENDS in an inline-code span
  defeats a test on the preceding character; tightening it to literal-initial
  (``^\s*(///|//)\s*`[^`]+`\s*—``) then missed 5 of the 9 genuine glosses in `EmitJs.fs`,
  which qualify the literal before the dash (`` `while cond do body` as a bare loop
  statement — … ``). The distinction is semantic: does the left side NAME a thing the right
  side defines, or make a CLAIM the right side relates to? No regex sees that, and dashes are
  sparse enough per file (14 in 476 lines) that reading all of them is cheap. Exclude
  `docs/*.md` (ephemeral plan docs; one alone holds 197) and the FSharp.Core-ported
  `prim-types-*.fsi`, whose docs are not ours to rewrite.

## H16 is the subtlest mode, and the one that survives careless sweeps

It passes every other test — short, true, states a reason, not self-congratulating. It fails
only on SELF-CONTAINMENT.

> **Bad:** *"it wraps the flat function in a curried adapter, so a consumer sees the
> SOURCE-shaped currying."*

"Flat", "curried adapter" and "SOURCE-shaped currying" all have to be learned from another
file before the sentence means anything — so recovering the goal costs the reader dozens of
lines, which is the job the comment existed to do.

> **Good:** *"a module function compiles FLAT (`add(a, b)`), so a bare value-use must
> re-curry it: `let f = add` emits `(c0) => (c1) => add(c0, c1)`."*

Same length, same two facts, no prerequisite reading.

### Second form: a name that is not in scope

Missed by four agents in a row. The comment references a data structure that does not exist
at the site, so the reader must go find it:

> `Index 0 of the finished `sources[]` is the CONSUMING file, held elsewhere, so the first
> `publish` takes slot 1.`

`sources[]` occurred four times in one file and is nothing in that file — it is a JSON field
assembled two files away. "Held elsewhere" gestures at a location the reader cannot guess.

> `The emitted map's `"sources"` opens with the file being compiled, so the first `publish`
> here takes slot 1: `["app.fs", "Vesper.Core/math/z.fs", …]`.`

**Detection heuristic:** every back-ticked identifier in a comment should be findable in that
file, or be a literal of the emitted output. Vague locatives — "held elsewhere", "set
upstream", "the caller" — are the same failure, and mark exactly where a name was avoided.

### The example itself can be fabricated — check it, don't admire it

Showing input → output is the most durable comment shape, but only if the shown thing is
real. An unchecked example is WORSE than no example, because concreteness reads as evidence.
Four kinds found in one file pair:

- **A member that does not exist.** `ClrEnv.ClosureTyparMode` — the member is
  `ClosureTyparScope`. The comment also misattributed the work to it.
- **The wrong domain's type.** `TyTypar` is a `SemType` case; `FTTypar` is its `FrozenType`
  counterpart. Four docs in a `FrozenType`-native file named the wrong one.
- **Syntax the language does not have.** `Emit.buildMember ~voidReturn:true` — OCaml labelled
  arguments. The parameter is real; that call form cannot be written in F#.
- **A citation of an unreadable artefact.** See H11.

All four survive review because they look specific. Run the identifier through a grep, and run
the syntax through the compiler's own grammar in your head — if you cannot say which file
would contain the shown line, you invented it.

### H3 beats H16

H16 governs comments that MUST exist. It is not a licence to create one. An over-correction
found in this sweep:

```fsharp
/// `a"b` prints `"a\"b"`. One escape set safe in both a JS string literal and JSON, so the
/// source-map writer shares it: no control char (< 0x20) reaches either output raw.
module internal JsEscape =
```

`JsEscape` can mean one thing, its single function is `quoted : string -> string`, and the
example is the match arms three lines below, restated. Showing the output turned a redundant
doc into a redundant doc WITH AN EXAMPLE — it reads as an implementation note, and it is
longer.

## H17 and H18: one word doing several jobs

The same disease at two scales — a phrase, and a term. Both differ from every mode above in
that the fact is usually RIGHT and only the words are wrong, so the disposition is rephrase
rather than delete. That is a licence to fiddle, so each has to earn its place by catching a
comment that is false or empty, not one that is merely ugly. Both do.

Evidence base is a different pass from the rest of this file: `SemanticAnalysis`, both
backends and the parser, ~800 file-touches across nine commits. H18's ninth word, `flow`, came
later still, from a review that started on one comment and ended in a 30-site sweep.

### H17 — the negation that stood for three verdicts

`names no X` occurred 64 times in 44 files and read as one fact. It was three:

- **Structurally has none — no lookup ever ran.** A counter-minted `NodeKey`, a virtual
  token, a provider layer with no tail. Say **has no** / **carries no** / **occupies no**.
- **Lookup ran, nothing of that spelling exists.** Say **does not resolve to a X**, or **is
  not in scope here** when the miss is positional.
- **Resolved fine, wrong kind.** Say **is neither … nor …** / **is not a**.

Three defects the uniform phrasing was hiding, which is what makes this a precision mode:

- **A false claim.** *"A pattern introducing no bound variable (`let (a, b) = p`) names no
  value"* — that pattern introduces two bound variables. It has no single EXPORTABLE name,
  which is what the function actually tests. A vague verb let a wrong sentence read as true.
- **Two vacuous ones.** *"A non-type key names no type, so it mints no `TypeRef`"* and *"a
  non-type key names no interface"* restate their own subject. Deleting the phrase forced the
  real rule out: *"only a type key mints a `TypeRef`"*.
- **An asserted lookup that never happens.** A counter-minted key does not FAIL to resolve a
  source position — it has none by construction. "names" implies a resolution that could have
  succeeded, which is a claim about a code path that does not exist.

The grammar is the mechanism, not a taste: **negation in the middle.** `names no type`
negates the object where English negates the verb, so the reader parses the sentence before
the concept. The trap on the way out is that `resolves to no type` has the identical shape,
and a two-word verb is worse — `resolves to` is held open across the negation. Negate the
verb. The plain transitive with no preposition (`resolves no values`) reads fine; leave it.

### H18 — one noun for several concepts

Ten words, each naming several independent things. The replacement count is the measure:

- `head` → `fn`/`Function`, `tyCtor`, `anchorIdent`, `ctorFun`/`ctorPat`, `current`,
  `zonkShallow`; kept for cons only — 160 files
- `receiver` → `objArg`, `qualifier`/`prefix`, `supportTy`, `ctorTy`, the `this` pointer — 153 files
- `binder` → the LHS is a `pattern`, the names are `bound variables`, `Binding` is the whole
  construct; kept for a monadic `'a -> M<'b>` — 142 files
- `face` → `interface`, `surface`, canon-only — 83 files
- `spine` → `appArgs`, `collectAppChain`, `mintAppChain`, `mapAppChain`, `peelFunDomains` — 77 files
- `holder` → `Container` (`ModuleContainer`, `TypeContainer`, `ContainerKey`) — 75 files
- `harvest` → `extract` (read a value out), `lift` (member → this-first function) — 53 files
- `drain` → `discharge` (constraints), `finalize` (refs, diagnostics), `collect` — 52 files
- `flow` → `passed to` (an argument), `assigned to` (a field), `is accepted by` (assignability),
  `propagates` (a verdict down graph edges), `escapes` (a closure), `comes from` / `derives
  from` (an origin), `is preserved` (an order), `re-enters` (recursion); kept for `control
  flow` and for dataflow analysis — 23 files
- `leaf` → `…Channels` (a record oflookup functions), `tryReplace` (a callback fired at every node), `bound variable` (what a pattern introduces), `argument`/`slot` (a type argument at a position), `namingPat`,
  `TyparKey`, `atom`; kept for a childless tree node and a call-stack frame — 45 files
- `tail` → `PlatformMetadata…` (the layer-2 provider), `last segment` (of a long ident),
  `END` (of a ctor, a scope, a member list), `the rest` (of a fused operator token),
  `Codomain` (a function type's result), `Rest` (a `ValueTuple`8` nesting), `the implicit
  ones` (inferred typars), `fallback`, `Trampoline…`; kept for cons and for tail CALLS — 44 files

Every replacement is a word the codebase ALREADY used for that concept. None is a coinage,
and that is the acceptance test: **when a rename cannot find an existing word, the concept is
not modelled** — a type candidate, the same verdict the 3-line ceiling produces.

Two diagnostics that name the mode before any renaming:

- **The word is a metaphor, not a term of art** — `holder`, `spine`, `drain`, `face`,
  `harvest`. A picture accepts any concept that fits it, so it accretes.
- **The word IS a term of art, for something else** — `head` (cons), `binder` (monadic
  `'a -> M<'b>`), `receiver` (the OO sense), `arrow` (the JS function form), `flow` (control
  flow, and dataflow analysis), `leaf` (a tree node with no children). Used for their
  approximate meaning, these are worse than metaphors: a reader who knows the term is
  actively misled rather than merely uninformed.

> Those eight renames touched ~800 files because the vocabulary had reached the identifiers,
> and the comments only inherited it. **Catch it in the comment and you catch it before it is
> an API.**

`flow` is the case where that already held, and it is the reason it was added late. It reached
exactly one identifier — `Cil.fs`'s `flow`, correctly bound to a `ControlFlowBuilder` — so the
whole defect sat in prose and no rename pressure ever surfaced it. A word can be this
overloaded and still be invisible to every tool the compiler gives you.

`leaf` is the case where the warning above arrived too late: it had already reached the API
as `NamedLeaf`/`KeyedLeaf`/`KeyIndexedLeaf` and `ofNamedLeaf`/`ofKeyedLeaf`, none of which is
a leaf of anything — they are records of lookup functions, composable at any position in the
provider stack. Two independent falsehoods surfaced when the senses were separated, and both
are the signature of a term of art borrowed for its picture:

- **A parameter named for the opposite of its contract.** `FrozenType.mapVariant`'s `leaf`
  callback fires FIRST at every node, interior ones included, so the test pinning that had to
  be named *"the leaf is consulted first at NON-leaf nodes"*. A comment forced to negate its
  own noun is the tell.
- **A position asserted by a name the value does not have.** One test read *"a repr-only leaf
  AHEAD of the contract"* and another composed `[ leaf; realProvider ]` — leaf first.

**Its disposition is heavier than the other eight, and that is the point.** Renaming `holder`
to `Container` is a substitution. Replacing `flow` means first deciding WHICH relation was
meant, so it forces the verification H19 forces: *"that order flows through UNCHANGED"* is
**preservation**, *"never flows out"* is **escape** — and `escapes` was already the codebase's
word, sitting in `bridgeStaticFnEscapes` — *"flows to the caller's diagnostic"* is **left for
the caller to diagnose**. Three different facts behind one verb, none recoverable from it.

The user's own formulation is the rule to apply: **control flow is a genuine concept; values
are passed to a function or assigned to a field.** Anything using the word for a value is
naming a relation it declined to pick.

And the reason this belongs in a COMMENT doc rather than a naming one: re-running the
signature after all eight renames had landed found three surviving sites, and every one was a
comment — `HOLDER-CLASS` in a doc whose own code already said `ModuleContainer`, `the whole
spine` beside the arguments it had been renamed to, a `SPINE` in a design note. A rename
reaches identifiers; the compiler makes sure of it. Nothing reaches the prose, so the retired
word survives exactly where it is least checkable and reads most authoritative.

## H19 — the causal hedge

Same family as H17 and H18: the fact is usually right and only the words are wrong, so the
disposition is rephrase. It sits one level below both: not a phrase or a term but a mark of
punctuation.

`Codegen.Js` carried 147 em-dashes over 1038 comment lines, one per seven, and that is AFTER
the sweep that produced H1–H16. That sweep cut block length and never looked at punctuation,
so this is an orthogonal axis rather than residue. Repo-wide `src/` holds ~2000.

### Why this is a technical-writing defect, not a style preference

An em-dash is a legitimate hedge in prose about human affairs, where the writer may honestly
not know how two facts relate. That licence does not transfer.

> **Code is deterministic.** The relation between two facts about it is itself a fact, and the
> writer had the file open. A dash where `because` belongs is not economy; it is a fact the
> writer held and declined to hand over, leaving the reader to re-derive from the source
> exactly what the comment existed to save them.

The mark is also *unfalsifiable*, which is what makes it more than untidy. `X — Y` is equally
compatible with *Y causes X*, *X causes Y*, *Y is an instance of X*, and *Y is merely
adjacent*; a reader cannot tell which was meant, and a reviewer cannot tell whether any of
them was checked. `because` is a claim that can be traced and found false. So the hedge evades
the verification H2 demands, at the level of punctuation rather than the level of a sentence.
That evasion is why it feels comfortable to write.

**The corollary is the rule.** Naming the connective is not a rewording pass; it is a
verification pass that happens to change words. Writing `because` forces you to confirm it is
a cause. Twice in one file pair, the confirmation failed and the content changed:

- **The aside subsumed the clause it interrupted.**
  `// JS is dynamically typed — every value is already a boxed `obj` — so `e :> obj` is a no-op`
  Once forced to pick, the two halves proved to be one fact. The head deleted:
  `// Every JS value is already a boxed `obj`, so `e :> obj` is a no-op`.
- **The dash pair was hiding jargon.** `// … is either a primitive spelled canonically — mint
  the `Vesper` key … — or genuinely external. A name recogniser: no provider is in hand.`
  Rebuilding the `either … or` also killed `A name recogniser`, which on inspection meant
  "decided on the name alone" (H16).

### The one shape to keep: `literal — gloss`, literal LEFT

Here the dash is not prose punctuation but a two-column layout, and a colon reads worse:

```fsharp
/// `continue;` — re-enters the `While` trampoline after the parameter write-back.
/// `use x = value in body` — park the bound variable in a `const`, run the body in a `try`.
```

`JsAst.fs` runs 28 of these and reads as a table: the H16 shape (show the emitted output) in
its most compact form. Roughly half the corpus qualifies. Discipline: literal on the LEFT,
gloss on the right, ONE dash, and the right side must define rather than explain.

**Literal on the right means invert, and inverting usually shortens.** Two of the worked
sites had it backwards, which is the second bad shape below in disguise:

```fsharp
- /// Curry `base` over `args` — one unary `Call` per argument, in source order:
- /// `base(a)(b)…`.
+ /// `base(a)(b)…` — one unary `Call` per argument, in source order.
```

Two lines to one, and it became a legal glossary entry. The other dropped its lead outright
(`// Destructuring bound variable — `for (k, v) in map`: …`), because the discarded half was
restating the `PatShape.Tuple` arm on the next line (H3).

### The three shapes to cut

- **Parenthetical pair.** Best precision, and it strands whatever follows the second dash:
  ```fsharp
  // Member functions emit after the class decls — they reference the classes via
  // `new`/match, and `const` arrows are not hoisted — and before the body.
  ```
  `and before the body` is half the ordering constraint, marooned past a 15-word aside. It
  also splits verbs from their subject: `A *mutable* bound variable is excluded — … — and
  falls below` puts two verbs about one subject on opposite sides of the interruption.

  **It is a reliable detector and an unreliable prescription — do NOT reflexively delete the
  aside.** All 7 pairs in the worked files marked a genuine defect, but the fix is not
  positional: deleting the aside would have destroyed the only non-recoverable content in 4
  of the 7, and in 1 the correct edit was to delete the HEAD (`JS is dynamically typed —
  every value is already a boxed `obj` — so …`, where the aside subsumed the head). The
  mechanism inverts the intuition: a dash pair is an author interrupting a STRUCTURAL
  statement — ordering, classification, dispatch — to insert the REASON. The structure is
  what the code below already says (H14, H3: delete outright); the reason is what the reader
  cannot recover.

  > **In a parenthetical pair, suspect the HEAD.** The aside is usually the only thing in the
  > comment that is not already in the source.

  Strip the aside from the example above and what survives is bare ordering narration — an
  H14 violation with its why removed. n=7; treat as a tendency, not a law. The seventh
  instance is the standing counterexample, and it names the exception: **in a per-CASE doc
  neither half can go**, because the head names the case. `` /// A `float32` whose repr is a
  JS `number` — an IEEE-754 DOUBLE — so JS renders it at double precision `` needs the head
  (it is the `Single` case's doc) and needs the aside (a JS `number` BEING a double is what
  loses the width). The fix there is apposition, never deletion: `` …a JS `number`, an
  IEEE-754 DOUBLE, so… ``. Running tally over 7: aside is the keeper 3, aside droppable 2,
  delete the HEAD 1, both required 1.
- **Trailing afterthought where the tail is the point.** `A committed `.mjs` shipped beside
  the compiled output — one of the assets a package writes`. Invert; the tail is the fact.
- **Line-initial dash** — the wrapped remainder of one of the above, and the strongest tell
  that the mark was reflex, since a reader scanning the left margin meets a dash.

**The proof that it is reflex rather than composition** is the site that wrote both:

```fsharp
// `parts.Free` is all of `unionMembers` — hence no `addMembers` here.
```

The connective and the dash. Whoever wrote that knew the relation, said it, and reached for
the mark anyway.

That generalises into a reading tell, though NOT into a signature: **a dash in a sentence
that already carries a connective is a stacked consequence**, and the second link is the one
never named. `` /// …no front-end symbol resolves to it, so no provider shape carries its
home — codegen names both the key and the module `` chains a second *therefore* off the
first; it wants a full stop. Tested as a grep
(`(///|//).*\b(so|because|hence|since)\b.*—` and its mirror) it returned 12 hits over
`Codegen.Js` at ~50% precision, because every false positive is the legitimate glossary form
with its `so` INSIDE the gloss (`` `(target = value)` — parenthesised, so it is safe as a
comma-sequence operand ``). Half precision is no better than reading every dash, which is
cheap. Recorded as a negative result so it is not re-derived.

### Measured

Four files, the rule applied end to end:

| file | before | after | kept as | cut |
| --- | --- | --- | --- | --- |
| `TsManifestTypes.fs` | 10 | 1 | glossary | 90% |
| `EmitJs.fs` | 32 | 10 | 9 glossary + 1 diagnostic string | 69% |
| `EmitJsFormat.fs` | 14 | 1 | glossary | 93% |
| `EmitJsContext.fs` | 12 | 3 | 2 glossary + 1 diagnostic string | 75% |

68 → 15, and a NET LOSS OF ONE LINE across the four: the rule costs nothing in length, it
only forces a word to be chosen. Of the 53 fixed, roughly half were causal (*because* / *so*
/ *since*), 14 sat in parenthetical pairs, the rest appositive (*namely*), and 2 were
inverted glossaries.

**Count occurrences, not lines.** `rg -c '—'` counts matching LINES, and a single-line
parenthetical pair holds two dashes, so the two methods disagree by exactly the count of the
mode you most want to find. Use `rg -o '—' | wc -l`.

**The spread in cut rate is predictable, and it is the useful planning number.** A file that
already has a gloss convention has no legitimate work left for the dash. `EmitJsFormat.fs` is
a specifier table written as `` `%0wd`: <rule> ``, where the COLON does the naming, so all 14
dashes were connectives and 13 went — and it held zero parenthetical pairs and zero
line-initial dashes, a profile entirely unlike `EmitJs.fs`. Where a file's comments are
per-case one-liners over emitted output (`JsAst.fs`), expect the opposite: mostly legitimate.
Check a file's existing punctuation convention before estimating how much of it will move.

**Out of scope: string literals.** `EmitJs.fs:780` and `EmitJsContext.fs:124` hold em-dashes
inside `failwithf` messages with the same defect. That is user-facing diagnostic text, not a
comment; editing it can move test expectations, so it wants a separate decision rather than a
punctuation sweep.

## Where the rot accretes — the siting law

The rot is NOT uniform within a file. In the worst file of the sweep, the lowering arms
carried short, correct, reader-directed facts (`{ r with … }` → `new R(…)`; `Array.zeroCreate
count` → `Array(count).fill(null)`, dense not sparse) and those were most of the surviving
lines. Every essay was at a HEADER OR BOUNDARY — module headers, function docs, a function
prologue — where there is no single executable line for the prose to sit on and therefore
nothing keeping it honest.

> **Essay length correlates with distance from a single executable line.**

This is the most actionable finding, because it is preventative rather than corrective: a
comment that cannot be sited on one line of code is the one about to become an essay. It also
predicts where to look first, and it explains why a file of one-line-per-DU-case docs is fine
at a ratio that looks terrible.

Corollary, seen twice: **a wrapper with nothing to say attracts a summary.** Both fabricated
claims in one file sat on one-line wrappers that differed from each other only in an argument.

## Per-field docs fail differently from per-function docs

- **Per-function** docs fail by self-congratulation (H1) and false path claims (H2).
- **Per-field** docs fail by narrating producer protocol and ordering (H12).

The cause is mechanical: a field in isolation has no behaviour to describe, so the prose
expands into the protocol around it. H2 is nearly absent from type-definition files for the
same reason — a type has no fall-through path to lie about. H12 claims are the ones most
worth the verification budget, because nothing in the type enforces them and a true one is
usually a type-refactor candidate.

## The audience hypothesis, and its test

Every mode except H3 is a comment addressed to someone deciding whether to accept the change,
rather than to someone who has already accepted it and now has to modify the code.

The test that distinguishes this from "written carelessly for the next reader": **the
deleted material is disproportionately about things that DID NOT CHANGE.** A constructor
documented as "the ONE constructor — production and the tests share it"; a field documented
by a `voption` it does not have; a 25-line header spending three of five paragraphs
re-arguing that the alternative would be wrong. None of that helps someone modifying the
code; all of it answers "is this change correct?".

The same lens explains H6, which otherwise looks like simple sloppiness: **a doc written
against the DIFF rather than against the DECLARATION is exactly the doc that ends up one
declaration off once the diff lands.**

And it explains why type-definition files are worse, not better: with no code path to
describe, the prose expands to fill the space with rationale. All three of the longest blocks
in one such file (26, 16 and 17 lines) shared one skeleton — two lines of invariant, then ten
of "and here is why we didn't do the other thing." That is a comment written AT A DESIGN
FORK, addressed to the person standing at that fork, and it has no reader afterwards.

## How comments go wrong after being written

A wrong comment is wrong. Do not spend time deciding whether it was wrong when written —
the disposition is the same, and the answer changes nothing you do. Three rules earn their
place because they change WHERE YOU LOOK:

**A moved comment is a NEW claim.** A refactor that relocates a doc block is the moment it is
least likely to be checked and most in need of it, because the diff reads as a pure move.
Worse, refactors compose new prose FROM the block they are moving: one split wrote a fresh
two-line summary naming the three members that commit had just routed elsewhere, while the
correct doc sat five declarations lower in the same file.

> When you summarise code you are moving, the source of truth is the CODE, never the doc that
> travelled with it.

**Migrations leave a name trail — grep it in the same change.** A type migration rewrites many
declarations and touches no comment, so every doc naming the old type keeps compiling and
keeps reading plausibly. `SemType` → `FrozenType` gave most cases a near-twin
(`TyUnion`/`FTUnion`), and backend docs still named the old one two months later — they
survive review because the wrong name is a REAL type one letter from the right one.

```bash
grep -rInE '^\s*(///|//).*\bTy(Union|Record|Typar|Class|Const)\b' --include=*.fs src/<new-domain-project>/
```

Filter by which domain each file belongs to; do not mass-replace. In the old domain's own
project the names are still correct — 55 files legitimately, against 6 stale backend sites.

**`git blame` is not evidence of freshness**, so do not triage by recency. In the worked case
it dates the false lines nine days AFTER they became false. When you do need history, `git log
-S'<phrase>' --all` on the comment's SUBJECT is the query that works.

This is also the strongest argument for self-containment: a comment about another module's
internals has nothing linking it to that module's changes, while one showing this file's own
output can only go stale when this file changes — which is when someone is looking at it.
**Self-contained comments rot more slowly.**
