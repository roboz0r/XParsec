# Failure-mode taxonomy

H17–H19 are the only modes whose disposition is rephrase rather than delete/verify/relocate.
Provenance and raw measurements are in `evidence.md`. Run the modes with grep signatures
first — they are most of the deletable bulk and need no code read.

| # | mode | shape | disposition |
| --- | --- | --- | --- |
| H1 | Self-congratulation | any claim the code meets the ordinary baseline: "the ONE place X happens" | delete outright, no verification |
| H2 | Unverified failure claim | "falls through and fails loudly", "cannot happen", "rejected earlier" | trace to the `failwith` or delete |
| H3 | Signature restatement | doc repeats the name and types — `/// The heap ref-cell record Ref<'T>.` on `vesperRefKey` | delete outright |
| H4 | Process / history | "renamed from `Freeze`", "nothing reads them yet", milestone labels | delete outright |
| H5 | Argument for a stated invariant | one sentence states it, three paragraphs defend it | keep sentence 1, delete the rest |
| H6 | Displaced fact | a real invariant documented on the declaration ABOVE the one it describes, or duplicated where it is already correctly sited elsewhere | relocate — or delete, if the destination already says it |
| H7 | ASCII table / essay header | a multi-field table in prose that two code tables read off | becomes a type; record it, don't shorten it |
| H8 | Rejected-alternative defence | argues against a design the code does NOT have: "modelling absence a second time, as a `voption` around it, bought nothing and cost correctness…" | delete outright |
| H9 | Redundant sub-heading | an identical boilerplate prefix repeated across inline comments, categorising rather than informing: `// Backend name emission:` ×4 | strip the prefix, judge the remainder |
| H10 | Doc-by-negation | a lookup table documented by enumerating what does NOT hit it, re-narrated at the field, the accessor AND the builder | state once, at the accessor |
| H11 | Citation of an unreadable artefact | points the reader at a document they cannot open — a plan-table row (`Design-table row:`, `(W1)`), or a citation of a file not in the repo at all: a machine-local agent memory | delete; if load-bearing, restate the FACT as one sited line |
| H12 | Producer protocol on a field | a field's doc asserts who fills it, in what order, and what they guarantee — none of it enforced: "Built by `buildCtx` over ALL flat exports before any per-export walk" | trace it, then keep the consequence and drop the mechanism |
| H13 | Cloned cross-file essay | the same multi-line block byte-identical in a sibling backend | delete BOTH copies; the invariant lives at the shared helper |
| H14 | Routing-order narration | prose re-listing the order of match arms or of a probe chain that the code below reads in that order: "Resolved after the local-class path and before the external `exn`-repr fallback" | delete outright |
| H15 | Known-limitation hedge | a parenthetical pre-empting an objection: "(Not collision-proof against a source param literally named `_tc0` …)" | delete; the honest form is a failing test name |
| H16 | Jargon without an instance | states a what AND a why, but only in vocabulary defined in other files | replace with the concrete emitted shape, or delete |
| H17 | Collapsed verdict | one negation phrase standing for several distinct verdicts: "`name` names no type" covers *structurally has none*, *lookup found nothing*, and *found the wrong kind* | rephrase — pick the verdict, then assert it positively; where the fact is a genuine miss, negate the VERB (`does not resolve to a type`), never the object |
| H18 | Overloaded term | one noun or verb covering several independent concepts across the tree: `head`, `receiver`, `binder`, `holder`, `spine`, `drain`, `face`, `harvest`, `flow`, `leaf`, `tail`, the verb `name` | rename per concept, reusing the word the codebase already has; the number of distinct replacements measures the damage |
| H19 | Causal hedge | an em-dash standing in for a connective the code DETERMINES: "produces NO symbol — only its members do" (*but*) | rephrase — name the relation; keep the dash only as `literal — gloss`, literal LEFT |

## Grep signatures

These catch essentially every instance in a file:

- **H1** — `cannot drift|cannot diverge|ONE source|THE ONE|single source of truth|spelled ONCE`
  and the totality family: `TOTAL over|no wildcard|exhaustive|breaks THIS build`
- **H2** — `fails loudly|cannot happen|rejected earlier|must not reach`
- **H4** — dated futures: `not yet|nothing reads them yet|a later change will|the arm
  disappears once`. These are facts about a submission date.
- **H8** — `rather than a|not a .* whose|never a|which would be`, plus past-tense verdicts
  about the code itself: `bought`, `cost`, `had to fake`, `was forced`
- **H9** — an identical `//` prefix at 3+ sites
- **H10** — the same enumeration appearing twice in one file; `all MISS`, `deliberately`,
  `the mirror image of`
- **H11** — `Design-table row:`, `MVP scope:`, `Decision recorded here:`, a bare `(W1)` label,
  and `feedback_|project_|reference_` — a snake_case citation is usually an agent's private
  memory file, which no reader of the repo can ever open. Grep the whole tree; match the
  snake_case prefix and IGNORE the extension (a plausible `.md` filename reads as a repo
  document and survives review). Verify with one `Get-ChildItem`/`find` against the repo; the
  fix is always to restate the memory's one load-bearing sentence inline and drop the pointer.

  **A ticket ID is a different thing.** It resolves for every reader with tracker access, so
  the H11 warrant (*the reader cannot open it*) does not apply. Keep a citation only where
  the code does not make sense without the ticket — a motivation not visible in the repo at
  all. Not earned: stamping the ID on every block the ticket touched (one change left five
  `HOT-3304` markers; four on self-explaining guards were deleted, one kept on the parameter
  the ticket accounts for). The rule is per DECLARATION, not per hunk, and the sentence must
  work without clicking through.
- **H12** — a FIELD doc naming another function plus an ordering word (`before`, `AFTER`,
  `first`, `pre-pass`)
- **H13** — hash every n-line comment block and look for cross-FILE collisions, especially
  between a file and the files behind its `open` list
- **H14** — `resolved before|resolved after|falls to|anything else|else a`
- **H15** — a parenthetical containing `not .*-proof|does not handle|would need`
- **H16** — not greppable. The review question: does this contain a concrete instance
  (`x` → `y`), or only nouns from the design? Highest-value manual check in codegen code.
- **H17** — `names no |names none|names nothing|resolves to no |maps to no |projects to no `,
  plus the plain transitive family `\b(resolves|introduces|mints|carries|drops|binds|emits) no `.
  The first list is high-precision (only observed false positive: `names` as a plural NOUN);
  the transitive family is unmeasured (`evidence.md`). The positive verb (`key names the
  declaring type`) escapes this signature and is an H18 hit — see `name` in `vocabulary.md`.
- **H18** — `\b(holder|receiver|binder|spine|drain|harvest)\b` and `\bface`, case-insensitive,
  over comments AND identifiers (`interface` and `surface` do NOT match `\bface`; `head` needs
  manual filtering against legitimate cons use). The general test needs no word list: **a
  term appearing across three unrelated subsystems is either genuinely universal or
  overloaded, and there are very few genuinely universal terms.**
  For the verb `name`, `\bnames? (a|an|the|it|its|one|several|both|this|that|where|by)\b` over
  comments: the plural noun does not take a following determiner, so precision is high.
  For `flow`, `\bflow(s|ing|ed)?\b` then subtract the legitimate compounds:
  `control.flow|flow-sensitiv|flow-typing|flow-narrow|guard-flow|flow environment|data flow`.
  Read the residue; do not treat it as a hit list.
- **H19** — three signatures, in yield order. The parenthetical pair has the best precision,
  but that is DETECTION only — the fix is not positional, so read the entry before acting on
  a hit. Match it with a window, not per line:
  `rg -U --multiline-dotall -o '—[^—]{0,240}—' <path>` (the obvious `—.*—` sees one line only
  and misses about half the hits, `evidence.md`; the window can pair dashes across adjacent
  blocks, so eyeball). Then `^\s*(///|//)\s*—` for a wrapped aside whose dash opens the line
  — dead in any `///`-doc language, where the window covers it.
  **For everything else, grep the bare `—` and classify by reading** — every mechanical
  filter for sparing `literal — gloss` failed both ways (`evidence.md`) because the
  distinction is semantic, and dashes are sparse enough that reading them all is cheap.
  Exclude ephemeral plan docs under `docs/` and vendored or ported source.

  **The spaced ASCII hyphen ` - ` is the same defect and a different problem.** `—` is a
  near-perfect authorship tell (roughly 0% of human-written comments); ` - ` is what a human
  types for the same mark, and also spells a bullet, a range, a negative number, a CLI flag.
  Run it as a separate second pass: `rg -n '^\s*(///|//).* - '`. Recall is the reason to run
  it; precision is the reason to read every result rather than batch a disposition.

  **A codebase has a dash convention — check which before estimating.** Where comments spell
  the mark ` - `, an `—` hit is nearly always agent-written defect; where they spell it `—`,
  a ` - ` is usually honest punctuation.

## H16 — the mode that survives careless sweeps

An H16 comment passes every other test — short, true, states a reason — and fails only on
SELF-CONTAINMENT: every load-bearing term must be learned from another file, so recovering
the goal costs the reader the very reading the comment existed to save.

> **Good:** *"a module function compiles FLAT (`add(a, b)`), so a bare value-use must
> re-curry it: `let f = add` emits `(c0) => (c1) => add(c0, c1)`."*

Second form: a name that is not in scope in this file, with a vague locative gesturing at
where it lives. **Detection heuristic:** every back-ticked identifier in a comment should be
findable in that file, or be a literal of the emitted output; vague locatives — "held
elsewhere", "set upstream", "the caller" — mark exactly where a name was avoided.

**The example itself can be fabricated — check it, don't admire it.** An unchecked example is
WORSE than no example, because concreteness reads as evidence: a member that does not exist
(`ClrEnv.ClosureTyparMode` for `ClosureTyparScope`), the wrong domain's type, syntax the
language does not have. Grep the identifier and run the syntax through the compiler's grammar
in your head — if you cannot say which file would contain the shown line, you invented it.

**H3 beats H16.** H16 governs comments that MUST exist; it is not a licence to create one.
Adding an emitted example to a doc whose module can mean only one thing yields a redundant
doc WITH AN EXAMPLE — longer, and reading as an implementation note.

## H17 and H18: one word doing several jobs

The same defect at two scales — a phrase, and a term. The fact is usually RIGHT and only the
words are wrong, so the disposition is rephrase — applied where the wording hides a false or
empty claim, not where it is merely ugly.

### H17 — the negation that stood for three verdicts

`names no X` read as one fact. It was three:

- **Structurally has none — no lookup ever ran.** Say **has no** / **carries no** /
  **occupies no**.
- **Lookup ran, nothing of that spelling exists.** Say **does not resolve to a X**, or **is
  not in scope here** when the miss is positional.
- **Resolved fine, wrong kind.** Say **is neither … nor …** / **is not a**.

The uniform phrasing hides false claims, vacuous restatements, and asserted lookups that
never happen: *"A pattern introducing no bound variable (`let (a, b) = p`) names no value"* —
that pattern introduces two bound variables; a vague verb let a wrong sentence read as true.
The mechanism: `names no type` negates the object where English negates the verb (`resolves
to no type` has the identical shape, no better). The repair order is `writing.md`'s: assert
positively where a positive fact exists (exclusion framing, construction 4), and negate the
verb where the fact is a genuine miss.

### H18 — one noun for several concepts

Twelve words, each covering several independent things (per-word counts in `evidence.md`;
`vocabulary.md` holds each word's senses and replacements in full):

- `head` → `fn`/`Function`, `tyCtor`, `anchorIdent`, `ctorFun`/`ctorPat`, `current`,
  `zonkShallow`; kept for cons only
- `receiver` → `objArg`, `qualifier`/`prefix`, `supportTy`, `ctorTy`, the `this` pointer
- `binder` → the LHS is a `pattern`, the names are `bound variables`, `Binding` is the whole
  construct; kept for a monadic `'a -> M<'b>`
- `face` → `interface`, `surface`, canon-only
- `spine` → `appArgs`, `collectAppChain`, `mintAppChain`, `mapAppChain`, `peelFunDomains`
- `holder` → `Container` (`ModuleContainer`, `TypeContainer`, `ContainerKey`)
- `harvest` → `extract` (read a value out), `lift` (member → this-first function)
- `drain` → `discharge` (constraints), `finalize` (refs, diagnostics), `collect`
- `flow` → `passed to` (an argument), `assigned to` (a field), `is accepted by` (assignability),
  `propagates` (a verdict down graph edges), `escapes` (a closure), `comes from` / `derives
  from` (an origin), `is preserved` (an order), `re-enters` (recursion); kept for `control
  flow` and for dataflow analysis
- `leaf` → `…Channels` (a record of lookup functions), `tryReplace` (a callback fired at every
  node), `bound variable` (what a pattern introduces), `argument`/`slot` (a type argument at a
  position), `namingPat`, `TyparKey`, `atom`; kept for a childless tree node and a call-stack
  frame
- `tail` → `PlatformMetadata…` (the layer-2 provider), `last segment` (of a long ident),
  `END` (of a ctor, a scope, a member list), `the rest` (of a fused operator token),
  `Codomain` (a function type's result), `Rest` (a `ValueTuple`8` nesting), `the implicit
  ones` (inferred typars), `fallback`, `Trampoline…`; kept for cons and for tail CALLS
- the verb `name` → `identifies` (a key, its row), `resolves to` (an ident, a symbol),
  `lists` (a manifest, a path), `points to` (a token, a source position), `hardcodes`
  (codegen, an identity the providers omit), `binds` (an emitted wrapper, a value),
  `refers to it as` (a diagnostic's phrase); kept for literally assigning a name

Three more — `arrow`, `contract` and `package` — were caught at review; their entries are in
`vocabulary.md`.

Every replacement is a word the codebase ALREADY used for that concept, and that is the
acceptance test: **when a rename cannot find an existing word, the concept is not modelled**
— a type candidate. Two diagnostics identify the mode before any renaming: the word is a
metaphor, not a term of art (`holder`, `spine`, `drain`, `face`, `harvest` — a picture
accepts any concept that fits it, so it accretes), or the word IS a term of art for
something else (`head`, `binder`, `receiver`, `arrow`, `flow`, `leaf` — worse, because a
reader who knows the term is actively misled).

> Catch it in the comment and you catch it before it is an API — the worked renames touched
> ~800 files because the vocabulary had reached the identifiers first.

**`flow`'s disposition is heavier than the others'.** Replacing it means first deciding WHICH
relation was meant, so it forces the verification H19 forces. The rule: **control flow is a
genuine concept; values are passed to a function or assigned to a field.** A rename reaches
identifiers; nothing reaches the prose, so the retired word survives exactly where it is
least checkable — after the renames landed, every surviving hit was in a comment.

## H19 — the causal hedge

The fact is usually right and only the words are wrong, so the disposition is rephrase. A
sweep on every other mode leaves this one intact, so run it as its own pass.

Why it is a defect and not a style call: **code is deterministic, so the relation between two
facts about it is itself a fact the writer held and declined to hand over** — and `X — Y` is
unfalsifiable (equally compatible with cause, instance, and mere adjacency), evading the
verification H2 demands at the level of punctuation. Writing `because` forces you to confirm
it is a cause, and where the confirmation fails the content changes.

### The one shape to keep: `literal — gloss`, literal LEFT

Here the dash is a two-column layout, not prose punctuation:

```fsharp
/// `continue;` — re-enters the `While` trampoline after the parameter write-back.
```

A file of these reads as a table — the H16 shape at its most compact. Discipline: literal on
the LEFT, gloss on the right, ONE dash, and the right side must define rather than explain.
**Literal on the right means invert, and inverting usually shortens.**

### The three shapes to cut

- **Parenthetical pair.** Best precision, and it strands whatever follows the second dash:
  ```fsharp
  // Member functions emit after the class decls — they reference the classes via
  // `new`/match, and `const` arrows are not hoisted — and before the body.
  ```
  **A reliable detector and an unreliable prescription — do NOT reflexively delete the
  aside.** A dash pair interrupts a STRUCTURAL statement to insert the REASON; the structure
  is what the code below already says (H14, H3: delete outright), the reason is what the
  reader cannot recover.

  > **In a parenthetical pair, suspect the HEAD.** The aside is usually the only thing in the
  > comment that is not already in the source.

  A tendency, not a law (tally in `evidence.md`). Exception: **in a per-CASE doc neither half
  can go**, because the head identifies the case — fix by apposition, never deletion.
- **Trailing afterthought where the tail is the point.** Invert; the tail is the fact.
- **Line-initial dash** — the wrapped remainder of one of the above, and the strongest tell
  that the mark was reflex.

A reading tell, though NOT a signature (as a grep it measured only ~50% precision,
`evidence.md`): **a dash in a sentence that already carries a connective is a stacked
consequence**, the second link never named; it wants a full stop.

**Count occurrences, not lines.** `rg -c '—'` counts matching LINES, and a single-line
parenthetical pair holds two dashes — exactly the mode you most want to find. Use
`rg -o '—' | wc -l`.

**The cut rate per file is predictable from its punctuation convention — check it before
estimating.** A file whose docs already name by colon has no legitimate work left for the
dash; per-case one-liners over emitted output are mostly legitimate glossary; a codebase with
no gloss convention at all is nearly all defect (`evidence.md`).

**Message strings carry every mode a comment does, H19 included — but they are their own
pass**, because editing a string can move a test expectation: check what asserts on the text
first, then rerun the suites. A message often splices a phrase built elsewhere
(`sprintf "%s — '%s'" r.Description name`), so read the composed output, not the format
string; and `unreachable — <the actual fault>` is the string-literal form of the "cannot
happen" claim. Prose shipped to a RUNTIME consumer is a further case; see the
runtime-consumer section of `SKILL.md`.

## Where the rot accretes — the siting law

The essays sit at headers and boundaries — module headers, function docs, prologues — where
no single executable line keeps the prose honest, while per-arm one-liners stay short and
correct.

> **Essay length correlates with distance from a single executable line.**

Corollary: **a wrapper with nothing to say attracts a summary** — fabricated claims cluster
on one-line wrappers.

## Per-field docs fail differently from per-function docs

**Per-function** docs fail by self-congratulation (H1) and false path claims (H2);
**per-field** docs fail by narrating producer protocol (H12), because a field in isolation
has no behaviour to describe. H12 claims are the ones most worth the verification budget:
nothing in the type enforces them, and a true one is usually a type-refactor candidate.

**H12's disposition assumes the protocol is accidental, and sometimes it is a calling
convention.** *"Must be called on the STA thread"* restated on five COM interop types is
correct — no type encodes apartment affinity, and the restatement is the only warning the
caller gets. **Distinguish a protocol that happens to hold from one the platform imposes** —
for the second, count the restatements and propose the type, but do not cut the prose until
it exists.

## The audience test

Every mode except H3 is a comment addressed to someone deciding whether to accept the change,
rather than to someone modifying the code afterwards. The test: **the deleted material is
disproportionately about things that DID NOT CHANGE.** The same lens explains H6 — a doc
written against the DIFF ends up one declaration off once the diff lands — and why
type-definition files are worse: a comment written at a design fork has no reader afterwards.

## How comments go wrong after being written

A wrong comment is wrong; do not spend time deciding whether it was wrong when written. Three
rules change WHERE YOU LOOK:

**A moved comment is a NEW claim.** A relocating refactor is the moment a doc is least likely
to be checked and most in need of it, because the diff reads as a pure move. When you
summarise code you are moving, the source of truth is the CODE, never the doc that travelled
with it.

**Migrations leave a name trail — grep it in the same change.** A type migration touches no
comment, so every doc naming the old type keeps compiling and reads plausibly, especially
when the new names are near-twins (`TyUnion`/`FTUnion`):

```bash
grep -rInE '^\s*(///|//).*\b<OldName1|OldName2|…>\b' <the new domain's project>
```

Filter by which domain each file belongs to; do not mass-replace — in the old domain's own
project the names are still correct.

**`git blame` is not evidence of freshness** — it can date false lines AFTER they became
false. When you need history, `git log -S'<phrase>' --all` on the comment's SUBJECT is the
query that works.

**Self-contained comments rot more slowly**: a comment showing this file's own output can
only go stale when this file changes — which is when someone is looking at it.
