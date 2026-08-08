# Failure-mode taxonomy

Eighteen named modes, each with the shape, a verbatim example found in the wild, and a
disposition. Examples are real — H1–H16 come from the `Codegen.Js` overhaul, H17–H18 from a
later repo-wide vocabulary pass (see the section on those two, which have their own evidence
base and are the only modes whose disposition is not delete/verify/relocate).

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
| H18 | Overloaded term | one noun or verb naming several independent concepts across the tree: `head`, `receiver`, `binder`, `holder`, `spine`, `drain`, `face`, `harvest` | rename per concept, reusing the word the codebase already has; the number of distinct replacements measures the damage |

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
backends and the parser, ~800 file-touches across nine commits.

### H17 — the negation that stood for three verdicts

`names no X` occurred 64 times in 44 files and read as one fact. It was three:

- **Structurally has none — no lookup ever ran.** A counter-minted `NodeKey`, a virtual
  token, a provider layer with no leaf. Say **has no** / **carries no** / **occupies no**.
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

Eight words, each naming several independent things. The replacement count is the measure:

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

Every replacement is a word the codebase ALREADY used for that concept. None is a coinage,
and that is the acceptance test: **when a rename cannot find an existing word, the concept is
not modelled** — a type candidate, the same verdict the 3-line ceiling produces.

Two diagnostics that name the mode before any renaming:

- **The word is a metaphor, not a term of art** — `holder`, `spine`, `drain`, `face`,
  `harvest`. A picture accepts any concept that fits it, so it accretes.
- **The word IS a term of art, for something else** — `head` (cons), `binder` (monadic
  `'a -> M<'b>`), `receiver` (the OO sense), `arrow` (the JS function form). Used for their
  approximate meaning, these are worse than metaphors: a reader who knows the term is
  actively misled rather than merely uninformed.

> Those eight renames touched ~800 files because the vocabulary had reached the identifiers,
> and the comments only inherited it. **Catch it in the comment and you catch it before it is
> an API.**

And the reason this belongs in a COMMENT doc rather than a naming one: re-running the
signature after all eight renames had landed found three surviving sites, and every one was a
comment — `HOLDER-CLASS` in a doc whose own code already said `ModuleContainer`, `the whole
spine` beside the arguments it had been renamed to, a `SPINE` in a design note. A rename
reaches identifiers; the compiler makes sure of it. Nothing reaches the prose, so the retired
word survives exactly where it is least checkable and reads most authoritative.

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
