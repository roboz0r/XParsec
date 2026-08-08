---
name: comment-hygiene
description: Rules for writing and cutting code comments — use when adding doc comments, reviewing comments in a diff, or running a comment-density cleanup on a file or project. Encodes the failure modes agentic writing produces and what to delete outright.
---

# Comment hygiene

Derived from a full comment overhaul of `XParsec.FSharp.Codegen.Js` — 20 agent-written files,
2544 → 1033 comment lines with no fact lost, longest block 42 lines → 4. Every rule here has
a worked instance behind it; `taxonomy.md` has the instances.

## The one rule everything else follows from

> **A comment is addressed to someone who has ALREADY accepted the change and now has to
> modify the code — never to the reviewer deciding whether to accept it.**

That single framing generates nearly every prohibition below. An agent writing code is, in
effect, submitting it, so its comments drift into arguing that the work is correct: this is
the only place X happens, the alternative would have been worse, the match is total, this
case cannot happen. All of that answers *"is this change right?"*. None of it helps the next
person change it, and it is all deletable without losing a fact.

**Test before writing a line:** would this sentence be worth writing a year from now, to
someone who never saw the diff?

## Ask in this order

1. **Does this declaration need a comment at all?** If the name and signature say it, stop.
   A `private`/`internal` helper with an unambiguous name is the common case for stopping
   here. Deleting beats improving.
2. **Only then: is it self-contained at its site?** Show input → output. Do not characterise
   a transformation in the design's own nouns. Every back-ticked identifier should be
   findable in this file, or be a literal of the emitted output.
3. **Can it sit on one line of code?** If there is no single executable line for it to
   attach to, it is about to become an essay — and that is the signal it wants to be a type,
   not a longer comment.

Order matters. Rule 2 applied first manufactures noise: it turns a redundant doc into a
longer redundant doc with an example.

## When to write it

Write a declaration's doc **when you write the declaration**, not at the end of the change.

Late-written docs are composed from what is still in working memory rather than from the
code, and that is the derivation that produces every provenance failure below: prose
fabricated while the truth is visible in the same diff, blocks cloned from a sibling the
writer can no longer recall reading, files that ship self-contradictory.

Two rules follow:

- **If you are summarising code you did not just read, re-read it.** Especially when moving
  it — the doc travelling with the code is not evidence about the code.
- **Be most suspicious of comments written last, in a large change.** Every known-false
  comment traced in this repo came from an above-median commit, four of five above the 80th
  percentile by lines touched. Correlational, n=5, and large commits contain more comments to
  begin with — but the cost of re-reading before you write is one grep.
- **When you rename or migrate a type, grep the old name in comments as part of that change.**
  A migration rewrites many declarations and touches no comment, so every doc naming the old
  type keeps compiling and keeps reading plausibly. This is the cheapest moment to fix it —
  you have the mapping in your head and nobody later will have both names.

## Two habits to distrust as you write

Both are cheap to avoid at writing time and expensive to undo; the evidence, the word list
and the grep signatures are in `taxonomy.md` (H17, H18).

- **Do not negate the object.** "names no type", "resolves to no class", "maps to no slot"
  put the negation in the middle, so the reader parses grammar before concept — and one
  phrase ends up standing for three different verdicts. Negate the verb: *does not resolve to
  a type*. Then check WHICH is true — nothing was looked up (**has no**), the lookup missed
  (**does not resolve to**), or it hit the wrong kind (**is neither … nor …**). Picking the
  right one is where the false comments surface.
- **Do not reach for a metaphor when the codebase has a term.** `holder`, `spine`, `drain`,
  `face` and `harvest` each accreted several unrelated concepts. So did `head`, `binder` and
  `receiver`, which are worse, because they are real terms of art borrowed for their
  approximate meaning: a reader who knows the term is misled rather than merely uninformed.
  Reuse the word this codebase already uses for THIS concept. If there is no such word, the
  concept is not modelled — a type candidate, the same verdict the 3-line ceiling produces.

## Budget

- **Block length is the diagnostic; ratio is the symptom.** Judge by the longest block.
- **3 lines is a hard ceiling, and it is not negotiable by a good argument.** In the whole
  sweep, every block that wanted four got to three by cutting words, with no fact lost. A
  block that still wants more is a type waiting to be written.

  **Recording it as a type candidate does not license the lines.** Both happen: cut the block
  to three AND record the candidate. A block left long because "the refactor is the real fix"
  is the failure this rule exists to catch — the prose is what the refactor is supposed to
  delete, so leaving it in place leaves the debt invisible. Finishing a file with any block
  over three means the ceiling was not applied.
- **~6:1 code:comment.** Not a style-guide figure — it is what this repo measures wherever a
  human wrote the file. Two shapes legitimately measure lower and must not be cut to hit it:
  type-definition files whose per-case docs show emitted output, and public API surfaces
  where one doc line per function is the floor.

`<skill dir>/comment-density.ps1 -Path <dir>` reports both numbers, F#-aware, worst file
first. The scripts live beside this file — run them from there, with `-Path` pointing at the
code. They are not on the repo root.

## Delete outright — no verification needed

- Anything the name and signature already say.
- **Claims of ordinary good practice.** Single-source-of-truth ("the ONE place a key becomes
  a capability", "spelled ONCE so the three sites cannot disagree") and, the widest and most
  common, **totality**: "its dispatch is TOTAL over `ExprShape` with no `_`". An exhaustive
  match is how the code is written here; announcing it is not news. Same for immutability, no
  nulls, no downcasts. *A claim about the DATA is not this* — "the groups are non-empty by
  construction" explains why no empty branch exists and stays.
- **Rejected-alternative defence** — argues against a design the code does not have.
- **History and process** — "renamed from X", "nothing reads them yet", "a later change
  will…", milestone labels from a plan doc.
- **The argument FOR an invariant**, once the invariant itself is stated. Keep sentence one.
- **Routing-order narration** — re-listing the order of match arms or a probe chain. Control
  flow is the one thing source states exactly.
- **Cross-references of the form `Module.func`** — they rot on rename. State the fact.

## Verify before keeping — and only what survives

Deleting needs no research. Spend the whole verification budget on what you intend to keep;
checking a claim you then delete is wasted work.

**"Fails loudly" / "cannot happen" / "rejected earlier" is a claim, not a phrase.** Trace it
to the `failwith` or delete it. The predictor, which held across the sweep: **a failure with
a NAMED OWNER is almost always true** ("`addRef` fails loudly" — one grep, correct), and
**unnamed control flow is almost always false** ("it falls through and fails loudly" — a
guess about a path the author never opened). Trace either way; expect the named ones to
survive.

**Direction tells you where the falsehoods are.** Two failure modes point opposite ways:

- **Backward** — a file re-narrates the types it depends on, often byte-identical to a block
  in the dependency. Redundant, but true. **Delete on sight; do not spend time verifying.**
- **Forward** — a file narrates who fills its parameters, in what order, and what they
  guarantee. Nothing enforces it and it is a guess about a downstream consumer. **This is
  where to spend the budget.** Measured: 0 of 6 backward clones false, 2 of 3 forward claims
  false.

Compile position predicts nothing on its own; the `open` list does.

## Where wrong comments come from

A wrong comment is wrong — whether it was ever true changes nothing about what you do with
it. What is worth knowing is where they collect:

> **A moved comment is a NEW claim.** A refactor that relocates a doc block is the moment it
> is least likely to be checked and most in need of it — the diff looks like a pure move. And
> a refactor does not only carry old prose forward, it composes new prose FROM it, so a
> fresh-looking sentence can be wrong at birth. When summarising code you are moving, the
> source of truth is the CODE, never the doc that travelled with it.

> **A migration leaves a name trail.** Renaming a type rewrites declarations and touches no
> comment, so docs naming the old type keep compiling and keep reading plausibly. Grep the old
> name in comments as part of the rename — see `taxonomy.md`.

> **`git blame` is not evidence of comment freshness.** In the worked case blame dates the
> false lines nine days *after* they became false, so recency-based triage ranks the comment
> as newer than the truth it contradicts.

This is also the strongest argument for self-containment, stronger than readability: a
comment about another module's internals has no mechanism linking it to that module's
changes, while one showing this file's own output can only go stale when this file changes —
which is exactly when someone is looking at it. **Self-contained comments rot more slowly.**

## When the comment is genuinely for the reviewer

Sometimes it is — "I considered X and rejected it", "note the ordering here". That is real
communication, and it should not go in the persistent artefact. Write it in the strippable
form:

```
//FOR-REVIEW <text>              one line
(*FOR-REVIEW <text> *)           spans lines, nests, ordinary F#
```

`<skill dir>/review-comments.ps1` lists them, `-Strip` removes them and drops lines left
blank, `-Check` exits 1 for a pre-commit gate. A malformed block is an F# lexer error, so it
fails the build rather than going unnoticed.

## Files in this skill directory

Everything below is beside this file. Nothing it needs is on the repo root.

- **`taxonomy.md`** — the 18 named failure modes with verbatim examples, dispositions, and
  the grep signatures that catch most of them without reading any code. Load this when
  reviewing or cutting existing comments.
- **`sweep.md`** — the procedure for a whole-file or whole-project cleanup: the two-stage
  verification gate that makes comment edits provably code-preserving, batching to parallel
  agents, and measurement. Load this when the task is a cleanup rather than a single comment.
- **`comment-density.ps1`** — the measurement script. F#-aware, so `//` inside a string is
  not a comment and `(* … *)` nests.
- **`review-comments.ps1`** — lists, checks or strips `FOR-REVIEW` comments.

## Scope of the evidence

H1–H16 were measured on one F# project; H17–H18 come from a later vocabulary pass over the
whole repo (~800 file-touches, nine commits) and are the only modes whose disposition is
rephrase rather than delete. The reviewer-audience hypothesis and the mode list reference no
language feature and should transfer. The *directional* findings were measured in a language
whose compile order is forced to be dependency order with no forward references; the
mechanism behind them (a consumer knows its dependency, a dependency does not know its
consumers) is general, but the cheap way to read the order is F#'s. State the work-order rule
as a heuristic with its evidence named, not as a law.
