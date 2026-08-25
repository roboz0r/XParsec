# Evidence base

This file is for **revising the skill, not for using it**. Nothing here changes what a review
or a sweep does — the operational files carry their own calibration in place. What lives here
is where each rule came from, the raw measurements behind the short claims, and the negative
results, so a revision can tell a measured finding from a guess and does not re-derive what
has already been tried. Do not load it for a sweep.

## The three codebases

Two agent-written codebases, and the difference between them is what most of the caveats in
`SKILL.md` are for. A third, human-authored one is recorded at the foot of this section; it is
the reason the sweep now has a scoping rule before it has any rules about words.

1. **An F# compiler backend.** H1–H16 come from a full overhaul of one 20-file project;
   H17–H18 from a later vocabulary pass over that whole repo (~800 file-touches across nine
   commits); H19 from a punctuation pass over four already-swept files. Those last three are
   the only modes whose disposition is rephrase rather than delete.
2. **A C# MCP server over Excel COM, ~18k lines.** An independent replication.

**What replicated:** H4, H6, H11, H13 and H19, with fresh instances and no change of
disposition. The one-rule framing and the mode list reference no language feature, and
transferred intact.

**What did not:** H1 (self-congratulation) is almost absent from the C# tree. The mode may
need code that argues for its own correctness — a compiler's totality claims have no analogue
in interop code, where nothing is total. Be aware its grep signature also carries a `the one`
false positive that dominated the hits; guard it with `THE ONE\b`.

**What the second codebase changed**, all recorded in place in `SKILL.md` and `taxonomy.md`:
the 3-line ceiling is about derivability rather than length; H12 must distinguish a
platform-imposed calling convention from an accidental protocol; H2's named-owner predictor is
really about distance; the cross-reference prohibition inverts where the compiler checks
crefs; and the runtime-consumer audience had no instance in the first study at all. The
measurement needed fixing first — see the PROSE and DELIM measurements below.

**State the directional rules as heuristics, not laws.** They were measured in a language
whose compile order is forced to be dependency order. The mechanism — a consumer knows its
dependency, a dependency does not know its consumers — is general; the cheap way to read the
order is not.

**The ratio behaved as a symptom in both.** The C# project sat comfortably above target at
9.2:1 while 290 blocks ran past the ceiling and the longest hit 33 lines. Judging it by ratio
would have found nothing — which is the whole reason block length is the diagnostic.

### Third codebase: a long-lived, mostly human-authored C# library

A .NET WinForms/Excel tooling repo, roughly ten years old, where recent branches are
AI-assisted and the bulk is not. Scope was one branch's staged change: six files, 195 prose
lines down to 158, longest block 9 → 3, blocks over the ceiling 6 → 0.

**What replicated:** H5, H8 and H11 carried the whole sweep, with H8 the largest single
category — the same "here is why this design is right" shape, arriving in test-helper docs
rather than type definitions. H16 and H18 each produced one instance. The gate passed first
try on all six files, twice.

**What did not, and changed the skill:**

- **The `—` signature has near-zero recall here.** This tree spells the mark ` - `. One
  em-dash across six files, in a file the change had not touched; every genuine causal hedge
  was ASCII. The em-dash's value as an authorship tell is exactly why it misses in a repo
  where humans set the convention.
- **Authorship is a scoping question that comes before every rule.** See *Establish who wrote
  the comments* in `SKILL.md`. The modes are diagnostic of machine authorship, so on a mixed
  tree they point at the recent branches and nowhere else.
- **The sweep found a line that needed a comment and the procedure had no answer.** It does
  now: flag, do not add (`sweep.md` §5).
- **A quarter of apparent C# "code" was punctuation.** 899 of 3647 lines were nothing but
  `(){}[];,` — see the DELIM measurement below. The 6:1 target had been quietly generous to
  every C# file measured before this.
- **The ticket-ID rule needed splitting.** A JIRA ID in a commercial repo resolves for every
  reader, so it is not H11 at all; the defect is stamping it on every hunk. See `taxonomy.md`.
- **The ceiling's exemption is positional, not rhetorical** — I/O and interop boundaries,
  where the other side is non-deterministic and must be pinned by test rather than derived.
  Inside them the type candidate has a known shape: parse, don't validate.

## Measurements behind the in-place claims

Each entry backs a short claim that stayed in the operational files.

- **Late-written docs in large commits** (`SKILL.md`, *When to write it*). Five known-false
  comments traced to their commits: every one above-median by lines touched, four of five
  above the 80th percentile. Correlational, n=5, and large commits contain more comments to
  begin with.
- **Verification direction** (`SKILL.md`, *Verify before keeping*). 0 of 6 backward
  byte-identical clones were false; 2 of 3 forward claims were false.
- **PROSE vs COMMENT** (`SKILL.md`, Budget). On the 18k-line C# project, `<summary>`
  scaffolding lines were a quarter of both the comment total and the overall total.
- **DELIM** (`SKILL.md`, Budget). Over one C# test project, 899 of 3647 apparent code lines —
  a quarter of the denominator — were nothing but `(){}[];,`. Excluding them moved the
  aggregate from 14.5:1 to 10.9:1 and doubled the count of files under target. The same
  filter run over F# barely moved the number, which is the evidence it is a correction rather
  than a thumb on the scale.
- **Runtime-consumer prose volume** (`SKILL.md`). One MCP server measured ~467 lines of
  `[Description]` strings against 1432 lines of prose comments.
- **`git blame` freshness** (`SKILL.md` / `taxonomy.md`). Blame dated known-false lines nine
  days *after* they became false.
- **Migration name trail** (`taxonomy.md`). After `SemType` → `FrozenType`, the old domain's
  own project used the old names legitimately in 55 files, against 6 stale sites in the
  project that had migrated — the reason the grep filters by domain rather than
  mass-replacing.
- **H17 signature precision** (`taxonomy.md`). `names no X` occurred 64 times in 44 files.
  The first signature list measured 63/65 precision over the tree; the only false positive is
  `names` as a plural noun ("short names no longer resolve"). The plain-transitive family
  (`resolves no `, `mints no `, …) was carved out of that measurement afterwards and is
  unmeasured.
- **H18 per-word file counts** — the number of files each retired word touched, which is the
  measure of accretion: `head` 160, `receiver` 153, `binder` 142, `face` 83, `spine` 77,
  `holder` 75, `harvest` 53, `drain` 52, `leaf` 45, `tail` 44, `flow` 23, the verb `name` ~108
  hits over 67 files (unswept at time of writing). `arrow`, `contract` and `package` were
  caught at review before any measured sweep and carry no count.
- **H18 re-run after the renames landed**: 98 hits, of which 3 were genuine residue — all in
  comments — and the rest sanctioned survivors (monadic `binder` in the combinators, `cons
  spine` in list tests, a `Holder<'T>` test fixture). One triage pass.
- **H18 `flow` signature**: 72 occurrences, 27 on a legitimate-compound line, 45 residue of
  which 30 were defects — two in three, which is why the residue is read rather than treated
  as a hit list.
- **H19 density**: the worked project carried 147 em-dashes over 1038 comment lines — one per
  seven — *after* the H1–H16 sweep, which is the evidence the mode is an orthogonal axis
  rather than residue. Dashes are sparse per file (14 in 476 lines in one measured file), so
  reading all of them is cheap. One ephemeral plan doc under `docs/` alone held 197, hence
  the exclusion.
- **H19 parenthetical-pair signature**: 7 hits over the worked files, every one a genuine
  defect — but deleting the aside would have destroyed the only non-recoverable content in 4
  of the 7, and in 1 the correct edit was to delete the head. Running tally over the 7: aside
  is the keeper 3, aside droppable 2, delete the head 1, both required 1. n=7 — a tendency,
  not a law.
- **H19 window vs per-line grep**: 3 of the first 6 parenthetical pairs wrapped across lines.
  The per-line `—.*—` found matches in 6 files against the multiline window's 20 matches in
  12 files — it was missing about half.
- **H19 line-initial signature in `///` languages**: 0 hits across 146 comment-line dashes in
  the C# project, because a wrapped doc line re-opens with `///` and the dash never lands
  first on the line.
- **H19 end-to-end pass**: four files, 68 dashes → 15, cut rates 69–93%, net loss of ONE line
  across the four. Of the 53 fixed, roughly half were causal (*because*/*so*/*since*), 14 sat
  in parenthetical pairs, the rest were appositive (*namely*), and 2 were inverted
  glossaries. The spread is convention-driven: the file whose docs already had a colon-based
  naming convention cut 13 of its 14 dashes; the C# project with no gloss convention at all
  measured 146 comment-line dashes of which only 4 were the legitimate form.
- **H19 spaced-hyphen pass on the human-authored repo**: over the six swept C# files it was
  the only signature that found the causal hedges — the `—` signature returned one hit in the
  whole set, in a file the change never touched. Precision on a human tree lands in the low
  tens of percent.

## Negative results — tried, measured, rejected

Recorded so they are not re-derived.

- **A grep for connective-plus-dash** (a dash in a sentence that already carries *so*,
  *because*, *hence*, *since* — the "stacked consequence" tell):
  `(///|//).*\b(so|because|hence|since)\b.*—` and its mirror returned 12 hits over one
  project at ~50% precision, because every false positive is the legitimate glossary form
  with its connective *inside* the gloss (`` `(target = value)` — parenthesised, so it is
  safe as a comma-sequence operand ``). Half precision is no better than reading every dash,
  which is cheap. The tell stands as a reading heuristic only.
- **Mechanically sparing the `literal — gloss` form fails in both directions.** `[^`]\s—`,
  intended to skip glosses, spared 4 dashes in one file of which only 1 was a real gloss — a
  sentence that merely *ends* in an inline-code span defeats a test on the preceding
  character. Tightening it to literal-initial (``^\s*(///|//)\s*`[^`]+`\s*—``) then missed 5
  of the 9 genuine glosses in another file, which qualify the literal before the dash. The
  distinction is semantic — does the left side *name* a thing the right side defines, or make
  a *claim* the right side relates to — and no regex sees it. Hence the rule: grep the bare
  `—` and classify by reading.
- **Triaging comments by `git blame` recency** ranks a false comment as newer than the truth
  it contradicts (see the nine-days measurement above). `git log -S'<phrase>' --all` on the
  comment's subject is the query that works.
- **The H11 "no extension" qualifier** made the signature miss the worst case — a plausible
  `.md` filename reads as a repo document and survives review on that appearance alone. The
  signature now matches the snake_case prefix and ignores the extension.
