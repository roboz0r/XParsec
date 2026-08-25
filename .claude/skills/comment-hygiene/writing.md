# Writing comments

These rules only work if they are in context when the comment is being written — a review or
a sweep loads too late to prevent the defect. **Copy the section below into your `CLAUDE.md`
or rules file.**

---

## Writing Comments

Do not write the case for why the code is right. Assume the reader is a caller looking for a
contract, not a reviewer looking for a justification.

Seven constructions to stop producing:

1. **Definition by derivation** — Defines a type by the operation that made it, forcing the
   reader to replay a procedure. Define a type by what it *is* and/or what it *guarantees*.
   (Functions are the exception: a function's doc SHOULD say what it computes).
   - *DON'T:* "A compilation's defined symbols, narrowed to those..."
   - *DO:* "The subset of compilation symbols referenced by..."
2. **Premise-then-inference (Defensive Writing)** — Do not state a premise just to prove the
   conclusion. State the consequence alone, with committed modality (*will*, *guarantees*).
   The caller wants the contract asserted, not proved.
   - *DON'T:* "Parsing consults no other symbol, so two compilations that agree here parse
     identically."
   - *DO:* "If two compilations share this exact subset, they will parse identically."
3. **Object-extracted relative clauses with heavy possessive subjects** — The verb lands
   last, the subject is a two-level possessive, and the reader has to re-read. Use a reduced
   passive.
   - *DON'T:* "those one file's `#if` lines name" / "branches no define set makes active"
   - *DO:* "referenced by a file's `#if` directives" / "inactive branches"
4. **Exclusion framing** — Forces the reader to hold a universe and subtract. Assert
   positively.
   - *DON'T:* "consults no other symbol" / "drops nothing parsing can read"
   - *DO:* "depends exclusively on" / "retains all parsed elements"
5. **Anthropomorphism & Coined Verbs** — Do not assign cognitive actions to code processes,
   and avoid spatial/temporal deixis.
   - *DON'T:* "parsing *consults*", "agree *here*", "to hand", "name" (as a verb)
   - *DO:* "evaluates", "share", "available", "referenced by"
6. **Emphatic singularity** — Marks a singularity nobody questioned, which costs a referent
   re-establishment in every later sentence.
   - *DON'T:* "that file" / "one file's"
   - *DO:* "the file" / "a file's"
7. **Restating the type signature** — A member whose meaning is exhausted by its name and
   type gets NO comment. An `Empty` static needs nothing. Do not explain that a type is a
   `Set` if the F# type signature already says `Set<_>`. Write type annotations on parameters
   where the function's inferred type is non-obvious.

**Why:** The underlying instinct of an AI is to write comments as an argument for the design
instead of a description of the value. Exclusion framing, premise-then-inference, and
derivation histories are symptoms of trying to persuade someone the code is correct.

**How to apply:** After drafting any doc comment, check whether it would survive the code
being obviously correct. If a clause only exists to prove the code is right, delete it. Then
check for a trailing verb in a relative clause, cognitive verbs assigned to systems, and the
words "no/nothing/never".

**When the comment genuinely IS for the reviewer** — "I considered X and rejected it", "note
the ordering here" — that is real communication, but it does not go in the persistent
artefact. Write it in the strippable form, at the moment you would have written the plain
comment:

```
//FOR-REVIEW <text>              one line, any language
(*FOR-REVIEW <text> *)           F# only — spans lines, nests, ordinary F#
```

The comment-hygiene skill's `review-comments.ps1` lists them, strips them, and gates a commit
on none remaining.

---

## How the constructions map to the taxonomy

The seven constructions and the nineteen H-modes are the same audience error seen from two
sides: these rules stop the prose being written; the modes catch what was written anyway.
Where a construction has a retroactive twin, the grep signature for the mode doubles as a
self-check after drafting.

| construction | retroactive twin |
| --- | --- |
| 1 Definition by derivation | no direct mode — nearest kin is H16 (the derivation is usually in another file's nouns) |
| 2 Premise-then-inference | H5 (argument for a stated invariant) |
| 3 Heavy relative clauses | none — purely sentence-level, caught only at writing time |
| 4 Exclusion framing | H10 (doc-by-negation), H17 (collapsed verdict) |
| 5 Anthropomorphism / coined verbs | H18 (the verb `name`; metaphors generally) |
| 6 Emphatic singularity | H1 (self-congratulation — "the ONE place") |
| 7 Restating the type signature | H3 (signature restatement) |

Constructions 1, 3 and 6 have no grep and no mode: they produce prose that is merely hard to
read rather than false or redundant, so a sweep leaves it alone (on a human-authored tree,
deliberately — see *Establish who wrote the comments* in `SKILL.md`). Writing time is the
only time they are cheap to fix.
