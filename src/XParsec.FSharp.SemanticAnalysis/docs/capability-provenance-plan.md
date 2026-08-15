# `.fsi` shadowing and backend satisfaction — what is left

Working document, rewritten down once every step landed. Ephemeral: delete it when the
follow-up below is closed (the CODE is the canonical record of everything already done —
see [feedback_plan_docs_ephemeral]).

Every numbered step of this plan is DONE:

0. Relocating the SA tests that need a platform — 2026-08-12.
1. The capability query on the provider (Change B) — 2026-08-12.
2. Its representation axis and `Regions` — 2026-08-13. `tuple-platform-type-plan.md` carries
   what it left open.
3. **Change A — a file is published as SIGNATURES + BODIES, and a `.fsi` HIDES** — 2026-08-13.

## What Change A landed (context, not work)

`analyseAssemblyWith` takes `SourceUnit`s — an implementation, and optionally the signature
that publishes it — instead of bare implementation files. Each file is turned into two
objects: `FrozenSignature.toSignatures` (signatures alone) and `InlineBodies.collect` (the
splice templates, keyed by `SymbolKey` and nothing else). A unit with a `.fsi` is extracted
through the contract extractor, checked against the implementation, and its signatures
REPLACE the `.fs`-derived ones; the templates are layered back on either way. Both drivers
pass units, so a `.fsi` reaches the assembly pipeline for the first time.

## Follow-up — the `let inline` typar exemption, now on two routes

`ConformanceTypars.checkFile` matches `IsInline = false`, so an inline binding's typar order
is checked nowhere. `typar-fsi-fs-faithfulness-plan.md` names this "the last thing standing
between this class of bug and the compiler", having already had one miscompile from it. The
in-assembly conformance check inherits the exemption rather than opening it.
