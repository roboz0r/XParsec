# `.fsi` shadowing and backend satisfaction — what is left

Working document, rewritten down once every step landed. Ephemeral: delete it when the two
follow-ups below are closed (the CODE is the canonical record of everything already done —
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

## Follow-up 1 — a `val` after a type body in a `.fsi` is swallowed (PARSER)

Found by Change A, and the reason `AssemblyFilesTests`'s hiding fixture declares its `val`
before its `type`. In a signature file:

```fsharp
module M =
    type Shown = { value: int }

    val shown: int -> int          // silently becomes a member of `Shown`
    val shown: x: int -> int       // instead recovers as `UnexpectedTopLevel`
```

`TypeExtensionElementsSignature.parseLight` (`SignatureParsing.fs:258-264`) reads its `with`
through `nextSyntaxTokenVirtualIfNot`, which never fails, so a record / union / `extern` body
unconditionally enters augmentation mode. `withContext` then anchors the `WithAugment` frame
on the NEXT token's column — the sibling `val`'s own column — so no offside check can reject
it. `val` is both a module-level element and a legal type-body element in a signature, which
is why only `.fsi` files can see this; the implementation side shares the shape but not the
token collision.

The fix is to anchor the augmentation past the `type`/`and` keyword's column, which
`pTypeGroup` has and does not thread into the body parser. Plain abbreviations are unaffected
(that branch attaches no extension at all — itself a divergence from the implementation
side). NO `.fsi` in the tree trips this today: they all declare types at namespace level and
`val`s inside a following `module`, and an attribute or `module` keyword makes the
augmentation parse fail and backtrack.

**It makes a class of signature unwritable, which raises the priority.** Working the `.fsi`
route through the CLR driver hit the collision head-on: a signature module that declares a
record AND a `val` over it has no legal ordering. `val` first cannot name the type — extraction
is single-pass and top-down, so the `val` reports "Type 'Point' could not be resolved during
contract extraction" — and `type` first swallows the `val` into the record's augmentation. Only
one of the two orders can be satisfied at a time. Until this is fixed, an in-assembly `.fsi`
can publish a record OR functions over it, not both, and both backend tests here are written
around the gap rather than through it.

## Follow-up 2 — the `let inline` typar exemption, now on two routes

`ConformanceTypars.checkFile` matches `IsInline = false`, so an inline binding's typar order
is checked nowhere. `typar-fsi-fs-faithfulness-plan.md` names this "the last thing standing
between this class of bug and the compiler", having already had one miscompile from it. The
in-assembly conformance check inherits the exemption rather than opening it.
