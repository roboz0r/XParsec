# Constant folding and attribute enforcement — follow-ups

Ephemeral: delete when the work lands. Carries the open threads of the deleted
`attribute-representation-plan.md` (landed 2026-08-28) and
`attribute-emission-and-constants-plan.md` (superseded by it).

Where it stands: `ConstFold.tryConstant` folds the attribute-argument constant domain over the
raw CST — a literal; a named constant through `AttributeFold.tryNamedConstant` (a `[<Literal>]`
value first, then an enum case, matching expression resolution's shadowing);
`|||`/`&&&`/`^^^` on integral constants of one width; unary minus. `AttributeFold` enforces
`AttributeUsage` target masks (FS0842) at every folded position, signature and impl path alike.
Gaps ptest-pinned already, needing no entry here: the bracket-sibling misfold and
`AllowMultiple` (`AttributeFoldTests`), union-case/enum-case rows and property-row placement
(`AttributeRowTests`).

## 1. The literal contract leg

A `.fsi` carries a literal's value (`[<Literal>] val Mask: int = 3`; omitting the value is
FS0876), and `ExternalMember.ConstValue` carries a `[<Literal>]` / C# `const`'s declared value
for elaboration to substitute at the access. But `AttributeFold.tryLiteralValue` reads only
`Resolution.LiteralValues` — the compilation's own module `let`s — so a literal declared in a
reference assembly does not fold in attribute or `[<Literal>]`-RHS position. The fix is a
second leg in `tryNamedConstant`: resolve the spelling through the external providers and read
`ConstValue`, the same seam `tryEnumCase` already crosses for an external enum case.

## 2. Wider fold domain (fsc parity)

fsc's literal-body domain folds `+`, `*`, `<<<` and string concatenation (F# 5+), and accepts
a named literal as an enum case's value; `ConstFold` rejects all of these as
`NotConstantExpression`. Constraint carried from the superseded emission plan: a primitive's
arithmetic is platform-defined (the JS bodies compute in float64 behind `Math.imul` / `| 0`),
so widen only where the targets agree by construction — integral two's-complement ops, `bool`,
string concatenation — and keep `float` / `float32` / `decimal` out. Folding stays
constant-context-only, total-or-error.

## 3. `Inherited` enforcement

`AttributeUsage`'s `Inherited` flag is decoded nowhere and has no ptest pin (`AllowMultiple`
has one).

## 4. Unfolded attribute positions

Attributes on typar defns, parameters, signature `ArgSpec`/`val`s, abstract member signatures,
exception decls, class `let`/`do` preambles and abbreviations pass unchecked and are not
stored in the frozen tree; a module-level `let` enforces targets
(`AttributeFold.enforceTargets`) but does not store. Each position needs the fold + store +
target check the type-defn positions have.

## 5. Optional / default parameter values

`OptionalDefault` is populated only by the external declaration readers, from already-constant
metadata. No local-source path folds a default-parameter expression. Verify what a
Vesper-authored optional-parameter default does today; if it is literal-only, route it through
`ConstFold` as the third consumer (after attribute arguments and `[<Literal>]` bodies).
