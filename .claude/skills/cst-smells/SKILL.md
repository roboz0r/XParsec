---
name: cst-smells
description: Detect structural smells in F# source using the repo's own CST parser (vestigial/splittable let rec and type-and groups, record-of-closures, tuple arity, oversized declarations, retired list idioms, shape nits), and mechanically restructure let rec groups. Use when hunting refactoring targets, before/after restructuring a recursive group, or when adding a new smell rule.
---

# CST smell analysis and rec-group restructuring

Two scripts, run with `dotnet fsi` (the sanctioned raw-`dotnet` exception):

- **`cstsmells.fsx`** — the detector. Parses every `.fs` file with the repo's own lexer and
  CST parser, runs a list of pluggable rules, prints findings sorted by weight (roughly:
  lines that could leave a recursive group).
- **`flattenrec.fsx`** — the transformer for `let rec ... and` groups. Rewrites a group
  into its SCC condensation: members in no cycle become plain `let`s, each genuine cycle
  becomes its own minimal `let rec ... and` chain, all in dependency order.

Both are proven: they restructured 20 groups (~4900 relocated lines) across the compiler
with zero behaviour change, verified by the full test suites.

## Running the detector

```
dotnet fsi --nologo .claude/skills/cst-smells/cstsmells.fsx                # all rules, src/
dotnet fsi --nologo .claude/skills/cst-smells/cstsmells.fsx -- rec-group   # one rule
dotnet fsi --nologo .claude/skills/cst-smells/cstsmells.fsx -- <dir>       # other root
```

Prerequisite: `XParsec.FSharp.SemanticAnalysis` built in Debug (the script `#r`s its
output dlls). Build it first via the `xparsec-dev` skill if the parse fails to load.

### Reading a finding

```
src\...\Engine.fs:487  members=10 lines=326  liftable=0 (0 lines)  cycles=1
src\...\Foo.fs:65  members=109 lines=2723  liftable=36 (955 lines)  cycles=3  <-- SPLITTABLE
```

- **liftable** — members in no cycle: candidates to become standalone declarations.
- **cycles** — genuine multi-member cycles. `cycles=1, liftable=0` is the healthy,
  irreducible shape and needs no action; the rec-group rule still lists it as inventory.
- **SPLITTABLE** — one chain fusing several independent cycles.
- **pinned** — operator / active-pattern members (rec-group rule only). Their references
  are not plain idents, so the analysis conservatively keeps them in the chain and never
  reports them liftable.
- **(nested)** — the group sits inside an expression or a class body, not at module level.

### Current rules

- **`rec-group`** — `let rec ... and` groups (module-level, expression-nested, class-body
  preambles, member bodies) of >= 3 bindings. Reference edges come from real ident nodes,
  so strings and comments cannot create or hide a reference.
- **`type-group`** — `type A = ... and B = ...` groups of >= 2 where the reference graph
  (declared type positions plus expression references in member bodies) is not one cycle.
  Only smelly groups are reported. A self-referential type is still liftable: a standalone
  `type` may reference itself. Cross-group cycles are impossible (type scoping is strictly
  top-down), so within-group analysis is sound.
- **`record-of-closures`** — records where over half the fields are function-typed: a
  disguised interface (root `CLAUDE.md` design rules). Walker/visitor records
  (`ExprWalker`, `TypeIter`-style) hit this rule by design; judge each finding.
- **`tuple-arity`** — tuple types of arity >= 3, `string * string`, and union cases with
  >= 3 positional fields, in annotations, signatures, fields and expression type
  positions. The "becomes a record" rule.
- **`decl-size`** — bindings spanning >= 100 lines or taking >= 5 parameters, with
  token-accurate extents. Thresholds are the two `declSize*Threshold` lets.
- **`list-idioms`** — `@`, `List.rev`, and `.Length` / `List.item` inside loop bodies.
  `.Length` is untyped here, so array and string hits are expected noise; the rule flags
  for reading, not mechanical fixing.
- **`nits`** — `xs.Length = 0` comparisons (match on the shape instead) and calls passing
  two or more boolean literals.

The per-file rules (`tuple-arity`, `decl-size`, `list-idioms`, `nits`) print one finding
per file with a `line N: <hit>` detail per site; `Weight` is the hit count, so they sort
below the big structural findings.

## Restructuring a `let rec` group

```
dotnet fsi --nologo .claude/skills/cst-smells/flattenrec.fsx -- "<file>:<headLine>" [more...]
```

`headLine` is the line of the `let rec` keyword (the detector's reported line). The script
is text-based (indentation extents, word-boundary name regex over comment-stripped
bodies); the CST detector is the oracle it is checked against. The loop:

1. Run the detector; pick a group.
2. Run `flattenrec.fsx` on it. **The printed member count must match the detector's** —
   a mismatch means the text parse misread the group extent; stop and inspect.
   Doc comments and attributes travel with their member. Expect small deviations in the
   lifted count (a "liftable" member that is self-recursive comes out as a standalone
   `let rec` instead — fine).
3. Build the project, `Format`, rebuild, run the affected test suites (see `xparsec-dev`).
   Insertions must ≈ deletions; binding-head count (`^\s*(let rec|let|and)\s`) must be
   unchanged.
4. Re-run the detector: the group should now report `liftable=0` or vanish.

Cautions:

- **Class-body groups**: reordering class-body `let`s is only safe when every member is a
  function. Check for value bindings (evaluated at construction, order-sensitive) first.
- **`src/Vesper.*` is off-limits** — FSharp.Core transliteration; the detector reports it,
  but do not restructure (see the root `CLAUDE.md`).
- Splitting a rec group makes each part generalise independently. If the build fails with
  new typar/inference errors, revert that file and report; do not patch annotations in.
- `type-group` findings have **no transformer yet** — split them by hand (reorder the
  definitions topologically, turn `and` into `type`), or extend `flattenrec.fsx`.

## Adding a rule

In `cstsmells.fsx`, a rule is:

```fsharp
type Rule = { Id: string; Summary: string; Run: FileCtx -> Finding list }
```

`FileCtx` gives the parsed `ImplementationFile`, the `Lexed` token stream, and a
`LineIndex` for offsets → lines. Write the `Run`, append to the `rules` list at the
bottom. Reusable machinery already in the script: `sccsOf` (Tarjan), `collectExprRefs`
(ident references + extent via `CstWalk.iterExpr`), `boundName` (a binding pattern's
name), `forEachTopLevel` (every top-level binding and standalone expression, including
class bodies and type extensions), `HitSet` / `hitFinding` (per-file hit aggregation
deduplicated by token offset), `tryFirstTokenOfType`, `stripParens` / `stripTypeParens`,
`longIdentParts`. For type positions use `CstTypeWalk.iterTypeDefnTypes` / `iterType`;
for module structure `CstModuleTree.implFileElems` (flattens namespaces and nested
modules).

Remaining scoped candidate: scope-aware rules via `CstWalk`'s `EnterBindingRhs` /
`EnterFun` / `EnterMatchArm` hooks (parameter shadowing an outer binding, a mutable whose
uses span far beyond its introduction). Anything needing name *resolution* (unused opens,
dead code) is out of scope until semantic analysis matures.

## Known analysis gaps (all err toward keeping a member in its group, or are caught by
the build step of the transform loop)

- rec-group: references made from match *patterns* (active patterns) and operator uses —
  handled by pinning those members.
- type-group: measure arguments, attribute arguments, and `member val` initialisers are
  not walked; a reference living only there is missed, so a member could be reported
  liftable that is not. The compile after a manual split catches it.
- `.fsi` signature files are not analysed.
