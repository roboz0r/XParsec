# Front-end gaps plan

Concrete in-repo work to close before IL emission can sensibly start.
Scope is the TAST surface (Desugar / Unification / Freeze) — nothing
here touches codegen. Order is **locked**: `A → D → C → B`.

The driving requirement is the canonical sample:

```fsharp
let inline sum xs = List.fold (+) 0 xs
let nums = [1; 2; 3; 4; 5]
printfn "%d" (sum nums)
```

Each item below is the smallest unit that closes one of the gaps that
sample exposes.

## A — List literal desugaring — **Done**

**Goal:** `[1; 2; 3; 4; 5]` types and freezes as nested
`UnionCons("Cons", …)` / `UnionCons("Nil", [])` over
`Microsoft.FSharp.Collections.list`.

**Scope:** a `Desugar` rule keyed on the list-literal CST node (and
the array-literal node — bundled per the decision below). No new
TAST shape needed: TAST's `TExpr.UnionCons` is the right node.

> **Reality vs. the original plan.** The plan claimed `FSharpList<_>` was
> already a registered union via the Phase 4 walk over `Common/list.fsi`
> — that turned out to be incorrect. The `'T list` abbreviation lives in
> `Clr/prim-types.fsi` (not `Common/list.fsi`), and the extractor
> currently skips its body because `caseName` returns `ValueNone` for the
> `(::)` constructor (no `Token.OpCons` entry in `opTokenToCompiled`).
> The implementation works around this by referring to the type by name
> (`TyRecord("Microsoft.FSharp.Collections.list", [elemTy])`) and
> hard-coding `"Cons"` / `"Nil"` as the case names in the freeze output.
> Fixing the registration so the type is properly minted from the
> provider is a follow-up (file under `extract-symbols-plan.md`).

**Dependencies:** none. Landed first so the test corpus can include
list-bearing programs that exercise everything downstream.

**Decisions made:**

- **Array literals `[| … |]` separately or bundled?** Bundled. Both
  literal shapes share `inferListLikeLiteral` /
  `translateListLikeLiteral`; arrays simply wrap the lowered list chain
  in `App(External "Microsoft.FSharp.Collections.ArrayModule.OfList", …)`
  so codegen sees a single lowering target.
- **Sequence expressions `seq { … }`?** Out of scope. CE desugaring is
  its own bigger problem and not needed for the sample.
- **Mismatched-paren diagnostic.** Added a Unification-pass backstop:
  `Unification.checkLiteralClose` walks the `rParen` of each list /
  array literal and emits an Error diagnostic on `ctx.Diagnostics`
  when the token is virtual (which is how `pEnclosed` recovers from
  `[| ... ]`, `[ ... |]`, `[| ...`, etc.). Parser diagnostics aren't
  visible to downstream semantic-analysis consumers, so without the
  backstop the malformed literal would type and freeze as if the
  source were correct.

**Annotation:** Desugar tags each list / array `EnclosedBlock` /
`EmptyBlock` node with `DesugaredForm.ListLiteral` or
`DesugaredForm.ArrayLiteral`. Unification and Freeze currently
dispatch on `ParenKind` directly (the annotation is redundant for
their decision); the side-table entry exists so a future consumer
that wants a single point of truth for "this node is a list literal"
doesn't have to re-derive it.

**Where the work landed:**

- `SemanticInfo.fs` — new `DesugaredForm.ListLiteral` /
  `DesugaredForm.ArrayLiteral` cases.
- `Passes/Desugar.fs` — `literalFormOfParen` + Visit-hook arm.
- `Passes/Unification.fs` — `inferListLikeLiteral`,
  `emptyListLikeLiteral`, `checkLiteralClose`.
- `Freeze.fs` — `listLiteralItems` + `translateListLikeLiteral`.

**Test gate (verified):** `let xs = [1; 2; 3]` types as
`TyRecord("Microsoft.FSharp.Collections.list", [TyConst "int"])` and
the frozen TExpr is the corresponding nested `UnionCons` chain.
`let xs = [|1; 2; 3|]` types as
`TyRecord("Microsoft.FSharp.Core.[]", [TyConst "int"])` and freezes
as `App(External "Microsoft.FSharp.Collections.ArrayModule.OfList",
<list-chain>)`. Empty literals (`[]`, `[||]`) leave the element
TyVar free for surrounding context to pin. `[1; true]` and the
mismatched-paren shape both surface diagnostics. New tests in
`DesugarTests.fs`, `UnificationTests.fs`, and `FreezeTests.fs`
cover each case.

## D — Resolved-type validation walker — **Done**

**Goal:** assert that no `TExpr.*` / `TPat.*` carries a `SemType`
with an unresolved `TypeVar` reachable via union-find Link chains
after Freeze. Failures surface as `Diagnostic`s on
`TastFile.Diagnostics`.

**Scope:** a sibling validation `ResolvedTypes` that
`Pipeline.analyseWithContext` invokes after `Freeze.run`. Cheap
walk: every TAST node has an inline `ty: SemType`; we chase each
through union-find Links and check it bottoms out in a concrete
shape (`TyConst`, `TyFun`, `TyTuple`, `TyRecord`, `TyUnion`, `TyClass`).

Stays on indefinitely. Codegen depends on it; turning it off lets
latent generalisation bugs surface as mysterious IL-emission errors
much later.

**Decisions made:**

- **Hard failure vs warning?** Hard failure. An unresolved TyVar at
  this point is a bug in inference, not a user error.
- **Free TyVars at generalised positions OK?** Yes — they're allowed
  when they're a quantified typar of an enclosing generalised scheme.
  The walker maintains a stack-scoped `HashSet<TypeVar>` (reference
  equality on the union-find root) and pushes/pops each scheme's
  `Quantified` roots around the corresponding `TDecl.Let` /
  `TExpr.Let` body. Looking up the scheme by the binding's
  `NamedSimple` `NodeKey` matches the key `Unification.generalise`
  writes into `ctx.Scheme`.
- **Diagnostic granularity.** One diagnostic per declaration,
  carrying the count of offending roots — the validator is a
  developer-facing sanity check, not a user-facing error, so per-node
  noise isn't needed. Attribution is the binding's `NodeKey` for a
  `NamedSimple` head, otherwise a synthetic fallback key.
- **Where to land in the pipeline.** As a sibling pass invoked by
  `Pipeline.analyseWithContext` after `Freeze.run`, with a final
  `ctx.Diagnostics` re-snapshot onto the `TastFile` so its findings
  reach `TastFile.Diagnostics`. The "end of `Freeze.run`" alternative
  was rejected because it conflated the tree-projection step with
  the validation step.

**Where the work landed:**

- `ResolvedTypes.fs` — new top-level module, slotted between
  `Freeze.fs` and `Pipeline.fs`.
- `Pipeline.fs` — invokes `ResolvedTypes.run`, then rebuilds the
  `TastFile` with the merged diagnostics.
- `passes.md` — pipeline table grew a row 7.
- `Passes/Unification.fs` — `inferTryWith` now pins the scrutinee
  to `TyConst "exn"` instead of a fresh TyVar so wildcard /
  variable arm patterns don't ship a free TyVar into the TAST.
  Surfaced by the validator's corpus-clean gate.

**Test gate (verified):** the corpus passes with the validator on
(369 / 370 tests, 1 skipped). One latent inference gap (try-with
arm scrutinee) was fixed before closing. New tests in
`ResolvedTypesTests.fs` cover the clean / quantified / synthetic-
free / synthetic-quantified cases.

## C — `inline` semantics — **Done**

**Goal:** `let inline sum xs = …` retains its body in the TAST for
per-call-site expansion at codegen time. Resolving SRTP / IWSAM
bounds against caller-side concrete types remains in `Unification`
(see [passes.md §Where `inline` fires](passes.md)).

**Scope:** a `bool` marker on `TDecl.Let`, the binding body retained
verbatim (Freeze already does this), and a codegen-facing
`inlineExpand` helper.

**Dependencies:** none structurally; landed after §A and §D so the
TAST had stabilised before the `TDecl.Let` arity bump.

**Decisions made:**

- **`Inline: bool` field, not a `TDecl.LetInline` case.** A `let` is a
  `let` whether or not it's inline; a separate case would duplicate the
  `binding * value * ty` payload and force every TAST consumer to handle
  two near-identical arms. The field is positioned **before** `ty` so
  `ty` stays last, matching the convention every other `TExpr` / `TPat` /
  `TDecl` case follows.
- **Source of the flag.** Freeze reads `b.inlineToken.IsSome` directly
  off the CST `Binding` in `translateModuleElem` — no new side table.
  (`NameResolution` already records the same bit on `ResolvedBinding`
  for Unification's SRTP path; the TAST marker is the codegen-facing
  companion.)
- **Retain bodies vs eager pre-monomorphise.** Retain bodies. Eager
  expansion forecloses the Fun-driven JIT devirt path
  ([function-representation-plan](function-representation-plan.md)) —
  every `inline`-d call site would lose the chance to dispatch
  through a constrained typar.
- **What `inlineExpand` does (and doesn't).** It performs *type*
  substitution only: the binding's quantified typars → the caller's
  concrete types, walked through the retained `value`. Beta-reduction
  of the resulting lambda against the actual arguments and NodeKey
  freshening across call sites stay codegen's responsibility — the
  helper's signature (`TDecl → SemType[] → TExpr`) carries no value
  arguments by design. A monomorphic binding round-trips its body by
  reference (zero-cost no-op). The `SemType[]` is positional: index `i`
  pins `quantifiedTypars declTy |> List.item i`, where
  `quantifiedTypars` collects free roots in first-occurrence pre-order
  over the decl's generalised type — the same order
  `Unification.generalise` quantifies in, so an order recovered from the
  frozen TAST lines up with the scheme that produced it.
- **Substitution depth.** v1 substitutes through one level of
  inlining at a time and lets codegen iterate. Nested inline-of-inline
  is fine: each callee's `Inline` flag drives its own expansion.
- **Marker scope is `TDecl.Let`.** The canonical sample's
  `let inline sum xs = …` is a module-level binding, and at module
  level even the `let inline succ x = … in succ 41` test-gate form
  lifts to a top-level inline `TDecl.Let` followed by the body as its
  own `TDecl.Expression` — so the marker covers it. A *genuinely
  nested* `let inline` inside an expression body would freeze to
  `TExpr.Let`, which doesn't carry the flag yet; deferred until a
  codegen thin-slice needs it (the `inline`-expansion slice).

**Where the work landed:**

- `Tast.fs` — `TDecl.Let` grew an `isInline: bool` field (before `ty`).
- `Freeze.fs` — `translateModuleElem` stamps `b.inlineToken.IsSome`.
- `Inline.fs` — new codegen-facing module: `quantifiedTypars`,
  `inlineExpand`, and the private `substType` / `substPat` / `substExpr`
  walk. Slotted right after `Tast.fs` (depends only on the TAST types +
  `UnionFind`).
- `ResolvedTypes.fs`, `TastShape.fs` (test DSL, now renders
  `let inline`), and the `TDecl.Let` pattern sites across the test
  suite picked up the extra field.

**Test gate (verified):** `let inline succ x = x + 1` types as
`int -> int`, freezes with `isInline = true` and its `fun x -> x + 1`
body retained, and `inlineExpand` returns that body verbatim (by
reference, no typars). The polymorphic `let inline id x = x` exposes one
quantified typar; `inlineExpand decl [| int |]` rewrites the body to a
fully-`int` `fun x -> x` without mutating the original (a second
expansion at `bool` still succeeds). The §C example
`let inline succ x = x + 1 in succ 41` splits into the inline `TDecl.Let`
plus a `succ 41` use expression, both diagnostic-clean. New tests in
`InlineTests.fs`.

## B — Printf format string — **Done**

**Goal:** `printfn "%d" 42 : unit` types correctly. The format spec
in the literal drives the curried result type — `"%d"` produces
`PrintfFormat<(int -> unit), TextWriter, unit, unit>` with the appropriate
curried final shape.

**Scope:**

- **Format-spec parser (reused, not re-implemented).** The lexer already
  classifies each `%[flags][width][.precision][type]` placeholder into a
  `FormatType` (`Lexing.lFormatPlaceholder`), so the canonical grammar is
  exposed once as the public `Lexing.parseFormatSpecifier : string ->
  FormatPlaceholder voption` and the semantic layer reuses it over each
  `StringPart.FormatSpecifier` token. The `PrintfSpec` module supplies
  only the semantic layer on top: the `FormatType -> SemType` typing map
  (`argType`) and the entry-point `families` table.
- **`PrintfFormat<_, _, _, _>` as a built-in type.** Modelled as
  `TyClass("Microsoft.FSharp.Core.PrintfFormat", [printer; state;
  residue; result])`. The arg list is derived from the format spec at
  the call site, not from the literal's apparent `string` type.
- **Special-case rule** in `Unification.inferApp`
  (`tryInferPrintfApp`) that inspects the format-string literal at the
  recognised entry point's argument position and drives the
  application's curried result type from the format spec.

**Dependencies:** none structurally. Landed after C as planned.

**Decisions made:**

- **Self-contained typing, not provider-signature-driven.** The rule
  computes the call's whole type itself (`PrintfSpec.appliedTypeOf`):
  the format literal types as `PrintfFormat<printer, …>` where
  `printer` is the curried arg-types folded onto the family's tail
  (`unit` for the writing families, `string` for `sprintf`), and the
  function node is stamped `leading… -> PrintfFormat<…> -> printer` so
  the normal app loop pins the value arguments. This sidesteps the
  fragile abbreviation chain (`TextWriterFormat<'T>` = `Format<…>` =
  `PrintfFormat<…>`) the real `printf.fsi` would otherwise force the
  extractor to expand. The entry points are still *registered* in
  `MockBuiltins` (generic `… -> PrintfFormat<'T,…> -> 'T` signature,
  built from the same `PrintfSpec.families` table) so plain name
  resolution and the non-literal fallback stay coherent.
- **Recognised entry points.** v1 covers `printf`, `printfn`,
  `sprintf`, `fprintf`, `eprintf` (plus the `…n` newline variants).
  Recognition keys on the **resolved source name** (last segment) via
  `PrintfSpec.tryFamily`, gated on the reference *not* having a local
  `Binding` entry — so a `let printfn = …` shadow is an ordinary
  function. This diverges from the plan's "keyed on the compiled name
  resolved by `IExternalSymbolProvider`": the real lib's `[<CompiledName>]`
  values are ambiguous across entry points (`sprintf` and `ksprintf`
  both compile to `PrintFormatToStringThen`, with *different*
  format-arg positions), and bare `printfn` doesn't resolve through
  the current extractor's compiled names at all. The short-name table
  is the unambiguous, position-aware key; the shadowing gate preserves
  the "respect name resolution, not raw syntax" intent.
- **Spec coverage keyed on `FormatType`.** Because the typing map keys
  on the lexer's `FormatType`, the representation keeps distinctions the
  earlier hand-rolled scanner lost: integer bases (`%x %o %B …`) stay
  separate `FormatType`s even though `argType` collapses them to `int`,
  and `%A` (`Structured`) vs `%O` (`Object`) are distinct (both type as a
  fresh typar, so codegen can re-read the `FormatType` to choose a
  structured printer vs. a virtual `ToString`). `%M` now types as
  `decimal` (free, once the spec is structured). Width / precision /
  flags are parsed but don't change the typing result (`*`-driven
  width/precision args aren't modelled — the lexer flags those as
  `InvalidFormatPlaceholder`). `%a` (callback) and `%t` (thunk) involve
  the `State`/`Residue` typars and aren't typed: `argType` returns
  `ValueNone`, so `appliedTypeOf` bails and the rule falls through.
- **Non-literal / untypeable format strings.** Interpolated literals
  (holes), lexer-error parts, and any format containing `%a`/`%t` make
  the rule defer to standard inference against the registered generic
  signature. Faithful `printfn fmt` (a pre-built `PrintfFormat`) types
  through that signature; an interpolated `printfn $"…"` or a `%a`/`%t`
  literal is a known v1 gap (it surfaces a mismatch diagnostic rather
  than special-casing the construct).
- **Freeze shape.** The format literal freezes to
  `TExpr.New("Microsoft.FSharp.Core.PrintfFormat", [Const(String text)],
  formatTy)` — the `new PrintfFormat<…>(value)` constructor — when its
  inferred type is the `PrintfFormat` `TyClass`. The entry point itself
  freezes as a plain `TExpr.External` carrying the source name (the
  existing `translateIdent` convention; the qualified compiled name is
  a separate, broader concern).

**Where the work landed:**

- `XParsec.FSharp/Lexing.fs` — `lFormatPlaceholder` refactored to a
  state-generic function (drops an unused `getUserState`) so it can run
  outside the lexer, and a public `parseFormatSpecifier : string ->
  FormatPlaceholder voption` wrapper. Carries a TODO weighing a
  `FormatPlaceholder` side-table on `LexBuilder` against on-demand
  re-parsing.
- `PrintfSpec.fs` — new module (slotted between `SemanticInfo.fs` and
  `ExternalSymbols.fs`): the `argType` typing map over `FormatType`, the
  `families` table, and the `formatType` / `printerType` /
  `appliedTypeOf` / `genericSignature` SemType builders.
- `ExternalSymbols.fs` — `MockBuiltins` registers the printf family
  from `PrintfSpec.families`.
- `Passes/Unification.fs` — `formatSpecifiers` helper (reuses
  `Lexing.parseFormatSpecifier` per `FormatSpecifier` token) +
  `tryInferPrintfApp`, invoked first from `inferApp`.
- `Freeze.fs` — `translateString` wraps the string `Const` in a
  `New PrintfFormat(…)` when the node's type is the `PrintfFormat`
  class.

**Test gate (verified):** `printfn "%d" 42` types as `unit` and
freezes as `App(App(External "printfn", New
Microsoft.FSharp.Core.PrintfFormat("%d")), 42)` — the format literal's
`PrintfFormat` carries `printer = int -> unit`. `sprintf "%d" 42`
types as `string`; `printfn "%s %d" "n" 42` curries the result through
both args; `printfn "%d"` partially applies to `int -> unit`;
`printfn "%d" true` surfaces a type-mismatch diagnostic; `fprintf w
"%d" 42` pins `w : TextWriter`; and a local `let printfn` shadow types
as an ordinary function. New tests in `PrintfTests.fs` (pure scanner,
typing, and freeze shape).

## What's *not* in this plan

- **E — Closure / eta lowering** moves to the codegen layer; it
  doesn't change what gets typed, only how it gets emitted. See
  [function-representation-plan](function-representation-plan.md).
- Anything bigger than the sample needs: CEs, async, classes,
  measures, records / DUs beyond what the lib already uses internally.
  Each is its own plan elsewhere in this directory.
- Optimisations of any kind. The front-end produces correct TAST;
  perf wins live at codegen or below.

## Cross-references

- [function-representation-plan](function-representation-plan.md) —
  why §C's "retain bodies" decision is non-negotiable.
- [passes.md](passes.md) — current pipeline; §C adds the `Inline`
  marker and §D adds a new validation slot.
- [extract-symbols-plan](extract-symbols-plan.md) §Phase 4 — the
  registered `FSharpList<_>` union §A piggybacks on.
- [generalisation-plan](generalisation-plan.md) — the existing
  generalisation work §D piggybacks on; the validator's "in-scope
  quantification set" comes from there.
