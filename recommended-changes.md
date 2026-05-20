# Recommended Changes — XParsec.FSharp

Distilled from the 12 gap-analysis documents in <D:/roboz0r/xparsec-gap-analysis>. Each item points
at a concrete file/line, says what to change, and why. Items are ordered by
expected impact on real-world F# code, not by spec line number.

The project's stated convention (see memory `feedback_relax_parser_defer_to_typecheck`,
`feedback_match_fsharp_grammar`) is to **mirror `pars.fsy` productions and defer
semantic rejection to the type checker**. So "the parser accepts X that F# rejects"
is usually *not* a bug. The items below are filtered against that policy:

- **Tier 1–2**: real semantic gaps — code that compiles in `fsc` but fails in XParsec.
- **Tier 3**: dead-code / AST cleanup — purely internal, no behaviour change.
- **Tier 4**: missing feature work (signature files).
- **Tier 5**: documentation only.
- **Out of scope**: items the gap analysis flags as "parser is over-permissive";
  per project convention these stay as-is unless the type checker can't recover.

---

## Tier 1 — Real correctness bugs in common F# code

### 1.1 Dynamic operator `?` and `?<-` are not parsed as infix

`ExpressionParsing.fs:2617` — `OpDynamic` is wired only as an atomic prefix for
optional argument `?ident`. There is **no** Pratt RHS handler for `expr ? expr`
or `expr ? ident <- expr`.

Real-world impact: any code using `FSharp.Data` row accessors (`row?Name`),
`Newtonsoft.Json`-style dynamic typing, dynamic dispatch on records, or
`System.Dynamic` integration will misparse. `obj?Member` currently becomes
`App(Ident "obj", [OptionalArgExpr(?, Ident "Member")])` — a structurally wrong
AST that downstream tools will reject or mishandle.

**Action**: Add an infix `?` operator to `rhsOperators[]`
(`ExpressionParsing.fs:1594`) at a precedence between `Dot` and `HighApplication`
(matches FCS — `?` is a dotted-style lookup). Produce `Expr.DynamicLookup(l, ?, r)`.
For the setter form, the existing `Assignment` operator at the right precedence
will compose: `obj?Name <- v` parses as `Assignment(DynamicLookup(obj, ?, Name), <-, v)`.
Add the corresponding AST cases to `Expr.fs`.

The `?ident` optional-argument atomic path (`pOptionalArgExpr`,
`ExpressionParsing.fs:2617`) must remain — it fires only when `?` is at expression
start (no LHS). The Pratt machinery handles the dispatch automatically.

### 1.2 Negative numeric-literal patterns

`PatternParsing.fs:502` (`pConstPat`) — `-1` in pattern position fails because
`-` is parsed as a prefix operator that does not exist for patterns.

Real-world impact: `match n with | -1 -> "sentinel" | _ -> ...` is idiomatic and
will not parse.

**Action** (two options, pick one):

1. **Lexer-level**: when the lexer is in pattern context, fuse a leading `-`
   immediately before a numeric literal into a single negative-literal token.
   Cleaner but requires context tracking the lexer doesn't currently do.
2. **Parser-level**: special-case `-` as a pattern-only prefix in
   `parseAtomic` / `parseAtomicBindingArg` (`PatternParsing.fs:618, 665`). When
   `-` is immediately followed by an `IsNumeric` token, consume both and emit
   `Pat.Const(Constant.Literal negativeLiteral)` with a fused span. This
   matches FCS's approach (`SynPat.Const` with a synthesised negative numeric).

Option 2 is the pragmatic fix — keep it inside the pattern parser without
disturbing the lexer.

### 1.3 `while!` (F# 7+) not in `kwPrefixRoutes`

`ExpressionParsing.fs:1902` — `kwPrefixRoutes` handles `let!`/`use!`/`do!`/
`yield!`/`return!`/`match!` but not `while!`.

Real-world impact: any `task { while! cond do ... }` or `async { while! ... }`
written against F# 7+ won't parse.

**Action**: Add a `KWWhileBang` route to `kwPrefixRoutes` mirroring the existing
`while` handler (`KWBody.pWhileExpr`, `ExpressionParsing.fs:948`). The existing
`Expr.While` AST case can carry the bang token via the keyword slot, or add a
sibling `Expr.WhileBang` if downstream consumers need to distinguish.

### 1.4 SRTP member-trait constraints don't accept `with get` / `with set`

`TypeParsing.fs:125–144` — `pConstraintMemberSig` does not parse a trailing
`with` clause.

Real-world impact: property-trait SRTP constraints like
`^T : (member Length : int with get)` fail to parse. F# accepts this; libraries
that constrain on indexer/property shape will hit it.

**Action**: In `pConstraintMemberSig` (`TypeParsing.fs:125`), after parsing the
type, optionally consume a `pWithClause` (already defined at line 216). Wrap in
`MemberSig.MethodOrPropSig` with the appropriate `getSet` slot populated
(currently always `ValueNone`).

While there: also reconsider flattening `curried-sig` into a `FunctionType`
chain (`TypeParsing.fs:127–131`). Tools inspecting `CurriedSig.args.Length` see
0 instead of the real arg-group count. Either parse a real `CurriedSig` here, or
document the AST-shape divergence prominently in `Expr.fs`.

### 1.5 Measure arguments in generic instantiations

`TypeParsing.fs:277` (`pTypeArg`) — only ever produces `TypeArg.Type`. The
`TypeArg.Measure` AST case (`Expr.fs:103`) is dead.

Real-world impact: a generic type whose typar is `[<Measure>]`-tagged
(`type Vector<[<Measure>] 'U> = ...`) cannot be instantiated with a measure
expression: `Vector<kg / s>` fails because `Type.parse` chokes on the bare `/`.

**Action**: This is hard without per-typar-slot Measure-vs-Type sort information
that the parser doesn't have. Options:

1. **Speculative parse** — try `Type.parse` first; on failure due to dangling
   `/`/`*`/`^`, retry as `Measure.parse |>> TypeArg.Measure`. Same pattern as
   `parseAbbrevOrImplicitClass` in `TypeDefnParsing.fs:1287–1310`.
2. **Always-measure** — try measure first; fall back to type. Simpler but
   risks misparse on `Foo<'a / 'b>` (which would fail as a type but parse as a
   measure quotient).

Option 1 mirrors the existing measure-abbreviation retry and is the recommended
approach. Wire `TypeArg.Measure` into the AST emit path.

---

## Tier 2 — Real divergences but in less common code

### 2.1 `additional-constr-expr` recursive forms

`TypeDefnParsing.fs:446` (`AdditionalConstrExpr.parse`) — only emits `Init` and
`SequenceBefore`. Three AST cases are dead:

- `AdditionalConstrExpr.SequenceAfter` (`Expr.fs:554`)
- `AdditionalConstrExpr.Conditional` (`Expr.fs:556–562`)
- `AdditionalConstrExpr.LetIn` (`Expr.fs:563`)

Spec describes `if/then/else`, `let in`, and `stmt ';' rest` as ctor body forms.
Codebases using complex ctor bodies (rare in modern F# but present in older
OO-style code, especially around WPF/WinForms) will not parse.

**Action**: Either implement the three recursive forms in
`AdditionalConstrExpr.parse`, or remove the dead AST cases. If implementing,
the grammar mirrors expression `let in` / `if/then/else` but with body type
`AdditionalConstrExpr` rather than `Expr` for the recursive slot.

Suggest implementing — the code would be a small stack mirroring the existing
`pInit + thenClause` shape. The dead cases imply intent.

### 2.2 Quotation pat-params for active patterns

`PatternParsing.fs:660ff` (`parseAtomicBindingArg`) — has no quotation arm.

Real-world impact: active patterns called with quotation arguments
(`(|MyPattern|_|) <@ x + 1 @> input`) fail. Listed in spec patterns:60–61. Rare
in practice but legitimate.

**Action**: Add `KWQuotationTypedLeft` / `KWQuotationUntypedLeft` arms to
`parseAtomicBindingArg`'s dispatch, routing to `pQuoteTyped` / `pQuoteUntyped`
(reusing the expression parsers — patterns reinterpret the quotation back to
expression at type-check time per spec).

### 2.3 Pure-juxtaposition measure abbreviations parse as `SuffixedType`

`TypeDefnParsing.fs:1287–1310` — the measure-retry trigger only fires on
dangling `/`, `*`, `^`, or `^-`/`^+`. So `[<Measure>] type X = kg m` produces
`Type.SuffixedType(NamedType "kg", LongIdent "m")` instead of
`Type.MeasureType (Measure.Juxtaposition …)`.

**Action**: In `parseAbbrevOrImplicitClass`, add a post-`Type.parse` check: if
the result is a `SuffixedType` chain of long-idents and the typedef carries a
`[<Measure>]` attribute, retry as `Measure.parse`. Or — simpler — always retry
as a measure when the typedef has `[<Measure>]` and the type parse leaves
*anything* parseable as a measure-product.

Low frequency in practice; flag as worth a corpus regression test before
investing in this.

### 2.4 `member P = expr` / `member P : T = expr` without `val`

`TypeDefnParsing.fs:307, 329` — these arms produce `Property` AST nodes for
forms that F# itself rejects (auto-properties require `member val`).

Worth a regression test against the corpus to confirm whether real `.fs` files
in the wild exercise this. If never exercised, tighten the dispatch to only
fire after `member val …` was seen. If exercised by code that `fsc` accepts
(maybe via signature files or an obscure F# version), document the relaxation.

---

## Tier 3 — Dead AST cleanup (no behaviour change)

These AST cases are defined but no parser path emits them. Either start
producing them (preferred where the syntactic intent is distinct) or delete.

### 3.1 Splice AST cases

`Expr.fs:353–354`:
- `Expr.ExpressionSplice` — `% expr`
- `Expr.WeaklyTypedExpressionSplice` — `%% expr`

Currently `%`/`%%` go through generic prefix-operator handling and produce
`Expr.PrefixApp`. Downstream consumers can't distinguish a splice from a
user-defined `%` prefix without re-inspecting the operator token.

**Action**: Special-case `Token.OpSplice` and `Token.OpSpliceUntyped` in
`pOperatorPrefix` (`ExpressionParsing.fs:1797`) — same pattern as `KWLazy` /
`KWAssert` / `KWFixed`. Produce the dedicated AST nodes.

### 3.2 `Expr.Null`

`Expr.fs:300` — never emitted; `null` becomes `Expr.Const(Constant.Literal …)`.

The pattern side does the opposite: `Pat.Null` is reachable, `Pat.Const` is not
used for `null`. **Make them symmetric** — pick one direction:

- Either route `null` through `Expr.Null` in `Constant.parse` (matches pattern
  side, slightly cleaner downstream consumers).
- Or remove `Expr.Null` and route patterns through `Pat.Const` instead.

I recommend keeping the dedicated `*.Null` cases and emitting both — `null` is
semantically distinct from a numeric/string literal and downstream tools
benefit from the explicit case.

### 3.3 `Binding.fixedToken`

`Expr.fs:184` — slot exists but parser never populates it. `fixed` is parsed as
a regular prefix operator producing `Expr.Fixed`.

**Action**: Either:

1. **Wire it up**: in `pLetOrUseBody` (`ExpressionParsing.fs:1125`), when the
   keyword is `use` and the RHS starts with `fixed`, peel off the `fixed`
   token, populate `Binding.fixedToken`, and pass the inner expression as the
   binding RHS. Spec restricts `fixed` to `use ident = fixed expr` (line 2680).
2. **Delete the slot**.

Option 1 is more spec-faithful and gives downstream consumers the structural
information. Either way, removes the current AST inconsistency.

### 3.4 Type AST dead cases

`Expr.fs`:
- `Type.IncompleteGenericType` (line 78) — `Foo<>` is emitted as `GenericType`
  with empty args.
- `Type.ConstrainedType` (line 82) — only `WhenConstrainedType` is emitted.
- `TypeArg.StaticParameter` (line 104) — type-provider static params not
  implemented (out of scope: type providers are a niche feature).

**Action**: Delete `IncompleteGenericType` and `ConstrainedType`. They have no
syntactic distinction from `GenericType` (empty) and `WhenConstrainedType`
respectively; the AST cases just create disambiguation cliffs for consumers.

`TypeArg.Measure` should *not* be deleted — it's part of the fix in §1.5.

`TypeArg.StaticParameter` deletion is fine for now; revisit if type providers
ever come into scope.

### 3.5 Additional ctor dead cases

See §2.1. Either implement the three forms or delete them.

### 3.6 `Pat.Struct` (1-element struct tuple fallback)

`PatternParsing.fs:73` (`completeStruct`), 588 (`pStructPat`) — only fires for
`struct(x)` (one element), which spec implies is invalid.

**Action**: Per project policy (defer to type checker), keep the parser
permissive but delete `Pat.Struct`. The fallback can produce `Pat.StructTuple`
with a one-element array — same shape as a 1-tuple, downstream rejection works
identically. Removes one AST case and the duplicate `pStructPat`.

### 3.7 `MemberDefn.parse` dead-code block

`TypeDefnParsing.fs:732–802` — 70 lines of commented-out alternative
implementation.

**Action**: Delete. Comment at line 525 documents *why* the dispatch chain
replaced it; the dead code is no longer needed for reference.

---

## Tier 4 — Missing feature: signature file (`.fsi`) parsing

`ProgramStructureParsing.fs:114–211` — `FSharpAst.parse` only ever returns
`ImplementationFile` or `ScriptFragment`. The `SignatureFile` and `ScriptFile`
arms are unreachable. `walkFSharpAst` (`AstTraversal.fs:2406–2413`) explicitly
notes `"SignatureFile: <not yet implemented>"`.

The AST in `Signatures.fs` (112 lines) and `ProgramStructure.fs:34–37` is
complete and mirrors the spec, but **every constructor is dead**.

This is the largest gap in the project. A `.fsi` file currently parses as a
soup of failed expressions because `val` isn't a recognised module element.

**Action**: Add `SignatureParsing.fs` mirroring `Signatures.fs`. The grammar is
a strict subset of the existing `module-elem` grammar — replace
`function-defn`/`value-defn` with `val curried-sig`, and per-`type-signature`
parsers are smaller variants of the existing `TypeDefn` parsers (no member
bodies, just signatures).

Sequence:

1. Add `ValSig.parse` in a new `SignatureParsing.fs` — `val mutable? access?
   ident typar-defns? : curried-sig`. Reuses `CurriedSig.parse`,
   `TyparDefns.parse`, `Access.parse` already in `TypeParsing.fs` /
   `PrimitivesParsing.fs`.
2. Add `TypeSignature.parse` mirroring `TypeDefn.parse` dispatch, but with
   member-sig elements only (no bodies). Most spec forms (`abbrev-type-signature`,
   `record-type-signature`, etc.) parallel existing `TypeDefn` shapes — copy
   the dispatch, replace body parsers with their signature variants.
3. Add `TypeSignatureElement.parse` paralleling `TypeDefnElement.parse` —
   member-sig, abstract-sig, override-sig, default-sig, static-member-sig,
   interface-sig, constructor-sig.
4. Add `ModuleSignatureElement.parse` paralleling `ModuleElem.parse`.
5. Add `ModuleSignature.parse`, `NamespaceDeclGroupSignature.parse`,
   `SignatureFile.parse`.
6. Wire into `pNormal` (`ProgramStructureParsing.fs:59–69`): if the file
   extension is `.fsi`, try `SignatureFile.parse` first.

Estimated work: ~600–800 LOC, mostly mechanical replication. Half the AST
machinery is already in place; the parsers themselves are smaller than their
implementation-file counterparts because there are no expression bodies to
descend into.

While here, decide on `ScriptFile` (`ProgramStructure.fs:43`):

- If it's meant to differ from `ImplementationFile` (e.g. `#load`/`#r` valid
  only in scripts), define how and wire up.
- Otherwise, collapse `ScriptFile` into `ImplementationFile` and remove the
  dead constructor.

---

## Tier 5 — Documentation

These are fine as-is per project policy but need a one-line note somewhere
visible so they don't get re-discovered as bugs.

### 5.1 Asymmetry: `Expr.Null` dead vs `Pat.Null` reachable

If §3.2 isn't done, document this in `Expr.fs` near both case definitions —
downstream consumers should not assume the expression-side conventions when
walking patterns.

### 5.2 `Pat.NamedFieldPats` / `UnionArgPat` split

`Expr.fs:407–461` — the `UnionArgPat.{Named,Positional}` shape is parser-side
machinery for mixed-positional-named ctor patterns (`Square(_, height = h)`).
Spec writes only fully-named-or-positional; the AST shape will surprise spec
readers. One sentence in `Expr.fs` near `UnionArgPat` would suffice.

### 5.3 `Split*` flag mechanism

`SplitRAttrBracket` (measure close) and `SplitPowerMinus` (`^-` exponent) are
two places in the parser that split a single lexer token into two virtual
tokens at parse time. Currently described in scattered comments
(`MeasureParsing.fs:125–140`, `ParsingHelpers.fs:576–584`,
`ConstantParsing.fs:36–53`). Add a short section to the existing parser README
or to `ParsingTypes.fs` near the `ParseState` definition explaining the
pattern; future contributors hitting the same lexer-greediness problem will
find it.

### 5.4 Five `TypeAnnotation` entry points

`ExpressionParsing.fs:228, 553, 1591, 1357, 2120` — five distinct parser sites
all converge on `Expr.TypeAnnotation`. The motivation (capture the colon's
column for offside) is explained in scattered comments. A header comment in
`ExpressionParsing.fs` near the first occurrence (`pBindingBody`) listing the
five sites and the reason would help future maintainers avoid adding a sixth
when they should be reusing one.

### 5.5 GADT-style union cases

`UnionTypeCaseData.parseGadtNary` / `parseGadtNullary`
(`TypeDefnParsing.fs:1066, 1077`) — F# 9+ feature. Note in `Expr.fs` near the
case definitions that this is post-snapshot.

### 5.6 Adjacency rule `Foo<int>` vs `Foo < int >`

`TypeParsing.fs` atomic long-ident arm — uses plain `pLessThan` without
checking for whitespace between `Foo` and `<`. F# uses lexical filtering to
enforce no-whitespace; if the lexer doesn't apply that rule (it doesn't, per
the gap analysis), we accept syntax F# rejects.

Two reasonable responses:

1. **Document as intentional**: per project policy, defer to type checker.
   Most callers don't write whitespace-separated generic args.
2. **Add adjacency check**: in `parseAtomic`'s long-ident arm, when peeking
   `<`, also check `isPrevTokenSyntax` (no whitespace between long-ident and
   `<`). Same gating mechanism `pHighPrecLParen` uses.

Recommend option 1 unless real corpus code surfaces the issue.

---

## Out of scope (per project policy "defer to type checker")

These are flagged in the gap analysis as parser over-permissiveness. Per
`feedback_relax_parser_defer_to_typecheck`, the parser deliberately accepts
syntactically-valid forms that the type checker will reject. Listing here so
future readers don't refile them as bugs:

- **`use rec`**, **`use … and …`** (`ExpressionParsing.fs:1080`) — accepted;
  semantic check rejects.
- **`use (x, y) = …`** (full pattern in `use`-binding) — accepted; type
  checker rejects.
- **Generic let in expression** (`let f<'T> x = …`) — accepted; spec rejects.
- **Top-level expression as module element** in `.fs` files
  (`DeclarationParsing.fs:273`) — accepted; type checker handles.
- **`enum-type-case` value as `Expr`** rather than `const`
  (`TypeDefnParsing.fs:1145`) — accepted; type checker validates.
- **`primary-constr-args` accepting non-simple patterns**
  (`TypeDefnParsing.fs:17`) — explicitly documented as deliberate
  (`Expr.fs:474`).
- **`Foo<>` empty generic args** — emitted as `GenericType` with empty args.
- **`null` as standalone type** (`TypeParsing.fs:318`) — reachable as
  `Type.Null`; type checker rejects in non-union contexts.
- **`struct()` and `struct(T)`** zero/one element — accepted.
- **`?ident` accepted in any pattern position** — F# only accepts it in
  member-arg positions.
- **`attributes pat` as atomic** — spec doesn't list it as atomic.
- **`[<>]` empty attribute set** and **whitespace-separated attributes
  `[<A B>]`** — accepted; harmless in practice.
- **Compiler-directive `#mybogus`** — accepted; whitelist is
  semantic-checker territory.
- **Mid-body `inherit` in class/struct/interface** — accepted as
  `TypeDefnElement.Inherit`; ought to be rejected by a semantic check.
- **`let rec a = … and b = …` for non-rec** — parser accepts; F# rejects.
- **`override val` / `default val` auto-property dispatch** — accepted; FCS
  behaviour unclear, worth a regression test but low priority.
- **`long-ident` as type name** (`type Foo.Bar = …`) — accepted; F# rejects.

---

## Suggested execution order

If picking items off this list:

1. **§1.1 dynamic operator** — highest correctness impact, smallest patch.
2. **§1.2 negative-numeric patterns** — second-highest impact, contained to
   `PatternParsing.fs`.
3. **§1.3 `while!`** — trivial addition to `kwPrefixRoutes`.
4. **§3.1 splice AST cases** — clean win, removes dead code.
5. **§3.7 dead-code block deletion** — pure cleanup.
6. **§1.4 SRTP `with get/set`** — moderate work, real impact.
7. **§1.5 measure args in generics** — moderate work, real impact.
8. **§3.3 `Binding.fixedToken`** — small win or small deletion, decide first.
9. **§3.4 type AST dead cases** — small cleanup, tightens AST surface.
10. **Tier 4 signature files** — large, do when there's a use case.
11. **Tier 5 documentation** — sprinkle into commits as you touch nearby code.

Items §2.1, §2.2, §2.3, §2.4 are corpus-frequency-dependent. Run a corpus
regression pass before investing — the gap analysis flags them as worth a
test, not as known live problems.
