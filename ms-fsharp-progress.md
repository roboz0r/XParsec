# Corpus Progress Report

## Corpus Test Infrastructure

A subprocess-based corpus test parses files from a configurable subdirectory of `test/XParsec.FSharp.Tests/data/`. Each file is parsed in its own process to isolate StackOverflowExceptions (which crash the entire process and cannot be caught in .NET). The MS F# compiler corpus (249 files) lives in the `corpus/` subdirectory.

**How to run:**
```
dotnet build
./test/XParsec.FSharp.Tests/bin/Debug/net10.0/XParsec.FSharp.Tests.exe --corpus corpus
```

The `--corpus <subdir>` flag accepts any subdirectory name under `data/`.

**Outputs:**
- `test/XParsec.FSharp.Tests/data/<subdir>/REPORT.txt` — summary of clean/diagnostic/error counts per file
- `test/XParsec.FSharp.Tests/data/<subdir>/*.fs.parsed` — full AST and diagnostics for every corpus file. These are written during each `--corpus` run and can be read directly (via `grep "[error]"` etc.) to investigate diagnostics without re-parsing individual files.

**Infrastructure files:**
- `TestHelpers.fs`: `corpusTestData`, `tryParseCorpusFile`, `CorpusParseResult` type
- `CorpusReport.fs`: `CorpusResult` type, `printReport` function
- `Program.fs`: `--parse-file <path>` (subprocess mode), `--corpus <subdir>` (runs all files in subdir)

## Bug Fixing Process

The workflow for finding and fixing parsing bugs uses the corpus as a guide and the "Step Debugging Test" as the primary investigation tool.

### Step 1: Identify candidates from corpus report

Run `--corpus` and read `REPORT.txt`. Pick files with low diagnostic counts (DIAG 1-5) — these are most likely to have a single root cause that's easy to isolate.

### Step 2: Investigate with the Step Debugging Test

Copy the corpus file to `test/XParsec.FSharp.Tests/data/manual/file.fs`, change the `ptest "Step Debugging Test"` to `ftest` in `ParserTests.fs`, and run tests with `-UpdateSnapshots`. This generates `file.fs.parsed` containing the full AST and diagnostics with exact positions.

### Step 3: Locate the diagnostic in source

Read the `[error]` entries in the `.parsed` file. Each includes a position (line/col) and error message. Cross-reference with the source file to see what syntax is failing.

### Step 4: Create a minimal reproduction

Write a small `.fs` file in `test/XParsec.FSharp.Tests/data/` (e.g., `222_primary_constructor.fs`) that reproduces just the failing syntax. Unfocus the Step Debugging Test, run tests with `-UpdateSnapshots`, and verify the repro shows the same diagnostic.

### Step 5: Fix and verify

Make the parser change, build with `./claude_tools.cmd -Action Build -SourceProject "XParsec.FSharp"`, then run tests with `-UpdateSnapshots`. The repro test should now parse cleanly (no diagnostics). All existing tests should still pass.

### Step 6: Corpus impact check

Rerun `--corpus` and compare with the previous `REPORT.txt`. Note: fixing one issue often exposes others — files that previously failed early now parse further and may hit different pre-existing bugs (especially the try/with offside issue). CRASH count may increase as deeper parsing triggers stack overflows in large files.

### Key tips
- **Don't rerun corpus repeatedly** — read `REPORT.txt` instead
- **Files with DIAG(1)** often share the same preamble issue — check if the error position is at the same offset across files
- **Always unfocus** the Step Debugging Test before running the full test suite

## Bugs Fixed This Session

### 1. Lexer: `%%` escape in interpolated strings (Lexing.fs)

**Symptom:** `$" %% "` failed to lex.
**Root cause:** `pFormatSpecifierTokens` used `many1Chars (pchar '%')` which consumed all `%` chars, then called `skipN percents.Length` which double-advanced the reader past the closing `"`.
**Fix:** Changed to `lookAhead (many1Chars (pchar '%'))` so the chars aren't consumed by many1Chars, then explicit `skipN` to advance correctly. The `while` loop was also extracted out of the `parser { }` CE due to a ParserCE bug with mutable variables in while loops.
**Test:** `data/lex-only/lo_210_interp_escape_percent.fs`

### 2. Lexer: `"` inside `$"""..."""` triple-quoted interpolated strings (Lexing.fs)

**Symptom:** `$"""value="{x}" done"""` failed to lex at the `"` after `}`.
**Root cause:** The dispatch table sent `'"'` in `Interpolated3String` context to `pInterpolated3EndToken` which requires `"""`. A lone `"` or `""` matched nothing — the fragment parser also excludes `"`.
**Fix:** Added `pInterpolated3QuoteOrFragment` parser that tries `"""` (close) first, falling back to treating `"`s as `Interpolated3StringFragment` content.
**Test:** `data/lex-only/lo_211_interp_triple_quote_embed.fs`

### 3. Parser: Prefix-only operators in function application (ExpressionParsing.fs)

**Symptom:** `f !!a` threw `InvalidOperationException: No operator handler for precedence level Prefix`.
**Root cause:** Two issues:
1. The Pratt RHS parser's `handleToken` function sent ALL operators to `getRhsOperatorHandler`, but LHS-only precedence levels (Prefix, Function, If, Let, etc.) have no RHS handler registered, hitting the `invalidOp` default case.
2. `pApplication.isAtomicExprToken` didn't recognize prefix-only operators as valid starts of application arguments.

**Fix applied (two parts):**

**Part A - RHS rejection (line ~693):** Added a guard in `handleToken` to fail for LHS-only precedence levels:
```fsharp
| _ when (match opInfo.Precedence with
          | PrecedenceLevel.Prefix | PrecedenceLevel.Function
          | PrecedenceLevel.If | PrecedenceLevel.Let
          | PrecedenceLevel.As | PrecedenceLevel.When
          | PrecedenceLevel.PatternMatchBar | PrecedenceLevel.Parens -> true
          | _ -> false) ->
    fail (Message "LHS-only operator cannot appear in infix position")
```

**Part B - pApplication (line ~600):** Changed `isAtomicExprToken` to recognize prefix-ONLY operators:
```fsharp
| ValueSome opInfo ->
    // Only prefix-ONLY operators (Precedence = Prefix) start application args.
    // Optionally-prefix operators like +/- (CanBePrefix but Precedence != Prefix)
    // must remain infix in application context (e.g. `f + g` not `f (+g)`).
    opInfo.CanBePrefix && opInfo.Precedence = PrecedenceLevel.Prefix
```

**IMPORTANT distinction:**
- `!!`, `~~~` → `Precedence = Prefix`, `CanBePrefix = true` → prefix-ONLY → should be treated as atomic arg in application
- `+`, `-`, `%` → `Precedence = InfixAdd/InfixMultiply`, `CanBePrefix = true` → optionally-prefix → must remain infix in application

The naive fix `opInfo.CanBePrefix` caused massive regression because `f + g` was parsed as `f (+g)` instead of infix addition.

**Test:** `data/212_prefix_op_in_application.fs`

### 4. Parser: ADJACENT_PREFIX_OP in function application (ExpressionParsing.fs) — RESOLVED

**Symptom:** `g -x` parsed as infix `g - x` instead of `g` applied to `(-x)`.
**Root cause:** `isAtomicExprToken` only recognized prefix-only operators (`Precedence = Prefix`). Dual-use operators like `-`/`+` were always treated as infix in application context.
**Fix:** Added `isAdjacentPrefixOp` implementing F# spec 3.8.1. Checks the raw token stream for adjacency: if the operator has no whitespace after but whitespace before, it's an ADJACENT_PREFIX_OP and starts an application argument. All-adjacent cases (`a-b`) correctly remain infix.
**Test:** `data/213_adjacent_prefix_op.fs`

### 5. Parser: RefParser initialization order (ProgramStructureParsing.fs) — RESOLVED

**Symptom:** `[<RequireQualifiedAccess>] module internal Foo` parsed as `AnonymousModule > ModuleDefn` with a spurious `Expected '=' after module identifier` diagnostic, instead of `NamedModule`.
**Root cause:** `pNamedModule` is a static value in `ProgramStructureParsing.fs`. Its body uses `Attributes.parse`, which internally calls `refObjectConstruction.Parser`. But `ObjectConstruction.init()` (in `ExpressionParsing.fs`) hadn't been triggered yet, so the `RefParser` was still set to the default `fail` parser. `Attributes.parse` silently failed inside `opt`, causing `pNamedModule` to fail and fall through to the anonymous module path.
**Fix:** Added `do ObjectConstruction.init()` in `pNamedModule` to force initialization. Also changed `RefParser` default from `fail` to `invalidOp` so uninitialized refs crash immediately instead of silently failing.
**Debugging note:** Added `TraceEvent.Message` case and `trace` helper in `ParsingHelpers.fs`, and added `TokenConsumed` tracing to `consumePeeked` for better trace visibility.
**Test:** `data/214_module_internal.fs`

### 6. Parser: Single-case DU without leading `|` (TypeDefnParsing.fs) — RESOLVED

**Symptom:** `type Foo = Foo of int` parsed as type abbreviation `type Foo = Foo`, then `of int` was unexpected.
**Root cause:** In `parseBody`, the `| _` branch (for tokens after `=` that aren't `|`, `{`, `struct`, etc.) went directly to `parseAbbrevOrImplicitClass` without attempting union parsing.
**Fix:** The `| _` branch now tries `UnionTypeCases.parse` first via `choiceL`, then falls back to `parseAbbrevOrImplicitClass`. A guard rejects single nullary cases (`type Foo = Bar`) since those are type abbreviations, not DUs.
**Key distinction:** `type Foo = Bar` → abbreviation (single nullary case rejected). `type Foo = Bar of int` → union (nary case accepted). `type Foo = A | B` → union (multiple cases accepted).
**Test:** `data/215_single_case_du.fs`
**Corpus impact:** +3 clean files (PathMap.fs and others)

### 7. Lexer: Standalone `#` token for flexible types (Lexing.fs, KeywordParsing.fs) — RESOLVED

**Symptom:** `#seq<int>` in type annotations failed to parse. `BinaryResourceFormats.fs` had 1 diagnostic.
**Root cause:** The lexer's `pHashToken` fallback emitted `Token.ReservedIdentifierHash` for non-directive `#`, but `pHash` in `KeywordParsing.fs` expected `Token.KWHash`. The `KWHash` token was never produced by the lexer.
**Fix:** Changed `pHashToken` fallback from `Token.ReservedIdentifierHash` to `Token.KWHash`. Updated `KeywordTests.fs` to expect `KWHash`.
**Note:** `ReservedIdentifierHash` is still used for identifier suffixes (`atomic#` etc.) in the identifier lexer — that's a separate code path.
**Test:** `data/216_flexible_type.fs`
**Corpus impact:** +1 clean file (BinaryResourceFormats.fs). TypeHashing.fs moved from DIAG to CRASH (deeper parsing now triggers pre-existing stack overflow).

### 8. Parser: `#nowarn` / `#warnon` directives (ParsingHelpers.fs, ParsingTypes.fs) — RESOLVED

**Symptom:** `#nowarn "9"` caused `UnexpectedTopLevel` parse error. Affected 5+ corpus files.
**Root cause:** The lexer already emitted `Token.NoWarnDirective` / `Token.WarnOnDirective` tokens, but `nextNonTriviaTokenImpl` didn't handle them. They fell through as regular tokens.
**Fix:** Added `processWarnDirective` function in `ParsingHelpers.fs` that:
1. Scans the directive line for warning codes (bare integers like `#nowarn 6` or strings like `#nowarn "9"`)
2. Accumulates `WarnDirective` entries in a new `ParseState.WarnDirectives` field
3. Advances the reader past the directive line
4. Tail-calls back to `nextNonTriviaTokenImpl`

Added `WarnDirective` struct type and `isWarningSuppressed` query helper in `ParsingTypes.fs`.
Wired `NoWarnDirective`/`WarnOnDirective` dispatch into `nextNonTriviaTokenImpl` (after `EndIfDirective`, before trivia check).
**Test:** `data/218_nowarn_directive.fs`
**Corpus impact:** +3 clean files (ControlledExecution.fs, MutableTuple.fs, Nullable.fs)

### 9. Parser: Point-free active pattern definitions (ExpressionParsing.fs) — RESOLVED

**Symptom:** `let (|PointFree|) = List.last` failed to parse. Parameterized active patterns like `let (|Last|) xs = ...` worked fine.
**Root cause:** `Binding.parseFunction` used `Pat.parseAtomicMany1` (requires one or more argument patterns) unconditionally. Point-free operator definitions have zero arguments, so `many1` failed. The fallback `Binding.parseValue` also failed because `Pat.parseAtomicOrTuple` dispatched `(` to `pParenPat`, which tried to parse `|PointFree|` as a regular pattern — and `|` isn't a valid pattern start.
**Fix:** In `parseFunction`, use `many` (zero or more) for operator definitions (`ParenOp`/`StarOp`) while keeping `many1` for plain identifier bindings. This correctly handles `let (|X|) = expr` alongside `let (|X|) y = expr` without changing parsing of `let x = 1` (which still goes through `parseValue`).
**Test:** `data/220_active_pattern_defn.fs`
**Corpus impact:** 1 file recovered from CRASH to DIAG (deeper parsing now succeeds before other issues). SynPat.fs and SynExpr.fs should have fewer diagnostics.

### 10. Parser: Optional parameter `?` in primary constructor patterns (PatternParsing.fs) — RESOLVED

**Symptom:** `type MyClass(?label: string) = ...` failed to parse. The `?` token was skipped.
**Root cause:** `PatternParsing.parseAtomic` had no dispatch entry for `Token.OpDynamic` (`?`). The `?` prefix for optional parameters was simply not handled in pattern parsing.
**Fix:** Added `Pat.Optional` case to the `Pat<'T>` AST (in `Expr.fs`), `pOptionalPat` parser in `PatternParsing.fs` dispatched on `Token.OpDynamic`, and visitor support in `AstTraversal.fs`.
**Test:** `data/222_primary_constructor.fs`
**Corpus impact:** 2 files moved from CRASH to DIAG (deeper parsing now succeeds).

### 11. Parser: Parenthesized operators `(=)` in expressions (ExpressionParsing.fs) — RESOLVED

**Symptom:** `(=)` (equality operator as first-class function) failed to parse inside parentheses.
**Root cause:** The expression dispatch for `(` went directly to `pParen` which tries to parse a general expression. `=` (`OpEquality`) isn't a valid standalone expression, so it failed. The `IdentOrOp.ParenOp` path was only reachable from `IdentOrOp.parse`, not from expression parsing.
**Fix:** Added `pParenOpExpr` parser that tries `( OpName )` → `Expr.LongIdentOrOp(LongIdentOrOp.Op(IdentOrOp.ParenOp(...)))` before falling back to `pParen`. Also fixed `walkLongIdentOrOp` and `walkIdentOrOp` in `AstTraversal.fs` to properly visit `Op`, `QualifiedOp`, and `StarOp` cases instead of writing stubs.
**Test:** `data/225_paren_op_equality.fs`
**Corpus impact:** `LanguageFeatures.fs` now clean.

### 12. Parser: `T | null` nullable reference type syntax (TypeParsing.fs) — RESOLVED

**Symptom:** `(string | null)` type annotation failed to parse. `|` was treated as a pattern match bar.
**Root cause:** The type parser had no support for F# 9's nullable reference type syntax `T | null`.
**Fix:** Added `Type.Null` and `Type.UnionType` cases to the AST. Added `null` as a valid atomic type in `parseAtomic`. Added `pUnionType` between `pPostfixType` and `pTupleType` in the type precedence chain (`|` binds tighter than `*` and `->`). Wired `pTupleType` to call `pUnionType` instead of `pPostfixType`.
**Test:** `data/224_nullable_type.fs`
**Corpus impact:** +1 clean file (`LanguageFeatures.fs`). Some files show more diagnostics because parsing now proceeds past the previously-failing `(string | null)` and hits other pre-existing issues (primarily the try/with offside bug).

### 13. Parser: Inline `when` type constraints and `not null` constraint (TypeParsing.fs) — RESOLVED

**Symptom:** `'a when 'a: not null` in type annotations failed to parse. Two issues: (1) the type parser didn't handle `when` after a type, (2) the constraint parser only handled `not struct`, not `not null`.
**Root cause:** `Type.parse` was just `pFunctionType` with no `when` handling. The `not` branch in `Constraint.parse` hardcoded `pStruct` after `not`.
**Fix:** Added `Type.WhenConstrainedType` AST case (uses `TyparConstraints` directly, unlike `ConstrainedType` which uses `TyparDefns` with angle brackets). Added `Constraint.NotNull` case. Added `pWhenConstraints` parser and wired it into `Type.parse` as an optional suffix after `pFunctionType`. Fixed the `not` branch to handle both `struct` and `null`.
**Test:** `data/219_when_type_constraint.fs`
**Corpus impact:** `Graph.fs` and `option.fs` should improve.

## Remaining Known Issues

### Corpus results (as of 2026-04-18, post operator-case-name fix):
- **248 clean** (0 diagnostics)
- **1 with diagnostics** (parsed with recovery — 2 errors)
- **0 lex errors**
- **0 parse errors**
- **0 exceptions**
- **0 timeouts**
- **0 crashed**

File-level counts unchanged from the earlier GADT fix (still 248/1). Inside `prim-types.fs` the seed has moved: Ln 4134 (operator case names) is resolved, but parsing now reaches Ln 5198/5199 and hits a fresh failure — an SRTP static-optimization clause `when 'T struct = match box value with ...`. The earlier error count was 1 at Ln 4134; the new cascade lands on 2 errors at Ln 5198/5199.

Prior snapshot context: **+1 clean**, **−1 CRASH** vs the earlier 247/1/1. The `ilwrite.fs` AST-walker stack overflow is resolved (see below). The `prim-types.fs` DIAG seed has shifted repeatedly as each layer is fixed: old Ln 2601 `match`-arm-trailing-`in` (resolved by `0b5df16`), then Ln 4024 GADT (resolved in this session), then Ln 4134 operator-named DU cases (also resolved in this session), now Ln 5198 SRTP `struct` constraint in static-opt clause.

**Resolved: `ilwrite.fs` AST-walker stack overflow.** `walkExpr` in `src/XParsec.FSharp/AstTraversal.fs` was a ~580-line match with ~50 arms. The F# compiler emitted a single method whose stack frame had to accommodate the union of every arm's locals. Deep right-leaning `Expr` chains (InfixApp / App / DotLookup / EnclosedBlock pipelines — hit in `ilwrite.fs`'s big `origExnClauses |> List.map (fun ... -> (... (match kind with ...)))` tuple-of-match) stacked that wide frame once per level and blew the .NET stack.

Fix: extracted each non-trivial arm of the `walkExpr` match into its own `and walkExpr*` helper taking the deconstructed DU fields. `walkExpr` itself is now a thin dispatcher. Each helper's frame holds only the locals it actually uses, so per-level stack footprint during recursion drops substantially. Same approach benefits any other hot walker, but `walkExpr` was the only one hitting overflow in the corpus.

**DIAG file (2 errors):**
- `prim-types.fs` — `MissingModuleElem` at line 5198 col 14, `UnexpectedTopLevel` at line 5199 col 33. Seed is an inline SRTP static-optimization clause with a `struct` constraint and a multi-line `match` body (inside `let inline anyToString`):
  ```fsharp
  when 'T struct =
     match box value with
     | :? IFormattable as f -> defaultIfNull "" (...)
     | _ -> defaultIfNull "" (value.ToString())
  ```
  The surrounding `when 'T : SomeType = <single-line expr>` clauses parse (Category Q / `243d29f`). This new variant combines a bare `struct` constraint (no `:`/type) with an undented multi-line match on the RHS — one or both aren't yet supported in static-opt-clause position. See "Remaining: SRTP static-optimization clause with `struct` constraint..." for details.

### Resolved since the 244/5 snapshot

Recent commits that eliminated the remaining DIAG categories:
- `22a8fc7` Fix lexing "(*)" — resolved Category S (module-qualified `Module.(*)` vs block comment).
- `243d29f` Parse expressions containing the dot operator in IL intrinsics — eliminated most of the prim-types.fs errors (Category Q inline IL).
- `419ef18` Parse multiple named argument patterns / `e502468` / `7ac1311` Fix nested named fields — resolved Category V (curried/named-field active patterns in CheckComputationExpressions.fs).
- `f4c2d95` Parse SRTP default constraint / `63aaf9c` Print < and > in TyparDefns — resolved Category U (multi-line SRTP generic parameter constraints in Query.fs).
- `930c99c` Fix undentation of match expressions in parens — resolved Category R (multi-line tuple arg with match in TypedTreeOps.fs) and Category T (paren-tuple with trailing match in ilwrite.fs — now parses, though AST is deep).
- `a67b3cf` Allow undentation rule 15.1.10.4 for fun and function keywords / `2adcbb9` Fix anchoring offside column for if-elif-else if chains / `1a19aff` Refactoring isPermittedUndentation rule 15.1.10.4 / `d261f35` / `0ee9fa9` Eliminate dead undentation rules — additional offside cleanup.

### Previous snapshot (2026-04-18, post if-chain offside fix):
- 244 clean / 5 DIAG / 0 CRASH.

### Bug 31: If/else-if/elif chain body offside regression — RESOLVED

**Symptom:** From commit `046d26e` onward, `if ... then ... else if ... then ... body` chains inside `let` bodies failed with `MissingExpression` when arm bodies sat at a column less than the inner `if` keyword's column. Affected `map.fs`, `set.fs`, `array.fs`, `async.fs`, `ilread.fs`, ~28 corpus files in total.

**Bisect:** Located by `git bisect` to `046d26e` ("Fix indentation context of elif branches"):
- Before (`d0af4ca`): 235 clean / 14 DIAG
- After (`046d26e`): 207 clean / 42 DIAG — −28 clean in a single commit

**Root cause (two entangled issues):**
1. `046d26e` rewrote `ElifBranches.pConditionThen` to anchor the arm's `If` and `Then` offside contexts at `ifTok_col + 1` (the inner `if` of an `else if` pair). For collapsed `else if` sugar, the inner `if` can be to the right of the `else` — its column is irrelevant to the chain alignment. Arm bodies aligned with the chain's leftmost keyword then fell below the inner-`if`-anchored offside line.
2. The commit was written as a workaround for symptoms of an unrelated bug: `findLineNumberImpl` in `ParsingTypes.fs` had a broken binary search (returning wrong line numbers), which made the `pElifOrElseIf` same-line-collapse decision misbehave. The real fix came later in `59ca412` ("Fix findLineNumberImpl"), which corrected the binary search but left the incorrect context-anchoring in place as pure regression.

**Fix:** Anchor the arm's `If` and `Then` contexts at `indent + 1`, where `indent` is the chain's leftmost keyword column:
- Plain `elif` → `elif_col`.
- Collapsed `else if` → `min(else_col, if_col)`. Same-line `else if` has `else < if`, so leftmost is `else`; multi-line `else\nif` collapses only when `if_col <= else_col` (per `pElifOrElseIf`), so leftmost is `if`. In both cases this is the chain's actual alignment column.

Using `indent + 1` (instead of peek-based `withContext`, which would anchor at the first condition token's column) is required so multi-line conditions that undent onto the following line remain within the If context.

**Files:** `src/XParsec.FSharp/ExpressionParsing.fs` only.
**Tests added:**
- `data/357_if_else_if_let_body.fs` — `map.fs` pattern (same-line `else if`, bodies at shared indent)
- `data/358_elif_chain.fs` — plain `elif` chain
- `data/359_else_if_multiline.fs` — multi-line `else\nif` collapse (`if_col <= else_col`)
- `data/360_else_nested_if.fs` — `else` with genuinely nested `if` strictly to the right (no collapse)

**Corpus impact:** 212/37 → 244/5, +32 clean files.

### Current DIAG distribution (1 file)

| Count | File |
|-------|------|
| 2     | prim-types.fs |

No CRASH files (the previous `ilwrite.fs` AST-walker overflow is resolved — see the corpus-results section).

### Historical repro categories (most resolved — see "Resolved since the 244/5 snapshot" above)

#### Q: Inline IL expression syntax `(# "instr" args : type #)` (prim-types.fs) — MOSTLY RESOLVED (`243d29f`)
**Syntax:**
```fsharp
let inline F (x: 'T) (y: 'T) =
    GenericIntrinsic x y
    when 'T : int32 = (# "ceq" x y : bool #)
    when 'T : float = if (# "ceq" x y : bool #) then true else false
```
**Root cause:** The F# inline IL emission form `(# instrString args : retType #)` is FSharp.Core-internal surface syntax. Lexer/parser has no dispatch for `(#` / `#)` as an expression form. The inline `when 'T : T = expr` static-optimization clauses then also fail to parse the RHS.
**Affects:** prim-types.fs (13 errors — all are this form or static-opt clauses containing this form).
**Repro:** `data/361_inline_il_expression.fs` (1 error — seed error cascades to the `let inline` body).

#### R: Multi-line tuple argument inside match-arm function application (TypedTreeOps.fs) — RESOLVED (`930c99c`)
**Syntax:**
```fsharp
match expr with
| Expr.Op (TOp.Tuple tupInfo, argTys, args, m) when not (evalTupInfoIsStruct tupInfo) ->
  args |> List.iteri (fun n ->
      F g None rvs
        (mkTupleFieldGet g (tupInfo, access, argTys, n, m),
        (fun e -> errorR (...); e)))
```
**Root cause:** Inside a match arm guarded by `when`, the arm body's offside interacts with the paren-wrapped tuple. The `(mkTupleFieldGet ...)` opens at a column deeper than the outer function call, and its inner tuple contains a multi-line lambda. Something in this stacking causes the paren content to fail parsing (the inner is skipped as tokens).
**Affects:** TypedTreeOps.fs (6 errors, all variants of this — lines 7974, 7989, 8000, 10843).
**Repro:** `data/362_multiline_tuple_arg.fs` (2 errors).

#### S: Module-qualified star operator `Module.(*)` vs comment open `(*` (TypedTreeOps.fs) — RESOLVED (`22a8fc7`)
**Syntax:** `EvalArithBinOp (Checked.(*), Checked.(*), Checked.(*))`
**Root cause:** `(*` is the block-comment open. After the `.` in `Checked.(*)` the lexer/parser must recognize `(*)` as a parenthesized operator (analogous to `(=)` and `Module.(=)` from bugs 11 & 17), but the current path treats `(*` as comment start.
**Affects:** TypedTreeOps.fs (at least line 10843).
**Repro:** `data/363_module_qualified_star_op.fs` (2 errors).

#### T: Paren-tuple with multi-line `match` as trailing tuple element (ilwrite.fs) — RESOLVED (`930c99c`; but ilwrite.fs now CRASH in AST walker)
**Syntax:**
```fsharp
origExnClauses |> List.map (fun (st1, sz1, st2, sz2, kind) ->
    (adjuster st1, (adjuster (st1 + sz1) - adjuster st1),
     adjuster st2, (adjuster (st2 + sz2) - adjuster st2),
     (match kind with
     | FinallyClause | FaultClause | TypeFilterClause _ -> kind
     | FilterClause n -> FilterClause (adjuster n))))
```
**Root cause:** A 5-tuple whose last element is a paren-wrapped `match` expression. The match's `|` bars sit at a column less than the tuple's non-match elements. The surrounding `List.map (fun ... -> (...))` adds another paren/lambda layer.
**Affects:** ilwrite.fs (6 errors, all one site).
**Repro:** `data/364_paren_tuple_with_match.fs` (5 errors).

#### U: SRTP generic parameter with multi-line constraint clauses (Query.fs) — RESOLVED (`f4c2d95`, `63aaf9c`)
**Syntax:**
```fsharp
member inline _.SumByNullable< 'T, 'Q, ^Value
                                   when ^Value :> System.ValueType
                                   and ^Value : struct
                                   and ^Value : (new : unit -> ^Value)
                                   and ^Value : (static member ( + ) : ^Value * ^Value -> ^Value)
                                   and default ^Value : int>
              (source: QuerySource<'T, 'Q>) = ...
```
**Root cause:** Type-parameter list `<...>` that spans multiple lines, mixing regular typars (`'T`, `'Q`), SRTP typars (`^Value`), member-shape constraints `(static member Name : Sig)`, and `default ^Value : T` constraint. The multi-line continuation inside `<...>` isn't accepted.
**Affects:** Query.fs (5 errors — all `member inline _.XxxBy<...>` declarations).
**Repro:** `data/365_srtp_generic_param_constraints.fs` (1 error).

#### V: Curried parameterized active pattern in match-arm pattern (CheckComputationExpressions.fs) — RESOLVED (`419ef18`, `e502468`, `7ac1311`)
**Syntax:**
```fsharp
match synExpr with
| SynExpr.App (_, _, CustomOpId (isCustomOperation ceenv) (customOperationIsLikeZip ceenv) nm,
               ExprAsPat secondSourcePat, _) -> ...
| CustomOpId (isCustomOperation ceenv) (customOperationIsLikeZip ceenv) nm -> ...
```
**Root cause:** F# parameterized active patterns `PatName arg1 arg2 boundVar` use curried application in pattern position, where `argN` are expressions (evaluated at match time) and `boundVar` is the binding. The pattern parser doesn't recognize the intermediate paren-expr arguments as active-pattern arguments — treats them as a value tuple or fails.
**Affects:** CheckComputationExpressions.fs (3 errors).
**Repro:** `data/366_curried_active_pattern_in_pat.fs` (2 errors).

### Resolved: `match` arm terminated by `in` keyword (prim-types.fs)
Resolved by `0b5df16` ("Parse in keywords between declarations at module level"). Line 2601 in `prim-types.fs` now parses. Parsing then reaches Ln 4024, which is the current DIAG seed — see next entry.

### Resolved: GADT-style DU cases with explicit type signatures (prim-types.fs Ln 4024/4027)
**Syntax:**
```fsharp
type Option<'T> =
    | None :       'T option
    | Some : Value:'T -> 'T option
```
**Root cause:** DU-case parser accepted `| Case of <fields>` and `| Case` (nullary) but not the FSharp.Core-internal surface form where each case is given an explicit signature: `| None : 'T option` (nullary with return type) or `| Some : Value:'T -> 'T option` (named argument + return type). The `:` after the case name wasn't dispatched.

**Fix:**
- `src/XParsec.FSharp/Expr.fs` — added `UnionTypeCaseData.GadtNullary(ident, colon, Type)` (for `| Name : retType`) and renamed the unused `NaryUncurried` to `GadtNary(ident, colon, UncurriedSig)` (for `| Name : args -> retType`). Removed the old `of :` path (not real F# grammar).
- `src/XParsec.FSharp/TypeDefnParsing.fs` — split `UnionTypeCaseData.parse` into ordered branches: `parseNaryOf` (with `of`), `parseGadtNary` (with `->`), `parseGadtNullary` (no `->`), `Nullary`. Tried in that order so `| X of Y` isn't misread as GADT and `NullaryTyped` catches the no-arrow case only after `UncurriedSig` fails.
- `src/XParsec.FSharp/TypeParsing.fs` — added `Type.parseNoUnion` (a copy of `parseFunction` that threads `pSubtypeType` through `pTupleType` in place of `pUnionType`). Used for GADT return types so a following `| NextCase` is not swallowed as a `T | T` nullable-ref type union.
- `src/XParsec.FSharp/AstTraversal.fs` — visitor arms for the new variants at both `walkUnionCaseData` and the inline union-case walker.

**Test:** `data/369_gadt_du_case_signature.fs` — parses cleanly with no diagnostics.
**Corpus impact:** `prim-types.fs` dropped from 2 errors to 1 (remaining seed is operator-named GADT cases — see next entry).

### Resolved: GADT-style DU case with parenthesized operator case name (prim-types.fs Ln 4134)
**Syntax:**
```fsharp
type List<'T> =
   | ([])  :                  'T list
   | ( :: )  : Head: 'T * Tail: 'T list -> 'T list
```
**Root cause:** After the GADT fix, DU cases accepted `ident : retType` / `ident : args -> retType`, but case names were parsed via `nextNonTriviaIdentifierL` — so `([])` and `( :: )` (FSharp.Core's internal `op_Nil` / `op_ColonColon` naming) failed on the leading `(`.

**Fix:**
- `src/XParsec.FSharp/Expr.fs` — added `OpName.NilOp(lBracket, rBracket)` for the `[]` operator form (not a symbolic op, so it didn't fit SymbolicOp/RangeOp/ActivePatternOp). Changed `UnionTypeCaseData` name fields from `'T` (raw ident token) to `IdentOrOp<'T>` across all four variants (`Nullary`, `Nary`, `GadtNary`, `GadtNullary`).
- `src/XParsec.FSharp/PrimitivesParsing.fs` — added `pNilOp` (`[ ]`) to `OpName.parse`, tried between ActivePatternOp and SymbolicOp. Parenthesized forms flow through the existing `IdentOrOp.ParenOp` path. `::` is `KWColonColon`/`OpCons` (same numeric value), which is already in the `isOperatorKeyword` whitelist, so `pSymbolicOp` picks it up inside parens without extra work.
- `src/XParsec.FSharp/TypeDefnParsing.fs` — swapped `nextNonTriviaIdentifierL "Union Case Name"` for `IdentOrOp.parse` in all four `UnionTypeCaseData` parsers. `ExceptionDefn` callsite wraps its plain ident in `IdentOrOp.Ident` to match.
- `src/XParsec.FSharp/AstTraversal.fs` — added `OpName.NilOp` walker arm; `walkUnionCaseData` and the inline union-case walker now call `walkIdentOrOp` for the case name.

**Test:** `data/370_gadt_operator_case_name.fs` — `type List<'T>` parses cleanly with `([])` → `ParenOp/NilOp` and `( :: )` → `ParenOp/SymbolicOp`.
**Corpus impact (verified 2026-04-18 rerun):** `prim-types.fs` errors moved from Ln 4134 → Ln 5198/5199 — a fresh seed deeper in the file. Top-level corpus counts unchanged at 248/1, but internal DIAG count on `prim-types.fs` went from 1 → 2 (new seed has a slightly larger two-error cascade). `List<'T>` now parses cleanly. See next entry for the new seed.

### Remaining: SRTP static-optimization clause with `struct` constraint and multi-line `match` body (prim-types.fs, 2 errors at Ln 5198/5199)
**Syntax:**
```fsharp
let inline anyToString nullStr (x: 'T) =
    ...
    when 'T : uint32 = let x = (# "" value : 'T #) in x.ToString()
    ...
    when 'T struct =
       match box value with
       | :? IFormattable as f -> defaultIfNull "" (f.ToString(...))
       | _ -> defaultIfNull "" (value.ToString())
    ...
```
**Root cause (candidate):** Two interacting features not yet supported together. (a) The `when 'T struct = <expr>` form of an inline static-optimization clause — constraint is bare `struct` with no `:` or type, distinct from the `when 'T : SomeType = <expr>` form that surrounds it. (b) The RHS is a multi-line `match` expression whose arms undent relative to the clause's `when` column. Previous work (`243d29f` — Category Q) made the `when 'T : T = (# "..." ... #)` inline-IL clauses parse, but that path assumes `: Type = <single-line>`.
**Affects:** prim-types.fs (2 errors — `MissingModuleElem` at 5198:14, `UnexpectedTopLevel` at 5199:33).
**Repro:** `data/371_srtp_struct_static_opt.fs` (1 error). Confirmed the first `when 'T : int = "int"` clause parses as `LibraryOnlyStaticOptimization`, then the parser stops at the `struct` token in `when 'T struct` — it never reaches the `=` or the `match` body. So the seed is purely the bare `struct` constraint in static-opt position, not the multi-line `match` body.
**Fix direction:** the static-opt-clause constraint parser currently expects `'T : SomeType` (via a WhenTyparTyconEqualsTycon AST node). Needs a parallel `'T struct` form (and likely `'T : not struct`, `'T : null`, etc. for completeness). These already exist as `Constraint` productions used by `when`-constraints on type definitions — candidate is to reuse `Constraint.parse` or add a dedicated static-opt variant.

### Resolved: AST walker stack overflow on ilwrite.fs
`AstTraversal.walkExpr` was a single ~580-line pattern match. F# compiled this to one method whose stack frame had to fit the union of every arm's locals, so every recursive call through a deep `Expr` chain (InfixApp/App/DotLookup pipelines) stacked that wide frame — eventually overflowing on `ilwrite.fs`'s `origExnClauses |> List.map (fun ... -> (... (match kind with ...)))`.
**Fix:** split every non-trivial arm of `walkExpr` into its own `and walkExpr*` helper taking the deconstructed DU fields. Dispatcher `walkExpr` now only does the match and delegates. Each helper's frame contains only that arm's locals, shrinking per-level stack cost. `walkElifBranch` and `walkExprObjectInterface` were split out from their parent arms for the same reason.
**File:** `src/XParsec.FSharp/AstTraversal.fs`.

### Resolved: `else if` chain regression (commit `812fdad` → fixed by `5e6898c`)
The `else if` flattening regression described in prior progress notes is resolved. The post-processing flattening approach was refined so the inner `if` is parsed in the outer if's offside context, restoring previously-clean files (`TypedTreePickle.fs`, `array.fs`, `il.fs`, `ilreflect.fs`, `infos.fs`, `illib.fs`, `console.fs`, `async.fs`, `quotations.fs`, and more).

### Bug 30: Function-arm `->` undentation after multi-line `when` guard — RESOLVED
**Commit:** `6ee954a`
**Symptom:** `function pat when cond \n -> body` inside a piped paren-expression failed to parse. The `->` on the next line was at a column less than the pattern, triggering `Offside` → `MissingRule` → `UnclosedDelimiter` cascade. Common idiom in `TypedTreeOps.fs` piped-function applications.
**Root cause:** `isPermittedUndentation` had no case for `OffsideContext.MatchClauses`. The `MatchClauses` context exists to enforce `|` bar alignment, but the default offside check also rejected any non-bar token (including `->`) at columns less than the pattern.
**Fix:** Added a new branch to `isPermittedUndentation` in `src/XParsec.FSharp/ParsingHelpers.fs` that handles `OffsideContext.MatchClauses` for non-`|` tokens, reusing the 15.1.10.1 FunBody skip-past-containers logic. `|` bar alignment remains strict.
**Test:** `data/336_function_when_multiline_body.fs`
**Corpus impact:** TypedTreeOps.fs 10→6 relative to the stale `218/31` report. No file flipped from DIAG to clean in the current corpus state; the residual TypedTreeOps errors are from other categories.

### Bug 29: Uninitialized `refMeasure` in type abbreviation — RESOLVED
**Symptom:** SI.fs threw `InvalidOperationException: RefParser was not initialized` from `TypeDefnParsing.parseAbbrevOrImplicitClass` line 1254. Surfaced after the type-annotation parsing commits reached deeper into SI.fs's `[<Measure>] type hertz = / second` abbreviations.
**Root cause:** `parseAbbrevOrImplicitClass` referenced `refMeasure.Parser` (from `ParserRefs.fs`) directly. `Measure`'s `do refMeasure.Set parse` initializer only runs when something touches the `Measure` module. SI.fs hit type-abbreviation measure parsing before any constant-suffix path through `ConstantParsing.pMeasure`, so `Measure` module static init never ran.
**Fix:** Replaced `refMeasure.Parser` with direct `Measure.parse` reference. MeasureParsing.fs is compiled before TypeDefnParsing.fs, so this is valid and avoids the RefParser indirection entirely (and forces module init on use).
**File:** `src/XParsec.FSharp/TypeDefnParsing.fs:1254`

**Newly clean since 2026-04-05:** FSharpRequestContext.fs, SemanticClassificationKey.fs, fsimain.fs, local.fs, tasks.fs, z.fs (+6).

**Significantly improved:** CheckExpressions 30→14, ilread 16→1, TypedTreeOps 14→10, quotations 9→2, Query 7→5, IlxGen 6→2, Optimizer 5→2.

**Regression resolved:** The `InvalidOperationException` from SI.fs was fixed by replacing `refMeasure.Parser` with direct `Measure.parse` in `TypeDefnParsing.fs:1254`. The remaining DIAG(1) from `type becquerel = second^-1` was then fixed via the `^-` split pattern (see Category F below).

### Previous results (2026-04-05, post-session 5):
- 211 clean, 38 with diagnostics, 0 exceptions.

### Remaining root cause categories (ordered by impact):

#### Category F: Shorthand lambda `_.Property` (6+ errors in 6 files)
**Syntax:** `xs |> List.partition _.IsCoercesTo`
**Root cause:** F# 9 shorthand member access lambda not supported. `_` is consumed as wildcard, `.Property` breaks the surrounding expression.

#### Category: `static member Name\n  with get/set` property syntax
**Syntax:** `static member BuildPhase\n    with get () = ...` — multi-line property with getter/setter where name is on one line and `with get/set` is on the next.

#### Category I: Measure abbreviation with leading `/` (1 error in 1 file)
**Syntax:** `type hertz = / second`
**Root cause:** Reciprocal measure abbreviation not recognized.

#### Other remaining issues
Many remaining DIAG files have miscellaneous issues including: hat-type parameters (`^T`), inline type constraints in parameters, `DebuggerDisplay` attribute parsing quirks, and various cascading errors from the above categories.

### Stack overflow issues — RESOLVED
All 32 previously-crashing files now parse successfully. The root cause was `pSepVirt` emitting zero-width `VirtualSep` tokens for non-expression-starting keywords (like `with`, `finally`, `module`). The Pratt parser's `InfixNary` handler with `allowTrailingOp = true` accepted the trailing separator, then the RHS loop picked up another VirtualSep — infinite loop leading to stack overflow.

**Fix (bug #14):** Replaced the ad-hoc blacklist in `pSepVirt` with a `TokenInfo.canStartExpression` whitelist. Only tokens that can actually start an expression now trigger virtual separator emission.

### try/with offside issue — RESOLVED
**Bug #15:** `try...with _ ->` without leading `|` failed offside check when the clause body was on the next line at `try_col`. This was because the Try context was popped before match rules parsing, and the MatchClauses context inherited its indent from the pattern position (e.g., `_` at col=9) instead of `try_col`.

**Fix:** Restructured `pTryExpr` to manually manage the Try context (like `parseMatchBody` manages Match context), keeping it active through with/finally parsing. MatchClauses now use `withContextAt` at `try_col` so clause bodies can be at `try_col` per F# spec. Added `OpBar` as permitted undentation for Try context.

**Corpus impact:** Files previously affected (`FSharpInteractiveServer.fs`, `SubstituteText.fs`, `FSharp.DependencyManager.Utilities.fs`, `ilmorph`, `OptimizeInputs`, `ilwritepdb`) are now clean.

## Files Changed

### Source changes:
- `src/XParsec.FSharp/Lexing.fs` — `%%` fix, `$"""` quote fix, `KWHash` for standalone `#`, updated TODO comment for spec 3.8.1
- `src/XParsec.FSharp/Token.fs` — `TokenInfo.canStartExpression` whitelist for virtual separator gating
- `src/XParsec.FSharp/ExpressionParsing.fs` — RHS handler guard, `pApplication` prefix fix, `isAdjacentPrefixOp`, point-free active pattern in `Binding.parseFunction`, `pSepVirt` whitelist, `pTryExpr` restructured with manual Try context management
- `src/XParsec.FSharp/ParsingHelpers.fs` — `trace` helper, `consumePeeked` tracing, `processWarnDirective`, `#nowarn`/`#warnon` dispatch in `nextNonTriviaTokenImpl`, `OpBar` as permitted undentation for Try context
- `src/XParsec.FSharp/ParsingTypes.fs` — `TraceEvent.Message` case, `WarnDirective` type, `ParseState.WarnDirectives` field, `isWarningSuppressed` helper
- `src/XParsec.FSharp/Debug.fs` — `printWarnDirectives` for golden file output
- `src/XParsec.FSharp/KeywordParsing.fs` — `pHash` uses `KWHash` (was already correct, lexer was the issue)
- `src/XParsec.FSharp/TypeDefnParsing.fs` — Single-case DU without `|` in `parseBody`
- `src/XParsec.FSharp/ProgramStructureParsing.fs` — `ObjectConstruction.init()` for RefParser initialization

### Test infrastructure:
- `test/XParsec.FSharp.Tests/TestHelpers.fs` — `corpusTestData`, `tryParseCorpusFile`, `printWarnDirectives` in golden output
- `test/XParsec.FSharp.Tests/CorpusReport.fs` — `CorpusResult`, `printReport`
- `test/XParsec.FSharp.Tests/Program.fs` — `--parse-file`, `--corpus <subdir>` CLI modes
- `test/XParsec.FSharp.Tests/XParsec.FSharp.Tests.fsproj` — Added `CorpusReport.fs`
- `test/XParsec.FSharp.Tests/KeywordTests.fs` — Updated `#` token expectation to `KWHash`

### Test data added:
- `data/212_prefix_op_in_application.fs` + `.parsed` — prefix-only ops in application
- `data/213_adjacent_prefix_op.fs` + `.parsed` — adjacent prefix ops (spec 3.8.1)
- `data/214_module_internal.fs` + `.parsed` — attributed module with access modifier
- `data/215_single_case_du.fs` + `.parsed` — single-case DU without leading `|`
- `data/216_flexible_type.fs` + `.parsed` — `#type` flexible type annotation
- `data/217_try_with_in_match.fs` + `.parsed` — try/with offside variants (repro)
- `data/218_nowarn_directive.fs` + `.parsed` — `#nowarn`/`#warnon` directive handling
- `data/219_when_type_constraint.fs` + `.parsed` — when type constraint (repro)
- `data/220_active_pattern_defn.fs` + `.parsed` — point-free active pattern definition
- `data/lex-only/lo_210_interp_escape_percent.fs` + `.lexed` — `%%` escape
- `data/lex-only/lo_211_interp_triple_quote_embed.fs` + `.lexed` — `"` in `$"""`
- `data/lex-only/AsyncMemoize.fs` + `.lexed` — corpus file
- `data/lex-only/FSharp.DependencyManager.ProjectFile.fs` + `.lexed`
- `data/lex-only/FSharp.DependencyManager.Utilities.fs` + `.lexed`
- `data/lex-only/fsihelp.fs` + `.lexed`

- `src/XParsec.FSharp/Expr.fs` — Added `Pat.Optional` case
- `src/XParsec.FSharp/PatternParsing.fs` — `pOptionalPat` parser, `Token.OpDynamic` dispatch
- `src/XParsec.FSharp/AstTraversal.fs` — `Pat.Optional` visitor
- `data/222_primary_constructor.fs` + `.parsed` — optional parameter in primary constructor
- `data/224_nullable_type.fs` + `.parsed` — nullable type annotation (repro)
- `data/225_paren_op_equality.fs` + `.parsed` — parenthesized equality operator (repro)
- `data/232_do_min_indent.fs` + `.parsed` + `.lexed` — do block with min indentation
- `data/236_try_with_min_indent.fs` + `.parsed` + `.lexed` — try/with clause body at try_col
- `data/237_try_finally_min_indent.fs` + `.parsed` + `.lexed` — try/finally body at try_col

## Bugs Fixed — Session 2 (2026-03-23)

### 16. Parser: Named field patterns with semicolons (PatternParsing.fs) — RESOLVED
**Symptom:** `| Foo(isStruct = false; elementPats = pats) ->` failed to parse. The semicolons between named fields were not accepted.
**Root cause:** `pNamedFieldPats` used `sepBy1 pUnionFieldPat pComma` which only accepted commas. F# allows semicolons as separators in named field patterns.
**Fix:** Changed separator to `pComma <|> pSemi`.
**Test:** `data/270_named_field_pattern.fs`
**Corpus impact:** SynPat.fs 28→10, CheckExpressions.fs 18→10, TypedTreeOps.fs 29→19, many files improved. +18 clean files.

### 17. Parser: `Module.(operator)` qualified operator syntax (ExpressionParsing.fs) — RESOLVED
**Symptom:** `NonStructuralComparison.(=) x y` failed. The dot-access parser didn't recognize `(op)` after a dot.
**Root cause:** `parseDotRhs` only handled `.ident` and `.[expr]`, not `.(op)`.
**Fix:** Added `DotParenOp` case to `ExprAux`. Added `.(op)` parser in `parseDotRhs` using `IdentOrOp.ParenOp`. Handled in `completeDot` to build `LongIdentOrOp.QualifiedOp`.
**Test:** `data/268_module_paren_op.fs`
**Corpus impact:** Linq.fs 86→2 (eliminated 84 diagnostics). list.fs, array.fs, local.fs became clean.

### 18. Parser: Optional `?param` in function calls (ExpressionParsing.fs) — RESOLVED
**Symptom:** `f(?x=value)` failed. `?` was not recognized in expression context.
**Root cause:** No dispatch entry for `Token.OpDynamic` in the expression atomic parser. `?ident` needs to produce an optional argument expression.
**Fix:** Added `Expr.OptionalArgExpr` to AST. Added `pOptionalArgExpr` parser dispatched on `Token.OpDynamic`. Added `OpDynamic` to `canStartExpression`.
**Test:** `data/269_optional_arg.fs`
**Corpus impact:** ServiceDeclarationLists.fs 49→3. FSharpCheckerResults.fs 17→17 (other issues). mailbox.fs became clean.

### 19. Parser: Static-only type bodies (TypeDefnParsing.fs) — RESOLVED
**Symptom:** `type Cancellable = static let tokenHolder = ...` parsed as abbreviation, failing at `static`.
**Root cause:** `parseAbbrevOrImplicitClass` lookahead didn't include `pStatic` in the top-level check list (only in the attributed sub-check).
**Fix:** Added `pStatic` to the implicit class detection lookahead.
**Test:** `data/264_static_type_body.fs`
**Corpus impact:** Cancellable.fs became clean.

### 20. Parser: Negative literal patterns (PatternParsing.fs) — RESOLVED
**Symptom:** `| -1 -> ...` failed in match expressions. `-` was parsed as an operator, not as part of a negative constant.
**Root cause:** Pattern parser had no support for `-<numeric>` as a single constant pattern.
**Fix:** Added `Constant.NegativeLiteral` to AST. Added `pNegativeConstPat` parser that parses `-` followed by numeric literal. Used as fallback before `pConstPat`.
**Test:** `data/265_negative_literal_pattern.fs`
**Corpus impact:** il.fs, console.fs improved.

### 21. Parser: `for _ = 0 to N do` wildcard loop variable (ExpressionParsing.fs) — RESOLVED
**Symptom:** `for _ = 0 to 10 do` failed. For-to loop only accepted identifiers.
**Root cause:** `pForExpr` used `pIdentTok` which doesn't accept wildcard `_`.
**Fix:** Changed to `pIdentTok <|> pWildcard`.
**Test:** `data/266_for_wildcard.fs`

### 22. Parser: `open type` declarations (DeclarationParsing.fs) — RESOLVED
**Symptom:** `open type System.Math` failed. `type` keyword was not expected after `open`.
**Root cause:** `ImportDecl.parse` only handled `open long-ident`, not the F# 5 `open type` feature.
**Fix:** Added `ImportDeclType` case to `ImportDecl` AST. Added `opt pType` after `open` in parser.
**Test:** `data/267_open_type.fs`
**Corpus impact:** PatternMatchCompilation.fs 8→7.

### Test status: 1114 passed, 0 failed, 3 skipped

## Bugs Fixed — Session 3 (2026-03-25)

These fixes are from a prior session that was not documented here.

### 23. Parser: Wildcard `_` as function argument (ExpressionParsing.fs) — RESOLVED
**Symptom:** `f _.Name` and `g _ x` failed to parse. `_` was not recognized as a valid function argument start.
**Root cause:** `isAtomicExprToken` did not include `Token.Wildcard`.
**Fix:** Added `Token.Wildcard` to `isAtomicExprToken`.
**Test:** Part of various corpus improvements.
**Corpus impact:** +11 clean files.

### 24. Parser: Prefix operator ambiguity `-x - 1` (ExpressionParsing.fs) — RESOLVED
**Symptom:** `-x - 1` caused "Ambiguous operator associativity" error. Prefix and infix `-` had the same binding power.
**Root cause:** Dual-use operators (e.g., `-`, `+`) used their infix precedence for both prefix and infix, causing ambiguity when the same operator appeared in both roles.
**Fix:** For dual-use operators, prefix binding power is `infixPower + 1`. This ensures `-x - 1` parses as `(-x) - 1` while staying below Application so `-f x` still parses as `-(f x)`. Special-cased `&` and `&&` (address-of operators) to use `PrecedenceLevel.Prefix` since they are unrelated to their infix precedence.
**Test:** `data/288_prefix_infix_ambiguity.fs`
**Corpus impact:** +2 clean files.

### 25. Parser: Member property with return type annotation (TypeDefnParsing.fs) — RESOLVED
**Symptom:** `static member Empty : Foo = { Root = 0 }` failed to parse. The `:` after the member name was not recognized.
**Root cause:** `MethodOrPropDefn.parse` had no branch for `OpColon` after a simple identifier — only handled patterns, equals, and type parameters.
**Fix:** Added `OpColon` branch that parses `ReturnType`, then `=`, then expression body, producing a `Property` binding with return type.
**Test:** `data/289_record_with_members.fs`
**Corpus impact:** +5 clean files.

## Bugs Fixed — Session 4 (2026-03-29)

### 26. Parser: `pLetOrUseIn` indent check inside parenthesized expressions (ExpressionParsing.fs) — RESOLVED
**Symptom:** "Expected 'in' at the same indent as 'let'" when `let` bindings appear inside parenthesized expressions at non-standard indents (e.g., `f(let x = 1 in x)`). 16 occurrences across 15 corpus files.
**Root cause:** `pLetOrUseIn` checked `indent = ctxIndent` against the top of the context stack. Inside `parseHighPrecApp` (which doesn't push a Paren context), the top context was the enclosing SeqBlock at a different indent, causing the check to fail.
**Fix:** Added `letIndent` parameter to `pLetOrUseIn`. The function now also checks `indent = letIndent` alongside the existing context indent check. The call site in `pLetOrUseBody` passes the `let` keyword's indent.
**Corpus impact:** +3 clean files (180→183).

## Changes — Intermediate Commits (2026-03-29 to 2026-04-05)

These commits were made between sessions 4 and 5 and contributed to the corpus improvement:

- `17172d4` **Parse all string literals as fragments** — Lexer/parser refactor for string handling, pattern parsing additions
- `8361557` **Switch to immutable array in AST nodes** — AST structural refactor
- `d3af88f` **Include parsed separators in AST** — Record separators now tracked in AST
- `d9a06a4` **Parse StaticTypars, .N positional DU field access, f[]** — Static type parameters (`^T`), positional DU field access (`.0`, `.1`), empty bracket indexing (`f[]`)
- `577cd6c` **Fix parsing expressions in when guards** — `when` guard expressions now parse correctly; test `data/299_when_guard_let.fs`
- `616b892` **Fix undentation interaction between parens and SeqBlocks** — Offside fix for multiline boolean expressions at SeqBlock column; test `data/298_multiline_boolean_at_seqblock_col.fs`
- `e4e11fd` **Allow trailing semicolons in record expressions** — test `data/300_record_trailing_semicolon.fs`

## Bugs Fixed — Session 5 (2026-04-05)

### 27. Parser: High-precedence adjacency gating with `isPrevTokenNonTrivia` (ParsingHelpers.fs, ExpressionParsing.fs, ConstantParsing.fs) — RESOLVED
**Symptom:** After `for...do...done` or `while...do...done`, the Pratt parser treated the next `(expr)` at the same indent as a high-precedence application argument, rather than a sequential expression. Also affected `<` for type application and measures.
**Root cause:** `pHighPrecLParen`, `pHighPrecLBracket`, `pTypeApplication`, and `pMeasure` used `satisfy`/`satisfyL` to check the raw token, but by that point `peekNextNonTriviaToken` had already skipped intervening trivia. The raw token adjacency was an illusion — the previous consumed token could be trivia (whitespace/newline).
**Fix:** Added `isPrevTokenNonTrivia` helper in `ParsingHelpers.fs` that checks `reader.Input[idx - 1]` against `isTriviaToken`. All four adjacency-sensitive parsers now gate on this check before attempting to match. Generalized `pHighPrecLParen`/`pHighPrecLBracket` into a single `pHighPrec` parameterized function.
**Test:** `data/304_for_loop_then_paren_expr.fs`

### 28. Parser: Nested fun body undentation past outer `Fun` contexts (ParsingHelpers.fs) — RESOLVED
**Symptom:** Inner `fun` body undentation was blocked by an outer `Fun` context in the offside stack.
**Root cause:** `findEnclosingIndent` in `isPermittedUndentation` (rule 15.1.10.1) skipped `SeqBlock`/`Paren`/`Bracket`/etc. but stopped at `Fun`/`Function` contexts, rejecting the inner fun body's indent against the outer fun's indent.
**Fix:** Added `OffsideContext.Fun` and `OffsideContext.Function` to the skip list in `findEnclosingIndent`.
**Test:** `data/301_nested_pipe_fun_multiline.fs`

**Combined corpus impact (intermediate commits + session 5):** +28 clean files (183→211).

## Remaining Root Cause Categories (updated 2026-04-05)

### Category A: Anonymous records `{| |}` — Not implemented
**Syntax:** `{| startLine = r.StartLine - 1; startCol = r.StartColumn |}` and `{| field: int; ... |}` types.
**Root cause:** Lexer produces `{|` and `|}` tokens but the parser has no support for anonymous record expressions or types.
**Affects:** FSharpRequestContext.fs (12 errors), and likely others with anonymous record usage.
**Repro:** `data/310_anonymous_record.fs`

### Category B: SRTP member constraints `(^T: (member ...) ...)` — Not implemented
**Syntax:** `(^T: (member GetAwaiter: unit -> ^Awaiter) task)` — statically resolved member invocation.
**Root cause:** The `^T:` member constraint call syntax is not implemented in the expression parser.
**Affects:** tasks.fs (5 errors), Query.fs (7 errors — also has `new` constraint), ConstraintSolver.fs (partially).
**Repro:** `data/312_srtp_member_constraint.fs`

### Category C: `fixed` expression — Not implemented
**Syntax:** `use ptr = fixed semanticClassification`
**Root cause:** `fixed` is not recognized as an expression keyword. Parsed as identifier, then fails.
**Affects:** SemanticClassificationKey.fs (3 errors).
**Repro:** `data/311_fixed_expression.fs`

### Category D: Tuple pattern with `as` alias
**Syntax:** `let a, _, _, b as res = (1, 2, 3, 4)`
**Root cause:** The `as` keyword after a tuple destructuring pattern is not handled — the parser doesn't expect `as` after the tuple elements.
**Affects:** CheckExpressions.fs (partially — first error at line 2814).
**Repro:** `data/313_tuple_pattern_alias.fs`

### Category E: Type annotation suffix on let-bound expression
**Syntax:** `let res = zeroCreateUnchecked count : 'T array`
**Root cause:** The `: type` annotation after the RHS expression of a `let` binding is not parsed. The parser finishes the expression at `count` and then sees `: 'T array` as unexpected.
**Affects:** local.fs (4 errors — first error at line 1155).
**Repro:** `data/314_let_type_annotation_suffix.fs`

### Category F (resolved): Measure power with negative integer exponent
**Syntax:** `type becquerel = second^-1`
**Root cause:** `^-` is lexed as a fused custom operator at Append precedence (same as `**`), not as `OpConcatenate` + `OpSubtraction`. `pPowerRhs` never saw a power operator at all, and the retry-as-measure logic in `parseAbbrevOrImplicitClass` only checked for `OpConcatenate`.
**Fix:** Mirrored the `>]` split pattern. Added `SplitPowerMinus: bool` flag to `ParseState`. `pSplitPowerOp` in `MeasureParsing.fs` peeks for a `^-` literal, sets the flag, and returns a virtual `^` so the Pratt parser dispatches to the power path. `nextNonTriviaTokenImpl` sees the flag on the next read and rewrites the `^-` token to `OpSubtraction` at `StartIndex + 1`, clearing the flag. `pPowerRhs` now accepts an optional leading `-` followed by the numeric exponent. `Measure.Power` gained an `neg: 'T voption` field. `parseAbbrevOrImplicitClass` retry-as-measure also checks for fused `^` via `tokenStringStartsWith "^"`.
**Files:** `src/XParsec.FSharp/MeasureParsing.fs`, `ParsingTypes.fs`, `ParsingHelpers.fs`, `Expr.fs`, `AstTraversal.fs`, `ConstantParsing.fs`, `TypeDefnParsing.fs`, `Debug.fs`.
**Repro:** `data/330_measure_negative_exponent.fs` — parses cleanly.
**Corpus impact:** SI.fs dropped from 1 diagnostic to 0 (clean).

### Category F (resolved): Reciprocal measure abbreviation `type hertz = / second`
Fixed in commit `7eeb559` ("Parse measure type aliases"). Repro `data/318_measure_reciprocal_abbrev.fs` parses cleanly.

### Category G: SRTP `when` type dispatch (FSharp.Core internal)
**Syntax:** `(get32 0 :?> 'T) when 'T: BigInteger = BigInteger.Zero`
**Root cause:** This is internal FSharp.Core syntax for type-specialized dispatch — a conditional retype with a default expression. Not standard F# surface syntax.
**Affects:** z.fs (5 errors).
**Repro:** `data/320_srtp_when_dispatch.fs`

### Category H: Shorthand lambda `_.Property` (F# 9)
**Syntax:** `xs |> List.partition _.IsCoercesTo`
**Root cause:** F# 9 shorthand member access lambda not supported. `_` is consumed as wildcard, `.Property` breaks the surrounding expression.

### Category I: `static member Name\n  with get/set` property syntax
**Syntax:** `static member BuildPhase\n    with get () = ...` — multi-line property where `with get/set` is on the next line.

### Category J: Complex offside cascading in deep nesting
**Symptom:** Deep match/if/for nesting causes premature Type/SeqBlock context pops when control flow returns to an outer indent level. Many corpus files with high diagnostic counts (ConstraintSolver 29, CheckExpressions 30, CompilerDiagnostics 17) have errors that cascade from an earlier parse failure in deeply nested match/CE contexts. The initial failure is often an unrelated bug (e.g., SRTP syntax), but recovery can't re-sync cleanly inside deep nesting.
**Affects:** ConstraintSolver.fs, CheckExpressions.fs, CompilerDiagnostics.fs, CheckComputationExpressions.fs, ilread.fs, and most files with DIAG > 5.

### Category K: Mixed named-field + positional/wildcard patterns in DU/ctor patterns
**Syntax:** `SynExprRecordField(fieldName = lid, _) :: _ ->` — named field followed by `_` wildcard positional.
**Root cause:** `pNamedFieldPats` in `PatternParsing.fs:336` requires every field to be `name = pat` via `sepBy1 pUnionFieldPat ...`. When it hits a bare `_`, the parse fails entirely; the fallback `pNamed` path treats the inside as a regular paren-tuple and can't interpret the named fields there either.
**Affects:** SynPat.fs (8), FileContentMapping.fs (3), ServiceStructure.fs (4), SignatureHash.fs (5), ConstraintSolver.fs (partial), CheckExpressions.fs (partial), CheckComputationExpressions.fs (partial), quotations.fs (2), ServiceParsedInputOps.fs (1), SynExpr.fs (1), CheckDeclarations.fs (1), and possibly more.
**Repro:** `data/331_named_field_pattern_with_wildcard.fs` (2 errors — match arm + destructuring let)

### Category L: List pattern with newline-separated record patterns
**Syntax:** `[ { Kind = "X" } as prop\n  { Kind = "Y" }\n  { Kind = "Z" } ]` — list pattern whose elements are record patterns on consecutive lines with no `;` separator.
**Root cause:** The list-pattern parser requires explicit `;` between elements, but F# permits newline-as-separator inside `[ ... ]` at the opening column's offside line (same rule that allows `[ 1\n 2\n 3 ]` as a list literal).
**Affects:** FSharpCheckerResults.fs (1).
**Repro:** `data/332_list_pattern_newline_records.fs` (1 error)

### Category M: Object expression with multiple `interface X with` and deeply-nested member body
**Syntax:**
```
{ new I1 with
    member _.M1 = ...
  interface I2 with
    member _.M2 () =
        if ... then ...
        elif ...
        else
            if ... then ...
            if ... then false else true
  interface I3 with
    member _.Dispose() = ()
}
```
**Root cause:** After a deeply-nested if/elif/else member body, the offside/context stack fails to pop back to the object-expression-interfaces level. The second `interface ... with` clause is either eaten by the previous member body or triggers an `UnclosedDelimiter`.
**Affects:** seq.fs (3). May also affect other object-expression-heavy files not yet identified.
**Repro:** `data/333_obj_expr_multiple_interfaces.fs` (2 errors)

### Category N: Active pattern with `[<return: Struct>]` attribute on `and`-bound form
**Syntax:** `and [<return: Struct>] (|BoolExpr|_|) = function ...`
**Root cause:** The attribute on an `and`-bound let-binding of an active pattern with a `return:` target is not parsed. Standalone `[<return: Struct>] let (|Foo|_|) = ...` may work but the `and` variant does not.
**Affects:** IlxGen.fs (2).
**Repro:** `data/334_active_pattern_return_attribute.fs` (1 error)

### Category O: Method call argument with inline type annotation `meth(name: type)`
**Syntax:** `if not (p.WaitForExit(timeout: int)) then ...` — the argument `timeout: int` is a type ascription on a local value, not a named-argument.
**Root cause:** Inside a method-call argument list, `:` is not being parsed as a type-annotation operator on the expression. The `:` breaks the expression and the `if` parser can't find its `then`.
**Affects:** FxResolver.fs (8 — this is the seed error that cascades to 8).
**Repro:** `data/335_method_call_typed_arg.fs` (4 errors from the single construct)

### Category P (resolved): `function` arm with `when`-guard body continuation on a less-indented next line
See Bug 30 above. Fixed in commit `6ee954a` by adding a `MatchClauses` branch to `isPermittedUndentation` that applies the FunBody skip-past-containers rule to non-`|` tokens.

### Patterns verified as already supported
The following patterns were tested with minimal repros and parse correctly — they do NOT cause corpus errors on their own:
- OR patterns over string constants in tuples: `| ("add" | "sub"), [a; b] ->`
- Secondary constructors: `new(x: int) = MyClass(x, 0)`
- Match expression as operand in `|||` chain
- Multiline `if` with `&&` and nested `match` in condition
- `||` operator continuation across lines with `if`/`elif`

## Test Data Added — Sessions 3 & 4

- `data/288_prefix_infix_ambiguity.fs` + `.parsed` — prefix/infix operator interaction
- `data/289_record_with_members.fs` + `.parsed` — record type with member return types
- `data/290_multiple_interface_impl.fs` + `.parsed` — multiple interface implementations (repro for Pratt application issue)
- `data/291_member_val.fs` + `.parsed` — `member val` auto-properties
- `data/292_fun_undentation.fs` + `.parsed` — lambda undentation inside piped parens
- `data/293_multiline_infix_condition.fs` + `.parsed` — multi-line infix conditions
- `data/294_record_with_semicolons.fs` + `.parsed` — record expressions with semicolons and `with` copy

## Test Data Added — Session 5

- `data/301_nested_pipe_fun_multiline.fs` + `.parsed` + `.lexed` — nested fun body undentation
- `data/304_for_loop_then_paren_expr.fs` + `.parsed` + `.lexed` — for loop followed by paren expr (adjacency gating)
- `data/310_anonymous_record.fs` + `.parsed` + `.lexed` — anonymous record expression and type (repro, has errors)
- `data/311_fixed_expression.fs` + `.parsed` + `.lexed` — `use ptr = fixed arr` (repro, has errors)
- `data/312_srtp_member_constraint.fs` + `.parsed` + `.lexed` — SRTP member constraint invocation (repro, has errors)
- `data/313_tuple_pattern_alias.fs` + `.parsed` + `.lexed` — tuple pattern with `as` alias (repro, has errors)
- `data/314_let_type_annotation_suffix.fs` + `.parsed` + `.lexed` — type annotation on let RHS (repro, has errors)
- `data/318_measure_reciprocal_abbrev.fs` + `.parsed` + `.lexed` — reciprocal measure abbreviation (repro, has errors)
- `data/320_srtp_when_dispatch.fs` + `.parsed` + `.lexed` — SRTP when-dispatch syntax (repro, has errors)

## Test Data Added — Session 6 (corpus-diagnostics reproductions)

- `data/330_measure_negative_exponent.fs` + `.parsed` + `.lexed` — `type becquerel = second^-1` (repro + fix verified clean)
- `data/331_named_field_pattern_with_wildcard.fs` + `.parsed` + `.lexed` — `Ctor(name = pat, _)` mixed named-field + wildcard (repro, has errors)
- `data/332_list_pattern_newline_records.fs` + `.parsed` + `.lexed` — `[ { ... } as a\n  { ... }\n  { ... } ]` (repro, has errors)
- `data/333_obj_expr_multiple_interfaces.fs` + `.parsed` + `.lexed` — object expression with multiple `interface X with` clauses and nested if/elif member body (repro, has errors)
- `data/334_active_pattern_return_attribute.fs` + `.parsed` + `.lexed` — `and [<return: Struct>] (|Ap|_|)` (repro, has errors)
- `data/335_method_call_typed_arg.fs` + `.parsed` + `.lexed` — `if not (p.WaitForExit(timeout: int)) then ...` (repro, has errors)
- `data/336_function_when_multiline_body.fs` + `.parsed` + `.lexed` — `function` clause with multi-line `when`-guard and undented body (repro, has errors)
