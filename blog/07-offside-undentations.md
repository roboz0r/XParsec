# 7. The offside rule, part 2 — permitted undentations

If F# strictly enforced "no token below the offside line," a lot of idiomatic F# wouldn't parse. `elif` after a deeply indented `then` branch. A multi-line `fun x -> body` whose body sits under the `fun`. A trailing `)` on its own line. The spec acknowledges this: §15.1.9 enumerates *exceptions to the offside rules*, and §15.1.10 lists *permitted undentations* — four sub-sections, each lifting the rule for a specific construct.

Post 6 ended on the stack discipline — what gets pushed, what gets popped, and how paren-like contexts sit on the stack as `Indent = 0` markers. That's the data structure permitted-undentations need to inspect. This post is what does the inspecting: the function `isPermittedUndentation`, the rules that survived contact with real F#, and the two rules that turned out to be artifacts of someone else's architecture.

## What the spec asks for, plain English

§15.1.9 covers small exceptions. Most of them say the same thing in different shapes: a token at a particular alignment with the head context's offside line is acceptable, even though strict offside would reject it. Examples:

- In a `_Match_` context, `with` and `|` may align *precisely* with the `match` keyword. Without this, every match clause would have to indent past `match`, which would push every `match` deeper than the surrounding code.
- In a `_For_` context, `done` may align with `for`. Same idea: closing tokens want to share a column with the keyword that opened them.
- In a `_SeqBlock_` context, an infix operator may be offside by the size of the operator plus one. The intent is precise and a little surprising — it keeps the *operands* column-aligned while the operators hang into the left margin, each undented by exactly its own width:

  ```fsharp
  let f a b c =
        a
      + b
     |> c
  ```

  `a`, `b`, and `c` all sit at column 6 — the SeqBlock's offside line. `+` (one character) undents to column 4; `|>` (two characters) undents to column 3. Each operator may reach as far left as `offsideLine - (tokenSize + 1)`; the operator and the single space after it then span exactly `tokenSize + 1` characters, so the operand that follows lands right back on the offside line, however wide the operator. Strict offside would reject every one of these operator lines.

  It is a deliberate, almost fussy affordance — and, as far as I can tell, a forgotten one. No tutorial mentions it, no documentation I've found describes it, and Fantomas — the de-facto F# formatter — never emits it. I know the rule exists because it's in the spec, and I know what it's *for* only because the column arithmetic resolves cleanly in just this one reading: alignment by design, rediscovered by reading §15.1.9 closely enough to ask what the "plus one" was buying.
- In a `_Type_` context, `}`, `end`, `and`, and `|` may align with `type`. Same closing-token logic as `for`/`done`.

§15.1.10 is bigger. It enumerates four constructs whose bodies are allowed to break the indentation rule altogether:

- **15.1.10.1** *Bodies of function expressions.* The body of a `fun` or `function` may undent from the keyword. The example the spec gives:

  ```fsharp
  let HashSample(tab: Collections.HashTable<_,_>) =
      tab.Iterate (fun c v ->
          printfn "Entry (%O,%O)" c v)
  ```

  The `printfn` at column 8 is left of `fun` at column 17. Strict offside would reject it; 15.1.10.1 allows it.

- **15.1.10.2** *Branches of if/then/else expressions.* A parenthesised body following `then` or `else` may undent to the offside line of `if`:

  ```fsharp
  if day = System.DayOfWeek.Monday then (
      printf "I don't like Mondays"
  )
  ```

- **15.1.10.3** *Bodies of modules and module types.* `begin`/`end` and `class`/`end` blocks may undent to the offside line established by `type` or `module`:

  ```fsharp
  module MyNestedModule = begin
      let one = 1
      let two = 2
  end
  ```

- **15.1.10.4** *Bodies of collection and computation expressions.* The bodies of `[`, `[|`, and `seq { ... }`-style braces may undent to the enclosing expression's offside line, *specifically ignoring the SeqBlock introduced by `(` or `=`*:

  ```fsharp
  Class.Method(seq {
      ...
  })

  Class.Method(arg1=expr1, arg2=expr2, [
      ...
  ])
  ```

  `Class.Method` establishes the offside line that the bracketed body is measured against, not the SeqBlock that the `=` or `(` introduced.

The spec is admirably terse. The implementation isn't.

## `isPermittedUndentation`, the one function

The lexical filter from post 4 fires the offside check every time a token is fetched. When the token's column is at or below the head context's `Indent`, the filter calls one function before deciding whether to reject:

```fsharp
let rec private isPermittedUndentation
    (token: Token)
    (tokenCol: int)
    (context: Offside list)
    (state: ParseState)
    (readerIndex: int64)
    : string voption
```

Five arguments, returning `string voption`. `ValueSome rule` means "permitted, here's the rule that fired"; `ValueNone` means "reject as offside." The string is for tracing only — every permission carries the spec section it implements (`"15.1.9 InfixUndent"`, `"15.1.10.4 SeqBlockParen"`, etc.) so a `--trace` run can show which rule rescued which token. When a corpus file fails on what looks like a layout bug, the trace tells you immediately whether the rule fired and matched the wrong condition or didn't fire at all.

The body is one cascading `if`/`elif` chain, in a deliberate order. Closing-delimiter exemptions first (`)`, `]`, `}` are never offside from their matching opener). Then 15.1.9's specific alignment exceptions. Then 15.1.10's broader permissions. Then a fall-through `ValueNone`. The order matters: the cheapest checks come first, and the structural rules at the end depend on the earlier ones already having handled their special cases.

## What the spec asks for and what the code shipped with

The first pass through this code implemented all four 15.1.10 rules faithfully. Each had its own arm in the `match`. The corpus then did what the corpus does, and after a few months of running the F# compiler's source through the parser, two of those four arms had never fired. Not "rarely fired." Never. The commits `0ee9fa9 Eliminate undentation rule 15.1.10.3 as dead code` and `d261f35 Eliminate undentation rule 15.1.10.2 as dead code` removed them. The reason they could be removed is the design choice from post 6:

```fsharp
// 15.1.10.2 (if/then/else + paren/begin undentation) and 15.1.10.3 (module/class
// body undentation inside begin/end) are intentionally omitted. Both spec rules
// exist because F#'s Lexical Filtering step retrofits offside onto a token stream
// after lexing, requiring special cases for paren-like frames. XParsec.FSharp is
// offside-aware by construction: Paren and Begin are pushed (by pEnclosed and
// withContextAt) with Indent=0 as pure stack markers, so they can never be the
// head context in an offside check (tokenCol < 0 is impossible). Content inside
// `(...)` or `begin...end` is bounded by the SeqBlock inside pInner, which is
// handled by the SeqBlockParen arm of tryCollectionUndent below.
```

The spec's 15.1.10.2 and 15.1.10.3 exist because FCS's pre-parse runs *over a token stream that has already been lexed*. By the time pre-parse sees `(`, the bracket has already been emitted as a regular token, and pre-parse has to retrofit a frame onto its stack with the `(`'s column as the offside line. That column is a real number on the line, so the offside check would otherwise reject anything left of it — and the spec has to *re-permit* the body inside.

XParsec's parsers push paren-like frames with `Indent = 0` directly. A column of zero is unreachable, so the check trivially passes; the body never needed re-permitting. The two spec rules existed to undo a problem that the project's choice of representation didn't create.

This was not the framing I started with. I implemented all four 15.1.10 sub-sections because that's what the spec said. The dead-code finding came later — after a corpus run failed to produce a single trace event for `"15.1.10.2"` or `"15.1.10.3"`, even on F# files chock full of nested `(` and `begin`. The rules still appeared in the code, plumbed through, untriggered. Once I understood why, deleting them simplified the function meaningfully — eight fewer arms, one fewer container-walking loop. The dead-code result is the project earning its own design back. The rule that the spec needs and the rule that the parser needs aren't always the same rule.

## 15.1.10.1: function bodies and the SeqBlock+Paren walk

The function-body rule survived in close-to-spec form, but only after a corpus discovery extended it. The shape it shipped with:

```fsharp
elif head.Context = OffsideContext.Fun || head.Context = OffsideContext.Function then
    let rec findEnclosingIndent (stack: Offside list) =
        match stack with
        | [] -> true
        | ctx :: deeper ->
            match ctx.Context with
            | OffsideContext.SeqBlock
            | OffsideContext.Paren
            | OffsideContext.Bracket
            | OffsideContext.BracketBar
            | OffsideContext.BraceBar
            | OffsideContext.Brace
            | OffsideContext.Begin
            | OffsideContext.Fun
            | OffsideContext.Function -> findEnclosingIndent deeper
            | _ -> tokenCol >= ctx.Indent

    if findEnclosingIndent rest then
        ValueSome "15.1.10.1 FunBody"
    else
        ValueNone
```

The body of a `fun` may undent past the `fun` keyword's column, but it must not undent past *other* offside lines. The function walks the stack underneath the head, skipping past frames that don't contribute their own offside line, and asks whether the token is still at or right of the first frame that does.

Three of the skip cases are predictable: SeqBlock (because the SeqBlock that wraps a fun body is bookkeeping, not a real line), the six paren-like contexts (post 6's markers), and Fun/Function recursively. The recursive Fun/Function entry is the corpus extension: commit `cc5933a Add fun and function keywords as allowed undentations in fun and function bodies`. Without it, a chain of nested lambdas — common in computation-expression-heavy F# — couldn't unwind. A `fun a -> fun b -> body` whose innermost body undented past the outer `fun` would be checked against the outer `fun`'s column, even though the *real* enclosing line was further out. Adding `Fun` and `Function` to the skip list let the walk continue past intermediate lambdas to whatever real construct enclosed them.

The same paragraph of code, with one extra case, also handles `MatchClauses`:

```fsharp
elif head.Context = OffsideContext.MatchClauses && token <> Token.OpBar then
    // ... same shape, slightly larger skip list ...
```

The reasoning is paragraphed into the source: `MatchClauses` controls `|` alignment, and only that. A non-`|` token inside a match rule (a guard `when`, an `->`, the rule body) has nothing to do with the pattern column; it should be measured against the enclosing `Match` or `Function` context, just like a `fun` body. Same skip-past-bookkeeping walk, with `Match` and `MatchClauses` added to the skip list.

## 15.1.10.4: where most of the real work is

15.1.10.4 is the rule the spec describes most loosely and the rule the corpus exercised the hardest. The shape that landed:

```fsharp
elif
    head.Context = OffsideContext.Bracket
    || head.Context = OffsideContext.BracketBar
    || head.Context = OffsideContext.BraceBar
    || head.Context = OffsideContext.Brace
then
    tryCollectionUndent tokenCol context

elif head.Context = OffsideContext.SeqBlock then
    tryCollectionUndent tokenCol context
```

Two arms feed the same helper. The first is the spec case verbatim: a token directly inside `[`, `[|`, `{|`, or `{` may undent to the enclosing expression's offside line. The second is the extension that made the rule actually work: when `withContext` pushes a SeqBlock on top of a paren-like frame (because `pEnclosed` did its `Indent = 0` push first, and then the inner parser opened its own SeqBlock for the contents), the head is the SeqBlock and the paren-like frame is *one below it*. Without this arm, the rule would fail every time a parenthesised expression contained a multi-line body — which is most of them.

The helper is `tryCollectionUndent`:

```fsharp
and private tryCollectionUndent (tokenCol: int) (stack: Offside list) : string voption =
    match stack with
    | { Context = OffsideContext.SeqBlock } :: { Context = ctx } :: deeper when isParenLike ctx ->
        if checkCollectionUndent tokenCol deeper then
            ValueSome "15.1.10.4 SeqBlockParen"
        else
            ValueNone
    | { Context = ctx } :: rest when isParenLike ctx ->
        if checkCollectionUndent tokenCol rest then
            ValueSome "15.1.10.4 Collection"
        else
            ValueNone
    | _ -> ValueNone
```

Two patterns. `SeqBlock :: Paren-like :: deeper` is the SeqBlock-on-top case; `Paren-like :: rest` is the bare-paren-like case. Both delegate to `checkCollectionUndent`, which walks past the paren-like layers looking for the real outer offside line:

```fsharp
and private checkCollectionUndent (tokenCol: int) (stack: Offside list) : bool =
    match stack with
    | [] -> true
    | ctx :: deeper ->
        match (ctx: Offside).Context with
        | OffsideContext.SeqBlock
        | OffsideContext.Fun
        | OffsideContext.Function -> checkCollectionUndent tokenCol deeper
        | c when isParenLike c -> checkCollectionUndent tokenCol deeper
        | _ ->
            tokenCol >= ctx.Indent
```

The skip list is similar to `findEnclosingIndent` from 15.1.10.1. SeqBlock, Fun, Function, and any paren-like — none of these contribute an offside line that can validly bound an undented body, because they're either bookkeeping or known-permissive containers. The first context in the stack that *isn't* one of those is the line the token has to clear. `Fun` and `Function` landed in this skip list for the same reason they landed in `findEnclosingIndent` — commit `a67b3cf Allow undentation rule 15.1.10.4 for fun and function keywords`, the collection-side counterpart of the lambda-chain fix above.

What this captures is the subtle phrasing from the spec: undentation is allowed *to the offside line of the enclosing expression, ignoring the SeqBlock introduced by `(` or `=`*. The spec's "ignoring" is the skip; the "enclosing expression" is the first non-bookkeeping frame the walk lands on. `Class.Method(seq { ... })` works because `Class.Method`'s SeqBlock is the first non-bookkeeping frame, and its offside line is column 0 (or wherever `Class.Method` started); the body of `seq` may undent to that.

The chain of corpus discoveries that drove this section reads like a list of common F# shapes that a strict offside check rejects:

- `616b892 Fix undentation interaction between parens and SeqBlocks` — the SeqBlock-on-top extension above. The first time I saw a paren and a SeqBlock interact in a way that wasn't covered, I assumed it was a one-off. It wasn't.
- `c5bb9cd Fix allowed undentation in infix in paren context` — multi-line `(` `expr |> f` `|> g` `)` blocks where the leading `|>` is the SeqBlock-infix exception, but only when the SeqBlock is recognised as inside a paren.
- `d0af4ca Fix allowed undentation in application` — application chains across newlines like `f x` `  y` `  z`. The continuation arguments need to clear the call site's offside line, not the SeqBlock that the application happens to have opened.
- `930c99c Fix undentation of match expressions in parens` — the closing `|`, `with`, and `finally` of a match/try inside a paren can undent to the enclosing expression. This added the `MatchParen` arm in `isPermittedUndentation`.
- `6ee954a Add allowed undentation in multiline function expression` — multi-line `function | A -> ... | B -> ...` whose `|`s sit further left than the `function` keyword.

Each commit added a corpus test, ran it through the harness, and either narrowed an existing rule or carved a new arm into `isPermittedUndentation`. The shape of the code at the end of that process is the shape `isPermittedUndentation` is now: a long, deliberate cascade of cases, each carrying a spec citation, each backed by at least one corpus test that catches the specific phrase of F# the rule needs to permit.

## elif anchoring: a column the spec doesn't quite name

§15.1.9 says `elif` may align with `if`. That's clear. What it doesn't say — and what burned a long afternoon — is what column the *body* under `elif` is anchored at.

The naive answer is "the column of `elif`," and it's wrong. F#'s lex filter has a piece of sugar: `else if` written as two tokens collapses into a single chain arm equivalent to `elif`. The user can write either, and the AST has to treat them identically. But if the body's offside column is anchored at the `elif` keyword's column for the `elif` form and at the `else` keyword's column for the `else if` form, the two forms will accept different inputs — and the tests fail in confusing ways, because the discrepancy doesn't surface until a multi-line body of the chained branch starts undenting against the wrong line.

The shape that landed (in `pConditionThen`, after `2adcbb9 Fix anchoring offside column for if-elif-else if chains`):

```fsharp
// Anchor the arm's If and Then contexts at (indent + 1). `indent` is the
// chain's effective offside column — the leftmost of the arm's keywords:
//   - `elif`     — elif's column
//   - `else if`  — min(else_col, if_col).
let pConditionThen (indent: int) (armKeyword: SyntaxToken) reader =
    let pCond     = withContextAt OffsideContext.If   (indent + 1) armKeyword.PositionedToken refExpr.Parser
    let pThenExpr = withContextAt OffsideContext.Then (indent + 1) armKeyword.PositionedToken refTypedSeqExprBlock.Parser
    ...
```

And the caller, in `parseBranches`:

```fsharp
| Ok(ElIfTok.ElseIf(elseTok, ifTok)) ->
    // Leftmost of the pair is the chain's alignment column.
    let indent = min (getIndent reader elseTok) (getIndent reader ifTok)
    match pConditionThen indent elseTok reader with
    ...
```

The chain's effective column is the *leftmost* of the arm's introducing keywords. For `elif`, that's just `elif`'s column. For `else if`, it's `min(else_col, if_col)` — usually `else_col` for same-line shapes, but `if_col` for the multi-line case where the `if` starts the next line indented further than `else`. The condition and the then-branch are then anchored at `indent + 1` so that a condition or body undented onto the following line stays within the If/Then context.

This is the kind of detail that doesn't show up in the spec because the spec talks about the rule at the language level, not at the implementation level. The lex filter sugar that collapses `else if` into `elif` is invisible from the spec's vantage point; the parser has to decide which column wins, and there isn't a "right" answer until you've looked at a few hundred files of real F# and noticed which choice doesn't break them.

## The indentation bug that wasn't

Chasing the elif chain turned up two bugs. The `else if` reconciliation above was one. The other one wasn't in the offside code at all — and it took the longest to find, precisely because I kept looking in the offside code.

The symptom was a multi-line conditional lifted from the compiler's own `fsi.fs`, failing the offside check on a statement that was plainly at the right indent. I went back through the rules in `isPermittedUndentation` more than once before accepting that none of them was wrong. The context stack was right. The offside lines were right. The token's *column* was wrong.

Columns aren't a field the lexer emits; they're computed on demand. `getIndent` takes a token and subtracts its line's starting offset from its own:

```fsharp
token.StartIndex - lineStartToken.StartIndex
```

`lineStartToken` is the first token of whatever line the token sits on, and finding that line is a binary search over `LineStarts`, the array mapping each line to its first token. Return the wrong line and the subtraction uses the wrong base; the column comes out shifted by the gap between two lines' offsets — tens of characters, easily. A column off by tens of characters reads as wildly offside. The offside machinery was doing exactly what it was told. It was being told the wrong number.

It was a floor search — find the largest line whose start is at or before the token — and the bug was the textbook one:

```fsharp
// before
if midVal > index then search low (mid - 1<line>)
else                    search (mid + 1<line>) high   // discards a valid answer

// after
if lineStarts.[mid] <= index then search mid high     // keeps mid as a candidate
else                              search low (mid - 1<line>)
```

When `lineStarts[mid] <= index`, `mid` is itself a valid floor — the answer might *be* `mid`. The old code stepped past it to `mid + 1` and lost it, overshooting by a line for any token sitting on a line boundary. Most tokens aren't on a boundary, which is why it hid so long; an elif chain, whose conditions and bodies keep starting fresh lines, is exactly the shape that puts tokens on boundaries.

There was a tell. The midpoint is computed upper-biased — `low + (high - low + 1) / 2` — under a comment that it's "to prevent infinite loops." That ceiling is only needed when the keep-`mid` branch recurses as `search mid high`; otherwise `low + 1 = high` spins forever. The old code had the ceiling but recursed to `mid + 1`, guarding against a loop it couldn't reach — a sign the invariant had been half-remembered. The fix made both correct at once: keep `mid`, recurse `search mid high`, base case `low`, precondition `lineStarts[low] <= index` held throughout.

The fix is `59ca412 Fix findLineNumberImpl`: four lines of search logic, and one golden-file change — a `#warnon` directive that had been logged on line 4 moving to line 3. That one-line correction is the entire bug, written in the language of its symptom.

The lesson stuck. The offside check (post 4), the context stack (post 6), and the undentation rules (this post) can all three be correct and the parser can still mislay a token, because all three sit on a column that a binary search several layers down is responsible for computing. When a layout test fails, "which offside rule is wrong" is the second question. The first is "is the column even right."

## What the corpus produces

Reading the commit log against `isPermittedUndentation` is reading a list of small F# shapes that a strict reading of the spec would reject. Multi-line tuples whose elements undent. Function chains with leading pipes. `match` expressions in argument position. `fun` lambdas inside collection initialisers. Each commit is a single test file added to the corpus, a single arm added or extended in the function, and a single `git log` line that's almost interchangeable with the file name.

Cumulatively, they're the gap between the spec and what F# actually is. The spec is correct about the rules; it's calibrated for a faithful pre-parse implementation. The corpus is calibrated for the language a person actually writes. Closing the gap is what `isPermittedUndentation` does, and the only honest way to know whether it's done is to run a few hundred real files through it and count diagnostics.

The spec's offside rule is cheap in principle. Each specific case of it is an afternoon.

## Anchor commits / files

- `src/XParsec.FSharp/ParsingHelpers.fs` — `isPermittedUndentation`, `tryCollectionUndent`, `checkCollectionUndent`, `contextPermitsToken`, `contextPermitsTokenBounded`. The whole rule cascade lives here.
- `src/XParsec.FSharp/ExpressionParsing.fs` — `ElifBranches`, `pConditionThen`, the if-chain anchoring.
- `src/XParsec.FSharp/ParsingTypes.fs` — `findLineNumberImpl`, `getIndent`. The line-number binary search behind every column the offside check reads.
- `a86e642 Add permitted undentation infrastructure` — the first scaffolding for 15.1.10.
- `ebbb01e Handle offside exceptions and permitted undentations` — the first complete pass through 15.1.9 and 15.1.10.
- `1a19aff Refactoring isPermittedUndentation rule 15.1.10.4` — the SeqBlock+Paren walk lands.
- `cc5933a Add fun and function keywords as allowed undentations in fun and function bodies` — the recursive-fun extension in `findEnclosingIndent` (15.1.10.1); `a67b3cf Allow undentation rule 15.1.10.4 for fun and function keywords` — the same skip added to `checkCollectionUndent` (15.1.10.4).
- `0ee9fa9 Eliminate undentation rule 15.1.10.3 as dead code` and `d261f35 Eliminate undentation rule 15.1.10.2 as dead code` — the dead-code discoveries.
- `046d26e Fix indentation context of elif branches` and `2adcbb9 Fix anchoring offside column for if-elif-else if chains` — the `else if` / `elif` column reconciliation.
- `59ca412 Fix findLineNumberImpl` — the line-number binary-search bug, found while debugging the elif chain.
- `616b892`, `c5bb9cd`, `d0af4ca`, `930c99c`, `6ee954a` — the corpus chain. Each adds one test file and one arm or skip-list entry.

## Takeaway

Four ideas earned their place.

**The spec rules reflect the spec's implementation strategy.** §15.1.10.2 and §15.1.10.3 exist to undo problems that an offside-aware-by-construction parser doesn't create. The `Indent = 0` choice from post 6 is what made them dead code. Following the spec literally would have meant carrying two arms whose only purpose was to repair damage the project had decided not to do.

**One function, ordered cascade, one trace string per arm.** `isPermittedUndentation` is a flat `if`/`elif` chain in deliberate order: cheap exceptions first, structural rules last. Every branch tags its return with the spec section it implements. When something fails, `--trace` shows which arm fired or didn't, and that's the difference between a five-minute fix and an afternoon of guessing.

**Corpus extensions outnumber spec rules.** The shape `isPermittedUndentation` ended at is mostly the spec rules, and partly extensions: `Fun`/`Function` recursing through the skip list, `MatchClauses` for non-`|` tokens, `else if` collapsing to its leftmost keyword's column. Each extension came from a corpus test that didn't pass and refused to be coerced into one of the existing arms. The spec stops where the spec authors had no reason to keep going; the corpus doesn't.

**The `(` `SeqBlock :: Paren-like :: deeper` walk does most of the work.** When real F# code spans multiple lines inside parens or brackets, the offside line that bounds the body is rarely the head context. It's two or three frames down. `checkCollectionUndent` walks past the bookkeeping frames to find the line that actually matters. This single walk subsumes most of what 15.1.10.4 needed, and most of what the corpus discoveries needed too.

The offside check from post 4 reads one comparison; the stack from post 6 keeps the data; this function is the bridge that makes both of those work for real source. Three layers, each cheap on its own, expensive only when read as a single mechanism. And once it's right, the parsers above stop knowing about layout entirely — which is the whole point.
