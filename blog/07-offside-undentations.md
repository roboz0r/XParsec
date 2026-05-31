# 7. The offside rule, part 2: permitted undentations

If F# strictly enforced "no token below the offside line," then F# would be much less legible and pleasant to use. The spec dedicates two subsections to where and how the lexical filter is allowed to deviate: [§15.1.9](https://fsharp.github.io/fslang-spec/lexical-filtering/#1519-exceptions-to-the-offside-rules) enumerates *exceptions to the offside rules*, and [§15.1.10](https://fsharp.github.io/fslang-spec/lexical-filtering/#15110-permitted-undentations) lists *permitted undentations*.

Post 6 ended with the stack discipline, the mechanical state tracking of the context stack, and treating paren-like contexts as `Indent = 0` markers: a column no token can ever be left of, which effectively makes them a safe harbour from the offside check. This post describes how that data structure is inspected and used to enforce the requirements transparently to the AST parsers. We'll first look at the rules as written in the spec, then how my implementation interprets and enforces them.

## Exceptions to the Offside Rules

§15.1.9 covers small exceptions. Most of them say the same thing in different shapes: a token at a particular alignment with the head context's offside line is acceptable, even though strict offside would reject it or lead it to be parsed differently. The very first is likely surprising to even experienced F# developers: "In a *SeqBlock* context, an infix token may be offside by the size of the token plus one."

```fs
let f a b c =
      a         // column 6 - the SeqBlock's offside line
    + b         // column 4 - '+' (1 char) undents by tokenSize + 1 = 2
   |> c         // column 3 - '|>' (2 chars) undents by tokenSize + 1 = 3
```

It is a deliberate, almost fussy affordance, and, as far as I can tell, a forgotten one. No tutorial mentions it, no documentation I've found describes it. [Fantomas](https://fsprojects.github.io/fantomas/), the de-facto F# formatter, never emits it. I know the rule exists because it's in the spec, and I know what it's for only because the column arithmetic resolves cleanly to provide the visually striking result.

To elaborate, `a`, `b`, and `c` all sit at column 6, the SeqBlock's offside line. `+` (one character) undents to column 4; `|>` (two characters) undents to column 3. Each operator may reach as far left as `offsideLine - (tokenSize + 1)`; the operator and the single space after it then span exactly `tokenSize + 1` characters, so the operand that follows lands right back on the offside line, however wide the operator. This is handled quite cleanly, it's the first arm of 15.1.9 in `isPermittedUndentation`:

```fsharp
// SeqBlock infix: an infix token may be offside by (tokenSize + 1)
elif head.Context = OffsideContext.SeqBlock && isInfixToken token then
    let tokenLength = getTokenLength state (int readerIndex * 1<token>)

    if tokenCol >= head.Indent - (tokenLength + 1) then
        ValueSome "15.1.9 InfixUndent"
    else if
        // Still check deeper contexts, but don't let bars pass through MatchClauses
        contextPermitsTokenBounded token tokenCol rest
    then
        ValueSome "15.1.9 ContextPermits"
    else
        // 15.1.10.4 fallback: infix undentation exceeded, but if the SeqBlock
        // sits directly inside a paren-like context, delegate to the collection
        // undentation rule.
        tryCollectionUndent tokenCol context
```

The `tokenCol >= head.Indent - (tokenLength + 1)` is the column arithmetic spelled out directly: `getTokenLength` measures the operator's width in characters, and the token is permitted as long as it reaches no further left than `tokenLength + 1` past the offside line. The other two branches are the fallbacks: a deeper context that permits the token, or a SeqBlock-inside-paren case that 15.1.10.4 handles.

The next rule is also regarding the use of infix operators, but appears regularly in user code, typically with the pipe operator `|>`: "In a SeqBlock context, an infix token may align precisely with the offside line of the SeqBlock."

```fs
let someFunction(someCollection) =
    someCollection                  // column 4 - the SeqBlock's offside line
    |> List.map (fun x -> x + 1)    // column 4 - '|>' aligns precisely with it
```

Without this rule, `|>` on the next line would start a new expression (after inserting a virtual `;`) in the SeqBlock rather than passing `someCollection` into the `map`. In XParsec this case needs no rule of its own. The offside check only fires when a token is *strictly* left of the offside line:

```fsharp
let tokenCol = ParseState.getIndent reader.State (reader.Index * 1<token>)

if tokenCol < contextIndent then
    match isPermittedUndentation token.Token tokenCol context reader.State reader.Index with
    | ValueSome rule -> (* permitted *) false
    | ValueNone      -> (* reject as offside *) true
else
    false
```

This snippet computes `isOffside`, so the polarity is inverted: returning `false` means "not offside, admit the token," and `true` means "reject." A `|>` that aligns *precisely* with the SeqBlock's offside line has `tokenCol = contextIndent`, so `tokenCol < contextIndent` is false, the `else` branch returns `false`, and the check never reaches `isPermittedUndentation` at all. The token is admitted. The "may align precisely" exception is satisfied by the comparison being strict; the `tokenSize + 1` arm above is what handles the genuinely-offside case where the operator undents *past* the line.

But passing the offside check is only half of it. The spec's worry is the *virtual* `;`: a token at the SeqBlock's offside line is the signal to terminate the current expression and start a new one in the sequence. If that fired on the aligned `|>`, the `someCollection` above would close off as its own statement and the `|>` would dangle. The reason it doesn't is `pSepVirt`, the SeqBlock's separator parser. A virtual separator is only synthesised when the next token can actually *begin* an expression:

```fsharp
let pSepVirt =
    parser {
        match! peekNextNonTriviaToken with
        | t when t.Token = Token.EOF -> return! failSep
        | t when t.Token = Token.OpSemicolon -> return! consumePeeked t
        | t ->
            // Only emit VirtualSep when the next token can actually start an expression.
            // Tokens that cannot start expressions (closing delimiters, block-continuation
            // keywords like `with`/`finally`/`then`/`else`, pure infix operators, etc.) must
            // NOT trigger VirtualSep - doing so causes infinite loops in the Pratt parser's
            // InfixNary handler because the zero-width virtual token never advances the reader.
            if TokenInfo.canStartExpression t.Token then
                let! indent = currentIndent
                let! state = getUserState

                let atContextIndent =
                    match state.Context with
                    | { Indent = ctxIndent } :: _ -> indent = ctxIndent
                    | [] -> indent = 0

                if atContextIndent then
                    return virtualToken (PositionedToken.Create(Token.VirtualSep, t.StartIndex))
                else
                    return! failSep
            else
                return! failSep
    }
```

`TokenInfo.canStartExpression`'s final arm admits operators *only* when they can be a prefix:

```fsharp
let canStartExpression (token: Token) =
    match token with
    | Token.Identifier
    | ... // identifiers, literals, opening delimiters, if/match/fun/let/...
    | _ when isLiteral token -> true
    // Prefix operators (-, +, !, ~, &, &&, .., *, not, etc.)
    | _ when isOperator token && canBePrefix token -> true
    | _ -> false
```

`|>` is purely infix, `canBePrefix Token.OpPipeRight` is false, so `canStartExpression` returns false, `pSepVirt` falls to `failSep`, and no `VirtualSep` is produced. The SeqBlock's sequencing loop ends, control returns to the Pratt parser one level up, and it consumes the aligned `|>` as an infix continuation of `someCollection`. So the two halves cooperate: the strict offside comparison lets the operator *through*, and the prefix-only `canStartExpression` gate ensures it is *not* mistaken for the start of a new statement. The comment also records the harder consequence of getting this wrong: a zero-width `VirtualSep` that doesn't advance the reader sends the Pratt parser's `InfixNary` application handler into an infinite loop.

By coordinating the strict offside check with the virtual-separator logic, the parser handles a notorious edge case without a bespoke layout rule at all. With that out of the way, the rest of §15.1.9 is the more conventional kind of exception: keywords aligning with the construct that introduced them.

### Alignment in expressions

The remaining rules are all around aligning keywords or symbolic keywords with their initiating offside line:

- In a Let context, the `and` token may align precisely with the `let` keyword.
- In a Type context, the `}`, `end`, `and`, and `|` tokens may align precisely with the type keyword.
- In a For context, the `done` token may align precisely with the `for` keyword.
- In a Match context, on the right-hand side of an arrow for a match expression, a token may align precisely with the `match` keyword.
  - This exception allows the last expression to align with the match, so that a long series of matches does not increase indentation.
- In an Interface context, the `end` token may align precisely with the `interface` keyword.
- In an If context, the `then`, `elif`, and `else` tokens may align precisely with the `if` keyword.
- In a Try context, the `finally` and `with` tokens may align precisely with the `try` keyword.
- In a Do context, the `done` token may align precisely with the `do` keyword.

All of these keyword-alignment exceptions share a single mechanism. They're not separate arms in `isPermittedUndentation`, just rows in one lookup, `contextPermitsToken`, which asks "does *this* context, sitting somewhere in the stack, permit *this* token at *this* column?":

```fsharp
let private contextPermitsToken (token: Token) (tokenCol: int) (ctx: Offside) =
    ctx.Indent <= tokenCol
    && (
        match ctx.Context, token with
        // 15.1.9: then/elif/else may align with if
        | OffsideContext.If, (Token.KWThen | Token.KWElif | Token.KWElse) -> true
        // 15.1.9: with/finally/| may align with try
        | OffsideContext.Try, (Token.KWWith | Token.KWFinally | Token.OpBar) -> true
        // 15.1.9: done may align with for
        | OffsideContext.For, Token.KWDone -> true
        // 15.1.9: done may align with do
        | OffsideContext.Do, Token.KWDone -> true
        // 15.1.9: and may align with let
        | OffsideContext.Let, Token.KWAnd -> true
        // 15.1.9: }, end, and, | may align with type
        | OffsideContext.Type, (Token.KWRBrace | Token.KWEnd | Token.KWAnd | Token.OpBar) -> true
        // 15.1.9: end may align with interface (WithAugment)
        | OffsideContext.WithAugment, Token.KWEnd -> true
        // 15.1.9: with/| may align with match
        | OffsideContext.Match, (Token.KWWith | Token.OpBar) -> true
        // 15.1.9: | may align with function
        | OffsideContext.Function, Token.OpBar -> true
        // 15.1.9: done may align with while
        | OffsideContext.While, Token.KWDone -> true
        | _ -> false
    )
```

Read against the bullet list above, this is a near-transliteration of §15.1.9: the `let`/`and` line is `OffsideContext.Let, Token.KWAnd -> true`, the `if`/`then`/`elif`/`else` line is the first arm, and so on. The `ctx.Indent <= tokenCol` guard out front says "the token may align with *or* sit right of the keyword's offside line, but not left of it", exactly what the spec means by "may align precisely."

The `Let` context's offside line is the column of the `let` keyword itself: `pLetOrUseDefn` is pushed `withContextAt OffsideContext.Let indent token`, where `indent` is `getIndent` of the `let`/`use` keyword. So for

```fsharp
let rec even n = n = 0 || odd (n - 1)
and odd n = n <> 0 && even (n - 1)
```

the `and` at column 0 isn't measured against the `Let` frame directly. The binding body `n = 0 || odd (n - 1)` opened its own SeqBlock further right, and *that* SeqBlock is the head context when the filter reaches `and`. `tokenCol (0) < contextIndent` is true, so the check escalates to `isPermittedUndentation`, which scans the enclosing stack with `contextPermitsTokenBounded`. The `Let` frame, sitting below the body's SeqBlock, matches the `OffsideContext.Let, Token.KWAnd` row; its indent of 0 satisfies `ctx.Indent <= tokenCol`, and the token is admitted. Without the row, `and` would be offside against the binding body's SeqBlock and the second binding would be rejected.

### `elif` anchoring: a column the spec doesn't quite name

§15.1.9 says `elif` may align with `if`. That's clear. What it doesn't say, and what burned a long afternoon, is what column the *body* under `elif` is anchored at.

The naive answer is "the column of `elif`", and it's wrong. F#'s lex filter has a piece of sugar: `else if` written as two tokens collapses into a single chain arm equivalent to `elif`. The user can write either, and the AST has to treat them identically. But if the body's offside column is anchored at `elif` for one form and at `else` for the other, the two forms accept different inputs. And the tests fail in deeply confusing ways, because the discrepancy doesn't surface until a multi-line body of the chained branch starts undenting against the wrong line.

The shape that landed (in `pConditionThen`, after `2adcbb9 Fix anchoring offside column for if-elif-else if chains`):

```fsharp
// Anchor the arm's If and Then contexts at (indent + 1). `indent` is the
// chain's effective offside column - the leftmost of the arm's keywords:
//   - `elif`     - elif's column
//   - `else if`  - min(else_col, if_col).
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

The chain's effective column is the *leftmost* of the arm's introducing keywords. For `elif`, that's just `elif`'s column. For `else if`, it's `min(else_col, if_col)`, usually `else_col`, but `if_col` for the multi-line case where the `if` starts the next line indented further than `else`. The condition and the then-branch are then anchored at `indent + 1` so that a condition or body undented onto the following line stays within the If/Then context.

This is the kind of detail that doesn't show up in the spec because the spec talks about the rule at the language level, not at the implementation level. The sugar that collapses `else if` into `elif` is invisible from the spec's vantage point; the parser has to decide which column wins, and there isn't a "right" answer until you've looked at a few hundred files of real F# and noticed which choice doesn't break them.

### The indentation bug that wasn't

Chasing the `elif` chain turned up two bugs. The `else if` reconciliation above was one. The other one wasn't in the offside code at all, and it took the longest to find, precisely because I kept looking in the offside code and didn't question my basic arithmetic.

The symptom was a multi-line conditional lifted from the compiler's own `fsi.fs`, failing the offside check on a statement that was plainly at the right indent. I went back through the rules in `isPermittedUndentation` more than once before accepting that none of them was wrong. The context stack was right. The offside lines were right. The token's *column* was wrong.

Columns aren't a field the lexer emits; they're computed on demand. `getIndent` takes a token and subtracts its line's starting offset from its own:

```fsharp
token.StartIndex - lineStartToken.StartIndex
```

`lineStartToken` is the first token of whatever line the token sits on, and finding that line is a binary search over `LineStarts`, the array mapping each line to its first token. If the search returns the wrong line, the subtraction uses the wrong base. The column comes out shifted by the gap between two lines' offsets, easily tens of characters. A column off by tens of characters reads as wildly offside. The offside machinery was was working perfectly; It was being told the wrong number.

It was a floor search, find the largest line whose start is at or before the token, and the bug was the textbook one:

```fsharp
// before
if midVal > index then search low (mid - 1<line>)
else                    search (mid + 1<line>) high   // discards a valid answer

// after
if lineStarts.[mid] <= index then search mid high     // keeps mid as a candidate
else                              search low (mid - 1<line>)
```

When `lineStarts[mid] <= index`, `mid` is itself a valid floor, the answer might *be* `mid`. The old code stepped past it to `mid + 1` and lost it, overshooting by a line for any token sitting on a line boundary. Most tokens aren't on a boundary, which is why it hid so long; an `elif` chain, whose conditions and bodies keep starting fresh lines, is exactly the shape that puts tokens on boundaries.

There was a tell. The midpoint is computed upper-biased (`low + (high - low + 1) / 2`) under a comment that it's "to prevent infinite loops." That ceiling is only needed when the keep-`mid` branch recurses as `search mid high`; otherwise `low + 1 = high` spins forever. The old code had the ceiling but recursed to `mid + 1`, guarding against a loop it couldn't reach. The fix made both correct at once: keep `mid`, recurse `search mid high`, base case `low`, precondition `lineStarts[low] <= index` held throughout.

The fix is `59ca412 Fix findLineNumberImpl`: four lines of search logic, and one golden-file change: a `#warnon` directive that had been logged on line 4 moving to line 3. That one-line correction in the golden files, had it been noted at the time, could have saved hours of searching when it eventually remanifested after 100s of other well-parsed files deep in the `if-elif` offside logic.

The lesson stuck. The offside check (post 4), the context stack (post 6), and the undentation rules (this post) can all three be correct and the parser can still mislay a token, because all three sit on a column that a binary search several layers down is responsible for computing. When a layout test fails, "which offside rule is wrong" should have been the second question. The first is "is the column even right".

## Permitted Undentations

While §15.1.9 handles the micro-level alignment of specific tokens and keywords, §15.1.10 zooms out. It deals with macro-level structural exemptions, four constructs whose bodies are allowed to break the indentation rule altogether:

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

### `isPermittedUndentation`, the one function

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

Five arguments, returning `string voption`. `ValueSome rule` means "permitted, here's the rule that fired"; `ValueNone` means "reject as offside." The string is for tracing only; every permission carries the spec section it implements (`"15.1.9 InfixUndent"`, `"15.1.10.4 SeqBlockParen"`, etc.) so a `--trace` run can show which rule rescued which token. When a corpus file fails on what looks like a layout bug, the trace tells you immediately whether the rule fired and matched the wrong condition or didn't fire at all. Returning a literal here is barely more expensive than a `bool` and provides you with infinitely more information. "Avoid prematurely discarding context" has become one of my guiding principles when programming.

The body is one cascading `if`/`elif` chain, in a deliberate order. Closing-delimiter exemptions first (`)`, `]`, `}` are never offside from their matching opener). Then 15.1.9's specific alignment exceptions. Then 15.1.10's broader permissions. Then a fall-through `ValueNone`. The order matters: the cheapest checks come first, and the structural rules at the end depend on the earlier ones already having handled their special cases.

### What the spec asks for and what the code shipped with

The first pass through this code implemented all four 15.1.10 rules faithfully. Each had its own arm in the `match`. The corpus then did what the corpus does, and after a few months of running the F# compiler's source through the parser, two of those four arms had never fired. Not "rarely fired." Never. The commits `0ee9fa9 Eliminate undentation rule 15.1.10.3 as dead code` and `d261f35 Eliminate undentation rule 15.1.10.2 as dead code` removed them. The reason they could be removed is the design choice from post 6:

```fsharp
// 15.1.10.2 (if/then/else + paren/begin undentation) and 15.1.10.3 (module/class
// body undentation inside begin/end) are intentionally omitted. Both spec rules
// exist because FCS's LexFilter is a separate token-stream pass with no back-channel
// from the parser: it reconstructs nesting from tokens alone, so it pushes a paren
// context (CtxtParen) recording the bracket's own column as the offside line, then
// undentationLimit must walk the stack to re-anchor the body at the enclosing
// construct's column. XParsec.FSharp fuses offside tracking into the recursive-descent
// parser: Paren and Begin are pushed (by pEnclosed and withContextAt) with Indent=0
// as pure stack markers, so they can never be the head context in an offside check
// (tokenCol < 0 is impossible). Content inside `(...)` or `begin...end` is bounded by
// the SeqBlock inside pInner, which is handled by the SeqBlockParen arm of
// tryCollectionUndent below.
```

The reason is FCS is forced to implement these rules comes down to *where the offside logic lives* relative to the parser.

In FCS, offside is enforced by `LexFilter`, a separate pass that sits between the lexer and the FsYacc-generated parser. It's a token-stream transducer: tokens flow lexer → filter → parser, and the filter inserts the virtual block tokens (`OBLOCKBEGIN`, `OBLOCKEND`, `OBLOCKSEP`, …) that drive the grammar. Crucially the interaction is one-directional. The LALR parser has no back-channel to tell the filter "I'm now inside the arguments of an application" or "this paren is a sub-expression of an `if` branch." So the filter has to reconstruct the nesting *itself*, heuristically, from the tokens alone. When it sees a `(` it reacts by pushing a context built from what it can observe, the bracket's own column:

```fsharp
// LexFilter.fs - the filter reacts to the '(' token by recording its column
| (TokenLExprParen | SIG | INTERP_STRING_BEGIN_PART _), _ ->
    pushCtxt tokenTup (CtxtParen (token, pos))   // pos = the '(' token's position
    pushCtxtSeqBlock tokenTup NoAddBlockEnd
```

`CtxtParen of token * Position` carries a real column, and `StartCol = StartPos.Column` feeds the offside comparison. So a body inside the parens *is* offside against the bracket, and §15.1.10.2/.3 exist to walk back out: `undentationLimit` pattern-matches the stack shape (`CtxtParen … :: CtxtSeqBlock _ :: CtxtThen _ :: CtxtIf _ :: …`) to discover the *enclosing* construct and re-anchor the limit at *its* column instead of the bracket's. The rule is a repair for the fact that the filter could only ever see the bracket, not the construct that contains it.

XParsec has no separate filter pass. The offside context stack is pushed and popped by the *same* recursive-descent combinators that build the AST: `pEnclosed` is the function that consumes the `(`, parses the body, and consumes the `)`. Because the parser already knows structurally that it's inside a paren (it's the code that opened it), it pushes the paren frame with `Indent = 0` on purpose, as an inert marker. A column of zero is unreachable, so the frame can never be the limiting context; the body is bounded instead by the enclosing construct's own SeqBlock, which is sitting right there on the stack. The body never needed re-permitting, because no bracket column was ever recorded to permit it against. §15.1.10.2 and §15.1.10.3 are repairs for a token-driven filter; a structure-driven parser never inflicts the wound.

This was not the framing I started with. I implemented all four 15.1.10 sub-sections because that's what the spec said. The dead-code finding came later, after a corpus run failed to produce a single trace event for `"15.1.10.2"` or `"15.1.10.3"`, even on F# files chock full of nested `(` and `begin`. The rules still appeared in the code, plumbed through, untriggered. Once I understood why, deleting them simplified the function meaningfully: eight fewer arms, one fewer container-walking loop. The dead-code result is the project earning its own design back and proof that it's impossible to write a specification completely free of the parser's implementation details.

### 15.1.10.1: function bodies and the SeqBlock+Paren walk

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

Three of the skip cases are predictable: SeqBlock (because the SeqBlock that wraps a fun body is bookkeeping, not a real line), the six paren-like contexts (post 6's markers), and Fun/Function recursively. The recursive Fun/Function entry is the corpus extension: commit `cc5933a Add fun and function keywords as allowed undentations in fun and function bodies`. Without it, a chain of nested lambdas, couldn't unwind. A `fun a -> fun b -> body` whose innermost body undented past the outer `fun` would be checked against the outer `fun`'s column, even though the *real* enclosing line was further out. Adding `Fun` and `Function` to the skip list let the walk continue past intermediate lambdas to whatever real construct enclosed them.

The same block of code, with one extra case, also handles `MatchClauses`:

```fsharp
elif head.Context = OffsideContext.MatchClauses && token <> Token.OpBar then
    // ... same shape, slightly larger skip list ...
```

The reasoning is written right into the source comments: `MatchClauses` controls `|` alignment, and only that. A non-`|` token inside a match rule (a guard `when`, an `->`, the rule body) has nothing to do with the pattern column; it should be measured against the enclosing `Match` or `Function` context, just like a `fun` body. Same skip-past-bookkeeping walk, with `Match` and `MatchClauses` added to the skip list.

### 15.1.10.4: where most of the real work is

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

Two arms feed the same helper. The first is the spec case verbatim: a token directly inside `[`, `[|`, `{|`, or `{` may undent to the enclosing expression's offside line. The second is the extension that made the rule actually work: when `withContext` pushes a SeqBlock on top of a paren-like frame (because `pEnclosed` did its `Indent = 0` push first, and then the inner parser opened its own SeqBlock for the contents), the head is the SeqBlock and the paren-like frame is *one below it*. Without this arm, the rule would fail every time a parenthesised expression contained a multi-line body, which is most of them.

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

The skip list is similar to `findEnclosingIndent` from 15.1.10.1. SeqBlock, Fun, Function, and any paren-like, because they're either bookkeeping or known-permissive containers. The first context in the stack that *isn't* one of those is the line the token has to clear. `Fun` and `Function` landed in this skip list for the same reason they landed in `findEnclosingIndent`: commit `a67b3cf Allow undentation rule 15.1.10.4 for fun and function keywords`, the collection-side counterpart of the lambda-chain fix above.

What this captures is the subtle phrasing from the spec: undentation is allowed *to the offside line of the enclosing expression, ignoring the SeqBlock introduced by `(` or `=`*. The spec's "ignoring" is the skip; the "enclosing expression" is the first non-bookkeeping frame the walk lands on. `Class.Method(seq { ... })` works because `Class.Method`'s SeqBlock is the first non-bookkeeping frame, and its offside line is column 0 (or wherever `Class.Method` started); the body of `seq` may undent to that.

The chain of corpus discoveries that drove this section reads like a list of common F# shapes that a strict offside check rejects:

- `616b892 Fix undentation interaction between parens and SeqBlocks`: the SeqBlock-on-top extension above. The first time I saw a paren and a SeqBlock interact in a way that wasn't covered, I assumed it was a one-off. It wasn't.
- `c5bb9cd Fix allowed undentation in infix in paren context`: multi-line `(` `expr |> f` `|> g` `)` blocks where the leading `|>` is the SeqBlock-infix exception, but only when the SeqBlock is recognised as inside a paren.
- `d0af4ca Fix allowed undentation in application`: application chains across newlines like `f x` `  y` `  z`. The continuation arguments need to clear the call site's offside line, not the SeqBlock that the application happens to have opened.
- `930c99c Fix undentation of match expressions in parens`: the closing `|`, `with`, and `finally` of a match/try inside a paren can undent to the enclosing expression. This added the `MatchParen` arm in `isPermittedUndentation`.
- `6ee954a Add allowed undentation in multiline function expression`: multi-line `function | A -> ... | B -> ...` whose `|`s sit further left than the `function` keyword.

Each commit added a corpus test, ran it through the harness, and either narrowed an existing rule or carved a new arm into `isPermittedUndentation`. The shape of the code at the end of that process is the shape `isPermittedUndentation` is now: a long, deliberate cascade of cases, each carrying a spec citation, each backed by at least one corpus test that catches the specific phrase of F# the rule needs to permit.

## The Reality of Real-World Code

Reading the commit log against `isPermittedUndentation` is reading a list of small F# shapes that a strict reading of the spec would reject. Multi-line tuples whose elements undent. Function chains with leading pipes. `match` expressions in argument position. `fun` lambdas inside collection initialisers. Each commit is a single test file added to the corpus and a single arm added or extended in the function.

Cumulatively, they're the gap between the spec and what F# actually is. The spec is correct about the rules it covers, calibrated for a faithful pre-parse implementation. The corpus is calibrated for the language a person actually writes. Closing the gap is what `isPermittedUndentation` does, and the only way to know whether it's done is to run a few hundred real files through it and count diagnostics.

The deeper reason the gap exists is that the spec is a snapshot, frozen at the moment it was last revised. F# the language kept moving after that: the lexical-filtering rules accreted small changes, exceptions, and sugar that were never fed back to keep the spec accurate. The spec isn't wrong so much as out of date, and there's no single document that tracks the drift. So the corpus isn't just a richer source than the spec, for the parts of the language that changed under it, the corpus and the compiler are the *only* accurate sources. Building a correct parser today means iterating against the F# compiler and real code, and treating the spec as the excellent starting point it is rather than the final word it reads like.

The spec's offside rule is cheap in principle. Each specific case of it is an afternoon.

## Anchor commits / files

- `src/XParsec.FSharp/ParsingHelpers.fs`: `isPermittedUndentation`, `tryCollectionUndent`, `checkCollectionUndent`, `contextPermitsToken`, `contextPermitsTokenBounded`. The whole rule cascade lives here.
- `src/XParsec.FSharp/ExpressionParsing.fs`: `ElifBranches`, `pConditionThen`, the if-chain anchoring.
- `src/XParsec.FSharp/ParsingTypes.fs`: `findLineNumberImpl`, `getIndent`. The line-number binary search behind every column the offside check reads.
- `a86e642 Add permitted undentation infrastructure`: the first scaffolding for 15.1.10.
- `ebbb01e Handle offside exceptions and permitted undentations`: the first complete pass through 15.1.9 and 15.1.10.
- `1a19aff Refactoring isPermittedUndentation rule 15.1.10.4`: the SeqBlock+Paren walk lands.
- `cc5933a Add fun and function keywords as allowed undentations in fun and function bodies`: the recursive-fun extension in `findEnclosingIndent` (15.1.10.1); `a67b3cf Allow undentation rule 15.1.10.4 for fun and function keywords`: the same skip added to `checkCollectionUndent` (15.1.10.4).
- `0ee9fa9 Eliminate undentation rule 15.1.10.3 as dead code` and `d261f35 Eliminate undentation rule 15.1.10.2 as dead code`: the dead-code discoveries.
- `046d26e Fix indentation context of elif branches` and `2adcbb9 Fix anchoring offside column for if-elif-else if chains`: the `else if` / `elif` column reconciliation.
- `59ca412 Fix findLineNumberImpl`: the line-number binary-search bug, found while debugging the elif chain.
- `616b892`, `c5bb9cd`, `d0af4ca`, `930c99c`, `6ee954a`: the corpus chain. Each adds one test file and one arm or skip-list entry.

## Takeaway

Four ideas earned their place.

**The spec rules reflect the reference implementation's architecture.** §15.1.10.2 and §15.1.10.3 exist to undo a problem specific to FCS's design; a separate token-driven `LexFilter` that records the bracket's own column and then has to walk back out to the enclosing construct. A structure-driven parser, where the combinator that opens the paren is the one that tracks the context, never creates that problem. The `Indent = 0` choice from post 6 is what made the two rules dead code. Following the spec literally would have meant carrying two arms whose only purpose was to repair damage the project had decided not to do.

**One function, ordered cascade, one trace string per arm.** `isPermittedUndentation` is a flat `if`/`elif` chain in deliberate order: cheap exceptions first, structural rules last. Every branch tags its return with the spec section it implements. When something fails, `--trace` shows which arm fired or didn't, and that's the difference between a five-minute fix and an afternoon of guessing. Including string literals `"15.1.9 InfixUndent"` is cheap and unambiguous.

**Corpus extensions outnumber spec rules.** The shape `isPermittedUndentation` ended at is mostly the spec rules, and partly extensions: `Fun`/`Function` recursing through the skip list, `MatchClauses` for non-`|` tokens, `else if` collapsing to its leftmost keyword's column. Each extension came from a corpus test that didn't pass and refused to be coerced into one of the existing arms. The spec is a snapshot; the language kept moving and the whitespace rules drifted out from under it, so the compiler and real code are the only sources still in sync with what F# actually accepts.

**The `(` `SeqBlock :: Paren-like :: deeper` walk does most of the work.** When real F# code spans multiple lines inside parens or brackets, the offside line that bounds the body is rarely the head context. It's two or three frames down. `checkCollectionUndent` walks past the bookkeeping frames to find the line that actually matters. This single walk subsumes most of what 15.1.10.4 needed, and most of what the corpus discoveries needed too.

The offside check from post 4 reads one comparison; the stack from post 6 keeps the data; this function is the bridge that makes both of those work for real source. Three layers, each cheap on its own, expensive only when read as a single mechanism. And once it's right, the parsers above stop knowing about layout entirely, which is the whole point.
