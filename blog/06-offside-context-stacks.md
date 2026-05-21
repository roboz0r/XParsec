# 6. The offside rule, part 1: context stacks

The F# language specification devotes an [entire chapter](https://fsharp.github.io/fslang-spec/lexical-filtering/) (§15) to layout. Lightweight syntax, the mode essentially every F# programmer writes in, turns indentation and newlines into syntactic tokens. Implementing it isn't optional. It's also where parser combinator libraries traditionally give up.

The offside work is split across two layers. Post 4 covered the *check*: a single column comparison the lexical filter performs at every token fetch, against the head of `ParseState.Context`. This post is about the other half: the **stack itself**, who pushes and pops it, and the design realization that paren-like contexts sit on it as markers rather than as offside lines. Post 7 takes on the *exceptions*: the permitted undentations that let real F# look reasonable.

## What the spec asks for

§15.1 of the spec frames lightweight syntax as a transformation over a *pre-parse stack* of *offside contexts*. Each context carries an *offside line*, the column position established by some significant token, like the first non-whitespace token after `=`, or the column of `let` itself. The strictest line on the stack is the *offside limit*. A token at or before that limit is offside, and unless a *permitted undentation* applies, enclosing contexts are popped until the token is no longer offside. The popping is what produces the synthesised `$end`, `$in`, `$sep`, and `$done` tokens. Post 4 covered the closers and `$in`. Here, we are primarily concerned with `$sep`, the separator that turns same-indent lines into a sequence.

The spec describes this as a separate pre-parse pass that runs after lexing and before the syntactic grammar. F# Compiler Service implements it that way. XParsec.FSharp doesn't: post 4 collapsed the pre-parse work into the lazy filter that the parsers consume their tokens through.

The project's code calls it the *context stack* rather than the *pre-parse stack*. They're the same data structure; I'll use the project's name from here on, with the spec's terminology surfacing where the rule it cites is the point.

## The stack is a list of triples

`ParseState.Context` is an `Offside list`. Each entry is a record of three fields:

```fsharp
[<RequireQualifiedAccess>]
type OffsideContext =
    | Let
    | If
    | Match
    | MatchClauses
    | Fun
    | Paren
    | Brace
    | Begin
    // ... 21 more cases, listed in ParsingTypes.fs
    | SeqBlock

type Offside =
    {
        Context: OffsideContext
        Indent: int
        Token: PositionedToken
    }
```

`Context` is one of thirty `OffsideContext` cases, named after the F# construct that pushes them. `Indent` is the column the offside line sits at. `Token` is the source token that triggered the push, kept around so diagnostics can point at it ("the `let` on line 12 is still open here") without the filter having to track a parallel array.

Most cases are real offside contexts: each pushes with an `Indent` calculated from a real source position, and the lexical filter compares incoming token columns against it. Six are *paren-like* (`Paren`, `Bracket`, `BracketBar`, `BraceBar`, `Brace`, `Begin`) and go on the stack with `Indent = 0`, which the filter recognises as a marker rather than as an offside line. A seventh delimiter context, `Quote` (for `<@ @>` and `<@@ @@>`), also goes on the stack with `Indent = 0`, but it sits *outside* the `isParenLike` family the next section describes, so it gets the closing-delimiter exemption without the collection-undentation one. Two cases, `Struct` and `Sig`, are vestigial: they were defined alongside `Begin` for symmetry early on, but no parser pushes them today; `struct ... end` and `sig ... end` are handled at a different layer.

The head of the stack is the innermost open context. The lexical filter reads the head on every token fetch, though a few parsers peek at it too (record-field separators, the `|`-bounding in match clauses). The bulk of this post is about how the parsers *maintain* it.

## `withContext`, the bracketing helper

The cleanest way to push a context is to wrap the parser whose scope it should be open for. `withContext` does exactly that:

```fsharp
let withContext (ctx: OffsideContext) innerParser
                (reader: Reader<PositionedToken, ParseState, _, _>) =
    let savedState = reader.State

    match peekNextSyntaxToken reader with
    | Error e -> Error e
    | Ok peekTok ->
        let indent =
            match peekTok.Index with
            | TokenIndex.Regular iT -> ParseState.getIndent reader.State iT
            | TokenIndex.Virtual -> 0

        let entry =
            { Context = ctx; Indent = indent; Token = peekTok.PositionedToken }

        reader.State <- ParseState.pushOffside entry reader.State

        match innerParser reader with
        | Ok result ->
            reader.State <- ParseState.popOffside entry reader.State
            Ok result
        | Error _ as e ->
            reader.State <- savedState
            e
```

The peek picks the first token the inner parser will see. The column of that token becomes the offside line. For almost every body in F#'s grammar, that's what the spec actually wants: a `let` binding's body, a `match` expression's matched expression, a `then` branch's body, and so on, all establish their offside line at the column where the body's first token actually starts.

On success, the entry is popped and the result returned. On failure, the *entire* saved state is restored, the whole `ParseState` record. That includes, the diagnostics list, the split flags from post 4, and any context frames the inner parser may have pushed and not popped. Backtracking through this helper is one assignment: `reader.State <- savedState`, which dovetails with the "one position is the whole truth" invariant from post 4. Combinator backtracking and offside backtracking share the same wire. The thing we don't backtrack is the `Index`. A tiny optimisation to avoid repeeking through trivia, and, a potential source of bugs if the inner parser had proceeded past a significant token, though it never came up in practice.

`withContextAt` is the same helper without the peek:

```fsharp
let withContextAt (ctx: OffsideContext) (indent: int) (token: PositionedToken)
                  innerParser (reader: Reader<_, ParseState, _, _>) =
    let savedState = reader.State
    let entry = { Context = ctx; Indent = indent; Token = token }
    reader.State <- ParseState.pushOffside entry reader.State
    match innerParser reader with
    | Ok result ->
        reader.State <- ParseState.popOffside entry reader.State
        Ok result
    | Error _ as e ->
        reader.State <- savedState
        e
```

The two are split because the spec sometimes pins the offside line to a token that has *already been consumed*. §15.1.7's `Let` context, for instance, takes the column of the `let` keyword itself, not the column of the binding's RHS (a subtle distinction that matters for `let` bindings where the `=` and the body sit on a different line). `withContextAt` is also how `Fun`, `Function`, `If`, and `Then` contexts are pushed: the keyword has been consumed, its column is in hand, and the inner parser hasn't started yet. (`Match` and `Try` look like they belong on this list, but their context has to outlive a single inner parser, so they use neither helper.)

## Paren-like contexts are markers, not lines

The first draft of this code had paren-like contexts behaving like everything else. A `(` would push `Paren` at the column of the `(` itself, `[` would push `Bracket` at its column, and so on. The offside rule would then prevent tokens from drifting left of the open delimiter, which sounds reasonable until you start running real code through it.

The spec's §15.1.10 enumerates *permitted undentations*, which post 7 deals with in detail, says that constructs enclosed in brackets may be undented arbitrarily. The reason is mechanical: paren-like delimiters establish their *own* scope, closed by the matching delimiter, not by indentation. There's no risk of accidentally continuing a `(` past where the user meant, the `)` is unambiguous.

Implementing 15.1.10.4 the obvious way means: every time a token is offside relative to a paren-like context, *also* check whether the offending context is a paren-like one, and if so, exempt it. Doable, but it pushes the exemption into every offside check. The simpler shape, which is what the code settled on, is to push paren-like contexts with `Indent = 0`:

```fsharp
// In pEnclosed, ParsingHelpers.fs
let entry: Offside =
    {
        Context = offsideCtx
        Indent = 0 // Paren-like contexts use indent 0;
                   // undentation rules inspect them as stack markers
        Token = l.PositionedToken
    }
```

A column of zero is unreachable from any real token, so a paren-like frame never *is* the offside limit. It still sits on the stack, and so do constructs like `MatchClauses` deciding whether an `|` token is meant for them or for an outer `Match`. The frame is a marker for "we are inside brackets," not an offside line of its own.

The predicate that recognises them lives once, in `isParenLike`:

```fsharp
let private isParenLike (ctx: OffsideContext) =
    match ctx with
    | OffsideContext.Paren
    | OffsideContext.Bracket
    | OffsideContext.BracketBar
    | OffsideContext.BraceBar
    | OffsideContext.Brace
    | OffsideContext.Begin -> true
    | _ -> false
```

The filter's offside check reads `head.Indent` and compares it to the incoming token's column; for any paren-like frame on the head, the comparison passes trivially.

## When `withContext` isn't enough

The intent was clean: `withContext` (and its `withContextAt` sibling) would be the *only* way a context reached the stack, every push matched to a pop by construction, and it does cover most of the parsers in `ExpressionParsing.fs` and `PatternParsing.fs`. But the helper bakes in the assumption that a context's scope is exactly one inner parser, and several constructs break it. Where they do, the parser pushes and pops `ParseState.Context` by hand. There turned out to be seven such sites: six in two families, plus one at the file level.

**Delimiter-bracketed constructs.** `pEnclosed` (parens, lists, arrays, `begin ... end`, quotations) and `pBracedExpr` (braces and `{| ... |}` anonymous records) push by hand, as do their pattern-side twins `pRecordPat` and `pNamedFieldPats`. Here's the relevant slice of `pEnclosed`:

```fsharp
fun reader ->
    match pLeft reader with
    | Error e -> Error e
    | Ok l ->
        let savedState = reader.State

        let entry: Offside =
            { Context = offsideCtx; Indent = 0; Token = l.PositionedToken }

        reader.State <- ParseState.pushOffside entry reader.State

        let inline popAndReturn result =
            reader.State <- ParseState.popOffside entry reader.State
            result

        match peekNextSyntaxToken reader with
        | Error e -> reader.State <- savedState; Error e
        | Ok t when t.Token = expectedRightTok ->
            // Fast path: empty block
            match consumePeeked t reader with
            | Ok r -> popAndReturn (Ok(completeEmpty (parenKindConstructor l) r))
            | Error e -> reader.State <- savedState; Error e
        | _ ->
            // Normal path with recovery
            let innerParser = recoverWith StoppingTokens.afterParen ... pInner
            match innerParser reader with
            | Ok result -> popAndReturn (Ok result)
            | Error _ -> reader.State <- savedState; ...
```

Three things force the manual shape. First, the context has to be on the stack *before* the first peek inside the brackets, because the collection-undentation rule (§15.1.10.4) inspects the stack as soon as the inner parser asks for its next token. With `withContext`, the push happens around a single inner parser; here, the inner parser is a peek-then-dispatch with multiple branches, and the push has to bracket all of them.

Second, the recovery branch needs the context active while the recovery walker scans for stopping tokens. `recoverWith StoppingTokens.afterParen` is what stops scanning at the matching close delimiter; if the paren-like frame weren't on the stack, the stopping logic for paren-like recovery wouldn't have anything to anchor to.

Third, the empty-block fast path returns at a different point than the normal path. A `withContext` wrapper would mean either popping early (and re-pushing for the slow path) or popping late (and walking through the normal path's tail unnecessarily). The `popAndReturn` helper is the cheapest split.

`pBracedExpr` is the same shape for `{`-introduced expressions. It sits behind `pRecordOrObjectExpr`, which dispatches via `choiceL` over several mutually exclusive shapes: a computation-expression body, an object expression, a record literal/clone, and a bare `{ expr }` fallback (anonymous records `{| ... |}` are a *separate* `pBracedExpr` call with a `BraceBar` context). Any of them can fail and force the next to be tried, and the `Brace` frame has to remain on the stack across all of them. A `choiceL` inside a `withContext` would push and pop on every attempted alternative; a single manual push around the whole `choiceL` is the right shape.

**Keyword constructs whose context outlives one parser.** `match` and `try` are the other family, and the more interesting break. A `Match` context stays open across the matched expression, *then* the `with`, *then* the rules, because it's what lets `with` and `|` align back at the `match` column. So `parseMatchBody` pushes `Match` by hand and pops it only once the rules are in:

```fsharp
// In parseMatchBody, ExpressionParsing.fs
reader.State <- ParseState.pushOffside matchEntry reader.State
// ...matched expr (its own SeqBlock), then `with`, then the rules...
reader.State <- ParseState.popOffside matchEntry reader.State
```

Same again with `try`. The `Try` context spans the body, the `with`/`finally`, and the handler rules, managing the context manually "similar to how `parseMatchBody` keeps Match context active." The seventh push is the odd one out, at the very top of the file: the top-level parser seeds a `SeqBlock` so the undentation rules always find a base offside line at the first non-trivia token of the file.

After rereading and documenting all these, there's certainly some opportinities for refactoring, maybe I could even get back to just the `withContext`/`withContextAt` pair, but this is how it's built and what works today.

## The third outcome: same indent is a new item

Post 4 framed the offside rule as deciding one of three things about each token: it continues the current construct, starts a nested one, or is rejected. The stack drives all three off the single number at its head. *Rejected* is the filter's offside check (post 4): a token strictly left of `head.Indent` fails, unless post 7 rescues it. *Nested* is a push. The third case is a token sitting *exactly* on `head.Indent`, the next item in the current block.

F# marks that boundary with nothing but a newline. The grammar requires a separator (the same `;` you'd write in `[1; 2; 3]`) so when an expression-starting token lands on the offside line, the parser invents one. `pSepVirt`:

```fsharp
let pSepVirt =
    parser {
        match! peekNextSyntaxToken with
        | t when t.Token = Token.OpSemicolon -> return! consumePeeked t   // a real ';'
        | t ->
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

It's the same shape as `pLetOrUseIn`: peek, take a real token if it's there, otherwise synthesise a virtual one at the same `StartIndex` with `IsVirtual` set. The `VirtualSep (= IsVirtual ||| OpSemicolon)` reaches the parser as an ordinary `;`, and the `IsVirtual` bit still lets a formatter tell it from one the user typed.

The sharp edge is the `canStartExpression` guard. A `VirtualSep` is *zero-width*: it carries a position but consumes no input. Drop one in front of a token that can't begin a new item (a closing `)`, a `with`/`then`/`else` that belongs to an enclosing construct, a bare infix operator) and the sequence parser reads "separator," looks for the next element, finds the same non-starting token, synthesises another separator, and promptly throws `InfiniteLoopException` or overflows the stack; the zero-width token never advances the reader. The guard bounds it: a separator is only inserted when there is genuinely a next item to separate. The source comment is blunt about the infinite loop it prevents.

So the same `head.Indent` answers three questions by comparison alone. Left of it: reject. On it: separate. Right of it: the token continues the current item (a function argument, the tail of an infix expression). Three outcomes for a token is the whole reason an F# block needs no braces and no `in`. The offside line *is* the delimiter; a virtual separator dropped on it is the punctuation the source leaves out.

Two narrower cousins read the same head the same way: `pRecordFieldSep` (through `makeVirtualSep`) separates record fields written on their own lines, and `pSepVirtPat` does it for the patterns of a match or a destructuring.

## Walking through `let x = match y with`

A short trace shows the stack at work. Take the four-line shape:

```fsharp
let x =
    match y with
    | A -> 1
    | B -> 2
```

`let` sits at column 0; `match` and `|` at column 4; `y` at column 10; the rule bodies at column 11. The stack at four checkpoints:

**(1) After `let` is consumed, before `=`.** `withContextAt OffsideContext.Let 0` runs around the binding parser. Stack: `[Let(0)]`.

**(2) Inside the matched expression `y`, deepest point.** Once `=` has been consumed, `withContext OffsideContext.SeqBlock` peeks `match` at column 4 and pushes `SeqBlock(4)`. `parseMatchBody` then pushes `Match(4)` manually around the rest of the match, and `withContextAt OffsideContext.SeqBlock 5` runs around the matched expression itself. Stack: `[Let(0), SeqBlock(4), Match(4), SeqBlock(5)]`.

**(3) After `with`, ready to read rules.** The inner `SeqBlock(5)` has popped on its way out. `pMatchRules` is now running `withContext OffsideContext.MatchClauses`, which peeks the `|` at column 4 and pushes `MatchClauses(4)`. Inside each rule, a `SeqBlock` for the rule body comes and goes. Stack at the deepest point inside a rule body: `[Let(0), SeqBlock(4), Match(4), MatchClauses(4), SeqBlock(11)]`.

**(4) After the binding completes.** Every push has matched a pop in reverse order. Stack: `[]`.

Two observations. First, the stack is small, four to five entries at the deepest point of a real F# expression. The lexical filter's offside check reads only the head; the size is bounded by syntactic nesting, not by file size. Second, several entries can share a column. `SeqBlock(4)`, `Match(4)`, and `MatchClauses(4)` all sit at column 4 because the whole match expression and its rule patterns happen to align there. Each plays a different role in the permitted-undentation rules of post 7: `Match(4)` is what lets `with` and `|` align with `match`; `MatchClauses(4)` is what bounds outer `|`s from being consumed by inner matches. They're separate frames precisely because they govern separate questions.

## A body to the left of its `then`

The walkthrough above is well-behaved with every body sittin to the right of the keyword that opens it. F# doesn't require that, and the stack is what makes this alternative legal. The following is perfectly valid F# even if your coworkers would shoot you for writing it:

```fsharp
let thenIndented x =
    if x > 0
       then
     x            // Depends on `if` indent + 1 (NOT `then` indent + 1)
       else
     -x           // Depends on `if` indent + 1 (NOT `else` indent + 1)
```

`if` is at column 4. `then` and `else` are pushed out to column 7. And both bodies sit at column 5, *to the left of the keywords that introduce them*. It reads like a misindentation but the offside discipline in this post is exactly why it works.

The load-bearing number is the body's offside line: `if_col + 1`, which is 5, not `then_col + 1 = 8`. Both bodies must clear column 5, and the then-branch shows the mechanism most directly. Recall from the `withContextAt` section that `If` and `Then` are pushed with the peek-*less* helper, anchored to the already-consumed `if` keyword:

```fsharp
// In pIfExpr, ExpressionParsing.fs `indent` is the column of `if`
withContextAt OffsideContext.If   (indent + 1) ifTok.PositionedToken refExpr.Parser
// ... `then` is consumed here ...
withContextAt OffsideContext.Then (indent + 1) ifTok.PositionedToken refTypedSeqExprBlock.Parser
```

Both the condition and the then-body get an offside line of `if_col + 1` (5), anchored to `ifTok` (the `if` token, captured when it was consumed). The `then` keyword's own column never enters the calculation. So when the lexical filter checks the body token `x` at column 5 against the head of the stack, `Then(5)`, it passes: `x` is not left of 5, so it's the first token of the then-body. `then` at column 7 is irrelevant to the body: its column was checked against a *different* rule, the permitted undentation that lets `then`/`elif`/`else` align back at `if_col` (post 7).

This is the entire reason `withContextAt` exists as a separate helper. Had `Then` been pushed with plain `withContext`, the offside line would have been the column of the first token the body parser *peeked* (column 5 here by luck) but the wrong answer the moment a body's first token undents below where the construct began. The source comment on the condition path is blunt about it:

```fsharp
// Anchoring at +1 (rather than the peeked expression token's column, which
// is what plain withContext would use) is needed so multi-line conditions
// that undent onto the following line remain within the If context.
```

The condition side of the same test file makes that concrete:

```fsharp
let condOnNextLine =
    if
     true // Depends on `if` indent + 1
    then 1
    else 2
```

`true` sits aligned with `if`, on the line below it, and stays inside the `If` context because that context's offside line for the `if` condition and body is different to the expression keywords: `then`, `else`, `elif`. A `withContext` push would have had nothing to anchor to until it peeked `true`, by which point "what column did this construct start at" is no longer answerable. The offside line, here as everywhere in this post, is a property of where the construct *began*, not of where any one delimiter happens to sit. The parser code itself reads straightforwardly (especially if you ignore the error recovery):

```fsharp
let pIfExpr =
    parser {
        let! (ifTok, indent) = assertKeywordToken Token.KWIf
        // Condition at if_col + 1
        let! cond =
            recoverExprMissing (
                withContextAt OffsideContext.If (indent + 1) ifTok.PositionedToken refExpr.Parser
            )
        // then permitted undentation at if_col via contextPermitsToken
        let! thenTok = recoverWithVirtualToken Token.KWThen "Expected 'then' after condition" pThen
        // Body anchored to if_col + 1, NOT then_col + 1
        let! thenExpr =
            recoverExprMissing (
                withContextAt OffsideContext.Then (indent + 1) ifTok.PositionedToken refTypedSeqExprBlock.Parser
            )

        let! elifs, elseBranch = ElifBranches.parse

        return ExprAux.ForExpr(Expr.IfThenElse(ifTok, cond, thenTok, thenExpr, elifs, elseBranch))
    }
```

What the stack discipline laid out here doesn't yet handle is the case where a token *would* be offside but mustn't be rejected. `elif` after a deeply indented `then` branch. A multi-line `fun x -> ...` whose body sits under the `fun`. A trailing `)` on its own line. Each of those is a permitted undentation, and that's the harder half of the problem. Post 7 is where it lands.

## Anchor commits / files

- `src/XParsec.FSharp/ParsingTypes.fs` — `OffsideContext`, `Offside`, the `ParseState.Context` field.
- `src/XParsec.FSharp/ParsingHelpers.fs` — `withContext`, `withContextAt`, `isParenLike`, `pEnclosed`, `pRecordFieldSep`, `makeVirtualSep`. The whole stack discipline lives here.
- `src/XParsec.FSharp/ExpressionParsing.fs` — `pBracedExpr`, `pRecordOrObjectExpr`, `pSepVirt`, `pIfExpr`/`pConditionThen` (the `withContextAt` anchoring for `if`/`then` shown above), the larger expression parsers that drive most of the push/pop traffic.
- `test/XParsec.FSharp.Tests/data/233_if_min_indent.fs` — the golden test the `thenIndented` and `condOnNextLine` examples are taken from, alongside the rest of the `if`/`then`/`else` minimum-indentation cases.
- `a4237ab WIP handling #if directives` — where the `OffsideContext` DU first lands (bundled, confusingly, with conditional-compilation work).
- `4b63566 Add context-aware hook to parsing helpers` — the first context-aware *machinery*: `withContext` and the filter's offside check, built on the already-existing type.
- `ca3e05f Preliminary offside context parsing` — a few more DU cases, and the first parsers start pushing contexts.
- `a86e642 Add permitted undentation infrastructure` — the supporting machinery that made `Indent = 0` markers viable.
- `ebbb01e Handle offside exceptions and permitted undentations` — the pass that moved paren-like contexts onto marker semantics.
- `1a19aff Refactoring isPermittedUndentation rule 15.1.10.4` — where `isParenLike` is factored out into the predicate quoted above.

## Takeaway

Five ideas hold this layer up.

**A list of `(kind, indent, token)` triples is the entire data structure.** No specialised tree, no parallel index, no separate stack per concern. The parsers maintain the list; the lexical filter reads the head on every token fetch (with a few parsers peeking at it too), and that head-read is the (almost) whole interface between layers.

**`withContext` is the bracketing helper, and backtracking is one assignment.** Push-then-run-then-pop on success, restore-the-whole-state on failure. By keeping `ParseState` immutable `reader.State <- savedState` unwinds any pushes the inner parser left behind.

**Paren-like contexts sit on the stack as markers.** `Indent = 0` makes them invisible to the offside check while keeping them visible to the rules in post 7 that need to see "we're inside brackets." A single design choice replaced what would otherwise have been a special case in every offside-check call site.

**Manual push/pop is the principled exception and there are more exceptions than the clean design wanted.** Seven sites push by hand, in two families: delimiter-bracketed constructs (`pEnclosed`, `pBracedExpr`, and the pattern twins) whose bodies have multiple exit points and recovery scans that need the context active; and keyword constructs (`match`, `try`) whose context must outlive a single inner parser to govern the alignment of `with`, `|`, and `finally`. `withContext` assumes a context's scope is exactly one parser; each manual site is a place that assumption doesn't hold.

**The offside line answers three questions, not one.** A token left of `head.Indent` is rejected (or undented, per post 7); one exactly on it starts a new item, punctuated by a virtual `;`; one right of it continues the current expression. The same number the parsers push and pop drives all three.

The stack itself is almost the entire offside machinery. The hard part, what's allowed to violate the rule, and why, is the next post.
