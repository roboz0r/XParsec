# 4. Pratt parsing for F#'s operator zoo

## Hook

F# lets you define operators. Lots of them. They have precedence defined by their leading characters (`**>` is high, `||>` is low), they can be prefix-only, infix, or both depending on spelling and whitespace, and some "operators" (`=`, `<-`, `:`, `as`, `when`, `|`) aren't what a textbook would call operators at all. A `choice` of hand-listed cases doesn't scale. Pratt parsing does — but only if the framework is honest about what an "operator" is.

This is also the post where the F# work starts paying rent back to XParsec itself. Work on XParsec.FSharp began against XParsec 0.2. The first serious grammar feature that didn't fit — variadic, mixed-arity, context-sensitive operator shapes — forced a redesign of the operator parser that eventually shipped as XParsec 0.3. The F# branch was rebased onto the redesigned API once it stabilized.

## What this post covers

- **Why Pratt parsing for F#.** Left/right binding power matches F#'s associativity rules almost one-to-one. Expressions and patterns both fit. Even `as` in patterns and `when` in match clauses can be modeled as operators with LHS-only binding power — they're fundamentally *infixes that take a constrained RHS*.
- **The 0.2 starting point, and what broke it.** XParsec 0.2's operator parser assumed operators were uniformly binary (or unary prefix), with a single `BindingPower` per token. F# immediately violated this on multiple axes: operator tokens are context-sensitive (`|` is a pattern separator, a match-clause bar, or a bitwise-or depending on where it appears); operators can be n-ary (sequence expressions separated by `;`); and some "operators" need entirely different completion logic (`?` followed by an identifier vs. `?` as part of `?.`). Document the concrete grammar cases that showed the limits.
- **The redesign that became 0.3.** `85fa6a7 Rework operator parsing`, `4e8455f Make Operators an interface`, `66b6f45 Add RHSOperator.InfixNary`, `62c8785 Add InfixMapped and LHSTernary operators`, `80cd647 More detailed errors in operator parsing`, `2348ae7 Add custom Operators.OpComparer`, `e060d8b Expose BindingPower in OperatorParsing`. Each of these commits exists because a specific F# construct wouldn't parse under 0.2's model. Walk through `OperatorParsing.fs` as it stands today and point at which piece of F#'s grammar forced each addition.
- **The pluggable `Operators` interface.** The 0.2 API baked operator lookup into a concrete data structure. F# needs to consult parse state to decide what an operator *means* (post 3's filter layer for `|`, the string-check pattern for `>`). The interface abstraction in `4e8455f` let XParsec.FSharp plug in its own context-sensitive lookup without forking the library.
- **Prefix-only vs optionally-prefix.** `!`, `~`, `%`, `&` are prefix-only. `+` and `-` are *optionally* prefix — still infix in `f + g`. The parser has to know the difference, and the `pApplication` code's `isAtomicExprToken` guard is where that shows up. The `ms-fsharp-progress.md` dump captures the exact fix (`handleToken` rejection + `pApplication` recognition).
- **LHS-only precedence levels.** `Prefix`, `Function`, `If`, `Let`, `As`, `When`, `PatternMatchBar`, `Parens` all have no legitimate RHS handler. The Pratt loop used to dispatch them anyway and hit `invalidOp`. Show the guard added in `handleToken` that fails cleanly instead.
- **Rollback protection in InfixNary.** Commit `278ca3a Fix rollback and infinite loop protection in InfixNary operator parsing`. A tiny but representative bug where a nullary-accepting operator could loop. This is the kind of thing that only shows up after the corpus test starts finding novel inputs.
- **Point-free operators in parens.** `(+)`, `( * )`, and `(*)` — yes, `(*)` — are all valid function identifiers. Commit `500619e Parse point-free operators in parens`. The lexer fix for `(*)` (post 2) only made this parse correctly.
- **A preview of the second rewrite.** The 0.3 redesign covered *features*. It did not cover stack consumption. The Pratt loop still recursed along the left spine, so a 500-operator expression opened 500 stack frames — and stack overflow debugging is what post 11 is about. That post describes the second, stack-bounded rewrite; the completion of the operator-parsing story sits across the two.

## Anchor commits / files

- `src/XParsec.FSharp/ExpressionParsing.fs` (2,605 lines)
- `278ca3a`, `09d9d40 Fix handling of adjacent prefix operators in application`
- `500619e`, `adb41f3 Parse parenthesized operators in active patterns`

## Takeaway

Pratt parsing handles "what precedence?" beautifully and "is this even an operator here?" not at all. The second question is what took the most work.
