---
category: Documentation
categoryindex: 0
index: 2
---

# Combinators: Building Complex Parsers

A "combinator" is a function that takes one or more parsers and returns a new, more powerful parser. They are the glue that allows you to combine the primitive parsers into sophisticated structures that can parse complex grammars.

The XParsec combinators are located in the `XParsec.Combinators` module, which is decorated with `[<AutoOpen>]`, so they are available automatically in your code.

This page is a reference for the most common and useful combinators.

## The `parser` Computation Expression

While operators are powerful, the most readable way to sequence multiple parsing steps is often with a `parser { ... }` computation expression. It allows you to write parsers in a familiar, sequential style.

```fsharp
open XParsec.CharParsers

let pJsonString = between (pchar '"') (pchar '"') (manyChars (pitem '\\' >>. pid <|> noneOf ['"']))
let pJsonNumber = pfloat |>> string

let pKeyValuePair = parser {
    let! key = pJsonString
    do! pchar ':'
    let! value = pJsonString <|> pJsonNumber
    return (key, value)
}
```

---

## Sequencing Combinators

These combinators and operators run parsers one after another, combining their results or discarding them as needed.

For the examples in this section, let's assume we have these simple parsers defined:

```fsharp
let pA = pitem 'a'
let pB = pitem 'b'
let pNum = pint32
```

| Operator / Function | Description | Example Usage & Result |
|---|---|---|
| `p1 >>= f` (bind) | Runs `p1`, then passes its result to function `f` which returns the next parser to run. | `pA >>= (fun c -> preturn (Char.ToUpper c))` on input "a" -> `Ok 'A'` |
| `p1 >>. p2` | Runs `p1`, discards its result, then runs `p2` and returns `p2`'s result. | `pA >>. pB` on "ab" -> `Ok 'b'` |
| `p1 .>> p2` | Runs `p1`, then runs `p2`, discards `p2`'s result, and returns `p1`'s result. | `pA .>> pB` on "ab" -> `Ok 'a'` |
| `p1 .>>. p2` | Runs both `p1` and `p2` and returns their results as a tuple. | `pA .>>. pB` on "ab" -> `Ok ('a', 'b')` |
| `p \|>> f` (map) | Runs `p` and applies function `f` to the successful result. | `pNum \|>> ((*) 2)` on "12" -> `Ok 24` |
| `between pOpen pClose p`| Runs `pOpen`, `p`, then `pClose`, returning only the result of `p`. | `between (pitem '(') (pitem ')') pNum` on "(42)" -> `Ok 42` |
| `tupleN p1 ... pN`| A convenient way to run `N` parsers and get an `N`-tuple of their results. | `tuple2 pA pB` on "ab" -> `Ok ('a', 'b')` |

---

## Choice and Option Combinators

These combinators allow you to try different parsing paths or handle optional parts of a grammar.

For these examples, we'll use:

```fsharp
let pA = pitem 'a'
let pB = pitem 'b'
let pFail = fail "this always fails"
```

| Operator / Function | Description | Example Usage & Result |
|---|---|---|
| `p1 <\|> p2` (choice) | Tries `p1`. If it fails *without consuming input*, it backtracks and tries `p2`. | `pA <\|> pB` on "b" -> `Ok 'b'` |
| `choice ps` | Tries a sequence of parsers `ps` and returns the result of the first one that succeeds. | `choice [pFail; pB; pA]` on "b" -> `Ok 'b'` |
| `opt p`| Tries parser `p`. Returns `ValueSome result` on success, or `ValueNone` on failure. **Never fails.** | `opt pA` on "b" -> `Ok ValueNone` |
| `optional p`| Tries parser `p`. Consumes input on success but returns `unit`. **Never fails.** | `optional pA` on "b" -> `Ok ()` |
| `p <\|>% x` | Tries `p`. If it fails, it succeeds with the default value `x` without consuming input. | `pA <\|>% 'z'` on "b" -> `Ok 'z'` |

---

## Repetition Combinators

These combinators apply a parser zero or more times to handle repeating patterns.

For these examples, we'll use:

```fsharp
let pA = pitem 'a'
let pComma = pitem ','
let pPlus = pchar '+' >>% (+) // Parses '+' and returns the addition function
```

| Function | Description | Example Usage & Result |
|---|---|---|
| `many p` | Applies `p` zero or more times, returning a list of results. Never fails. | `many pA` on "aaab" -> `Ok ['a';'a';'a']` |
| `many1 p`| Applies `p` *one* or more times. Fails if `p` doesn't succeed at least once. | `many1 pA` on "b" -> `Fails` |
| `skipMany p`| Like `many`, but discards the results (returns `unit`). | `skipMany pA` on "aaab" -> `Ok ()` |
| `skipMany1 p`| Like `many1`, but discards the results. | `skipMany1 pA` on "b" -> `Fails` |
| `sepBy p sep` | Parses zero or more occurrences of `p` separated by `sep`. | `sepBy pint32 pComma` on "1,2,3" -> `Ok [1;2;3]` |
| `sepBy1 p sep`| Parses one or more occurrences of `p` separated by `sep`. | `sepBy1 pint32 pComma` on "" -> `Fails` |
| `manyTill p end` | Applies `p` until the `end` parser succeeds. Returns the list of `p`'s results and the result of `end`.| `manyTill pid (pitem '!')` on "abc!" -> `Ok (['a';'b';'c'], '!')` |
| `chainl1 p op` | Parses one or more `p`, separated by `op`, and applies the operator left-associatively. | `chainl1 pint32 pPlus` on "1+2+3" -> `Ok 6` |
| `chainr1 p op` | Like `chainl1`, but applies the operator right-associatively. | `chainr1 ...` (useful for power operators) |

---

## Lookahead and Assertion Combinators

These combinators inspect the input stream without consuming it, or assert conditions about the parse.

For these examples, we'll use:

```fsharp
let pA = pitem 'a'
let pB = pitem 'b'
let pLet = pseq "let"
```

| Function | Description | Example Usage & Result |
|---|---|---|
| `lookAhead p` | Runs `p`, and if it succeeds, returns its result but **resets the stream position** to where it started. | `lookAhead pLet` on "let x" -> `Ok "let"`, next char is still 'l' |
| `notFollowedBy p`| Succeeds if `p` fails. Fails if `p` succeeds. **Never consumes input.** | `pA .>> notFollowedBy pB` on "ac" -> `Ok 'a'` |
| `notEmpty p` | Runs `p` and fails if `p` succeeded without consuming any input. Prevents infinite loops in `many`. | `many (notEmpty pA)` |
| `followedBy p`| Asserts that `p` would succeed, but doesn't consume input or return its result. | `followedBy pLet .>>. pIdentifier` |

---

## Error Customization Combinators

Use these label operators to provide more context-friendly error messages.

| Operator | Description |
|---|---|
| `p <?> msg` | If `p` fails *without consuming input*, replaces its error with the string `msg`. |
| `p <??> msg`| If `p` fails (even with consumption), wraps its original error inside a new nested error with `msg`. |

```fsharp
let pIdentifier =
    many1 (satisfy Char.IsLetter)
    <?> "Expected a valid identifier"

// Running on "123" gives "Expected a valid identifier"
// Running on "abc!" gives "Unexpected '!'..." because the base parser consumed "abc" first.
```
