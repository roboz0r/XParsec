---
category: Documentation
categoryindex: 0
index: 4
---

# Character Parsers

The `XParsec.CharParsers` module provides a rich set of parsers specifically optimized for working with `char` and `string` inputs. These parsers are the go-to tools for most text-based parsing tasks.

## How to Read the Examples

The examples below show how to call each parser. To see what they do, you can run them with a simple test function like this:

```fsharp
open XParsec
open XParsec.CharParsers

// A helper to run a parser and print its result
let testParser parser input =
    match parser (Reader.ofString input ()) with
    | Ok success -> printfn $"Input: \"{input}\" -> Ok {success.Parsed}"
    | Error err ->
        let errorMsg = ErrorFormatting.formatStringError input err
        printfn $"Input: \"{input}\" -> Error:\n{errorMsg}"

// Example usage:
testParser (pchar 'a') "abc"
// Prints: Input: "abc" -> Ok a
```

---

## Simple Character Parsers

These parsers consume and match single characters based on various criteria.

| Parser | Description | Example Usage |
|---|---|---|
| `pchar c` | Parses the specific character `c`. | `pchar 'a'` |
| `skipChar c` | Parses and discards the specific character `c`. | `skipChar ':'` |
| `anyChar` | Parses any single character. | `anyChar` |
| `skipAnyChar` | Parses and discards any single character. | `skipAnyChar` |
| `satisfy p` | Parses any character that satisfies predicate `p`. | `satisfy Char.IsUpper` |
| `digit` | Parses any ASCII digit ('0'-'9'). | `digit` |
| `asciiLetter` | Parses any ASCII letter ('a'-'z', 'A'-'Z'). | `asciiLetter` |
| `anyOf cs` | Parses any character from the sequence `cs`. | `anyOf ['a'; 'e'; 'i'; 'o'; 'u']`|
| `noneOf cs` | Parses any character *not* in the sequence `cs`. | `noneOf "\r\n"` |

---

## String Parsers

These parsers match sequences of characters.

| Parser | Description | Example Usage |
|---|---|---|
| `pstring s`| Parses the specific string `s`. | `pstring "let"` |
| `stringReturn s x` | Parses string `s`, but returns value `x`. | `stringReturn "true" true` |
| `stringCIReturn s x` | Case-insensitive version of `stringReturn`. | `stringCIReturn "true" true` |
| `manyChars p`| Applies char parser `p` zero or more times, returning the result as a string. | `manyChars digit` |
| `many1Chars p`| Applies char parser `p` *one* or more times, returning the result as a string. | `many1Chars (satisfy Char.IsLetter)` |
| `manyCharsTill p end` | Applies `p` until `end` succeeds. Returns the matched string and the result of `end`. | `manyCharsTill anyChar (pchar '"')` |

---

### Multi-String Parsers

These parsers are optimized to match **one of many** strings, such as a list of keywords.

**Key Features:**

1. **Greedy Matching:** They always attempt to match the longest candidate first. For example, if your candidates are `["=", "=="]`, matching against `==` will correctly consume both characters.
2. **Performance:** Much faster than using `choice` or `<|>` over a list of strings.
3. **Canonical Returns:** The `CI` (Case Insensitive) variants return the string *as defined in your list*, effectively normalizing the input case.

| Parser | Description | Example Usage |
|---|---|---|
| `anyString xs` | Matches any string in `xs`. Returns the matched string. | `anyString ["yes"; "no"]` |
| `anyStringCI xs` | Case-insensitive match. Returns the string from `xs` (canonical form). | `anyStringCI ["Select"; "From"]` |
| `anyStringReturn xs` | Matches a string key, returns the associated value. | `anyStringReturn ["+", Add; "-", Sub]` |
| `anyStringCIReturn xs` | Case-insensitive version of `anyStringReturn`. | `anyStringCIReturn ["true", true]` |
| `anyStringBy comp xs` | Matches any string in `xs` using the provided `StringComparison`. Returns the matched string. | `anyStringBy StringComparison.CurrentCulture ["yes"; "no"]` |
| `anyStringByReturn comp xs` | Matches any string in `xs` using the provided `StringComparison`. Returns the associated value. | `anyStringByReturn StringComparison.CurrentCulture ["yes", true; "no", false]` |

> **Note:** All parsers above have an **`L`** variant (e.g., `anyStringL`, `anyStringReturnL`) which accepts a `message: string`. These are highly recommended for production parsers as they avoid allocating detailed error lists on failure.
>
> ```fsharp
> // Efficient usage with a custom label
> let operator = anyStringL ["+"; "-"; "*"; "/"] "arithmetic operator"
> ```

---

## Whitespace and Newline Parsers

These are common helpers for handling whitespace and line endings.

| Parser | Description | Example Usage |
|---|---|---|
| `spaces` | Skips zero or more whitespace characters (`' '`, `\t`, `\r`, `\n`). Never fails. | `spaces` |
| `spaces1` | Skips *one* or more whitespace characters. Fails if no whitespace is present. | `spaces1` |
| `newline` | Parses a newline ("\n", "\r", or "\r\n"), returning `\n`. | `newline` |
| `skipNewline`| Parses a newline and returns `unit`. | `skipNewline` |

---

## Numeric Parsers

These powerful parsers can parse various numeric formats from a string. They automatically handle signs (`+`/`-`) and different bases.

- **Decimal**: `123`, `-45`
- **Hexadecimal**: `0x7B`, `0xFF`
- **Octal**: `0o173`
- **Binary**: `0b1111011`

| Parser | Description | Example Usage |
|---|---|---|
| `pint16` | Parses a 16-bit signed integer. | `pint16` on `"0xFF"` -> `Ok 255s` |
| `pint32` | Parses a 32-bit signed integer. | `pint32` on `"-123"` -> `Ok -123` |
| `pint64` | Parses a 64-bit signed integer. | `pint64` |
| `puint16` | Parses a 16-bit unsigned integer. | `puint16` |
| `puint32` | Parses a 32-bit unsigned integer. | `puint32` |
| `puint64` | Parses a 64-bit unsigned integer. | `puint64` |
| `pbigint` | Parses an arbitrarily large integer (`System.Numerics.BigInteger`). | `pbigint` |
| `pfloat` | Parses a double-precision floating-point number. Also handles "NaN", "Infinity", and hex floats (e.g., `0x1.921fb54442d18p+1`). | `pfloat` on `"1.2e-3"` -> `Ok 0.0012`|
