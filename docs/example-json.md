# Worked Example: A Complete JSON Parser

One of the best ways to see the power of a parser combinator library is to build a parser for a well-known format. This guide will walk you through the creation of a complete, robust JSON parser using XParsec.

Our goal is to parse a JSON string into a structured F# object model. This parser was written following the official [JSON specification](https://www.json.org/json-en.html). The inclusion of the "railroad diagrams" from the spec shows how XParsec's approach allows you to write a parser that is a near-direct transliteration of a formal grammar.

## 1. Defining the F# Model (AST)

First, we need to define the F# types that will represent a parsed JSON document. This is our Abstract Syntax Tree (AST). We'll use a recursive discriminated union for `JsonValue` and `ImmutableArray` for collections to ensure our model is efficient and immutable.

Note: JSON types are mutually recursive so we used `type ... and ...` to specify the type definitions.

```fsharp
open System.Collections.Immutable

[<RequireQualifiedAccess>]
type JsonValue =
    | String of string
    | Number of float
    | Object of JsonObject
    | Array of JsonArray
    | True
    | False
    | Null
and JsonMember = { Name: string; Value: JsonValue }
and JsonObject = ImmutableArray<JsonMember>
and JsonArray = ImmutableArray<JsonValue>
```

## 2. Structuring the Parsers: The `static let` Pattern

We want our parser to be able to parse any kind of character input, not just `string`s. While XParsec parsers are functions and can be generic, defining them as top-level `let`-bound values runs into F#'s "value restriction," which prevents simple values from being automatically generalized.

A clean solution is to define our parsers as `static let` members inside a generic type. This allows the parsers to be implicitly parameterized by the type's generics, enabling both reusability and mutual recursion without issue. At the point of use, a concrete version of the `JsonParsers` type will be created based on the input `Reader`.

For a deeper dive, see Microsoft's documentation on [Value Restriction](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/generics/automatic-generalization#value-restriction) and [Finer Points of F# Value Restriction](https://learn.microsoft.com/en-us/archive/blogs/mulambda/finer-points-of-f-value-restriction).

**Caution:**

It's tempting to define a parser like `let pThing reader = parser { ... } reader`. But this is a performance trap. By giving `pThing` an explicit reader parameter it causes the `parser { }` builder to be re-evaluated each time the parser is run.

```fsharp
open XParsec
open XParsec.CharParsers

// Generic parameters must match the contrains of the XParsec.Reader type
type JsonParsers<'Input, 'InputSlice
    when 'Input :> IReadable<char, 'InputSlice> and 'InputSlice :> IReadable<char, 'InputSlice>> =

    // Parsers will be defined here as static let bindings...
    
    // We can expose the final parser as a static member
    static member Parser = pJson
```

This structure forms the backbone of our JSON parser. All subsequent parsers will be defined inside this `JsonParsers` type.

## 3. Parsing Primitives

### Whitespace

> Whitespace can be inserted between any pair of tokens.

![JSON Whitespace](./images/json-whitespace.png)

The diagram shows that whitespace is zero or more space, tab, or newline characters. The `skipMany` combinator is a perfect fit.

```fsharp
static let pWhitespace = skipMany (anyOf [ ' '; '\t'; '\n'; '\r' ])
```

### String

> A string is a sequence of zero or more Unicode characters, wrapped in double quotes, using backslash escapes. A character is represented as a single character string. A string is very much like a C or Java string.

![JSON String](./images/json-string.png)

A string is enclosed in quotes, containing a sequence of characters. These characters can be normal (`pOtherChar`) or escaped (`pEscape`).

```fsharp
static let pString =
    parser {
        let! _ = pitem '"'
        let! chars = manyChars (choiceL [ pEscape; pOtherChar ] "")
        let! _ = pitem '"'
        return chars
    }

// Any codepoint except " or \ or control characters
static let pOtherChar: Parser<_, _, _, _, _> =
    satisfyL
        (function
        | '"'
        | '\\' -> false
        | c -> not (Char.IsControl c))
        "Other Char"
```

Escape sequences are all preceded by `\`. Where there are many posibilities with the same prefix it can be more efficient to use XParsec in an imperative mode, using the `PeekN` and `SkipN` methods on `Reader`. This makes the parser more efficient in several ways:

- Error messages won't be generated for each failing case
- The reader's position isn't force to skip back and forth

Avoiding creating unnecessary error messages is often one of the best ways to speed up your parser. That is one reason why the `parseL` parser variants exist like `satisfyL` or `choiceL`, it allows XParsec to skip generating a dynamic error messages and use the provided string.

```fsharp
// Parse a hex digit to an integer (case insensitive)
static let pHexDigit =
    satisfyL Char.IsAsciiHexDigit ("Hex digit")
    |>> function
        | c when c >= '0' && c <= '9' -> int c - int '0'
        | c when c >= 'a' && c <= 'f' -> int c - int 'a' + 10
        | c when c >= 'A' && c <= 'F' -> int c - int 'A' + 10
        | _ -> failwith "Invalid hex digit" // Unreachable

// A unicode escape sequence is \u followed by 4 hex digits
static let pUnicodeEscape: Parser<_, _, _, _, _> =
    parser {
        let! _ = pstring "\\u"
        let! hex0 = pHexDigit
        let! hex1 = pHexDigit
        let! hex2 = pHexDigit
        let! hex3 = pHexDigit
        let hexValue = (hex0 <<< 12) + (hex1 <<< 8) + (hex2 <<< 4) + hex3
        return Convert.ToChar(hexValue)
    }

// An escape sequence is \ followed by one of [ '"'; '\\'; '/'; 'b'; 'f'; 'n'; 'r'; 't'; 'u' ]
static let pEscape =
    fun (reader: Reader<_, _, _, _>) ->
        let span = reader.PeekN 6

        if span.Length >= 2 && span[0] = '\\' then
            match span[1] with
            | '"'
            | '\\'
            | '/' as c ->
                reader.SkipN 2
                preturn c reader
            | 'b' ->
                reader.SkipN 2
                preturn '\b' reader
            | 'f' ->
                reader.SkipN 2
                preturn '\f' reader
            | 'n' ->
                reader.SkipN 2
                preturn '\n' reader
            | 'r' ->
                reader.SkipN 2
                preturn '\r' reader
            | 't' ->
                reader.SkipN 2
                preturn '\t' reader
            | 'u' ->
                if span.Length < 6 then
                    fail (Message "Unicode escape sequence must be followed by 4 hex digits") reader
                else
                    pUnicodeEscape reader
            | c -> fail (Unexpected c) reader
        else
            fail (Message "Escape char") reader
```

### Number

> A number is very much like a C or Java number, except that the octal and hexadecimal formats are not used.

![JSON Number](./images/json-number.png)

The number grammar can be broken down into an optional sign, an integer part, an optional fractional part, and an optional exponent part. We build parsers for each piece and combine them.

```fsharp
static let pDigit = satisfyL (fun c -> c >= '0' && c <= '9') "Char in range '0' - '9'"
static let pOneNine = satisfyL (fun c -> c >= '1' && c <= '9') "Char in range '1' - '9'"

// A fraction is a dot followed by one or more digits
static let pFraction =
    parser {
        let! dot = pitem '.'
        let! digits = many1Chars pDigit
        return digits
    }

// An exponent is 'e' or 'E', an optional sign, and one or more digits.
static let pExponent =
    parser {
        let! e = anyOf [ 'e'; 'E' ]
        let! sign = opt (anyOf [ '+'; '-' ])
        let! digits = many1Chars pDigit
        return struct (sign, digits)
    }

// The complete number parser.
static let pNumber =
    parser {
        // 1. Parse optional sign.
        let! sign = opt (pitem '-')

        // 2. Parse integer part (either "0" or "1-9" followed by digits).
        // Leading zeroes are not allowed.
        let! int = (pitem '0' >>% "0") <|> (many1Chars2 pOneNine pDigit)

        // 3. Parse optional fractional and exponent parts.
        let! fraction = opt pFraction
        let! exponent = opt pExponent

        // 4. Build the final number string and parse it to a float.
        let! number =
            let sb = StringBuilder(16)

            match sign with
            | ValueSome c -> sb.Append(c) |> ignore
            | _ -> ()

            sb.Append(int) |> ignore

            match fraction with
            | ValueSome f -> sb.Append('.').Append(f) |> ignore
            | _ -> ()

            match exponent with
            | ValueSome(sign, digits) ->
                sb.Append('e') |> ignore

                match sign with
                | ValueSome sign -> sb.Append(sign) |> ignore
                | _ -> ()

                sb.Append(digits) |> ignore
            | _ -> ()

            let number = sb.ToString()

            match Double.TryParse(number) with
            | true, result -> preturn result
            | _ -> failwithf "Failed to parse number: %s" number

        return JsonValue.Number number
    }
```

## 4. Recursive Parsers: Values, Objects and Arrays

Now we get to the core of the JSON structure: values, objects, and arrays. These parsers are mutually recursive. We define them using F#'s `let rec ... and ...` syntax.

### Value

> A value can be a string in double quotes, or a number, or true or false or null, or an object or an array. These structures can be nested.

![JSON Value](./images/json-value.png)

This is the central dispatcher. It looks at the next character to decide which specific value parser to run. This is a common and efficient technique called "predictive parsing."

```fsharp
static let pTrue = pstring "true" >>% JsonValue.True
static let pFalse = pstring "false" >>% JsonValue.False
static let pNull = pstring "null" >>% JsonValue.Null
static let pStringValue = pString |>> JsonValue.String

static let rec pValue: Parser<JsonValue, char, _, _, _> =
    fun cursor ->
        match cursor.Peek() with
        | ValueSome '{' -> pObject cursor
        | ValueSome '[' -> pArray cursor
        | ValueSome '"' -> pStringValue cursor
        | ValueSome 't' -> pTrue cursor
        | ValueSome 'f' -> pFalse cursor
        | ValueSome 'n' -> pNull cursor
        | _ -> pNumber cursor

// A helper to parse any value, surrounded by optional whitespace
and pElement =
    parser {
        let! _ = pWhitespace
        let! value = pValue
        let! _ = pWhitespace
        return value
    }
```

### Object

> An object is an unordered set of name/value pairs. An object begins with `{` left brace and ends with `}` right brace. Each name is followed by `:` colon and the name/value pairs are separated by `,` comma.

![JSON Object](./images/json-object.png)

```fsharp
// A key-value pair inside an object
and pMember =
    parser {
        let! _ = pWhitespace
        let! name = pString
        let! _ = pWhitespace
        let! _ = pitem ':'
        let! value = pElement
        return { Name = name; Value = value }
    }

// A full JSON object: '{' members '}'
and pObject =
    parser {
        let! _ = pitem '{'
        let! _ = pWhitespace
        // sepBy will not parse a trailing comma, consistent with the JSON spec
        let! members, _ = sepBy pMember (pitem ',')
        let! _ = pitem '}'
        return JsonValue.Object members
    }
```

### Array

> An array is an ordered collection of values. An array begins with `[` left bracket and ends with `]` right bracket. Values are separated by `,` comma.

![JSON Array](./images/json-array.png)

```fsharp
// A full JSON array: '[' elements ']'
and pArray = parser {
    do! pchar '['
    let! values = sepBy pElement (pchar ',')
    do! pchar ']'
    return JsonValue.Array (values |> fst)
}
```

## 5. The Final Parser

A valid JSON document consists of a single element (a value, object or array) followed by the end of the input.

```fsharp
// inside JsonParsers type...
static let pJson: Parser<JsonValue, char, unit, 'Input, 'InputSlice> = 
    pElement .>> eof

// We expose it as a public member
static member Parser = pJson
```

With `pJson` defined, our `JsonParsers.Parser` static member is now complete and ready to use. This example shows how to build a complex, efficient, and readable parser by breaking the problem down into small, manageable pieces that mirror the official specification.
