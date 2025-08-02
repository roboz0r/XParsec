namespace XParsec.Json

open System
open System.Collections.Immutable
open System.Text

open XParsec
open XParsec.Parsers
open XParsec.CharParsers

#nowarn "40" // Recursive value definitions

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

type JsonParsers<'Input, 'InputSlice
    when 'Input :> IReadable<char, 'InputSlice> and 'InputSlice :> IReadable<char, 'InputSlice>> =

    static let pWhitespace = skipMany (anyOf [ ' '; '\t'; '\n'; '\r' ])

    static let pDigit = satisfyL (fun c -> c >= '0' && c <= '9') "Char in range '0' - '9'"

    static let pOneNine = satisfyL (fun c -> c >= '1' && c <= '9') "Char in range '1' - '9'"

    static let pFraction =
        parser {
            let! dot = pitem '.'
            let! digits = many1Chars pDigit
            return digits
        }

    static let pExponent =
        parser {
            let! e = anyOf [ 'e'; 'E' ]
            let! sign = opt (anyOf [ '+'; '-' ])
            let! digits = many1Chars pDigit
            return struct (sign, digits)
        }

    static let pNumber =
        parser {
            let! sign = opt (pitem '-')

            let! int = (pitem '0' >>% "0") <|> (many1Chars2 pOneNine pDigit)

            let! fraction = opt pFraction
            let! exponent = opt pExponent

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
                    | ValueNone -> ()

                    sb.Append(digits) |> ignore
                | _ -> ()

                let number = sb.ToString()

                match Double.TryParse(number) with
                | true, result -> preturn result
                | _ -> failwithf "Failed to parse number: %s" number

            return JsonValue.Number number
        }

    static let pHexDigit =
        satisfyL Char.IsAsciiHexDigit ("Hex digit")
        |>> function
            | c when c >= '0' && c <= '9' -> int c - int '0'
            | c when c >= 'a' && c <= 'f' -> int c - int 'a' + 10
            | c when c >= 'A' && c <= 'F' -> int c - int 'A' + 10
            | _ -> failwith "Invalid hex digit"

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

    static let pOtherChar: Parser<_, _, _, _, _> =
        satisfyL
            (function
            | '"'
            | '\\' -> false
            | c -> not (Char.IsControl c))
            "Other Char"

    static let pString =
        parser {
            let! _ = pitem '"'
            let! chars = manyChars (choiceL [ pEscape; pOtherChar ] "")
            let! _ = pitem '"'
            return chars
        }

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

    and pElement =
        parser {
            let! _ = pWhitespace
            let! value = pValue
            let! _ = pWhitespace
            return value
        }

    and pMember =
        parser {
            let! _ = pWhitespace
            let! name = pString
            let! _ = pWhitespace
            let! _ = pitem ':'
            let! value = pElement
            return { Name = name; Value = value }
        }

    and pObject =
        parser {
            let! _ = pitem '{'
            let! _ = pWhitespace
            let! members, _ = sepBy pMember (pitem ',')
            let! _ = pitem '}'
            return JsonValue.Object members
        }

    and pArray =
        parser {
            let! _ = pitem '['
            let! _ = pWhitespace
            let! values, _ = sepBy pElement (pitem ',')
            let! _ = pitem ']'
            return JsonValue.Array values
        }

    static let pJson: Parser<JsonValue, char, unit, 'Input, 'InputSlice> = pElement .>> eof

    static member Parser = pJson
    static member PString = pString
