module XParsec.Json

open System
open System.Collections.Immutable

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

module JsonParsers =
    open System.Text

    let pWhitespace = many (anyOf [ ' '; '\t'; '\n'; '\r' ])

    let pDigit = digit

    let pOneNine = satisfyL (fun c -> c >= '1' && c <= '9') ("Char in range '1' - '9'")

    let pFraction =
        parser {
            let! dot = pchar '.'
            let! digits = many1Chars pDigit
            return digits
        }

    let pExponent =
        parser {
            let! e = anyOf [ 'e'; 'E' ]
            let! sign = opt (anyOf [ '+'; '-' ])
            let! digits = many1Chars pDigit
            return (struct (sign, digits))
        }

    let pNumber =
        parser {
            let! sign = opt (pchar '-')
            let! int = (pchar '0' >>% "0") <|> (many1Chars2 pOneNine pDigit)
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

            return number
        }

    let pEscape =
        parser {
            let! _ = pchar '\\'
            let! escaped = anyOf [ '"'; '\\'; '/'; 'b'; 'f'; 'n'; 'r'; 't' ]

            return
                match escaped with
                | 'b' -> '\b'
                | 'f' -> '\f'
                | 'n' -> '\n'
                | 'r' -> '\r'
                | 't' -> '\t'
                | c -> c
        }

    let pHexDigit =
        satisfyL Char.IsAsciiHexDigit ("Hex digit")
        |>> function
            | c when c >= '0' && c <= '9' -> int c - int '0'
            | c when c >= 'a' && c <= 'f' -> int c - int 'a' + 10
            | c when c >= 'A' && c <= 'F' -> int c - int 'A' + 10
            | _ -> failwith "Invalid hex digit"

    let pUnicodeEscape =
        parser {
            let! _ = pchar '\\'
            let! _ = pchar 'u'
            let! hex0 = pHexDigit
            let! hex1 = pHexDigit
            let! hex2 = pHexDigit
            let! hex3 = pHexDigit
            let hexValue = (hex0 <<< 12) + (hex1 <<< 8) + (hex2 <<< 4) + hex3
            return Convert.ToChar(hexValue)
        }

    let pOtherChar =
        satisfyL
            (function
            | '"'
            | '\\' -> false
            | c -> not (Char.IsControl c))
            ("Other Char")

    let pString =
        parser {
            let! _ = pchar '"'
            let! chars = manyChars (choice [ pEscape; pUnicodeEscape; pOtherChar ])
            let! _ = pchar '"'
            return chars
        }

    let pTrue = pstring "true" >>% JsonValue.True

    let pFalse = pstring "false" >>% JsonValue.False

    let pNull = pstring "null" >>% JsonValue.Null

    let rec pValue =
        choice
            [
                pString |>> JsonValue.String
                pNumber |>> JsonValue.Number
                pTrue
                pFalse
                pNull
                pObject
                pArray
            ]

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
            let! _ = pchar ':'
            let! value = pElement
            return { Name = name; Value = value }
        }

    and pObject =
        parser {
            let! _ = pchar '{'
            let! _ = pWhitespace
            let! (members, _) = sepBy pMember (pchar ',')
            let! _ = pchar '}'
            return JsonValue.Object members
        }

    and pArray =
        parser {
            let! _ = pchar '['
            let! _ = pWhitespace
            let! (values, _) = sepBy pElement (pchar ',')
            let! _ = pchar ']'
            return JsonValue.Array values
        }

    let pJson: Parser<JsonValue, char, unit, ReadableString, ReadableStringSlice> =
        (pElement .>> eof)
