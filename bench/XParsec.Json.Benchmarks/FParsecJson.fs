namespace XParsec.Json.Benchmarks

open System
open System.Collections.Immutable
open System.Text

open XParsec.Json

open FParsec

#nowarn "40" // Recursive value definitions

type FParsecJsonParsers =

    static let pWhitespace = skipMany (anyOf [ ' '; '\t'; '\n'; '\r' ])

    static let pDigit = satisfyL (fun c -> c >= '0' && c <= '9') ("Char in range '0' - '9'")

    static let pOneNine = satisfyL (fun c -> c >= '1' && c <= '9') ("Char in range '1' - '9'")

    static let pFraction =
        parse {
            let! dot = pchar '.'
            let! digits = many1Chars pDigit
            return digits
        }

    static let pExponent =
        parse {
            let! e = anyOf [ 'e'; 'E' ]
            let! sign = opt (anyOf [ '+'; '-' ])
            let! digits = many1Chars pDigit
            return (struct (sign, digits))
        }

    static let pNumber =
        parse {
            let! sign = opt (pchar '-')

            let! int = (pchar '0' >>% "0") <|> (many1Chars2 pOneNine pDigit)

            let! fraction = opt pFraction
            let! exponent = opt pExponent

            let! number =
                let sb = StringBuilder(16)

                match sign with
                | Some c -> sb.Append(c) |> ignore
                | _ -> ()

                sb.Append(int) |> ignore

                match fraction with
                | Some f -> sb.Append('.').Append(f) |> ignore
                | _ -> ()

                match exponent with
                | Some(sign, digits) ->
                    sb.Append('e') |> ignore

                    match sign with
                    | Some sign -> sb.Append(sign) |> ignore
                    | None -> ()

                    sb.Append(digits) |> ignore
                | _ -> ()

                let number = sb.ToString()

                match Double.TryParse(number) with
                | true, result -> preturn result
                | _ -> failwithf "Failed to parse number: %s" number

            return number
        }

    static let pEscape =
        parse {
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

    static let pHexDigit =
        satisfyL Char.IsAsciiHexDigit ("Hex digit")
        |>> function
            | c when c >= '0' && c <= '9' -> int c - int '0'
            | c when c >= 'a' && c <= 'f' -> int c - int 'a' + 10
            | c when c >= 'A' && c <= 'F' -> int c - int 'A' + 10
            | _ -> failwith "Invalid hex digit"

    static let pUnicodeEscape =
        parse {
            let! _ = pchar '\\'
            let! _ = pchar 'u'
            let! hex0 = pHexDigit
            let! hex1 = pHexDigit
            let! hex2 = pHexDigit
            let! hex3 = pHexDigit
            let hexValue = (hex0 <<< 12) + (hex1 <<< 8) + (hex2 <<< 4) + hex3
            return Convert.ToChar(hexValue)
        }

    static let pOtherChar =
        satisfyL
            (function
            | '"'
            | '\\' -> false
            | c -> not (Char.IsControl c))
            ("Other Char")

    static let pString =
        parse {
            let! _ = pchar '"'
            let! chars = manyChars (choiceL [ pEscape; pUnicodeEscape; pOtherChar ] "")
            let! _ = pchar '"'
            return chars
        }

    static let pTrue = pstring "true" >>% JsonValue.True

    static let pFalse = pstring "false" >>% JsonValue.False

    static let pNull = pstring "null" >>% JsonValue.Null

    static let rec pValue =
        choiceL
            [
                pString |>> JsonValue.String
                pNumber |>> JsonValue.Number
                pTrue
                pFalse
                pNull
                pObject
                pArray
            ]
            ""

    and pElement =
        parse {
            let! _ = pWhitespace
            let! value = pValue
            let! _ = pWhitespace
            return value
        }

    and pMember =
        parse {
            let! _ = pWhitespace
            let! name = pString
            let! _ = pWhitespace
            let! _ = pchar ':'
            let! value = pElement
            return { Name = name; Value = value }
        }

    and pObject =
        parse {
            let! _ = pchar '{'
            let! _ = pWhitespace
            let! members = sepBy pMember (pchar ',')
            let! _ = pchar '}'
            return JsonValue.Object(ImmutableArray.CreateRange members)
        }

    and pArray =
        parse {
            let! _ = pchar '['
            let! _ = pWhitespace
            let! values = sepBy pElement (pchar ',')
            let! _ = pchar ']'
            return JsonValue.Array(ImmutableArray.CreateRange values)
        }

    static let pJson: Parser<_, _> = (pElement .>> eof)

    static member Parser = pJson
    static member PString = pString
