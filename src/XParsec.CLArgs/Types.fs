namespace XParsec.CLArgs

open XParsec
open XParsec.CharParsers

type CLOption<'T> =
    | CLFlag of name: string * result: 'T * alias: char option
    | CLSetting of name: string * toResult: (string -> 'T) * alias: char option * allowEqualsAssginment: bool
    | CLBoolSetting of name: string * toResult: (bool -> 'T) * alias: char option * allowEqualsAssginment: bool
    | CLParsedSetting of
        name: string *
        parser: Parser<'T, char, unit, ReadableString, ReadableStringSlice> *
        alias: char option *
        allowEqualsAssginment: bool

module NestedParser =
    open System.Text

    let pNestedString innerState (pInner: Parser<_, _, _, _, _>) (reader: Reader<_, _, _, _>) : ParseResult<_, _, _> =
        match reader.Peek() with
        | ValueSome(x: string) ->
            let innerReader = Reader.ofString x innerState

            match pInner innerReader with
            | Ok result ->
                reader.Skip()
                ParseSuccess.create result.Parsed

            | Error e ->
                let sb = StringBuilder()
                let formatError = ErrorFormatting.formatStringError x e sb
                ParseError.create (Message(formatError.ToString())) reader.Position

        | ValueNone -> ParseError.create EndOfInput reader.Position

open NestedParser


module CLParser =
    open XParsec.Parsers

    let private pEqualsAssignment setting pValue =
        parser {
            let! _ = pstring setting
            let! _ = pchar '='
            let! value = pValue
            do! eof
            return value
        }

    let private pbool =
        parser { return! (pstring "true" >>% true) <|> (pstring "false" >>% false) }

    let private parseSetting name parser alias allowEqualsAssginment =
        seq {
            let setting = $"--%s{name}"
            yield pitem setting >>. (pNestedString () parser)

            if allowEqualsAssginment then
                let pSetting = pEqualsAssignment setting parser

                yield pNestedString () pSetting

            match alias with
            | Some alias ->
                let aliasSetting = $"-%c{alias}"
                yield pitem aliasSetting >>. (pNestedString () parser)

                if allowEqualsAssginment then
                    let pAliasSetting = pEqualsAssignment aliasSetting parser

                    yield pNestedString () pAliasSetting

            | None -> ()

        }
        |> choice

    let ofOption (option: CLOption<'T>) : Parser<_, _, _, _, _> =
        match option with
        | CLFlag(name, result, alias) ->
            let flag = $"--%s{name}"

            match alias with
            | Some alias ->
                let aliasFlag = $"-%c{alias}"
                let pName = pitem flag
                let pAlias = pitem aliasFlag
                (pName <|> pAlias) >>% result

            | None -> itemReturn flag result
        | CLSetting(name, toResult, alias, allowEqualsAssginment) ->
            let p = (many1Chars anyChar) |>> toResult
            parseSetting name p alias allowEqualsAssginment

        | CLBoolSetting(name, toResult, alias, allowEqualsAssginment) ->
            parseSetting name (pbool |>> toResult) alias allowEqualsAssginment

        | CLParsedSetting(name, parser, alias, allowEqualsAssginment) ->
            parseSetting name parser alias allowEqualsAssginment

    let ofOptions (options: CLOption<'T> list) : Parser<_, _, _, _, _> =
        let p = options |> List.map ofOption |> choice |> many

        p .>> eof
