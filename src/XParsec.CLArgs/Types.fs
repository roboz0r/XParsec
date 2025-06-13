namespace XParsec.CLArgs

open System
open System.Collections.Immutable
open XParsec
open XParsec.CharParsers

type ParsedOption<'T> =
    /// The parsed value of the option.
    | Parsed of 'T
    /// The error that occurred while parsing the option.
    | Unparsed of string

type ParsedOptions<'T> =
    {
        /// The parsed options.
        Options: ImmutableArray<'T>
        /// The unparsed options, empty if all options are parsed successfully.
        Unparsed: ImmutableArray<string>
    }

type ParsedCommand<'T> =
    {
        /// The parsed command.
        Command: 'T
        /// The unparsed options, empty if all options are parsed successfully.
        Unparsed: ImmutableArray<string>
    }

module ParsedOptions =
    let ofOptions (options: ImmutableArray<ParsedOption<'T>>) : ParsedOptions<'T> =
        let parsed, unparsed =
            let parsed = ImmutableArray.CreateBuilder<'T>()
            let unparsed = ImmutableArray.CreateBuilder<string>()

            for option in options do
                match option with
                | Parsed x -> parsed.Add(x)
                | Unparsed x -> unparsed.Add(x)

            parsed.ToImmutable(), unparsed.ToImmutable()

        {
            Options = parsed
            Unparsed = unparsed
        }

type EqualsAssignment =
    /// The equals assignment is allowed. e.g. --name=John or --name John
    | AllowEqualsAssignment
    /// The equals assignment is not allowed. e.g. --name John
    | NoEqualsAssignment
    /// The equals assignment is required. e.g. --name=John
    | RequireEqualsAssignment

type CLOption<'T> =
    /// An option that is either present or absent. e.g. --name or -n
    | CLFlag of name: string * result: 'T * alias: char option * description: string
    /// An option that has a string value. e.g. --name=John or -n John
    | CLSetting of
        name: string *
        toResult: (string -> 'T) *
        alias: char option *
        equalsAssginment: EqualsAssignment *
        description: string
    /// An option that has a boolean value. e.g. --name=true or -n true
    | CLBoolSetting of
        name: string *
        toResult: (bool -> 'T) *
        alias: char option *
        equalsAssginment: EqualsAssignment *
        description: string
    /// An option that has a parsed value. e.g. --name=123 or -n 123
    /// If the parser fails, all arguments are considered invalid.
    | CLParsedSetting of
        name: string *
        parser: Parser<'T, char, unit, ReadableString, ReadableStringSlice> *
        alias: char option *
        equalsAssginment: EqualsAssignment *
        description: string
    /// An option that has a string value and no preceding flag. e.g. John
    /// Only one of these options can be present in a command.
    | CLDefaultSetting of name: string * toResult: (string -> 'T) * description: string
    /// An option that has a parsed value and no preceding flag. e.g. 123
    /// Only one of these options can be present in a command.
    | CLDefaultParsedSetting of
        name: string *
        parser: Parser<'T, char, unit, ReadableString, ReadableStringSlice> *
        description: string

// TODO: Add a CLPassthrough option that allows passing through any arguments that are not recognized.
// like --name John --age 30 -- --extra "extra argument"
// everything after -- is considered a passthrough argument.

/// Command options with the parser removed.
type CLCmdOption =
    | CLCmdFlag of name: string * alias: char option * description: string

    | CLCmdSetting of name: string * alias: char option * equalsAssignment: EqualsAssignment * description: string

    | CLCmdBoolSetting of name: string * alias: char option * equalsAssignment: EqualsAssignment * description: string

    | CLCmdParsedSetting of name: string * alias: char option * equalsAssignment: EqualsAssignment * description: string
    | CLCmdDefaultSetting of name: string * description: string
    | CLCmdDefaultParsedSetting of name: string * description: string

module internal CLOption =
    let toCLCmdOption (option: CLOption<'T>) : CLCmdOption =
        match option with
        | CLFlag(name, _, alias, description) -> CLCmdFlag(name, alias, description)
        | CLSetting(name, _, alias, equalsAssignment, description) ->
            CLCmdSetting(name, alias, equalsAssignment, description)

        | CLBoolSetting(name, _, alias, equalsAssignment, description) ->
            CLCmdBoolSetting(name, alias, equalsAssignment, description)

        | CLParsedSetting(name, _, alias, equalsAssignment, description) ->
            CLCmdParsedSetting(name, alias, equalsAssignment, description)

        | CLDefaultSetting(name, _, description) -> CLCmdDefaultSetting(name, description)
        | CLDefaultParsedSetting(name, _, description) -> CLCmdDefaultParsedSetting(name, description)

type CLCommand<'T> =
    /// The options accepted when no command is parsed
    | CLDefault of
        options: CLCmdOption list *
        pCommand: Parser<ParsedCommand<'T>, string, unit, ReadableArray<string>, ReadableArraySlice<string>> *
        description: string
    /// A command that has a name and a list of options.
    | CLCommand of
        commands: string list *
        options: CLCmdOption list *
        pCommand: Parser<ParsedCommand<'T>, string, unit, ReadableArray<string>, ReadableArraySlice<string>> *
        description: string

module NestedParser =

    let pNestedString
        formatNestedError
        (pInner: Parser<_, _, _, _, _>)
        innerState
        (reader: Reader<_, _, _, _>)
        : ParseResult<_, _, _> =
        match reader.Peek() with
        | ValueSome(x: string) ->
            let innerReader = Reader.ofString x innerState

            match pInner innerReader with
            | Ok result ->
                reader.Skip()
                ParseSuccess.create result.Parsed

            | Error e ->
                let formatError = formatNestedError x e
                ParseError.create (Message formatError) reader.Position

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

    let private parseSetting name parser alias equalsAssginment =
        let inline pNested parser =
            pNestedString ErrorFormatting.formatStringError parser ()

        let setting = $"--%s{name}"
        let pNormal = pitem setting >>. pNested parser

        let pEq =
            let pSetting = pEqualsAssignment setting parser
            pNested pSetting

        seq {
            match equalsAssginment with
            | AllowEqualsAssignment ->
                yield pNormal
                yield pEq
            | NoEqualsAssignment -> yield pNormal
            | RequireEqualsAssignment -> yield pEq

            match alias with
            | Some alias ->
                let aliasSetting = $"-%c{alias}"
                let pAlias = pitem aliasSetting >>. pNested parser

                let pEqAlias =
                    let pAliasSetting = pEqualsAssignment aliasSetting parser
                    pNested pAliasSetting

                match equalsAssginment with
                | AllowEqualsAssignment ->
                    yield pAlias
                    yield pEqAlias
                | NoEqualsAssignment -> yield pAlias
                | RequireEqualsAssignment -> yield pEqAlias


            | None -> ()

        }
        |> choice

    let private parseFlag name result alias =
        let flag = $"--%s{name}"

        match alias with
        | Some alias ->
            let aliasFlag = $"-%c{alias}"
            let pName = pitem flag
            let pAlias = pitem aliasFlag
            (pName <|> pAlias) >>% result

        | None -> itemReturn flag result

    let private orUnparsed p = (p |>> Parsed) <|> (pid |>> Unparsed)

    let private ofOption (option: CLOption<'T>) : Parser<_, _, _, _, _> =
        match option with
        | CLFlag(name, result, alias, _) -> parseFlag name result alias

        | CLSetting(name, toResult, alias, equalsAssginment, _) ->
            let p = (many1Chars anyChar) |>> toResult
            parseSetting name p alias equalsAssginment

        | CLBoolSetting(name, toResult, alias, equalsAssginment, _) ->
            parseSetting name (pbool |>> toResult) alias equalsAssginment

        | CLParsedSetting(name, parser, alias, equalsAssginment, _) -> parseSetting name parser alias equalsAssginment

        | CLDefaultSetting(name, toResult, _) ->
            let p = (many1Chars anyChar) |>> toResult
            pNestedString ErrorFormatting.formatStringError p ()

        | CLDefaultParsedSetting(name, p, _) -> pNestedString ErrorFormatting.formatStringError p ()


    let ofOptions (options: CLOption<'T> list) : Parser<_, _, _, _, _> =
        let p = options |> List.map ofOption |> choice |> orUnparsed |> many

        p .>> eof |>> ParsedOptions.ofOptions

    let command (commands: string list) (description: string) mapOptions (options: CLOption<'T> list) =
        let info = options |> List.map CLOption.toCLCmdOption

        let pCmd =
            parser {
                let! _ = pseq commands
                let! options = ofOptions options
                do! eof
                let cmdArgs = mapOptions options.Options

                return
                    {
                        Command = cmdArgs
                        Unparsed = options.Unparsed
                    }
            }

        CLCommand(commands, info, pCmd, description)

    let defaultCommand (description: string) mapOptions (options: CLOption<'T> list) =
        let pOptions = ofOptions options
        let info = options |> List.map CLOption.toCLCmdOption

        let pCmd =
            parser {
                let! options = pOptions
                do! eof
                let cmdArgs = mapOptions options.Options

                return
                    {
                        Command = cmdArgs
                        Unparsed = options.Unparsed
                    }
            }

        CLDefault(info, pCmd, description)
