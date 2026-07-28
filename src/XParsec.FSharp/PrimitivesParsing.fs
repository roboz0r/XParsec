namespace XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.Parsers
open XParsec.FSharp.Lexer


[<RequireQualifiedAccess>]
module AttributeTarget =
    let private pContextualKeyword s ctor =
        // Hoist err out of the parser CE body — allocated once per (s) at module load.
        let err: ErrorType<PositionedToken, ParseState> = Message(sprintf "Expected '%s'" s)

        parser {
            let! state = getUserState

            let! t =
                nextSyntaxTokenSatisfiesL
                    (fun t -> t.Token = Token.Identifier && ParseState.tokenStringIs s t state)
                    err

            return ctor t
        }

    let private pKw k ctor =
        nextSyntaxTokenIsLMsg k (sprintf "Expected '%A'" k) |>> ctor

    let parse: FSParser<AttributeTarget<SyntaxToken>> =
        choiceL
            [
                pContextualKeyword "assembly" AttributeTarget.Assembly
                pKw Token.KWModule AttributeTarget.Module
                pKw Token.KWReturn AttributeTarget.Return
                pContextualKeyword "field" AttributeTarget.Field
                pContextualKeyword "property" AttributeTarget.Property
                pContextualKeyword "param" AttributeTarget.Param
                pKw Token.KWType AttributeTarget.Type
                pContextualKeyword "constructor" AttributeTarget.Constructor
                pContextualKeyword "event" AttributeTarget.Event
            ]
            "AttributeTarget"

[<RequireQualifiedAccess>]
module Attribute =
    let parse: FSParser<Attribute<SyntaxToken>> =
        parser {
            // Attempt to parse the target (e.g. "assembly:")
            // We need `opt` because "assembly" could also be the start of the ObjectConstruction (the Attribute Type name)
            // if the colon is missing.
            let! target =
                opt (
                    parser {
                        let! t = AttributeTarget.parse
                        let! c = pColon
                        return (t, c)
                    }
                )

            let! construction = refObjectConstruction.Parser
            return Attribute.Attribute(target, construction)
        }

[<RequireQualifiedAccess>]
module AttributeSet =
    let private pLAttrBracket =
        nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.KWLAttrBracket) "Expected '[<'"

    let private pRAttrBracket =
        nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.KWRAttrBracket) "Expected '>]'"

    let private pAttributeItem =
        parser {
            let! attr = Attribute.parse
            let! sep = opt pSemi
            return (attr, sep)
        }

    let private pAttributeItems = many pAttributeItem

    let parse: FSParser<AttributeSet<SyntaxToken>> =
        parser {
            let! lBracket = pLAttrBracket
            let! attributes = pAttributeItems
            let! rBracket = pRAttrBracket
            return AttributeSet(lBracket, attributes, rBracket)
        }

[<RequireQualifiedAccess>]
module Attributes =
    let parse: FSParser<Attributes<SyntaxToken>> = many1 AttributeSet.parse

[<RequireQualifiedAccess>]
module RangeOpName =
    // `.. ..` is the step-range operator name. It appears only as an operator name
    // inside parens (e.g. `let inline (.. ..) start step finish = ...`); never infix.
    // The lexer emits two separate `OpRange` tokens because the greedy operator scan
    // can't span the whitespace/comments/newlines that may sit between the two `..`
    // pieces, so the parser fuses them here instead.
    let private pRangeFirst =
        nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.OpRange) "Expected '..'"

    let private pRangeSecond =
        nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.OpRange) "'..'"

    let parse: FSParser<RangeOpName<SyntaxToken>> =
        parser {
            let! first = pRangeFirst

            let! second = opt pRangeSecond

            match second with
            | ValueSome second -> return RangeOpName.DotDotDotDot(first, second)
            | ValueNone -> return RangeOpName.DotDot first
        }

[<RequireQualifiedAccess>]
module ActivePatternOpName =
    let private pIdent = nextSyntaxIdentifierLMsg "Expected identifier"

    let parse: FSParser<ActivePatternOpName<SyntaxToken>> =
        // Recursive helper to parse segments: ident | ...
        let rec parseSegments (builder: ImmutableArray<_>.Builder) =
            parser {
                let! ident = pIdent
                builder.Add(ident)

                let! bar = pBar

                // Look ahead to see if we are at the end (RParen) or if there is a wildcard
                match! opt (lookAhead pRParen) with
                | ValueSome _ ->
                    // Found ')', so 'bar' is the rBar
                    return (builder.ToImmutable(), ValueNone, bar)
                | ValueNone ->
                    // Check for wildcard '_'
                    match! opt (lookAhead pWildcard) with
                    | ValueSome _ ->
                        let! underscore = nextSyntaxToken // Consume '_'
                        let! finalBar = pBar
                        return (builder.ToImmutable(), ValueSome underscore, finalBar)
                    | ValueNone ->
                        // Must be another identifier, loop
                        return! parseSegments builder
            }

        parser {
            // Parses the inside of (| ... |). The surrounding parens are handled by IdentOrOp.ParenOp.
            // Starts with '|'
            let! lBar = pBar

            let! idents, underscore, rBar = parseSegments (ImmutableArray.CreateBuilder())

            return ActivePatternOp(lBar, idents, underscore, rBar)
        }

[<RequireQualifiedAccess>]
module OpName =
    let private pSymbolicOp =
        nextSyntaxTokenSatisfiesLMsg
            (fun t -> t.Token.IsOperator || TokenInfo.isOperatorKeyword t.Token)
            "Expected symbolic operator"
        |>> OpName.SymbolicOp

    // [] — op_Nil. Internal FSharp.Core syntax for `type List<'T> = | ([]) : 'T list`.
    let private pNilOp =
        parser {
            let! lBracket = pLBracket
            let! rBracket = pRBracket
            return OpName.NilOp(lBracket, rBracket)
        }

    let parse: FSParser<OpName<SyntaxToken>> =
        choiceL
            [
                RangeOpName.parse |>> OpName.RangeOp
                // Attempt active pattern first because it starts with '|', which is also a symbolic op
                ActivePatternOpName.parse |>> OpName.ActivePatternOp
                pNilOp
                pSymbolicOp
            ]
            "OpName"

[<RequireQualifiedAccess>]
module IdentOrOp =
    let private pIdentOrOpIdent = nextSyntaxIdentifierLMsg "Expected Identifier"

    let parse: FSParser<IdentOrOp<SyntaxToken>> =
        choiceL
            [
                // Simple Identifier (including backticked)
                pIdentOrOpIdent |>> IdentOrOp.Ident

                // Parenthesized Operator (op) or Active Pattern (| ... |)
                parser {
                    let! l = pLParen
                    let! op = OpName.parse
                    let! r = pRParen
                    return IdentOrOp.ParenOp(l, op, r)
                }
            ]
            "IdentOrOp"

[<RequireQualifiedAccess>]
module LongIdent =
    // Simple parser for A.B.C — every identifier pattern, type reference, and module
    // path goes through here, so this is hot.
    let private pIdent = nextSyntaxIdentifierLMsg "Expected Identifier"

    /// Build a singleton LongIdent (one ident, no dots). For the common bare-identifier
    /// case, lets callers avoid threading an empty dots array through manually.
    let inline single (ident: 'T) : LongIdent<'T> =
        {
            Idents = ImmutableArray.Create(ident)
            Dots = ImmutableArray.Empty
        }

    // Direct implementation rather than `sepBy1 pIdent pDot`: the dots ARE part of
    // the AST (CST-style — kept for downstream tooling), but writing a single loop
    // with two SmallArrayBuilders lets the common count=1 case stay entirely on the
    // stack (single-ident lids get `ImmutableArray.Empty` for dots, no allocation).
    let parse: FSParser<LongIdent<SyntaxToken>> =
        fun reader ->
            match pIdent reader with
            | Error e -> Error e
            | Ok first ->
                let mutable idents = SmallArrayBuilder<_>()
                let mutable dots = SmallArrayBuilder<_>()
                idents.Add(first)
                let mutable ok = true

                while ok do
                    let pos = reader.Position

                    match pDot reader with
                    | Ok dot ->
                        match pIdent reader with
                        | Ok ident ->
                            if reader.Position = pos then
                                raise (InfiniteLoopException pos)

                            dots.Add(dot)
                            idents.Add(ident)
                        | Error _ ->
                            reader.Position <- pos
                            ok <- false
                    | Error _ ->
                        reader.Position <- pos
                        ok <- false

                Ok
                    {
                        Idents = idents.ToImmutable()
                        Dots = dots.ToImmutable()
                    }


[<RequireQualifiedAccess>]
module Access =
    let parse: FSReader -> ParseResult<Access<SyntaxToken>, PositionedToken, ParseState> =
        nextSyntaxTokenSatisfiesLMsg
            (fun t ->
                t.Token = Token.KWPrivate
                || t.Token = Token.KWInternal
                || t.Token = Token.KWPublic
            )
            "Access modifier"
        |>> function
            | t when t.Token = Token.KWPrivate -> Access.Private t
            | t when t.Token = Token.KWInternal -> Access.Internal t
            | t when t.Token = Token.KWPublic -> Access.Public t
            | _ -> failwith "Unreachable"
