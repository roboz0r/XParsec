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

    let parse: Parser<AttributeTarget<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>> =
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
    let parse: Parser<Attribute<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>> =
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

    let parse: Parser<AttributeSet<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>> =
        parser {
            let! lBracket = pLAttrBracket
            let! attributes = pAttributeItems
            let! rBracket = pRAttrBracket
            return AttributeSet(lBracket, attributes, rBracket)
        }

[<RequireQualifiedAccess>]
module Attributes =
    let parse: Parser<Attributes<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>> =
        many1 AttributeSet.parse

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

    let parse: Parser<RangeOpName<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>> =
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

    let parse: Parser<ActivePatternOpName<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>> =
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

    let parse: Parser<OpName<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>> =
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

    let private pStarOpDecl =
        nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.KWOpDeclareMultiply) "Expected '(*)'"

    let parse: Parser<IdentOrOp<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>> =
        choiceL
            [
                // Case 1: Simple Identifier (including backticked)
                pIdentOrOpIdent |>> IdentOrOp.Ident

                // Case 2: Star Operator (*)
                // Subcase 2a: Parsed as a single token KWOpDeclareMultiply `(*)`
                (parser {
                    let! token = pStarOpDecl
                    // Synthesize virtual tokens to match the AST requirement of ( * )
                    let lParen =
                        { token with
                            Index = TokenIndex.Virtual
                            PositionedToken = PositionedToken.Create(Token.KWLParen, token.StartIndex)
                        }

                    let star =
                        { token with
                            Index = TokenIndex.Virtual
                            PositionedToken = PositionedToken.Create(Token.OpMultiply, token.StartIndex + 1)
                        }

                    let rParen =
                        { token with
                            Index = TokenIndex.Virtual
                            PositionedToken = PositionedToken.Create(Token.KWRParen, token.StartIndex + 2)
                        }

                    return IdentOrOp.StarOp(lParen, star, rParen)
                })
                // Subcase 2b: Parsed as separate tokens ( * )
                <|> (parser {
                    let! l = pLParen
                    let! s = pOpMultiply
                    let! r = pRParen
                    return IdentOrOp.StarOp(l, s, r)
                })

                // Case 3: Parenthesized Operator (op) or Active Pattern (| ... |)
                <|> (parser {
                    let! l = pLParen
                    let! op = OpName.parse
                    let! r = pRParen
                    return IdentOrOp.ParenOp(l, op, r)
                })
            ]
            "IdentOrOp"

[<RequireQualifiedAccess>]
module LongIdentOrOp =
    // Could be LongIdent or QualifiedOp
    let rec private parseRest ident (builder: ImmutableArray<_>.Builder) =
        parser {
            // Look ahead for a dot
            match! opt (lookAhead pDot) with
            | ValueSome dot ->
                // Consume the dot
                let! dotConsumed = pDot

                // Parse the next IdentOrOp
                let! nextIdentOrOp = IdentOrOp.parse

                match nextIdentOrOp with
                | IdentOrOp.Ident identNext ->
                    // Continue parsing the LongIdent
                    builder.Add(identNext)
                    return! parseRest ident builder
                | _ ->
                    // Found an operator, return QualifiedOp
                    let longIdent = builder.ToImmutable()
                    return LongIdentOrOp.QualifiedOp(longIdent, dotConsumed, nextIdentOrOp)
            | ValueNone ->
                // No more dots, return LongIdent
                let longIdent = builder.ToImmutable()
                return LongIdentOrOp.LongIdent longIdent
        }

    let parse: Parser<LongIdentOrOp<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>> =
        parser {
            let! first = IdentOrOp.parse

            match first with
            | IdentOrOp.Ident ident ->
                let builder = ImmutableArray.CreateBuilder()
                builder.Add(ident)
                return! parseRest ident builder
            | _ ->
                // Just an operator
                return LongIdentOrOp.Op first
        }


[<RequireQualifiedAccess>]
module LongIdent =
    // Simple parser for A.B.C
    let private pIdent = nextSyntaxIdentifierLMsg "Expected Identifier"
    let parse = sepBy1 pIdent pDot |>> fun struct (xs, dots) -> xs


[<RequireQualifiedAccess>]
module Access =
    let parse
        : Reader<PositionedToken, ParseState, ReadableImmutableArray<PositionedToken>>
              -> ParseResult<Access<SyntaxToken>, PositionedToken, ParseState> =
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
