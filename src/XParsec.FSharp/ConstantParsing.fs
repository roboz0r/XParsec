namespace XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.Parsers
open XParsec.OperatorParsing
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser.SyntaxToken

[<RequireQualifiedAccess>]
module Constant =
    let isLiteralToken (t: Token) =

        t.IsNumeric
        || t = Token.KWTrue
        || t = Token.KWFalse
        || t = Token.KWNull
        || t = Token.StringLiteral
        || t = Token.String3Literal
        || t = Token.VerbatimStringLiteral
        || t = Token.ByteArrayLiteral
        || t = Token.VerbatimByteArrayLiteral

    let private pLiteral: Parser<_, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        nextNonTriviaTokenSatisfiesL
            (fun (synTok: SyntaxToken) -> isLiteralToken synTok.Token)
            "Expected constant literal"

    let private pMeasure =
        parser {
            let! pos = getPosition

            // '<' must be immediately after literal
            let! lAngle = satisfyL (fun (t: PositionedToken) -> t.Token = Token.OpLessThan) "Expected '<' for measure"

            let! m = Measure.parse
            let! rAngle = nextNonTriviaTokenIsL (Token.OpGreaterThan) "Expected '>' for measure"
            return struct (syntaxToken lAngle pos.Index, m, rAngle)
        }

    /// <summary>
    /// Traverses the measure AST and collects all Type Variable nodes (e.g. 'u).
    /// Used to validate that a measure literal (e.g. float<...>) does not contain type variables.
    /// </summary>
    let collectTypars (measure: Measure<'T>) : ResizeArray<Typar<'T>> =
        let results = ResizeArray()

        let rec visit m =
            match m with
            // The Target: Found a type variable
            | Measure.Typar token -> results.Add(token)

            // Base cases: Clean
            | Measure.Named _
            | Measure.One _
            | Measure.Anonymous _ -> ()

            // Unary recursive cases
            | Measure.Paren(_, inner, _)
            | Measure.Reciprocal(_, inner)
            | Measure.Power(inner, _, _) -> visit inner

            // Binary recursive cases
            | Measure.Product(l, _, r)
            | Measure.Quotient(l, _, r) ->
                visit l
                visit r

            // N-ary recursive case
            | Measure.Juxtaposition(ms, ops) ->
                for m in ms do
                    visit m

        visit measure
        results

    let validateMeasureNoTypars (measure: Measure<SyntaxToken>) (reader: Reader<_, _, _, _>) =
        let typars = collectTypars measure

        if typars.Count > 0 then
            let mutable state: ParseState = reader.State

            for t in typars do
                let code = DiagnosticCode.TyparInConstant t

                let tokStart, tokEnd =
                    match t with
                    | Typar.Anon x -> x.PositionedToken, None
                    | Typar.Named(x, x1) -> x.PositionedToken, Some x1.PositionedToken
                    | Typar.Static(x, x1) -> x.PositionedToken, Some x1.PositionedToken

                state <- ParseState.addDiagnostic code DiagnosticSeverity.Error tokStart tokEnd state

            reader.State <- state

        preturn () reader

    let pUnit: Parser<Constant<_>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! lParen = pLParen
            let! rParen = pRParen
            return Constant.Unit(lParen, rParen)
        }

    let private pOtherValues =
        parser {

            let! literal = pLiteral

            if literal.Token.IsNumeric then
                let! measure = opt pMeasure

                match measure with
                | ValueSome(lAngle, m, rAngle) ->
                    do! validateMeasureNoTypars m
                    return Constant.MeasuredLiteral(literal, lAngle, m, rAngle)
                | ValueNone -> return Constant.Literal literal
            else
                return Constant.Literal literal
        }

    let parse: Parser<Constant<_>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        dispatchNextNonTriviaTokenFallback [ Token.KWLParen, pUnit ] pOtherValues
