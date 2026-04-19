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
        || t = Token.CharLiteral

    let private pLiteral: Parser<_, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        nextNonTriviaTokenSatisfiesL
            (fun (synTok: SyntaxToken) -> isLiteralToken synTok.Token)
            "Expected constant literal"

    // Accept either standalone '>' or '>]' (KWRAttrBracket).
    // When '>]' is encountered, we split it: return a virtual '>' for the measure close
    // and set SplitRAttrBracket so the next token read yields ']' for the enclosing indexer.
    let private pMeasureRAngle =
        parser {
            let! state = getUserState

            match! peekNextNonTriviaToken with
            | t when t.Token = Token.OpGreaterThan && ParseState.tokenStringIs ">" t state -> return! consumePeeked t
            | t when t.Token = Token.KWRAttrBracket ->
                // Don't consume the >] token. Set the flag so the next read
                // rewrites it from KWRAttrBracket to KWRBracket (yielding `]`).
                do!
                    updateUserState (fun s ->
                        s.Trace.Invoke(TraceEvent.SplitRAttrBracketSet(t.StartIndex))
                        { s with SplitRAttrBracket = true }
                    )

                return virtualToken (PositionedToken.Create(Token.OpGreaterThan, t.StartIndex))
            | _ -> return! fail (Message "Expected '>' for measure")
        }

    let private pMeasure =
        // '<' must be truly adjacent to the preceding literal — verify previous raw token is not trivia,
        // then check raw token (no trivia skip) and verify string is exactly "<"
        parser {
            let! canBeMeasure = isPrevTokenNonTrivia >> Ok

            if canBeMeasure then
                let! state = getUserState
                let! pos = getPosition

                let! lAngle =
                    satisfyL
                        (fun (t: PositionedToken) ->
                            t.Token = Token.OpLessThan
                            && state.Lexed.GetTokenSpan(pos.Index * 1<token>, state.Input).SequenceEqual("<".AsSpan())
                        )
                        "Expected '<' for measure"

                let! m = Measure.parse
                let! rAngle = pMeasureRAngle

                return struct (syntaxToken lAngle pos.Index, m, rAngle)
            else
                return! fail (Message "Expected '<' for measure")
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
            | Measure.Power(inner, _, _, _) -> visit inner

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

                state <- ParseState.addDiagnostic code DiagnosticSeverity.Error tokStart tokEnd None state

            reader.State <- state

        preturn () reader

    let parse: Parser<Constant<_>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
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
