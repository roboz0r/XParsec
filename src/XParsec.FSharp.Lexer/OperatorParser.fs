namespace XParsec.FSharp.Parser.OperatorParsing

open XParsec.FSharp.Lexer

open System
open System.Collections.Immutable
open XParsec

#nowarn "44" // Suppress warning for obsolete member usage

[<AutoOpen>]
module private Precedence =
    let MinP = 0uy

[<Struct; NoEquality; NoComparison>]
type OperatorLookup<'Key, 'Value when 'Key: equality> =
    internal
        {
            Ops: ImmutableArray<'Key>
            Operators: ImmutableArray<'Value>
        }

    member this.Item
        with get op =
            let rec f this i =
                if i >= this.Ops.Length then
                    invalidOp $"Operator {op} not found"
                elif this.Ops.[i] = op then
                    this.Operators.[i]
                else
                    f this (i + 1)

            f this 0


type RHSOperator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice
    when 'Op: equality and 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>> =
    internal
    | InfixLeft of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        leftPower: byte *
        completeInfix: ('Expr -> 'Op -> 'Expr -> 'Expr)

    | InfixRight of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        leftPower: byte *
        completeInfix: ('Expr -> 'Op -> 'Expr -> 'Expr)

    | InfixNonAssociative of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        leftPower: byte *
        completeInfix: ('Expr -> 'Op -> 'Expr -> 'Expr)

    | Postfix of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        leftPower: byte *
        completePostfix: ('Expr -> 'Op -> 'Expr)

    | Indexer of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        leftPower: byte *
        closeOp: 'Op *
        parseCloseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        parseInnerExpr: Parser<'Index, 'T, 'State, 'Input, 'InputSlice> *
        completeIndexer: ('Expr -> 'Op -> 'Index -> 'Expr)

    | Ternary of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        leftPower: byte *
        parseTernaryOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        completeTernary: ('Expr -> 'Op -> 'Expr -> 'Expr -> 'Expr)

type LHSOperator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice
    when 'Op: equality and 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>> =
    internal
    | Prefix of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        rightPower: byte *
        completePrefix: ('Op -> 'Expr -> 'Expr)

    | Enclosed of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        rightPower: byte *
        closeOp: 'Op *
        parseCloseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        complete: ('Op -> 'Expr -> 'Op -> 'Expr)


type Operator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice
    when 'Op: equality and 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>> =
    internal
    | RHS of RHSOperator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>
    | LHS of LHSOperator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>

/// A collection of operators used for parsing expressions.
/// It contains both left-hand side (LHS) and right-hand side (RHS) operators, along with their associated parsers.
/// Use `Operator.create` to create an instance of this type.
type Operators<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice
    when 'Op: equality and 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>> =
    abstract LhsParser:
        Parser<LHSOperator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>, 'T, 'State, 'Input, 'InputSlice>

    abstract RhsParser:
        Parser<RHSOperator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>, 'T, 'State, 'Input, 'InputSlice>


module internal Pratt =
    open Parsers
    // Pratt parsing based on https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

    let private failure = Message "Operator parsing failed"
    let private ambiguous = Message "Ambiguous operator associativity"

    let rec private parseRhs
        (pExpr: Parser<'Expr, 'T, _, _, _>)
        (ops: Operators<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>)
        minBinding
        lhs
        (reader: Reader<_, _, _, _>)
        : ParseResult<_, _, _> =

        let inline parseRhs lhs reader =
            parseRhs pExpr ops minBinding lhs reader

        let inline parseLhs minBinding reader = parseLhs pExpr ops minBinding reader

        let pos = reader.Position

        match ops.RhsParser reader with
        | Ok op ->
            let op = op.Parsed

            match op with
            | Postfix(op, parseOp, leftPower, completePostfix) ->
                if leftPower < minBinding then
                    //Left Power < minBinding so return LHS
                    //This completes the parsing if at outer recursion
                    //Operator not consumed on stack so return previous reader
                    reader.Position <- pos
                    preturn lhs reader
                elif leftPower = minBinding then
                    // Ambiguous case: both sides have the same binding power
                    fail ambiguous reader
                else
                    parseRhs (completePostfix lhs op) reader

            | InfixLeft(op, parseOp, leftPower, completeInfix) ->
                if leftPower < minBinding then
                    reader.Position <- pos
                    preturn lhs reader
                elif leftPower = minBinding then
                    fail ambiguous reader
                else
                    let rightPower = leftPower + 1uy
                    // Found operator has higher binding than existing tokens so need to get next operator recursively
                    match parseLhs rightPower reader with
                    | Ok rhs -> parseRhs (completeInfix lhs op rhs.Parsed) reader
                    | Error e -> Error e

            | InfixRight(op, parseOp, leftPower, completeInfix) ->
                if leftPower < minBinding then
                    reader.Position <- pos
                    preturn lhs reader
                elif leftPower = minBinding then
                    fail ambiguous reader
                else
                    let rightPower = leftPower - 1uy

                    match parseLhs rightPower reader with
                    | Ok rhs -> parseRhs (completeInfix lhs op rhs.Parsed) reader
                    | Error e -> Error e

            | InfixNonAssociative(op, parseOp, leftPower, completeInfix) ->
                if leftPower < minBinding then
                    reader.Position <- pos
                    preturn lhs reader
                elif leftPower = minBinding then
                    fail ambiguous reader
                else
                    let rightPower = leftPower
                    // Found operator has higher binding than existing tokens so need to get next operator recursively
                    match parseLhs rightPower reader with
                    | Ok rhs -> parseRhs (completeInfix lhs op rhs.Parsed) reader
                    | Error e -> Error e

            | Indexer(op, parseOp, leftPower, closeOp, parseCloseOp, innerParser, completeIndexer) ->
                if leftPower < minBinding then
                    reader.Position <- pos
                    preturn lhs reader
                elif leftPower = minBinding then
                    fail ambiguous reader
                else
                    match innerParser (reader) with
                    | Ok inner ->

                        match parseCloseOp (reader) with
                        | Ok closeTok -> parseRhs (completeIndexer lhs op inner.Parsed) reader
                        | Error e -> Error e
                    | Error e -> Error e

            | Ternary(op, parseOp, leftPower, parseTernaryOp, completeTernary) ->
                if leftPower < minBinding then
                    reader.Position <- pos
                    preturn lhs reader
                elif leftPower = minBinding then
                    fail ambiguous reader
                else
                    let rightPower = leftPower - 1uy

                    match parseLhs rightPower reader with
                    | Ok mid ->

                        match parseTernaryOp (reader) with
                        | Ok ternaryOp ->
                            match parseLhs MinP reader with
                            | Ok rhs -> parseRhs (completeTernary lhs op mid.Parsed rhs.Parsed) reader
                            | Error e -> Error e
                        | Error e -> Error e
                    | Error e -> Error e

        | Error _ ->
            // TODO: The error is discarded here which is usually correct, however,
            // in the case of a missing operator definition this would be a useful error to return.
            reader.Position <- pos
            preturn lhs reader

    and parseLhs
        (pExpr: Parser<'Expr, _, _, _, _>)
        (ops: Operators<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>)
        minBinding
        (reader: Reader<_, _, _, _>)
        : ParseResult<_, _, _> =

        let inline parseLhs minBinding reader = parseLhs pExpr ops minBinding reader

        let inline parseRhs lhs reader =
            parseRhs pExpr ops minBinding lhs reader

        // Get the first element
        // Should be any of Token, Open Bracket, Prefix Operator
        let pos = reader.Position

        match pExpr (reader) with
        | Ok lhsSuccess -> parseRhs lhsSuccess.Parsed reader
        | Error e0 ->
            reader.Position <- pos

            match ops.LhsParser(reader) with
            | Ok op ->
                let op = op.Parsed

                match op with
                | Prefix(op, parseOp, rightPower, completePrefix) ->
                    match parseLhs rightPower (reader) with
                    | Ok rhs -> parseRhs (completePrefix op rhs.Parsed) reader
                    | Error e -> Error e

                | Enclosed(op, parseOp, rightPower, closeOp, closeOpParser, completeBracket) ->
                    match parseLhs MinP (reader) with
                    | Ok inner ->
                        match closeOpParser (reader) with
                        | Ok closeTok ->
                            let closeOp = closeTok.Parsed
                            parseRhs (completeBracket op inner.Parsed closeOp) reader
                        | Error e -> Error e
                    | Error e -> Error e

            | Error e -> ParseError.createNested failure [ e; e0 ] pos

/// Provides functions to create and parse expressions using operators.
module Operator =

    type internal OperatorLookupBuilder<'Key, 'Value when 'Key: equality>() =
        let ops = ImmutableArray.CreateBuilder<'Key>()
        let operators = ImmutableArray.CreateBuilder<'Value>()

        member _.Add(op, operator) =
            ops.Add op
            operators.Add operator

        member _.ToOperatorLookup() =
            {
                Ops = ops.ToImmutable()
                Operators = operators.ToImmutable()
            }

    let private rhsOp =
        function
        | InfixLeft(op, _, _, _)
        | InfixRight(op, _, _, _)
        | InfixNonAssociative(op, _, _, _) -> op
        | Postfix(op, _, _, _)
        | Indexer(op, _, _, _, _, _, _)
        | Ternary(op, _, _, _, _) -> op

    let private rhsParseOp =
        function
        | InfixLeft(_, parseOp, _, _)
        | InfixRight(_, parseOp, _, _)
        | InfixNonAssociative(_, parseOp, _, _) -> parseOp
        | Postfix(_, parseOp, _, _)
        | Indexer(_, parseOp, _, _, _, _, _)
        | Ternary(_, parseOp, _, _, _) -> parseOp

    let private rhsLeftPower =
        function
        | InfixLeft(_, _, leftPower, _)
        | InfixRight(_, _, leftPower, _)
        | InfixNonAssociative(_, _, leftPower, _) -> leftPower
        | Postfix(_, _, leftPower, _)
        | Indexer(_, _, leftPower, _, _, _, _)
        | Ternary(_, _, leftPower, _, _) -> leftPower

    let private rhsRightPower =
        function
        | InfixLeft(_, _, leftPower, _) -> leftPower + 1uy
        | InfixRight(_, _, leftPower, _) -> leftPower - 1uy
        | InfixNonAssociative(_, _, leftPower, _) -> leftPower
        | Ternary(_, _, leftPower, _, _) -> leftPower - 1uy
        | Postfix(_, _, _, _)
        | Indexer(_, _, _, _, _, _, _) -> MinP

    let private lhsOp =
        function
        | Prefix(op, _, _, _)
        | Enclosed(op, _, _, _, _, _) -> op

    let private lhsParseOp =
        function
        | Prefix(_, parseOp, _, _)
        | Enclosed(_, parseOp, _, _, _, _) -> parseOp

    let private lhsRightPower =
        function
        | Prefix(_, _, rightPower, _)
        | Enclosed(_, _, rightPower, _, _, _) -> rightPower

    let private leftPower =
        function
        | RHS op -> rhsLeftPower op
        | LHS op -> MinP

    let private rightPower =
        function
        | RHS op -> rhsRightPower op
        | LHS op -> lhsRightPower op

    let private powerForSort op =
        ((int (leftPower op)) <<< 8) ||| (int (rightPower op))

    /// Creates an `Operators` instance from a collection of operators.
    /// The returned instance can be used to parse expressions using the defined operators.
    let create (ops: Operator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice> seq) =
        let lhsOps = OperatorLookupBuilder()
        let mutable lhsParsers = []
        let rhsOps = OperatorLookupBuilder()
        let mutable rhsParsers = []

        let ops = ops |> Seq.sortBy powerForSort

        for op in ops do
            match op with
            | RHS op ->
                rhsOps.Add(rhsOp op, op)
#if FABLE_COMPILER
                // Workaround for Fable compiler issue
                // https://github.com/fable-compiler/Fable/issues/4031
                // Fixed by https://github.com/fable-compiler/Fable/pull/4032 pending next release
                rhsParsers <- (fun reader -> (rhsParseOp op >>% op) reader) :: rhsParsers
#else
                rhsParsers <- (rhsParseOp op >>% op) :: rhsParsers
#endif

            | LHS op ->
                lhsOps.Add(lhsOp op, op)
#if FABLE_COMPILER
                lhsParsers <- (fun reader -> (lhsParseOp op >>% op) reader) :: lhsParsers
#else
                lhsParsers <- (lhsParseOp op >>% op) :: lhsParsers
#endif

        { new Operators<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice> with
            member _.LhsParser = choiceL lhsParsers "LHS did not match any known operator"
            member _.RhsParser = choiceL rhsParsers "RHS did not match any known operator"
        }

    /// Creates a left-associative infix operator with the specified properties.
    let infixLeftAssoc op (power: PrecedenceLevel) parseOp complete =
        let power = byte power * 2uy
        RHS(InfixLeft(op, parseOp, power, complete))

    /// Creates a right-associative infix operator with the specified properties.
    let infixRightAssoc op (power: PrecedenceLevel) parseOp complete =
        let power = byte power * 2uy
        RHS(InfixRight(op, parseOp, power + 1uy, complete))

    /// Creates a non-associative infix operator with the specified properties.
    let infixNonAssoc op (power: PrecedenceLevel) parseOp complete =
        let power = byte power * 2uy
        RHS(InfixNonAssociative(op, parseOp, power, complete))

    /// Creates a prefix operator with the specified properties.
    let prefix op (power: PrecedenceLevel) parseOp complete =
        let power = byte power * 2uy
        LHS(Prefix(op, parseOp, power, complete))

    /// Creates a postfix operator with the specified properties.
    let postfix op (power: PrecedenceLevel) parseOp complete =
        let power = byte power * 2uy
        RHS(Postfix(op, parseOp, power, complete))

    /// Creates an operator defining a pair of delimiters with the specified properties.
    let enclosedBy op closeOp (power: PrecedenceLevel) parseOp parseCloseOp complete =
        let power = byte power * 2uy
        LHS(Enclosed(op, parseOp, power, closeOp, parseCloseOp, complete))

    /// Creates an operator defining a pair of brackets with the specified properties.
    [<Obsolete("Use 'enclosedBy' for clarity.")>]
    let brackets op closeOp (power: PrecedenceLevel) parseOp parseCloseOp complete =
        enclosedBy op closeOp power parseOp parseCloseOp complete

    /// Creates an indexer operator with the specified properties.
    let indexer op closeOp (power: PrecedenceLevel) parseOp innerParser parseCloseOp complete =
        let power = byte power * 2uy
        RHS(Indexer(op, parseOp, power, closeOp, parseCloseOp, innerParser, complete))

    /// Creates a ternary operator with the specified properties.
    /// Ternary operators are always right associative.
    let ternary op (power: PrecedenceLevel) parseOp parseTernaryOp complete =
        let power = byte power * 2uy
        RHS(Ternary(op, parseOp, power + 1uy, parseTernaryOp, complete))

    /// Parses an expression using the provided expression parser and operator definitions.
    let parser
        (pExpr: Parser<'Expr, _, _, _, _>)
        (operators: Operators<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>)
        : Parser<'Expr, _, _, _, _> =
        Pratt.parseLhs pExpr operators MinP
