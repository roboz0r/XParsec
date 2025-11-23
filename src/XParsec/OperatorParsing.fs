namespace XParsec.OperatorParsing

open System
open System.Collections.Immutable
open XParsec

#nowarn "44" // Suppress warning for obsolete member usage

/// Operator precedence is used to determine the order of operations in expressions.
/// Operators with higher precedence are evaluated first, resulting in the higher value appearing in the inner expression.
type Precedence =
    | P1
    | P2
    | P3
    | P4
    | P5
    | P6
    | P7
    | P8
    | P9
    | P10
    | P11
    | P12
    | P13
    | P14
    | P15
    | P16
    | P17
    | P18
    | P19
    | P20
    | P21
    | P22
    | P23
    | P24
    | P25
    | P26
    | P27
    | P28
    | P29
    | P30

[<RequireQualifiedAccess>]
module internal Precedence =
    let value =
        function
        | P1 -> 1uy
        | P2 -> 3uy
        | P3 -> 5uy
        | P4 -> 7uy
        | P5 -> 9uy
        | P6 -> 11uy
        | P7 -> 13uy
        | P8 -> 15uy
        | P9 -> 17uy
        | P10 -> 19uy
        | P11 -> 21uy
        | P12 -> 23uy
        | P13 -> 25uy
        | P14 -> 27uy
        | P15 -> 29uy
        | P16 -> 31uy
        | P17 -> 33uy
        | P18 -> 35uy
        | P19 -> 37uy
        | P20 -> 39uy
        | P21 -> 41uy
        | P22 -> 43uy
        | P23 -> 45uy
        | P24 -> 47uy
        | P25 -> 49uy
        | P26 -> 51uy
        | P27 -> 53uy
        | P28 -> 55uy
        | P29 -> 57uy
        | P30 -> 59uy

    [<Literal>]
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
        completeInfix: ('Expr -> 'Expr -> 'Expr)

    | InfixRight of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        leftPower: byte *
        completeInfix: ('Expr -> 'Expr -> 'Expr)

    | InfixNonAssociative of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        leftPower: byte *
        completeInfix: ('Expr -> 'Expr -> 'Expr)

    /// Used for operators like the Tuple comma (,) which are technically "Not Associative"
    /// in type theory (creating a flat list rather than nested pairs) but strictly
    /// require chaining in the parser.
    | InfixNary of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        leftPower: byte *
        completeNary: (ResizeArray<'Expr> -> 'Expr)

    | Postfix of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        leftPower: byte *
        completePostfix: ('Expr -> 'Expr)

    | Indexer of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        leftPower: byte *
        closeOp: 'Op *
        parseCloseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        parseInnerExpr: Parser<'Index, 'T, 'State, 'Input, 'InputSlice> *
        completeIndexer: ('Expr -> 'Index -> 'Expr)

    | Ternary of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        leftPower: byte *
        parseTernaryOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        completeTernary: ('Expr -> 'Expr -> 'Expr -> 'Expr)

type LHSOperator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice
    when 'Op: equality and 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>> =
    internal
    | Prefix of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        rightPower: byte *
        completePrefix: ('Expr -> 'Expr)

    | Enclosed of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        rightPower: byte *
        closeOp: 'Op *
        parseCloseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        complete: ('Expr -> 'Expr)


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
    internal
        {
            LhsOperators: OperatorLookup<'Op, LHSOperator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>>
            RhsOperators: OperatorLookup<'Op, RHSOperator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>>
            LhsParser:
                Parser<LHSOperator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>, 'T, 'State, 'Input, 'InputSlice>
            RhsParser:
                Parser<RHSOperator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>, 'T, 'State, 'Input, 'InputSlice>
        }


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

            match op.Parsed with
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
                    parseRhs (completePostfix lhs) reader

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
                    | Ok rhs -> parseRhs (completeInfix lhs rhs.Parsed) reader
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
                    | Ok rhs -> parseRhs (completeInfix lhs rhs.Parsed) reader
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
                    | Ok rhs -> parseRhs (completeInfix lhs rhs.Parsed) reader
                    | Error e -> Error e

            | InfixNary(op, parseOp, leftPower, completeNary) ->
                if leftPower < minBinding then
                    reader.Position <- pos
                    preturn lhs reader
                elif leftPower = minBinding then
                    fail ambiguous reader
                else
                    // Loop to consume all chained n-ary operators and expressions (e.g. commas in a tuple)
                    let rec loopNary (items: ResizeArray<_>) =
                        // Bind tighter (leftPower + 1) to parse the next expression.
                        // If the inner `parseLhs` encounters another comma (precedence = leftPower),
                        // it will see (leftPower < minBinding), stop, and return the expression.
                        // This allows us to catch the comma here in the loop.
                        let rightPower = leftPower + 1uy

                        match parseLhs rightPower reader with
                        | Error e -> Error e
                        | Ok { Parsed = next } ->
                            items.Add next

                            let nextPos = reader.Position

                            // Peek/Consume the next operator using the `RhsParser` to check the next token
                            // We only want to continue if it is the SAME operator (e.g. another comma).
                            match ops.RhsParser reader with
                            | Ok { Parsed = nextOp } ->
                                match nextOp with
                                | InfixNary(nextSym, _, _, _) when nextSym = op -> loopNary items
                                | _ ->
                                    // Found a different operator (e.g. ')' or '+').
                                    // Backtrack so the outer recursive call or caller can handle it.
                                    reader.Position <- nextPos
                                    preturn items reader
                            | Error _ ->
                                // EOF or no operator found. Backtrack and finish.
                                reader.Position <- nextPos
                                preturn items reader

                    let items = ResizeArray()
                    items.Add lhs

                    match loopNary items with
                    | Ok { Parsed = items } -> parseRhs (completeNary items) reader
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
                        | Ok closeTok -> parseRhs (completeIndexer lhs inner.Parsed) reader
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
                            match parseLhs Precedence.MinP reader with
                            | Ok rhs -> parseRhs (completeTernary lhs mid.Parsed rhs.Parsed) reader
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

                match op.Parsed with
                | Prefix(op, parseOp, rightPower, completePrefix) ->
                    match parseLhs rightPower (reader) with
                    | Ok rhs -> parseRhs (completePrefix rhs.Parsed) reader
                    | Error e -> Error e

                | Enclosed(op, parseOp, rightPower, closeOp, closeOpParser, completeBracket) ->
                    match parseLhs Precedence.MinP (reader) with
                    | Ok inner ->
                        match closeOpParser (reader) with
                        | Ok closeTok ->
                            let closeOp = closeTok.Parsed
                            parseRhs (completeBracket inner.Parsed) reader
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
        | InfixNonAssociative(op, _, _, _)
        | InfixNary(op, _, _, _)
        | Postfix(op, _, _, _)
        | Indexer(op, _, _, _, _, _, _)
        | Ternary(op, _, _, _, _) -> op

    let private rhsParseOp =
        function
        | InfixLeft(_, parseOp, _, _)
        | InfixRight(_, parseOp, _, _)
        | InfixNonAssociative(_, parseOp, _, _)
        | InfixNary(_, parseOp, _, _) -> parseOp
        | Postfix(_, parseOp, _, _)
        | Indexer(_, parseOp, _, _, _, _, _)
        | Ternary(_, parseOp, _, _, _) -> parseOp

    let private rhsLeftPower =
        function
        | InfixLeft(_, _, leftPower, _)
        | InfixRight(_, _, leftPower, _)
        | InfixNonAssociative(_, _, leftPower, _)
        | InfixNary(_, _, leftPower, _)
        | Postfix(_, _, leftPower, _)
        | Indexer(_, _, leftPower, _, _, _, _)
        | Ternary(_, _, leftPower, _, _) -> leftPower

    let private rhsRightPower =
        function
        | InfixLeft(_, _, leftPower, _) -> leftPower + 1uy
        | InfixRight(_, _, leftPower, _) -> leftPower - 1uy
        | InfixNonAssociative(_, _, leftPower, _) -> leftPower
        | InfixNary(_, _, leftPower, _) -> leftPower
        | Ternary(_, _, leftPower, _, _) -> leftPower - 1uy
        | Postfix(_, _, _, _)
        | Indexer(_, _, _, _, _, _, _) -> Precedence.MinP

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
        | LHS op -> Precedence.MinP

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

        {
            LhsOperators = lhsOps.ToOperatorLookup()
            RhsOperators = rhsOps.ToOperatorLookup()
            LhsParser = choiceL lhsParsers "LHS did not match any known operator"
            RhsParser = choiceL rhsParsers "RHS did not match any known operator"
        }

    /// Creates a left-associative infix operator with the specified properties.
    let infixLeftAssoc op power parseOp complete =
        let power = Precedence.value power
        RHS(InfixLeft(op, parseOp, power, complete))

    /// Creates a right-associative infix operator with the specified properties.
    let infixRightAssoc op power parseOp complete =
        let power = Precedence.value power
        RHS(InfixRight(op, parseOp, power + 1uy, complete))

    /// Creates a non-associative infix operator with the specified properties.
    let infixNonAssoc op power parseOp complete =
        let power = Precedence.value power
        RHS(InfixNonAssociative(op, parseOp, power, complete))

    /// Creates an n-ary infix operator with the specified properties.
    /// N-ary operators are similar to left-associative operators but allow chaining without ambiguity.
    let infixNary op power parseOp complete =
        let power = Precedence.value power
        RHS(InfixNary(op, parseOp, power, complete))

    /// Creates a prefix operator with the specified properties.
    let prefix op power parseOp complete =
        let power = Precedence.value power
        LHS(Prefix(op, parseOp, power, complete))

    /// Creates a postfix operator with the specified properties.
    let postfix op power parseOp complete =
        let power = Precedence.value power
        RHS(Postfix(op, parseOp, power, complete))

    /// Creates an operator defining a pair of delimiters with the specified properties.
    let enclosedBy op closeOp power parseOp parseCloseOp complete =
        let power = Precedence.value power
        LHS(Enclosed(op, parseOp, power, closeOp, parseCloseOp, complete))

    /// Creates an operator defining a pair of brackets with the specified properties.
    [<Obsolete("Use 'enclosedBy' for clarity.")>]
    let brackets op closeOp power parseOp parseCloseOp complete =
        enclosedBy op closeOp power parseOp parseCloseOp complete

    /// Creates an indexer operator with the specified properties.
    let indexer op closeOp power parseOp innerParser parseCloseOp complete =
        let power = Precedence.value power
        RHS(Indexer(op, parseOp, power, closeOp, parseCloseOp, innerParser, complete))

    /// Creates a ternary operator with the specified properties.
    /// Ternary operators are always right associative.
    let ternary op power parseOp parseTernaryOp complete =
        let power = Precedence.value power
        RHS(Ternary(op, parseOp, power + 1uy, parseTernaryOp, complete))

    /// Parses an expression using the provided expression parser and operator definitions.
    let parser
        (pExpr: Parser<'Expr, _, _, _, _>)
        (operators: Operators<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>)
        : Parser<'Expr, _, _, _, _> =
        Pratt.parseLhs pExpr operators Precedence.MinP
