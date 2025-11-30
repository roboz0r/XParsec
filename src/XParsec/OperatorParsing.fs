namespace XParsec.OperatorParsing

open System
open System.Collections.Immutable
open System.Collections.Generic
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

/// A unit of measure representing binding power for operators.
/// Binding power is used in Pratt parsing to determine the precedence and associativity of operators.
/// Higher binding power indicates higher precedence.
/// By convention, binding powers are odd numbers N with the next higher even number indicating the operator associativity.
/// A left-associative operator with binding power N has left power N and right power N + 1.
/// A right-associative operator with binding power N has left power N + 1 and right power N.
/// A non-associative operator with binding power N has both left and right power N.
[<Measure>]
type bp

/// Binding power is used in Pratt parsing to determine the precedence and associativity of operators.
/// Higher binding power indicates higher precedence.
/// By convention, binding powers are odd numbers N with the next higher even number indicating the operator associativity.
/// A left-associative operator with binding power N has left power N and right power N + 1.
/// A right-associative operator with binding power N has left power N + 1 and right power N.
/// A non-associative operator with binding power N has both left and right power N.
module BindingPower =
    /// Converts a raw precedence level (0-126) into a Base Binding Power (Odd Number).
    /// Level 0 -> 1<bp>, Level 1 -> 3<bp>, Level 10 -> 21<bp>.
    let fromLevel (level: int) : byte<bp> =
        let oddBase = (byte level * 2uy) + 1uy
        LanguagePrimitives.ByteWithMeasure oddBase

    /// Calculates the recursion power for a Left Associative operator.
    /// (LBP = N, RBP = N + 1)
    let leftAssocRhs (basePower: byte<bp>) = basePower + 1uy<bp>

    /// Calculates the LHS power for a Right Associative operator.
    /// (LBP = N + 1, RBP = N)
    let rightAssocLhs (basePower: byte<bp>) = basePower + 1uy<bp>

    let internal rightAssocRhs (leftPower: byte<bp>) = leftPower - 1uy<bp>

[<RequireQualifiedAccess>]
module internal Precedence =
    let bindingPower =
        function
        | P1 -> 1uy<bp>
        | P2 -> 3uy<bp>
        | P3 -> 5uy<bp>
        | P4 -> 7uy<bp>
        | P5 -> 9uy<bp>
        | P6 -> 11uy<bp>
        | P7 -> 13uy<bp>
        | P8 -> 15uy<bp>
        | P9 -> 17uy<bp>
        | P10 -> 19uy<bp>
        | P11 -> 21uy<bp>
        | P12 -> 23uy<bp>
        | P13 -> 25uy<bp>
        | P14 -> 27uy<bp>
        | P15 -> 29uy<bp>
        | P16 -> 31uy<bp>
        | P17 -> 33uy<bp>
        | P18 -> 35uy<bp>
        | P19 -> 37uy<bp>
        | P20 -> 39uy<bp>
        | P21 -> 41uy<bp>
        | P22 -> 43uy<bp>
        | P23 -> 45uy<bp>
        | P24 -> 47uy<bp>
        | P25 -> 49uy<bp>
        | P26 -> 51uy<bp>
        | P27 -> 53uy<bp>
        | P28 -> 55uy<bp>
        | P29 -> 57uy<bp>
        | P30 -> 59uy<bp>

    [<Literal>]
    let MinP = 0uy<bp>

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


type RHSOperator<'Op, 'Aux, 'Expr, 'T, 'State, 'Input, 'InputSlice
    when 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>> =
    | InfixLeft of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        leftPower: byte<bp> *
        completeInfix: ('Expr -> 'Op -> 'Expr -> 'Expr)

    | InfixRight of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        leftPower: byte<bp> *
        completeInfix: ('Expr -> 'Op -> 'Expr -> 'Expr)

    | InfixNonAssociative of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        leftPower: byte<bp> *
        completeInfix: ('Expr -> 'Op -> 'Expr -> 'Expr)

    /// Used for operators like the Tuple comma (,) which are technically "Not Associative"
    /// in type theory (creating a flat list rather than nested pairs) but strictly
    /// require chaining in the parser.
    | InfixNary of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        leftPower: byte<bp> *
        completeNary: (ResizeArray<'Expr> -> ResizeArray<'Op> -> 'Expr)

    /// A generalized infix operator where the Right-Hand Side is NOT necessarily an expression.
    /// It uses a specific parser `parseRight` and maps the result into the 'Expr.
    /// Used for Member Access (.), Type Checks (is/as), or rigid syntax.
    | InfixMapped of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        leftPower: byte<bp> *
        parseRight: Parser<'Aux, 'T, 'State, 'Input, 'InputSlice> *  // <--- Custom Parser
        complete: ('Expr -> 'Op -> 'Aux -> 'Expr)

    | Postfix of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        leftPower: byte<bp> *
        completePostfix: ('Expr -> 'Op -> 'Expr)

    | Indexer of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        leftPower: byte<bp> *
        closeOp: 'Op *
        parseCloseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        parseInnerExpr: Parser<'Aux, 'T, 'State, 'Input, 'InputSlice> *
        completeIndexer: ('Expr -> 'Op -> 'Aux -> 'Op -> 'Expr)

    | Ternary of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        leftPower: byte<bp> *
        parseTernaryOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        completeTernary: ('Expr -> 'Op -> 'Expr -> 'Op -> 'Expr -> 'Expr)

type LHSOperator<'Op, 'Aux, 'Expr, 'T, 'State, 'Input, 'InputSlice
    when 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>> =
    | Prefix of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        rightPower: byte<bp> *
        completePrefix: ('Op -> 'Expr -> 'Expr)

    | Enclosed of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        rightPower: byte<bp> *
        closeOp: 'Op *
        parseCloseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        complete: ('Op -> 'Expr -> 'Op -> 'Expr)

    /// Used for control flow constructs like "if <expr> then <expr>" or "while <expr> do <expr>"
    | LHSTernary of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        rightPower: byte<bp> *  // Binding power for the 'condition' (middle expr)
        delimiter: 'Op *
        parseDelimiter: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        complete: ('Op -> 'Expr -> 'Op -> 'Expr -> 'Expr)


type Operator<'Op, 'Aux, 'Expr, 'T, 'State, 'Input, 'InputSlice
    when 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>> =
    | RHS of RHSOperator<'Op, 'Aux, 'Expr, 'T, 'State, 'Input, 'InputSlice>
    | LHS of LHSOperator<'Op, 'Aux, 'Expr, 'T, 'State, 'Input, 'InputSlice>

/// A collection of operators used for parsing expressions.
/// It contains both left-hand side (LHS) and right-hand side (RHS) operators, along with their associated parsers.
/// Use `Operator.create` to create an instance of this type.
type Operators<'Op, 'Aux, 'Expr, 'T, 'State, 'Input, 'InputSlice
    when 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>> =
    abstract LhsParser:
        Parser<LHSOperator<'Op, 'Aux, 'Expr, 'T, 'State, 'Input, 'InputSlice>, 'T, 'State, 'Input, 'InputSlice>

    abstract RhsParser:
        Parser<RHSOperator<'Op, 'Aux, 'Expr, 'T, 'State, 'Input, 'InputSlice>, 'T, 'State, 'Input, 'InputSlice>

    abstract OpComparer: IEqualityComparer<'Op>

type OperatorsCollection<'Op, 'Aux, 'Expr, 'T, 'State, 'Input, 'InputSlice
    when 'Op: equality and 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>> =
    internal
        {
            LhsOperators: OperatorLookup<'Op, LHSOperator<'Op, 'Aux, 'Expr, 'T, 'State, 'Input, 'InputSlice>>
            RhsOperators: OperatorLookup<'Op, RHSOperator<'Op, 'Aux, 'Expr, 'T, 'State, 'Input, 'InputSlice>>
            LhsParserImpl:
                Parser<LHSOperator<'Op, 'Aux, 'Expr, 'T, 'State, 'Input, 'InputSlice>, 'T, 'State, 'Input, 'InputSlice>
            RhsParserImpl:
                Parser<RHSOperator<'Op, 'Aux, 'Expr, 'T, 'State, 'Input, 'InputSlice>, 'T, 'State, 'Input, 'InputSlice>
        }

    interface Operators<'Op, 'Aux, 'Expr, 'T, 'State, 'Input, 'InputSlice> with
        member this.LhsParser = this.LhsParserImpl
        member this.RhsParser = this.RhsParserImpl
        member this.OpComparer = EqualityComparer<'Op>.Default


module internal Pratt =
    open Parsers
    // Pratt parsing based on https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

    let private failure = Message "Operator parsing failed"
    let private ambiguous = Message "Ambiguous operator associativity"

    let rec private parseRhs
        (pExpr: Parser<'Expr, 'T, _, _, _>)
        (ops: Operators<'Op, 'Aux, 'Expr, 'T, 'State, 'Input, 'InputSlice>)
        minBinding
        lhs
        (reader: Reader<_, _, _, _>)
        : ParseResult<_, _, _> =

        let inline parseRhs lhs reader =
            parseRhs pExpr ops minBinding lhs reader

        let inline parseLhs minBinding reader = parseLhs pExpr ops minBinding reader

        let pos = reader.Position

        match ops.RhsParser reader with
        | Ok { Parsed = op } ->

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
                    let rightPower = leftPower + 1uy<bp>
                    // Found operator has higher binding than existing tokens so need to get next operator recursively
                    match parseLhs rightPower reader with
                    | Ok { Parsed = rhs } -> parseRhs (completeInfix lhs op rhs) reader
                    | Error e -> Error e

            | InfixRight(op, parseOp, leftPower, completeInfix) ->
                if leftPower < minBinding then
                    reader.Position <- pos
                    preturn lhs reader
                elif leftPower = minBinding then
                    fail ambiguous reader
                else
                    let rightPower = leftPower - 1uy<bp>

                    match parseLhs rightPower reader with
                    | Ok { Parsed = rhs } -> parseRhs (completeInfix lhs op rhs) reader
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
                    | Ok { Parsed = rhs } -> parseRhs (completeInfix lhs op rhs) reader
                    | Error e -> Error e

            | InfixNary(op, parseOp, leftPower, completeNary) ->
                if leftPower < minBinding then
                    reader.Position <- pos
                    preturn lhs reader
                elif leftPower = minBinding then
                    fail ambiguous reader
                else
                    // Loop to consume all chained n-ary operators and expressions (e.g. commas in a tuple)
                    let rec loopNary (items: ResizeArray<'Expr>) (parsedOps: ResizeArray<'Op>) =
                        // Bind tighter (leftPower + 1) to parse the next expression.
                        // If the inner `parseLhs` encounters another comma (precedence = leftPower),
                        // it will see (leftPower < minBinding), stop, and return the expression.
                        // This allows us to catch the comma here in the loop.
                        let rightPower = leftPower + 1uy<bp>

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
                                | InfixNary(nextSym, _, _, _) when ops.OpComparer.Equals(nextSym, op) ->
                                    parsedOps.Add nextSym
                                    loopNary items parsedOps
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
                    let parsedOps = ResizeArray()
                    items.Add lhs
                    parsedOps.Add op

                    match loopNary items parsedOps with
                    | Ok { Parsed = items } -> parseRhs (completeNary items parsedOps) reader
                    | Error e -> Error e

            | InfixMapped(op, parseOp, leftPower, parseRight, complete) ->
                if leftPower < minBinding then
                    reader.Position <- pos
                    preturn lhs reader
                elif leftPower = minBinding then
                    fail ambiguous reader
                else
                    match parseRight reader with
                    | Ok { Parsed = rightContent } ->
                        // Resume Pratt parsing loop with the combined result
                        parseRhs (complete lhs op rightContent) reader
                    | Error e -> Error e

            | Indexer(op, parseOp, leftPower, closeOp, parseCloseOp, innerParser, completeIndexer) ->
                if leftPower < minBinding then
                    reader.Position <- pos
                    preturn lhs reader
                elif leftPower = minBinding then
                    fail ambiguous reader
                else
                    match innerParser reader with
                    | Ok { Parsed = inner } ->

                        match parseCloseOp reader with
                        | Ok { Parsed = closeTok } -> parseRhs (completeIndexer lhs op inner closeTok) reader
                        | Error e -> Error e
                    | Error e -> Error e

            | Ternary(op, parseOp, leftPower, parseTernaryOp, completeTernary) ->
                if leftPower < minBinding then
                    reader.Position <- pos
                    preturn lhs reader
                elif leftPower = minBinding then
                    fail ambiguous reader
                else
                    let rightPower = leftPower - 1uy<bp>

                    match parseLhs rightPower reader with
                    | Ok { Parsed = mid } ->

                        match parseTernaryOp reader with
                        | Ok { Parsed = ternaryOp } ->
                            match parseLhs Precedence.MinP reader with
                            | Ok { Parsed = rhs } -> parseRhs (completeTernary lhs op mid ternaryOp rhs) reader
                            | Error e -> Error e
                        | Error e ->
                            let expectedMsg =
                                { e with
                                    Errors = Message "Expected close ternary operator"
                                }

                            ParseError.createNested failure [ expectedMsg; e ] e.Position
                    | Error e -> Error e

        | Error _ ->
            // TODO: The error is discarded here which is usually correct, however,
            // in the case of a missing operator definition this would be a useful error to return.
            reader.Position <- pos
            preturn lhs reader

    and parseLhs
        (pExpr: Parser<'Expr, _, _, _, _>)
        (ops: Operators<'Op, 'Aux, 'Expr, 'T, 'State, 'Input, 'InputSlice>)
        minBinding
        (reader: Reader<_, _, _, _>)
        : ParseResult<_, _, _> =

        let inline parseLhs minBinding reader = parseLhs pExpr ops minBinding reader

        let inline parseRhs lhs reader =
            parseRhs pExpr ops minBinding lhs reader

        // Get the first element
        // Should be any of Token, Open Bracket, Prefix Operator
        let pos = reader.Position

        // 1. Try to parse an atomic expression (Integer, Identifier, etc.)
        match pExpr (reader) with
        | Ok { Parsed = lhsSuccess } -> parseRhs lhsSuccess reader
        | Error e0 ->
            reader.Position <- pos

            // 2. If atom failed, try to match an LHS Operator (Prefix, Enclosed, LHSTernary)
            match ops.LhsParser(reader) with
            | Ok { Parsed = op } ->

                match op with
                | Prefix(op, parseOp, rightPower, completePrefix) ->
                    match parseLhs rightPower (reader) with
                    | Ok { Parsed = rhs } -> parseRhs (completePrefix op rhs) reader
                    | Error e -> Error e

                | Enclosed(op, parseOp, rightPower, closeOp, closeOpParser, completeBracket) ->
                    match parseLhs Precedence.MinP (reader) with
                    | Ok { Parsed = inner } ->
                        match closeOpParser reader with
                        | Ok { Parsed = closeTok } -> parseRhs (completeBracket op inner closeTok) reader
                        | Error e ->
                            let expectedMsg =
                                { e with
                                    Errors = Message $"Expected closing operator '{closeOp}'"
                                }

                            ParseError.createNested failure [ expectedMsg; e ] e.Position
                    | Error e -> Error e

                | LHSTernary(op, parseOp, rightPower, delim, parseDelim, complete) ->
                    // 1. Parse the "Condition" using the specified binding power
                    match parseLhs rightPower reader with
                    | Ok { Parsed = condition } ->

                        // 2. Expect the delimiter (e.g., "then", "do", "in")
                        match parseDelim reader with
                        | Ok { Parsed = delimTok } ->

                            // 3. Parse the "Body" (usually with Min Precedence)
                            match parseLhs Precedence.MinP reader with
                            | Ok { Parsed = body } -> parseRhs (complete op condition delimTok body) reader
                            | Error e -> Error e

                        | Error e ->
                            let expectedMsg =
                                { e with
                                    Errors = Message $"Expected delimiter '{delim}'"
                                }

                            ParseError.createNested failure [ expectedMsg; e ] e.Position
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
        | InfixMapped(op, _, _, _, _)
        | Postfix(op, _, _, _)
        | Indexer(op, _, _, _, _, _, _)
        | Ternary(op, _, _, _, _) -> op

    let private rhsParseOp =
        function
        | InfixLeft(_, parseOp, _, _)
        | InfixRight(_, parseOp, _, _)
        | InfixNonAssociative(_, parseOp, _, _)
        | InfixNary(_, parseOp, _, _)
        | InfixMapped(_, parseOp, _, _, _)
        | Postfix(_, parseOp, _, _)
        | Indexer(_, parseOp, _, _, _, _, _)
        | Ternary(_, parseOp, _, _, _) -> parseOp

    let private rhsLeftPower =
        function
        | InfixLeft(_, _, leftPower, _)
        | InfixRight(_, _, leftPower, _)
        | InfixNonAssociative(_, _, leftPower, _)
        | InfixNary(_, _, leftPower, _)
        | InfixMapped(_, _, leftPower, _, _)
        | Postfix(_, _, leftPower, _)
        | Indexer(_, _, leftPower, _, _, _, _)
        | Ternary(_, _, leftPower, _, _) -> leftPower

    let private rhsRightPower =
        function
        | InfixLeft(_, _, leftPower, _) -> BindingPower.leftAssocRhs leftPower
        | InfixRight(_, _, leftPower, _) -> BindingPower.rightAssocRhs leftPower
        | InfixNonAssociative(_, _, leftPower, _) -> leftPower
        | InfixNary(_, _, leftPower, _) -> leftPower
        | InfixMapped(_, _, leftPower, _, _) -> BindingPower.leftAssocRhs leftPower
        | Ternary(_, _, leftPower, _, _) -> BindingPower.rightAssocRhs leftPower
        | Postfix(_, _, _, _)
        | Indexer(_, _, _, _, _, _, _) -> Precedence.MinP

    let private lhsOp =
        function
        | Prefix(op, _, _, _)
        | LHSTernary(op, _, _, _, _, _)
        | Enclosed(op, _, _, _, _, _) -> op

    let private lhsParseOp =
        function
        | Prefix(_, parseOp, _, _)
        | LHSTernary(_, parseOp, _, _, _, _)
        | Enclosed(_, parseOp, _, _, _, _) -> parseOp

    let private lhsRightPower =
        function
        | Prefix(_, _, rightPower, _)
        | LHSTernary(_, _, rightPower, _, _, _)
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
    let create (ops: Operator<'Op, 'Aux, 'Expr, 'T, 'State, 'Input, 'InputSlice> seq) =
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
            LhsParserImpl = choiceL lhsParsers "LHS did not match any known operator"
            RhsParserImpl = choiceL rhsParsers "RHS did not match any known operator"
        }
        :> Operators<_, _, _, _, _, _, _>

    /// Creates a left-associative infix operator with the specified properties.
    let infixLeftAssoc op precedence parseOp complete =
        let power = Precedence.bindingPower precedence
        RHS(InfixLeft(op, parseOp, power, complete))

    /// Creates a right-associative infix operator with the specified properties.
    let infixRightAssoc op precedence parseOp complete =
        let power = Precedence.bindingPower precedence
        RHS(InfixRight(op, parseOp, BindingPower.rightAssocLhs power, complete))

    /// Creates a non-associative infix operator with the specified properties.
    let infixNonAssoc op precedence parseOp complete =
        let power = Precedence.bindingPower precedence
        RHS(InfixNonAssociative(op, parseOp, power, complete))

    /// Creates an n-ary infix operator with the specified properties.
    /// N-ary operators are similar to left-associative operators but allow chaining without ambiguity.
    let infixNary op precedence parseOp complete =
        let power = Precedence.bindingPower precedence
        RHS(InfixNary(op, parseOp, power, complete))

    /// Creates a prefix operator with the specified properties.
    let prefix op precedence parseOp complete =
        let power = Precedence.bindingPower precedence
        LHS(Prefix(op, parseOp, power, complete))

    /// Creates a postfix operator with the specified properties.
    let postfix op precedence parseOp complete =
        let power = Precedence.bindingPower precedence
        RHS(Postfix(op, parseOp, power, complete))

    /// Creates an operator defining a pair of delimiters with the specified properties.
    let enclosedBy op closeOp precedence parseOp parseCloseOp complete =
        let power = Precedence.bindingPower precedence
        LHS(Enclosed(op, parseOp, power, closeOp, parseCloseOp, complete))

    /// Creates an operator defining a pair of brackets with the specified properties.
    [<Obsolete("Use 'enclosedBy' for clarity.")>]
    let brackets op closeOp precedence parseOp parseCloseOp complete =
        enclosedBy op closeOp precedence parseOp parseCloseOp complete

    /// Creates an indexer operator with the specified properties.
    let indexer op closeOp precedence parseOp innerParser parseCloseOp complete =
        let power = Precedence.bindingPower precedence
        RHS(Indexer(op, parseOp, power, closeOp, parseCloseOp, innerParser, complete))

    /// Creates a ternary operator with the specified properties.
    /// Ternary operators are always right associative.
    let ternary op precedence parseOp parseTernaryOp complete =
        let power = Precedence.bindingPower precedence
        RHS(Ternary(op, parseOp, BindingPower.rightAssocLhs power, parseTernaryOp, complete))

    /// Creates a left-hand side ternary operator with the specified properties.
    let lhsTernary op precedence parseOp delimiter parseDelimiter complete =
        let power = Precedence.bindingPower precedence
        LHS(LHSTernary(op, parseOp, power, delimiter, parseDelimiter, complete))

    let infixMapped op precedence parseOp parseRight complete =
        let power = Precedence.bindingPower precedence
        RHS(InfixMapped(op, parseOp, power, parseRight, complete))

    /// Parses an expression using the provided expression parser and operator definitions.
    let parser
        (pExpr: Parser<'Expr, _, _, _, _>)
        (operators: Operators<'Op, 'Aux, 'Expr, 'T, 'State, 'Input, 'InputSlice>)
        : Parser<'Expr, _, _, _, _> =
        Pratt.parseLhs pExpr operators Precedence.MinP

    /// Parses an expression starting at a specific minimum precedence.
    /// Use this when parsing expressions inside delimiters (like lists or array indices)
    /// where you want to stop parsing before consuming the delimiter.
    let parserAt
        (minBindingPower: byte<bp>)
        (pExpr: Parser<'Expr, _, _, _, _>)
        (operators: Operators<'Op, 'Aux, 'Expr, 'T, 'State, 'Input, 'InputSlice>)
        : Parser<'Expr, _, _, _, _> =
        Pratt.parseLhs pExpr operators minBindingPower
