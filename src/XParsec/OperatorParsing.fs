namespace XParsec.OperatorParsing

open System
open System.Collections.Immutable
open XParsec

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
    | Infix of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        leftPower: byte *
        rightPower: byte *
        completeInfix: ('Expr -> 'Expr -> 'Expr)

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

    member internal this.Op =
        match this with
        | Infix(op, _, _, _, _)
        | Postfix(op, _, _, _)
        | Indexer(op, _, _, _, _, _, _) -> op

    member internal this.ParseOp =
        match this with
        | Infix(_, parseOp, _, _, _)
        | Postfix(_, parseOp, _, _)
        | Indexer(_, parseOp, _, _, _, _, _) -> parseOp

    member internal this.LeftPower =
        match this with
        | Infix(_, _, leftPower, _, _)
        | Postfix(_, _, leftPower, _)
        | Indexer(_, _, leftPower, _, _, _, _) -> leftPower

    member internal this.RightPower =
        match this with
        | Infix(_, _, _, rightPower, _) -> rightPower
        | Postfix(_, _, _, _)
        | Indexer(_, _, _, _, _, _, _) -> Precedence.MinP

type LHSOperator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice
    when 'Op: equality and 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>> =
    internal
    | Prefix of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        rightPower: byte *
        completePrefix: ('Expr -> 'Expr)
    | Brackets of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        rightPower: byte *
        closeOp: 'Op *
        parseCloseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        completeBracket: ('Expr -> 'Expr)

    member internal this.Op =
        match this with
        | Prefix(op, _, _, _)
        | Brackets(op, _, _, _, _, _) -> op

    member internal this.ParseOp =
        match this with
        | Prefix(_, parseOp, _, _)
        | Brackets(_, parseOp, _, _, _, _) -> parseOp

    member internal this.LeftPower = Precedence.MinP

    member internal this.RightPower =
        match this with
        | Prefix(_, _, rightPower, _)
        | Brackets(_, _, rightPower, _, _, _) -> rightPower


type Operator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice
    when 'Op: equality and 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>> =
    internal
    | RHS of RHSOperator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>
    | LHS of LHSOperator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>

    member internal this.Op =
        match this with
        | RHS op -> op.Op
        | LHS op -> op.Op

    member internal this.LeftPower =
        match this with
        | RHS op -> op.LeftPower
        | LHS op -> op.LeftPower

    member internal this.RightPower =
        match this with
        | RHS op -> op.RightPower
        | LHS op -> op.RightPower

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
                else
                    parseRhs (completePostfix lhs) reader
            | Infix(op, parseOp, leftPower, rightPower, completeInfix) ->
                if leftPower < minBinding then
                    reader.Position <- pos
                    preturn lhs reader
                else
                    // Found operator has higher binding than existing tokens so need to get next operator recursively
                    match parseLhs rightPower reader with
                    | Ok rhs -> parseRhs (completeInfix lhs rhs.Parsed) reader
                    | Error e -> Error e
            | Indexer(op, parseOp, leftPower, closeOp, parseCloseOp, innerParser, completeIndexer) ->
                if leftPower < minBinding then
                    reader.Position <- pos
                    preturn lhs reader
                else
                    match innerParser (reader) with
                    | Ok inner ->

                        match parseCloseOp (reader) with
                        | Ok closeTok -> parseRhs (completeIndexer lhs inner.Parsed) reader
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

                | Brackets(op, parseOp, rightPower, closeOp, closeOpParser, completeBracket) ->
                    match parseLhs Precedence.MinP (reader) with
                    | Ok inner ->
                        match closeOpParser (reader) with
                        | Ok closeTok ->
                            let closeOp = closeTok.Parsed
                            parseRhs (completeBracket inner.Parsed) reader
                        | Error e -> Error e
                    | Error e -> Error e

            | Error e -> ParseError.createNested failure [ e; e0 ] pos

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

    let create (ops: Operator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice> seq) =
        let lhsOps = OperatorLookupBuilder()
        let mutable lhsParsers = []
        let rhsOps = OperatorLookupBuilder()
        let mutable rhsParsers = []

        let ops =
            ops |> Seq.sortBy (fun op -> ((int op.LeftPower) <<< 8) &&& (int op.RightPower))

        for op in ops do
            match op with
            | RHS op ->
                rhsOps.Add(op.Op, op)
#if FABLE_COMPILER
                // Workaround for Fable compiler issue
                // https://github.com/fable-compiler/Fable/issues/4031
                rhsParsers <- (fun reader -> (op.ParseOp >>% op) reader) :: rhsParsers
#else
                rhsParsers <- (op.ParseOp >>% op) :: rhsParsers
#endif

            | LHS op ->
                lhsOps.Add(op.Op, op)
#if FABLE_COMPILER
                lhsParsers <- (fun reader -> (op.ParseOp >>% op) reader) :: lhsParsers
#else
                lhsParsers <- (op.ParseOp >>% op) :: lhsParsers
#endif

        {
            LhsOperators = lhsOps.ToOperatorLookup()
            RhsOperators = rhsOps.ToOperatorLookup()
            LhsParser = choiceL lhsParsers "LHS did not match any known operator"
            RhsParser = choiceL rhsParsers "RHS did not match any known operator"
        }

    let infixLeftAssoc op power parseOp complete =
        let power = Precedence.value power
        RHS(Infix(op, parseOp, power, power + 1uy, complete))

    let infixRightAssoc op power parseOp complete =
        let power = Precedence.value power
        RHS(Infix(op, parseOp, power + 1uy, power, complete))

    let prefix op power parseOp complete =
        let power = Precedence.value power
        LHS(Prefix(op, parseOp, power, complete))

    let postfix op power parseOp complete =
        let power = Precedence.value power
        RHS(Postfix(op, parseOp, power, complete))

    let brackets op closeOp power parseOp parseCloseOp complete =
        let power = Precedence.value power
        LHS(Brackets(op, parseOp, power, closeOp, parseCloseOp, complete))

    let indexer op closeOp innerParser power parseOp parseCloseOp complete =
        let power = Precedence.value power
        RHS(Indexer(op, parseOp, power, closeOp, parseCloseOp, innerParser, complete))

    let parser
        (pExpr: Parser<'Expr, _, _, _, _>)
        (operators: Operators<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>)
        : Parser<'Expr, _, _, _, _> =
        Pratt.parseLhs pExpr operators Precedence.MinP
