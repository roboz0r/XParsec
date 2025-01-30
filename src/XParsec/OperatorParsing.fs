module XParsec.OperatorParsing

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

[<NoEquality; NoComparison>]
type CloseOperator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice
    when 'Op: equality and 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>> =
    internal
        {
            Op: 'Op
            ParseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice>
            ParseInnerExpr: Parser<'Index, 'T, 'State, 'Input, 'InputSlice> voption
        }

[<NoEquality; NoComparison>]
type Operator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice
    when 'Op: equality and 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>> =
    internal
        {
            Op: 'Op
            ParseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice>
            LeftPower: byte
            RightPower: byte
            CloseOp: CloseOperator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice> voption
        }

type OperatorHandler<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice
    when 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>> =
    abstract Infix: opInfix: 'Op * lhs: 'Expr * rhs: 'Expr -> Parser<'Expr, 'T, 'State, 'Input, 'InputSlice>
    abstract Prefix: opPrefix: 'Op * expr: 'Expr -> Parser<'Expr, 'T, 'State, 'Input, 'InputSlice>
    abstract Postfix: opPostfix: 'Op * expr: 'Expr -> Parser<'Expr, 'T, 'State, 'Input, 'InputSlice>
    abstract Bracketed: opLeft: 'Op * opRight: 'Op * expr: 'Expr -> Parser<'Expr, 'T, 'State, 'Input, 'InputSlice>

    abstract Indexer:
        opLeft: 'Op * opRight: 'Op * lhs: 'Expr * index: 'Index -> Parser<'Expr, 'T, 'State, 'Input, 'InputSlice>

[<Struct>]
type OperatorLookup<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice
    when 'Op: equality and 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>> =
    internal
        {
            Ops: 'Op array
            Operators: Operator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice> array
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


type internal OperatorLookupBuilder<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice
    when 'Op: equality and 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>>() =

    let ops = ResizeArray<_>()

    let operators =
        ResizeArray<Operator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>>()

    member this.Add operator =
        operators.Add operator
        ops.Add operator.Op

    member this.ToOperatorLookup() =
        {
            Ops = ops.ToArray()
            Operators = operators.ToArray()
        }


type Operators<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice
    when 'Op: equality and 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>> =
    internal
        {
            LhsOperators: OperatorLookup<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>
            RhsOperators: OperatorLookup<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>
            LhsParser:
                Parser<Operator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>, 'T, 'State, 'Input, 'InputSlice>
            RhsParser:
                Parser<Operator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>, 'T, 'State, 'Input, 'InputSlice>
            Handler: OperatorHandler<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>
        }

module Operator =
    [<Literal>]
    let private MinP = Precedence.MinP

    let private (|Infix|_|) op =
        if op.LeftPower > MinP && op.RightPower > MinP && op.CloseOp.IsNone then
            Some op
        else
            None

    let private (|Prefix|_|) op =
        if op.LeftPower = MinP && op.RightPower > MinP && op.CloseOp.IsNone then
            Some op
        else
            None

    let private (|Postfix|_|) op =
        if op.LeftPower > MinP && op.RightPower = MinP && op.CloseOp.IsNone then
            Some op
        else
            None

    let private (|Bracket|_|) op =
        if op.LeftPower = MinP && op.RightPower > MinP && op.CloseOp.IsSome then
            Some op
        else
            None

    let private (|Indexer|_|) op =
        if op.LeftPower > MinP && op.RightPower = MinP && op.CloseOp.IsSome then
            Some op
        else
            None

    let create handler (ops: Operator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice> seq) =
        let lhsOps = OperatorLookupBuilder()
        let mutable lhsParsers = []
        let rhsOps = OperatorLookupBuilder()
        let mutable rhsParsers = []

        let ops =
            ops
            |> Seq.sortBy (fun op -> (int op.LeftPower <<< 8) &&& (int op.RightPower))
            |> Array.ofSeq

        for op in ops do
            match op with
            | Infix op
            | Postfix op
            | Indexer op ->
                rhsOps.Add(op)

                rhsParsers <- (op.ParseOp >>% op) :: rhsParsers
            | Prefix op
            | Bracket op ->
                lhsOps.Add(op)

                lhsParsers <- (op.ParseOp >>% op) :: lhsParsers
            | _ -> failwith $"Unrecognised operator {op}"

        {
            LhsOperators = lhsOps.ToOperatorLookup()
            RhsOperators = rhsOps.ToOperatorLookup()
            LhsParser = fun x -> choiceL lhsParsers "LHS did not match any known operator" x
            RhsParser = fun x -> choiceL rhsParsers "RHS did not match any known operator" x
            Handler = handler
        }

    let infixLeftAssoc op power parseOp =
        let power = Precedence.value power

        {
            Op = op
            ParseOp = parseOp
            LeftPower = power
            RightPower = power + 1uy
            CloseOp = ValueNone
        }

    let infixRightAssoc op power parseOp =
        let power = Precedence.value power

        {
            Op = op
            ParseOp = parseOp
            LeftPower = power + 1uy
            RightPower = power
            CloseOp = ValueNone
        }

    let prefix op power parseOp =
        let power = Precedence.value power

        {
            Op = op
            ParseOp = parseOp
            LeftPower = MinP
            RightPower = power
            CloseOp = ValueNone
        }

    let postfix op power parseOp =
        let power = Precedence.value power

        {
            Op = op
            ParseOp = parseOp
            LeftPower = power
            RightPower = MinP
            CloseOp = ValueNone
        }

    let brackets op closeOp power parseOp parseCloseOp =
        let power = Precedence.value power

        {
            Op = op
            ParseOp = parseOp
            LeftPower = MinP
            RightPower = power
            CloseOp =
                ValueSome
                    {
                        Op = closeOp
                        ParseOp = parseCloseOp
                        ParseInnerExpr = ValueNone
                    }
        }

    let indexer op closeOp innerParser power parseOp parseCloseOp =
        let power = Precedence.value power

        {
            Op = op
            ParseOp = parseOp
            LeftPower = power
            RightPower = MinP
            CloseOp =
                ValueSome
                    {
                        Op = closeOp
                        ParseOp = parseCloseOp
                        ParseInnerExpr = ValueSome innerParser
                    }
        }


module Pratt =
    open Parsers
    // Pratt parsing based on https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    [<Literal>]
    let MinP = Precedence.MinP

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
        | Ok rhsSuccess ->

            let rhsOp = rhsSuccess.Parsed

            match rhsOp.RightPower, rhsOp.CloseOp with
            | MinP, ValueNone ->
                // Postfix operator
                if rhsOp.LeftPower < minBinding then
                    //Left Power < minBinding so return LHS
                    //This completes the parsing if at outer recursion
                    //Operator not consumed on stack so return previous reader
                    reader.Position <- pos
                    preturn lhs reader
                else

                    match ops.Handler.Postfix (rhsOp.Op, lhs) reader with
                    | Ok lhs -> parseRhs lhs.Parsed reader
                    | Error e -> Error e
            | _, ValueNone ->
                //Infix Operator
                if rhsOp.LeftPower < minBinding then
                    reader.Position <- pos
                    preturn lhs reader
                else
                    // Found operator has higher binding than existing tokens so need to get next operator recursively
                    match parseLhs rhsOp.RightPower reader with
                    | Ok rhs ->
                        match ops.Handler.Infix (rhsOp.Op, lhs, rhs.Parsed) reader with
                        | Ok lhs -> parseRhs lhs.Parsed reader
                        | Error e -> Error e
                    | Error e -> Error e
            | _, ValueSome(closeOp) ->
                match closeOp.ParseInnerExpr with
                | ValueSome pInner ->
                    //Indexer
                    if rhsOp.LeftPower < minBinding then
                        reader.Position <- pos
                        preturn lhs reader
                    else
                        match pInner (reader) with
                        | Ok inner ->
                            let pClose = closeOp.ParseOp

                            match pClose (reader) with
                            | Ok closeTok ->
                                match ops.Handler.Indexer (rhsOp.Op, closeTok.Parsed, lhs, inner.Parsed) reader with
                                | Ok lhs -> parseRhs lhs.Parsed reader
                                | Error e -> Error e
                            | Error e -> Error e
                        | Error e -> Error e
                | ValueNone -> failwith "Not implemented"

        | Error _ ->
            // TODO: The error is discarded here which is usually correct, however,
            // in the case of a missing operator definition this would be a useful error to return.
            reader.Position <- pos
            preturn lhs reader

    and private parseLhs
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
            | Ok pOp ->
                let op = pOp.Parsed

                match op.CloseOp with
                | ValueNone ->
                    // Prefix operators
                    match parseLhs op.RightPower (reader) with
                    | Ok rhs ->
                        match ops.Handler.Prefix (op.Op, rhs.Parsed) reader with
                        | Ok lhs -> parseRhs lhs.Parsed reader
                        | Error e -> Error e
                    | Error e -> Error e
                | ValueSome(closeOp) ->
                    let pClose = closeOp.ParseOp

                    match closeOp.ParseInnerExpr with
                    | ValueSome pInner ->
                        // Special Bracket. Does this make sense?
                        match pInner (reader) with
                        | Ok inner ->
                            match pClose (reader) with
                            | Ok closeTok -> failwith "Not implemented"
                            | Error e -> Error e
                        | Error e -> Error e
                    | ValueNone ->
                        // Brackets
                        match parseLhs MinP (reader) with
                        | Ok inner ->
                            match pClose (reader) with
                            | Ok closeTok ->
                                let closeOp = closeTok.Parsed

                                match ops.Handler.Bracketed (op.Op, closeOp, inner.Parsed) reader with
                                | Ok lhs -> parseRhs lhs.Parsed reader
                                | Error e -> Error e
                            | Error e -> Error e
                        | Error e -> Error e
            | Error e -> ParseError.createNested failure [ e; e0 ] pos


    let parser
        (pExpr: Parser<'Expr, _, _, _, _>)
        (operators: Operators<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>)
        : Parser<'Expr, _, _, _, _> =
        parseLhs pExpr operators MinP
