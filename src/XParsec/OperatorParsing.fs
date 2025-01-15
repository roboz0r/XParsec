module XParsec.OperatorPrecedence

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

    [<Literal>]
    let MinP = 0uy

[<NoEquality; NoComparison>]
type CloseOperator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice
    when 'Op: comparison and 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>> =
    internal
        {
            Op: 'Op
            ParseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice>
            ParseInnerExpr: Parser<'Index, 'T, 'State, 'Input, 'InputSlice> voption
        }

[<NoEquality; NoComparison>]
type Operator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice
    when 'Op: comparison and 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>> =
    internal
        {
            Op: 'Op
            ParseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice>
            LeftPower: byte
            RightPower: byte
            CloseOp: CloseOperator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice> voption
        }

type OperatorHandler<'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice
    when 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>> =
    abstract Infix: opInfix: 'T * lhs: 'Expr * rhs: 'Expr -> Parser<'Expr, 'T, 'State, 'Input, 'InputSlice>
    abstract Prefix: opPrefix: 'T * expr: 'Expr -> Parser<'Expr, 'T, 'State, 'Input, 'InputSlice>
    abstract Postfix: opPostfix: 'T * expr: 'Expr -> Parser<'Expr, 'T, 'State, 'Input, 'InputSlice>
    abstract Bracketed: opLeft: 'T * opRight: 'T * expr: 'Expr -> Parser<'Expr, 'T, 'State, 'Input, 'InputSlice>

    abstract Indexer:
        opLeft: 'T * opRight: 'T * lhs: 'Expr * index: 'Index -> Parser<'Expr, 'T, 'State, 'Input, 'InputSlice>

type Operators<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice
    when 'Op: comparison and 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>> =
    internal
        {
            LhsOperators: Map<'Op, Operator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>>
            RhsOperators: Map<'Op, Operator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>>
            LhsParser:
                Parser<
                    'T * Operator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>,
                    'T,
                    'State,
                    'Input,
                    'InputSlice
                 >
            RhsParser:
                Parser<
                    'T * Operator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>,
                    'T,
                    'State,
                    'Input,
                    'InputSlice
                 >
            Handler: OperatorHandler<'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>
        }


let private pOp
    (op: Operator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>)
    : Reader<_, _, _, _> -> ParseResult<('T * Operator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>), _, _> =
    tuple2 (lookAhead (Parsers.pid)) (op.ParseOp >>% op)

let private pCloseOp
    (op: CloseOperator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>)
    : Reader<_, _, _, _> -> ParseResult<('T * CloseOperator<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>), _, _> =
    tuple2 (lookAhead (Parsers.pid)) (op.ParseOp >>% op)

module Operator =
    [<Literal>]
    let MinP = Precedence.MinP

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
        let mutable lhsOps = Map.empty
        let mutable lhsParsers = []
        let mutable rhsOps = Map.empty
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
                rhsOps <- rhsOps.Add(op.Op, op)

                rhsParsers <- (pOp op) :: rhsParsers
            | Prefix op
            | Bracket op ->
                lhsOps <- lhsOps.Add(op.Op, op)

                lhsParsers <- (pOp op) :: lhsParsers
            | _ -> failwith $"Unrecognised operator {op}"

        {
            LhsOperators = lhsOps
            RhsOperators = rhsOps
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
    // Pratt parsing based on https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    [<Literal>]
    let MinP = Precedence.MinP

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

            let rhsTok, rhsOp = rhsSuccess.Parsed

            match rhsOp.RightPower, rhsOp.CloseOp with
            | MinP, ValueNone ->
                // Postfix operator
                if rhsOp.LeftPower < minBinding then
                    //Left Power < minBinding so return LHS
                    //This completes the parsing if at outer recursion
                    //Operator not consumed on stack so return previous reader
                    reader.Position <- pos
                    ParseSuccess.create lhs reader
                else

                    match ops.Handler.Postfix (rhsTok, lhs) reader with
                    | Ok lhs -> parseRhs lhs.Parsed reader
                    | Error e -> Error e
            | _, ValueNone ->
                //Infix Operator
                if rhsOp.LeftPower < minBinding then
                    reader.Position <- pos
                    ParseSuccess.create lhs reader
                else
                    // Found operator has higher binding than existing tokens so need to get next operator recursively
                    match parseLhs rhsOp.RightPower reader with
                    | Ok rhs ->
                        match ops.Handler.Infix (rhsTok, lhs, rhs.Parsed) reader with
                        | Ok lhs -> parseRhs lhs.Parsed reader
                        | Error e -> Error e
                    | Error e -> Error e
            | _, ValueSome(closeOp) ->
                match closeOp.ParseInnerExpr with
                | ValueSome pInner ->
                    //Indexer
                    if rhsOp.LeftPower < minBinding then
                        reader.Position <- pos
                        ParseSuccess.create lhs reader
                    else
                        match pInner (reader) with
                        | Ok inner ->
                            let pClose = pCloseOp closeOp

                            match pClose (reader) with
                            | Ok closeTok ->
                                match ops.Handler.Indexer (rhsTok, fst closeTok.Parsed, lhs, inner.Parsed) reader with
                                | Ok lhs -> parseRhs lhs.Parsed reader
                                | Error e -> Error e
                            | Error e -> Error e
                        | Error e -> Error e
                | ValueNone -> failwith "Not implemented"

        | Error _ ->
            // TODO: The error is discarded here which is usually correct, however,
            // in the case of a missing operator definition this would be a useful error to return.
            reader.Position <- pos
            ParseSuccess.create lhs reader

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
                let tok, op = pOp.Parsed

                match op.CloseOp with
                | ValueNone ->
                    // Prefix operators
                    match parseLhs op.RightPower (reader) with
                    | Ok rhs ->
                        match ops.Handler.Prefix (tok, rhs.Parsed) reader with
                        | Ok lhs -> parseRhs lhs.Parsed reader
                        | Error e -> Error e
                    | Error e -> Error e
                | ValueSome(closeOp) ->
                    let inline pClose reader = pCloseOp closeOp reader

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
                                let closeToken, closeOp = closeTok.Parsed

                                match ops.Handler.Bracketed (tok, closeToken, inner.Parsed) reader with
                                | Ok lhs -> parseRhs lhs.Parsed reader
                                | Error e -> Error e
                            | Error e -> Error e
                        | Error e -> Error e
            | Error e -> ParseError.createNestedP (Message "Operator parsing failed") [ e; e0 ] pos


    let parser
        (pExpr: Parser<'Expr, _, _, _, _>)
        (operators: Operators<'Op, 'Index, 'Expr, 'T, 'State, 'Input, 'InputSlice>)
        : Parser<'Expr, _, _, _, _> =
        parseLhs pExpr operators MinP
