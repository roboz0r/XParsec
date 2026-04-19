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
#if FABLE_COMPILER
        oddBase * 1uy<bp>
#else
        LanguagePrimitives.ByteWithMeasure oddBase
#endif

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
        allowTrailingOp: bool *
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

    /// A prefix operator whose right-hand side is fully handled by a custom parser rather than
    /// by the standard Pratt parsing mechanism. Used where the body parser controls its own
    /// structure and context.
    | PrefixMapped of
        op: 'Op *
        parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice> *
        parseRight: Parser<'Aux, 'T, 'State, 'Input, 'InputSlice> *
        complete: ('Op -> 'Aux -> 'Expr)


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


module internal rec Pratt =
    open Parsers
    // Pratt parsing based on https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

    let private failure = Message "Operator parsing failed"
    let private ambiguous = Message "Ambiguous operator associativity"

    module ParseError =
        let inline createNested error children position : ParseError<'T, 'State> =
            {
                Position = position
                Errors = Nested(error, children)
            }

    [<Struct>]
    type internal PrattParsed<'Expr, 'T, 'State> =
        {
            Expr: 'Expr
            Error: ParseError<'T, 'State> voption
        }

    module PrattParsed =
        let inline success expr : PrattParsed<'Expr, 'T, 'State> = { Expr = expr; Error = ValueNone }

        let inline withError expr error : PrattParsed<'Expr, 'T, 'State> = { Expr = expr; Error = error }

    // Combines two optional soft errors. Typically, you want to nest them
    // or pick the furthest one depending on your exact ParseError design.
    // Cases ordered hot-path-first: the steady-state success path is
    // (ValueNone, ValueNone); the allocating nested cases are cold.
    let inline private mergeSoftErrors
        (e1: ParseError<'T, 'State> voption)
        (e2: ParseError<'T, 'State> voption)
        : ParseError<'T, 'State> voption =
        match e1, e2 with
        | ValueNone, ValueNone -> ValueNone
        | _, ValueNone -> e1
        | ValueNone, _ -> e2
        | ValueSome {
                        Position = aPos
                        Errors = Nested(aParent, aErrs)
                    },
          ValueSome {
                        Position = bPos
                        Errors = Nested(bParent, bErrs)
                    } when aParent = failure && bParent = failure ->
            ValueSome(ParseError.createNested failure [ yield! bErrs; yield! aErrs ] bPos)
        | ValueSome {
                        Position = aPos
                        Errors = Nested(aParent, aErrs)
                    },
          ValueSome b when aParent = failure -> ValueSome(ParseError.createNested failure [ b; yield! aErrs ] aPos)
        | ValueSome a,
          ValueSome {
                        Position = bPos
                        Errors = Nested(bParent, bErrs)
                    } when bParent = failure -> ValueSome(ParseError.createNested failure [ yield! bErrs; a ] bPos)
        | ValueSome a, ValueSome b ->
            // Assuming a nested structure is desired when combining soft errors
            ValueSome(ParseError.createNested failure [ b; a ] b.Position)

    // Injects a preceding soft error into a hard parser failure
    let private mergeWithError (hardErr: ParseError<'T, 'State>) (softErr: _ voption) =
        match hardErr, softErr with
        | {
              Position = hPos
              Errors = Nested(hParent, hErrors)
          },
          ValueSome {
                        Position = sPos
                        Errors = Nested(sParent, sErrors)
                    } when sParent = failure && hParent = failure ->
            ParseError.createNested failure [ yield! hErrors; yield! sErrors ] hPos
        | {
              Position = hPos
              Errors = Nested(hParent, hErrors)
          },
          ValueSome s when hParent = failure -> ParseError.createNested failure [ yield! hErrors; s ] hPos
        | hardErr,
          ValueSome {
                        Position = sPos
                        Errors = Nested(sParent, sErrors)
                    } when sParent = failure ->
            ParseError.createNested failure [ hardErr; yield! sErrors ] hardErr.Position
        | hardErr, ValueSome s -> ParseError.createNested failure [ hardErr; s ] hardErr.Position
        | hardErr, ValueNone -> hardErr

    let inline private ensureAdvanced (pos: Position<'State>) (reader: Reader<_, _, _, _>) : unit =
        if reader.Index = pos.Index then
            raise (InfiniteLoopException(pos))

    // ── RHS operator case helpers ──────────────────────────────────────
    // Each case is a separate function so the JIT allocates stack only for
    // the locals of the branch actually taken, rather than for all branches.

    let inline private rhsInfix
        pExpr
        ops
        minBinding
        lhs
        (errAcc: ParseError<'T, 'State> voption)
        pos
        rightPower
        op
        completeInfix
        (reader: Reader<_, _, _, _>)
        =
        match parseLhsInternal pExpr ops rightPower reader with
        | Ok { Expr = rhs; Error = errRhs } ->
            ensureAdvanced pos reader
            let combinedErr = mergeSoftErrors errAcc errRhs
            parseRhsInternal pExpr ops minBinding (completeInfix lhs op rhs) combinedErr reader
        | Error e -> Error(mergeWithError e errAcc)

    let private rhsInfixNary
        pExpr
        (ops: Operators<'Op, 'Aux, 'Expr, 'T, 'State, 'Input, 'InputSlice>)
        minBinding
        lhs
        leftPower
        op
        (parseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice>)
        allowTrailingOp
        completeNary
        (reader: Reader<_, _, _, _>)
        =
        let rec loopNary (items: ResizeArray<'Expr>) (parsedOps: ResizeArray<'Op>) accumulatedErr =
            let rightPower = leftPower + 1uy<bp>
            let nextItemPos = reader.Position

            match parseLhsInternal pExpr ops rightPower reader with // Parse item
            | Error e when allowTrailingOp ->
                reader.Position <- nextItemPos

                if parsedOps.Count > 0 then
                    parsedOps.RemoveAt(parsedOps.Count - 1)

                preturn (items, ValueSome(mergeWithError e accumulatedErr)) reader
            | Error e -> Error(mergeWithError e accumulatedErr)
            | Ok { Expr = next; Error = errNext } ->
                ensureAdvanced nextItemPos reader // Parsed item without consuming input - likely an infinite loop, so throw
                items.Add next
                let currentErr = mergeSoftErrors accumulatedErr errNext
                let nextOpPos = reader.Position

                // Use the operator-specific parseOp to detect a subsequent occurrence
                // of the same operator. Avoids re-dispatching through the full RhsParser
                // (and the associated `OpComparer` equality check) on every iteration.
                match parseOp reader with
                | Ok nextSym ->
                    parsedOps.Add nextSym
                    loopNary items parsedOps currentErr
                | Error errOp ->
                    reader.Position <- nextOpPos
                    preturn (items, mergeSoftErrors currentErr (ValueSome errOp)) reader

        // Typical nary use — tuples, app chains, sequences — rarely exceeds 4 items.
        // Hint an initial capacity of 4 so `Add lhs` + up to three more adds don't
        // trigger the first grow allocation. For sequences with many elements, the
        // usual doubling growth still kicks in.
        let items = ResizeArray(4)
        let parsedOps = ResizeArray(4)
        items.Add lhs
        parsedOps.Add op
        let entryPos = reader.Position

        match loopNary items parsedOps ValueNone with
        | Ok(items, errOpt) ->
            let result = completeNary items parsedOps

            if reader.Position = entryPos then
                // No progress was made (e.g. trailing separator with no following item).
                // Return directly to prevent infinite recursion when virtual tokens
                // can repeatedly fire at the same position.
                preturn (PrattParsed.withError result errOpt) reader
            else
                match parseRhsInternal pExpr ops minBinding result errOpt reader with
                | Ok { Expr = finalExpr; Error = errFinal } ->
                    preturn (PrattParsed.withError finalExpr (mergeSoftErrors errOpt errFinal)) reader
                | Error e -> Error(mergeWithError e errOpt)
        | Error e -> Error e

    let private rhsInfixMapped
        pExpr
        ops
        minBinding
        lhs
        (errAcc: ParseError<'T, 'State> voption)
        pos
        op
        (parseRight: Parser<_, 'T, 'State, 'Input, 'InputSlice>)
        complete
        (reader: Reader<_, _, _, _>)
        =
        match parseRight reader with
        | Ok rightContent ->
            ensureAdvanced pos reader
            parseRhsInternal pExpr ops minBinding (complete lhs op rightContent) errAcc reader
        | Error e -> Error(mergeWithError e errAcc)

    let private rhsIndexer
        pExpr
        ops
        minBinding
        lhs
        (errAcc: ParseError<'T, 'State> voption)
        pos
        op
        (parseCloseOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice>)
        (innerParser: Parser<_, 'T, 'State, 'Input, 'InputSlice>)
        completeIndexer
        (reader: Reader<_, _, _, _>)
        =
        match innerParser reader with
        | Ok inner ->
            ensureAdvanced pos reader

            match parseCloseOp reader with
            | Ok closeTok ->
                ensureAdvanced pos reader
                parseRhsInternal pExpr ops minBinding (completeIndexer lhs op inner closeTok) errAcc reader
            | Error e -> Error(mergeWithError e errAcc)
        | Error e -> Error(mergeWithError e errAcc)

    let private rhsTernary
        pExpr
        ops
        minBinding
        lhs
        (errAcc: ParseError<'T, 'State> voption)
        pos
        op
        rightPower
        (parseTernaryOp: Parser<'Op, 'T, 'State, 'Input, 'InputSlice>)
        completeTernary
        (reader: Reader<_, _, _, _>)
        =
        match parseLhsInternal pExpr ops rightPower reader with
        | Ok { Expr = mid; Error = errMid } ->
            match parseTernaryOp reader with
            | Ok ternaryOp ->
                match parseLhsInternal pExpr ops rightPower reader with
                | Ok { Expr = rhs; Error = errRhs } ->
                    ensureAdvanced pos reader
                    let combinedErr = mergeSoftErrors errAcc errRhs
                    parseRhsInternal pExpr ops minBinding (completeTernary lhs op mid ternaryOp rhs) combinedErr reader
                | Error e -> Error(mergeWithError e errAcc)
            | Error e ->
                let expectedMsg =
                    { e with
                        Errors = Message "Expected close ternary operator"
                    }

                let hard = ParseError.createNested failure [ expectedMsg; e ] e.Position
                Error(mergeWithError hard errMid)
        | Error e -> Error e

    // ── LHS operator case helpers ──────────────────────────────────────

    let private lhsPrefix pExpr ops minBinding op rightPower completePrefix (reader: Reader<_, _, _, _>) =
        match parseLhsInternal pExpr ops rightPower reader with
        | Ok { Expr = rhs; Error = errOpt } ->
            match parseRhsInternal pExpr ops minBinding (completePrefix op rhs) errOpt reader with
            | Ok { Expr = finalExpr; Error = errFinal } ->
                preturn (PrattParsed.withError finalExpr (mergeSoftErrors errOpt errFinal)) reader
            | Error e -> Error(mergeWithError e errOpt)
        | Error e -> Error e

    let private lhsEnclosed
        pExpr
        ops
        minBinding
        op
        (closeOp: 'Op)
        (closeOpParser: Parser<'Op, 'T, 'State, 'Input, 'InputSlice>)
        completeBracket
        (reader: Reader<_, _, _, _>)
        =
        match parseLhsInternal pExpr ops Precedence.MinP reader with
        | Ok { Expr = inner; Error = errOpt } ->
            match closeOpParser reader with
            | Ok closeTok ->
                match parseRhsInternal pExpr ops minBinding (completeBracket op inner closeTok) errOpt reader with
                | Ok { Expr = finalExpr; Error = errFinal } ->
                    preturn (PrattParsed.withError finalExpr (mergeSoftErrors errOpt errFinal)) reader
                | Error e -> Error(mergeWithError e errOpt)
            | Error e ->
                let expectedMsg =
                    { e with
                        Errors = Message $"Expected closing operator '{closeOp}'"
                    }

                let hard = ParseError.createNested failure [ expectedMsg; e ] e.Position
                Error(mergeWithError hard errOpt)
        | Error e -> Error e

    let private lhsTernary
        pExpr
        ops
        minBinding
        op
        rightPower
        (delim: 'Op)
        (parseDelim: Parser<'Op, 'T, 'State, 'Input, 'InputSlice>)
        complete
        (reader: Reader<_, _, _, _>)
        =
        match parseLhsInternal pExpr ops rightPower reader with
        | Ok { Expr = condition; Error = errCond } ->
            match parseDelim reader with
            | Ok delimTok ->
                match parseLhsInternal pExpr ops Precedence.MinP reader with
                | Ok { Expr = body; Error = errBody } ->
                    match
                        parseRhsInternal
                            pExpr
                            ops
                            minBinding
                            (complete op condition delimTok body)
                            (mergeSoftErrors errCond errBody)
                            reader
                    with
                    | Ok { Expr = finalExpr; Error = errFinal } ->
                        let combined = mergeSoftErrors errCond errBody |> mergeSoftErrors errFinal
                        preturn (PrattParsed.withError finalExpr combined) reader
                    | Error e -> Error(mergeWithError e (mergeSoftErrors errCond errBody))
                | Error e -> Error(mergeWithError e errCond)
            | Error e ->
                let expectedMsg =
                    { e with
                        Errors = Message $"Expected delimiter '{delim}'"
                    }

                let hard = ParseError.createNested failure [ expectedMsg; e ] e.Position
                Error(mergeWithError hard errCond)
        | Error e -> Error e

    // ── Core recursive functions ───────────────────────────────────────

    [<TailCall>]
    let rec private parseRhsInternal
        (pExpr: Parser<'Expr, 'T, _, _, _>)
        (ops: Operators<'Op, 'Aux, 'Expr, 'T, 'State, 'Input, 'InputSlice>)
        minBinding
        lhs
        (errAcc: ParseError<'T, 'State> voption)
        (reader: Reader<_, _, _, _>)
        : ParseResult<PrattParsed<'Expr, 'T, 'State>, _, _> =

        let pos = reader.Position

        match ops.RhsParser reader with
        | Ok op ->
            // Every RHS operator variant carries leftPower in slot 3; extract once so
            // the < / = minBinding checks are hoisted out of the per-case dispatch
            // and the JIT only emits one branch ladder per token instead of eight.
            let leftPower =
                match op with
                | Postfix(_, _, lp, _)
                | InfixLeft(_, _, lp, _)
                | InfixRight(_, _, lp, _)
                | InfixNonAssociative(_, _, lp, _)
                | InfixNary(_, _, lp, _, _)
                | InfixMapped(_, _, lp, _, _)
                | Indexer(_, _, lp, _, _, _, _)
                | Ternary(_, _, lp, _, _) -> lp

            if leftPower < minBinding then
                reader.Position <- pos
                // InfixNonAssociative drops errAcc on the low-power branch; every
                // other variant propagates it. Preserve that distinction.
                match op with
                | InfixNonAssociative _ -> preturn (PrattParsed.success lhs) reader
                | _ -> preturn (PrattParsed.withError lhs errAcc) reader
            elif leftPower = minBinding then
                fail ambiguous reader
            else
                match op with
                | Postfix(op, _parseOp, _, completePostfix) ->
                    ensureAdvanced pos reader
                    parseRhsInternal pExpr ops minBinding (completePostfix lhs op) errAcc reader

                | InfixLeft(op, _parseOp, _, completeInfix) ->
                    rhsInfix pExpr ops minBinding lhs errAcc pos (leftPower + 1uy<bp>) op completeInfix reader

                | InfixRight(op, _parseOp, _, completeInfix) ->
                    rhsInfix pExpr ops minBinding lhs errAcc pos (leftPower - 1uy<bp>) op completeInfix reader

                | InfixNonAssociative(op, _parseOp, _, completeInfix) ->
                    rhsInfix pExpr ops minBinding lhs errAcc pos leftPower op completeInfix reader

                | InfixNary(op, parseOp, _, allowTrailingOp, completeNary) ->
                    rhsInfixNary pExpr ops minBinding lhs leftPower op parseOp allowTrailingOp completeNary reader

                | InfixMapped(op, _parseOp, _, parseRight, complete) ->
                    rhsInfixMapped pExpr ops minBinding lhs errAcc pos op parseRight complete reader

                | Indexer(op, _parseOp, _, _closeOp, parseCloseOp, innerParser, completeIndexer) ->
                    rhsIndexer pExpr ops minBinding lhs errAcc pos op parseCloseOp innerParser completeIndexer reader

                | Ternary(op, _parseOp, _, parseTernaryOp, completeTernary) ->
                    rhsTernary
                        pExpr
                        ops
                        minBinding
                        lhs
                        errAcc
                        pos
                        op
                        (leftPower - 1uy<bp>)
                        parseTernaryOp
                        completeTernary
                        reader

        | Error eRhs ->
            reader.Position <- pos
            preturn (PrattParsed.withError lhs (ValueSome eRhs)) reader

    let private parseLhsInternal pExpr ops minBinding reader : ParseResult<PrattParsed<'Expr, 'T, 'State>, _, _> =
        let pos = reader.Position

        match pExpr reader with
        | Ok lhsSuccess -> parseRhsInternal pExpr ops minBinding lhsSuccess ValueNone reader
        | Error e0 ->
            reader.Position <- pos

            match ops.LhsParser reader with
            | Ok op ->
                match op with
                | Prefix(op, _parseOp, rightPower, completePrefix) ->
                    lhsPrefix pExpr ops minBinding op rightPower completePrefix reader

                | Enclosed(op, _parseOp, _rightPower, closeOp, closeOpParser, completeBracket) ->
                    lhsEnclosed pExpr ops minBinding op closeOp closeOpParser completeBracket reader

                | LHSTernary(op, _parseOp, rightPower, delim, parseDelim, complete) ->
                    lhsTernary pExpr ops minBinding op rightPower delim parseDelim complete reader

                | PrefixMapped(op, _parseOp, parseRight, complete) ->
                    match parseRight reader with
                    | Ok result -> parseRhsInternal pExpr ops minBinding (complete op result) ValueNone reader
                    | Error e -> Error e

            | Error e1 -> Error(ParseError.createNested failure [ e1; e0 ] pos)

    let parseLhs pExpr ops minBinding reader =
        match parseLhsInternal pExpr ops minBinding reader with
        | Ok { Expr = expr; Error = _ } -> Ok expr
        | Error e -> Error e

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
        | InfixNary(op, _, _, _, _)
        | InfixMapped(op, _, _, _, _)
        | Postfix(op, _, _, _)
        | Indexer(op, _, _, _, _, _, _)
        | Ternary(op, _, _, _, _) -> op

    let private rhsParseOp =
        function
        | InfixLeft(_, parseOp, _, _)
        | InfixRight(_, parseOp, _, _)
        | InfixNonAssociative(_, parseOp, _, _)
        | InfixNary(_, parseOp, _, _, _)
        | InfixMapped(_, parseOp, _, _, _)
        | Postfix(_, parseOp, _, _)
        | Indexer(_, parseOp, _, _, _, _, _)
        | Ternary(_, parseOp, _, _, _) -> parseOp

    let private rhsLeftPower =
        function
        | InfixLeft(_, _, leftPower, _)
        | InfixRight(_, _, leftPower, _)
        | InfixNonAssociative(_, _, leftPower, _)
        | InfixNary(_, _, leftPower, _, _)
        | InfixMapped(_, _, leftPower, _, _)
        | Postfix(_, _, leftPower, _)
        | Indexer(_, _, leftPower, _, _, _, _)
        | Ternary(_, _, leftPower, _, _) -> leftPower

    let private rhsRightPower =
        function
        | InfixLeft(_, _, leftPower, _) -> BindingPower.leftAssocRhs leftPower
        | InfixRight(_, _, leftPower, _) -> BindingPower.rightAssocRhs leftPower
        | InfixNonAssociative(_, _, leftPower, _) -> leftPower
        | InfixNary(_, _, leftPower, _, _) -> leftPower
        | InfixMapped(_, _, leftPower, _, _) -> BindingPower.leftAssocRhs leftPower
        | Ternary(_, _, leftPower, _, _) -> BindingPower.rightAssocRhs leftPower
        | Postfix(_, _, _, _)
        | Indexer(_, _, _, _, _, _, _) -> Precedence.MinP

    let private lhsOp =
        function
        | Prefix(op, _, _, _)
        | LHSTernary(op, _, _, _, _, _)
        | Enclosed(op, _, _, _, _, _)
        | PrefixMapped(op, _, _, _) -> op

    let private lhsParseOp =
        function
        | Prefix(_, parseOp, _, _)
        | LHSTernary(_, parseOp, _, _, _, _)
        | Enclosed(_, parseOp, _, _, _, _)
        | PrefixMapped(_, parseOp, _, _) -> parseOp

    let private lhsRightPower =
        function
        | Prefix(_, _, rightPower, _)
        | LHSTernary(_, _, rightPower, _, _, _)
        | Enclosed(_, _, rightPower, _, _, _) -> rightPower
        | PrefixMapped(_, _, _, _) -> Precedence.MinP

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
    /// Set `allowTrailingOp` to true to silently accept a trailing separator (e.g. semicolons in lists).
    let infixNary op precedence parseOp allowTrailingOp complete =
        let power = Precedence.bindingPower precedence
        RHS(InfixNary(op, parseOp, power, allowTrailingOp, complete))

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

    /// Creates a prefix operator whose right-hand side is fully parsed by a custom parser.
    /// Unlike `prefix`, there is no recursive Pratt descent for the right-hand side.
    /// Used where the body parser controls its own structure and context.
    let prefixMapped op parseOp parseRight complete =
        LHS(PrefixMapped(op, parseOp, parseRight, complete))

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
