module OperatorParsingTests

open System

#if FABLE_COMPILER
open Fable.Pyxpecto
#else
open Expecto
#endif

open XParsec
open XParsec.Parsers
open XParsec.OperatorParsing

[<Struct>]
type Tokens =
    | Number of number: int
    | Op of op: char

type Expr<'Token> =
    | Token of 'Token
    | Infix of opInfix: 'Token * lhs: Expr<'Token> * rhs: Expr<'Token>
    | Prefix of prefix: 'Token * expr: Expr<'Token>
    | Postfix of postfix: 'Token * expr: Expr<'Token>
    | Bracketed of left: 'Token * right: 'Token * expr: Expr<'Token>
    | Indexer of left: 'Token * right: 'Token * Expr<'Token> * Expr<'Token>
    | Ternary of opLeft: 'Token * opRight: 'Token * cond: Expr<'Token> * thenExpr: Expr<'Token> * elseExpr: Expr<'Token>

module Expr =
    let infix op lhs rhs = Infix(op, lhs, rhs)
    let prefix op expr = Prefix(op, expr)
    let postfix op expr = Postfix(op, expr)
    let bracketed left right expr = Bracketed(left, right, expr)
    let indexer left right lhs index = Indexer(left, right, lhs, index)

    let ternary opLeft opRight cond thenExpr elseExpr =
        Ternary(opLeft, opRight, cond, thenExpr, elseExpr)

#if !FABLE_COMPILER
[<Tests>]
#endif
let tests =
    testList
        "OperatorParsing"
        [
            test "1+2" {
                let tokens = [| Number 1; Op '+'; Number 2 |]
                let reader = Reader.ofArray tokens ()

                let ops =
                    [ Operator.infixLeftAssoc (Op '+') P1 (Expr.infix (Op '+')) ]
                    |> Operator.createSimple

                let p = Operator.parser (pid |>> Token) ops

                match p (reader) with
                | Ok success ->
                    ""
                    |> Expect.equal success.Parsed (Infix(Op '+', Token(Number 1), Token(Number 2)))

                    "" |> Expect.isTrue (reader.AtEnd)
                | Error err -> failwith $"%A{err}"
            }

            test "1+2*3" {
                let tokens = [| Number 1; Op '+'; Number 2; Op '*'; Number 3 |]

                let reader = Reader.ofArray tokens ()

                let ops =
                    [
                        Operator.infixLeftAssoc (Op '+') P1 (Expr.infix (Op '+'))
                        Operator.infixLeftAssoc (Op '*') P2 (Expr.infix (Op '*'))
                    ]
                    |> Operator.createSimple

                let p = Operator.parser (pid |>> Expr.Token) ops

                match p (reader) with
                | Ok success ->
                    ""
                    |> Expect.equal
                        success.Parsed
                        (Infix(Op '+', Token(Number 1), Infix(Op '*', Token(Number 2), Token(Number 3))))

                    "" |> Expect.isTrue (reader.AtEnd)
                | Error err -> failwith $"%A{err}"
            }

            test "1*2+3" {
                let tokens = [| Number 1; Op '*'; Number 2; Op '+'; Number 3 |]

                let reader = Reader.ofArray tokens ()

                let ops =
                    [
                        Operator.infixLeftAssoc (Op '+') P1 (Expr.infix (Op '+'))
                        Operator.infixLeftAssoc (Op '*') P2 (Expr.infix (Op '*'))
                    ]
                    |> Operator.createSimple

                let p = Operator.parser (pid |>> Expr.Token) ops

                match p (reader) with
                | Ok success ->
                    ""
                    |> Expect.equal
                        success.Parsed
                        (Infix(Op '+', Infix(Op '*', Token(Number 1), Token(Number 2)), Token(Number 3)))

                    "" |> Expect.isTrue (reader.AtEnd)
                | Error err -> failwith $"%A{err}"
            }

            test "1-2+3 left" {
                let tokens = [| Number 1; Op '-'; Number 2; Op '+'; Number 3 |]

                let reader = Reader.ofArray tokens ()

                let ops =
                    [
                        Operator.infixLeftAssoc (Op '-') P1 (Expr.infix (Op '-'))
                        Operator.infixLeftAssoc (Op '+') P1 (Expr.infix (Op '+'))
                    ]
                    |> Operator.createSimple

                let p = Operator.parser (pid |>> Expr.Token) ops

                match p (reader) with
                | Ok success ->
                    ""
                    |> Expect.equal
                        success.Parsed
                        (Infix(Op '+', Infix(Op '-', Token(Number 1), Token(Number 2)), Token(Number 3)))

                    "" |> Expect.isTrue (reader.AtEnd)
                | Error err -> failwith $"%A{err}"
            }

            test "1-2+3 right" {
                let tokens = [| Number 1; Op '-'; Number 2; Op '+'; Number 3 |]

                let reader = Reader.ofArray tokens ()

                let ops =
                    [
                        Operator.infixRightAssoc (Op '-') P1 (Expr.infix (Op '-'))
                        Operator.infixRightAssoc (Op '+') P1 (Expr.infix (Op '+'))
                    ]
                    |> Operator.createSimple

                let p = Operator.parser (pid |>> Expr.Token) ops

                match p (reader) with
                | Ok success ->
                    ""
                    |> Expect.equal
                        success.Parsed
                        (Infix(Op '-', Token(Number 1), Infix(Op '+', Token(Number 2), Token(Number 3))))

                    "" |> Expect.isTrue (reader.AtEnd)
                | Error err -> failwith $"%A{err}"
            }

            test "Ternary operator" {
                let tokens = [| Number 1; Op '?'; Number 2; Op ':'; Number 3 |]

                let reader = Reader.ofArray tokens ()

                let ops =
                    [
                        Operator.ternary (Op '?') P1 (pitem (Op ':')) (Expr.ternary (Op '?') (Op ':'))
                    ]
                    |> Operator.createSimple

                let p = Operator.parser (pid |>> Expr.Token) ops

                match p (reader) with
                | Ok success ->
                    ""
                    |> Expect.equal
                        success.Parsed
                        (Ternary(Op '?', Op ':', Token(Number 1), Token(Number 2), Token(Number 3)))

                    "" |> Expect.isTrue (reader.AtEnd)

                | Error err -> failwith $"%A{err}"
            }
        ]


type Tokens2 =
    | N0
    | N1
    | N2
    | N3
    | N4
    | N5
    | N6
    | N7
    | N8
    | N9
    | Add
    | Sub
    | Mul
    | Div
    | Pow
    | LParen
    | RParen
    | Factorial
    | If
    | Else
    | LIdx
    | RIdx

module Tokens2 =
    let ofString (s: string) =
        s.ToCharArray()
        |> Array.map (fun c ->
            match c with
            | '0' -> N0
            | '1' -> N1
            | '2' -> N2
            | '3' -> N3
            | '4' -> N4
            | '5' -> N5
            | '6' -> N6
            | '7' -> N7
            | '8' -> N8
            | '9' -> N9
            | '+' -> Add
            | '-' -> Sub
            | '*' -> Mul
            | '/' -> Div
            | '^' -> Pow
            | '(' -> LParen
            | ')' -> RParen
            | '!' -> Factorial
            | '?' -> If
            | ':' -> Else
            | '[' -> LIdx
            | ']' -> RIdx
            | _ -> failwith $"Invalid token '{c}' in '{s}'"
        )

    let isNumber =
        function
        | N0
        | N1
        | N2
        | N3
        | N4
        | N5
        | N6
        | N7
        | N8
        | N9 -> true
        | _ -> false

#if !FABLE_COMPILER
[<Tests>]
#endif
let tests2 =

    let ops =
        [
            Operator.ternary If P1 (pitem Else) (Expr.ternary If Else)

            Operator.infixLeftAssoc Add P2 (Expr.infix Add)
            Operator.infixLeftAssoc Sub P2 (Expr.infix Sub)

            Operator.infixLeftAssoc Mul P3 (Expr.infix Mul)
            Operator.infixLeftAssoc Div P3 (Expr.infix Div)
            Operator.infixRightAssoc Pow P4 (Expr.infix Pow)

            Operator.prefix Sub P5 (Expr.prefix Sub)
            Operator.prefix Add P5 (Expr.prefix Add)

            Operator.postfix Factorial P6 (Expr.postfix Factorial)

            Operator.indexer LIdx P7 (satisfy Tokens2.isNumber |>> Expr.Token) (pitem RIdx) (Expr.indexer LIdx RIdx)

            Operator.enclosedBy LParen P10 (pitem RParen) (Expr.bracketed LParen RParen)
        ]
        |> Operator.createSimple

    let testParser (tokens, expected) =
        let p = Operator.parser (satisfy Tokens2.isNumber |>> Token) ops
        let tokens = Tokens2.ofString tokens
        let reader = Reader.ofArray tokens ()

        match p (reader) with
        | Ok success ->
            "" |> Expect.equal success.Parsed expected

            "" |> Expect.isTrue (reader.AtEnd)
        | Error err -> failwith $"%A{err}"

    testList
        "OperatorParsing2"
        [
            test "Basic expressions" {
                [
                    "1+2", Infix(Add, Token(N1), Token(N2))
                    "1-2", Infix(Sub, Token(N1), Token(N2))
                    "1*2", Infix(Mul, Token(N1), Token(N2))
                    "1/2", Infix(Div, Token(N1), Token(N2))
                    "1^2", Infix(Pow, Token(N1), Token(N2))
                    "-1", Prefix(Sub, Token(N1))
                    "+1", Prefix(Add, Token(N1))
                    "1!", Postfix(Factorial, Token(N1))
                    "(1)", Bracketed(LParen, RParen, Token(N1))
                    "1?2:3", Ternary(If, Else, Token(N1), Token(N2), Token(N3))
                    "1[2]", Indexer(LIdx, RIdx, Token(N1), Token(N2))
                ]
                |> List.iter testParser
            }

            test "Precedence expressions" {
                [
                    "1+2*3", Infix(Add, Token(N1), Infix(Mul, Token(N2), Token(N3)))
                    "1*2+3", Infix(Add, Infix(Mul, Token(N1), Token(N2)), Token(N3))
                    "1+2-3", Infix(Sub, Infix(Add, Token(N1), Token(N2)), Token(N3))
                    "1-2+3", Infix(Add, Infix(Sub, Token(N1), Token(N2)), Token(N3))
                    "1*2/3", Infix(Div, Infix(Mul, Token(N1), Token(N2)), Token(N3))
                    "1/2*3", Infix(Mul, Infix(Div, Token(N1), Token(N2)), Token(N3))
                    "1^2^3", Infix(Pow, Token(N1), Infix(Pow, Token(N2), Token(N3)))
                    "-1+2", Infix(Add, Prefix(Sub, Token(N1)), Token(N2))
                    "1+-2", Infix(Add, Token(N1), Prefix(Sub, Token(N2)))
                    "(1+2)*3", Infix(Mul, Bracketed(LParen, RParen, Infix(Add, Token(N1), Token(N2))), Token(N3))
                    "1*(2+3)", Infix(Mul, Token(N1), Bracketed(LParen, RParen, Infix(Add, Token(N2), Token(N3))))
                    "1+2!", Infix(Add, Token(N1), Postfix(Factorial, Token(N2)))
                    "1!*2", Infix(Mul, Postfix(Factorial, Token(N1)), Token(N2))
                    "1!+2", Infix(Add, Postfix(Factorial, Token(N1)), Token(N2))
                    "(1+2)!", Postfix(Factorial, Bracketed(LParen, RParen, Infix(Add, Token(N1), Token(N2))))

                    "1+2*3*4+5",
                    Infix(
                        Add,
                        Infix(Add, Token(N1), Infix(Mul, Infix(Mul, Token(N2), Token(N3)), Token(N4))),
                        Token(N5)
                    )

                    "1[2][3]", Indexer(LIdx, RIdx, Indexer(LIdx, RIdx, Token(N1), Token(N2)), Token(N3))

                    "1?2:3?4:5",
                    Ternary(If, Else, Token(N1), Token(N2), Ternary(If, Else, Token(N3), Token(N4), Token(N5)))

                    "1?2:3^9?4*5+6:7",
                    Ternary(
                        If,
                        Else,
                        Token(N1),
                        Token(N2),
                        Ternary(
                            If,
                            Else,
                            Infix(Pow, Token(N3), Token(N9)),
                            Infix(Add, Infix(Mul, Token(N4), Token(N5)), Token(N6)),
                            Token(N7)
                        )
                    )
                ]
                |> List.iter testParser
            }
        ]

open XParsec.CharParsers

#if !FABLE_COMPILER
[<Tests>]
#endif
let tests3 =
    let pNum =
        parser {
            let! c = anyInRange '0' '9'

            return
                match c with
                | '0' -> N0
                | '1' -> N1
                | '2' -> N2
                | '3' -> N3
                | '4' -> N4
                | '5' -> N5
                | '6' -> N6
                | '7' -> N7
                | '8' -> N8
                | '9' -> N9
                | _ -> failwith "Unreachable"
        }

    let ops =
        [
            Operator.ternary If P1 (pitem ':' >>% Else) (Expr.ternary If Else)

            Operator.infixLeftAssoc Add P2 (Expr.infix Add)
            Operator.infixLeftAssoc Sub P2 (Expr.infix Sub)

            Operator.infixLeftAssoc Mul P3 (Expr.infix Mul)
            Operator.infixLeftAssoc Div P3 (Expr.infix Div)

            Operator.infixRightAssoc Pow P4 (Expr.infix Pow)

            Operator.prefix Sub P5 (Expr.prefix Sub)
            Operator.prefix Add P5 (Expr.prefix Add)

            Operator.postfix Factorial P6 (Expr.postfix Factorial)

            Operator.indexer LIdx P7 (pNum |>> Expr.Token) (pitem ']' >>% RIdx) (Expr.indexer LIdx RIdx)

            Operator.enclosedBy LParen P10 (pitem ')' >>% RParen) (Expr.bracketed LParen RParen)
        ]
        |> Operator.create (fun lhsLookup rhsLookup ->

            let lhsLookup2 = ImmutableLookupBuilder<char, _>()
            lhsLookup2.Add('-', lhsLookup.[Sub])
            lhsLookup2.Add('+', lhsLookup.[Add])
            lhsLookup2.Add('(', lhsLookup.[LParen])
            let lhsLookup2 = lhsLookup2.ToImmutable()

            let rhsLookup2 = ImmutableLookupBuilder<char, _>()
            rhsLookup2.Add('?', If)
            rhsLookup2.Add('+', Add)
            rhsLookup2.Add('-', Sub)
            rhsLookup2.Add('*', Mul)
            rhsLookup2.Add('/', Div)
            rhsLookup2.Add('!', Factorial)
            rhsLookup2.Add('[', LIdx)
            let rhsLookup2 = rhsLookup2.ToImmutable()

            let lhsParser: Parser<_, _, _, _, _> = pLookupL lhsLookup2 "LHS operator"

            let rhsParser: Parser<_, _, _, _, _> =
                pLookupL rhsLookup2 "RHS operator"
                >>= function
                    | Mul ->
                        // Handle '**' for Pow. We alread consumed the first '*'.
                        pchar '*' >>% rhsLookup.[Pow] <|>% rhsLookup.[Mul]
                    | op -> preturn rhsLookup.[op]


            { new Operators<_, _, _, _, _, _, _> with
                member _.LhsParser = lhsParser
                member _.RhsParser = rhsParser
            }
        )

    let testParser (tokens, expected) =


        let p = Operator.parser (pNum |>> Token) ops
        let reader = Reader.ofString tokens ()

        match p (reader) with
        | Ok success ->
            $"{tokens} wasn't parsed" |> Expect.equal success.Parsed expected

            "" |> Expect.isTrue (reader.AtEnd)
        | Error err -> failwith $"{tokens} wasn't parsed\n%A{err}"

    testList
        "Multi Char OperatorParsing"
        [
            test "Basic expressions" {
                [
                    "1+2", Infix(Add, Token(N1), Token(N2))
                    "1-2", Infix(Sub, Token(N1), Token(N2))
                    "1*2", Infix(Mul, Token(N1), Token(N2))
                    "1/2", Infix(Div, Token(N1), Token(N2))
                    "1**2", Infix(Pow, Token(N1), Token(N2))
                    "-1", Prefix(Sub, Token(N1))
                    "+1", Prefix(Add, Token(N1))
                    "1!", Postfix(Factorial, Token(N1))
                    "(1)", Bracketed(LParen, RParen, Token(N1))
                    "1?2:3", Ternary(If, Else, Token(N1), Token(N2), Token(N3))
                    "1[2]", Indexer(LIdx, RIdx, Token(N1), Token(N2))
                ]
                |> List.iter testParser
            }

            test "Precedence expressions" {
                [
                    "1+2*3", Infix(Add, Token(N1), Infix(Mul, Token(N2), Token(N3)))
                    "1*2+3", Infix(Add, Infix(Mul, Token(N1), Token(N2)), Token(N3))
                    "1+2-3", Infix(Sub, Infix(Add, Token(N1), Token(N2)), Token(N3))
                    "1-2+3", Infix(Add, Infix(Sub, Token(N1), Token(N2)), Token(N3))
                    "1*2/3", Infix(Div, Infix(Mul, Token(N1), Token(N2)), Token(N3))
                    "1/2*3", Infix(Mul, Infix(Div, Token(N1), Token(N2)), Token(N3))
                    "1**2**3", Infix(Pow, Token(N1), Infix(Pow, Token(N2), Token(N3)))
                    "-1+2", Infix(Add, Prefix(Sub, Token(N1)), Token(N2))
                    "1+-2", Infix(Add, Token(N1), Prefix(Sub, Token(N2)))
                    "(1+2)*3", Infix(Mul, Bracketed(LParen, RParen, Infix(Add, Token(N1), Token(N2))), Token(N3))
                    "1*(2+3)", Infix(Mul, Token(N1), Bracketed(LParen, RParen, Infix(Add, Token(N2), Token(N3))))
                    "1+2!", Infix(Add, Token(N1), Postfix(Factorial, Token(N2)))
                    "1!*2", Infix(Mul, Postfix(Factorial, Token(N1)), Token(N2))
                    "1!+2", Infix(Add, Postfix(Factorial, Token(N1)), Token(N2))
                    "(1+2)!", Postfix(Factorial, Bracketed(LParen, RParen, Infix(Add, Token(N1), Token(N2))))

                    "1+2*3*4+5",
                    Infix(
                        Add,
                        Infix(Add, Token(N1), Infix(Mul, Infix(Mul, Token(N2), Token(N3)), Token(N4))),
                        Token(N5)
                    )

                    "1[2][3]", Indexer(LIdx, RIdx, Indexer(LIdx, RIdx, Token(N1), Token(N2)), Token(N3))

                    "1?2:3?4:5",
                    Ternary(If, Else, Token(N1), Token(N2), Ternary(If, Else, Token(N3), Token(N4), Token(N5)))

                    "1?2:3**9?4*5+6:7",
                    Ternary(
                        If,
                        Else,
                        Token(N1),
                        Token(N2),
                        Ternary(
                            If,
                            Else,
                            Infix(Pow, Token(N3), Token(N9)),
                            Infix(Add, Infix(Mul, Token(N4), Token(N5)), Token(N6)),
                            Token(N7)
                        )
                    )
                ]
                |> List.iter testParser
            }
        ]


#if !FABLE_COMPILER
[<Tests>]
#endif
let tests4 =
    let pNum =
        parser {
            let! c = anyInRange '0' '9'

            return
                match c with
                | '0' -> N0
                | '1' -> N1
                | '2' -> N2
                | '3' -> N3
                | '4' -> N4
                | '5' -> N5
                | '6' -> N6
                | '7' -> N7
                | '8' -> N8
                | '9' -> N9
                | _ -> failwith "Unreachable"
        }

    let ops =
        [
            Operator.infixLeftAssoc Add P2 (Expr.infix Add)
            Operator.infixLeftAssoc Sub P2 (Expr.infix Sub)

            Operator.infixLeftAssoc Mul P3 (Expr.infix Mul)
            Operator.infixLeftAssoc Div P3 (Expr.infix Div)
            Operator.infixNonAssoc Pow P3 (Expr.infix Pow)

            Operator.prefix Sub P5 (Expr.prefix Sub)
            Operator.prefix Add P5 (Expr.prefix Add)

            Operator.postfix Factorial P6 (Expr.postfix Factorial)

            Operator.enclosedBy LParen P10 (pitem ')' >>% RParen) (Expr.bracketed LParen RParen)
        ]
        |> Operator.create (fun lhsLookup rhsLookup ->
            let lhsParser: Parser<_, _, _, _, _> =
                fun reader ->
                    match reader.Peek() with
                    | ValueNone -> fail EndOfInput reader
                    | ValueSome '-' ->
                        reader.Skip()
                        preturn (lhsLookup.[Sub]) reader
                    | ValueSome '+' ->
                        reader.Skip()
                        preturn (lhsLookup.[Add]) reader
                    | ValueSome '(' ->
                        reader.Skip()
                        preturn (lhsLookup.[LParen]) reader
                    | ValueSome _ -> fail (Message "LHS operator") reader

            let rhsParser: Parser<_, _, _, _, _> =
                fun reader ->
                    match reader.Peek() with
                    | ValueNone -> fail EndOfInput reader
                    | ValueSome '?' ->
                        reader.Skip()
                        preturn (rhsLookup.[If]) reader
                    | ValueSome '+' ->
                        reader.Skip()
                        preturn (rhsLookup.[Add]) reader
                    | ValueSome '-' ->
                        reader.Skip()
                        preturn (rhsLookup.[Sub]) reader
                    | ValueSome '*' ->
                        reader.Skip()
                        ((pchar '*' >>% (rhsLookup.[Pow])) <|>% (rhsLookup.[Mul])) reader
                    | ValueSome '/' ->
                        reader.Skip()
                        preturn (rhsLookup.[Div]) reader
                    | ValueSome '!' ->
                        reader.Skip()
                        preturn (rhsLookup.[Factorial]) reader
                    | ValueSome _ -> fail (Message "RHS operator") reader


            { new Operators<_, _, _, _, _, _, _> with
                member _.LhsParser = lhsParser
                member _.RhsParser = rhsParser
            }
        )

    let testParser (tokens, expected) =
        let p = Operator.parser (pNum |>> Token) ops
        let reader = Reader.ofString tokens ()

        match p (reader) with
        | Ok success ->
            $"{tokens} wasn't parsed" |> Expect.equal success.Parsed expected

            "" |> Expect.isTrue (reader.AtEnd)
        | Error err -> failwith $"{tokens} wasn't parsed\n%A{err}"

    let parserShouldFail (tokens, expected) =
        let p = Operator.parser (pNum |>> Token) ops
        let reader = Reader.ofString tokens ()

        match p (reader) with
        | Ok success -> failwith $"Expected failure but got {success}"
        | Error err -> tokens |> Expect.equal (err.Errors, err.Position.Index) expected

    testList
        "Multi Char OperatorParsing NonAssoc"
        [
            test "Basic expressions" {
                [
                    "1+2", Infix(Add, Token(N1), Token(N2))
                    "1-2", Infix(Sub, Token(N1), Token(N2))
                    "1*2", Infix(Mul, Token(N1), Token(N2))
                    "1/2", Infix(Div, Token(N1), Token(N2))
                    "1**2", Infix(Pow, Token(N1), Token(N2))
                    "-1", Prefix(Sub, Token(N1))
                    "+1", Prefix(Add, Token(N1))
                    "1!", Postfix(Factorial, Token(N1))
                    "(1)", Bracketed(LParen, RParen, Token(N1))
                ]
                |> List.iter testParser
            }

            test "Precedence expressions" {
                [
                    "1+2*3", Infix(Add, Token(N1), Infix(Mul, Token(N2), Token(N3)))
                    "1*2+3", Infix(Add, Infix(Mul, Token(N1), Token(N2)), Token(N3))
                    "1+2-3", Infix(Sub, Infix(Add, Token(N1), Token(N2)), Token(N3))
                    "1-2+3", Infix(Add, Infix(Sub, Token(N1), Token(N2)), Token(N3))
                    "1*2/3", Infix(Div, Infix(Mul, Token(N1), Token(N2)), Token(N3))
                    "1/2*3", Infix(Mul, Infix(Div, Token(N1), Token(N2)), Token(N3))
                    "1+2**3", Infix(Add, Token(N1), Infix(Pow, Token(N2), Token(N3)))
                    // Left associative with same power as non-associative
                    "1*2**3", Infix(Pow, Infix(Mul, Token(N1), Token(N2)), Token(N3))
                    "-1+2", Infix(Add, Prefix(Sub, Token(N1)), Token(N2))
                    "1+-2", Infix(Add, Token(N1), Prefix(Sub, Token(N2)))
                    "(1+2)*3", Infix(Mul, Bracketed(LParen, RParen, Infix(Add, Token(N1), Token(N2))), Token(N3))
                    "1*(2+3)", Infix(Mul, Token(N1), Bracketed(LParen, RParen, Infix(Add, Token(N2), Token(N3))))
                    "1+2!", Infix(Add, Token(N1), Postfix(Factorial, Token(N2)))
                    "1!*2", Infix(Mul, Postfix(Factorial, Token(N1)), Token(N2))
                    "1!+2", Infix(Add, Postfix(Factorial, Token(N1)), Token(N2))
                    "(1+2)!", Postfix(Factorial, Bracketed(LParen, RParen, Infix(Add, Token(N1), Token(N2))))

                    "1+2*3*4+5",
                    Infix(
                        Add,
                        Infix(Add, Token(N1), Infix(Mul, Infix(Mul, Token(N2), Token(N3)), Token(N4))),
                        Token(N5)
                    )
                ]
                |> List.iter testParser
            }

            test "Failing Precedence expressions" {
                [
                    "1**2**3", (Message "Ambiguous operator associativity", 6L)
                    "1**2*3", (Message "Ambiguous operator associativity", 5L)
                ]
                |> List.iter parserShouldFail
            }
        ]


module Docs =
    // Tests for operator-parsing.md documentation
    type Expr =
        | Number of int
        | Add of Expr * Expr
        | Multiply of Expr * Expr
        | Power of Expr * Expr
        | Negate of Expr

    // A parser for an integer, wrapped in our `Number` AST case.
    let pAtom = pint32 .>> spaces |>> Expr.Number

    // A helper to parse an operator token and skip any trailing whitespace.
    let op p = p .>> spaces

    let operators: Operators<string, obj, Expr, char, unit, ReadableString, ReadableStringSlice> =
        [
            // P1: Addition and Subtraction (Left-associative)
            Operator.infixLeftAssoc "+" P1 (fun l r -> Add(l, r))
            Operator.infixLeftAssoc "-" P1 (fun l r -> Add(l, Negate r)) // Subtraction as adding a negation

            // P2: Multiplication (Left-associative)
            Operator.infixLeftAssoc "*" P2 (fun l r -> Multiply(l, r))
            // P3: Exponentiation (Right-associative)
            Operator.infixRightAssoc "**" P3 (fun l r -> Power(l, r))

            // P4: Unary Negation (Prefix)
            Operator.prefix "-" P4 (fun expr -> Negate expr)

            // P10: Grouping (Highest precedence)
            // This tells the main parser how to handle parentheses. Using `id` means the
            // parentheses only control precedence and don't add a node to the AST.
            Operator.enclosedBy "(" P10 (op (pchar ')') >>% ")") id
        ]
        |> Operator.create (fun lhsLookup rhsLookup ->
            { new Operators<_, _, _, _, _, _, _> with
                member _.LhsParser =
                    // TODO: Use a better way to build parsers from keys/values
                    choiceL (lhsLookup.Keys |> Seq.map (fun k -> pstring k |>> fun k -> lhsLookup.[k])) "lhs"
                    .>> spaces

                member _.RhsParser =
                    choiceL (rhsLookup.Keys |> Seq.map (fun k -> pstring k |>> fun k -> rhsLookup.[k])) "rhs"
                    .>> spaces
            }
        )

    // The full expression parser. It handles optional leading whitespace,
    // then calls the generated operator parser.
    let pExpression = spaces >>. (Operator.parser pAtom operators)

    let runParser input =
        printfn $"Parsing: '{input}'"

        match pExpression (Reader.ofString input ()) with
        | Ok success -> printfn $"  Success: %A{success.Parsed}"
        | Error e ->
            let errorMsg = ErrorFormatting.formatStringError input e
            printfn $"  Error:\n%s{errorMsg}"

    let testParser input expected =
        match pExpression (Reader.ofString input ()) with
        | Ok success -> Expect.equal success.Parsed expected $"Parsing '{input}'"
        | Error e -> failwith $"Parsing '{input}' failed with error: %A{e}"

#if !FABLE_COMPILER
[<Tests>]
#endif
let testsDocs =
    test "Operator Documentation Tests" {
        Docs.testParser "1 + 2 + 3" (Docs.Add(Docs.Add(Docs.Number 1, Docs.Number 2), Docs.Number 3))
        Docs.testParser "1 + 2 * 3" (Docs.Add(Docs.Number 1, Docs.Multiply(Docs.Number 2, Docs.Number 3)))
        Docs.testParser "(1 + 2) * 3" (Docs.Multiply(Docs.Add(Docs.Number 1, Docs.Number 2), Docs.Number 3))
        Docs.testParser "-5 ** 2" (Docs.Power(Docs.Number -5, Docs.Number 2))
        Docs.testParser "2 ** 3 ** 4" (Docs.Power(Docs.Number 2, Docs.Power(Docs.Number 3, Docs.Number 4)))
        Docs.runParser "1 + 2 + 3"
        Docs.runParser "1 + 2 * 3"
        Docs.runParser "(1 + 2) * 3"
        Docs.runParser "-5 ** 2"
        Docs.runParser "2 ** 3 ** 4"
    }
