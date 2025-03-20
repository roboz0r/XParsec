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
                    [ Operator.infixLeftAssoc (Op '+') P1 (pitem (Op '+')) (Expr.infix (Op '+')) ]
                    |> Operator.create

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
                        Operator.infixLeftAssoc (Op '+') P1 (pitem (Op '+')) (Expr.infix (Op '+'))
                        Operator.infixLeftAssoc (Op '*') P2 (pitem (Op '*')) (Expr.infix (Op '*'))
                    ]
                    |> Operator.create

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
                        Operator.infixLeftAssoc (Op '+') P1 (pitem (Op '+')) (Expr.infix (Op '+'))
                        Operator.infixLeftAssoc (Op '*') P2 (pitem (Op '*')) (Expr.infix (Op '*'))
                    ]
                    |> Operator.create

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
                        Operator.infixLeftAssoc (Op '-') P1 (pitem (Op '-')) (Expr.infix (Op '-'))
                        Operator.infixLeftAssoc (Op '+') P1 (pitem (Op '+')) (Expr.infix (Op '+'))
                    ]
                    |> Operator.create

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
                        Operator.infixRightAssoc (Op '-') P1 (pitem (Op '-')) (Expr.infix (Op '-'))
                        Operator.infixRightAssoc (Op '+') P1 (pitem (Op '+')) (Expr.infix (Op '+'))
                    ]
                    |> Operator.create

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
                        Operator.ternary (Op '?') P1 (pitem (Op '?')) (pitem (Op ':')) (Expr.ternary (Op '?') (Op ':'))
                    ]
                    |> Operator.create

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
            Operator.ternary If P1 (pitem If) (pitem Else) (Expr.ternary If Else)

            Operator.infixLeftAssoc Add P2 (pitem Add) (Expr.infix Add)
            Operator.infixLeftAssoc Sub P2 (pitem Sub) (Expr.infix Sub)

            Operator.infixLeftAssoc Mul P3 (pitem Mul) (Expr.infix Mul)
            Operator.infixLeftAssoc Div P3 (pitem Div) (Expr.infix Div)

            Operator.infixRightAssoc Pow P4 (pitem Pow) (Expr.infix Pow)

            Operator.prefix Sub P5 (pitem Sub) (Expr.prefix Sub)
            Operator.prefix Add P5 (pitem Add) (Expr.prefix Add)

            Operator.postfix Factorial P6 (pitem Factorial) (Expr.postfix Factorial)

            Operator.indexer
                LIdx
                RIdx
                P7
                (pitem LIdx)
                (satisfy Tokens2.isNumber |>> Expr.Token)
                (pitem RIdx)
                (Expr.indexer LIdx RIdx)

            Operator.brackets LParen RParen P10 (pitem LParen) (pitem RParen) (Expr.bracketed LParen RParen)
        ]
        |> Operator.create

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
            Operator.ternary If P1 (pitem '?' >>% If) (pitem ':' >>% Else) (Expr.ternary If Else)

            Operator.infixLeftAssoc Add P2 (pitem '+' >>% Add) (Expr.infix Add)
            Operator.infixLeftAssoc Sub P2 (pitem '-' >>% Sub) (Expr.infix Sub)

            Operator.infixLeftAssoc Mul P3 (pitem '*' >>% Mul) (Expr.infix Mul)
            Operator.infixLeftAssoc Div P3 (pitem '/' >>% Div) (Expr.infix Div)

            Operator.infixRightAssoc Pow P4 (pstring "**" >>% Pow) (Expr.infix Pow)

            Operator.prefix Sub P5 (pitem '-' >>% Sub) (Expr.prefix Sub)
            Operator.prefix Add P5 (pitem '+' >>% Add) (Expr.prefix Add)

            Operator.postfix Factorial P6 (pitem '!' >>% Factorial) (Expr.postfix Factorial)

            Operator.indexer
                LIdx
                RIdx
                P7
                (pitem '[' >>% LIdx)
                (pNum |>> Expr.Token)
                (pitem ']' >>% RIdx)
                (Expr.indexer LIdx RIdx)

            Operator.brackets
                LParen
                RParen
                P10
                (pitem '(' >>% LParen)
                (pitem ')' >>% RParen)
                (Expr.bracketed LParen RParen)
        ]
        |> Operator.create

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
