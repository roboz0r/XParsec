module OperatorParsingTests

open System

open Expecto

open XParsec
open XParsec.OperatorPrecedence
open XParsec.Parsers

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

let handler =
    { new OperatorHandler<_, Expr<_>, _, _, _, _> with
        member _.Bracketed(opLeft, opRight, expr) =
            preturn (Expr.Bracketed(opLeft, opRight, expr))

        member _.Indexer(opLeft, opRight, lhs, index) =
            preturn (Indexer(opLeft, opRight, lhs, index))

        member _.Infix(opInfix, lhs, rhs) = preturn (Infix(opInfix, lhs, rhs))
        member _.Postfix(opPostfix, expr) = preturn (Postfix(opPostfix, expr))
        member _.Prefix(opPrefix, expr) = preturn (Prefix(opPrefix, expr))
    }

[<Tests>]
let tests =
    testList
        "OperatorParsing"
        [
            test "1+2" {
                let tokens = [| Number 1; Op '+'; Number 2 |]
                let reader = Reader.ofArray tokens ()

                let ops =
                    [ Operator.infixLeftAssoc (Op '+') P1 (pitem (Op '+')) ]
                    |> Operator.create handler

                let p = Pratt.parser (pid |>> Token) ops

                match p (reader) with
                | Ok success ->
                    ""
                    |> Expect.equal success.Parsed (Infix(Op '+', Token(Number 1), Token(Number 2)))

                    "" |> Expect.isTrue (reader.AtEnd)
                | Error err -> failtest $"%A{err}"
            }

            test "1+2*3" {
                let tokens = [| Number 1; Op '+'; Number 2; Op '*'; Number 3 |]

                let reader = Reader.ofArray tokens ()

                let ops =
                    [
                        Operator.infixLeftAssoc (Op '+') P1 (pitem (Op '+'))
                        Operator.infixLeftAssoc (Op '*') P2 (pitem (Op '*'))
                    ]
                    |> Operator.create handler

                let p = Pratt.parser (pid |>> Expr.Token) ops

                match p (reader) with
                | Ok success ->
                    ""
                    |> Expect.equal
                        success.Parsed
                        (Infix(Op '+', Token(Number 1), Infix(Op '*', Token(Number 2), Token(Number 3))))

                    "" |> Expect.isTrue (reader.AtEnd)
                | Error err -> failtest $"%A{err}"
            }

            test "1*2+3" {
                let tokens = [| Number 1; Op '*'; Number 2; Op '+'; Number 3 |]

                let reader = Reader.ofArray tokens ()

                let ops =
                    [
                        Operator.infixLeftAssoc (Op '+') P1 (pitem (Op '+'))
                        Operator.infixLeftAssoc (Op '*') P2 (pitem (Op '*'))
                    ]
                    |> Operator.create handler

                let p = Pratt.parser (pid |>> Expr.Token) ops

                match p (reader) with
                | Ok success ->
                    ""
                    |> Expect.equal
                        success.Parsed
                        (Infix(Op '+', Infix(Op '*', Token(Number 1), Token(Number 2)), Token(Number 3)))

                    "" |> Expect.isTrue (reader.AtEnd)
                | Error err -> failtest $"%A{err}"
            }

            test "1-2+3 left" {
                let tokens = [| Number 1; Op '-'; Number 2; Op '+'; Number 3 |]

                let reader = Reader.ofArray tokens ()

                let ops =
                    [
                        Operator.infixLeftAssoc (Op '-') P1 (pitem (Op '-'))
                        Operator.infixLeftAssoc (Op '+') P1 (pitem (Op '+'))
                    ]
                    |> Operator.create handler

                let p = Pratt.parser (pid |>> Expr.Token) ops

                match p (reader) with
                | Ok success ->
                    ""
                    |> Expect.equal
                        success.Parsed
                        (Infix(Op '+', Infix(Op '-', Token(Number 1), Token(Number 2)), Token(Number 3)))

                    "" |> Expect.isTrue (reader.AtEnd)
                | Error err -> failtest $"%A{err}"
            }

            test "1-2+3 right" {
                let tokens = [| Number 1; Op '-'; Number 2; Op '+'; Number 3 |]

                let reader = Reader.ofArray tokens ()

                let ops =
                    [
                        Operator.infixRightAssoc (Op '-') P1 (pitem (Op '-'))
                        Operator.infixRightAssoc (Op '+') P1 (pitem (Op '+'))
                    ]
                    |> Operator.create handler

                let p = Pratt.parser (pid |>> Expr.Token) ops

                match p (reader) with
                | Ok success ->
                    ""
                    |> Expect.equal
                        success.Parsed
                        (Infix(Op '-', Token(Number 1), Infix(Op '+', Token(Number 2), Token(Number 3))))

                    "" |> Expect.isTrue (reader.AtEnd)
                | Error err -> failtest $"%A{err}"
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
            | _ -> failwith "Invalid token")

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

[<Tests>]
let tests2 =

    let ops =
        [
            Operator.infixLeftAssoc Add P1 (pitem Add)
            Operator.infixLeftAssoc Sub P1 (pitem Sub)

            Operator.infixLeftAssoc Mul P2 (pitem Mul)
            Operator.infixLeftAssoc Div P2 (pitem Div)

            Operator.infixRightAssoc Pow P3 (pitem Pow)

            Operator.prefix Sub P4 (pitem Sub)
            Operator.prefix Add P4 (pitem Add)

            Operator.postfix Factorial P5 (pitem Factorial)

            Operator.brackets LParen RParen P10 (pitem LParen) (pitem RParen)
        ]
        |> Operator.create handler

    let testParser (tokens, expected) =
        let p = Pratt.parser (satisfy Tokens2.isNumber |>> Token) ops
        let tokens = Tokens2.ofString tokens
        let reader = Reader.ofArray tokens ()

        match p (reader) with
        | Ok success ->
            "" |> Expect.equal success.Parsed expected

            "" |> Expect.isTrue (reader.AtEnd)
        | Error err -> failtest $"%A{err}"

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
                ]
                |> List.iter testParser
            }
        ]
