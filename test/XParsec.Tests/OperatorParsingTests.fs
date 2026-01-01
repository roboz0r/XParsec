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
    | Nary of op: 'Token * exprs: Expr<'Token> list
    | IfThen of cond: Expr<'Token> * body: Expr<'Token>
    | Dot of lhs: Expr<'Token> * rhs: 'Token

module Expr =
    let infix lhs op rhs = Infix(op, lhs, rhs)
    let prefix op expr = Prefix(op, expr)
    let postfix expr op = Postfix(op, expr)
    let bracketed left expr right = Bracketed(left, right, expr)
    let indexer lhs left index right = Indexer(left, right, lhs, Token index)

    let ternary cond opLeft thenExpr opRight elseExpr =
        Ternary(opLeft, opRight, cond, thenExpr, elseExpr)

    let nary op exprs = Nary(op, exprs)

    let tupleReducer exprs x = Nary(Seq.head x, List.ofSeq exprs)
    let lhsTernary _ cond _ body = IfThen(cond, body)
    let dot lhs _ rhs = Dot(lhs, rhs)

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
                    [ Operator.infixLeftAssoc (Op '+') P1 (pitem (Op '+')) Expr.infix ]
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
                        Operator.infixLeftAssoc (Op '+') P1 (pitem (Op '+')) Expr.infix
                        Operator.infixLeftAssoc (Op '*') P2 (pitem (Op '*')) Expr.infix
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
                        Operator.infixLeftAssoc (Op '+') P1 (pitem (Op '+')) Expr.infix
                        Operator.infixLeftAssoc (Op '*') P2 (pitem (Op '*')) Expr.infix
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
                        Operator.infixLeftAssoc (Op '-') P1 (pitem (Op '-')) Expr.infix
                        Operator.infixLeftAssoc (Op '+') P1 (pitem (Op '+')) Expr.infix
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
                        Operator.infixRightAssoc (Op '-') P1 (pitem (Op '-')) Expr.infix
                        Operator.infixRightAssoc (Op '+') P1 (pitem (Op '+')) Expr.infix
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
                    [ Operator.ternary (Op '?') P1 (pitem (Op '?')) (pitem (Op ':')) Expr.ternary ]
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

            test "Tuple operator" {
                let tokens = [| Number 1; Op ','; Number 2; Op ','; Number 3 |]

                let reader = Reader.ofArray tokens ()

                let ops =
                    [ Operator.infixNary (Op ',') P1 (pitem (Op ',')) Expr.tupleReducer ]
                    |> Operator.create

                let p = Operator.parser (pid |>> Expr.Token) ops

                match p (reader) with
                | Ok success ->
                    ""
                    |> Expect.equal success.Parsed (Nary(Op ',', [ Token(Number 1); Token(Number 2); Token(Number 3) ]))

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
    | Tuple
    | Dot // For InfixMapped
    | IfKey
    | ThenKey // For LHSTernary (represented by 'i' and 't' in string)

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
            | ',' -> Tuple
            | '.' -> Dot
            | 'i' -> IfKey // 'i' stands for "if"
            | 't' -> ThenKey // 't' stands for "then"
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
            Operator.ternary If P1 (pitem If) (pitem Else) Expr.ternary

            // LHS Ternary / Mixfix (if cond then body)
            // Note: We use a low precedence (P1) so it wraps loosely
            Operator.lhsTernary IfKey P1 (pitem IfKey) ThenKey (pitem ThenKey) Expr.lhsTernary

            Operator.infixNary Tuple P2 (pitem Tuple) Expr.tupleReducer

            Operator.infixLeftAssoc Add P3 (pitem Add) Expr.infix
            Operator.infixLeftAssoc Sub P3 (pitem Sub) Expr.infix

            Operator.infixLeftAssoc Mul P4 (pitem Mul) Expr.infix
            Operator.infixLeftAssoc Div P4 (pitem Div) Expr.infix

            Operator.infixRightAssoc Pow P5 (pitem Pow) Expr.infix

            Operator.prefix Sub P6 (pitem Sub) Expr.prefix
            Operator.prefix Add P6 (pitem Add) Expr.prefix

            Operator.postfix Factorial P7 (pitem Factorial) Expr.postfix

            Operator.indexer LIdx RIdx P8 (pitem LIdx) (satisfy Tokens2.isNumber) (pitem RIdx) Expr.indexer

            Operator.infixMapped Dot P9 (pitem Dot) (satisfy Tokens2.isNumber) Expr.dot

            Operator.enclosedBy LParen RParen P10 (pitem LParen) (pitem RParen) Expr.bracketed
        ]
        |> Operator.create

    let testParser (tokens, expected) =
        let p = Operator.parser (satisfy Tokens2.isNumber |>> Token) ops
        let tokens2 = Tokens2.ofString tokens
        let reader = Reader.ofArray tokens2 ()

        match p (reader) with
        | Ok success ->
            $"{tokens} wasn't parsed" |> Expect.equal success.Parsed expected
            "Parser did not consume all input" |> Expect.isTrue reader.AtEnd
        | Error err -> failwith $"parsing '{tokens}' failed\n%A{err}"

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
                    "1,2", Nary(Tuple, [ Token(N1); Token(N2) ])
                    "1,2,3", Nary(Tuple, [ Token(N1); Token(N2); Token(N3) ])
                    "1.2", Expr.Dot(Token(N1), N2)
                    "i1t2", IfThen(Token(N1), Token(N2))
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

                    "1,2+3,4", Nary(Tuple, [ Token(N1); Infix(Add, Token(N2), Token(N3)); Token(N4) ])

                    "1*2,3+4", Nary(Tuple, [ Infix(Mul, Token(N1), Token(N2)); Infix(Add, Token(N3), Token(N4)) ])

                    "(1,2),3",
                    Nary(Tuple, [ Bracketed(LParen, RParen, Nary(Tuple, [ Token(N1); Token(N2) ])); Token(N3) ])

                    "1,2?3:4", Ternary(If, Else, Nary(Tuple, [ Token(N1); Token(N2) ]), Token(N3), Token(N4))

                    "1?2,3:4", Ternary(If, Else, Token(N1), Nary(Tuple, [ Token(N2); Token(N3) ]), Token(N4))

                    "-1,-2", Nary(Tuple, [ Prefix(Sub, Token(N1)); Prefix(Sub, Token(N2)) ])

                    "(1,2),(3,4)",
                    Nary(
                        Tuple,
                        [
                            Bracketed(LParen, RParen, Nary(Tuple, [ Token(N1); Token(N2) ]))
                            Bracketed(LParen, RParen, Nary(Tuple, [ Token(N3); Token(N4) ]))
                        ]
                    )

                    // InfixMapped (Dot) has P9, Mul has P4
                    // 1.2 * 3 -> (1.2) * 3
                    "1.2*3", Infix(Mul, Expr.Dot(Token(N1), N2), Token(N3))

                    // Dot vs Indexer (P9 vs P8)
                    // 1[2].3 -> (1[2]).3
                    "1[2].3", Expr.Dot(Indexer(LIdx, RIdx, Token(N1), Token(N2)), N3)

                    // 1.2[3] -> (1.2)[3]
                    "1.2[3]", Indexer(LIdx, RIdx, Expr.Dot(Token(N1), N2), Token(N3))

                    // Chained Dot
                    // 1.2.3 -> (1.2).3
                    "1.2.3", Expr.Dot(Expr.Dot(Token(N1), N2), N3)
                ]
                |> List.iter testParser
            }

            test "LHS Ternary Interaction" {
                [
                    // i 1 t 2 + 3
                    // If is P1, Add is P3.
                    // Should parse as: if 1 then (2 + 3)
                    // Because 'Then' acts as a delimiter, the RHS of 'Then' starts with MinPrecedence.
                    // However, 'If' itself returns an Expr that participates in the outer loop.
                    // If 'LHSTernary' P1 < Add P3, then `(if ...) + 3` isn't possible unless enclosed.

                    // Case 1: Inside the body
                    "i1t2+3", IfThen(Token(N1), Infix(Add, Token(N2), Token(N3)))

                    // Case 2: Inside the condition
                    "i1+2t3", IfThen(Infix(Add, Token(N1), Token(N2)), Token(N3))

                    // Case 3: Chaining (Right Associative recursion in the body)
                    // if 1 then if 2 then 3
                    "i1ti2t3", IfThen(Token(N1), IfThen(Token(N2), Token(N3)))

                    // Case 4: Operator on the result of the If (Needs Parens usually if precedence is low)
                    // (if 1 then 2) + 3
                    "(i1t2)+3", Infix(Add, Bracketed(LParen, RParen, IfThen(Token(N1), Token(N2))), Token(N3))
                ]
                |> List.iter testParser
            }

            test "Tuple Stress Test" {
                // Generate: "1,1,1,..."
                // Ensures we don't stack overflow on large n-ary operator usage
                let count = 10_000
                let hugeTupleString = String.concat "," (List.replicate count "1")

                // Expected: Tuple [Token(N1); Token(N1); ...]
                let expectedItems = List.replicate count (Token(N1))
                let expected = Nary(Tuple, expectedItems)

                testParser (hugeTupleString, expected)
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
            Operator.ternary If P1 (pitem '?' >>% If) (pitem ':' >>% Else) Expr.ternary

            Operator.infixLeftAssoc Add P2 (pitem '+' >>% Add) Expr.infix
            Operator.infixLeftAssoc Sub P2 (pitem '-' >>% Sub) Expr.infix

            Operator.infixLeftAssoc Mul P3 (pitem '*' >>% Mul) Expr.infix
            Operator.infixLeftAssoc Div P3 (pitem '/' >>% Div) Expr.infix

            Operator.infixRightAssoc Pow P4 (pstring "**" >>% Pow) Expr.infix

            Operator.prefix Sub P5 (pitem '-' >>% Sub) Expr.prefix
            Operator.prefix Add P5 (pitem '+' >>% Add) Expr.prefix

            Operator.postfix Factorial P6 (pitem '!' >>% Factorial) Expr.postfix

            Operator.indexer LIdx RIdx P7 (pitem '[' >>% LIdx) pNum (pitem ']' >>% RIdx) Expr.indexer

            Operator.enclosedBy LParen RParen P10 (pitem '(' >>% LParen) (pitem ')' >>% RParen) Expr.bracketed
        ]
        |> Operator.create

    let testParser (tokens, expected) =


        let p = Operator.parser (pNum |>> Token) ops
        let reader = Reader.ofString tokens ()

        match p (reader) with
        | Ok success ->
            $"{tokens} wasn't parsed" |> Expect.equal success.Parsed expected
            "Parser did not consume all input" |> Expect.isTrue reader.AtEnd
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


[<RequireQualifiedAccess>]
type SimpleError<'T> =
    | Expected of 'T
    | ExpectedSeq of 'T list
    | ExpectedOneOf of 'T seq
    | ExpectedSeqOneOf of 'T seq seq
    | Unexpected of 'T
    | UnexpectedSeq of 'T list
    | Message of string
    | EndOfInput
    | Nested of parent: SimpleError<'T> * children: SimpleError<'T> list

[<RequireQualifiedAccess>]
module SimpleError =

    /// Recursively converts the complex ErrorType into a test-friendly SimpleError
    let rec ofErrorType (e: ErrorType<'T, 'State>) : SimpleError<'T> =
        match e with
        | Expected x -> SimpleError.Expected x
        | ExpectedSeq xs -> SimpleError.ExpectedSeq(List.ofSeq xs)
        | ExpectedOneOf xs -> SimpleError.ExpectedOneOf xs
        | ExpectedSeqOneOf xss -> SimpleError.ExpectedSeqOneOf xss
        | Unexpected x -> SimpleError.Unexpected x
        | UnexpectedSeq xs -> SimpleError.UnexpectedSeq(List.ofSeq xs)
        | Message s -> SimpleError.Message s
        | EndOfInput -> SimpleError.EndOfInput
        | Nested(parent, children) ->
            SimpleError.Nested(ofErrorType parent, children |> List.map (fun c -> ofErrorType c.Errors))

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
            Operator.ternary If P1 (pitem '?' >>% If) (pitem ':' >>% Else) Expr.ternary
            Operator.infixNary Tuple P2 (pitem ',' >>% Tuple) Expr.tupleReducer

            Operator.infixLeftAssoc Add P3 (pitem '+' >>% Add) Expr.infix
            Operator.infixLeftAssoc Sub P3 (pitem '-' >>% Sub) Expr.infix
            Operator.infixLeftAssoc Mul P4 ((pitem '*' .>> notFollowedBy (pitem '*')) >>% Mul) Expr.infix
            Operator.infixLeftAssoc Div P4 (pitem '/' >>% Div) Expr.infix
            Operator.infixNonAssoc Pow P4 (pstring "**" >>% Pow) Expr.infix

            Operator.prefix Sub P6 (pitem '-' >>% Sub) Expr.prefix
            Operator.prefix Add P6 (pitem '+' >>% Add) Expr.prefix

            Operator.postfix Factorial P7 (pitem '!' >>% Factorial) Expr.postfix

            Operator.enclosedBy LParen RParen P10 (pitem '(' >>% LParen) (pitem ')' >>% RParen) Expr.bracketed
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

    let parserShouldFail (tokens, expected) =
        let p = Operator.parser (pNum |>> Token) ops
        let reader = Reader.ofString tokens ()

        match p (reader) with
        | Ok success -> failwith $"Expected failure but got {success}"
        | Error err ->
            tokens
            |> Expect.equal (SimpleError.ofErrorType err.Errors, err.Position.Index) expected

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
                    "1**2**3", (SimpleError.Message "Ambiguous operator associativity", 6)
                    "1**2*3", (SimpleError.Message "Ambiguous operator associativity", 5)

                    "1,",
                    (SimpleError.Nested(
                        SimpleError.Message "Operator parsing failed",
                        [
                            SimpleError.Message "LHS did not match any known operator"
                            SimpleError.EndOfInput
                        ]
                     ),
                     2)

                    "1,2,",
                    (SimpleError.Nested(
                        SimpleError.Message "Operator parsing failed",
                        [
                            SimpleError.Message "LHS did not match any known operator"
                            SimpleError.EndOfInput
                        ]
                     ),
                     4)

                    "1,,2",
                    (SimpleError.Nested(
                        SimpleError.Message "Operator parsing failed",
                        [
                            SimpleError.Message "LHS did not match any known operator"
                            SimpleError.Unexpected ','
                        ]
                     ),
                     2)

                    "1?2",
                    (SimpleError.Nested(
                        SimpleError.Message "Operator parsing failed",
                        [
                            SimpleError.Message "Expected close ternary operator"
                            SimpleError.EndOfInput
                        ]
                     ),
                     3)

                    "(1",
                    (SimpleError.Nested(
                        SimpleError.Message "Operator parsing failed",
                        [
                            SimpleError.Message "Expected closing operator 'RParen'"
                            SimpleError.EndOfInput
                        ]
                     ),
                     2)
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
            Operator.infixLeftAssoc "+" P1 (op (pchar '+') >>% "+") (fun l _ r -> Add(l, r))
            Operator.infixLeftAssoc "-" P1 (op (pchar '-') >>% "-") (fun l _ r -> Add(l, Negate r)) // Subtraction as adding a negation

            // P2: Multiplication (Left-associative)
            Operator.infixLeftAssoc "*" P2 (op (pchar '*') >>% "*") (fun l _ r -> Multiply(l, r))
            // P3: Exponentiation (Right-associative)
            Operator.infixRightAssoc "**" P3 (op (pstring "**")) (fun l _ r -> Power(l, r))

            // P4: Unary Negation (Prefix)
            Operator.prefix "-" P4 (op (pchar '-') >>% "-") (fun _ expr -> Negate expr)

            // P10: Grouping (Highest precedence)
            // This tells the main parser how to handle parentheses. Using `id` means the
            // parentheses only control precedence and don't add a node to the AST.
            Operator.enclosedBy "(" ")" P10 (op (pchar '(') >>% "(") (op (pchar ')') >>% ")") (fun _ expr _ -> expr)
        ]
        |> Operator.create // Compile the list into an efficient lookup table.

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

#if !FABLE_COMPILER
[<Tests>]
#endif
let sortingBugTests =
    testList
        "Operator Sorting Bug"
        [
            test "Ensure operators are sorted by precedence so longer/stronger operators are tried first" {
                // Setup a simple parser for numbers
                let pNum = anyInRange '0' '9' |>> (string >> int >> Number >> Token)

                // Define operators specifically in this order:
                // 1. High Precedence
                // 2. Lower Precedence
                let ops =
                    [
                        Operator.infixRightAssoc (Op '^') P4 (pstring "**" .>> spaces >>% (Op '^')) Expr.infix

                        Operator.infixLeftAssoc (Op '*') P3 (pstring "*" .>> spaces >>% (Op '*')) Expr.infix
                    ]
                    |> Operator.create

                let p = Operator.parser pNum ops

                // Input that requires the longer operator "**" to be matched
                let input = "1**2"
                let reader = Reader.ofString input ()

                match p reader with
                | Ok success ->
                    Expect.equal
                        success.Parsed
                        (Infix(Op '^', Token(Number 1), Token(Number 2)))
                        "Should parse '**' correctly as Power operator"
                | Error e -> failwith $"Parsing failed. This indicates 'Mul' was tried before 'Pow'.\nError: %A{e}"

                // Reverse input order to ensure consistent behavior
                let ops =
                    [
                        Operator.infixLeftAssoc (Op '*') P3 (pstring "*" .>> spaces >>% (Op '*')) Expr.infix

                        Operator.infixRightAssoc (Op '^') P4 (pstring "**" .>> spaces >>% (Op '^')) Expr.infix
                    ]
                    |> Operator.create

                let p = Operator.parser pNum ops

                // Input that requires the longer operator "**" to be matched
                let input = "1**2"
                let reader = Reader.ofString input ()

                match p reader with
                | Ok success ->
                    Expect.equal
                        success.Parsed
                        (Infix(Op '^', Token(Number 1), Token(Number 2)))
                        "Should parse '**' correctly as Power operator"
                | Error e -> failwith $"Parsing failed. This indicates 'Mul' was tried before 'Pow'.\nError: %A{e}"
            }
        ]
