module XParsec.FSharp.SemanticAnalysis.Tests.CstKeysTests

open System.Collections.Immutable
open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

let private mkToken (kind: Token) (offset: int) : SyntaxToken =
    {
        PositionedToken = PositionedToken.Create(kind, offset)
        Index = TokenIndex.Virtual
    }

let private mkLongIdent (offset: int) : LongIdent<SyntaxToken> =
    {
        Idents = ImmutableArray.Create(mkToken Token.Identifier offset)
        Dots = ImmutableArray.Empty
    }

[<Tests>]
let tests =
    testList
        "CstKeys"
        [
            test "ofExpr Ident -> ExprIdent at ident's offset" {
                let key = CstKeys.ofExpr (Expr.Ident(mkToken Token.Identifier 7))
                Expect.equal key.Offset 7 "offset"
                Expect.equal key.Kind NodeKind.ExprIdent "kind"
            }

            test "ofExpr Const Literal -> ExprConst at literal's offset" {
                let key = CstKeys.ofExpr (Expr.Const(Constant.Literal(mkToken Token.NumInt32 3)))

                Expect.equal key.Offset 3 "offset"
                Expect.equal key.Kind NodeKind.ExprConst "kind"
            }

            test "ofExpr App descends to func expr for offset" {
                let funcExpr = Expr.Ident(mkToken Token.Identifier 0)
                let argExpr = Expr.Const(Constant.Literal(mkToken Token.NumInt32 2))
                let key = CstKeys.ofExpr (Expr.App(funcExpr, ImmutableArray.Create(argExpr)))
                Expect.equal key.Offset 0 "offset is funcExpr's"
                Expect.equal key.Kind NodeKind.ExprApp "kind"
            }

            test "ofExpr InfixApp keys on operator offset" {
                // Using the op offset (not the left expr's) keeps nested
                // same-kind InfixApps distinct in left-assoc / precedence chains.
                let left = Expr.Const(Constant.Literal(mkToken Token.NumInt32 0))
                let right = Expr.Const(Constant.Literal(mkToken Token.NumInt32 4))
                let op = mkToken Token.OpAddition 2
                let key = CstKeys.ofExpr (Expr.InfixApp(left, op, right))
                Expect.equal key.Offset 2 "offset is operator's"
                Expect.equal key.Kind NodeKind.ExprInfixApp "kind"
            }

            test "ofExpr Fun -> ExprLambda at funToken's offset" {
                let funToken = mkToken Token.KWFun 0
                let argPat = Pat.NamedSimple(mkToken Token.Identifier 4)
                let arrow = mkToken Token.OpArrowRight 6
                let body = Expr.Ident(mkToken Token.Identifier 9)

                let key =
                    CstKeys.ofExpr (Expr.Fun(funToken, ImmutableArray.Create(argPat), arrow, body))

                Expect.equal key.Offset 0 "offset is funToken's"
                Expect.equal key.Kind NodeKind.ExprLambda "kind"
            }

            test "ofExpr LetOrUse -> ExprLet at let-keyword's offset" {
                let letKw = LetOrUseKeyword.Let(mkToken Token.KWLet 0)

                let binding =
                    {
                        attributes = ValueNone
                        inlineToken = ValueNone
                        mutableToken = ValueNone
                        access = ValueNone
                        pattern = Pat.NamedSimple(mkToken Token.Identifier 4)
                        typarDefns = ValueNone
                        argumentPats = ImmutableArray.Empty
                        returnType = ValueNone
                        equals = mkToken Token.OpEquality 6
                        expr = Expr.Const(Constant.Literal(mkToken Token.NumInt32 8))
                    }

                let inToken = ValueSome(mkToken Token.KWIn 10)

                let body = ValueSome(Expr.Ident(mkToken Token.Identifier 13))

                let key =
                    CstKeys.ofExpr (
                        Expr.LetOrUse(
                            letKw,
                            ValueNone,
                            ImmutableArray.Create(binding),
                            ImmutableArray.Empty,
                            inToken,
                            body
                        )
                    )

                Expect.equal key.Offset 0 "offset is let-keyword's"
                Expect.equal key.Kind NodeKind.ExprLet "kind"
            }

            test "ofPat NamedSimple -> PatIdent at ident's offset" {
                let key = CstKeys.ofPat (Pat.NamedSimple(mkToken Token.Identifier 4))
                Expect.equal key.Offset 4 "offset"
                Expect.equal key.Kind NodeKind.PatIdent "kind"
            }

            test "ofPat Wildcard -> PatWildcard at underscore's offset" {
                let key = CstKeys.ofPat (Pat.Wildcard(mkToken Token.Wildcard 5))
                Expect.equal key.Offset 5 "offset"
                Expect.equal key.Kind NodeKind.PatWildcard "kind"
            }

            test "ofBinding uses the binding pattern's NodeKey" {
                let patIdent = mkToken Token.Identifier 4

                let binding =
                    {
                        attributes = ValueNone
                        inlineToken = ValueNone
                        mutableToken = ValueNone
                        access = ValueNone
                        pattern = Pat.NamedSimple patIdent
                        typarDefns = ValueNone
                        argumentPats = ImmutableArray.Empty
                        returnType = ValueNone
                        equals = mkToken Token.OpEquality 6
                        expr = Expr.Const(Constant.Literal(mkToken Token.NumInt32 8))
                    }

                let key = CstKeys.ofBinding binding
                Expect.equal key.Offset 4 "the binding pattern's offset"
                Expect.equal key.Kind NodeKind.PatIdent "kind matches the binding pattern"
            }
        ]
