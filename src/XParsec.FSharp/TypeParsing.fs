namespace XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.Parsers
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser.ParseState


[<RequireQualifiedAccess>]
module Typar =

    let private pIdent =
        nextNonTriviaTokenIsL Token.Identifier "Expected identifier for type parameter"

    let pAnon = pWildcard |>> Typar.Anon

    let pNamed =
        parser {
            let! quote = pSingleQuote
            // This is an oddity of the parser. Technically F# allows bizarre things like
            // `' (* comment *)          T` as a type parameter name.
            let! ident = pIdent
            return Typar.Named(quote, ident)
        }

    let pStatic =
        parser {
            // Note: In Lexer, ^ might be OpConcatenate, need to check token text or handle appropriately
            // Assuming ^ comes as an operator or specific token.
            // Often ^identifier is lexed as a single token or Op + Ident.
            // Here assuming standard token stream:
            let! caret = pOpConcatenate
            let! ident = pIdent
            return Typar.Static(caret, ident)
        }

    let parse: Parser<Typar<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        dispatchNextNonTriviaTokenL
            [
                Token.Wildcard, pAnon
                Token.KWSingleQuote, pNamed
                // 'T lexed as a single TypeParameter token (not split into ' + T)
                Token.TypeParameter, (nextNonTriviaToken |>> fun tok -> Typar.Named(tok, tok))
                Token.OpConcatenate, pStatic
            ]
            "Typar"

[<RequireQualifiedAccess>]
module StaticTypars =
    // ^T
    let pSingle =
        parser {
            let! state = getUserState
            let! caret = nextNonTriviaTokenSatisfiesL (fun t -> tokenStringIs "^" t state) "Expected '^'"
            let! ident = nextNonTriviaTokenIsL Token.Identifier "Expected identifier"
            return StaticTypars.Single(caret, ident)
        }

    // (^T or ^U)
    let pOrList =
        parser {
            let! lParen = pLParen

            let pItem =
                parser {
                    let! tp = Typar.parse
                    let! orTok = pOr
                    return (tp, orTok)
                }

            // This is slightly loose (allows trailing 'or'), strict grammar might require sepBy but AST implies tuple structure
            let! items = many1 pItem
            let! rParen = pRParen
            return StaticTypars.OrList(lParen, List.ofSeq items, rParen)
        }

    let parse = choiceL [ pOrList; pSingle ] "Static Type Parameters"

[<RequireQualifiedAccess>]
module Constraint =

    // MemberSig is complex, using placeholder for now to satisfy type signature
    let private pMemberSig: Parser<MemberSig<SyntaxToken>, _, _, _, _> =
        // Consumes tokens until matching paren? Placeholder implementation.
        // In real impl, this parses property/method signatures.
        nextNonTriviaTokenIsL Token.Identifier "MemberSig Placeholder"
        |>> fun _ -> failwith "MemberSig parsing not implemented"

    let private pMemberTrait =
        parser {
            let! staticTypars = StaticTypars.parse
            let! colon = pColon
            let! lParen = pLParen
            let! sigs = pMemberSig
            let! rParen = pRParen
            return Constraint.MemberTrait(staticTypars, colon, lParen, sigs, rParen)
        }

    let private pDefaultConstructor (typar: Typar<_>) colon lParen (tokenNew: SyntaxToken) =
        parser {
            let! colonUnit = pColon
            //let! unitTok = nextNonTriviaTokenIsL Token.Unit "Expected '()'" // Simplified
            let! arrow = pArrowRight
            let! quoteT = nextNonTriviaTokenIsL Token.Identifier "Expected 'T" // Simplified
            let! rParen = pRParen
            // Re-map unitTok to ensure types align if AST expects specific tokens.
            // Note: AST asks for 'colonUnit' then 'arrow', logic adjusted to AST structure:
            return Constraint.DefaultConstructor(typar, colon, lParen, tokenNew, colonUnit, arrow, quoteT, rParen)
        }

    let private pTyparConstraints =
        parser {
            let! typar = Typar.parse

            // Check for :> (Coercion) vs : (Everything else)
            let! coercion = opt (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpUpcast) ":>") // :>

            match coercion with
            | ValueSome op ->
                let! typ = refType.Parser
                return Constraint.Coercion(typar, op, typ)
            | ValueNone ->
                let! colon = pColon

                // Branch based on next token
                let! state = getUserState
                let! token = nextNonTriviaToken

                match token.Token with
                | Token.KWNull -> return Constraint.Nullness(typar, colon, token)
                | Token.KWStruct -> return Constraint.Struct(typar, colon, token)
                | Token.KWDelegate ->
                    let! lAngle = pLessThan
                    let! t1 = refType.Parser
                    let! comma = pComma
                    let! t2 = refType.Parser
                    let! rAngle = pCloseTypeParams
                    return Constraint.Delegate(typar, colon, token, lAngle, t1, comma, t2, rAngle)
                | _ when tokenStringIs "equality" token state -> return Constraint.Equality(typar, colon, token)
                | _ when tokenStringIs "comparison" token state -> return Constraint.Comparison(typar, colon, token)
                | _ when tokenStringIs "unmanaged" token state -> return Constraint.Unmanaged(typar, colon, token)
                | _ when tokenStringIs "not" token state ->
                    let! structTok = pStruct
                    return Constraint.ReferenceType(typar, colon, token, structTok)
                | _ when tokenStringIs "enum" token state ->
                    let! lAngle = pLessThan
                    let! t = refType.Parser
                    let! rAngle = pCloseTypeParams
                    return Constraint.Enum(typar, colon, token, lAngle, t, rAngle)
                | Token.KWLParen ->
                    // Could be (new : unit -> 'T)
                    let! next = nextNonTriviaToken

                    if next.Token = Token.KWNew then
                        return! pDefaultConstructor typar colon token next
                    else
                        return! fail (Message "Unknown parenthesized constraint")
                | _ -> return! fail (Message "Unknown constraint type")
        }

    let parse = choiceL [ pMemberTrait; pTyparConstraints ] "Constraint"

[<RequireQualifiedAccess>]
module TyparDefns =
    let parse =
        parser {
            let! lAngle = pLessThan

            // Parse list of TyparDefn
            let! defns, _ =
                sepBy1
                    (parser {
                        let! attrs = opt Attributes.parse
                        let! tp = Typar.parse
                        return TyparDefn.TyparDefn(attrs, tp)
                    })
                    pComma

            let! constraints =
                opt (
                    parser {
                        let! whenTok = pWhen

                        let! constrs, _ = sepBy1 Constraint.parse pAnd

                        return TyparConstraints.TyparConstraints(whenTok, List.ofSeq constrs)
                    }
                )

            let! rAngle = pCloseTypeParams

            return TyparDefns.TyparDefns(lAngle, List.ofSeq defns, constraints, rAngle)
        }

[<RequireQualifiedAccess>]
module Type =

    let private pTypeArg =
        // Placeholder handling for TypeArg variations
        refType.Parser |>> TypeArg.Type

    let parseAtomic =
        choiceL
            [
                // (Type)
                parser {
                    let! l = pLParen
                    let! t = refType.Parser
                    let! r = pRParen
                    return Type.ParenType(l, t, r)
                }
                // struct (Type)
                parser {
                    let! s = pStruct
                    let! l = pLParen

                    let! ts, _ = sepBy refType.Parser pOpMultiply

                    let! r = pRParen
                    return Type.StructTupleType(s, l, List.ofSeq ts, r)
                }
                // #Type
                parser {
                    let! h = pHash
                    let! t = refType.Parser
                    return Type.AnonymousSubtype(h, t)
                }
                // 'a
                Typar.parse |>> Type.VarType
                // LongIdent or LongIdent<Types>
                parser {
                    let! lid = LongIdent.parse
                    // Check for Generic arguments <...>
                    let! genericPart =
                        opt (
                            parser {
                                let! l = pLessThan
                                let! args, _ = sepBy pTypeArg pComma
                                let! r = pCloseTypeParams
                                return (l, List.ofSeq args, r)
                            }
                        )

                    match genericPart with
                    | ValueSome(l, args, r) -> return Type.GenericType(lid, l, args, r)
                    | ValueNone -> return Type.NamedType(lid)
                }
            ]
            "Atomic Type"

    // Postfix operators: [] (Array), ident (Suffixed e.g. int list)
    // Note: This needs to be left-recursive elimination or chained.
    // Using a simple loop for postfix application.
    let private pPostfixType =
        parser {
            let! atom = parseAtomic

            let rec loop acc =
                choiceL
                    [
                        // Array: [] or [,]
                        parser {
                            let! l =
                                nextNonTriviaTokenSatisfiesL
                                    (fun t -> t.Token = Token.KWLArrayBracket || t.Token = Token.KWLBracket)
                                    "["
                            // Parse commas for rank
                            let! commas = many pComma

                            let! r =
                                nextNonTriviaTokenSatisfiesL
                                    (fun t -> t.Token = Token.KWRArrayBracket || t.Token = Token.KWRBracket)
                                    "]"

                            let newAcc = Type.ArrayType(acc, l, List.ofSeq commas, r)
                            return! loop newAcc
                        }
                        // Dotted nested type: Foo<int>.Builder
                        // The dot may be a regular OpDot token, or it may be
                        // embedded in a fused operator like ">." after pCloseTypeParams
                        // consumed the ">" and set CharsConsumedAfterTypeParams.
                        parser {
                            let! dot =
                                choiceL
                                    [
                                        pDot
                                        parser {
                                            let! reprocessed = reprocessedOperatorAfterTypeParams

                                            if reprocessed.PositionedToken.Token = Token.OpDot then
                                                return reprocessed
                                            else
                                                return! fail (Message "Expected '.'")
                                        }
                                    ]
                                    "."

                            let! lid = LongIdent.parse
                            let newAcc = Type.DottedType(acc, dot, lid)
                            return! loop newAcc
                        }
                        // Suffix: int list
                        // We only consume an identifier here if it's NOT a keyword,
                        // and not start of a new construct.
                        parser {
                            // Lookahead or logic to ensure this is a type suffix and not next token
                            let! lid = LongIdent.parse
                            // If we see <, it's a GenericType which is an atom, not a suffix to an existing type in this specific position
                            // F# parsing `int list` -> Suffixed(int, list).
                            // `int list list` -> Suffixed(Suffixed(int, list), list)
                            let newAcc = Type.SuffixedType(acc, lid)
                            return! loop newAcc
                        }
                        // Done
                        preturn acc
                    ]
                    "pPostfixType"

            return! loop atom
        }

    // Tuple: T * T * T
    let private pTupleType =
        parser {
            let! first = pPostfixType

            let! rest =
                many (
                    parser {
                        let! _ = pOpMultiply
                        return! pPostfixType
                    }
                )

            let restList = List.ofSeq rest

            if restList.IsEmpty then
                return first
            else
                return Type.TupleType(first :: restList)
        }

    // Function: T -> T -> T (Right Associative)
    // Recursive implementation
    let private pFunctionType =
        let rec pFunc () =
            parser {
                let! lhs = pTupleType

                return!
                    choice
                        [
                            // Arrow case
                            parser {
                                let! arrow = opt pArrowRight

                                match arrow with
                                | ValueSome arr ->
                                    let! rhs = pFunc ()
                                    return Type.FunctionType(lhs, arr, rhs)
                                | ValueNone -> return lhs
                            }
                            // If parsing fails, we return the inner success as the final type
                            preturn lhs
                        ]
            }

        pFunc ()

    // Entry point for simple types
    let parse = pFunctionType

    /// Parses a single type without consuming `*` as a tuple separator.
    /// Use in contexts where `*` is an explicit separator (e.g. union case fields).
    let parseField = pPostfixType

    // Initialize the recursive ref parser
    do refType.Set parse

module ReturnType =
    let parse: Parser<ReturnType<_>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! colon = pColon

            let! typ =
                Type.parse
                |> recoverWith
                    StoppingTokens.afterType
                    DiagnosticSeverity.Error
                    DiagnosticCode.MissingType
                    (fun toks ->
                        let m: Type<SyntaxToken> = Type<_>.Missing
                        if toks.IsEmpty then m else Type<_>.SkipsTokens(toks, m)
                    )

            return ReturnType(colon, typ)
        }
