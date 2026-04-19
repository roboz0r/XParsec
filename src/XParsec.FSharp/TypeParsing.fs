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
        nextNonTriviaIdentifierL "Expected identifier for type parameter"

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
    // '(' typar or typar or ... typar ')' — multiple typars joined by 'or'.
    // Requires at least one 'or' to distinguish from other '(...)' forms.
    let pOrList =
        parser {
            let! lParen = pLParen
            let! firstTypar = Typar.parse
            let! firstOr = pOr // at least one 'or' required
            let! restTypars, moreOrs = sepBy Typar.parse pOr
            let! rParen = pRParen

            let allTypars =
                ImmutableArray.CreateRange(
                    seq {
                        yield firstTypar
                        yield! restTypars
                    }
                )

            let allOrs =
                ImmutableArray.CreateRange(
                    seq {
                        yield firstOr
                        yield! moreOrs
                    }
                )

            return StaticTypars.OrList(lParen, allTypars, allOrs, rParen)
        }

    // typar — single typar; accepts both 'T (Named) and ^T (Static) since they are interchangeable in modern F#
    let pSingle = Typar.parse |>> StaticTypars.Single

    let parse = choiceL [ pOrList; pSingle ] "Static Type Parameters"

[<RequireQualifiedAccess>]
module Constraint =

    // Parses the member name inside a constraint member sig: plain ident or parenthesized operator.
    // For operators like (=), we consume '(' op ')' and return the op token as the ident,
    // since MemberSig.ident is a single token and the structural parens are not stored.
    let private pConstraintMemberName =
        choiceL
            [
                pIdent
                parser {
                    let! _lParen = pLParen
                    let! op = nextNonTriviaToken
                    let! _rParen = pRParen
                    return op
                }
            ]
            "member name (identifier or parenthesized operator)"

    // Parses the signature inside a constraint member: ident-or-(op) ':' Type
    // We parse a plain Type (not a CurriedSig) so that 'T * 'T -> bool is represented
    // as FunctionType(TupleType(...), bool) rather than a flattened CurriedSig arg-group.
    // The result is wrapped in a CurriedSig with no arg groups.
    let pConstraintMemberSig =
        parser {
            let! ident = pConstraintMemberName
            let! colon = pColon
            let! sigType = refType.Parser
            return MemberSig.MethodOrPropSig(ident, ValueNone, colon, CurriedSig(ImmutableArray.Empty, sigType))
        }

    // static-typars ':' '(' ['static'] 'member' member-sig ')'
    let private pMemberTrait =
        parser {
            let! staticTypars = StaticTypars.parse
            let! colon = pColon
            let! lParen = pLParen
            let! staticToken = opt (nextNonTriviaTokenIsL Token.KWStatic "static")
            let! memberToken = nextNonTriviaTokenIsL Token.KWMember "member"
            let! membersig = pConstraintMemberSig
            let! rParen = pRParen
            return Constraint.MemberTrait(staticTypars, colon, lParen, staticToken, memberToken, membersig, rParen)
        }

    let private pDefaultConstructor (typar: Typar<_>) colon lParen (tokenNew: SyntaxToken) =
        parser {
            let! colonUnit = pColon
            let! unitToken = nextNonTriviaIdentifierL "Expected 'unit'"
            let! arrow = pArrowRight
            let! resultTypar = Typar.parse
            let! rParen = pRParen

            return
                Constraint.DefaultConstructor(
                    typar,
                    colon,
                    lParen,
                    tokenNew,
                    colonUnit,
                    unitToken,
                    arrow,
                    resultTypar,
                    rParen
                )
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
                    let! next = nextNonTriviaToken

                    match next.Token with
                    | Token.KWStruct -> return Constraint.ReferenceType(typar, colon, token, next)
                    | Token.KWNull -> return Constraint.NotNull(typar, colon, token, next)
                    | _ -> return! fail (Message "Expected 'struct' or 'null' after 'not'")
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

    let private pDefaultConstraint =
        parser {
            let! defaultTok = pDefault
            let! typar = Typar.parse
            let! colon = pColon
            let! typ = refType.Parser
            return Constraint.Default(defaultTok, typar, colon, typ)
        }

    let parse =
        choiceL [ pDefaultConstraint; pMemberTrait; pTyparConstraints ] "Constraint"

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

                        let! constrs, ands = sepBy1 Constraint.parse pAnd

                        return TyparConstraints.TyparConstraints(whenTok, constrs, ands)
                    }
                )

            let! rAngle = pCloseTypeParams

            return TyparDefns.TyparDefns(lAngle, defns, constraints, rAngle)
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

                    let! ts, asterisks = sepBy refType.Parser pOpMultiply

                    let! r = pRParen
                    return Type.StructTupleType(s, l, ts, asterisks, r)
                }
                // #Type
                parser {
                    let! h = pHash
                    let! t = refType.Parser
                    return Type.AnonymousSubtype(h, t)
                }
                // null
                pNull |>> Type.Null
                // 'a
                Typar.parse |>> Type.VarType
                // (# "iltype" #) — IL intrinsic type
                parser {
                    let! l = nextNonTriviaTokenIsL Token.KWLHashParen "(#"
                    let! (kind, parts, instrClose) = parsePlainStringLiteral "IL instruction string"
                    let! r = nextNonTriviaTokenIsL Token.KWRHashParen "#)"
                    return Type.ILIntrinsic(l, kind, parts, instrClose, r)
                }
                // {| field: Type; ... |} — Anonymous record type
                parser {
                    let! lBraceBar = pLBraceBar

                    let pField =
                        parser {
                            let! ident = nextNonTriviaIdentifierL "field name"
                            let! colon = nextNonTriviaTokenIsL Token.OpColon ":"
                            let! typ = refType.Parser
                            return AnonRecordField(ident, colon, typ)
                        }

                    let! fields, seps = sepBy1 pField (nextNonTriviaTokenIsL Token.OpSemicolon ";")
                    let! rBraceBar = pRBraceBar
                    return Type.AnonRecordType(lBraceBar, fields, seps, rBraceBar)
                }
                // LongIdent or LongIdent<Types>
                parser {
                    let! lid = LongIdent.parse
                    // Check for Generic arguments <...>
                    let! genericPart =
                        opt (
                            parser {
                                let! l = pLessThan
                                let! args, commas = sepBy pTypeArg pComma
                                let! r = pCloseTypeParams
                                return (l, args, commas, r)
                            }
                        )

                    match genericPart with
                    | ValueSome(l, args, commas, r) -> return Type.GenericType(lid, l, args, commas, r)
                    | ValueNone -> return Type.NamedType(lid)
                }
            ]
            "Atomic Type"

    // Matches '[' or '[|' as an array-rank open bracket in postfix type position.
    let private pArrayOpenBracket =
        nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWLArrayBracket || t.Token = Token.KWLBracket) "["

    // Matches ']' or '|]' as an array-rank close bracket in postfix type position.
    let private pArrayCloseBracket =
        nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWRArrayBracket || t.Token = Token.KWRBracket) "]"

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
                            let! l = pArrayOpenBracket
                            // Parse commas for rank
                            let! commas = many pComma
                            let! r = pArrayCloseBracket

                            let newAcc = Type.ArrayType(acc, l, commas, r)
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

    // Subtype constraint: 'T :> IDisposable (flexible type annotation)
    let private pSubtypeType =
        parser {
            let! lhs = pPostfixType

            match lhs with
            | Type.VarType typar ->
                let! colonGreater = opt (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpUpcast) ":>")

                match colonGreater with
                | ValueSome op ->
                    let! rhs = pPostfixType
                    return Type.SubtypeConstraint(typar, op, rhs)
                | ValueNone -> return lhs
            | _ -> return lhs
        }

    // Union: T | T (for nullable reference types: string | null)
    let private pBarType =
        parser {
            let! bar = pBar
            let! rhs = pSubtypeType
            return (bar, rhs)
        }

    let private pUnionType =
        parser {
            let! lhs = pSubtypeType

            match! opt pBarType with
            | ValueSome(bar, rhs) -> return Type.UnionType(lhs, bar, rhs)
            | ValueNone -> return lhs
        }

    // Tuple: T * T * T
    let private pTupleType =
        parser {
            let! types, asterisks = sepBy1 pUnionType pOpMultiply

            if types.Length = 1 then
                return types[0]
            else
                return Type.TupleType(types, asterisks)
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

    let private pWhenConstraints =
        parser {
            let! whenTok = pWhen
            let! constrs, ands = sepBy1 Constraint.parse pAnd
            return TyparConstraints.TyparConstraints(whenTok, constrs, ands)
        }

    // Entry point for simple types
    let parse =
        parser {
            let! typ = pFunctionType

            match! opt pWhenConstraints with
            | ValueSome constraints -> return Type.WhenConstrainedType(typ, constraints)
            | ValueNone -> return typ
        }

    /// Parses a single type without consuming `*` as a tuple separator.
    /// Use in contexts where `*` is an explicit separator (e.g. union case fields).
    let parseField = pPostfixType

    // Variants that do NOT consume `|` as a nullable-ref type union.
    // Used in contexts where `|` separates something else (e.g. DU case separators).
    let private pTupleTypeNoUnion =
        parser {
            let! types, asterisks = sepBy1 pSubtypeType pOpMultiply

            if types.Length = 1 then
                return types[0]
            else
                return Type.TupleType(types, asterisks)
        }

    let private pFunctionTypeNoUnion =
        let rec pFunc () =
            parser {
                let! lhs = pTupleTypeNoUnion

                return!
                    choice
                        [
                            parser {
                                let! arrow = opt pArrowRight

                                match arrow with
                                | ValueSome arr ->
                                    let! rhs = pFunc ()
                                    return Type.FunctionType(lhs, arr, rhs)
                                | ValueNone -> return lhs
                            }
                            preturn lhs
                        ]
            }

        pFunc ()

    /// Type parser that does not treat `|` as a type-union separator.
    /// Use in contexts where `|` has a different meaning (e.g. DU case separator).
    let parseNoUnion =
        parser {
            let! typ = pFunctionTypeNoUnion

            match! opt pWhenConstraints with
            | ValueSome constraints -> return Type.WhenConstrainedType(typ, constraints)
            | ValueNone -> return typ
        }

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
                        if toks.IsEmpty then m else Type<_>.SkipsTokens(toks)
                    )

            return ReturnType(colon, typ)
        }
