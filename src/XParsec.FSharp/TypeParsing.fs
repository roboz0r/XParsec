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
        nextSyntaxIdentifierLMsg "Expected identifier for type parameter"

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

    let parse: FSParser<Typar<SyntaxToken>> =
        dispatchNextSyntaxTokenL
            [
                Token.Wildcard, pAnon
                Token.KWSingleQuote, pNamed
                // 'T lexed as a single TypeParameter token (not split into ' + T)
                Token.TypeParameter, (nextSyntaxToken |>> fun tok -> Typar.Named(tok, tok))
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
module WithClause =
    // Parses the property-accessor clause shared by abstract member signatures and
    // SRTP property-trait constraints:
    //   with get | with set | with get,set | with set,get
    // Lives in TypeParsing.fs (not TypeDefnParsing.fs) because pConstraintMemberSig
    // here needs it, and TypeParsing.fs compiles earlier in the DAG.

    let private errExpectedGet: ErrorType<PositionedToken, ParseState> =
        Message "Expected 'get'"

    let private errExpectedSet: ErrorType<PositionedToken, ParseState> =
        Message "Expected 'set'"

    let private pGet =
        parser {
            let! getTok = nextSyntaxTokenIsLMsg Token.Identifier "get"
            let! state = getUserState

            if tokenStringIs "get" getTok state then
                return getTok
            else
                return! fail errExpectedGet
        }

    let private pSet =
        parser {
            let! setTok = nextSyntaxTokenIsLMsg Token.Identifier "set"
            let! state = getUserState

            if tokenStringIs "set" setTok state then
                return setTok
            else
                return! fail errExpectedSet
        }

    let private pGetSet =
        choiceL
            [
                parser {
                    let! getTok = pGet

                    let! maybeSet =
                        opt (
                            parser {
                                let! comma = pComma
                                let! setTok = pSet
                                return setTok
                            }
                        )

                    return getTok, maybeSet
                }
                parser {
                    let! setTok = pSet

                    let! maybeGet =
                        opt (
                            parser {
                                let! comma = pComma
                                let! getTok = pGet
                                return getTok
                            }
                        )

                    return setTok, maybeGet
                }
            ]
            ""

    /// Parses `with get` / `with set` / `with get,set` / `with set,get`.
    /// Returns struct (withToken, (firstAccessor, optional second accessor)).
    let parse =
        parser {
            let! withTok = pWith
            let! getSet = pGetSet
            return struct (withTok, getSet)
        }

[<RequireQualifiedAccess>]
module Constraint =

    let private errExpectedStructOrNullAfterNot: ErrorType<PositionedToken, ParseState> =
        Message "Expected 'struct' or 'null' after 'not'"

    let private errUnknownParenConstraint: ErrorType<PositionedToken, ParseState> =
        Message "Unknown parenthesized constraint"

    let private errUnknownConstraintType: ErrorType<PositionedToken, ParseState> =
        Message "Unknown constraint type"

    // Hoisted helpers — allocate Message once at module load instead of per invocation.
    let private pUnitIdent = nextSyntaxIdentifierLMsg "Expected 'unit'"

    let private pUpcast =
        nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.OpUpcast) ":>"

    // Parses the member name inside a constraint member sig: plain ident, parenthesized operator,
    // or active-pattern name. Returns IdentOrOp to match MemberSig.ident.
    let private pConstraintMemberName: FSParser<IdentOrOp<SyntaxToken>> =
        IdentOrOp.parse

    // Parses the signature inside a constraint member: ident-or-(op) ':' curried-sig ('with' get/set)?
    // Uses the same CurriedSig grammar as ordinary abstract member sigs (via refCurriedSig)
    // so multi-arg curried shapes like `'T * 'T -> bool` land in CurriedSig.args rather than
    // being flattened into a FunctionType chain under an empty CurriedSig.
    let pConstraintMemberSig =
        parser {
            let! ident = pConstraintMemberName
            let! colon = pColon
            let! sign = refCurriedSig.Parser
            let! withClause = opt WithClause.parse

            match withClause with
            | ValueSome(withTok, getSet) -> return MemberSig.PropSig(ident, ValueNone, colon, sign, withTok, getSet)
            | ValueNone -> return MemberSig.MethodOrPropSig(ident, ValueNone, colon, sign)
        }

    // static-typars ':' '(' ['static'] 'member' member-sig ')'
    let private pMemberTrait =
        parser {
            let! staticTypars = StaticTypars.parse
            let! colon = pColon
            let! lParen = pLParen
            let! staticToken = opt pStatic
            let! memberToken = pMember
            let! membersig = pConstraintMemberSig
            let! rParen = pRParen
            return Constraint.MemberTrait(staticTypars, colon, lParen, staticToken, memberToken, membersig, rParen)
        }

    let private pDefaultConstructor (typar: Typar<_>) colon lParen (tokenNew: SyntaxToken) =
        parser {
            let! colonUnit = pColon
            let! unitToken = pUnitIdent
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
            let! coercion = opt pUpcast // :>

            match coercion with
            | ValueSome op ->
                let! typ = refType.Parser
                return Constraint.Coercion(typar, op, typ)
            | ValueNone ->
                let! colon = pColon

                // Branch based on next token
                let! state = getUserState
                let! token = nextSyntaxToken

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
                    let! next = nextSyntaxToken

                    match next.Token with
                    | Token.KWStruct -> return Constraint.ReferenceType(typar, colon, token, next)
                    | Token.KWNull -> return Constraint.NotNull(typar, colon, token, next)
                    | _ -> return! fail errExpectedStructOrNullAfterNot
                | _ when tokenStringIs "enum" token state ->
                    let! lAngle = pLessThan
                    let! t = refType.Parser
                    let! rAngle = pCloseTypeParams
                    return Constraint.Enum(typar, colon, token, lAngle, t, rAngle)
                | Token.KWLParen ->
                    // Could be (new : unit -> 'T)
                    let! next = nextSyntaxToken

                    if next.Token = Token.KWNew then
                        return! pDefaultConstructor typar colon token next
                    else
                        return! fail errUnknownParenConstraint
                | _ -> return! fail errUnknownConstraintType
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

    // Hoisted helpers — allocate Message + closure once at module load instead of per invocation.
    let private pLHashParen = nextSyntaxTokenIsLMsg Token.KWLHashParen "(#"
    let private pRHashParen = nextSyntaxTokenIsLMsg Token.KWRHashParen "#)"
    let private pAnonRecFieldIdent = nextSyntaxIdentifierLMsg "field name"

    let private pUpcast =
        nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.OpUpcast) ":>"

    let private errRetryAsMeasureArg: ErrorType<PositionedToken, ParseState> =
        Message "Retry as measure argument"

    // Generic type-argument parser. Tries Type first; if Type.parse leaves a measure
    // operator dangling at the peek position (/, *, ^, fused ^- / ^+), backtracks and
    // re-parses the slot as a Measure. Mirrors the abbrev measure-retry at
    // TypeDefnParsing.fs:1219-1243. Single-identifier args (Foo<kg>) take the Type branch
    // — the type checker resolves the measure-vs-type ambiguity using typar sort info.
    let private pTypeArg =
        choice
            [
                parser {
                    let! tNormal = refType.Parser
                    let! peekAfter = peekNextSyntaxToken
                    let! state = getUserState
                    let startsWithCaret = ParseState.tokenStringStartsWith "^" peekAfter state

                    if
                        peekAfter.Token = Token.OpDivision
                        || peekAfter.Token = Token.OpConcatenate
                        || peekAfter.Token = Token.OpMultiply
                        || startsWithCaret
                    then
                        return! fail errRetryAsMeasureArg
                    else
                        return TypeArg.Type tNormal
                }
                (refMeasure.Parser |>> TypeArg.Measure)
            ]

    // Hoisted: field-parser for anon record types. Previously defined inline inside
    // `parseAtomic`'s choiceL arm, which rebuilt it on every outer invocation.
    let private pAnonRecField =
        parser {
            let! ident = pAnonRecFieldIdent
            let! colon = pColon
            let! typ = refType.Parser
            return AnonRecordField(ident, colon, typ)
        }

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
                    let! l = pLHashParen
                    let! (kind, parts, instrClose) = parsePlainStringLiteral "IL instruction string"
                    let! r = pRHashParen
                    return Type.ILIntrinsic(l, kind, parts, instrClose, r)
                }
                // {| field: Type; ... |} — Anonymous record type
                parser {
                    let! lBraceBar = pLBraceBar
                    let! fields, seps = sepBy1 pAnonRecField pSemi
                    let! rBraceBar = pRBraceBar
                    return Type.AnonRecordType(lBraceBar, fields, seps, rBraceBar)
                }
                // LongIdent or LongIdent<Types>
                parser {
                    let! lid = LongIdent.parse
                    // Commit to generic-args parsing once '<' is consumed: a failure inside
                    // the args list (e.g. an unparseable arg, missing '>') should propagate
                    // up so outer recoverWith (ReturnType.parse, etc.) can skip to the right
                    // boundary. The previous shape wrapped the whole block in `opt`, which
                    // silently fell back to `NamedType lid` and left the offending tokens in
                    // the stream, causing the binding to fail at `pEquals` with no recovery.
                    let! lessOpt = opt pLessThan

                    match lessOpt with
                    | ValueNone -> return Type.NamedType(lid)
                    | ValueSome l ->
                        let! args, commas = sepBy pTypeArg pComma
                        let! r = pCloseTypeParams
                        return Type.GenericType(lid, l, args, commas, r)
                }
            ]
            "Atomic Type"

    // Matches '[' or '[|' as an array-rank open bracket in postfix type position.
    let private pArrayOpenBracket =
        nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.KWLArrayBracket || t.Token = Token.KWLBracket) "["

    // Matches ']' or '|]' as an array-rank close bracket in postfix type position.
    let private pArrayCloseBracket =
        nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.KWRArrayBracket || t.Token = Token.KWRBracket) "]"

    let private errExpectedDot: ErrorType<PositionedToken, ParseState> =
        Message "Expected '.'"

    // A dot that may appear as a regular OpDot token or embedded in a fused operator
    // like ">." after pCloseTypeParams consumed the ">" and set CharsConsumedAfterTypeParams.
    let private pPostfixDot =
        choiceL
            [
                pDot
                parser {
                    let! reprocessed = reprocessedOperatorAfterTypeParams

                    if reprocessed.PositionedToken.Token = Token.OpDot then
                        return reprocessed
                    else
                        return! fail errExpectedDot
                }
            ]
            "."

    // Array continuation: [] or [,] or [|]
    let private pPostfixArrayCont =
        parser {
            let! l = pArrayOpenBracket
            let! commas = many pComma
            let! r = pArrayCloseBracket
            return struct (l, commas, r)
        }

    // Dotted nested type continuation: .Foo.Bar
    let private pPostfixDottedCont =
        parser {
            let! dot = pPostfixDot
            let! lid = LongIdent.parse
            return struct (dot, lid)
        }

    // Suffix continuation: list (as in `int list`)
    let private pPostfixSuffixCont: FSParser<LongIdent<SyntaxToken>> = LongIdent.parse

    // Postfix operators: [] (Array), .Ident (Dotted), ident (Suffixed e.g. int list).
    // Peek once per iteration and dispatch to the single matching sub-parser rather than
    // trying all three via `opt` (which re-peeks the same token inside each attempt).
    // The three leading-token sets are disjoint, so the peeked token uniquely selects
    // at most one branch; anything else terminates the postfix chain.
    //
    // One extra case: when a generic type application consumed a virtual `>` from a fused
    // operator (e.g. `>.` in `Foo<int>.Bar`), the reader is still positioned at the fused
    // token with CharsConsumedAfterTypeParams > 0. `pPostfixDottedCont`'s inner `pPostfixDot`
    // knows how to reprocess such a token into `.`, but it can also legitimately fail (e.g.
    // if the tail is `>>` not `>.`), in which case the postfix chain must terminate cleanly.
    // For that case we use `opt pPostfixDottedCont` so a non-dot reprocessed operator rolls
    // back instead of propagating as a parse error.
    let private pPostfixType: FSParser<Type<SyntaxToken>> =
        fun reader ->
            match parseAtomic reader with
            | Error e -> Error e
            | Ok atom ->
                let mutable acc = atom
                let mutable errOpt = ValueNone
                let mutable keepGoing = true

                while keepGoing && errOpt.IsNone do
                    match peekNextSyntaxToken reader with
                    | Error _ -> keepGoing <- false
                    | Ok peeked ->
                        let state = reader.State

                        if state.CharsConsumedAfterTypeParams > 0 && TokenInfo.isOperator peeked.Token then
                            match opt pPostfixDottedCont reader with
                            | Ok(ValueSome(struct (dot, lid))) -> acc <- Type.DottedType(acc, dot, lid)
                            | Ok ValueNone -> keepGoing <- false
                            | Error e -> errOpt <- ValueSome e
                        else
                            match peeked.Token with
                            | Token.KWLBracket
                            | Token.KWLArrayBracket ->
                                match pPostfixArrayCont reader with
                                | Ok(struct (l, commas, r)) -> acc <- Type.ArrayType(acc, l, commas, r)
                                | Error e -> errOpt <- ValueSome e
                            | Token.OpDot ->
                                match pPostfixDottedCont reader with
                                | Ok(struct (dot, lid)) -> acc <- Type.DottedType(acc, dot, lid)
                                | Error e -> errOpt <- ValueSome e
                            | Token.Identifier
                            | Token.BacktickedIdentifier
                            | Token.UnterminatedBacktickedIdentifier ->
                                match pPostfixSuffixCont reader with
                                | Ok lid -> acc <- Type.SuffixedType(acc, lid)
                                | Error e -> errOpt <- ValueSome e
                            | _ -> keepGoing <- false

                match errOpt with
                | ValueSome e -> Error e
                | ValueNone -> Ok acc

    // Subtype constraint: 'T :> IDisposable (flexible type annotation)
    let private pSubtypeType =
        parser {
            let! lhs = pPostfixType

            match lhs with
            | Type.VarType typar ->
                let! colonGreater = opt pUpcast

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
    // Iterative implementation: collect a -> b -> c as (a, arrow0)(b, arrow1)(c) then
    // right-fold into FunctionType(a, arrow0, FunctionType(b, arrow1, c)).
    let private pFunctionType =
        parser {
            let! first = pTupleType
            let mutable current = first
            let mutable pairs: struct (Type<SyntaxToken> * SyntaxToken) list = []
            let mutable keepGoing = true

            while keepGoing do
                match! opt pArrowRight with
                | ValueSome arr ->
                    let! next = pTupleType
                    pairs <- struct (current, arr) :: pairs
                    current <- next
                | ValueNone -> keepGoing <- false

            let mutable acc = current

            for p in pairs do
                let struct (t, arr) = p
                acc <- Type.FunctionType(t, arr, acc)

            return acc
        }

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

    /// Like `parseField` but ALSO accepts nullable-union types (`T | null`).
    /// Use in CurriedSig/UncurriedSig arg-spec position, where `*` separates args
    /// but `|` inside an arg-type denotes a nullable union (F# 9+).
    let parseArgField = pUnionType

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
        parser {
            let! first = pTupleTypeNoUnion
            let mutable current = first
            let mutable pairs: struct (Type<SyntaxToken> * SyntaxToken) list = []
            let mutable keepGoing = true

            while keepGoing do
                match! opt pArrowRight with
                | ValueSome arr ->
                    let! next = pTupleTypeNoUnion
                    pairs <- struct (current, arr) :: pairs
                    current <- next
                | ValueNone -> keepGoing <- false

            let mutable acc = current

            for p in pairs do
                let struct (t, arr) = p
                acc <- Type.FunctionType(t, arr, acc)

            return acc
        }

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
    let parse: FSParser<ReturnType<_>> =
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
