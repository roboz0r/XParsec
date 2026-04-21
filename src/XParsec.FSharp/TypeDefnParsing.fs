namespace XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.Parsers
open XParsec.OperatorParsing
open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser.SyntaxToken
open XParsec.FSharp.Parser.ParseState


[<RequireQualifiedAccess>]
module PrimaryConstrArgs =
    let parse: Parser<PrimaryConstrArgs<SyntaxToken>, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            let! access = opt pAccessModifier
            let! lParen = pLParen
            let! pat = opt Pat.parse
            let! rParen = pRParen
            return PrimaryConstrArgs(attrs, access, lParen, pat, rParen)
        }

[<RequireQualifiedAccess>]
module TypeName =
    /// Parse ML-style prefix type parameters: 'T or ('a, 'b)
    let private pPrefixTypars: Parser<PrefixTypars<SyntaxToken>, _, _, _> =
        choiceL
            [
                // ('a, 'b) — multiple prefix typars
                parser {
                    let! lParen = pLParen
                    let! typars, commas = sepBy1 Typar.parse pComma
                    let! rParen = pRParen
                    return PrefixTypars.Multiple(lParen, typars, commas, rParen)
                }
                // 'T — single prefix typar
                Typar.parse |>> PrefixTypars.Single
            ]
            "Prefix type parameters"

    /// Parses a TypeName using pre-parsed attributes (from before the `type` keyword).
    let parseWithAttrs (attrs: Attributes<SyntaxToken> voption) : Parser<TypeName<SyntaxToken>, _, _, _> =
        parser {
            let! access = opt pAccessModifier
            let! prefixTypars = opt pPrefixTypars
            let! ident = LongIdent.parse
            let! typars = opt TyparDefns.parse

            // Parse optional postfix 'when' constraints (outside angle brackets):
            // type Set<'T, 'Tag> when 'Tag :> IComparer<'T> = ...
            let! postfixConstraints =
                opt (
                    parser {
                        let! whenTok = pWhen
                        let! constrs, ands = sepBy1 Constraint.parse pAnd
                        return TyparConstraints.TyparConstraints(whenTok, constrs, ands)
                    }
                )

            return TypeName(attrs, access, prefixTypars, ident, typars, postfixConstraints)
        }

    let parse: Parser<TypeName<SyntaxToken>, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            return! parseWithAttrs attrs
        }

[<RequireQualifiedAccess>]
module AsDefn =
    let parse: Parser<AsDefn<SyntaxToken>, _, _, _> =
        parser {
            let! asTok = pAs
            let! ident = pIdent
            return AsDefn(asTok, ident)
        }

// ----------------------------------------------------------------------------
// Signatures (MemberSig, ArgSpec)
// ----------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module ArgNameSpec =
    let parse =
        parser {
            let! optional = opt pQuestionMark
            let! ident = pIdent
            let! colon = pColon
            return ArgNameSpec.ArgNameSpec(optional, ident, colon)
        }

[<RequireQualifiedAccess>]
module ArgSpec =
    let parse =
        parser {
            let! attrs = opt Attributes.parse
            // Try parse "name :" first
            let! nameSpec = opt ArgNameSpec.parse
            // Use parseField (pPostfixType) not Type.parse: * separates args, -> separates arg groups
            let! typ = Type.parseField
            return ArgSpec.ArgSpec(attrs, nameSpec, typ)
        }


[<RequireQualifiedAccess>]
module ArgsSpec =
    let parse =
        sepBy1 ArgSpec.parse pOpMultiply
        |>> fun struct (args, asterisks) -> ArgsSpec.ArgsSpec(args, asterisks)

[<RequireQualifiedAccess>]
module CurriedSig =

    let private pArgsSpec = ArgsSpec.parse .>>. pArrowRight

    let parse: Parser<CurriedSig<SyntaxToken>, _, _, _> =
        parser {
            // Use `many` (not `many1Till`) so properties with zero `->` are handled.
            // For each arg group, greedily try pArgsSpec (args -> ), stop when it fails.
            // The remaining is the return type.
            let! args = many pArgsSpec
            let! ret = Type.parse
            return CurriedSig(args, ret)
        }

[<RequireQualifiedAccess>]
module UncurriedSig =

    let parse: Parser<UncurriedSig<SyntaxToken>, _, _, _> =
        parser {
            let! args = ArgsSpec.parse
            let! arrow = pArrowRight
            let! retType = Type.parse
            return UncurriedSig.UncurriedSig(args, arrow, retType)
        }

/// Signature for an abstract member (method or property)
[<RequireQualifiedAccess>]
module MemberSig =

    let private errExpectedGet: ErrorType<PositionedToken, ParseState> =
        Message "Expected 'get'"

    let private errExpectedSet: ErrorType<PositionedToken, ParseState> =
        Message "Expected 'set'"

    (*
member-sig :=
    ident typar-defns~opt : curried-sig -- method or property signature
    ident typar-defns~opt : curried-sig with get -- property signature
    ident typar-defns~opt : curried-sig with set -- property signature
    ident typar-defns~opt : curried-sig with get,set -- property signature
    ident typar-defns~opt : curried-sig with set,get -- property signature
*)

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

    let pWithClause =
        parser {
            let! withTok = pWith
            let! getSet = pGetSet
            return struct (withTok, getSet)
        }

    let parse: Parser<MemberSig<SyntaxToken>, _, _, _> =
        parser {
            let! ident = pIdent
            let! typars = opt TyparDefns.parse
            let! colon = pColon
            let! sigType = CurriedSig.parse

            // Check for optional 'with' get/set
            let! withClause = opt pWithClause

            match withClause with
            | ValueSome(withTok, getSet) -> return MemberSig.PropSig(ident, typars, colon, sigType, withTok, getSet)
            | ValueNone -> return MemberSig.MethodOrPropSig(ident, typars, colon, sigType)
        }


// ----------------------------------------------------------------------------
// Member Definitions (Method, Property, Ctor)
// ----------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module MethodOrPropDefn =

    let private errExpectedDotAfterUnderscore: ErrorType<PositionedToken, ParseState> =
        Message "Expected '.' after '_' in member definition"

    // Distinguishes:
    // member x.P = ... (Property)
    // member x.M args = ... (Method)
    // member x.P with get ... (PropWithGetSet)

    let pPropertyWithGetSet =
        parser {
            let! ident = pIdent
            let! w = pWith
            let! bindings, ands = Binding.parseSepByAnd1 ValueNone
            return fun thisIdent -> MethodOrPropDefn.PropertyWithGetSet(thisIdent, ident, w, bindings, ands)
        }

    let parse: Parser<MethodOrPropDefn<SyntaxToken>, _, _, _> =
        let buildMethodOrProp thisIdent (binding: Binding<SyntaxToken>) =
            if binding.argumentPats.IsEmpty then
                MethodOrPropDefn.Property(thisIdent, binding)
            else
                MethodOrPropDefn.Method(thisIdent, binding)

        parser {
            match! peekNextSyntaxToken with
            | tWild when tWild.Token = Token.Wildcard ->
                // `_` as self-identifier: must be followed by `.`
                let! underscore = consumePeeked tWild

                match! peekNextSyntaxToken with
                | tDot when tDot.Token = Token.OpDot ->
                    let! dot = consumePeeked tDot
                    let thisIdent = ValueSome struct (underscore, dot)

                    return!
                        choiceL
                            [
                                Binding.parse ValueNone |>> buildMethodOrProp thisIdent
                                pPropertyWithGetSet |>> fun f -> f thisIdent
                            ]
                            "MethodOrPropDefn after _."

                | _ -> return! fail errExpectedDotAfterUnderscore

            | _ ->

                match! opt pIdent with
                | ValueSome ident ->
                    match! peekNextSyntaxToken with
                    | t when t.Token = Token.OpDot ->
                        let! dot = consumePeeked t
                        let thisIdent = ValueSome struct (ident, dot)

                        return!
                            choiceL
                                [
                                    Binding.parse ValueNone |>> buildMethodOrProp thisIdent
                                    pPropertyWithGetSet |>> fun f -> f thisIdent
                                ]
                                "MethodOrPropDefn after dot"

                    | t when t.Token = Token.OpEquality ->
                        let! eq = consumePeeked t
                        // Grammar: member body is typedSeqExprBlock
                        let! expr = refTypedSeqExprBlock.Parser

                        let binding =
                            {
                                attributes = ValueNone
                                inlineToken = ValueNone
                                mutableToken = ValueNone
                                fixedToken = ValueNone
                                access = ValueNone
                                headPat = Pat.NamedSimple ident
                                typarDefns = ValueNone
                                argumentPats = ImmutableArray.Empty
                                returnType = ValueNone
                                equals = eq
                                expr = expr
                            }

                        return MethodOrPropDefn.Property(ValueNone, binding)

                    | t when t.Token = Token.OpColon ->
                        // Property with explicit return type (e.g., static member Empty : Foo = expr)
                        let! returnType = ReturnType.parse
                        let! equals = pEquals
                        // Grammar: member body is typedSeqExprBlock
                        let! expr = refTypedSeqExprBlock.Parser

                        let binding =
                            {
                                attributes = ValueNone
                                inlineToken = ValueNone
                                mutableToken = ValueNone
                                fixedToken = ValueNone
                                access = ValueNone
                                headPat = Pat.NamedSimple ident
                                typarDefns = ValueNone
                                argumentPats = ImmutableArray.Empty
                                returnType = ValueSome returnType
                                equals = equals
                                expr = expr
                            }

                        return MethodOrPropDefn.Property(ValueNone, binding)

                    | t when t.Token = Token.KWWith ->
                        // Property with explicit accessors (e.g., static member BuildPhase with get () = ... and set v = ...)
                        // The property ident was already consumed above.
                        let! w = consumePeeked t
                        let! bindings, ands = Binding.parseSepByAnd1 ValueNone
                        return MethodOrPropDefn.PropertyWithGetSet(ValueNone, ident, w, bindings, ands)

                    | _ ->
                        // No self-identifier prefix (e.g., static member Create(args) = body)
                        // The ident we consumed is the method/property name itself.
                        return!
                            choiceL
                                [
                                    parser {
                                        let! typarDefns = opt TyparDefns.parse
                                        let! argumentPats = Pat.parseAtomicBindingArgMany1
                                        let! returnType = opt ReturnType.parse
                                        let! equals = pEquals
                                        // Grammar: member body is typedSeqExprBlock
                                        let! expr = refTypedSeqExprBlock.Parser

                                        let binding =
                                            {
                                                attributes = ValueNone
                                                inlineToken = ValueNone
                                                mutableToken = ValueNone
                                                fixedToken = ValueNone
                                                access = ValueNone
                                                headPat = Pat.NamedSimple ident
                                                typarDefns = typarDefns
                                                argumentPats = argumentPats
                                                returnType = returnType
                                                equals = equals
                                                expr = expr
                                            }

                                        return MethodOrPropDefn.Method(ValueNone, binding)
                                    }
                                    pPropertyWithGetSet |>> fun f -> f ValueNone
                                ]
                                "MethodOrPropDefn no self-ident"
                | ValueNone ->
                    return!
                        choiceL
                            [
                                Binding.parse ValueNone |>> buildMethodOrProp ValueNone
                                pPropertyWithGetSet |>> fun f -> f ValueNone
                            ]
                            "MethodOrPropDefn no ident"
        }

[<AutoOpen>]
module internal TypeDefnHelpers =
    // Forward reference for MemberDefn (overriding the one in your stub if needed,
    // or we use the existing refMemberDefn from your provided MemberHelpers)
    let refAdditionalConstrExpr = FSRefParser<AdditionalConstrExpr<SyntaxToken>>()

[<RequireQualifiedAccess>]
module AdditionalConstrExpr =

    let private pInit =
        choiceL
            [
                parser {
                    let! lBrace = pLBrace
                    // Helper for inherits: inherit Type(expr) — now optional
                    let! inherits =
                        opt (
                            parser {
                                let! inh = pInherit
                                let! t = Type.parse
                                let! e = opt Expr.parseAtomic
                                return ClassInheritsDecl.ClassInheritsDecl(inh, t, e)
                            }
                        )

                    let! inits = many FieldInitializer.parse
                    let! rBrace = pRBrace
                    return AdditionalConstrInitExpr.Explicit(lBrace, inherits, inits, rBrace)
                }
                parser {
                    let! newTok = pNew
                    let! t = Type.parse
                    let! e = Expr.parseSeqBlock
                    return AdditionalConstrInitExpr.Delegated(newTok, t, e)
                }
                parser {
                    let! e = Expr.parseSeqBlock
                    return AdditionalConstrInitExpr.Expression(e)
                }
            ]
            "Constructor Init"

    let parse =
        parser {
            // Parse constructor init expression
            let! init = pInit
            let initExpr = AdditionalConstrExpr.Init init

            // Check for optional 'then' continuation
            let! thenClause =
                opt (
                    parser {
                        let! thenTok = pThen
                        let! expr = Expr.parseSeqBlock
                        return (thenTok, expr)
                    }
                )

            match thenClause with
            | ValueSome(thenTok, expr) -> return AdditionalConstrExpr.SequenceBefore(initExpr, thenTok, expr)
            | ValueNone -> return initExpr
        }

    do refAdditionalConstrExpr.Set parse

[<RequireQualifiedAccess>]
module AutoPropDefn =
    let parse: Parser<MethodOrPropDefn<SyntaxToken>, _, _, _> =
        parser {
            let! valTok = pVal
            let! access = opt pAccessModifier
            let! ident = pIdent

            let! returnType =
                opt (
                    parser {
                        let! colon = pColon
                        let! t = Type.parse
                        return ReturnType(colon, t)
                    }
                )

            let! eq = pEquals
            let! expr = Expr.parse

            // Handle post-expression type annotation: `val X = expr : Type with get, set`
            // The expression parser doesn't consume `:` at top level, so we handle it here.
            let! colonOpt = opt peekNextSyntaxToken

            let! expr =
                match colonOpt with
                | ValueSome t when t.Token = Token.OpColon ->
                    parser {
                        let! colon = consumePeeked t
                        let! typ = Type.parse
                        return Expr.TypeAnnotation(expr, colon, typ)
                    }
                | _ -> preturn expr

            let! withClause = opt MemberSig.pWithClause

            return
                match withClause with
                | ValueSome struct (withTok, (acc1, acc2)) ->
                    MethodOrPropDefn.AutoProperty(
                        valTok,
                        access,
                        ident,
                        returnType,
                        eq,
                        expr,
                        ValueSome struct (withTok, acc1, acc2)
                    )
                | ValueNone -> MethodOrPropDefn.AutoProperty(valTok, access, ident, returnType, eq, expr, ValueNone)
        }

[<RequireQualifiedAccess>]
module MemberDefn =
    let private errExpectedMemberOrValAfterStatic: ErrorType<PositionedToken, ParseState> =
        Message "Expected 'member' or 'val' after 'static'"

    // Ths spec has incorrect grammar for member definitions, so we need to
    // reverse-engineer it from examples and the F# spec text.
    // It seems like the grammar listing in the individual subsections are closed to correct than the
    // overall grammar in https://fsharp.github.io/fslang-spec/type-definitions/ introduction.
    let private pStaticMemberDefn =
        parser {
            let! staticTok = pStatic

            match! peekNextSyntaxToken with
            | t when t.Token = Token.KWMember ->
                let! mem = consumePeeked t
                let! inlineTok = opt pInline

                match! peekNextSyntaxToken with
                | t when t.Token = Token.KWVal ->
                    let! defn = AutoPropDefn.parse

                    return
                        (fun attrs ->
                            MemberDefn.Member(
                                attrs,
                                ValueSome staticTok,
                                MemberKeyword.Member mem,
                                inlineTok,
                                ValueNone,
                                defn
                            )
                        )
                | _ ->
                    let! access = opt pAccessModifier
                    let! defn = MethodOrPropDefn.parse

                    return
                        (fun attrs ->
                            MemberDefn.Member(
                                attrs,
                                ValueSome staticTok,
                                MemberKeyword.Member mem,
                                inlineTok,
                                access,
                                defn
                            )
                        )
            | t when t.Token = Token.KWVal ->
                let! valTok = consumePeeked t
                let! mut = opt pMutable
                let! access = opt pAccessModifier
                let! ident = pIdent
                let! colon = pColon
                let! typ = Type.parse

                return
                    (fun attrs -> MemberDefn.Value(attrs, ValueSome staticTok, valTok, mut, access, ident, colon, typ))
            | _ -> return! fail errExpectedMemberOrValAfterStatic
        }

    let private pMemberDefn =
        parser {
            let! memberTok = pMember
            let! inlineTok = opt pInline

            match! peekNextSyntaxToken with
            | t when t.Token = Token.KWVal ->
                let! defn = AutoPropDefn.parse

                return
                    (fun attrs ->
                        MemberDefn.Member(attrs, ValueNone, MemberKeyword.Member memberTok, inlineTok, ValueNone, defn)
                    )
            | _ ->
                let! access = opt pAccessModifier
                let! defn = MethodOrPropDefn.parse

                return
                    (fun attrs ->
                        MemberDefn.Member(attrs, ValueNone, MemberKeyword.Member memberTok, inlineTok, access, defn)
                    )
        }

    let private pAbstractMemberDefn =
        parser {
            let! abstractTok = pAbstract
            let! memTok = opt pMember
            let! access = opt pAccessModifier
            let! sigDef = MemberSig.parse

            return
                (fun attrs ->
                    MemberDefn.Member(
                        attrs,
                        ValueNone,
                        MemberKeyword.Abstract(abstractTok, memTok),
                        ValueNone,
                        access,
                        MethodOrPropDefn.AbstractSignature sigDef
                    )
                )
        }

    let private pOverrideMemberDefn =
        parser {
            let! overrideTok = pOverride

            match! peekNextSyntaxToken with
            | t when t.Token = Token.KWVal ->
                let! defn = AutoPropDefn.parse

                return
                    (fun attrs ->
                        MemberDefn.Member(
                            attrs,
                            ValueNone,
                            MemberKeyword.Override overrideTok,
                            ValueNone,
                            ValueNone,
                            defn
                        )
                    )
            | _ ->
                let! access = opt pAccessModifier
                let! defn = MethodOrPropDefn.parse

                return
                    (fun attrs ->
                        MemberDefn.Member(
                            attrs,
                            ValueNone,
                            MemberKeyword.Override overrideTok,
                            ValueNone,
                            access,
                            defn
                        )
                    )
        }

    let private pDefaultMemberDefn =
        parser {
            let! defaultTok = pDefault

            match! peekNextSyntaxToken with
            | t when t.Token = Token.KWVal ->
                let! defn = AutoPropDefn.parse

                return
                    (fun attrs ->
                        MemberDefn.Member(
                            attrs,
                            ValueNone,
                            MemberKeyword.Default defaultTok,
                            ValueNone,
                            ValueNone,
                            defn
                        )
                    )
            | _ ->
                let! access = opt pAccessModifier
                let! defn = MethodOrPropDefn.parse

                return
                    (fun attrs ->
                        MemberDefn.Member(attrs, ValueNone, MemberKeyword.Default defaultTok, ValueNone, access, defn)
                    )
        }

    let private pValueMemberDefn =
        parser {
            let! valTok = pVal
            let! mut = opt pMutable
            let! access = opt pAccessModifier
            let! ident = pIdent
            let! colon = pColon
            let! typ = Type.parse
            return (fun attrs -> MemberDefn.Value(attrs, ValueNone, valTok, mut, access, ident, colon, typ))
        }


    let private pAdditionalConstrDefn =
        parser {
            let! access = opt pAccessModifier
            let! newTok = pNew
            let! pat = Pat.parse
            let! asDefn = opt AsDefn.parse
            let! equals = pEquals
            let! body = refAdditionalConstrExpr.Parser

            return (fun attrs -> MemberDefn.AdditionalConstructor(attrs, access, newTok, pat, asDefn, equals, body))
        }

    let private memberDefnDispatcher =
        dispatchNextSyntaxTokenFallback
            [
                Token.KWMember, pMemberDefn
                Token.KWStatic, pStaticMemberDefn
                Token.KWOverride, pOverrideMemberDefn
                Token.KWVal, pValueMemberDefn
                Token.KWAbstract, pAbstractMemberDefn
                Token.KWDefault, pDefaultMemberDefn
            ]
            pAdditionalConstrDefn

    // Implementation of the forward reference stub
    let parse: Parser<MemberDefn<SyntaxToken>, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse

            return! memberDefnDispatcher |>> fun memberDefnBuilder -> memberDefnBuilder attrs

        // // Check for 'new' (Additional Constructor)
        // let! isNew = opt (lookAhead pNew)

        // match isNew with
        // | ValueSome _ ->
        //     let! ctor = AdditionalConstrDefn.parse
        //     return MemberDefn.AdditionalConstructor ctor
        // | ValueNone ->

        //     let! staticTok = opt pStatic
        //     let! access = opt pAccessModifier

        //     let! keyword = choiceL [ pMember; pOverride; pAbstract; pDefault; pVal ] "Member Keyword"

        //     match keyword.Token with
        //     | Token.KWAbstract ->
        //         let! memTok = opt pMember
        //         let! sigDef = MemberSig.parse
        //         return MemberDefn.Abstract(attrs, keyword, memTok, access, sigDef)

        //     | Token.KWVal ->
        //         let! mut = opt pMutable
        //         let! ident = pIdent
        //         let! colon = pColon
        //         let! t = Type.parse
        //         return MemberDefn.Value(attrs, staticTok, keyword, mut, access, ident, colon, t)

        //     | Token.KWOverride ->
        //         let! defn = MethodOrPropDefn.parse
        //         return MemberDefn.Override(attrs, keyword, access, defn)

        //     | Token.KWDefault ->
        //         let! defn = MethodOrPropDefn.parse
        //         return MemberDefn.Default(attrs, keyword, access, defn)

        //     | _ -> // Token.KWMember
        //         // Check for AutoProperty: member val Ident = Expr [with get[, set]]
        //         let! nextTok = peekNextNonTriviaToken

        //         match nextTok.Token with
        //         | Token.KWVal ->
        //             let! valTok = consumePeeked nextTok
        //             let! ident = pIdent
        //             let! eq = pEquals
        //             let! expr = Expr.parse

        //             let! withClause =
        //                 opt (
        //                     parser {
        //                         let! withTok = pWith
        //                         let! acc1 = pIdent

        //                         let! acc2 =
        //                             opt (
        //                                 parser {
        //                                     let! _ = pComma
        //                                     let! id = pIdent
        //                                     return id
        //                                 }
        //                             )

        //                         return (withTok, acc1, acc2)
        //                     }
        //                 )

        //             let autoProp = MethodOrPropDefn.AutoProperty(valTok, ident, eq, expr, withClause)

        //             return MemberDefn.Concrete(attrs, staticTok, keyword, access, autoProp)
        //         | _ ->
        //             let! defn = MethodOrPropDefn.parse
        //             return MemberDefn.Concrete(attrs, staticTok, keyword, access, defn)
        }

    // Set the forward reference in your existing framework
    do refMemberDefn.Set parse

// ----------------------------------------------------------------------------
// Type Body Elements
// ----------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module TypeDefnElement =
    let parse: Parser<TypeDefnElement<SyntaxToken>, _, _, _> =
        choiceL
            [
                parser {
                    let! intf = pInterface
                    let! t = Type.parse
                    // Distinguish between InterfaceSpec (in abstract class) and InterfaceImpl (with members)
                    let! withTok = opt pWith

                    match withTok with
                    | ValueSome wTok ->
                        // Parse members using offside rule (light syntax) or until member parsing fails (verbose syntax).
                        // In verbose syntax, the `end` that terminates the outer class/struct body
                        // is NOT part of the interface block — it belongs to the outer parser.
                        let! members = withContext OffsideContext.WithAugment (many refMemberDefn.Parser)

                        // Always synthesize a virtual `end` for ObjectMembers.
                        // In verbose syntax, the outer block's real `end` stays unconsumed for the outer parser.
                        // In light syntax, there is no real `end` to consume.
                        // Use `opt` so we never fail here: at EOF or when the next token is offside,
                        // `peekNextNonTriviaToken` would fail, which (via choiceL) would discard all
                        // the work done above. Falling back to position 0 is fine for a virtual token.
                        let! nextTokOpt = opt peekNextSyntaxToken

                        let virtualEnd =
                            {
                                PositionedToken =
                                    mkVirtualPT
                                        Token.KWEnd
                                        (match nextTokOpt with
                                         | ValueSome tok -> tok.StartIndex
                                         | ValueNone -> 0)
                                Index = TokenIndex.Virtual
                            }

                        let objMembers = ObjectMembers.ObjectMembers(wTok, members, virtualEnd)

                        return TypeDefnElement.InterfaceImpl(InterfaceImpl.InterfaceImpl(intf, t, ValueSome objMembers))
                    | ValueNone -> return TypeDefnElement.InterfaceSpec(InterfaceSpec.InterfaceSpec(intf, t))
                }
                parser {
                    let! inh = pInherit
                    let! t = Type.parse
                    let! e = opt Expr.parseAtomic
                    return TypeDefnElement.Inherit(ClassInheritsDecl.ClassInheritsDecl(inh, t, e))
                }
                MemberDefn.parse |>> TypeDefnElement.Member
            ]
            "Type Definition Element"

[<RequireQualifiedAccess>]
module TypeDefnElements =
    // Parses a list of elements until 'end' or other terminator
    let parseTill terminator =
        manyTill TypeDefnElement.parse terminator

    /// Parses elements using offside rule (many stops when indentation drops).
    /// Use inside a withContext block.
    let parseMany = many TypeDefnElement.parse

// ----------------------------------------------------------------------------
// Specific Type Bodies (Class, Union, Record, etc.)
// ----------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module ClassInheritsDecl =
    let parse: Parser<ClassInheritsDecl<SyntaxToken>, _, _, _> =
        parser {
            let! inh = pInherit
            let! t = Type.parse
            // Optional constructor args — atomic only (no infix operators, semicolons, or type annotations at top level)
            let! e = opt Expr.parseAtomic
            return ClassInheritsDecl.ClassInheritsDecl(inh, t, e)
        }

[<RequireQualifiedAccess>]
module ClassFunctionOrValueDefn =
    let parse: Parser<ClassFunctionOrValueDefn<SyntaxToken>, _, _, _> =
        choiceL
            [
                parser {
                    let! attrs = opt Attributes.parse
                    let! stat = opt pStatic
                    let! d = pDo
                    // Grammar: DO typedSeqExprBlock
                    let! e = refTypedSeqExprBlock.Parser
                    return ClassFunctionOrValueDefn.Do(attrs, stat, d, e)
                }
                parser {
                    let! attrs = opt Attributes.parse
                    let! stat = opt pStatic
                    let! l = pLet
                    let! r = opt pRec
                    let! bindings, ands = Binding.parseSepByAnd1 attrs
                    return ClassFunctionOrValueDefn.LetBindings(attrs, stat, l, r, bindings, ands)
                }
            ]
            "Class Let/Do"

/// Parses the body of a class or anonymous type definition.
[<RequireQualifiedAccess>]
module ClassTypeBody =
    let private errExpectedEndOfTypeBody: ErrorType<PositionedToken, ParseState> =
        Message "Expected end of type body"

    // Returns the body AND the consumed end token (from manyTill's terminator)
    // F# allows let/do bindings to appear after member definitions, so we interleave parsing.
    let parse terminator : Parser<ObjectModelBody<SyntaxToken> * SyntaxToken, _, _, _> =
        fun reader ->
            let inh =
                match (opt ClassInheritsDecl.parse) reader with
                | Ok v -> v
                | Error _ -> ValueNone

            let preamble = ResizeArray<_>()
            let elems = ResizeArray<_>()
            let mutable keepGoing = true
            let mutable endResult = ValueNone

            while keepGoing do
                match terminator reader with
                | Ok endTok ->
                    endResult <- ValueSome endTok
                    keepGoing <- false
                | Error _ ->
                    match ClassFunctionOrValueDefn.parse reader with
                    | Ok p -> preamble.Add(p)
                    | Error _ ->
                        match TypeDefnElement.parse reader with
                        | Ok e -> elems.Add(e)
                        | Error _ -> keepGoing <- false

            match endResult with
            | ValueSome endTok ->
                Ok(
                    {
                        inherits = inh
                        classPreamble = ImmutableArray.CreateRange(preamble)
                        elements = ImmutableArray.CreateRange(elems)
                    },
                    endTok
                )
            | ValueNone ->
                // Terminator not found — fail with a descriptive error
                fail errExpectedEndOfTypeBody reader

    /// Parses class body using offside rule. Use inside a withContext block.
    /// F# allows let/do bindings to appear after member definitions (e.g., instance lets after static members),
    /// so we interleave preamble and element parsing in a single loop.
    let parseOffside: Parser<ObjectModelBody<SyntaxToken>, _, _, _> =
        fun reader ->
            let inh =
                match (opt ClassInheritsDecl.parse) reader with
                | Ok v -> v
                | Error _ -> ValueNone

            let preamble = ResizeArray<_>()
            let elems = ResizeArray<_>()
            let mutable keepGoing = true

            while keepGoing do
                match ClassFunctionOrValueDefn.parse reader with
                | Ok p -> preamble.Add(p)
                | Error _ ->
                    match TypeDefnElement.parse reader with
                    | Ok e -> elems.Add(e)
                    | Error _ -> keepGoing <- false

            Ok
                {
                    inherits = inh
                    classPreamble = ImmutableArray.CreateRange(preamble)
                    elements = ImmutableArray.CreateRange(elems)
                }

/// Parses the body of a struct definition (no inherits or class preamble).
[<RequireQualifiedAccess>]
module StructTypeBody =
    // Returns the body AND the consumed end token (from manyTill's terminator)
    let parse terminator : Parser<ObjectModelBody<SyntaxToken> * SyntaxToken, _, _, _> =
        parser {
            let! elems, endTok = TypeDefnElements.parseTill terminator

            let body =
                {
                    inherits = ValueNone
                    classPreamble = ImmutableArray.Empty
                    elements = elems
                }

            return (body, endTok)
        }

/// Parses the body of an interface definition (no inherits or class preamble).
[<RequireQualifiedAccess>]
module InterfaceTypeBody =
    // Returns the body AND the consumed end token (from manyTill's terminator)
    let parse terminator : Parser<ObjectModelBody<SyntaxToken> * SyntaxToken, _, _, _> =
        parser {
            let! elems, endTok = TypeDefnElements.parseTill terminator

            let body =
                {
                    inherits = ValueNone
                    classPreamble = ImmutableArray.Empty
                    elements = elems
                }

            return (body, endTok)
        }

// --- Union ---

[<RequireQualifiedAccess>]
module UnionTypeField =
    let parse: Parser<UnionTypeField<SyntaxToken>, _, _, _> =
        // In union case field lists, `*` is a FIELD SEPARATOR, not a tuple operator.
        // So each individual field type must be parsed without tuple handling.
        choiceL
            [
                parser {
                    let! ident = pIdent
                    let! colon = pColon
                    let! t = Type.parseField
                    return UnionTypeField.Named(ident, colon, t)
                }
                parser {
                    let! t = Type.parseField
                    return UnionTypeField.Unnamed t
                }
            ]
            "Union field"

[<RequireQualifiedAccess>]
module UnionTypeCaseData =
    let parseFields = sepBy1 UnionTypeField.parse pOpMultiply

    // Case name: plain identifier, (op), (*), or ([]). IdentOrOp.parse covers all.
    let private pCaseName = IdentOrOp.parse

    // | Name of field * field ...
    let private parseNaryOf: Parser<UnionTypeCaseData<SyntaxToken>, _, _, _> =
        parser {
            let! name = pCaseName
            let! ofTok = pOf
            let! fields, asterisks = parseFields
            return UnionTypeCaseData.Nary(name, ofTok, fields, asterisks)
        }

    // | Name : arg -> retType  (GADT-style with args)
    // Uses Type.parseNoUnion for the return type so a following `| NextCase`
    // is not swallowed as a `T | T` nullable-ref type union.
    let private parseGadtNary: Parser<UnionTypeCaseData<SyntaxToken>, _, _, _> =
        parser {
            let! name = pCaseName
            let! colon = pColon
            let! args = ArgsSpec.parse
            let! arrow = pArrowRight
            let! retType = Type.parseNoUnion
            return UnionTypeCaseData.GadtNary(name, colon, UncurriedSig.UncurriedSig(args, arrow, retType))
        }

    // | Name : retType  (GADT-style nullary with explicit return type)
    let private parseGadtNullary: Parser<UnionTypeCaseData<SyntaxToken>, _, _, _> =
        parser {
            let! name = pCaseName
            let! colon = pColon
            let! t = Type.parseNoUnion
            return UnionTypeCaseData.GadtNullary(name, colon, t)
        }

    // Non-nullary forms (used by ExceptionDefn which falls back to its own Nullary path)
    let parseNary: Parser<UnionTypeCaseData<SyntaxToken>, _, _, _> =
        choiceL [ parseNaryOf; parseGadtNary; parseGadtNullary ] "Union Case (Nary)"

    let parse: Parser<UnionTypeCaseData<SyntaxToken>, _, _, _> =
        choiceL
            [
                parseNaryOf
                parseGadtNary
                parseGadtNullary
                parser {
                    let! name = pCaseName
                    return UnionTypeCaseData.Nullary name
                }
            ]
            "Union Case"

[<RequireQualifiedAccess>]
module UnionTypeCase =
    let parse: Parser<UnionTypeCase<SyntaxToken>, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            let! caseData = UnionTypeCaseData.parse
            return UnionTypeCase.UnionTypeCase(attrs, caseData)
        }

[<RequireQualifiedAccess>]
module UnionTypeCases =
    let parse =
        parser {
            let! firstBar = opt pBar
            let! cases, sepBars = sepBy1 UnionTypeCase.parse pBar

            let bars =
                match firstBar with
                | ValueSome b -> sepBars.Insert(0, b)
                | ValueNone -> sepBars

            return struct (cases, bars)
        }

// --- Record ---

[<RequireQualifiedAccess>]
module RecordField =
    let parse: Parser<RecordField<SyntaxToken>, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            let! mut = opt pMutable
            let! acc = opt pAccessModifier
            let! id = pIdent
            let! col = pColon
            let! t = Type.parse
            return RecordField.RecordField(attrs, mut, acc, id, col, t)
        }

// --- Enum ---

[<RequireQualifiedAccess>]
module EnumTypeCase =
    let parse: Parser<EnumTypeCase<SyntaxToken>, _, _, _> =
        parser {
            let! id = nextSyntaxIdentifierLMsg "Enum Name"
            let! eq = pEquals
            let! c = withContext OffsideContext.SeqBlock refExpr.Parser
            return EnumTypeCase.EnumTypeCase(id, eq, c)
        }

[<RequireQualifiedAccess>]
module EnumTypeCases =
    let parse =
        parser {
            let! firstBar = opt pBar
            let! cases, sepBars = sepBy1 EnumTypeCase.parse pBar

            let bars =
                match firstBar with
                | ValueSome b -> sepBars.Insert(0, b)
                | ValueNone -> sepBars

            return struct (cases, bars)
        }

// --- Type Extensions ---

[<RequireQualifiedAccess>]
module TypeExtensionElements =
    let parse: Parser<TypeExtensionElements<SyntaxToken>, _, _, _> =
        parser {
            let! withTok = pWith
            let! elems = withContext OffsideContext.WithAugment TypeDefnElements.parseMany
            let! endTok = nextSyntaxTokenVirtualIfNot Token.KWEnd
            return TypeExtensionElements.TypeExtensionElements(withTok, elems, endTok)
        }

    /// Light-syntax variant: synthesizes a virtual 'with' when member tokens follow
    /// without an explicit 'with' keyword (e.g. record/union augmentations in light mode).
    let parseLight: Parser<TypeExtensionElements<SyntaxToken>, _, _, _> =
        parser {
            let! withTok = nextSyntaxTokenVirtualIfNot Token.KWWith
            let! elems = withContext OffsideContext.WithAugment (many1 TypeDefnElement.parse)
            let! endTok = nextSyntaxTokenVirtualIfNot Token.KWEnd
            return TypeExtensionElements.TypeExtensionElements(withTok, elems, endTok)
        }

[<RequireQualifiedAccess>]
module DelegateSig =
    let parse: Parser<DelegateSig<SyntaxToken>, _, _, _> =
        parser {
            let! del = pDelegate
            let! ofTok = pOf
            let! sign = UncurriedSig.parse
            return DelegateSig.DelegateSig(del, ofTok, sign)
        }


// ----------------------------------------------------------------------------
// Top Level TypeDefn
// ----------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module TypeDefn =

    let private errNotImplicit: ErrorType<PositionedToken, ParseState> =
        Message "Not implicit"

    let private errRetryAsMeasureType: ErrorType<PositionedToken, ParseState> =
        Message "Retry as measure type"

    let private errSingleNullaryUnionCaseIsAbbrev: ErrorType<PositionedToken, ParseState> =
        Message "Single nullary union case is a type abbreviation"

    // Helper to detect specific type bodies based on lookahead or specific tokens

    let private pRecordField =
        parser {
            let! attrs = opt Attributes.parse
            let! mut = opt pMutable
            let! acc = opt pAccessModifier
            let! id = pIdent
            let! col = pColon
            let! t = withContext OffsideContext.SeqBlock Type.parse
            let! _ = opt pSemi
            return RecordField.RecordField(attrs, mut, acc, id, col, t)
        }

    let private parseAbbrevOrImplicitClass
        typeName
        (primaryConstr: PrimaryConstrArgs<SyntaxToken> voption)
        (asDefn: AsDefn<SyntaxToken> voption)
        equals
        =
        parser {
            // If it starts with a class-body keyword or has a primary constructor, it's an implicit class.
            // Otherwise it's a type abbreviation.
            let! isImplicitClass =
                lookAhead (
                    choiceL
                        [
                            pMember
                            pVal
                            pNew
                            pInherit
                            pAbstract
                            pDefault
                            pOverride
                            pStatic
                            // Attributes followed by a class-body keyword (e.g., [<FieldOffset(0)>] val ...)
                            parser {
                                let! _ = Attributes.parse

                                return!
                                    choiceL
                                        [ pMember; pVal; pNew; pAbstract; pDefault; pOverride; pStatic ]
                                        "Attributed class keyword"
                            }
                            // If primary constructor or as-defn was present, it's definitely a class/struct
                            (if primaryConstr.IsSome || asDefn.IsSome then
                                 preturn (Unchecked.defaultof<_>)
                             else
                                 fail errNotImplicit)
                        ]
                        "Implicit check"
                )
                |> opt

            match isImplicitClass with
            | ValueSome _ ->
                // Implicit Class — no explicit begin/end; use offside rule to determine body extent
                let! beginTok = nextSyntaxTokenVirtualIfNot Token.KWBegin
                let! body = withContext OffsideContext.Type ClassTypeBody.parseOffside
                let! endTok = nextSyntaxTokenVirtualIfNot Token.KWEnd
                return TypeDefn.Anon(typeName, primaryConstr, asDefn, equals, beginTok, body, endTok)
            | ValueNone ->
                // Abbreviation — try normal type parsing first. If it leaves dangling
                // measure operators ('/', '^', or '*'), backtrack and try measure parsing.
                // Leading '/' (reciprocal) also falls through here: normal Type.parse
                // fails immediately and choice backtracks to the measure parser. A
                // dangling '*' only appears when tuple parsing partially succeeded then
                // backtracked mid-element (e.g. 'kg * (meter / ...)'), since a completed
                // tuple consumes the '*' separators.
                let! t =
                    choice
                        [
                            parser {
                                let! tNormal = Type.parse
                                let! peekAfter = peekNextSyntaxToken
                                let! state = getUserState

                                // Also treat fused `^-` / `^+` operators (lexer merges
                                // `^-N` into a single custom operator at Append precedence)
                                // as a signal to retry as measure type.
                                let startsWithCaret = ParseState.tokenStringStartsWith "^" peekAfter state

                                if
                                    peekAfter.Token = Token.OpDivision
                                    || peekAfter.Token = Token.OpConcatenate
                                    || peekAfter.Token = Token.OpMultiply
                                    || startsWithCaret
                                then
                                    return! fail errRetryAsMeasureType
                                else
                                    return tNormal
                            }
                            (Measure.parse |>> Type.MeasureType)
                        ]
                    |> recoverWith
                        StoppingTokens.afterTypeDefn
                        DiagnosticSeverity.Error
                        DiagnosticCode.MissingType
                        (fun toks ->
                            let m: Type<SyntaxToken> = Type<_>.Missing
                            if toks.IsEmpty then m else Type<_>.SkipsTokens(toks)
                        )

                return TypeDefn.Abbrev(typeName, equals, t)
        }

    /// Parses the body of a type definition after the leading keyword (type or and) has been consumed.
    let private parseBody (attrs: Attributes<SyntaxToken> voption) : Parser<TypeDefn<SyntaxToken>, _, _, _> =
        parser {

            let! typeName = TypeName.parseWithAttrs attrs

            // 1. Check for Primary Constructor (Class/Struct)
            let! primaryConstr = opt PrimaryConstrArgs.parse

            // 1b. Check for 'as self' identifier (e.g., type Foo(x) as this = ...)
            let! asDefn = opt AsDefn.parse

            // 2. Check for TypeExtension ('with' without '=') vs regular definition ('=')
            let! next2 = peekNextSyntaxToken

            if next2.Token = Token.KWWith && primaryConstr.IsNone then
                let! elements = TypeExtensionElements.parse
                return TypeDefn.TypeExtension(typeName, elements)
            elif next2.Token <> Token.OpEquality then
                // No '=' — abstract/opaque type (e.g., [<Measure>] type token)
                return TypeDefn.AbstractType(typeName)
            else

                // 3. Check for '='
                let! equals = pEquals

                // 4. Branch based on what follows

                let! next = peekNextSyntaxToken

                match next.Token with
                | Token.KWStruct ->
                    // Explicit Struct
                    let! str = pStruct
                    let! body, endTok = StructTypeBody.parse pEnd
                    return TypeDefn.Struct(typeName, primaryConstr, asDefn, equals, str, body, endTok)

                | Token.KWInterface when primaryConstr.IsNone ->
                    // Explicit Interface type: type IFoo = interface ... end
                    // (Not an implicit class whose body starts with `interface X with`)
                    let! intf = pInterface
                    let! body, endTok = InterfaceTypeBody.parse pEnd
                    return TypeDefn.Interface(typeName, equals, intf, body, endTok)

                | Token.KWClass ->
                    // Explicit Class
                    let! cls = pClass
                    let! body, endTok = ClassTypeBody.parse pEnd
                    return TypeDefn.Class(typeName, primaryConstr, asDefn, equals, cls, body, endTok)

                | Token.KWDelegate ->
                    let! delSig = DelegateSig.parse
                    return TypeDefn.Delegate(typeName, equals, delSig)

                | Token.KWLBrace ->
                    // Record — wrap each field's Type.parse in a SeqBlock context so that
                    // the postfix type suffix loop doesn't consume the next field's identifier.
                    // The SeqBlock indent is set to the type's start column (e.g., `float` at column 7),
                    // so the next field name at column 4 is offside and stops the type parser.
                    let! lBrace = pLBrace
                    let! fields = many1 pRecordField
                    let! rBrace = pRBrace

                    let! ext =
                        opt (choiceL [ TypeExtensionElements.parse; TypeExtensionElements.parseLight ] "Type Extension")

                    return TypeDefn.Record(typeName, equals, lBrace, fields, rBrace, ext)

                | Token.OpBar ->
                    // Try Enum first (each case has '= <value>'), then Union
                    return!
                        choiceL
                            [
                                parser {
                                    let! cases, bars = EnumTypeCases.parse
                                    return TypeDefn.Enum(typeName, equals, cases, bars)
                                }
                                parser {
                                    let! cases, bars = UnionTypeCases.parse

                                    let! ext =
                                        opt (
                                            choiceL
                                                [ TypeExtensionElements.parse; TypeExtensionElements.parseLight ]
                                                "Type Extension"
                                        )

                                    return TypeDefn.Union(typeName, equals, cases, bars, ext)
                                }
                            ]
                            "Union or Enum"

                | _ ->
                    // Try union without leading '|' (e.g., type Foo = Foo of int | Bar of string)
                    // A single nullary case (type Foo = Bar) is ambiguous — treat as type abbreviation.
                    return!
                        choiceL
                            [
                                parser {
                                    let! cases, bars = UnionTypeCases.parse

                                    match cases.Length with
                                    | 1 when
                                        (match cases[0] with
                                         | UnionTypeCase.UnionTypeCase(_, UnionTypeCaseData.Nullary _) -> true
                                         | _ -> false)
                                        ->
                                        // Single nullary case without '|' is a type abbreviation, not a DU
                                        return! fail errSingleNullaryUnionCaseIsAbbrev
                                    | _ ->
                                        let! ext =
                                            opt (
                                                choiceL
                                                    [ TypeExtensionElements.parse; TypeExtensionElements.parseLight ]
                                                    "Type Extension"
                                            )

                                        return TypeDefn.Union(typeName, equals, cases, bars, ext)
                                }
                                parseAbbrevOrImplicitClass typeName primaryConstr asDefn equals
                            ]
                            "Union or Type body"
        }

    let parse: Parser<TypeDefn<SyntaxToken>, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            let! _ = pType
            // Attributes can also appear between 'type' and the type name:
            //   type [<NoEquality>] MyRecord = ...
            let! attrsAfterType = opt Attributes.parse

            let mergedAttrs =
                match attrs, attrsAfterType with
                | ValueNone, ValueNone -> ValueNone
                | ValueSome a, ValueNone -> ValueSome a
                | ValueNone, ValueSome a -> ValueSome a
                | ValueSome a1, ValueSome a2 -> ValueSome(a1.AddRange(a2))

            return! parseBody mergedAttrs
        }

    /// Parses a type definition continuation starting with 'and' (for mutual recursion groups).
    /// In F# syntax: and [attrs] TypeName = ...  (attributes come after 'and')
    let parseAndContinuation: Parser<TypeDefn<SyntaxToken>, _, _, _> =
        parser {
            let! _ = pAnd
            let! attrs = opt Attributes.parse
            return! parseBody attrs
        }

[<RequireQualifiedAccess>]
module ExceptionDefn =
    let parse: Parser<ExceptionDefn<SyntaxToken>, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            let! exTok = pException

            return!
                choiceL
                    [
                        // Full: exception Foo of int with members
                        parser {
                            let! caseData = UnionTypeCaseData.parseNary

                            let! ext =
                                opt (
                                    choiceL
                                        [ TypeExtensionElements.parse; TypeExtensionElements.parseLight ]
                                        "Type Extension"
                                )

                            return ExceptionDefn.Full(attrs, exTok, caseData, ext)
                        }
                        // Abbreviation: exception Foo = Other.Exception
                        parser {
                            let! ident = pIdent
                            let! eq = pEquals
                            let! lid = LongIdent.parse
                            return ExceptionDefn.Abbreviation(attrs, exTok, ident, eq, lid)
                        }
                        // Nullary: exception Foo
                        parser {
                            let! ident = pIdent

                            let! ext =
                                opt (
                                    choiceL
                                        [ TypeExtensionElements.parse; TypeExtensionElements.parseLight ]
                                        "Type Extension"
                                )

                            return
                                ExceptionDefn.Full(attrs, exTok, UnionTypeCaseData.Nullary(IdentOrOp.Ident ident), ext)
                        }
                    ]
                    "Exception definition"
        }
