namespace XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.Parsers
open XParsec.OperatorParsing
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser.SyntaxToken
open XParsec.FSharp.Parser.ParseState


[<RequireQualifiedAccess>]
module PrimaryConstrArgs =
    let parse: Parser<PrimaryConstrArgs<SyntaxToken>, _, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            let! access = opt pAccessModifier
            let! lParen = pLParen
            let! pats, _ = sepBy SimplePat.parse pComma
            let! rParen = pRParen
            return PrimaryConstrArgs(attrs, access, lParen, List.ofSeq pats, rParen)
        }

[<RequireQualifiedAccess>]
module TypeName =
    /// Parses a TypeName using pre-parsed attributes (from before the `type` keyword).
    let parseWithAttrs (attrs: Attributes<SyntaxToken> voption) : Parser<TypeName<SyntaxToken>, _, _, _, _> =
        parser {
            let! access = opt pAccessModifier
            let! ident = LongIdent.parse
            let! typars = opt TyparDefns.parse
            return TypeName(attrs, access, ident, typars)
        }

    let parse: Parser<TypeName<SyntaxToken>, _, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            return! parseWithAttrs attrs
        }

[<RequireQualifiedAccess>]
module AsDefn =
    let parse: Parser<AsDefn<SyntaxToken>, _, _, _, _> =
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
        sepBy1 ArgSpec.parse pOpMultiply |>> fun struct (args, _) -> List.ofSeq args

[<RequireQualifiedAccess>]
module CurriedSig =

    let private pArgsSpec = ArgsSpec.parse .>>. pArrowRight

    let parse: Parser<CurriedSig<SyntaxToken>, _, _, _, _> =
        parser {
            // Use `many` (not `many1Till`) so properties with zero `->` are handled.
            // For each arg group, greedily try pArgsSpec (args -> ), stop when it fails.
            // The remaining is the return type.
            let! args = many pArgsSpec
            let! ret = Type.parse
            return CurriedSig(List.ofSeq args, ret)
        }

[<RequireQualifiedAccess>]
module UncurriedSig =

    let parse: Parser<UncurriedSig<SyntaxToken>, _, _, _, _> =
        parser {
            let! args = ArgsSpec.parse
            let! arrow = pArrowRight
            let! retType = Type.parse
            return UncurriedSig.UncurriedSig(args, arrow, retType)
        }

/// Signature for an abstract member (method or property)
[<RequireQualifiedAccess>]
module MemberSig =

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
            let! getTok = nextNonTriviaTokenIsL Token.Identifier "get"
            let! state = getUserState

            if tokenStringIs "get" getTok state then
                return getTok
            else
                return! fail (Message "Expected 'get'")
        }

    let private pSet =
        parser {
            let! setTok = nextNonTriviaTokenIsL Token.Identifier "set"
            let! state = getUserState

            if tokenStringIs "set" setTok state then
                return setTok
            else
                return! fail (Message "Expected 'set'")
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

    let parse: Parser<MemberSig<SyntaxToken>, _, _, _, _> =
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

    // Distinguishes:
    // member x.P = ... (Property)
    // member x.M args = ... (Method)
    // member x.P with get ... (PropWithGetSet)

    let private parseAccessorDefn: Parser<FunctionOrValueDefn<SyntaxToken>, _, _, _, _> =
        parser {
            let! name = pIdent
            let! args = many Pat.parse
            let! eq = pEquals
            let! body = Expr.parse

            return
                FunctionOrValueDefn.Function(
                    FunctionDefn.FunctionDefn(
                        ValueNone,
                        ValueNone,
                        IdentOrOp.Ident name,
                        ValueNone,
                        List.ofSeq args,
                        ValueNone,
                        eq,
                        body
                    )
                )
        }

    let pPropertyWithGetSet =
        parser {
            let! ident = pIdent
            let! w = pWith
            let! defns = FunctionOrValueDefn.parseSepByAnd1
            return fun thisIdent -> MethodOrPropDefn.PropertyWithGetSet(thisIdent, ident, w, defns)
        }

    let parse: Parser<MethodOrPropDefn<SyntaxToken>, _, _, _, _> =
        parser {
            match! peekNextNonTriviaToken with
            | tWild when tWild.Token = Token.Wildcard ->
                // `_` as self-identifier: must be followed by `.`
                let! underscore = consumePeeked tWild

                match! peekNextNonTriviaToken with
                | tDot when tDot.Token = Token.OpDot ->
                    let! dot = consumePeeked tDot

                    return!
                        choiceL
                            [
                                FunctionOrValueDefn.parse
                                |>> (function
                                | FunctionOrValueDefn.Function funcDefn ->
                                    MethodOrPropDefn.Method(ValueSome struct (underscore, dot), funcDefn)
                                | FunctionOrValueDefn.Value valueDefn ->
                                    MethodOrPropDefn.Property(ValueSome struct (underscore, dot), valueDefn))

                                pPropertyWithGetSet
                                |>> fun propWithGetSetBuilder ->
                                    propWithGetSetBuilder (ValueSome struct (underscore, dot))

                            ]
                            "MethodOrPropDefn after _."

                | _ -> return! fail (Message "Expected '.' after '_' in member definition")

            | _ ->

                match! opt pIdent with
                | ValueSome ident ->
                    match! peekNextNonTriviaToken with
                    | t when t.Token = Token.OpDot ->
                        let! dot = consumePeeked t

                        return!
                            choiceL
                                [
                                    FunctionOrValueDefn.parse
                                    |>> (function
                                    | FunctionOrValueDefn.Function funcDefn ->
                                        MethodOrPropDefn.Method(ValueSome struct (ident, dot), funcDefn)
                                    | FunctionOrValueDefn.Value valueDefn ->
                                        MethodOrPropDefn.Property(ValueSome struct (ident, dot), valueDefn))

                                    pPropertyWithGetSet
                                    |>> fun propWithGetSetBuilder ->
                                        propWithGetSetBuilder (ValueSome struct (ident, dot))

                                ]
                                "MethodOrPropDefn after dot"

                    | t when t.Token = Token.OpEquality ->
                        let! eq = consumePeeked t
                        let! expr = Expr.parse

                        return
                            MethodOrPropDefn.Property(
                                ValueNone,
                                ValueDefn(ValueNone, ValueNone, Pat.NamedSimple ident, ValueNone, ValueNone, eq, expr)
                            )

                    | _ -> return! fail (Message "Expected '.' or '=' after identifier in member definition")
                | ValueNone ->
                    return!
                        choiceL
                            [
                                FunctionOrValueDefn.parse
                                |>> (function
                                | FunctionOrValueDefn.Function funcDefn -> MethodOrPropDefn.Method(ValueNone, funcDefn)
                                | FunctionOrValueDefn.Value valueDefn -> MethodOrPropDefn.Property(ValueNone, valueDefn))
                                pPropertyWithGetSet
                                |>> fun propWithGetSetBuilder -> propWithGetSetBuilder ValueNone

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
                    return AdditionalConstrInitExpr.Explicit(lBrace, inherits, List.ofSeq inits, rBrace)
                }
                parser {
                    let! newTok = pNew
                    let! t = Type.parse
                    let! e = Expr.parse
                    return AdditionalConstrInitExpr.Delegated(newTok, t, e)
                }
            ]
            "Constructor Init"

    let parse =
        parser {
            // Simplified recursive parser for constructor body
            let! init = pInit
            return AdditionalConstrExpr.Init init
        }

    do refAdditionalConstrExpr.Set parse

[<RequireQualifiedAccess>]
module AutoPropDefn =
    let parse: Parser<MethodOrPropDefn<SyntaxToken>, _, _, _, _> =
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
    // Ths spec has incorrect grammar for member definitions, so we need to
    // reverse-engineer it from examples and the F# spec text.
    // It seems like the grammar listing in the individual subsections are closed to correct than the
    // overall grammar in https://fsharp.github.io/fslang-spec/type-definitions/ introduction.
    let private pStaticMemberDefn =
        parser {
            let! staticTok = pStatic

            match! peekNextNonTriviaToken with
            | t when t.Token = Token.KWMember ->
                let! mem = consumePeeked t

                match! peekNextNonTriviaToken with
                | t when t.Token = Token.KWVal ->
                    let! defn = AutoPropDefn.parse
                    return (fun attrs -> MemberDefn.Concrete(attrs, ValueSome staticTok, mem, ValueNone, defn))
                | _ ->
                    let! access = opt pAccessModifier
                    let! defn = MethodOrPropDefn.parse
                    return (fun attrs -> MemberDefn.Concrete(attrs, ValueSome staticTok, mem, access, defn))
            | t when t.Token = Token.KWVal ->
                let! valTok = consumePeeked t
                let! mut = opt pMutable
                let! access = opt pAccessModifier
                let! ident = pIdent
                let! colon = pColon
                let! typ = Type.parse

                return
                    (fun attrs -> MemberDefn.Value(attrs, ValueSome staticTok, valTok, mut, access, ident, colon, typ))
            | _ -> return! fail (Message "Expected 'member' or 'val' after 'static'")
        }

    let private pMemberDefn =
        parser {
            let! memberTok = pMember

            match! peekNextNonTriviaToken with
            | t when t.Token = Token.KWVal ->
                let! defn = AutoPropDefn.parse
                return (fun attrs -> MemberDefn.Concrete(attrs, ValueNone, memberTok, ValueNone, defn))
            | _ ->
                let! access = opt pAccessModifier
                let! defn = MethodOrPropDefn.parse
                return (fun attrs -> MemberDefn.Concrete(attrs, ValueNone, memberTok, access, defn))
        }

    let private pAbstractMemberDefn =
        parser {
            let! abstractTok = pAbstract
            let! memTok = opt pMember
            let! access = opt pAccessModifier
            let! sigDef = MemberSig.parse
            return (fun attrs -> MemberDefn.Abstract(attrs, abstractTok, memTok, access, sigDef))
        }

    let private pOverrideMemberDefn =
        parser {
            let! overrideTok = pOverride

            match! peekNextNonTriviaToken with
            | t when t.Token = Token.KWVal ->
                let! defn = AutoPropDefn.parse
                return (fun attrs -> MemberDefn.Override(attrs, overrideTok, ValueNone, defn))
            | _ ->
                let! access = opt pAccessModifier
                let! defn = MethodOrPropDefn.parse
                return (fun attrs -> MemberDefn.Override(attrs, overrideTok, access, defn))
        }

    let private pDefaultMemberDefn =
        parser {
            let! defaultTok = pDefault

            match! peekNextNonTriviaToken with
            | t when t.Token = Token.KWVal ->
                let! defn = AutoPropDefn.parse
                return (fun attrs -> MemberDefn.Default(attrs, defaultTok, ValueNone, defn))
            | _ ->
                let! access = opt pAccessModifier
                let! defn = MethodOrPropDefn.parse
                return (fun attrs -> MemberDefn.Default(attrs, defaultTok, access, defn))
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
        dispatchNextNonTriviaTokenFallback
            [
                Token.KWStatic, pStaticMemberDefn
                Token.KWMember, pMemberDefn
                Token.KWAbstract, pAbstractMemberDefn
                Token.KWOverride, pOverrideMemberDefn
                Token.KWDefault, pDefaultMemberDefn
                Token.KWVal, pValueMemberDefn
            ]
            pAdditionalConstrDefn

    // Implementation of the forward reference stub
    let parse: Parser<MemberDefn<SyntaxToken>, _, _, _, _> =
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
    let parse: Parser<TypeDefnElement<SyntaxToken>, _, _, _, _> =
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
                        let! nextTokOpt = opt peekNextNonTriviaToken

                        let virtualEnd =
                            {
                                PositionedToken =
                                    PositionedToken.Create(
                                        Token.ofUInt16 (uint16 Token.KWEnd ||| TokenRepresentation.IsVirtual),
                                        match nextTokOpt with
                                        | ValueSome tok -> tok.StartIndex
                                        | ValueNone -> 0
                                    )
                                Index = TokenIndex.Virtual
                            }

                        let objMembers = ObjectMembers.ObjectMembers(wTok, List.ofSeq members, virtualEnd)

                        return TypeDefnElement.InterfaceImpl(InterfaceImpl.InterfaceImpl(intf, t, ValueSome objMembers))
                    | ValueNone -> return TypeDefnElement.InterfaceSpec(InterfaceSpec.InterfaceSpec(intf, t))
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
    let parse: Parser<ClassInheritsDecl<SyntaxToken>, _, _, _, _> =
        parser {
            let! inh = pInherit
            let! t = Type.parse
            // Optional constructor args — atomic only (no infix operators, semicolons, or type annotations at top level)
            let! e = opt Expr.parseAtomic
            return ClassInheritsDecl.ClassInheritsDecl(inh, t, e)
        }

[<RequireQualifiedAccess>]
module ClassFunctionOrValueDefn =
    let parse: Parser<ClassFunctionOrValueDefn<SyntaxToken>, _, _, _, _> =
        choiceL
            [
                parser {
                    let! attrs = opt Attributes.parse
                    let! stat = opt pStatic
                    let! d = pDo
                    let! e = Expr.parse
                    return ClassFunctionOrValueDefn.Do(attrs, stat, d, e)
                }
                parser {
                    let! attrs = opt Attributes.parse
                    let! stat = opt pStatic
                    let! l = pLet
                    let! r = opt pRec
                    let! defns = FunctionOrValueDefn.parseSepByAnd1
                    return ClassFunctionOrValueDefn.LetRecDefns(attrs, stat, l, r, defns)
                }
            ]
            "Class Let/Do"

[<RequireQualifiedAccess>]
module ClassTypeBody =
    // Returns the body AND the consumed end token (from manyTill's terminator)
    let parse terminator : Parser<ClassTypeBody<SyntaxToken> * SyntaxToken, _, _, _, _> =
        parser {
            let! inh = opt ClassInheritsDecl.parse
            let! lets = many ClassFunctionOrValueDefn.parse
            let! elems, endTok = TypeDefnElements.parseTill terminator
            return (ClassTypeBody.ClassTypeBody(inh, List.ofSeq lets, ValueSome(List.ofSeq elems)), endTok)
        }

    /// Parses class body using offside rule. Use inside a withContext block.
    let parseOffside: Parser<ClassTypeBody<SyntaxToken>, _, _, _, _> =
        parser {
            let! inh = opt ClassInheritsDecl.parse
            let! lets = many ClassFunctionOrValueDefn.parse
            let! elems = TypeDefnElements.parseMany
            return ClassTypeBody.ClassTypeBody(inh, List.ofSeq lets, ValueSome(List.ofSeq elems))
        }

[<RequireQualifiedAccess>]
module StructTypeBody =
    // Returns the body AND the consumed end token (from manyTill's terminator)
    let parse terminator : Parser<StructTypeBody<SyntaxToken> * SyntaxToken, _, _, _, _> =
        parser {
            let! elems, endTok = TypeDefnElements.parseTill terminator
            return (StructTypeBody.StructTypeBody(List.ofSeq elems), endTok)
        }

[<RequireQualifiedAccess>]
module InterfaceTypeBody =
    // Returns the body AND the consumed end token (from manyTill's terminator)
    let parse terminator : Parser<InterfaceTypeBody<SyntaxToken> * SyntaxToken, _, _, _, _> =
        parser {
            let! elems, endTok = TypeDefnElements.parseTill terminator
            return (InterfaceTypeBody.InterfaceTypeBody(List.ofSeq elems), endTok)
        }

// --- Union ---

[<RequireQualifiedAccess>]
module UnionTypeField =
    let parse: Parser<UnionTypeField<SyntaxToken>, _, _, _, _> =
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
    let parseFields: Parser<UnionTypeField<SyntaxToken> list, _, _, _, _> =
        sepBy1 UnionTypeField.parse pOpMultiply
        |>> fun struct (fields, _) -> List.ofSeq fields

    let parseNary: Parser<UnionTypeCaseData<SyntaxToken>, _, _, _, _> =
        parser {
            let! ident = nextNonTriviaTokenIsL Token.Identifier "Union Case Name"
            let! ofTok = pOf

            // Check for uncurried signature (colon Type) or field list
            let! next = peekNextNonTriviaToken

            match next.Token with
            | Token.OpColon ->
                // UncurriedSig
                let! colon = pColon
                let! sign = UncurriedSig.parse
                return UnionTypeCaseData.NaryUncurried(ident, colon, sign)
            | _ ->
                // Field list
                let! fields = parseFields
                return UnionTypeCaseData.Nary(ident, ofTok, fields)
        }

    let parse: Parser<UnionTypeCaseData<SyntaxToken>, _, _, _, _> =
        // Try Nary first (requires ident followed by 'of'); backtrack on failure
        choiceL
            [
                parseNary
                parser {
                    let! ident = nextNonTriviaTokenIsL Token.Identifier "Union Case Name"
                    return UnionTypeCaseData.Nullary ident
                }
            ]
            "Union Case"

[<RequireQualifiedAccess>]
module UnionTypeCase =
    let parse: Parser<UnionTypeCase<SyntaxToken>, _, _, _, _> =
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
            let! cases, _ = sepBy1 UnionTypeCase.parse pBar
            return List.ofSeq cases
        }

// --- Record ---

[<RequireQualifiedAccess>]
module RecordField =
    let parse: Parser<RecordField<SyntaxToken>, _, _, _, _> =
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
    let parse: Parser<EnumTypeCase<SyntaxToken>, _, _, _, _> =
        parser {
            let! id = nextNonTriviaTokenIsL Token.Identifier "Enum Name"
            let! eq = pEquals
            let! c = nextNonTriviaTokenSatisfiesL (fun t -> t.Token.IsNumeric) "Enum Constant"
            return EnumTypeCase.EnumTypeCase(id, eq, c)
        }

[<RequireQualifiedAccess>]
module EnumTypeCases =
    let parse =
        parser {
            let! firstBar = opt pBar
            let! cases, _ = sepBy1 EnumTypeCase.parse pBar
            return List.ofSeq cases
        }

// --- Type Extensions ---

[<RequireQualifiedAccess>]
module TypeExtensionElements =
    let parse: Parser<TypeExtensionElements<SyntaxToken>, _, _, _, _> =
        parser {
            let! withTok = pWith
            let! elems = withContext OffsideContext.WithAugment TypeDefnElements.parseMany
            let! endTok = nextNonTriviaTokenVirtualIfNot Token.KWEnd
            return TypeExtensionElements.TypeExtensionElements(withTok, List.ofSeq elems, endTok)
        }

    /// Light-syntax variant: synthesizes a virtual 'with' when member tokens follow
    /// without an explicit 'with' keyword (e.g. record/union augmentations in light mode).
    let parseLight: Parser<TypeExtensionElements<SyntaxToken>, _, _, _, _> =
        parser {
            let! withTok = nextNonTriviaTokenVirtualIfNot Token.KWWith
            let! elems = withContext OffsideContext.WithAugment (many1 TypeDefnElement.parse)
            let! endTok = nextNonTriviaTokenVirtualIfNot Token.KWEnd
            return TypeExtensionElements.TypeExtensionElements(withTok, List.ofSeq elems, endTok)
        }

[<RequireQualifiedAccess>]
module DelegateSig =
    let parse: Parser<DelegateSig<SyntaxToken>, _, _, _, _> =
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

    // Helper to detect specific type bodies based on lookahead or specific tokens

    /// Parses the body of a type definition after the leading keyword (type or and) has been consumed.
    let private parseBody (attrs: Attributes<SyntaxToken> voption) : Parser<TypeDefn<SyntaxToken>, _, _, _, _> =
        parser {

            let! typeName = TypeName.parseWithAttrs attrs

            // 1. Check for Primary Constructor (Class/Struct)
            let! primaryConstr = opt PrimaryConstrArgs.parse

            // 2. Check for TypeExtension ('with' without '=') vs regular definition ('=')
            let! next2 = peekNextNonTriviaToken

            if next2.Token = Token.KWWith && primaryConstr.IsNone then
                let! elements = TypeExtensionElements.parse
                return TypeDefn.TypeExtension(typeName, elements)
            else

                // 3. Check for '='
                let! equals = pEquals

                // 4. Branch based on what follows

                let! next = peekNextNonTriviaToken

                match next.Token with
                | Token.KWStruct ->
                    // Explicit Struct
                    let! str = pStruct
                    let! body, endTok = StructTypeBody.parse pEnd
                    return TypeDefn.Struct(typeName, primaryConstr, ValueNone, equals, str, body, endTok)

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
                    return TypeDefn.Class(typeName, primaryConstr, ValueNone, equals, cls, body, endTok)

                | Token.KWDelegate ->
                    let! delSig = DelegateSig.parse
                    return TypeDefn.Delegate(typeName, equals, delSig)

                | Token.KWLBrace ->
                    // Record — wrap each field's Type.parse in a SeqBlock context so that
                    // the postfix type suffix loop doesn't consume the next field's identifier.
                    // The SeqBlock indent is set to the type's start column (e.g., `float` at column 7),
                    // so the next field name at column 4 is offside and stops the type parser.
                    let! lBrace = pLBrace

                    let! fields =
                        many1 (
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
                        )

                    let! rBrace = pRBrace

                    let! ext =
                        opt (choiceL [ TypeExtensionElements.parse; TypeExtensionElements.parseLight ] "Type Extension")

                    return TypeDefn.Record(typeName, equals, lBrace, List.ofSeq fields, rBrace, ext)

                | Token.OpBar ->
                    // Try Enum first (each case has '= <value>'), then Union
                    return!
                        choiceL
                            [
                                parser {
                                    let! cases = EnumTypeCases.parse
                                    return TypeDefn.Enum(typeName, equals, cases)
                                }
                                parser {
                                    let! cases = UnionTypeCases.parse

                                    let! ext =
                                        opt (
                                            choiceL
                                                [ TypeExtensionElements.parse; TypeExtensionElements.parseLight ]
                                                "Type Extension"
                                        )

                                    return TypeDefn.Union(typeName, equals, cases, ext)
                                }
                            ]
                            "Union or Enum"

                | _ ->
                    // Abbreviation or Implicit Class?
                    // If it starts with Type, it's Abbrev.
                    // If it starts with 'member', 'val', 'new', 'inherit' -> Implicit Class.

                    // We attempt to parse Type. If successful and consumed everything? Abbrev.
                    // But 'new' is not a type start. 'member' is not.

                    // Let's lookahead at tokens that start a Class Body
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
                                    // If primary constructor was present, it's definitely a class/struct
                                    (if primaryConstr.IsSome then
                                         preturn (Unchecked.defaultof<_>)
                                     else
                                         fail (Message "Not implicit"))
                                ]
                                "Implicit check"
                        )
                        |> opt

                    match isImplicitClass with
                    | ValueSome _ ->
                        // Implicit Class — no explicit begin/end; use offside rule to determine body extent
                        let! beginTok = nextNonTriviaTokenVirtualIfNot Token.KWBegin
                        let! body = withContext OffsideContext.Type ClassTypeBody.parseOffside
                        let! endTok = nextNonTriviaTokenVirtualIfNot Token.KWEnd
                        return TypeDefn.Anon(typeName, primaryConstr, ValueNone, equals, beginTok, body, endTok)
                    | ValueNone ->
                        // Abbreviation
                        let! t =
                            Type.parse
                            |> recoverWith
                                StoppingTokens.afterTypeDefn
                                DiagnosticSeverity.Error
                                DiagnosticCode.MissingType
                                (fun toks ->
                                    let m: Type<SyntaxToken> = Type<_>.Missing
                                    if toks.IsEmpty then m else Type<_>.SkipsTokens(toks, m)
                                )

                        return TypeDefn.Abbrev(typeName, equals, t)
        }

    let parse: Parser<TypeDefn<SyntaxToken>, _, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            let! _ = pType
            return! parseBody attrs
        }

    /// Parses a type definition continuation starting with 'and' (for mutual recursion groups).
    let parseAndContinuation: Parser<TypeDefn<SyntaxToken>, _, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            let! _ = pAnd
            return! parseBody attrs
        }

[<RequireQualifiedAccess>]
module ExceptionDefn =
    let parse: Parser<ExceptionDefn<SyntaxToken>, _, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            let! exTok = pException

            return!
                choiceL
                    [
                        // Full: exception Foo of int  (case data requires ident + 'of')
                        parser {
                            let! caseData = UnionTypeCaseData.parseNary
                            return ExceptionDefn.Full(attrs, exTok, caseData)
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
                            return ExceptionDefn.Full(attrs, exTok, UnionTypeCaseData.Nullary ident)
                        }
                    ]
                    "Exception definition"
        }
