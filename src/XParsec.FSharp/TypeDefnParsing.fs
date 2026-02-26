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
    let parse: Parser<TypeName<SyntaxToken>, _, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            let! access = opt pAccessModifier
            let! ident = LongIdent.parse
            let! typars = opt TyparDefns.parse
            return TypeName(attrs, access, ident, typars)
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
            let! optional = opt (nextNonTriviaTokenIsL Token.OpDynamic "?")
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


    let parse: Parser<MemberSig<SyntaxToken>, _, _, _, _> =
        parser {
            let! ident = pIdent
            let! typars = opt TyparDefns.parse
            let! colon = pColon
            let! sigType = CurriedSig.parse

            // Check for optional 'with' get/set
            let! withClause =
                opt (
                    parser {
                        let! withTok = pWith
                        let! getSet = pGetSet
                        return struct (withTok, getSet)
                    }
                )

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

    let parse: Parser<MethodOrPropDefn<SyntaxToken>, _, _, _, _> =
        parser {
            // NOTE: The caller (MemberDefn) has consumed 'member', 'override', etc.
            // and likely the access modifier.
            // This parser focuses on the Identifier/Pattern and Body.

            // 1. Parse Identifier part (e.g. "x.Method" or "Method")
            // There might be a 'this' binding prefix: "x."
            // AST `identPrefix` captures the instance identifier.

            let! part1 = pIdent
            let! dot = opt pDot

            let! identPrefix, ident =
                match dot with
                | ValueSome _ ->
                    parser {
                        let! part2 = nextNonTriviaTokenIsL Token.Identifier "Member Name"
                        return (ValueSome part1, part2)
                    }
                | ValueNone -> preturn (ValueNone, part1)

            // 2. Check for Arguments (Method) vs Immediate `=` or `with` (Property)

            // If next is arguments, it's a method. Arguments are Patterns.
            // If next is `=`, it's a Property (or Method with unit arg omitted? F# rules apply).
            // If next is `with`, it's PropertyWithGetSet.

            let! nextTok = peekNextNonTriviaToken

            match nextTok.Token with
            | Token.KWWith ->
                // Property with get/set (e.g. "with get() = ... and set(v) = ...")
                let! withTok = nextNonTriviaTokenIsL Token.KWWith "with"
                let! defns, _ = sepBy1 parseAccessorDefn pAnd
                return MethodOrPropDefn.PropertyWithGetSet(identPrefix, ident, withTok, List.ofSeq defns)

            | Token.OpEquality ->
                // Property (Get-only usually)
                // AST: Property of ident voption * ValueDefn
                // Note: ValueDefn parser usually starts with `let`/`mutable`.
                // Here we construct a ValueDefn-like structure from `ident = expr`.
                let! eq = pEquals
                let! expr = Expr.parse

                // Construct a synthetic ValueDefn for the AST
                let valDefn =
                    ValueDefn.ValueDefn(
                        ValueNone,
                        ValueNone,
                        Pat.NamedSimple(ident), // Simplified pattern
                        ValueNone,
                        ValueNone,
                        eq,
                        expr
                    )

                return MethodOrPropDefn.Property(identPrefix, valDefn)

            | _ ->
                // Method
                // We parse parameters until `=`
                // Reuse FunctionDefn, but we've already consumed the name.
                // We need to feed the name back or use a specialized parser.

                // Let's assume FunctionDefn.parse can handle the rest if we hadn't consumed ident.
                // Since we did, we reconstruct:

                let! args = many1 Pat.parse
                let! retType = opt ReturnType.parse
                let! eq = pEquals
                let! expr = Expr.parse

                let funcDefn =
                    FunctionDefn.FunctionDefn(
                        ValueNone,
                        ValueNone,
                        IdentOrOp.Ident ident,
                        ValueNone,
                        List.ofSeq args,
                        retType,
                        eq,
                        expr
                    )

                return MethodOrPropDefn.Method(identPrefix, funcDefn)
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
                                let! e = opt Expr.parse
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
module AdditionalConstrDefn =
    let parse: Parser<AdditionalConstrDefn<SyntaxToken>, _, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            let! access = opt pAccessModifier
            let! newTok = pNew
            let! pat = Pat.parse
            let! asDefn = opt AsDefn.parse
            let! equals = pEquals
            let! body = refAdditionalConstrExpr.Parser

            return AdditionalConstrDefn(attrs, access, newTok, pat, asDefn, equals, body)
        }

[<RequireQualifiedAccess>]
module MemberDefn =
    // Implementation of the forward reference stub
    let parse: Parser<MemberDefn<SyntaxToken>, _, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse

            // Check for 'new' (Additional Constructor)
            let! isNew = opt (lookAhead pNew)

            match isNew with
            | ValueSome _ ->
                let! ctor = AdditionalConstrDefn.parse
                return MemberDefn.AdditionalConstructor ctor
            | ValueNone ->

                let! staticTok = opt pStatic
                let! access = opt pAccessModifier

                let! keyword = choiceL [ pMember; pOverride; pAbstract; pDefault; pVal ] "Member Keyword"

                match keyword.Token with
                | Token.KWAbstract ->
                    let! memTok = opt pMember
                    let! sigDef = MemberSig.parse
                    return MemberDefn.Abstract(attrs, keyword, memTok, access, sigDef)

                | Token.KWVal ->
                    let! mut = opt pMutable
                    let! ident = pIdent
                    let! colon = pColon
                    let! t = Type.parse
                    return MemberDefn.Value(attrs, staticTok, keyword, mut, access, ident, colon, t)

                | Token.KWOverride ->
                    let! defn = MethodOrPropDefn.parse
                    return MemberDefn.Override(attrs, keyword, access, defn)

                | Token.KWDefault ->
                    let! defn = MethodOrPropDefn.parse
                    return MemberDefn.Default(attrs, keyword, access, defn)

                | _ -> // Token.KWMember
                    // Check for AutoProperty: member val Ident = Expr [with get[, set]]
                    let! nextTok = peekNextNonTriviaToken

                    match nextTok.Token with
                    | Token.KWVal ->
                        let! valTok = consumePeeked nextTok
                        let! ident = pIdent
                        let! eq = pEquals
                        let! expr = Expr.parse

                        let! withClause =
                            opt (
                                parser {
                                    let! withTok = nextNonTriviaTokenIsL Token.KWWith "with"
                                    let! acc1 = pIdent

                                    let! acc2 =
                                        opt (
                                            parser {
                                                let! _ = pComma
                                                let! id = pIdent
                                                return id
                                            }
                                        )

                                    return (withTok, acc1, acc2)
                                }
                            )

                        let autoProp = MethodOrPropDefn.AutoProperty(valTok, ident, eq, expr, withClause)

                        return MemberDefn.Concrete(attrs, staticTok, keyword, access, autoProp)
                    | _ ->
                        let! defn = MethodOrPropDefn.parse
                        return MemberDefn.Concrete(attrs, staticTok, keyword, access, defn)
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
                        // `with` already consumed — parse members until end/dedent
                        // Use lookAhead on end so we don't consume it (it belongs to the class body)
                        let pTerminator = lookAhead (nextNonTriviaTokenIsL Token.KWEnd "end")
                        let! members, _ = manyTill refMemberDefn.Parser pTerminator

                        // Synthesize a virtual end token for the ObjectMembers without consuming the real end
                        let! nextTok = peekNextNonTriviaToken

                        let virtualEnd =
                            {
                                PositionedToken =
                                    PositionedToken.Create(
                                        Token.ofUInt16 (uint16 Token.KWEnd ||| TokenRepresentation.IsVirtual),
                                        nextTok.StartIndex
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

// ----------------------------------------------------------------------------
// Specific Type Bodies (Class, Union, Record, etc.)
// ----------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module ClassInheritsDecl =
    let parse: Parser<ClassInheritsDecl<SyntaxToken>, _, _, _, _> =
        parser {
            let! inh = pInherit
            let! t = Type.parse
            // Optional constructor args
            let! e = opt Expr.parse
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
                    let! r = opt (nextNonTriviaTokenIsL Token.KWRec "rec")
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
            let! ofTok = nextNonTriviaTokenIsL Token.KWOf "of"

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
            // parseTill consumes the terminator (pEnd); capture it from manyTill result
            let! elems, endTok = TypeDefnElements.parseTill pEnd
            return TypeExtensionElements.TypeExtensionElements(withTok, List.ofSeq elems, endTok)
        }

[<RequireQualifiedAccess>]
module DelegateSig =
    let parse: Parser<DelegateSig<SyntaxToken>, _, _, _, _> =
        parser {
            let! del = nextNonTriviaTokenIsL Token.KWDelegate "delegate"
            let! ofTok = nextNonTriviaTokenIsL Token.KWOf "of"
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
    let private parseBody: Parser<TypeDefn<SyntaxToken>, _, _, _, _> =
        parser {

            let! typeName = TypeName.parse

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

                | Token.KWInterface ->
                    // Interface
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
                    // Record
                    let! lBrace = pLBrace
                    let! fields, _ = sepBy1 RecordField.parse pSemi
                    let! rBrace = pRBrace
                    let! ext = opt TypeExtensionElements.parse
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
                                    let! ext = opt TypeExtensionElements.parse
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
                        // Implicit Class
                        // AST `Anon` is often used for Implicit Class definitions (begin/end inferred or explicit)
                        // Or `Class` with implicit tokens.
                        // The AST `Anon` expects `begin`/`end`. F# implicit classes don't always have them.
                        // We'll synthesize tokens or expect `begin`/`end` if the grammar strictly requires AST matching.
                        // Assuming AST requires `begin` `end`:
                        let! beginTok = nextNonTriviaTokenVirtualIfNot Token.KWBegin
                        let! body, endTok = ClassTypeBody.parse (nextNonTriviaTokenVirtualIfNot Token.KWEnd)
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
            let! _ = nextNonTriviaTokenIsL Token.KWType "type"
            return! parseBody
        }

    /// Parses a type definition continuation starting with 'and' (for mutual recursion groups).
    let parseAndContinuation: Parser<TypeDefn<SyntaxToken>, _, _, _, _> =
        parser {
            let! _ = nextNonTriviaTokenIsL Token.KWAnd "and"
            return! parseBody
        }

[<RequireQualifiedAccess>]
module ExceptionDefn =
    let parse: Parser<ExceptionDefn<SyntaxToken>, _, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            let! exTok = nextNonTriviaTokenIsL Token.KWException "exception"

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
