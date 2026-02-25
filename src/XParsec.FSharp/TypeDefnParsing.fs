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
            let! ident = pIdent
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
            let! typ = Type.parse
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
            let! (args, ret) = many1Till pArgsSpec Type.parse
            let args = List.ofSeq args
            return CurriedSig(args, ret)
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

            let! nextTok = pid

            match nextTok with
            | t when t.Token = Token.KWWith ->
                // Property with get/set
                let! withTok = nextNonTriviaTokenIsL Token.KWWith "with"
                // Parse get/set definitions (FunctionOrValueDefn list)
                // This usually requires a loop parsing `member val` or just `get() = ...`
                // Stubbing list for brevity:
                return MethodOrPropDefn.PropertyWithGetSet(identPrefix, ident, withTok, [])

            | t when t.Token = Token.OpEquality ->
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
                    // Helper for inherits: inherit Type(expr)
                    let! inherits =
                        parser {
                            let! inh = pInherit
                            let! t = Type.parse
                            let! e = opt Expr.parse
                            return ClassInheritsDecl.ClassInheritsDecl(inh, t, e)
                        }

                    let! inits = many FieldInitializer.parse // Simplified loop
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
                    | ValueSome _ ->
                        // InterfaceImpl requires members, usually `member ...`
                        // Reusing ObjectMembers parser logic roughly
                        // For precise AST `InterfaceImpl` expects `InterfaceImpl` type which has `opt ObjectMembers`
                        // Here we map roughly:
                        let! members = opt ObjectMembers.parse
                        return TypeDefnElement.InterfaceImpl(InterfaceImpl.InterfaceImpl(intf, t, members))
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
                    // Parsing multiple let bindings is complex in top level,
                    // assuming one for now or loop needed.
                    // FunctionOrValueDefn.parse handles one.
                    let! defn = FunctionOrValueDefn.parse
                    return ClassFunctionOrValueDefn.LetRecDefns(attrs, stat, l, r, [ defn ])
                }
            ]
            "Class Let/Do"

[<RequireQualifiedAccess>]
module ClassTypeBody =
    let parse terminator : Parser<ClassTypeBody<SyntaxToken>, _, _, _, _> =
        parser {
            // Implicit class body:
            // inherit?
            // let/do bindings*
            // member/interface elements*

            let! inh = opt ClassInheritsDecl.parse

            // Allow interleaving of let/do and members in implicit constructors?
            // Strictly F# puts let/do before members usually, but implicit classes allow mix slightly.
            // Simplified: Parse Let/Dos, then Elements.

            let! lets = many ClassFunctionOrValueDefn.parse

            let! elems =
                opt (TypeDefnElements.parseTill terminator)
                |>> function
                    | ValueSome(es, _) -> ValueSome(List.ofSeq es)
                    | ValueNone -> ValueNone

            return ClassTypeBody.ClassTypeBody(inh, List.ofSeq lets, elems)
        }

[<RequireQualifiedAccess>]
module StructTypeBody =
    let parse terminator =
        parser {
            let! elems, _ = TypeDefnElements.parseTill terminator
            return StructTypeBody.StructTypeBody(List.ofSeq elems)
        }

[<RequireQualifiedAccess>]
module InterfaceTypeBody =
    let parse terminator =
        parser {
            let! elems, _ = TypeDefnElements.parseTill terminator
            return InterfaceTypeBody.InterfaceTypeBody(List.ofSeq elems)
        }

// --- Union ---

[<RequireQualifiedAccess>]
module UnionTypeField =
    let parse: Parser<UnionTypeField<SyntaxToken>, _, _, _, _> =
        parser {
            // Try Named first: ident : Type
            let! namedField =
                opt (
                    parser {
                        let! ident = pIdent
                        let! colon = pColon
                        let! t = Type.parse
                        return UnionTypeField.Named(ident, colon, t)
                    }
                )

            match namedField with
            | ValueSome nf -> return nf
            | ValueNone ->
                // Unnamed field: just Type
                let! t = Type.parse
                return UnionTypeField.Unnamed t
        }

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
            let! next = pid

            match next with
            | t when t.Token = Token.OpColon ->
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
        parser {
            // Try Nary first
            let! nary = opt (lookAhead (nextNonTriviaTokenIsL Token.KWOf "of"))

            match nary with
            | ValueSome _ -> return! parseNary
            | ValueNone ->
                // Nullary
                let! ident = nextNonTriviaTokenIsL Token.Identifier "Union Case Name"
                return UnionTypeCaseData.Nullary ident
        }

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
            let! elems, _ = TypeDefnElements.parseTill pEnd
            let! endTok = pEnd
            return TypeExtensionElements.TypeExtensionElements(withTok, List.ofSeq elems, endTok)
        }

[<RequireQualifiedAccess>]
module DelegateSig =
    let parse: Parser<DelegateSig<SyntaxToken>, _, _, _, _> =
        parser {
            let! del = nextNonTriviaTokenIsL Token.KWDelegate "delegate"
            let! ofTok = nextNonTriviaTokenIsL Token.KWOf "of" // not a keyword we add: KWOf is rare
            let! t = Type.parse // Simplified mapping to UncurriedSig
            // Construct fake UncurriedSig for AST compliance
            let sigData = UncurriedSig.UncurriedSig([], Unchecked.defaultof<_>, t)
            return DelegateSig.DelegateSig(del, ofTok, sigData)
        }


// ----------------------------------------------------------------------------
// Top Level TypeDefn
// ----------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module TypeDefn =

    // Helper to detect specific type bodies based on lookahead or specific tokens

    let parse: Parser<TypeDefn<SyntaxToken>, _, _, _, _> =
        parser {
            let! typeKw = nextNonTriviaTokenIsL Token.KWType "type" // Consumed but typically part of TypeName?
            // Note: AST TypeName doesn't include 'type' keyword, but TypeDefn implies it's wrapped or parsed before.
            // Assuming we are at the name:

            let! typeName = TypeName.parse

            // 1. Check for Primary Constructor (Class/Struct)
            let! primaryConstr = opt PrimaryConstrArgs.parse

            // 2. Check for '='
            let! equals = pEquals

            // 3. Branch based on what follows

            let! next = pid

            match next with
            | t when t.Token = Token.KWStruct ->
                // Explicit Struct
                let! str = pStruct
                let! body = StructTypeBody.parse pEnd
                let! endTok = pEnd
                return TypeDefn.Struct(typeName, primaryConstr, ValueNone, equals, str, body, endTok)

            | t when t.Token = Token.KWInterface ->
                // Interface
                let! intf = pInterface
                let! body = InterfaceTypeBody.parse pEnd
                let! endTok = pEnd
                return TypeDefn.Interface(typeName, equals, intf, body, endTok)

            | t when t.Token = Token.KWClass ->
                // Explicit Class
                let! cls = pClass
                let! body = ClassTypeBody.parse pEnd
                let! endTok = pEnd
                return TypeDefn.Class(typeName, primaryConstr, ValueNone, equals, cls, body, endTok)

            | t when t.Token = Token.KWDelegate ->
                let! delSig = DelegateSig.parse
                return TypeDefn.Delegate(typeName, equals, delSig)

            | t when t.Token = Token.KWLBrace ->
                // Record
                let! lBrace = pLBrace
                let! fields, _ = sepBy1 RecordField.parse pSemi
                let! rBrace = pRBrace
                let! ext = opt TypeExtensionElements.parse
                return TypeDefn.Record(typeName, equals, lBrace, List.ofSeq fields, rBrace, ext)

            | t when t.Token = Token.OpBar ->
                // Union or Enum
                // Look ahead deeper to distinguish?
                // Heuristic: Parse first case. If it has '=', it's Enum.
                // Reusing UnionTypeCases but catching Enum pattern is complex without backtracking.
                // Assuming Union for now as it's more common with '|' start.
                let! cases = UnionTypeCases.parse
                let! ext = opt TypeExtensionElements.parse
                return TypeDefn.Union(typeName, equals, cases, ext)

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
                    let endTokParser = nextNonTriviaTokenVirtualIfNot Token.KWEnd
                    let! body = ClassTypeBody.parse endTokParser
                    let! endTok = endTokParser // consume the virtual/real end
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

[<RequireQualifiedAccess>]
module ExceptionDefn =
    let parse: Parser<ExceptionDefn<SyntaxToken>, _, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            let! exTok = nextNonTriviaTokenIsL Token.KWException "exception"

            // Try Full first
            let! isFull = opt (lookAhead (nextNonTriviaTokenIsL Token.KWOf "of"))

            match isFull with
            | ValueSome _ ->
                let! caseData = UnionTypeCaseData.parse
                return ExceptionDefn.Full(attrs, exTok, caseData)
            | ValueNone ->
                let! ident = nextNonTriviaTokenIsL Token.Identifier "Exception Name"
                let! eq = nextNonTriviaTokenIsL Token.OpEquality "="
                let! lid = LongIdent.parse
                return ExceptionDefn.Abbreviation(attrs, exTok, ident, eq, lid)
        }
