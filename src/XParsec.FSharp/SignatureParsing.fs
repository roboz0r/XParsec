namespace XParsec.FSharp.Parser

open XParsec
open XParsec.Parsers
open XParsec.FSharp.Lexer

[<AutoOpen>]
module internal SignatureParsingHelpers =
    let refModuleSignatureElement =
        RefParser<ModuleSignatureElement<SyntaxToken>, PositionedToken, ParseState, _>()

    // Module-level error values so the Message record isn't reallocated per parser invocation.
    // See memory `pattern_hoist_error_messages`: a combined `let err = ... in fun r -> ...`
    // triggers F#'s value restriction and compiles as a thunk that re-allocates on each call.
    let errExpectedTypeSigElement: ErrorType<PositionedToken, ParseState> =
        Message "Expected type signature element"

    let errExpectedMemberOrValAfterStatic: ErrorType<PositionedToken, ParseState> =
        Message "Expected 'member' or 'val' after 'static'"

    let errExpectedNewOrValAfterAccess: ErrorType<PositionedToken, ParseState> =
        Message "Expected 'new' or 'val' after access modifier"

    let errSingleNullaryUnionCaseIsAbbrev: ErrorType<PositionedToken, ParseState> =
        Message "Single nullary union case is a type abbreviation"

    let mergeAttrs
        (a1: Attributes<SyntaxToken> voption)
        (a2: Attributes<SyntaxToken> voption)
        : Attributes<SyntaxToken> voption =
        match a1, a2 with
        | ValueNone, ValueNone -> ValueNone
        | ValueSome a, ValueNone -> ValueSome a
        | ValueNone, ValueSome a -> ValueSome a
        | ValueSome x, ValueSome y -> ValueSome(x.AddRange(y))

// ValSig: val [inline] [access] [mutable] ident [typars] : curried-sig

[<RequireQualifiedAccess>]
module ValSig =
    let parse: FSParser<ValSig<SyntaxToken>> =
        parser {
            let! attrs = opt Attributes.parse
            let! valTok = pVal
            let! inlineTok = opt pInline
            let! access = opt Access.parse
            let! mut = opt pMutable
            let! ident = IdentOrOp.parse
            let! typars = opt TyparDefns.parse
            let! colon = pColon
            let! sig' = CurriedSig.parse

            // Optional `= literalExpr` tail for `[<Literal>] val FOO: string = "..."`.
            let! literalValue =
                opt (
                    parser {
                        let! eq = pEquals
                        let! e = Expr.parseAtomic
                        return (eq, e)
                    }
                )

            return ValSig.ValSig(attrs, valTok, inlineTok, access, mut, ident, typars, colon, sig', literalValue)
        }

// TypeSignatureElement: member-shaped elements inside a type sig body

[<RequireQualifiedAccess>]
module TypeSignatureElement =

    let private pConstructor =
        parser {
            let! access = opt Access.parse
            let! newTok = pNew
            let! colon = pColon
            let! sig' = UncurriedSig.parse
            return (fun attrs -> TypeSignatureElement.Constructor(attrs, access, newTok, colon, sig'))
        }

    let private pMemberSig =
        parser {
            let! memberTok = pMember
            let! inlineTok = opt pInline
            let! access = opt Access.parse
            let! sig' = MemberSig.parse
            return (fun attrs -> TypeSignatureElement.Member(attrs, memberTok, inlineTok, access, sig'))
        }

    let private pAbstractSig =
        parser {
            let! abstractTok = pAbstract
            let! memTok = opt pMember
            let! access = opt Access.parse
            let! sig' = MemberSig.parse
            return (fun attrs -> TypeSignatureElement.Abstract(attrs, abstractTok, memTok, access, sig'))
        }

    let private pOverrideSig =
        parser {
            let! overrideTok = pOverride
            let! sig' = MemberSig.parse
            return (fun attrs -> TypeSignatureElement.Override(attrs, overrideTok, sig'))
        }

    let private pDefaultSig =
        parser {
            let! defaultTok = pDefault
            let! sig' = MemberSig.parse
            return (fun attrs -> TypeSignatureElement.Default(attrs, defaultTok, sig'))
        }

    /// `static member` and `static val` share the `static` keyword; dispatch on the
    /// following token mirrors `pStaticMemberDefn` in TypeDefnParsing.fs.
    let private pStaticMemberOrValue =
        parser {
            let! staticTok = pStatic

            match! peekNextSyntaxToken with
            | t when t.Token = Token.KWMember ->
                let! memberTok = consumePeeked t
                let! inlineTok = opt pInline
                let! access = opt Access.parse
                let! sig' = MemberSig.parse

                return
                    (fun attrs ->
                        TypeSignatureElement.StaticMember(attrs, staticTok, memberTok, inlineTok, access, sig')
                    )

            | t when t.Token = Token.KWVal ->
                let! valTok = consumePeeked t
                let! mut = opt pMutable
                let! access = opt Access.parse
                let! ident = pIdent
                let! colon = pColon
                let! typ = Type.parse

                return
                    (fun attrs ->
                        TypeSignatureElement.Value(attrs, ValueSome staticTok, valTok, mut, access, ident, colon, typ)
                    )

            | _ -> return! fail errExpectedMemberOrValAfterStatic
        }

    let private pInterfaceSpecSig =
        parser {
            let! intf = pInterface
            let! t = Type.parse

            return
                (fun (_attrs: Attributes<SyntaxToken> voption) ->
                    TypeSignatureElement.Interface(InterfaceSpec.InterfaceSpec(intf, t))
                )
        }

    let private pValue =
        parser {
            let! valTok = pVal
            let! mut = opt pMutable
            let! access = opt Access.parse
            let! ident = pIdent
            let! colon = pColon
            let! typ = Type.parse
            return (fun attrs -> TypeSignatureElement.Value(attrs, ValueNone, valTok, mut, access, ident, colon, typ))
        }

    let private pInherit =
        parser {
            let! inh = pInherit
            let! t = Type.parse
            // Signature inherits never have a value expression (no constructor call).
            return
                (fun (_attrs: Attributes<SyntaxToken> voption) ->
                    TypeSignatureElement.Inherit(ClassInheritsDecl.ClassInheritsDecl(inh, t, ValueNone))
                )
        }

    /// Handles a leading access modifier (`internal`/`private`/`public`) followed by
    /// either `new` (constructor sig) or `val` (class-field sig). E.g.
    /// `internal new: isLegacy: bool -> Foo` in `prim-types.fsi`.
    let private pAccessThenNewOrVal =
        parser {
            let! access = Access.parse

            match! peekNextSyntaxToken with
            | t when t.Token = Token.KWNew ->
                let! newTok = consumePeeked t
                let! colon = pColon
                let! sig' = UncurriedSig.parse

                return (fun attrs -> TypeSignatureElement.Constructor(attrs, ValueSome access, newTok, colon, sig'))

            | t when t.Token = Token.KWVal ->
                let! valTok = consumePeeked t
                let! mut = opt pMutable
                let! ident = pIdent
                let! colon = pColon
                let! typ = Type.parse

                return
                    (fun attrs ->
                        TypeSignatureElement.Value(attrs, ValueNone, valTok, mut, ValueSome access, ident, colon, typ)
                    )

            | _ -> return! fail errExpectedNewOrValAfterAccess
        }

    let private elementDispatcher =
        dispatchNextSyntaxTokenFallback
            [
                Token.KWNew, pConstructor
                Token.KWMember, pMemberSig
                Token.KWAbstract, pAbstractSig
                Token.KWOverride, pOverrideSig
                Token.KWDefault, pDefaultSig
                Token.KWStatic, pStaticMemberOrValue
                Token.KWInterface, pInterfaceSpecSig
                Token.KWVal, pValue
                Token.KWInherit, pInherit
                Token.KWInternal, pAccessThenNewOrVal
                Token.KWPrivate, pAccessThenNewOrVal
                Token.KWPublic, pAccessThenNewOrVal
            ]
            (fail errExpectedTypeSigElement)

    let parse: FSParser<TypeSignatureElement<SyntaxToken>> =
        parser {
            let! attrs = opt Attributes.parse
            return! elementDispatcher |>> fun build -> build attrs
        }

[<RequireQualifiedAccess>]
module TypeElementsSignature =
    /// Parse signature elements until the terminator (e.g. `end`) is hit.
    /// Returns `(elements, terminatorResult)`.
    let parseTill (terminator: FSParser<'a>) =
        manyTill TypeSignatureElement.parse terminator

    /// Parse signature elements using the surrounding offside context. The caller is
    /// responsible for the `withContext` wrapper that establishes the offside line.
    let parseMany = many TypeSignatureElement.parse

// Type extensions in signatures: with type-elements-signature end

[<RequireQualifiedAccess>]
module TypeExtensionElementsSignature =
    let parse: FSParser<TypeExtensionElementsSignature<SyntaxToken>> =
        parser {
            let! withTok = pWith
            let! elems = withContext OffsideContext.WithAugment TypeElementsSignature.parseMany
            let! endTok = nextSyntaxTokenVirtualIfNot Token.KWEnd
            return TypeExtensionElementsSignature.TypeExtensionElementsSignature(withTok, elems, endTok)
        }

    /// Light-syntax variant: synthesises a virtual `with` when member tokens follow
    /// without an explicit `with` keyword (e.g. record/union augmentations in light mode).
    let parseLight: FSParser<TypeExtensionElementsSignature<SyntaxToken>> =
        parser {
            let! withTok = nextSyntaxTokenVirtualIfNot Token.KWWith
            let! elems = withContext OffsideContext.WithAugment (many1 TypeSignatureElement.parse)
            let! endTok = nextSyntaxTokenVirtualIfNot Token.KWEnd
            return TypeExtensionElementsSignature.TypeExtensionElementsSignature(withTok, elems, endTok)
        }

    /// The optional trailing `with …` augmentation (explicit `parse` or light-syntax
    /// `parseLight`) shared by the extern / record / union signature surfaces.
    let parseOpt = opt (choiceL [ parse; parseLight ] "Type Extension")

// TypeSignature: top-level dispatch mirroring TypeDefn.parseBody minus class
// preamble / primary constructor / measure-retry. The leading `type` (or `and`)
// keyword is consumed in `parse` / `parseAndContinuation`; `parseBody` runs after.

[<RequireQualifiedAccess>]
module TypeSignature =

    // Record fields use SeqBlock offside so the type's postfix loop doesn't eat the next field's identifier.
    // Mirrors TypeDefnParsing.fs pRecordField.
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

    /// Tokens that, when seen right after `type T =`, indicate an implicit anonymous
    /// type signature body (no explicit `class`/`struct`/`begin`/`interface` keyword).
    /// `interface` is excluded because at this position it means `type IFoo = interface ... end`
    /// (an explicit interface body), not a member.
    let private isImplicitClassStart (tok: SyntaxToken) =
        match tok.Token with
        | Token.KWMember
        | Token.KWVal
        | Token.KWNew
        | Token.KWInherit
        | Token.KWAbstract
        | Token.KWDefault
        | Token.KWOverride
        | Token.KWStatic
        | Token.KWLAttrBracket -> true // [<Attr>] member ...
        | _ -> false

    let parseBody (attrs: Attributes<SyntaxToken> voption) : FSParser<TypeSignature<SyntaxToken>> =
        parser {
            let! typeName = TypeName.parseWithAttrs attrs
            let! next2 = peekNextSyntaxToken

            if next2.Token = Token.KWWith then
                let! ext = TypeExtensionElementsSignature.parse
                return TypeSignature.TypeExtension(typeName, ext)
            elif next2.Token <> Token.OpEquality then
                // No `=` — opaque sig (e.g. `[<Measure>] type kg`, `type T`)
                return TypeSignature.AbstractType(typeName)
            else
                let! equals = pEquals
                let! next = peekNextSyntaxToken

                match next.Token with
                | Token.KWStruct ->
                    let! str = pStruct
                    let! elems, endTok = TypeElementsSignature.parseTill pEnd
                    return TypeSignature.Struct(typeName, equals, str, elems, endTok)

                | Token.KWInterface ->
                    // Two forms share the `interface` keyword after `=`:
                    //   1. Explicit body: `type IFoo = interface <members> end`
                    //   2. Implicit class: `type Unit = interface IComparable` (interface as first member).
                    // Disambiguate by peeking the token AFTER `interface`: a member-start or `end`
                    // means the explicit body form; anything else (a type-start ident, `'a`, etc.)
                    // means the implicit-class form.
                    let! isExplicitInterface =
                        lookAhead (
                            parser {
                                let! _ = pInterface
                                let! peek2 = peekNextSyntaxToken

                                return
                                    match peek2.Token with
                                    | Token.KWAbstract
                                    | Token.KWMember
                                    | Token.KWVal
                                    | Token.KWInherit
                                    | Token.KWNew
                                    | Token.KWStatic
                                    | Token.KWOverride
                                    | Token.KWDefault
                                    | Token.KWLAttrBracket
                                    | Token.KWEnd -> true
                                    | _ -> false
                            }
                        )

                    if isExplicitInterface then
                        let! intf = pInterface
                        let! elems, endTok = TypeElementsSignature.parseTill pEnd
                        return TypeSignature.Interface(typeName, equals, intf, elems, endTok)
                    else
                        let! beginTok = nextSyntaxTokenVirtualIfNot Token.KWBegin
                        let! elems = withContext OffsideContext.Type TypeElementsSignature.parseMany
                        let! endTok = nextSyntaxTokenVirtualIfNot Token.KWEnd
                        return TypeSignature.Anon(typeName, equals, beginTok, elems, endTok)

                | Token.KWClass ->
                    let! cls = pClass
                    let! elems, endTok = TypeElementsSignature.parseTill pEnd
                    return TypeSignature.Class(typeName, equals, cls, elems, endTok)

                | Token.KWBegin ->
                    let! beg = pBegin
                    let! elems, endTok = TypeElementsSignature.parseTill pEnd
                    return TypeSignature.Anon(typeName, equals, beg, elems, endTok)

                | Token.KWDelegate ->
                    let! d = DelegateSig.parse
                    return TypeSignature.Delegate(typeName, equals, d)

                | Token.KWExtern ->
                    // `type int = extern` — intrinsic primitive (no Vesper representation).
                    // Optional `with member … / interface …` publishes the capability
                    // surface a later extractor consumes; parsed unconditionally here —
                    // the opt-in gate lives at the extractor, not the parser.
                    let! ext = pExtern

                    // Optional `class` / `interface` tag: `type Attribute = extern class`
                    // marks a HERITABLE external reference base (repr extracted from the
                    // paired `.fs`'s `(# class "…" #)`); `type disposable = extern interface
                    // with …` marks a capability INTERFACE (an all-abstract surface published
                    // as an `IntrinsicInterface`). The tag introduces the type BODY, so
                    // `interface` is admitted only when `with` follows it: a bare
                    // `interface <Type> …` capability MEMBER (an interface implementation)
                    // below always carries a type name where the tag has `with`, so it is
                    // never mis-eaten. `choiceL` rewinds on the failed branch, leaving such
                    // a member for `parseOpt`.
                    let! kindTag =
                        opt (
                            choiceL
                                [
                                    pClass |>> ExternKind.Class
                                    parser {
                                        let! interfaceTok = pInterface
                                        let! _ = lookAhead pWith
                                        return ExternKind.Interface interfaceTok
                                    }
                                ]
                                "external class/interface tag"
                        )

                    let! members = TypeExtensionElementsSignature.parseOpt

                    return TypeSignature.Extern(typeName, equals, ext, kindTag, members)

                | _ when isImplicitClassStart next ->
                    // Implicit anonymous body: no explicit class/struct/begin keyword.
                    // Synthesize virtual begin/end and use offside rule (OffsideContext.Type)
                    // to delimit the body. Mirrors parseAbbrevOrImplicitClass in TypeDefnParsing.fs.
                    let! beginTok = nextSyntaxTokenVirtualIfNot Token.KWBegin
                    let! elems = withContext OffsideContext.Type TypeElementsSignature.parseMany
                    let! endTok = nextSyntaxTokenVirtualIfNot Token.KWEnd
                    return TypeSignature.Anon(typeName, equals, beginTok, elems, endTok)

                | Token.KWLBrace ->
                    let! lBrace = pLBrace
                    let! fields = many1 pRecordField
                    let! rBrace = pRBrace

                    let! ext = TypeExtensionElementsSignature.parseOpt

                    return TypeSignature.Record(typeName, equals, lBrace, fields, rBrace, ext)

                | Token.OpBar ->
                    return!
                        choiceL
                            [
                                parser {
                                    let! cases, _bars = EnumTypeCases.parse
                                    return TypeSignature.Enum(typeName, equals, cases)
                                }
                                parser {
                                    let! cases, _bars = UnionTypeCases.parse
                                    let! ext = TypeExtensionElementsSignature.parseOpt
                                    return TypeSignature.Union(typeName, equals, cases, ext)
                                }
                            ]
                            "Union or Enum signature"

                | _ ->
                    return!
                        choiceL
                            [
                                parser {
                                    let! cases, _bars = UnionTypeCases.parse

                                    match cases.Length with
                                    | 1 when
                                        (match cases[0] with
                                         | UnionTypeCase.UnionTypeCase(_, UnionTypeCaseData.Nullary _) -> true
                                         | _ -> false)
                                        ->
                                        return! fail errSingleNullaryUnionCaseIsAbbrev
                                    | _ ->
                                        let! ext = TypeExtensionElementsSignature.parseOpt
                                        return TypeSignature.Union(typeName, equals, cases, ext)
                                }
                                (Type.parse |>> fun t -> TypeSignature.Abbrev(typeName, equals, t))
                            ]
                            "Union or Type abbreviation"
        }

    let parse: FSParser<TypeSignature<SyntaxToken>> =
        parser {
            let! attrs = opt Attributes.parse
            let! _ = pType
            let! attrsAfter = opt Attributes.parse
            let merged = mergeAttrs attrs attrsAfter
            return! parseBody merged
        }

    /// Parses an `and`-continuation: `and [attrs] TypeName = ...`. Returns the
    /// `and` token alongside the parsed signature so `TypeSignatures.rest` can pair them.
    let parseAndContinuation: FSParser<SyntaxToken * TypeSignature<SyntaxToken>> =
        parser {
            let! andTok = pAnd
            let! attrs = opt Attributes.parse
            let! ts = parseBody attrs
            return (andTok, ts)
        }

[<RequireQualifiedAccess>]
module ModuleSignatureBody =
    let parse (modTok: SyntaxToken) : FSParser<ModuleSignatureBody<SyntaxToken>> =
        parser {
            let! state = getUserState

            let indent =
                match modTok.Index with
                | TokenIndex.Regular iT -> ParseState.getIndent state iT
                | TokenIndex.Virtual -> 0

            let! beginTok = nextSyntaxTokenVirtualIfNot Token.KWBegin

            let! elems =
                withContextAt
                    OffsideContext.Module
                    (indent + 1)
                    modTok.PositionedToken
                    (many refModuleSignatureElement.Parser)

            let! endTok = nextSyntaxTokenVirtualIfNot Token.KWEnd
            return ModuleSignatureBody(beginTok, elems, endTok)
        }

[<RequireQualifiedAccess>]
module ModuleSignature =
    let parse: FSParser<ModuleSignature<SyntaxToken>> =
        parser {
            let! attrs = opt Attributes.parse
            let! modTok = pModule
            let! access = opt Access.parse
            let! isRec = opt pRec
            let! ident = pIdent
            let! eq = pEquals
            let! body = ModuleSignatureBody.parse modTok
            return ModuleSignature.ModuleSignature(attrs, modTok, access, isRec, ident, eq, body)
        }

[<RequireQualifiedAccess>]
module ModuleSignatureElement =
    // Needed for Attributes.parse
    do ObjectConstruction.init ()

    /// Type sig group: `type [<Attrs>] Foo = ...` optionally followed by `and Bar = ...`.
    let private pTypeGroup =
        parser {
            let! attrs = opt Attributes.parse
            let! tyTok = pType
            let! attrsAfter = opt Attributes.parse
            let merged = mergeAttrs attrs attrsAfter
            let! first = TypeSignature.parseBody merged
            let! rest = many TypeSignature.parseAndContinuation
            return ModuleSignatureElement.Type(tyTok, TypeSignatures.TypeSignatures(first, rest))
        }

    /// `[<Attrs>] exception Foo of Type` — fields mirror impl-side `ExceptionDefn.Full`
    /// case-data but only the bare `UnionTypeCaseData` slot, per `Signatures.fs`.
    let private pExceptionSig =
        parser {
            let! attrs = opt Attributes.parse
            let! exTok = pException
            let! caseData = UnionTypeCaseData.parse
            return ModuleSignatureElement.Exception(attrs, exTok, caseData)
        }

    let parse: FSParser<ModuleSignatureElement<SyntaxToken>> =
        dispatchNextSyntaxTokenFallback
            [
                Token.KWOpen, ImportDecl.parse |>> ModuleSignatureElement.Import
                Token.KWHash, CompilerDirectiveDecl.parse |>> ModuleSignatureElement.CompilerDirective
            ]
            (choiceL
                [
                    // Type sig groups (handles attrs)
                    pTypeGroup

                    // Exception sig
                    pExceptionSig

                    // Module abbrev MUST precede ModuleSignature so `module X = Y.Z` is
                    // not consumed as a nested-module sig with body starting after `=`.
                    ModuleAbbrev.parse |>> ModuleSignatureElement.ModuleAbbrev

                    // Nested module signature
                    ModuleSignature.parse |>> ModuleSignatureElement.Module

                    // Val signature
                    ValSig.parse |>> ModuleSignatureElement.Val
                ]
                "ModuleSignatureElement")

    do refModuleSignatureElement.Set parse

[<RequireQualifiedAccess>]
module NamespaceDeclGroupSignature =
    let parse: FSParser<NamespaceDeclGroupSignature<SyntaxToken>> =
        parser {
            let! nsTok = pNamespace
            let! globalTok = opt pGlobal

            match globalTok with
            | ValueSome gTok ->
                let! elems = many refModuleSignatureElement.Parser
                return NamespaceDeclGroupSignature.Global(nsTok, gTok, elems)

            | ValueNone ->
                let! isRec = opt pRec
                let! ident = recoverLongIdent "Expected namespace identifier" LongIdent.parse
                let! elems = many refModuleSignatureElement.Parser
                return NamespaceDeclGroupSignature.Named(nsTok, isRec, ident, elems)
        }

[<RequireQualifiedAccess>]
module SignatureFile =
    /// Parses `[attributes] module [access] [rec] LongIdent <module-sig-elems>`.
    /// Distinguished from module abbreviation (`module X = Y.Z`) by absence of `=` after the LongIdent.
    let private pNamedModule =
        let notFollowedByEquals = notFollowedBySyntaxToken Token.OpEquality

        parser {
            let! attrs = opt Attributes.parse
            let! modTok = pModule
            let! access = opt Access.parse
            let! isRec = opt pRec
            let! longIdent = recoverLongIdent "Expected module identifier" LongIdent.parse
            do! notFollowedByEquals
            let! elems = many refModuleSignatureElement.Parser
            return NamedModuleSignature.NamedModuleSignature(attrs, modTok, access, isRec, longIdent, elems)
        }

    let parse: FSParser<SignatureFile<SyntaxToken>> =
        dispatchNextSyntaxTokenFallback
            [
                Token.KWNamespace, many1 NamespaceDeclGroupSignature.parse |>> SignatureFile.Namespaces
                Token.KWModule,
                choiceL
                    [
                        pNamedModule |>> SignatureFile.NamedModule
                        many refModuleSignatureElement.Parser |>> SignatureFile.AnonymousModule
                    ]
                    "SignatureFile"
            ]
            (choiceL
                [
                    pNamedModule |>> SignatureFile.NamedModule
                    many refModuleSignatureElement.Parser |>> SignatureFile.AnonymousModule
                ]
                "SignatureFile")
