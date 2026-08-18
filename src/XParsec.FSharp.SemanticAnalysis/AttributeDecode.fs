namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// Attributes matched on the long-ident's LAST SEGMENT rather than on a resolved `TypeKey`.
// A written `Struct` and a written `MyOwn.Struct` are indistinguishable here.

module AttributeDecode =

    let private sealedNames = [ "Sealed"; "SealedAttribute" ]

    let private allowNullLiteralNames =
        [ "AllowNullLiteral"; "AllowNullLiteralAttribute" ]

    /// A bare `type X = struct … end` carries no attribute, so this list is not the
    /// only path to a value type.
    let private structNames = [ "Struct"; "StructAttribute" ]

    let private byRefLikeNames = [ "IsByRefLike"; "IsByRefLikeAttribute" ]

    let private requireQualifiedAccessNames =
        [ "RequireQualifiedAccess"; "RequireQualifiedAccessAttribute" ]

    [<Struct>]
    type ClassAttributeVerdict =
        {
            IsSealed: bool
            AllowNullLiteral: bool
            IsValueType: bool
            IsByRefLike: bool
        }

        static member Default =
            {
                IsSealed = false
                AllowNullLiteral = false
                IsValueType = false
                IsByRefLike = false
            }

    /// `Microsoft.FSharp.Core.StructuralEquality` → `StructuralEquality`.
    /// `ValueNone` for anything that is not a named type.
    let attributeShortName (nameOf: SyntaxToken -> string) (typ: Type<SyntaxToken>) : string voption =
        let lastOf (li: LongIdent<SyntaxToken>) =
            match li.Idents.Length with
            | 0 -> ValueNone
            | n -> ValueSome(nameOf li.Idents.[n - 1])

        match typ with
        | Type.NamedType li -> lastOf li
        | Type.GenericType(longIdent = li) -> lastOf li
        | _ -> ValueNone

    let private constructedType (construction: ObjectConstruction<SyntaxToken>) : Type<SyntaxToken> =
        match construction with
        | ObjectConstruction(typ = t)
        | InterfaceConstruction(typ = t) -> t

    /// The construction of the first attribute written as `name`, with or without the
    /// `Attribute` suffix; `ValueNone` when none is.
    let private findAttribute
        (nameOf: SyntaxToken -> string)
        (attrs: Attributes<SyntaxToken> voption)
        (name: string)
        : ObjectConstruction<SyntaxToken> voption =
        let mutable found = ValueNone

        match attrs with
        | ValueNone -> ()
        | ValueSome sets ->
            for AttributeSet(attributes = entries) in sets do
                for Attribute(construction = construction), _sep in entries do
                    if found.IsNone then
                        match attributeShortName nameOf (constructedType construction) with
                        | ValueSome n when n = name || n = name + "Attribute" -> found <- ValueSome construction
                        | _ -> ()

        found

    let private constructionExpr (oc: ObjectConstruction<SyntaxToken>) : Expr<SyntaxToken> voption =
        match oc with
        | ObjectConstruction(_, e) -> ValueSome e
        | InterfaceConstruction _ -> ValueNone

    let rec private stripParens (e: Expr<SyntaxToken>) =
        match e with
        | Expr.EnclosedBlock(_, inner, _) -> stripParens inner
        | _ -> e

    /// Text of a parsed string-literal expression. Ignores expression holes and other
    /// interpolation artefacts: a compiled-name argument is never interpolated.
    let private stringExprText
        (nameOf: SyntaxToken -> string)
        (parts: System.Collections.Immutable.ImmutableArray<StringPart<SyntaxToken>>)
        : string =
        let sb = System.Text.StringBuilder()

        for p in parts do
            match p with
            // Source-level text, escapes and all: decoding them is the lexer's job and
            // an attribute argument never needs it.
            | StringPart.Text tok
            | StringPart.EscapeSequence tok -> sb.Append(nameOf tok) |> ignore
            | _ -> ()

        sb.ToString()

    /// Text of a string-literal argument expression; `ValueNone` when it is not one.
    let private stringLiteralText (nameOf: SyntaxToken -> string) (e: Expr<SyntaxToken>) : string voption =
        match stripParens e with
        | Expr.String(_, parts, _) -> ValueSome(stringExprText nameOf parts)
        | Expr.Const(Constant.Literal tok) -> ValueSome((nameOf tok).Trim([| '"' |]))
        | _ -> ValueNone

    /// The name `[<CompiledName("Foo")>]` gives a declaration, which is what a consumer of
    /// the assembly writes.
    let tryCompiledName (nameOf: SyntaxToken -> string) (attrs: Attributes<SyntaxToken> voption) : string voption =
        match
            findAttribute nameOf attrs "CompiledName"
            |> ValueOption.bind constructionExpr
            |> ValueOption.bind (stringLiteralText nameOf)
        with
        | ValueSome "" -> ValueNone
        | other -> other

    /// A well-formed `[<Import>]`: the binding's implementation is the export `Selector` of
    /// the committed runtime asset `Path` names, relative to the declaring package.
    [<Struct>]
    type ImportRef = { Selector: string; Path: string }

    /// `[<Import(selector, path)>]` on a binding, as written.
    [<RequireQualifiedAccess>]
    type ImportDecl =
        | Import of ImportRef
        /// The attribute is present but its arguments are not two non-empty string literals.
        | Malformed
        | NoImport

    let tryImport (nameOf: SyntaxToken -> string) (attrs: Attributes<SyntaxToken> voption) : ImportDecl =
        match findAttribute nameOf attrs "Import" with
        | ValueNone -> ImportDecl.NoImport
        | ValueSome construction ->
            match constructionExpr construction |> ValueOption.map stripParens with
            | ValueSome(Expr.Tuple(exprs, _)) when exprs.Length = 2 ->
                match stringLiteralText nameOf exprs.[0], stringLiteralText nameOf exprs.[1] with
                | ValueSome selector, ValueSome path when selector <> "" && path <> "" ->
                    ImportDecl.Import { Selector = selector; Path = path }
                | _ -> ImportDecl.Malformed
            | _ -> ImportDecl.Malformed

    /// True iff the attributes carry
    /// `[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]`, which
    /// pins a module's compiled name to `<name>Module`.
    let hasModuleSuffix (nameOf: SyntaxToken -> string) (attrs: Attributes<SyntaxToken> voption) : bool =
        match
            findAttribute nameOf attrs "CompilationRepresentation"
            |> ValueOption.bind constructionExpr
        with
        | ValueNone -> false
        | ValueSome argExpr ->
            let shortName (li: LongIdent<SyntaxToken>) =
                match li.Idents.Length with
                | 0 -> ""
                | n -> nameOf li.Idents.[n - 1]

            // The flags are an enum this compiler does not fold, so the written name of the
            // one flag that matters is what is read.
            match stripParens argExpr with
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li)
            | Expr.DotLookup(_, _, LongIdentOrOp.LongIdent li) -> shortName li = "ModuleSuffix"
            | _ -> false

    /// True iff the module-level attributes carry `[<AutoOpen>]`, so its members are in
    /// scope unqualified for a consumer.
    let isAutoOpen (nameOf: SyntaxToken -> string) (attrs: Attributes<SyntaxToken> voption) : bool =
        (findAttribute nameOf attrs "AutoOpen").IsSome

    /// An unrecognised attribute is silently ignored; the flags are independent.
    let decodeClassAttributes
        (nameOf: SyntaxToken -> string)
        (attrs: Attributes<SyntaxToken> voption)
        : ClassAttributeVerdict =
        match attrs with
        | ValueNone -> ClassAttributeVerdict.Default
        | ValueSome sets ->
            let mutable isSealed = false
            let mutable allowNullLiteral = false
            let mutable isValueType = false
            let mutable isByRefLike = false

            for AttributeSet(attributes = entries) in sets do
                for Attribute(construction = construction), _sep in entries do
                    let attrTy =
                        match construction with
                        | ObjectConstruction(typ = t) -> t
                        | InterfaceConstruction(typ = t) -> t

                    match attributeShortName nameOf attrTy with
                    | ValueSome n when List.contains n sealedNames -> isSealed <- true
                    | ValueSome n when List.contains n allowNullLiteralNames -> allowNullLiteral <- true
                    | ValueSome n when List.contains n structNames -> isValueType <- true
                    | ValueSome n when List.contains n byRefLikeNames -> isByRefLike <- true
                    | _ -> ()

            {
                IsSealed = isSealed
                AllowNullLiteral = allowNullLiteral
                // `[<IsByRefLike>]` alone implies a value type.
                IsValueType = isValueType || isByRefLike
                IsByRefLike = isByRefLike
            }

    let decodeRequireQualifiedAccess (nameOf: SyntaxToken -> string) (attrs: Attributes<SyntaxToken> voption) : bool =
        match attrs with
        | ValueNone -> false
        | ValueSome sets ->
            let mutable found = false

            for AttributeSet(attributes = entries) in sets do
                for Attribute(construction = construction), _sep in entries do
                    let attrTy =
                        match construction with
                        | ObjectConstruction(typ = t) -> t
                        | InterfaceConstruction(typ = t) -> t

                    match attributeShortName nameOf attrTy with
                    | ValueSome n when List.contains n requireQualifiedAccessNames -> found <- true
                    | _ -> ()

            found
