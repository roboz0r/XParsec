namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// Class-shaping attributes match on the long-ident's LAST SEGMENT, not on a resolved
// `TypeKey`: the `.fsi` extractor caller has no resolver.

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
    /// `ValueNone` for any head that is not a plain named type.
    let attributeShortName (nameOf: SyntaxToken -> string) (typ: Type<SyntaxToken>) : string voption =
        match typ with
        | Type.NamedType li when li.Idents.Length > 0 -> ValueSome(nameOf li.Idents.[li.Idents.Length - 1])
        | _ -> ValueNone

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
