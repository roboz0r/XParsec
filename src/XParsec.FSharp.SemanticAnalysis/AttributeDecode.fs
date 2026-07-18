namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// Syntactic decoding of the class-shaping attributes (`[<Sealed>]`,
// `[<AllowNullLiteral>]`, `[<Struct>]`, `[<IsByRefLike>]`) off a type's CST
// attribute sets. F# attributes resolve by short name (the `Attribute` suffix is
// optional, a fully qualified path collapses to the same leaf), so matching on
// the long-ident's last segment is what F# itself does for these BCL attributes.
//
// The decode is pure over a `nameOf` short-name resolver (`SyntaxToken -> string`)
// so the two callers share ONE decode and one set of canonical names: the
// name-resolution pass passes `PassContext.NameOf`, and the `.fsi` contract
// extractor passes `nameOfTok lexed input`. Neither re-hardcodes the attribute
// names — a value-type mis-decode there encodes a struct as `CLASS` not
// `ELEMENT_TYPE_VALUETYPE`, so a consumer's member-ref misses the value-type
// method (`MissingMethodException`).

module AttributeDecode =

    /// `[<Sealed>]` opts a class INTO sealed emission (`TypeAttributes.Sealed`).
    let private sealedNames = [ "Sealed"; "SealedAttribute" ]

    /// `[<AllowNullLiteral>]` lets `null` unify with the class type (B-8).
    let private allowNullLiteralNames =
        [ "AllowNullLiteral"; "AllowNullLiteralAttribute" ]

    /// `[<Struct>]` opts a class-shaped type into value-type
    /// (`System.ValueType`) emission. The bare `type X = struct … end` shape (no
    /// attribute) lands as `TypeDefn.Struct` and is normalised to the same flag
    /// by `registerClassTypeDefn`.
    let private structNames = [ "Struct"; "StructAttribute" ]

    /// `[<IsByRefLike>]` marks a value type as byref-like (a `ref struct`):
    /// codegen stamps `System.Runtime.CompilerServices.IsByRefLikeAttribute` so
    /// the CLR confines it to the stack. Implies value-type emission (a ref
    /// struct is necessarily a struct).
    let private byRefLikeNames = [ "IsByRefLike"; "IsByRefLikeAttribute" ]

    /// `[<RequireQualifiedAccess>]` forbids a record's / union's members from the
    /// enclosing unqualified index — F#'s `isILOrRequiredQualifiedAccess`.
    let private requireQualifiedAccessNames =
        [ "RequireQualifiedAccess"; "RequireQualifiedAccessAttribute" ]

    /// Decoded class-shaping attributes. `IsSealed` flips
    /// `TypeAttributes.Sealed` on the emitted `TypeDefinition`;
    /// `AllowNullLiteral` is consumed only by the front end (Unification's
    /// `Expr.Null` arm); `IsValueType` flips `System.ValueType` base +
    /// value-type layout (`[<Struct>]`); `IsByRefLike` additionally stamps the
    /// byref-like marker (and implies `IsValueType`). All default to `false` —
    /// silently ignored attributes (`[<DefaultValue>]`, etc.) leave them
    /// unchanged.
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

    /// The attribute "class" lives inside `ObjectConstruction.typ` as the
    /// long-ident the user wrote (`StructuralEquality`, or
    /// `Microsoft.FSharp.Core.StructuralEquality`). Yield the last segment so we
    /// match the F# resolution rule on short name. Generic / dotted / array /
    /// function shapes can't sit at the attribute head, so they yield
    /// `ValueNone`.
    let attributeShortName (nameOf: SyntaxToken -> string) (typ: Type<SyntaxToken>) : string voption =
        match typ with
        | Type.NamedType li when li.Idents.Length > 0 -> ValueSome(nameOf li.Idents.[li.Idents.Length - 1])
        | _ -> ValueNone

    /// Decode an attribute set list into a `ClassAttributeVerdict`. A recognised
    /// short name flips its flag; everything else is silently ignored. The flags
    /// are independent.
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
                // A ref struct is necessarily a value type, even without an
                // explicit `[<Struct>]` alongside `[<IsByRefLike>]`.
                IsValueType = isValueType || isByRefLike
                IsByRefLike = isByRefLike
            }

    /// True iff the attribute sets carry `[<RequireQualifiedAccess>]`. Shares the
    /// `attributeShortName` short-name rule with `decodeClassAttributes` so the
    /// name-resolution pass (`ctx.NameOf`) and any other decoder agree on the leaf.
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
