namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// Syntactic decoding of the class-shaping attributes (`[<Sealed>]`,
// `[<AllowNullLiteral>]`, `[<Struct>]`, `[<IsByRefLike>]`,
// `[<RequireQualifiedAccess>]`) off a type's CST attribute sets, matching the
// long-ident's last segment against the canonical names below.
//
// SHORT NAME, not resolved identity — unlike the equality / comparison and
// parameter attributes, which resolve their long-ident as a type and compare the
// resulting `TypeKey`. Two reasons, both structural:
//   * NOTHING DECLARES THESE. The Vesper contract declares `Attribute` and the
//     eight `compiler-attributes.fsi` markers; `Sealed` / `Struct` /
//     `RequireQualifiedAccess` / `AutoOpen` / … are used throughout the library
//     sources and declared nowhere, so resolution would find nothing and every
//     struct would silently encode as a class. Declaring that vocabulary is the
//     work that unblocks resolving them.
//   * ONE CALLER HAS NO RESOLVER. The `.fsi` contract extractor decodes value-type
//     shape with no `PassContext`, no registry and no provider.
//
// The decode is pure over a `nameOf` short-name resolver (`SyntaxToken -> string`)
// so both callers share ONE decode and one set of canonical names: the
// name-resolution pass passes `PassContext.NameOf`, the extractor
// `nameOfTok lexed input`. A value-type mis-decode there encodes a struct as
// `CLASS` not `ELEMENT_TYPE_VALUETYPE`, so a consumer's member-ref misses the
// value-type method (`MissingMethodException`).

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

    /// `[<Global>]` declares a module-level value to BE a target global (JS
    /// `undefined`): no definition is emitted for it and a reference emits its bare
    /// name, from any file, with no import.
    let private globalNames = [ "Global"; "GlobalAttribute" ]

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

    /// True iff the attribute sets carry one of `names`. Shares the short-name rule
    /// with `decodeClassAttributes` so every decoder that stays syntactic agrees on
    /// the leaf.
    let private hasAttribute
        (names: string list)
        (nameOf: SyntaxToken -> string)
        (attrs: Attributes<SyntaxToken> voption)
        : bool =
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
                    | ValueSome n when List.contains n names -> found <- true
                    | _ -> ()

            found

    /// True iff the attribute sets carry `[<RequireQualifiedAccess>]`.
    let decodeRequireQualifiedAccess (nameOf: SyntaxToken -> string) (attrs: Attributes<SyntaxToken> voption) : bool =
        hasAttribute requireQualifiedAccessNames nameOf attrs

    /// True iff the attribute sets carry `[<Global>]`.
    let decodeGlobal (nameOf: SyntaxToken -> string) (attrs: Attributes<SyntaxToken> voption) : bool =
        hasAttribute globalNames nameOf attrs
