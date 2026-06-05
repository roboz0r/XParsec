namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// C-Attr: decode the small set of
// attributes that govern a record / union's equality AND comparison postures,
// off the type's `TypeName.attributes` CST node. The decoder is intentionally
// syntactic — F# attributes resolve by short name (with the `Attribute` suffix
// optional) and a fully qualified path collapses to the same leaf, so matching
// on the long-ident's last segment is what F# itself does for these BCL
// attributes.
//
// Equality and comparison are independent axes: `[<StructuralEquality;
// NoComparison>]` is a valid combination. Each `decode*Attributes` decoder
// returns its own verdict, and `NameResolution` writes both onto the matching
// `*Info` mutable.

module Attributes =

    /// Canonical equality-relevant short names. `Attribute` is the F# suffix
    /// rule (`StructuralEqualityAttribute` ≡ `StructuralEquality`), accepted on
    /// either form.
    let private structuralEqualityNames =
        [ "StructuralEquality"; "StructuralEqualityAttribute" ]

    let private referenceEqualityNames =
        [ "ReferenceEquality"; "ReferenceEqualityAttribute" ]

    let private noEqualityNames = [ "NoEquality"; "NoEqualityAttribute" ]

    /// Canonical comparison-relevant short names. `[<StructuralComparison>]`
    /// opts a record / union INTO structural comparison (per
    /// brainstorm-comparison §9 the default is opt-in); `[<NoComparison>]` is
    /// explicit refusal. `CustomComparison` is reserved for the augmentation
    /// path (brainstorm-comparison §10) and is treated as
    /// `NoComparison` by the decoder until augmentation-member support lands —
    /// no triple is synthesised, but a use site is allowed (the augmentation
    /// would provide one).
    let private structuralComparisonNames =
        [ "StructuralComparison"; "StructuralComparisonAttribute" ]

    let private noComparisonNames = [ "NoComparison"; "NoComparisonAttribute" ]

    /// The attribute "class" lives inside `ObjectConstruction.typ` as the
    /// long-ident the user wrote (`StructuralEquality`, or
    /// `Microsoft.FSharp.Core.StructuralEquality`). Yield the last segment so
    /// we match the F# resolution rule on short name. Generic / dotted / array
    /// / function shapes can't sit at the attribute head, so they yield
    /// `ValueNone`.
    let private attributeShortName (ctx: PassContext) (typ: Type<SyntaxToken>) : string voption =
        match typ with
        | Type.NamedType li when li.Idents.Length > 0 ->
            let last = li.Idents.[li.Idents.Length - 1]
            ValueSome(ctx.NameOf last)
        | _ -> ValueNone

    /// Decode an attribute set list into an `EqualityVerdict`. The first
    /// equality-relevant attribute wins (F# diagnoses redundant /
    /// contradictory pairs as a separate error, which Phase 1 does not yet
    /// produce); irrelevant attributes (`[<Struct>]`, `[<DefaultValue>]`, etc.)
    /// are silently ignored.
    ///
    /// `ValueNone` ⇒ no equality-relevant attribute present — the caller falls
    /// back to its default rule (brainstorm §8): an
    /// all-immutable record / any union ⇒ `Structural`; a mutable record ⇒
    /// `Reference`.
    let decodeEqualityAttributes (ctx: PassContext) (attrs: Attributes<SyntaxToken> voption) : EqualityVerdict voption =
        match attrs with
        | ValueNone -> ValueNone
        | ValueSome sets ->
            let mutable verdict = ValueNone

            for AttributeSet(attributes = entries) in sets do
                if verdict.IsNone then
                    for Attribute(construction = construction), _sep in entries do
                        if verdict.IsNone then
                            let attrTy =
                                match construction with
                                | ObjectConstruction(typ = t) -> t
                                | InterfaceConstruction(typ = t) -> t

                            match attributeShortName ctx attrTy with
                            | ValueSome n when List.contains n structuralEqualityNames ->
                                verdict <- ValueSome EqualityVerdict.Structural
                            | ValueSome n when List.contains n referenceEqualityNames ->
                                verdict <- ValueSome EqualityVerdict.Reference
                            | ValueSome n when List.contains n noEqualityNames ->
                                verdict <- ValueSome EqualityVerdict.NoEquality
                            | _ -> ()

            verdict

    /// Decode an attribute set list into a `ComparisonVerdict`. Mirrors
    /// `decodeEqualityAttributes`. `ValueNone` ⇒ no comparison-relevant
    /// attribute is present, and the caller falls back to the default
    /// (`NoComparison` per brainstorm-comparison §9 — opt-in).
    let decodeComparisonAttributes
        (ctx: PassContext)
        (attrs: Attributes<SyntaxToken> voption)
        : ComparisonVerdict voption =
        match attrs with
        | ValueNone -> ValueNone
        | ValueSome sets ->
            let mutable verdict = ValueNone

            for AttributeSet(attributes = entries) in sets do
                if verdict.IsNone then
                    for Attribute(construction = construction), _sep in entries do
                        if verdict.IsNone then
                            let attrTy =
                                match construction with
                                | ObjectConstruction(typ = t) -> t
                                | InterfaceConstruction(typ = t) -> t

                            match attributeShortName ctx attrTy with
                            | ValueSome n when List.contains n structuralComparisonNames ->
                                verdict <- ValueSome ComparisonVerdict.Structural
                            | ValueSome n when List.contains n noComparisonNames ->
                                verdict <- ValueSome ComparisonVerdict.NoComparison
                            | _ -> ()

            verdict

    /// Pull the attributes off a `TypeName` (`TypeDefn.Record` /
    /// `TypeDefn.Union` carry these on their `typeName: TypeName`).
    let attributesOfTypeName (tn: TypeName<SyntaxToken>) : Attributes<SyntaxToken> voption =
        let (TypeName(attributes = a)) = tn
        a

    /// Canonical class-relevant short names. `[<Sealed>]` opts a class INTO
    /// sealed emission (`TypeAttributes.Sealed`); `[<AllowNullLiteral>]` lets
    /// `null` unify with the class type. See vesper-set-sprint-plan §1.6 / B-8.
    let private sealedNames = [ "Sealed"; "SealedAttribute" ]

    let private allowNullLiteralNames =
        [ "AllowNullLiteral"; "AllowNullLiteralAttribute" ]

    /// Decoded class-shaping attributes. `IsSealed` flips
    /// `TypeAttributes.Sealed` on the emitted `TypeDefinition`;
    /// `AllowNullLiteral` is consumed only by the front end (Unification's
    /// `Expr.Null` arm). Both default to `false` — silently ignored attributes
    /// (`[<Struct>]`, `[<DefaultValue>]`, etc.) leave them unchanged.
    [<Struct>]
    type ClassAttributeVerdict =
        {
            IsSealed: bool
            AllowNullLiteral: bool
        }

        static member Default =
            {
                IsSealed = false
                AllowNullLiteral = false
            }

    /// Decode an attribute set list into a `ClassAttributeVerdict`. Mirrors
    /// `decodeEqualityAttributes` — a recognised short name flips its flag;
    /// everything else is silently ignored. The two flags are independent.
    let decodeClassAttributes (ctx: PassContext) (attrs: Attributes<SyntaxToken> voption) : ClassAttributeVerdict =
        match attrs with
        | ValueNone -> ClassAttributeVerdict.Default
        | ValueSome sets ->
            let mutable isSealed = false
            let mutable allowNullLiteral = false

            for AttributeSet(attributes = entries) in sets do
                for Attribute(construction = construction), _sep in entries do
                    let attrTy =
                        match construction with
                        | ObjectConstruction(typ = t) -> t
                        | InterfaceConstruction(typ = t) -> t

                    match attributeShortName ctx attrTy with
                    | ValueSome n when List.contains n sealedNames -> isSealed <- true
                    | ValueSome n when List.contains n allowNullLiteralNames -> allowNullLiteral <- true
                    | _ -> ()

            {
                IsSealed = isSealed
                AllowNullLiteral = allowNullLiteral
            }
