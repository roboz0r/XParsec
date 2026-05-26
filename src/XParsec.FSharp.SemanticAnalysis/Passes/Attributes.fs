namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// C-Attr (docs/records-handoff.md Phase 1): decode the small set of attributes
// that govern a record / union's equality posture, off the type's
// `TypeName.attributes` CST node. The decoder is intentionally syntactic — F#
// attributes resolve by short name (with the `Attribute` suffix optional) and a
// fully qualified path collapses to the same leaf, so matching on the
// long-ident's last segment is what F# itself does for these BCL attributes.
//
// Scope (PR A): equality only. The parallel `[<NoComparison>]` /
// `[<StructuralComparison>]` axis is tracked separately by Phase 3; this
// module emits an `EqualityVerdict option` and ignores comparison-only
// attributes. (Combinations like `[<StructuralEquality; NoComparison>]` are
// still valid — the equality half lands here, the comparison half lands later.)

module Attributes =

    /// Canonical equality-relevant short names. `Attribute` is the F# suffix
    /// rule (`StructuralEqualityAttribute` ≡ `StructuralEquality`), accepted on
    /// either form.
    let private structuralEqualityNames =
        [ "StructuralEquality"; "StructuralEqualityAttribute" ]

    let private referenceEqualityNames =
        [ "ReferenceEquality"; "ReferenceEqualityAttribute" ]

    let private noEqualityNames = [ "NoEquality"; "NoEqualityAttribute" ]

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
    /// back to its default rule (records-plan §B4 / brainstorm §8): an
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

    /// Pull the attributes off a `TypeName` (`TypeDefn.Record` /
    /// `TypeDefn.Union` carry these on their `typeName: TypeName`).
    let attributesOfTypeName (tn: TypeName<SyntaxToken>) : Attributes<SyntaxToken> voption =
        let (TypeName(attributes = a)) = tn
        a
