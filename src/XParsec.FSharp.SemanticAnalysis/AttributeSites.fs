namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Parser

/// The declaration element an attribute set is written on, classified as fsc classifies
/// elements for `[<AttributeUsage>]` enforcement (each case's flag set below is fsc's, read
/// off FS0842's wording).
[<RequireQualifiedAccess>]
type AttrTarget =
    | Class
    | Struct
    | Enum
    | Interface
    /// A `type X = Y` alias, which fsc admits at every type kind.
    | Abbreviation
    /// A method member, static or instance.
    | Method
    /// A property member, static or instance, including an auto-property.
    | Property
    | RecordField
    | UnionCase
    | EnumCase
    /// A parameter of a function, member or constructor.
    | Parameter
    /// A declared type parameter (`type T<[<Measure>] 'M>`).
    | TypeParameter
    /// A module-level `let` or `val` whose compiled shape is a non-function value.
    | ModuleValue
    /// A module-level `let` or `val` function, or a generalised (explicitly generic) value:
    /// fsc classifies both as methods.
    | ModuleFunction

[<RequireQualifiedAccess>]
module AttrTarget =

    /// The `AttributeTargetFlags` the element occupies.
    let mask (t: AttrTarget) : int =
        match t with
        | AttrTarget.Class -> AttributeTargetFlags.Class
        | AttrTarget.Struct -> AttributeTargetFlags.Struct
        | AttrTarget.Enum -> AttributeTargetFlags.Enum
        | AttrTarget.Interface -> AttributeTargetFlags.Interface
        | AttrTarget.Abbreviation ->
            AttributeTargetFlags.Class
            ||| AttributeTargetFlags.Struct
            ||| AttributeTargetFlags.Enum
            ||| AttributeTargetFlags.Interface
            ||| AttributeTargetFlags.Delegate
        | AttrTarget.Method -> AttributeTargetFlags.Method ||| AttributeTargetFlags.ReturnValue
        | AttrTarget.Property ->
            AttributeTargetFlags.Property
            ||| AttributeTargetFlags.Event
            ||| AttributeTargetFlags.ReturnValue
        | AttrTarget.RecordField -> AttributeTargetFlags.Property ||| AttributeTargetFlags.Field
        | AttrTarget.UnionCase -> AttributeTargetFlags.Method ||| AttributeTargetFlags.Property
        | AttrTarget.EnumCase -> AttributeTargetFlags.Field
        | AttrTarget.Parameter -> AttributeTargetFlags.Parameter
        | AttrTarget.TypeParameter -> AttributeTargetFlags.GenericParameter
        | AttrTarget.ModuleValue ->
            AttributeTargetFlags.Property
            ||| AttributeTargetFlags.Field
            ||| AttributeTargetFlags.ReturnValue
        | AttrTarget.ModuleFunction -> AttributeTargetFlags.Method ||| AttributeTargetFlags.ReturnValue

    /// fsc's classification of a module-level value: a function shape and a generalised
    /// (explicitly generic) value compile as methods, any other value as a property / field.
    let ofModuleValue (isFunctionShaped: bool) (isGeneric: bool) : AttrTarget =
        if isFunctionShaped || isGeneric then
            AttrTarget.ModuleFunction
        else
            AttrTarget.ModuleValue

[<RequireQualifiedAccess>]
module AttributeSite =

    /// The key an element's attribute position is filed under, off the element's own anchor
    /// token.
    let ofToken (tok: SyntaxToken) : NodeKey =
        NodeKey.ofToken tok NodeKind.DeclAttributes

    let ofSite (site: NodeSite) : NodeKey = ofToken site.Tok

    /// The site of attributes written on an element with no anchor token of its own (a
    /// parameter, a typar slot): the `[<` opening the first set. Declared for the
    /// `[<AttributeUsage>]` check alone.
    let ofSets (sets: Attributes<SyntaxToken>) : NodeKey voption =
        match Seq.tryHead sets with
        | Some(AttributeSet(lBracket = lb)) -> ValueSome(ofToken lb)
        | None -> ValueNone

[<NoEquality; NoComparison>]
type AttributePosition =
    {
        /// The element `[<AttributeUsage>]` is enforced against.
        UsedOn: AttrTarget
        Attributes: ResolvedAttributes
        Checked: TAttributes
    }

[<RequireQualifiedAccess>]
module AttributePosition =

    /// The checked attributes of a declared position. Fails where none is declared at `site`.
    let checkedAt (site: NodeKey) (position: AttributePosition voption) : TAttributes =
        match position with
        | ValueSome position -> position.Checked
        | ValueNone -> failwithf "No attribute position declared at %O" site

/// Every attribute position of a file, taken once declaration is closed.
[<Sealed>]
type SealedAttributePositions
    (inSourceOrder: AttributePosition[], bySite: IReadOnlyDictionary<NodeKey, AttributePosition>) =

    member _.InSourceOrder: AttributePosition[] = inSourceOrder

    member _.TryGet(site: NodeKey) : AttributePosition voption =
        match bySite.TryGetValue site with
        | true, position -> ValueSome position
        | false, _ -> ValueNone

    member this.CheckedAt(site: NodeKey) : TAttributes =
        AttributePosition.checkedAt site (this.TryGet site)

/// The attribute positions of a file, filed as each declaration walk reaches them. A position
/// declared after `Seal` would never reach the `[<AttributeUsage>]` check, so `Declare` fails
/// there rather than dropping it.
[<Sealed>]
type AttributePositionTable() =
    let bySite = Dictionary<NodeKey, AttributePosition>(HashIdentity.Structural)
    let mutable isSealed = false

    member _.Declare(site: NodeKey, position: AttributePosition) : unit =
        if isSealed then
            failwithf "AttributePositionTable: position %O declared after the table was sealed" site

        if bySite.ContainsKey site then
            failwithf "AttributePositionTable: position %O declared twice" site

        bySite[site] <- position

    member _.TryGet(site: NodeKey) : AttributePosition voption =
        match bySite.TryGetValue site with
        | true, position -> ValueSome position
        | false, _ -> ValueNone

    /// Close the table to further declarations and take its positions in source order.
    member _.Seal() : SealedAttributePositions =
        isSealed <- true

        let inSourceOrder =
            bySite
            |> Seq.sortBy (fun kv -> (SourcePos.ofNodeKey kv.Key).Offset)
            |> Seq.map (fun kv -> kv.Value)
            |> Array.ofSeq

        SealedAttributePositions(inSourceOrder, bySite)
