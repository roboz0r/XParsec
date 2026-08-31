namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis

/// The metadata shape a union declaration is emitted in, selected by the value kind, the case
/// count and whether any case carries fields. These are FSC's four representations for a
/// reference union at its threshold of four, plus the one shape a `[<Struct>]` union takes.
///
/// The value kind is settled by the time a regime exists, so `StructTagged` is the only regime
/// a `[<Struct>]` union of two or more cases with a payload reaches, and `TypeTested` /
/// `Tagged` are reference-union regimes.
[<RequireQualifiedAccess>]
type UnionRegime =
    /// A single case, its fields inline on the union type itself, with no discriminant.
    | SingleCase
    /// Two or more cases, every one nullary: `_tag` discriminates, and each case is a
    /// cached singleton.
    | EnumLike
    /// A `[<Struct>]` union of two or more cases with at least one carrying fields: `_tag`
    /// plus every case's fields, co-resident on the one value type.
    | StructTagged
    /// A reference union of two or three cases with at least one carrying fields: a nested
    /// type per case on an abstract base, discriminated by an instance's runtime type.
    | TypeTested
    /// A reference union of four or more cases with at least one carrying fields: a nested
    /// type per case on an abstract base, discriminated by `_tag`.
    | Tagged

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module UnionRegime =

    /// The highest case count emitted as `UnionRegime.TypeTested`. At four cases the `isinst`
    /// chain grows to three tests and `ldfld _tag` becomes the cheaper discriminant.
    [<Literal>]
    let TypeTestCaseLimit = 3

    /// The regime a union of this shape is emitted in. `caseCount` is at least 1.
    let classify (valueKind: UnionValueKind) (caseCount: int) (anyCaseCarriesFields: bool) : UnionRegime =
        match caseCount with
        | 1 -> UnionRegime.SingleCase
        | _ when not anyCaseCarriesFields -> UnionRegime.EnumLike
        | _ when valueKind.IsValueType -> UnionRegime.StructTagged
        | _ when caseCount <= TypeTestCaseLimit -> UnionRegime.TypeTested
        | _ -> UnionRegime.Tagged

    /// The regime a referenced package's union was emitted in. A referenced package is
    /// compiled by this same emitter, so an identical shape yields an identical regime and
    /// its cases are spelled the same way.
    let ofExternalShape (u: ExternalUnionShape) : UnionRegime =
        let valueKind =
            if u.IsValueType then
                UnionValueKind.Struct
            else
                UnionValueKind.RefType

        classify valueKind u.Cases.Length (u.Cases |> EqArray.exists (fun c -> not c.FrozenFieldTypes.IsEmpty))

    /// Whether the regime nests a `TypeDef` per case on an abstract base.
    let isHierarchy (regime: UnionRegime) : bool =
        match regime with
        | UnionRegime.TypeTested
        | UnionRegime.Tagged -> true
        | UnionRegime.SingleCase
        | UnionRegime.EnumLike
        | UnionRegime.StructTagged -> false

    /// Whether the union's emitted layout declares a `_tag : int32` field row, written by
    /// its `.ctor`. `TypeTested` keeps its row until step 4 of the hierarchy plan drops it.
    let hasTagRow (regime: UnionRegime) : bool =
        match regime with
        | UnionRegime.SingleCase -> false
        | UnionRegime.EnumLike
        | UnionRegime.StructTagged
        | UnionRegime.TypeTested
        | UnionRegime.Tagged -> true

    /// Whether a consumer settles or orders a value of this union by loading `_tag`.
    /// `SingleCase` has no discriminant and `TypeTested` discriminates by runtime type.
    let readsTag (regime: UnionRegime) : bool =
        match regime with
        | UnionRegime.SingleCase
        | UnionRegime.TypeTested -> false
        | UnionRegime.EnumLike
        | UnionRegime.StructTagged
        | UnionRegime.Tagged -> true
