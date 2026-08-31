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

    /// Whether the union carries a `_tag : int32` discriminant, declared on the union type,
    /// written by its `.ctor` and loaded by every consumer that settles or orders a value.
    /// `SingleCase` has one shape to settle and `TypeTested` discriminates by runtime type.
    let hasTag (regime: UnionRegime) : bool =
        match regime with
        | UnionRegime.SingleCase
        | UnionRegime.TypeTested -> false
        | UnionRegime.EnumLike
        | UnionRegime.StructTagged
        | UnionRegime.Tagged -> true

/// The parameter list a union's own `.ctor` declares.
[<RequireQualifiedAccess>]
type UnionCtorShape =
    /// `(_tag, every case's field)` in flat declaration order, `newobj`ed whole by each
    /// case factory.
    | FlatTagged
    /// `(every case's field)` — a single-case struct union, whose one case is every case.
    | Flat
    /// `(_tag)`, stamped by whichever case `.ctor` chains it.
    | TagOnly
    /// `()`. A flat reference union's factories `stfld` after the `newobj`; a `TypeTested`
    /// base has no field to write.
    | Nullary

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module UnionCtorShape =

    /// The one regime-to-ctor mapping, shared by the emitted `MethodDef` and the
    /// `MemberRef` a generic union's use sites resolve through, so the two agree on the
    /// signature. The value kind is a separate argument because `SingleCase` and
    /// `EnumLike` classify the same either way.
    let ofRegime (valueKind: UnionValueKind) (regime: UnionRegime) : UnionCtorShape =
        if valueKind.IsValueType then
            if UnionRegime.hasTag regime then
                UnionCtorShape.FlatTagged
            else
                UnionCtorShape.Flat
        elif regime = UnionRegime.Tagged then
            UnionCtorShape.TagOnly
        else
            UnionCtorShape.Nullary
