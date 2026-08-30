namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis

/// The metadata shape a union declaration is emitted in, selected by the case count and by
/// whether any case carries fields. These are FSC's four representations for a reference
/// union, at its threshold of four.
[<RequireQualifiedAccess>]
type UnionRegime =
    /// A single case, its fields inline on the union type itself, with no discriminant.
    | SingleCase
    /// Two or more cases, every one nullary: `_tag` discriminates, and each case is a
    /// cached singleton.
    | EnumLike
    /// Two or three cases, at least one carrying fields: a nested type per case on an
    /// abstract base, discriminated by an instance's runtime type. Reference unions only,
    /// since a value type has no subclass to test.
    | TypeTested
    /// `_tag` discriminates. A reference union of four or more cases with at least one
    /// carrying fields nests a type per case on an abstract base; a `[<Struct>]` union of
    /// two or more cases with at least one carrying fields holds `_tag` plus every case's
    /// fields on the one value type.
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
        | _ when caseCount <= TypeTestCaseLimit && not valueKind.IsValueType -> UnionRegime.TypeTested
        | _ -> UnionRegime.Tagged
