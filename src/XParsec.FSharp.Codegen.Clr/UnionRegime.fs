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

/// The parameter list a union's own `.ctor` declares. This `.ctor` is the only writer of
/// every field it takes, so all of them are `initonly`.
[<RequireQualifiedAccess>]
type UnionCtorShape =
    /// `(_tag, every case's field)` in flat declaration order, `newobj`ed whole by each
    /// case factory.
    | FlatTagged
    /// `(every case's field)` — a single-case union, whose one case is every case.
    | Flat
    /// `(_tag)`, stamped by whichever case `.ctor` chains it, or by the `.cctor` that
    /// constructs an enum-like case's singleton.
    | TagOnly
    /// `()` — a `TypeTested` base, which declares no field.
    | Nullary

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module UnionCtorShape =

    /// The one regime-to-ctor mapping, shared by the emitted `MethodDef` and the
    /// `MemberRef` a generic union's use sites resolve through, so the two agree on the
    /// signature. The value kind separates the two tagged forms: a `[<Struct>]` union
    /// holds every case's payload co-resident with the discriminant, where a reference
    /// union puts a case's payload on the case.
    let ofRegime (valueKind: UnionValueKind) (regime: UnionRegime) : UnionCtorShape =
        match regime with
        | UnionRegime.SingleCase -> UnionCtorShape.Flat
        | UnionRegime.TypeTested -> UnionCtorShape.Nullary
        | UnionRegime.EnumLike
        | UnionRegime.StructTagged
        | UnionRegime.Tagged ->
            if valueKind.IsValueType then
                UnionCtorShape.FlatTagged
            else
                UnionCtorShape.TagOnly

/// The body one union case's static factory takes. The arity is a case-level fact where
/// the regime is a union-level one, so this is selected per case.
[<RequireQualifiedAccess>]
type UnionFactoryShape =
    /// `ldsfld _unique_<Case>` — the instance the `.cctor` built.
    | Cached
    /// Forward every parameter to the union's own `.ctor`.
    | UnionCtor
    /// Forward every parameter to the case type's own `.ctor`.
    | CaseCtor
    /// Push the discriminant, this case's parameters, and a zeroed default for every other
    /// case's field, then `newobj` the flat `.ctor`.
    | StructTagged

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module UnionFactoryShape =

    /// The one (value kind × regime × arity) mapping to a factory body. `arity` is the
    /// case's field count.
    let ofCase (valueKind: UnionValueKind) (regime: UnionRegime) (arity: int) : UnionFactoryShape =
        match valueKind with
        | UnionValueKind.Struct ->
            if UnionRegime.hasTag regime then
                UnionFactoryShape.StructTagged
            else
                UnionFactoryShape.UnionCtor
        | UnionValueKind.RefType ->
            match arity with
            | 0 -> UnionFactoryShape.Cached
            | _ when UnionRegime.isHierarchy regime -> UnionFactoryShape.CaseCtor
            | _ -> UnionFactoryShape.UnionCtor
