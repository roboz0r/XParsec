namespace XParsec.FSharp.SemanticAnalysis

// The frozen attribute value model, and the verdicts derived from it. Ahead of `TastDecl`:
// the frozen `TTypeDeclG`'s equality / comparison / qualified-access members are views over
// its stored `TAttributes`, computed here.

/// One constant-folded attribute argument. `Name` is the property a named setter assigns
/// (`AllowMultiple = true`); `ValueNone` for a positional constructor argument. `EnumKey` is
/// the enum whose case(s) the value was written as (`AttributeTargets.Class ||| …`);
/// `ValueNone` for a non-enum constant.
type TAttributeArg =
    {
        Name: string voption
        Value: TConstValue
        EnumKey: TypeKey voption
    }

/// One written attribute: the declaration's `TypeKey` and its arguments, constant-folded, in
/// written order. An attribute with an argument outside the constant domain was diagnosed
/// and is absent from its position's `TAttributes`.
type TAttribute =
    {
        Key: TypeKey
        Args: EqArray<TAttributeArg>
    }

/// A declaration position's attributes in written order.
type TAttributes = EqArray<TAttribute>

/// The axis the attribute-legality matrices key on: one case per shape a declaration can
/// take, because a record may carry `[<ReferenceEquality>]` where a struct record cannot.
/// `[<Struct>]` is its own axis on a record, union or class: the value-type form differs on
/// both the legality matrices and `[<AttributeUsage>]` target enforcement.
[<RequireQualifiedAccess>]
type TypeDefnKind =
    | Record
    /// A `[<Struct>]` record.
    | StructRecord
    | Union
    /// A `[<Struct>]` union.
    | StructUnion
    | Enum
    /// A `type X = Y` alias.
    | Abbrev
    /// A `[<Struct>]` or byref-like class shape.
    | StructClass
    | RefClass
    | Interface

[<RequireQualifiedAccess>]
module TypeDefnKind =

    let ofRecord (isValueType: bool) : TypeDefnKind =
        if isValueType then
            TypeDefnKind.StructRecord
        else
            TypeDefnKind.Record

    let ofUnion (isValueType: bool) : TypeDefnKind =
        if isValueType then
            TypeDefnKind.StructUnion
        else
            TypeDefnKind.Union

    /// A byref-like class is a `StructClass`; `[<IsByRefLike>]` is judged on its own row.
    let ofClass (isValueType: bool) : TypeDefnKind =
        if isValueType then
            TypeDefnKind.StructClass
        else
            TypeDefnKind.RefClass

    /// For a layer that registers an interface as a class shape. `isInterface` wins: an
    /// interface takes `Interface` whatever its `[<Struct>]` says.
    let ofClassOrInterface (isInterface: bool) (isValueType: bool) : TypeDefnKind =
        if isInterface then
            TypeDefnKind.Interface
        else
            ofClass isValueType

    /// The kinds emitted as a value type.
    let isStruct (k: TypeDefnKind) : bool =
        match k with
        | TypeDefnKind.StructRecord
        | TypeDefnKind.StructUnion
        | TypeDefnKind.StructClass
        | TypeDefnKind.Enum -> true
        | TypeDefnKind.Record
        | TypeDefnKind.Union
        | TypeDefnKind.Abbrev
        | TypeDefnKind.RefClass
        | TypeDefnKind.Interface -> false

/// One row of the equality / comparison attribute matrix: the marker's key, the verdict it
/// stamps, the kinds it may legally sit on, and the diagnostic for the rest.
type EqCompAttr<'Verdict> =
    {
        Key: TypeKey
        Verdict: 'Verdict
        LegalKinds: TypeDefnKind list
        OnWrongKind: Kind
    }

/// The equality / comparison / qualified-access verdicts a declaration's resolved
/// attributes decide.
[<RequireQualifiedAccess>]
module AttributeVerdicts =

    let private anyKind =
        [
            TypeDefnKind.Record
            TypeDefnKind.StructRecord
            TypeDefnKind.Union
            TypeDefnKind.StructUnion
            TypeDefnKind.Enum
            TypeDefnKind.Abbrev
            TypeDefnKind.StructClass
            TypeDefnKind.RefClass
            TypeDefnKind.Interface
        ]

    /// A structural posture states what the FIELDS decide, so only the kinds that have them.
    let private structuralKinds =
        [
            TypeDefnKind.Record
            TypeDefnKind.StructRecord
            TypeDefnKind.Union
            TypeDefnKind.StructUnion
            TypeDefnKind.StructClass
        ]

    /// Reference identity needs a reference: a value type is barred (FS0376).
    let private referenceKinds = [ TypeDefnKind.Record; TypeDefnKind.Union ]

    /// A custom posture needs members to carry it, which an interface cannot declare.
    let private customKinds = anyKind |> List.except [ TypeDefnKind.Interface ]

    /// FS0934. `[<AllowNullLiteral>]` states that `null` inhabits the type, which only a
    /// reference can hold. A record or union reaches `null` through `| null` instead, a struct
    /// (an enum included) has no reference to hold it, and an abbreviation states nothing.
    let allowNullLiteralKinds = [ TypeDefnKind.RefClass; TypeDefnKind.Interface ]

    /// The equality axis. Table ORDER is the within-axis verdict priority.
    let equalityAttrs: EqCompAttr<EqualityVerdict> list =
        [
            {
                Key = RuntimeNames.structuralEqualityAttributeKey
                Verdict = EqualityVerdict.Structural
                LegalKinds = structuralKinds
                OnWrongKind = Kind.StructuralEqualityAttributeOnWrongKind
            }
            {
                Key = RuntimeNames.referenceEqualityAttributeKey
                Verdict = EqualityVerdict.Reference
                LegalKinds = referenceKinds
                OnWrongKind = Kind.StructuralEqualityAttributeOnWrongKind
            }
            {
                Key = RuntimeNames.noEqualityAttributeKey
                Verdict = EqualityVerdict.NoEquality
                LegalKinds = anyKind
                OnWrongKind = Kind.StructuralEqualityAttributeOnWrongKind
            }
            {
                Key = RuntimeNames.customEqualityAttributeKey
                Verdict = EqualityVerdict.Custom
                LegalKinds = customKinds
                OnWrongKind = Kind.CustomEqualityAttributeOnInterface
            }
        ]

    /// Likewise in priority order. Comparison is OPT-IN where equality is not: a record
    /// gets the synthesised pair only under `[<StructuralComparison>]`.
    let comparisonAttrs: EqCompAttr<ComparisonVerdict> list =
        [
            {
                Key = RuntimeNames.structuralComparisonAttributeKey
                Verdict = ComparisonVerdict.Structural
                LegalKinds = structuralKinds
                OnWrongKind = Kind.StructuralEqualityAttributeOnWrongKind
            }
            {
                Key = RuntimeNames.noComparisonAttributeKey
                Verdict = ComparisonVerdict.NoComparison
                LegalKinds = anyKind
                OnWrongKind = Kind.StructuralEqualityAttributeOnWrongKind
            }
            {
                Key = RuntimeNames.customComparisonAttributeKey
                Verdict = ComparisonVerdict.Custom
                LegalKinds = customKinds
                OnWrongKind = Kind.CustomEqualityAttributeOnInterface
            }
        ]

    let has (attrs: TAttributes) (key: TypeKey) : bool =
        attrs |> EqArray.exists (fun a -> a.Key = key)

    /// Every present row, in table order, so a contradictory mix stays visible to the FS0377 check.
    let presentRows (attrs: TAttributes) (rows: EqCompAttr<'Verdict> list) : EqCompAttr<'Verdict> list =
        rows |> List.filter (fun r -> has attrs r.Key)

    let isLegalOn (kind: TypeDefnKind) (r: EqCompAttr<'Verdict>) : bool = List.contains kind r.LegalKinds

    /// The complaint an illegal row reports on `kind`: FS0376 for `[<ReferenceEquality>]` on
    /// a value type, else the row's own.
    let wrongKindDiag (kind: TypeDefnKind) (r: EqCompAttr<'Verdict>) : Kind =
        if r.Key = RuntimeNames.referenceEqualityAttributeKey && TypeDefnKind.isStruct kind then
            Kind.ReferenceEqualityOnStruct
        else
            r.OnWrongKind

    // Off the LEGAL rows only: a posture refused for this kind does not stamp its verdict.
    let private firstLegalVerdict
        (kind: TypeDefnKind)
        (attrs: TAttributes)
        (rows: EqCompAttr<'Verdict> list)
        : 'Verdict voption =
        match presentRows attrs rows |> List.filter (isLegalOn kind) with
        | r :: _ -> ValueSome r.Verdict
        | [] -> ValueNone

    /// The verdict the attributes decide, else the kind's default: `Reference` for a
    /// reference class / interface, `Structural` for every data kind — a record with a
    /// mutable field included, matching fsc.
    let equalitySupport (kind: TypeDefnKind) (attrs: TAttributes) : EqualityVerdict =
        match firstLegalVerdict kind attrs equalityAttrs with
        | ValueSome v -> v
        | ValueNone ->
            match kind with
            | TypeDefnKind.RefClass
            | TypeDefnKind.Interface -> EqualityVerdict.Reference
            | _ -> EqualityVerdict.Structural

    /// The verdict the attributes decide, else `NoComparison`: comparison is opt-in on every kind.
    let comparisonSupport (kind: TypeDefnKind) (attrs: TAttributes) : ComparisonVerdict =
        match firstLegalVerdict kind attrs comparisonAttrs with
        | ValueSome v -> v
        | ValueNone -> ComparisonVerdict.NoComparison

    let isRequireQualifiedAccess (attrs: TAttributes) : bool =
        has attrs RuntimeNames.requireQualifiedAccessAttributeKey

    /// `[<AllowNullLiteral>]` present AND legal for `kind`; illegal presence is FS0934,
    /// reported by the validation pass.
    let allowNullLiteral (kind: TypeDefnKind) (attrs: TAttributes) : bool =
        has attrs RuntimeNames.allowNullLiteralAttributeKey
        && List.contains kind allowNullLiteralKinds
