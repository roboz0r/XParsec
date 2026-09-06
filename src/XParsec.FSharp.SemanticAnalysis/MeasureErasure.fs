namespace XParsec.FSharp.SemanticAnalysis

/// The backend reading of a measured nominal: `float<m>` is `float`, the arity-1 claim's body
/// with the measure argument dropped.
[<RequireQualifiedAccess>]
module MeasureErasure =

    /// The arity-1 claim `key` expanded over `args`, itself erased. `lookup` resolves `key` to
    /// its abbreviation.
    let rec private expandClaim
        (lookup: TypeKey -> ExternalTypeShape voption)
        (key: TypeKey)
        (args: EqArray<FrozenType>)
        : FrozenType =
        match lookup key with
        | ValueSome(ExternalTypeShape.Abbrev { Body = body }) ->
            erase lookup (FrozenTypeBridge.substituteDeclaring (EqArray.toArray args) body)
        | other ->
            failwithf
                "MeasureErasure: the measured claim %s is not an abbreviation the referenced contracts publish: %A"
                key.DeclaredPath
                other

    /// `t` with every measured nominal replaced by its abbreviation's expansion. A bare
    /// `FTMeasure` passes through: it is a leaf its parent drops.
    and erase (lookup: TypeKey -> ExternalTypeShape voption) (t: FrozenType) : FrozenType =
        eraseNode lookup (FrozenType.mapChildren (erase lookup) t)

    /// `erase` at ONE node whose children are already erased, the shape of a `FrozenTypeTable`
    /// view.
    and eraseNode (lookup: TypeKey -> ExternalTypeShape voption) (t: FrozenType) : FrozenType =
        match t with
        | FTConst(key, args) & FrozenType.MeasuredNominal _ -> expandClaim lookup key args
        | t -> t

    /// The file's pools read through `eraseNode`: every type column, and every member key's
    /// argument signature, materialises erased.
    let pools (lookup: TypeKey -> ExternalTypeShape voption) (pools: FrozenPools) : FrozenPools =
        { pools with
            Types = FrozenTypeTable.OfRowsWith(pools.Types.Rows, eraseNode lookup)
        }
