namespace XParsec.FSharp.Codegen.Common

open Vesper
open XParsec.FSharp.SemanticAnalysis

/// The backend reading of a measured nominal: `float<m>` is `float`, the claim's body over its
/// type-slot arguments with the measure dropped.
[<RequireQualifiedAccess>]
module MeasureErasure =

    /// The claim expanded over its type-slot arguments, itself erased. `lookup` resolves the
    /// claim's key to its abbreviation.
    let rec private expandClaim
        (lookup: TypeKey -> ExternalTypeShape voption)
        (claim: FrozenType.MeasuredClaim)
        : FrozenType =
        match lookup claim.Key with
        | ValueSome(ExternalTypeShape.Abbrev { Body = body }) ->
            erase lookup (FrozenTypeBridge.substituteDeclaring claim.TypeArgs body)
        | other ->
            failwithf
                "MeasureErasure: the measured claim %s is not an abbreviation the referenced contracts publish: %A"
                claim.Key.DeclaredPath
                other

    /// `t` with every measured nominal replaced by its abbreviation's expansion. A bare
    /// `FTMeasure` passes through: it is a leaf its parent drops.
    and erase (lookup: TypeKey -> ExternalTypeShape voption) (t: FrozenType) : FrozenType =
        eraseNode lookup (FrozenType.mapChildren (erase lookup) t)

    /// `erase` at ONE node whose children are already erased, the shape of a `FrozenTypeTable`
    /// view.
    and eraseNode (lookup: TypeKey -> ExternalTypeShape voption) (t: FrozenType) : FrozenType =
        match t with
        | FrozenType.MeasuredNominal claim -> expandClaim lookup claim
        | t -> t

    /// The file's pools read through `eraseNode`: every type column, and every member key's
    /// argument signature, materialises erased.
    let pools (lookup: TypeKey -> ExternalTypeShape voption) (pools: FrozenPools) : FrozenPools =
        { pools with
            Types = FrozenTypeTable.OfRowsWith(pools.Types.Rows, eraseNode lookup)
        }
