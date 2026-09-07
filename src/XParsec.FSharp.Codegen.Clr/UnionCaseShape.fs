namespace XParsec.FSharp.Codegen.Clr

open Vesper
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

[<RequireQualifiedAccess>]
module UnionCaseFields =

    /// Whether a case's payload fields land on a `TypeDef` of their own: a case type in a
    /// hierarchy regime, or the union itself when it declares a single case.
    let ownType (regime: UnionRegime) : bool =
        UnionRegime.isHierarchy regime || regime = UnionRegime.SingleCase

    /// Each field's name in fsc's backing-field spelling, in declaration order:
    /// `of radius: float` ⇒ `_radius`, a lone positional field `item`, `item<n>` otherwise.
    let fscFieldNames (declared: string voption list) : string list =
        UnionCaseFieldName.ofCase declared
        |> List.map (fun n ->
            match n with
            | UnionCaseFieldName.Declared name -> "_" + name
            | UnionCaseFieldName.Lone -> "item"
            | UnionCaseFieldName.Positional i -> "item" + string i
        )

    /// The metadata name of the `Get_<Case>_<i>` reader of field `index` of `caseName`.
    /// Distinct cases spell distinct readers at every index.
    let getterName (caseName: string) (index: int) : string = sprintf "Get_%s_%d" caseName index

    /// The metadata name of the `GetPayload_<Case>` reader returning the case's
    /// `Payload_<Case>` view. Disjoint from `getterName` at every case name.
    let viewGetterName (caseName: string) : string = "GetPayload_" + caseName

[<RequireQualifiedAccess>]
module UnionCaseType =

    /// The `TypeKey` a hierarchy union's case type is registered under: the
    /// `TypeContainer.InType` container spells the emitted `Ns.Union`1+Case`, and the
    /// arity is 0 because a case adds no typars of its own.
    let key (unionKey: TypeKey) (caseName: string) : TypeKey =
        SymbolKeyOps.typeKeyOfContainer (TypeContainer.InType unionKey) caseName 0

    /// The case type at the union's own type arguments. Registered in `UserTypes`, and as a
    /// generic shape when the union is generic, so it encodes like any nominal.
    let ty (unionKey: TypeKey) (caseName: string) (args: FrozenType list) : FrozenType =
        FTClass(key unionKey caseName, Block.ofList args)

/// The value types a `StructTagged` union owns: `Payload` and one public `Payload_<Case>`
/// view per payload-bearing case, nested in the union; the non-generic `ExplicitLayout`
/// overlay `<Union>$Data` beside it, holding one `Data_<Case>` per case with unmanaged fields.
[<RequireQualifiedAccess>]
module UnionPayloadType =

    let payloadName = "Payload"
    let caseDataName (caseName: string) : string = "Data_" + caseName
    let viewName (caseName: string) : string = "Payload_" + caseName

    /// `<Union>$Data`, or `<Union>$Data$<N>` for a union of arity `N` (`GBox$Data$1`), so
    /// unions differing only in arity own distinct overlays. `$` is outside F# and C# source
    /// syntax, and the spelling carries no `` ` ``, so the name reads back as an arity-0 segment.
    let overlayName (unionKey: TypeKey) : string =
        match unionKey.TyparArity with
        | 0 -> unionKey.Name + "$Data"
        | n -> sprintf "%s$Data$%d" unionKey.Name n

    /// The union's field holding its `Payload`; also the view's field holding its copy.
    let payloadFieldName = "_payload"
    /// The `Payload` field holding the overlay.
    let overlayFieldName = "_data"

    let private nestedIn (outer: TypeKey) (name: string) : TypeKey =
        SymbolKeyOps.typeKeyOfContainer (TypeContainer.InType outer) name 0

    let payloadKey (unionKey: TypeKey) : TypeKey = nestedIn unionKey payloadName
    let viewKey (unionKey: TypeKey) (caseName: string) : TypeKey = nestedIn unionKey (viewName caseName)

    /// The overlay, a sibling of the union in the union's own container.
    let overlayKey (unionKey: TypeKey) : TypeKey =
        SymbolKeyOps.typeKeyOfContainer unionKey.Container (overlayName unionKey) 0

    /// One case's data struct, nested in the overlay.
    let caseDataKey (unionKey: TypeKey) (caseName: string) : TypeKey =
        nestedIn (overlayKey unionKey) (caseDataName caseName)

    /// `Payload` at the union's own type arguments.
    let payloadTy (unionKey: TypeKey) (args: FrozenType list) : FrozenType =
        FTClass(payloadKey unionKey, Block.ofList args)

    /// `Payload` in the scope of the union's own `arity` typars: the type its `_payload`
    /// field, its `.ctor` parameter and each view's wrapped field are declared at.
    let payloadTyDeclaring (unionKey: TypeKey) (arity: int<typeSlot>) : FrozenType =
        payloadTy unionKey (declaringMarkers unionKey arity)

    let overlayTy (unionKey: TypeKey) : FrozenType =
        FTClass(overlayKey unionKey, Block.empty)

    let caseDataTy (unionKey: TypeKey) (caseName: string) : FrozenType =
        FTClass(caseDataKey unionKey caseName, Block.empty)

    /// One case's view at the union's own type arguments.
    let viewTy (unionKey: TypeKey) (caseName: string) (args: FrozenType list) : FrozenType =
        FTClass(viewKey unionKey caseName, Block.ofList args)
