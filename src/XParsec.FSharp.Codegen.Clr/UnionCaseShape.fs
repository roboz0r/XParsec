namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

[<RequireQualifiedAccess>]
module UnionCaseFields =

    /// Whether a case's payload fields land on a `TypeDef` of their own: a case type in a
    /// hierarchy regime, or the union itself when it declares a single case.
    let ownType (regime: UnionRegime) : bool =
        UnionRegime.isHierarchy regime || regime = UnionRegime.SingleCase

    /// The metadata name of the `Get_<Case>_<i>` reader of field `index` of `caseName`.
    let getterName (caseName: string) (index: int) : string = sprintf "Get_%s_%d" caseName index

    /// The metadata name of the `Get_<Case>` reader returning the case's `Payload_<Case>`
    /// view.
    let viewGetterName (caseName: string) : string = "Get_" + caseName

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
        FTClass(key unionKey caseName, EqArray.ofList args)

/// The value types nested in a `StructTagged` union, each keyed like a hierarchy case type:
/// `Payload`, its `ExplicitLayout` overlay `Data`, one `Data_<Case>` per case with unmanaged
/// fields, and one public `Payload_<Case>` view per payload-bearing case.
[<RequireQualifiedAccess>]
module UnionPayloadType =

    let payloadName = "Payload"
    let overlayName = "Data"
    let caseDataName (caseName: string) : string = "Data_" + caseName
    let viewName (caseName: string) : string = "Payload_" + caseName

    /// The union's field holding its `Payload`; also the view's field holding its copy.
    let payloadFieldName = "_payload"
    /// The `Payload` field holding the overlay.
    let overlayFieldName = "_data"

    let private nested (unionKey: TypeKey) (name: string) : TypeKey =
        SymbolKeyOps.typeKeyOfContainer (TypeContainer.InType unionKey) name 0

    let payloadKey (unionKey: TypeKey) : TypeKey = nested unionKey payloadName
    let overlayKey (unionKey: TypeKey) : TypeKey = nested unionKey overlayName
    let caseDataKey (unionKey: TypeKey) (caseName: string) : TypeKey = nested unionKey (caseDataName caseName)
    let viewKey (unionKey: TypeKey) (caseName: string) : TypeKey = nested unionKey (viewName caseName)

    /// `Payload` at the union's own type arguments.
    let payloadTy (unionKey: TypeKey) (args: FrozenType list) : FrozenType =
        FTClass(payloadKey unionKey, EqArray.ofList args)

    /// `Payload` in the scope of the union's own `arity` typars: the type its `_payload`
    /// field, its `.ctor` parameter and each view's wrapped field are declared at.
    let payloadTyDeclaring (unionKey: TypeKey) (arity: int) : FrozenType =
        payloadTy unionKey (declaringMarkers arity)

    let overlayTy (unionKey: TypeKey) : FrozenType =
        FTClass(overlayKey unionKey, EqArray.empty)

    let caseDataTy (unionKey: TypeKey) (caseName: string) : FrozenType =
        FTClass(caseDataKey unionKey caseName, EqArray.empty)

    /// One case's view at the union's own type arguments.
    let viewTy (unionKey: TypeKey) (caseName: string) (args: FrozenType list) : FrozenType =
        FTClass(viewKey unionKey caseName, EqArray.ofList args)
