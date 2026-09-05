namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.SemanticAnalysis.Passes

/// A record, union or class declared in this compilation.
[<RequireQualifiedAccess>]
type LocalNominal =
    | Record of RecordTypeInfo
    | Union of UnionTypeInfo
    | Class of ClassTypeInfo

/// A record, union or class published by a referenced unit.
[<RequireQualifiedAccess>]
type PublishedNominal =
    | Record of ExternalRecordShape
    | Union of ExternalUnionShape
    | Class of ExternalClassShape

/// The declaration behind a nominal key, wherever it was declared.
[<RequireQualifiedAccess>]
type NominalDecl =
    | Local of LocalNominal
    | Published of PublishedNominal

[<RequireQualifiedAccess>]
module LocalNominal =

    let isValueType (d: LocalNominal) : bool =
        match d with
        | LocalNominal.Record info -> info.IsValueType
        | LocalNominal.Union info -> info.IsValueType
        | LocalNominal.Class info -> info.IsValueType

    let equalitySupport (d: LocalNominal) : EqualityVerdict =
        match d with
        | LocalNominal.Record info -> info.EqualitySupport
        | LocalNominal.Union info -> info.EqualitySupport
        | LocalNominal.Class info -> info.EqualitySupport

    let comparisonSupport (d: LocalNominal) : ComparisonVerdict =
        match d with
        | LocalNominal.Record info -> info.ComparisonSupport
        | LocalNominal.Union info -> info.ComparisonSupport
        | LocalNominal.Class info -> info.ComparisonSupport

    /// The instance field types at `args`: a record's fields, every case's fields of a
    /// union, and a class's primary-ctor parameters, `val` fields and instance `let`
    /// bindings, each of which the class carries as a field.
    let fieldTypes (store: TypeStore) (d: LocalNominal) (args: EqArray<SemType>) : SemType list =
        let at (typeParams: EqArray<string * TyVarId>) (tys: SemType seq) : SemType list =
            let subst = UnificationEngineCore.mkNamedTypeSubst store typeParams args
            [ for t in tys -> UnificationEngineCore.substituteWith store subst t ]

        match d with
        | LocalNominal.Record info -> at info.TypeParams (seq { for f in info.Fields -> f.Type })
        | LocalNominal.Union info ->
            at
                info.TypeParams
                (seq {
                    for c in info.Cases do
                        yield! c.Fields
                })
        | LocalNominal.Class info ->
            at
                info.TypeParams
                (seq {
                    for p in info.CtorParams -> p.Type
                    for f in info.InstanceFields -> f.Type
                    for l in ClassPreamble.lets info.InstancePreamble -> l.Type
                })

[<RequireQualifiedAccess>]
module PublishedNominal =

    let isValueType (d: PublishedNominal) : bool =
        match d with
        | PublishedNominal.Record shape -> shape.IsValueType
        | PublishedNominal.Union shape -> shape.IsValueType
        | PublishedNominal.Class shape -> shape.Flags.IsValueType

    /// The instance field types at `args`. `ValueNone` for a class, whose storage is
    /// private to the publishing unit.
    let fieldTypes (d: PublishedNominal) (args: EqArray<SemType>) : SemType list voption =
        let at (fts: FrozenType seq) : SemType list voption =
            let declaringArgs = EqArray.toArray args
            ValueSome [ for ft in fts -> FrozenTypeBridge.instantiateDeclaring ft declaringArgs ]

        match d with
        | PublishedNominal.Record shape -> at (seq { for f in shape.Fields -> f.Frozen })
        | PublishedNominal.Union shape ->
            at (
                seq {
                    for c in shape.Cases do
                        yield! c.FrozenFieldTypes
                }
            )
        | PublishedNominal.Class _ -> ValueNone

[<RequireQualifiedAccess>]
module NominalDecl =

    /// A local declaration shadows a published one under the same key.
    let tryOfKey (ctx: PassContext) (key: TypeKey) : NominalDecl voption =
        match TypeRegistry.tryRecordByKey ctx.Types key with
        | ValueSome info -> ValueSome(NominalDecl.Local(LocalNominal.Record info))
        | ValueNone ->
            match TypeRegistry.tryUnionByKey ctx.Types key with
            | ValueSome info -> ValueSome(NominalDecl.Local(LocalNominal.Union info))
            | ValueNone ->
                match TypeRegistry.tryClassByKey ctx.Types key with
                | ValueSome info -> ValueSome(NominalDecl.Local(LocalNominal.Class info))
                | ValueNone ->
                    match ctx.Provider.TryLookupType key with
                    | ValueSome(ExternalTypeShape.Record shape) ->
                        ValueSome(NominalDecl.Published(PublishedNominal.Record shape))
                    | ValueSome(ExternalTypeShape.Union shape) ->
                        ValueSome(NominalDecl.Published(PublishedNominal.Union shape))
                    | ValueSome(ExternalTypeShape.Class shape) ->
                        ValueSome(NominalDecl.Published(PublishedNominal.Class shape))
                    | _ -> ValueNone

    /// The `[<Struct>]` the declaration asked for, which the target may erase.
    let isValueType (d: NominalDecl) : bool =
        match d with
        | NominalDecl.Local l -> LocalNominal.isValueType l
        | NominalDecl.Published p -> PublishedNominal.isValueType p

    let isInterface (d: NominalDecl) : bool =
        match d with
        | NominalDecl.Local(LocalNominal.Class info) -> info.IsInterface
        | NominalDecl.Published(PublishedNominal.Class shape) -> shape.IsInterface
        | NominalDecl.Local(LocalNominal.Record _ | LocalNominal.Union _)
        | NominalDecl.Published(PublishedNominal.Record _ | PublishedNominal.Union _) -> false

    let isAbstract (d: NominalDecl) : bool =
        match d with
        | NominalDecl.Local(LocalNominal.Class info) -> info.Declared.IsAbstract
        | NominalDecl.Published(PublishedNominal.Class shape) -> shape.Flags.Declared.IsAbstract
        | NominalDecl.Local(LocalNominal.Record _ | LocalNominal.Union _)
        | NominalDecl.Published(PublishedNominal.Record _ | PublishedNominal.Union _) -> false

    let allowsNullLiteral (d: NominalDecl) : bool =
        match d with
        | NominalDecl.Local(LocalNominal.Class info) -> info.Declared.AllowNullLiteral
        | NominalDecl.Published(PublishedNominal.Class shape) -> shape.Flags.Declared.AllowNullLiteral
        | NominalDecl.Local(LocalNominal.Record _ | LocalNominal.Union _)
        | NominalDecl.Published(PublishedNominal.Record _ | PublishedNominal.Union _) -> false

    /// A declared parameterless constructor, of any accessibility. A record, a union and a
    /// value type's implicit one declare none.
    let hasParameterlessCtor (d: NominalDecl) : bool =
        match d with
        | NominalDecl.Local(LocalNominal.Class info) ->
            (info.HasPrimaryCtor && info.CtorParams.Length = 0)
            || info.SecondaryCtors |> Array.exists (fun c -> c.Params.Length = 0)
        | NominalDecl.Published(PublishedNominal.Class shape) ->
            shape.Members
            |> EqArray.exists (fun m ->
                m.Name = ".ctor"
                && m.Signature.ArgGroups.Length = 1
                && m.Signature.ArgGroups.[0] = RuntimeNames.unitTy
            )
        | NominalDecl.Local(LocalNominal.Record _ | LocalNominal.Union _)
        | NominalDecl.Published(PublishedNominal.Record _ | PublishedNominal.Union _) -> false

    /// The instance field types at `args`. `ValueNone` for a published class.
    let fieldTypes (store: TypeStore) (d: NominalDecl) (args: EqArray<SemType>) : SemType list voption =
        match d with
        | NominalDecl.Local l -> ValueSome(LocalNominal.fieldTypes store l args)
        | NominalDecl.Published p -> PublishedNominal.fieldTypes p args
