namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

// The verdicts on "may this written type be an `inherit` parent?" and "may this written
// type be implemented with `interface … with`?", with the diagnostic each rejection
// carries; shared by the implementation and signature front ends.

/// The verdict on a resolved `inherit` parent. `Base` admits it; every other case identifies
/// the rejection `BaseEligibility.admit` diagnoses.
[<RequireQualifiedAccess>]
type BaseVerdict =
    /// A non-interface class, or a heritable primitive's canon.
    | Base of BaseParent
    | Interface of key: TypeKey
    | NotAClass of key: TypeKey
    /// A shape with no nominal head: a tuple, a function type, a typar.
    | NotANominal
    /// A `TyUnknown` already diagnosed at its source; a second report would double up.
    | AlreadyDiagnosed
    /// A `TyUnknown` with no diagnostic reported at its source.
    | Undiagnosed of reason: UnknownReason

/// The verdict on a type written in an `interface` clause. `Iface` admits it on both front
/// ends; `IfaceCanon` is admitted only under `BaseEligibility.admitSigImpl`. Every other
/// case identifies the rejection `BaseEligibility.admitImpl` diagnoses.
[<RequireQualifiedAccess>]
type ImplVerdict =
    /// A nominal interface declaration.
    | Iface of SemNominal
    /// A capability's canon (`seq<'T>`, `disposable`): an interface, carried as a `TyConst`.
    | IfaceCanon of SemNominal
    | NotAnInterface of key: TypeKey
    /// A shape with no nominal head: a tuple, a function type, a typar.
    | NotANominal
    /// A `TyUnknown` already diagnosed at its source; a second report would double up.
    | AlreadyDiagnosed
    /// A `TyUnknown` with no diagnostic reported at its source.
    | Undiagnosed of reason: UnknownReason

[<RequireQualifiedAccess>]
module BaseEligibility =

    /// The head shape `classify` and `classifyImpl` share before each applies its policy.
    [<RequireQualifiedAccess>]
    type private TypeHead =
        | Class of SemNominal
        | Const of SemNominal
        /// A record, union or enum head.
        | Data of TypeKey
        /// A `TyUnknown` already diagnosed at its source.
        | Diagnosed
        /// A `TyUnknown` with no diagnostic reported at its source.
        | Undiagnosed of UnknownReason
        /// A shape with no nominal head: a tuple, a function type, a typar.
        | NonNominal

    let private typeHead (ty: SemType) : TypeHead =
        match ty with
        | TyClass(key, args) -> TypeHead.Class(NominalG.ofClass key args)
        | TyConst(key, args) -> TypeHead.Const(NominalG.ofConst key args)
        | TyRecord(key, _)
        | TyUnion(key, _) -> TypeHead.Data key
        | TyEnum key -> TypeHead.Data key
        | TyUnknown(UnknownReason.UndefinedName _) -> TypeHead.Diagnosed
        | TyUnknown reason -> TypeHead.Undiagnosed reason
        | TyVar _
        | TyFun _
        | TyTuple _
        | TyOr _
        | TyLiteral _
        | TyKeyOf _
        | TyIndexedAccess _
        | TyConditional _
        | TyTypar _ -> TypeHead.NonNominal

    /// Whether `key` denotes an interface: the local class registry first, then the
    /// provider. An unresolved key yields `false`.
    let isInterfaceKey (ctx: PassContext) (key: TypeKey) : bool =
        match TypeRegistry.tryClassByKey ctx.Types key with
        | ValueSome info -> info.IsInterface
        | ValueNone ->
            match ctx.Provider.TryLookupType key with
            | ValueSome(ExternalTypeShape.Class c) -> c.IsInterface
            | ValueSome(ExternalTypeShape.IntrinsicInterface _) -> true
            | _ -> false

    /// Whether `key` is a heritable primitive's canon: the file's own intrinsic bindings
    /// first, then the provider.
    let isHeritableCanon (ctx: PassContext) (key: TypeKey) : bool =
        match ctx.Types.IntrinsicBindings.TryGetValue key with
        | true, binding -> binding.Heritable
        | _ ->
            match ctx.Provider.TryLookupType key with
            | ValueSome(ExternalTypeShape.Intrinsic { Class = ValueSome surface }) -> surface.Heritable
            | _ -> false

    /// Classify a resolved `inherit` parent. `isInterface` and `isHeritable` are read for
    /// a nominal key from whichever authority declared it.
    let classify (isInterface: TypeKey -> bool) (isHeritable: TypeKey -> bool) (ty: SemType) : BaseVerdict =
        match typeHead ty with
        | TypeHead.Class n when isInterface n.Key -> BaseVerdict.Interface n.Key
        | TypeHead.Class n -> BaseVerdict.Base(BaseParentG.Class n)
        // A capability's canon written as a base reports the implement-it-instead message.
        | TypeHead.Const n when isInterface n.Key -> BaseVerdict.Interface n.Key
        | TypeHead.Const n when isHeritable n.Key -> BaseVerdict.Base(BaseParentG.PrimitiveCanon n)
        | TypeHead.Const n -> BaseVerdict.NotAClass n.Key
        | TypeHead.Data key -> BaseVerdict.NotAClass key
        | TypeHead.Diagnosed -> BaseVerdict.AlreadyDiagnosed
        | TypeHead.Undiagnosed reason -> BaseVerdict.Undiagnosed reason
        | TypeHead.NonNominal -> BaseVerdict.NotANominal

    /// Admit a `Base` verdict; diagnose any other at `tok` and yield `ValueNone`.
    let admit (ctx: PassContext) (tok: SyntaxToken) (verdict: BaseVerdict) : BaseParent voption =
        match verdict with
        | BaseVerdict.Base n -> ValueSome n
        | BaseVerdict.Interface key ->
            ctx.Report(
                tok,
                Kind.Message(
                    sprintf
                        "Cannot inherit from interface '%s'; implement it with 'interface %s with'"
                        key.Name
                        key.Name
                )
            )

            ValueNone
        | BaseVerdict.NotAClass key ->
            ctx.Report(
                tok,
                Kind.Message(sprintf "Cannot inherit from type '%s', because only classes are inheritable" key.Name)
            )

            ValueNone
        | BaseVerdict.NotANominal ->
            ctx.Report(tok, Kind.Message "Cannot inherit from this type, because only classes are inheritable")
            ValueNone
        | BaseVerdict.AlreadyDiagnosed -> ValueNone
        | BaseVerdict.Undiagnosed reason ->
            ctx.Report(
                tok,
                Kind.Message(
                    sprintf "Cannot inherit from this type (%s), because only classes are inheritable" reason.Render
                )
            )

            ValueNone

    /// Classify a type written in an `interface` clause. `isInterface` is read for a
    /// nominal key from whichever authority declared it.
    let classifyImpl (isInterface: TypeKey -> bool) (ty: SemType) : ImplVerdict =
        match typeHead ty with
        | TypeHead.Class n when isInterface n.Key -> ImplVerdict.Iface n
        | TypeHead.Const n when isInterface n.Key -> ImplVerdict.IfaceCanon n
        | TypeHead.Class n
        | TypeHead.Const n -> ImplVerdict.NotAnInterface n.Key
        | TypeHead.Data key -> ImplVerdict.NotAnInterface key
        | TypeHead.Diagnosed -> ImplVerdict.AlreadyDiagnosed
        | TypeHead.Undiagnosed reason -> ImplVerdict.Undiagnosed reason
        | TypeHead.NonNominal -> ImplVerdict.NotANominal

    /// Admit an `Iface` verdict; diagnose any other at `tok` and yield `ValueNone`. A
    /// capability's canon reports `NotYetSupported`: an implementation file cannot
    /// implement one.
    let admitImpl (ctx: PassContext) (tok: SyntaxToken) (verdict: ImplVerdict) : SemNominal voption =
        match verdict with
        | ImplVerdict.Iface n -> ValueSome n
        | ImplVerdict.IfaceCanon n ->
            ctx.Report(
                tok,
                Kind.NotYetSupported(
                    sprintf "implementing capability interface '%s' with 'interface … with'" n.Key.Name
                )
            )

            ValueNone
        | ImplVerdict.NotAnInterface key ->
            ctx.Report(tok, Kind.Message(sprintf "Type '%s' is not an interface" key.Name))
            ValueNone
        | ImplVerdict.NotANominal ->
            ctx.Report(tok, Kind.Message "This type is not an interface")
            ValueNone
        | ImplVerdict.AlreadyDiagnosed -> ValueNone
        | ImplVerdict.Undiagnosed reason ->
            ctx.Report(tok, Kind.Message(sprintf "This type is not an interface (%s)" reason.Render))
            ValueNone

    /// `admitImpl` under the signature policy, which additionally admits a capability's
    /// canon: a signature publishes the canon directly (`interface seq<'T>` freezes as
    /// `FTConst`).
    let admitSigImpl (ctx: PassContext) (tok: SyntaxToken) (verdict: ImplVerdict) : SemNominal voption =
        match verdict with
        | ImplVerdict.IfaceCanon n -> ValueSome n
        | other -> admitImpl ctx tok other
