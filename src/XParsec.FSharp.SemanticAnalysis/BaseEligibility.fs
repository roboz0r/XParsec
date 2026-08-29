namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

// One answer to "may this written type be an `inherit` parent?", shared by the implementation
// and signature front ends. Each front end resolves the written name its own way; the verdict
// on the resolved type, and the diagnostic each rejection carries, live here.

/// The verdict on a resolved `inherit` parent. `Base` admits it; every other case names the
/// rejection `BaseEligibility.admit` diagnoses.
[<RequireQualifiedAccess>]
type BaseVerdict =
    /// A non-interface class, or a heritable primitive's canon.
    | Base of SemType
    | Interface of key: TypeKey
    | NotAClass of key: TypeKey
    /// A shape with no nominal head: a tuple, a function type, a typar.
    | NotANominal
    | UnknownName of name: string
    /// A `TyUnknown` whose producer blamed the source; a second report would double up.
    | AlreadyDiagnosed
    /// A `TyUnknown` whose producer stayed silent.
    | Undiagnosed of reason: UnknownReason

[<RequireQualifiedAccess>]
module BaseEligibility =

    /// Whether `key` names an interface, answered by the local class registry, then the
    /// provider. An unknown key answers `false`.
    let isInterfaceKey (ctx: PassContext) (key: TypeKey) : bool =
        match TypeRegistry.tryClassByKey ctx.Types key with
        | ValueSome info -> info.IsInterface
        | ValueNone ->
            match ctx.Provider.TryLookupType key with
            | ValueSome(ExternalTypeShape.Class c) -> c.IsInterface
            | ValueSome(ExternalTypeShape.IntrinsicInterface _) -> true
            | _ -> false

    /// Classify a resolved `inherit` parent. `isInterface` answers for a nominal key by
    /// whichever authority declared it.
    let classify (isInterface: TypeKey -> bool) (ty: SemType) : BaseVerdict =
        match ty with
        | TyClass(key, _) when isInterface key -> BaseVerdict.Interface key
        | TyClass _
        | TyConst _ -> BaseVerdict.Base ty
        | TyRecord(key, _)
        | TyUnion(key, _) -> BaseVerdict.NotAClass key
        | TyEnum key -> BaseVerdict.NotAClass key
        | TyUnknown(UnknownReason.UndefinedName _) -> BaseVerdict.AlreadyDiagnosed
        | TyUnknown reason -> BaseVerdict.Undiagnosed reason
        | TyVar _
        | TyFun _
        | TyTuple _
        | TyOr _
        | TyLiteral _
        | TyKeyOf _
        | TyIndexedAccess _
        | TyConditional _
        | TyTypar _ -> BaseVerdict.NotANominal

    /// Admit a `Base` verdict; diagnose any other at `tok` and yield `ValueNone`.
    let admit (ctx: PassContext) (tok: SyntaxToken) (verdict: BaseVerdict) : SemType voption =
        match verdict with
        | BaseVerdict.Base ty -> ValueSome ty
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
        | BaseVerdict.UnknownName name ->
            ctx.Report(tok, Kind.Message(sprintf "Cannot inherit from unknown type '%s'" name))
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
