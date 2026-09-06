namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis

/// Whether a value of some type holds a GC reference anywhere in its physical layout.
[<RequireQualifiedAccess>]
type Unmanagedness =
    /// Every byte is a scalar or a nested unmanaged struct.
    | Unmanaged
    /// A reference, or a value holding one.
    | Managed
    /// `blocker` is the innermost type whose layout the provider cannot enumerate.
    | Undetermined of blocker: FrozenType

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Unmanagedness =

    /// The classification of a value holding both parts, keeping `a`'s blocker on a tie.
    let combine (a: Unmanagedness) (b: Unmanagedness) : Unmanagedness =
        match a, b with
        | Unmanagedness.Managed, _
        | _, Unmanagedness.Managed -> Unmanagedness.Managed
        | Unmanagedness.Undetermined _, _ -> a
        | _, Unmanagedness.Undetermined _ -> b
        | Unmanagedness.Unmanaged, Unmanagedness.Unmanaged -> Unmanagedness.Unmanaged

    /// The classification of a value holding every part; `Unmanaged` for none.
    let ofParts (parts: Unmanagedness seq) : Unmanagedness =
        Seq.fold combine Unmanagedness.Unmanaged parts

    /// An intrinsic settled by the CLR type its `(# … #)` binding denotes: a primitive value
    /// element type or a pointer is `Unmanaged`, an array is `Managed`. `ValueNone` for any
    /// other platform type, and for a key with no binding.
    let private ofPlatformId (symbols: ICodegenSymbols) (key: TypeKey) : Unmanagedness voption =
        match symbols.TryPlatformTypeId key with
        | ValueSome id when PlatformTypeIds.isUnmanagedScalar id -> ValueSome Unmanagedness.Unmanaged
        | ValueSome id when PlatformTypeIds.isArray id -> ValueSome Unmanagedness.Managed
        | ValueSome _
        | ValueNone -> ValueNone

    /// A nominal instantiated at `args`, classified through its declared shape. `part`
    /// classifies an instantiated field type. A shape the provider cannot resolve or
    /// enumerate is `Undetermined` at `t`.
    let private ofNominal
        (symbols: ICodegenSymbols)
        (part: FrozenType -> Unmanagedness)
        (t: FrozenType)
        (key: TypeKey)
        (args: EqArray<FrozenType>)
        : Unmanagedness =
        let instantiate (template: FrozenType) : FrozenType =
            FrozenTypeBridge.substituteDeclaring (EqArray.toArray args) template

        let fields (templates: FrozenType seq) : Unmanagedness =
            templates |> Seq.map (instantiate >> part) |> ofParts

        match symbols.TryLookupType key with
        | ValueSome(ExternalTypeShape.Abbrev(_, body)) -> part (instantiate body)
        // An enum is a struct over its underlying primitive.
        | ValueSome(ExternalTypeShape.Enum(underlying = underlying)) -> part (FTConst(underlying, EqArray.empty))
        | shape ->
            match symbols.IsValueType key, shape with
            | ValueSome false, _ -> Unmanagedness.Managed
            | ValueNone, _ -> Unmanagedness.Undetermined t
            | ValueSome true, ValueSome(ExternalTypeShape.Record r) -> fields (seq { for f in r.Fields -> f.Frozen })
            | ValueSome true, ValueSome(ExternalTypeShape.Union u) ->
                fields (
                    seq {
                        for c in u.Cases do
                            yield! c.FrozenFieldTypes
                    }
                )
            | ValueSome true, _ -> Unmanagedness.Undetermined t

    /// `path` is the chain of instantiations under classification. A repeat classifies
    /// `Undetermined` at the repeated type, terminating on a cyclic hand-built symbol view.
    let rec private classify (symbols: ICodegenSymbols) (path: FrozenType list) (t: FrozenType) : Unmanagedness =
        if List.contains t path then
            Unmanagedness.Undetermined t
        else
            let nominal (key: TypeKey) (args: EqArray<FrozenType>) : Unmanagedness =
                ofNominal symbols (classify symbols (t :: path)) t key args

            let intrinsic (key: TypeKey) (args: EqArray<FrozenType>) : Unmanagedness =
                match ofPlatformId symbols key with
                | ValueSome settled -> settled
                | ValueNone -> nominal key args

            match t with
            | FTConst(key, args) -> intrinsic key args
            | FTLiteral v -> intrinsic (RuntimeNames.literalBaseKey v) EqArray.empty
            | FTEnum key -> nominal key EqArray.empty
            | FTRecord(key, args)
            | FTUnion(key, args)
            | FTClass(key, args) -> nominal key args
            | FTTuple items -> ofParts (Seq.map (classify symbols (t :: path)) items)
            | FTFun _
            | FTOr _
            | FTTypar _
            | FTLocalTypar _ -> Unmanagedness.Managed
            | FTKeyOf _
            | FTIndexedAccess _
            | FTConditional _
            | FTMeasure _
            | FTUnknown _ -> Unmanagedness.Undetermined t

    /// The classification of a stored value of type `t`. Every nominal reached is resolved
    /// through `symbols`, this compilation's and referenced ones alike.
    let ofFrozen (symbols: ICodegenSymbols) (t: FrozenType) : Unmanagedness = classify symbols [] t
