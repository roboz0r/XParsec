namespace XParsec.FSharp.SemanticAnalysis

/// How the compiling target lays out a value of some type.
[<RequireQualifiedAccess>]
type TypeLayout =
    /// Flat, in place.
    | Value
    /// Through a reference to a cell the target allocates.
    | Reference
    /// Neither the target nor a declaration said, which is never a refusal: a compile
    /// composing no platform reaches it at every primitive.
    | Unanswered

/// What a type's layout turns on, and all it turns on. `SemType` and `FrozenType` are one
/// type either side of freezing, so each projects onto this and `resolve` reads nothing
/// else — the two images cannot answer differently.
[<RequireQualifiedAccess>]
type LayoutShape =
    /// An intrinsic: only the target answers, because no declaration stands behind `int`.
    | Primitive of key: TypeKey
    /// A record or class, whose `[<Struct>]` the target may erase.
    | Nominal of key: TypeKey
    /// An enum, whose declaration asks for a value type wherever the target lays one out.
    | Enum of key: TypeKey
    /// A closure, a union, or an anonymous union erasing to a boxed reference.
    | Reference
    /// A tuple. Structural, so there is no key to ask the target under, and the backend
    /// that encodes it is the only source: `System.ValueTuple`n` on the CLR, an array on JS.
    | Encoded
    /// An unevaluated type-level computation, settled at neither layout until it evaluates.
    | Unevaluated
    /// Not ground, or not key-addressed.
    | Opaque

[<RequireQualifiedAccess>]
module TypeLayout =

    let ofValueness (isValueType: bool) : TypeLayout =
        if isValueType then
            TypeLayout.Value
        else
            TypeLayout.Reference

    let ofAnswer (answer: bool voption) : TypeLayout =
        match answer with
        | ValueSome isValueType -> ofValueness isValueType
        | ValueNone -> TypeLayout.Unanswered

    let private orElse (fallback: unit -> TypeLayout) (first: TypeLayout) : TypeLayout =
        match first with
        | TypeLayout.Unanswered -> fallback ()
        | settled -> settled

    /// A literal is laid out as the base primitive it erases to.
    let private literalShape (v: LiteralConst) : LayoutShape =
        match RuntimeNames.literalBaseKey v with
        | SymbolKey.Type key -> LayoutShape.Primitive key
        | _ -> LayoutShape.Opaque

    /// Through the union-find Link chain, so a bound `TyVar` reaches the shape it stands for.
    let shapeOf (store: TypeStore) (t: SemType) : LayoutShape =
        match UnionFind.zonkShallow store t with
        | TyConst(SymbolKey.Type key, _) -> LayoutShape.Primitive key
        | TyRecord(key, _)
        | TyClass(key, _) -> LayoutShape.Nominal key
        | TyEnum key -> LayoutShape.Enum key
        | TyLiteral v -> literalShape v
        | TyFun _
        | TyUnion _
        | TyOr _ -> LayoutShape.Reference
        | TyTuple _ -> LayoutShape.Encoded
        | TyKeyOf _
        | TyIndexedAccess _
        | TyConditional _ -> LayoutShape.Unevaluated
        | TyConst _
        | TyVar _
        | TyTypar _
        | TyUnknown _ -> LayoutShape.Opaque

    /// The same projection over the frozen image, arm for arm.
    let shapeOfFrozen (t: FrozenType) : LayoutShape =
        match t with
        | FTConst(SymbolKey.Type key, _) -> LayoutShape.Primitive key
        | FTRecord(key, _)
        | FTClass(key, _) -> LayoutShape.Nominal key
        | FTEnum key -> LayoutShape.Enum key
        | FTLiteral v -> literalShape v
        | FTFun _
        | FTUnion _
        | FTOr _ -> LayoutShape.Reference
        | FTTuple _ -> LayoutShape.Encoded
        | FTKeyOf _
        | FTIndexedAccess _
        | FTConditional _ -> LayoutShape.Unevaluated
        | FTConst _
        | FTTypar _
        | FTLocalTypar _
        | FTUnknown _ -> LayoutShape.Opaque

    /// `platform` is what the TARGET lays a key out as and `declared` what that type's own
    /// declaration asked for. The target leads: `[<Struct>]` is the request, the target is
    /// what it gets. `Encoded` reaches no answer here — only a backend can settle it.
    let resolve (platform: TypeKey -> TypeLayout) (declared: TypeKey -> TypeLayout) (shape: LayoutShape) : TypeLayout =
        match shape with
        | LayoutShape.Primitive key -> platform key
        | LayoutShape.Nominal key -> platform key |> orElse (fun () -> declared key)
        | LayoutShape.Enum key -> platform key |> orElse (fun () -> TypeLayout.Value)
        | LayoutShape.Reference -> TypeLayout.Reference
        | LayoutShape.Encoded
        | LayoutShape.Unevaluated
        | LayoutShape.Opaque -> TypeLayout.Unanswered

    let private platformOf (ctx: PassContext) (key: TypeKey) : TypeLayout = ofAnswer (ctx.Provider.IsValueType key)

    /// What a nominal's declaration asked for, from this compilation or from the unit that
    /// published it.
    let private declaredOf (ctx: PassContext) (key: TypeKey) : TypeLayout =
        TypeRegistry.tryRecordByKey ctx.Types key
        |> ValueOption.map (fun info -> info.IsValueType)
        |> ValueOption.orElseWith (fun () ->
            TypeRegistry.tryClassByKey ctx.Types key
            |> ValueOption.map (fun info -> info.IsValueType)
        )
        |> ValueOption.orElseWith (fun () ->
            ctx.Provider.TryLookupType(SymbolKey.Type key)
            |> ValueOption.bind ExternalSymbols.declaredValueType
        )
        |> ofAnswer

    /// The compiling target's layout for an already-projected shape, for a caller that
    /// branches on the shape too.
    let ofShape (ctx: PassContext) (shape: LayoutShape) : TypeLayout =
        resolve (platformOf ctx) (declaredOf ctx) shape

    /// The compiling target's layout for `t`. `Unanswered` at a tuple: the front end holds no
    /// key for one, so the backend that encodes it answers instead.
    let ofSemType (ctx: PassContext) (t: SemType) : TypeLayout = ofShape ctx (shapeOf ctx.Store t)
