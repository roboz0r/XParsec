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
    /// A tuple. Structural, so it carries no key of its own; the target answers what a
    /// tuple of this many elements BECOMES, and that nominal's layout is the answer.
    | Tuple of arity: int
    /// An unevaluated type-level computation, settled at neither layout until it evaluates.
    | Unevaluated
    /// Not ground, or not key-addressed.
    | Opaque

/// The answers a layout question is resolved against; the front end and a backend fill
/// them differently.
type LayoutOracle =
    {
        /// What the compile has already SETTLED for a key, however that end settles it.
        Settled: TypeKey -> TypeLayout
        /// What the type's own declaration ASKED for, reached only when nothing settled it.
        Declared: TypeKey -> TypeLayout
        /// The target's facts, for a shape carrying no key of its own.
        Platform: IPlatformFacts voption
    }

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
        | TyTuple items -> LayoutShape.Tuple items.Length
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
        | FTTuple items -> LayoutShape.Tuple items.Length
        | FTKeyOf _
        | FTIndexedAccess _
        | FTConditional _ -> LayoutShape.Unevaluated
        | FTConst _
        | FTTypar _
        | FTLocalTypar _
        | FTUnknown _ -> LayoutShape.Opaque

    /// What is settled leads what was asked for: `[<Struct>]` is the request, the target is
    /// what it gets. A tuple resolves through the nominal it becomes, asked for like any
    /// other key.
    let resolve (oracle: LayoutOracle) (shape: LayoutShape) : TypeLayout =
        match shape with
        | LayoutShape.Primitive key -> oracle.Settled key
        | LayoutShape.Nominal key -> oracle.Settled key |> orElse (fun () -> oracle.Declared key)
        | LayoutShape.Enum key -> oracle.Settled key |> orElse (fun () -> TypeLayout.Value)
        | LayoutShape.Reference -> TypeLayout.Reference
        | LayoutShape.Tuple arity ->
            match oracle.Platform |> ValueOption.bind (fun p -> p.TupleType arity) with
            | ValueSome key -> oracle.Settled key
            | ValueNone -> TypeLayout.Unanswered
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

    /// Nothing is emitted yet, so the target alone settles a key: every declaration, this
    /// compilation's or a referenced unit's, is still only the request.
    let private oracleOf (ctx: PassContext) : LayoutOracle =
        {
            Settled = platformOf ctx
            Declared = declaredOf ctx
            Platform = ctx.Provider.Platform
        }

    /// The compiling target's layout for an already-projected shape, for a caller that
    /// branches on the shape too.
    let ofShape (ctx: PassContext) (shape: LayoutShape) : TypeLayout = resolve (oracleOf ctx) shape

    let ofSemType (ctx: PassContext) (t: SemType) : TypeLayout = ofShape ctx (shapeOf ctx.Store t)
