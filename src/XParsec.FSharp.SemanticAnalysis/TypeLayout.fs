namespace XParsec.FSharp.SemanticAnalysis

/// How the compiling target lays out a value of some type.
[<RequireQualifiedAccess>]
type TypeLayout =
    /// Flat, in place.
    | Value
    /// Through a reference to a cell the target allocates.
    | Reference
    /// Settled by neither the target nor a declaration, which is never a refusal: a compile
    /// composing no platform reaches it at every primitive.
    | Unsettled

/// What a type's layout turns on, and all it turns on. `SemType` and `FrozenType` each
/// project onto this, so the two images resolve to the same layout.
[<RequireQualifiedAccess>]
type LayoutShape =
    /// An intrinsic: settled by the target alone, no declaration standing behind `int`.
    | Primitive of key: TypeKey
    /// A record, union or class, whose `[<Struct>]` the target may erase.
    | Nominal of key: TypeKey
    /// An enum, whose declaration asks for a value type wherever the target lays one out.
    | Enum of key: TypeKey
    /// A closure, or an anonymous union erasing to a boxed reference.
    | Reference
    /// A tuple. Structural, so it carries no key of its own; the target supplies the nominal
    /// a tuple of this arity BECOMES, and that nominal's layout applies.
    | Tuple of arity: int
    /// An unevaluated type-level computation, settled at neither layout until it evaluates.
    | Unevaluated
    /// Not ground, or not key-addressed.
    | Opaque

/// The sources a layout question is resolved against; the front end and a backend supply
/// them differently.
type ILayoutOracle =
    /// What the compile has already SETTLED for a key, however that end settles it.
    abstract Settled: key: TypeKey -> TypeLayout
    /// What the type's own declaration ASKED for, reached only when nothing settled it.
    abstract Declared: key: TypeKey -> TypeLayout
    /// The target's facts, for a shape carrying no key of its own.
    abstract Platform: IPlatformFacts voption

[<RequireQualifiedAccess>]
module TypeLayout =

    let ofValueness (isValueType: bool) : TypeLayout =
        if isValueType then
            TypeLayout.Value
        else
            TypeLayout.Reference

    let ofSettled (isValueType: bool voption) : TypeLayout =
        match isValueType with
        | ValueSome v -> ofValueness v
        | ValueNone -> TypeLayout.Unsettled

    let private orElse (fallback: unit -> TypeLayout) (first: TypeLayout) : TypeLayout =
        match first with
        | TypeLayout.Unsettled -> fallback ()
        | settled -> settled

    /// A literal is laid out as the base primitive it erases to.
    let private literalShape (v: LiteralConst) : LayoutShape =
        LayoutShape.Primitive(RuntimeNames.literalBaseKey v)

    /// Through the union-find Link chain, so a bound `TyVar` reaches the shape it stands for.
    let shapeOf (store: TypeStore) (t: SemType) : LayoutShape =
        match UnionFind.zonkShallow store t with
        | TyConst(key, _) -> LayoutShape.Primitive key
        | TyRecord(key, _)
        | TyUnion(key, _)
        | TyClass(key, _) -> LayoutShape.Nominal key
        | TyEnum key -> LayoutShape.Enum key
        | TyLiteral v -> literalShape v
        | TyFun _
        | TyOr _ -> LayoutShape.Reference
        | TyTuple items -> LayoutShape.Tuple items.Length
        | TyKeyOf _
        | TyIndexedAccess _
        | TyConditional _ -> LayoutShape.Unevaluated
        | TyVar _
        | TyTypar _
        | TyUnknown _ -> LayoutShape.Opaque

    /// The same projection over the frozen image, arm for arm.
    let shapeOfFrozen (t: FrozenType) : LayoutShape =
        match t with
        | FTConst(key, _) -> LayoutShape.Primitive key
        | FTRecord(key, _)
        | FTUnion(key, _)
        | FTClass(key, _) -> LayoutShape.Nominal key
        | FTEnum key -> LayoutShape.Enum key
        | FTLiteral v -> literalShape v
        | FTFun _
        | FTOr _ -> LayoutShape.Reference
        | FTTuple items -> LayoutShape.Tuple items.Length
        | FTKeyOf _
        | FTIndexedAccess _
        | FTConditional _ -> LayoutShape.Unevaluated
        | FTTypar _
        | FTUnknown _
        // Argument position only, so never a value's own layout.
        | FTMeasure _ -> LayoutShape.Opaque

    /// What is settled leads what was asked for: `[<Struct>]` is the request, the target is
    /// what it gets. A tuple resolves through the nominal it becomes, asked for like any
    /// other key.
    let resolve (oracle: ILayoutOracle) (shape: LayoutShape) : TypeLayout =
        match shape with
        | LayoutShape.Primitive key -> oracle.Settled key
        | LayoutShape.Nominal key -> oracle.Settled key |> orElse (fun () -> oracle.Declared key)
        | LayoutShape.Enum key -> oracle.Settled key |> orElse (fun () -> TypeLayout.Value)
        | LayoutShape.Reference -> TypeLayout.Reference
        | LayoutShape.Tuple arity ->
            match oracle.Platform |> ValueOption.bind (fun p -> p.TupleType arity) with
            | ValueSome key -> oracle.Settled key
            | ValueNone -> TypeLayout.Unsettled
        | LayoutShape.Unevaluated
        | LayoutShape.Opaque -> TypeLayout.Unsettled

    let private platformOf (ctx: PassContext) (key: TypeKey) : TypeLayout =
        ofSettled (ctx.Provider.IsValueType key)

    /// What a nominal's declaration asked for, from this compilation or from the unit that
    /// published it.
    let private declaredOf (ctx: PassContext) (key: TypeKey) : TypeLayout =
        NominalDecl.tryOfKey ctx key
        |> ValueOption.map NominalDecl.isValueType
        |> ofSettled

    /// Nothing is emitted yet, so the target alone settles a key: every declaration, this
    /// compilation's or a referenced unit's, is still only the request.
    let private oracleOf (ctx: PassContext) : ILayoutOracle =
        { new ILayoutOracle with
            member _.Settled key = platformOf ctx key
            member _.Declared key = declaredOf ctx key
            member _.Platform = ctx.Provider.Platform
        }

    /// The compiling target's layout for an already-projected shape, for a caller that
    /// branches on the shape too.
    let ofShape (ctx: PassContext) (shape: LayoutShape) : TypeLayout = resolve (oracleOf ctx) shape

    let ofSemType (ctx: PassContext) (t: SemType) : TypeLayout = ofShape ctx (shapeOf ctx.Store t)
