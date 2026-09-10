namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Immutable
open System.Reflection.Metadata
open Vesper
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis

/// The Vesper keys whose CLR metadata identity is a BCL type. The emitter writes rows and
/// blob type names against the `System` spelling.
module ClrAttributeNames =

    /// `System.AttributeUsageAttribute`, the CLR spelling of
    /// `RuntimeNames.attributeUsageAttributeKey`.
    [<Literal>]
    let AttributeUsageNamespace = "System"

    [<Literal>]
    let AttributeUsageName = "AttributeUsageAttribute"

    /// `System.AttributeTargets`, the CLR spelling of `RuntimeNames.attributeTargetsKey`:
    /// the flags enum typing `System.AttributeUsageAttribute`'s constructor parameter.
    [<Literal>]
    let AttributeTargetsNamespace = "System"

    [<Literal>]
    let AttributeTargetsName = "AttributeTargets"

    /// The full name a `SerString` carries for a BCL-identified enum key; `ValueNone` for a
    /// key with no BCL identity.
    let tryBclEnumFullName (key: TypeKey) : string voption =
        if key = RuntimeNames.attributeTargetsKey then
            ValueSome(AttributeTargetsNamespace + "." + AttributeTargetsName)
        else
            ValueNone

/// A compiler-owned `CustomAttribute` row on a type or method, written without a source
/// attribute.
[<RequireQualifiedAccess>]
type SyntheticAttribute =
    /// `System.Runtime.CompilerServices.IsReadOnlyAttribute`: a consumer reads it as C#'s
    /// `readonly struct` and skips defensive copies.
    | IsReadOnly
    /// `System.Runtime.CompilerServices.IsByRefLikeAttribute`: the CLR confines the value
    /// type to the stack.
    | IsByRefLike
    /// `System.ComponentModel.EditorBrowsable(EditorBrowsableState.Never)`: the member is
    /// withheld from IDE completion.
    | EditorBrowsableNever

module SyntheticAttribute =

    /// The II.23.3 blob: prolog, the fixed arguments, zero named arguments.
    let blob (attr: SyntheticAttribute) : BlobBuilder =
        let b = BlobBuilder()
        b.WriteUInt16(1us)

        match attr with
        | SyntheticAttribute.IsReadOnly
        | SyntheticAttribute.IsByRefLike -> ()
        | SyntheticAttribute.EditorBrowsableNever -> b.WriteInt32(1)

        b.WriteUInt16(0us)
        b

/// Why an attribute argument failed to encode under II.23.3.
[<RequireQualifiedAccess>]
type AttributeEncodeFailure =
    /// `tryTypeName` missed `ty`: a reified type, or a named enum lacking a reflection name.
    | UnspellableType of ty: FrozenType
    /// II.23.3 lacks an `Elem` or `FieldOrPropType` for `ty`: a pointer-width integral,
    /// `decimal`, `unit`, a string-valued enum, or a shape mismatched with its position.
    | UnencodableValue of ty: FrozenType

/// ECMA-335 II.23.3 `CustomAttrib` blob encoding over the frozen constant-folded arguments.
/// A `tryTypeName` parameter yields a type's SerString: assembly-qualified for a referenced
/// type, the full name alone for a type of the assembly under emission.
module internal AttributeBlob =

    type Encoded = Result<ImmutableArray<byte>, AttributeEncodeFailure>

    /// The single `FieldOrPropType` byte `key` spells: `bool`, `char`, a fixed-width integral,
    /// `float32`, `float`, `string`, `0x50` for `Vesper.Type` and `0x51` for the boxed `obj`.
    /// `ValueNone` for every other key.
    let private tryFieldOrPropTypeByte (key: TypeKey) : byte voption =
        match RuntimeNames.intKindOfKey key with
        | ValueSome IntKind.SByte -> ValueSome 0x04uy
        | ValueSome IntKind.Byte -> ValueSome 0x05uy
        | ValueSome IntKind.Int16 -> ValueSome 0x06uy
        | ValueSome IntKind.UInt16 -> ValueSome 0x07uy
        | ValueSome IntKind.Int32 -> ValueSome 0x08uy
        | ValueSome IntKind.UInt32 -> ValueSome 0x09uy
        | ValueSome IntKind.Int64 -> ValueSome 0x0Auy
        | ValueSome IntKind.UInt64 -> ValueSome 0x0Buy
        | ValueSome IntKind.NativeInt
        | ValueSome IntKind.UNativeInt -> ValueNone
        | ValueNone ->
            if key = RuntimeNames.boolKey then ValueSome 0x02uy
            elif key = RuntimeNames.charKey then ValueSome 0x03uy
            elif key = RuntimeNames.float32Key then ValueSome 0x0Cuy
            elif key = RuntimeNames.floatKey then ValueSome 0x0Duy
            elif key = RuntimeNames.stringKey then ValueSome 0x0Euy
            elif key = RuntimeNames.runtimeTypeKey then ValueSome 0x50uy
            elif key = RuntimeNames.objKey then ValueSome 0x51uy
            else ValueNone

    /// The bytes `write` puts into a fresh blob.
    let private bytes (write: BlobBuilder -> unit) : ImmutableArray<byte> =
        let b = BlobBuilder()
        write b
        b.ToImmutableArray()

    /// The SerString of `ty`; `UnspellableType` where `tryTypeName` misses.
    let private spell
        (tryTypeName: FrozenType -> string voption)
        (ty: FrozenType)
        : Result<string, AttributeEncodeFailure> =
        match tryTypeName ty with
        | ValueSome name -> Ok name
        | ValueNone -> Error(AttributeEncodeFailure.UnspellableType ty)

    /// The `FieldOrPropType` of `ty`: an enum is `0x55` + its SerString, an array `0x1D` +
    /// its item type.
    let rec private tryFieldOrPropType (tryTypeName: FrozenType -> string voption) (ty: FrozenType) : Encoded =
        match ty with
        | FTEnum _ ->
            spell tryTypeName ty
            |> Result.map (fun name ->
                bytes (fun b ->
                    b.WriteByte 0x55uy
                    b.WriteSerializedString name
                )
            )
        | FTArray elemTy ->
            tryFieldOrPropType tryTypeName elemTy
            |> Result.map (fun elem ->
                bytes (fun b ->
                    b.WriteByte 0x1Duy
                    b.WriteBytes elem
                )
            )
        | FTConst(key, args) when args.IsEmpty ->
            match tryFieldOrPropTypeByte key with
            | ValueSome t -> Ok(bytes (fun b -> b.WriteByte t))
            | ValueNone -> Error(AttributeEncodeFailure.UnencodableValue ty)
        | _ -> Error(AttributeEncodeFailure.UnencodableValue ty)

    /// One scalar's `Elem`. Pointer-width integrals, `unit` and `decimal` (fsc lowers it to
    /// `DecimalConstantAttribute`) are refused.
    let private tryScalarElem (v: TConstValue) : Encoded =
        let write: (BlobBuilder -> unit) voption =
            match v with
            | TConstValue.Bool x -> ValueSome(fun b -> b.WriteBoolean x)
            | TConstValue.Char c -> ValueSome(fun b -> b.WriteUInt16(uint16 c))
            | TConstValue.Integral v ->
                match v with
                | IntValue.SByte n -> ValueSome(fun b -> b.WriteSByte n)
                | IntValue.Byte n -> ValueSome(fun b -> b.WriteByte n)
                | IntValue.Int16 n -> ValueSome(fun b -> b.WriteInt16 n)
                | IntValue.UInt16 n -> ValueSome(fun b -> b.WriteUInt16 n)
                | IntValue.Int32 n -> ValueSome(fun b -> b.WriteInt32 n)
                | IntValue.UInt32 n -> ValueSome(fun b -> b.WriteUInt32 n)
                | IntValue.Int64 n -> ValueSome(fun b -> b.WriteInt64 n)
                | IntValue.UInt64 n -> ValueSome(fun b -> b.WriteUInt64 n)
                | IntValue.NativeInt _
                | IntValue.UNativeInt _ -> ValueNone
            | TConstValue.Float32 f -> ValueSome(fun b -> b.WriteSingle f)
            | TConstValue.Float f -> ValueSome(fun b -> b.WriteDouble f)
            | TConstValue.String s -> ValueSome(fun b -> b.WriteSerializedString s)
            | TConstValue.Decimal _
            | TConstValue.Unit -> ValueNone

        match write with
        | ValueSome w -> Ok(bytes w)
        | ValueNone -> Error(AttributeEncodeFailure.UnencodableValue(FTConst(TConstValue.canonKey v, Block.empty)))

    /// One scalar or type value's `Elem` at its type `ty`: a `Type` is the SerString of the
    /// type's name. A string-valued (TS) enum is refused; `tryElemAt` encodes `null` and an
    /// array at their position.
    let private tryValueElem (tryTypeName: FrozenType -> string voption) (ty: FrozenType) (r: TConstResult) : Encoded =
        match ty, r with
        | FTEnum _, TConstResult.Scalar(TConstValue.Integral _ as v) -> tryScalarElem v
        | FTEnum _, _ -> Error(AttributeEncodeFailure.UnencodableValue ty)
        | _, TConstResult.Scalar v -> tryScalarElem v
        | _, TConstResult.TypeVal t ->
            spell tryTypeName t
            |> Result.map (fun name -> bytes (fun b -> b.WriteSerializedString name))
        | _, TConstResult.Null
        | _, TConstResult.ArrayVal _ -> Error(AttributeEncodeFailure.UnencodableValue ty)

    /// The `Elem` of `e` at the position's declared type. An `obj` position boxes: the value's
    /// own `FieldOrPropType` precedes its `Elem`.
    let rec private tryElemAt
        (tryTypeName: FrozenType -> string voption)
        (declared: FrozenType)
        (e: TConstExpr)
        : Encoded =
        match declared, e with
        | FTObj, TConstExpr.Null _ ->
            Ok(
                bytes (fun b ->
                    b.WriteByte 0x0Euy
                    b.WriteByte 0xFFuy
                )
            )
        | FTObj, _ -> tryBoxedElem tryTypeName (TConstExpr.ty e) e
        | FTArray _, TConstExpr.Null _ -> Ok(bytes (fun b -> b.WriteUInt32 0xFFFFFFFFu))
        | _, TConstExpr.Null _ -> Ok(bytes (fun b -> b.WriteByte 0xFFuy))
        | FTArray elemTy, TConstExpr.ArrayLit(items = items) ->
            Block.mapResult (tryElemAt tryTypeName elemTy) items
            |> Result.map (fun elems ->
                bytes (fun b ->
                    b.WriteUInt32(uint32 elems.Length)

                    for elem in elems do
                        b.WriteBytes elem
                )
            )
        | _, TConstExpr.ArrayLit _ -> Error(AttributeEncodeFailure.UnencodableValue(TConstExpr.ty e))
        | _ -> tryValueElem tryTypeName declared (TConstExpr.result e)

    /// `e` boxed at a position of type `ty`: its `FieldOrPropType`, then its `Elem`.
    and private tryBoxedElem (tryTypeName: FrozenType -> string voption) (ty: FrozenType) (e: TConstExpr) : Encoded =
        tryElemAt tryTypeName ty e
        |> Result.bind (fun value ->
            tryFieldOrPropType tryTypeName ty
            |> Result.map (fun fieldOrPropType ->
                bytes (fun b ->
                    b.WriteBytes fieldOrPropType
                    b.WriteBytes value
                )
            )
        )

    /// The innermost type in `e` that II.23.3 cannot encode at a position of declared type
    /// `declared`. Every type is taken as spellable, so an unnamed type yields `ValueNone`.
    let tryUnencodable (declared: FrozenType) (e: TConstExpr) : FrozenType voption =
        let spellable _ = ValueSome ""

        match tryBoxedElem spellable declared e with
        | Ok _
        | Error(AttributeEncodeFailure.UnspellableType _) -> ValueNone
        | Error(AttributeEncodeFailure.UnencodableValue ty) -> ValueSome ty

    /// A named argument's segment: `0x54` for a property or `0x53` for a field, the member's
    /// `FieldOrPropType`, its SerString name, then the `Elem` at the member's type.
    let private tryNamedSegment
        (tryTypeName: FrozenType -> string voption)
        (m: TAttributeMember, e: TConstExpr)
        : Encoded =
        let kind =
            match m with
            | TAttributeMember.Property _ -> 0x54uy
            | TAttributeMember.Field _ -> 0x53uy

        tryElemAt tryTypeName m.Ty e
        |> Result.bind (fun value ->
            tryFieldOrPropType tryTypeName m.Ty
            |> Result.map (fun fieldOrPropType ->
                bytes (fun b ->
                    b.WriteByte kind
                    b.WriteBytes fieldOrPropType
                    b.WriteSerializedString m.Name
                    b.WriteBytes value
                )
            )
        )

    /// The blob: prolog `0x0001`, the fixed arguments in the constructor's parameter order,
    /// each at its parameter's type, the named-argument count, then the named arguments in
    /// written order; the first failing argument in that order.
    let tryEncode
        (tryTypeName: FrozenType -> string voption)
        (ctorParams: Block<FrozenType>)
        (args: Block<TAttributeArg>)
        : Result<BlobBuilder, AttributeEncodeFailure> =
        let fixedArg (i: int) (paramTy: FrozenType) : Encoded =
            match args |> Block.tryFind (fun a -> a.Target = TAttributeArgTarget.Parameter i) with
            | ValueSome a -> tryElemAt tryTypeName paramTy a.Expr
            | ValueNone -> failwithf "AttributeBlob: the front end filled no argument for constructor parameter %d" i

        let named =
            Block.ofList
                [
                    for a in args do
                        match a.Target with
                        | TAttributeArgTarget.Member m -> yield m, a.Expr
                        | TAttributeArgTarget.Parameter _ -> ()
                ]

        Block.mapResult id (Block.mapi fixedArg ctorParams)
        |> Result.bind (fun fixedSegments ->
            Block.mapResult (tryNamedSegment tryTypeName) named
            |> Result.map (fun namedSegments ->
                let b = BlobBuilder()
                b.WriteUInt16 1us

                for segment in fixedSegments do
                    b.WriteBytes segment

                b.WriteUInt16(uint16 namedSegments.Length)

                for segment in namedSegments do
                    b.WriteBytes segment

                b
            )
        )
