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

/// ECMA-335 II.23.3 `CustomAttrib` blob encoding over the frozen constant-folded arguments.
/// A `tryTypeName` parameter yields a type's SerString: assembly-qualified for a referenced
/// type, the full name alone for a type of the assembly under emission.
module internal AttributeBlob =

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

    /// The `FieldOrPropType` of `ty`: an enum is `0x55` + its SerString, an array `0x1D` +
    /// its item type. `ValueNone` for a type with no II.23.3 encoding.
    let rec private tryFieldOrPropType
        (tryTypeName: FrozenType -> string voption)
        (ty: FrozenType)
        : ImmutableArray<byte> voption =
        match ty with
        | FTEnum _ ->
            tryTypeName ty
            |> ValueOption.map (fun name ->
                bytes (fun b ->
                    b.WriteByte 0x55uy
                    b.WriteSerializedString name
                )
            )
        | FTArray elemTy ->
            tryFieldOrPropType tryTypeName elemTy
            |> ValueOption.map (fun elem ->
                bytes (fun b ->
                    b.WriteByte 0x1Duy
                    b.WriteBytes elem
                )
            )
        | FTConst(key, args) when args.IsEmpty ->
            tryFieldOrPropTypeByte key
            |> ValueOption.map (fun t -> bytes (fun b -> b.WriteByte t))
        | _ -> ValueNone

    /// One scalar's `Elem` write. `ValueNone` for pointer-width integrals, `decimal` (fsc
    /// lowers it to `DecimalConstantAttribute`) and `unit`, which have no `Elem` encoding.
    let private tryScalarElem (v: TConstValue) : (BlobBuilder -> unit) voption =
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

    /// One scalar or type value's `Elem` at its type `ty`: a `Type` is the SerString of the
    /// type's name. `ValueNone` for a string-valued (TS) enum, and a scalar or type outside
    /// the `Elem` encoding.
    let private tryValueElem
        (tryTypeName: FrozenType -> string voption)
        (ty: FrozenType)
        (r: TConstResult)
        : ImmutableArray<byte> voption =
        match ty, r with
        | FTEnum _, TConstResult.Scalar(TConstValue.Integral _ as v) -> tryScalarElem v |> ValueOption.map bytes
        | FTEnum _, _ -> ValueNone
        | _, TConstResult.Scalar v -> tryScalarElem v |> ValueOption.map bytes
        | _, TConstResult.TypeVal t ->
            tryTypeName t
            |> ValueOption.map (fun name -> bytes (fun b -> b.WriteSerializedString name))
        | _, TConstResult.Null
        | _, TConstResult.ArrayVal _ -> ValueNone

    /// The `Elem` of `e` at the position's declared type. An `obj` position boxes: the value's
    /// own `FieldOrPropType` precedes its `Elem`. `ValueNone` for a value outside the `Elem`
    /// encoding.
    let rec private tryElemAt
        (tryTypeName: FrozenType -> string voption)
        (declared: FrozenType)
        (e: TConstExpr)
        : ImmutableArray<byte> voption =
        match declared, e with
        | FTObj, TConstExpr.Null _ ->
            ValueSome(
                bytes (fun b ->
                    b.WriteByte 0x0Euy
                    b.WriteByte 0xFFuy
                )
            )
        | FTObj, _ ->
            let ty = TConstExpr.ty e

            match tryFieldOrPropType tryTypeName ty, tryElemAt tryTypeName ty e with
            | ValueSome fieldOrPropType, ValueSome value ->
                ValueSome(
                    bytes (fun b ->
                        b.WriteBytes fieldOrPropType
                        b.WriteBytes value
                    )
                )
            | _ -> ValueNone
        | FTArray _, TConstExpr.Null _ -> ValueSome(bytes (fun b -> b.WriteUInt32 0xFFFFFFFFu))
        | _, TConstExpr.Null _ -> ValueSome(bytes (fun b -> b.WriteByte 0xFFuy))
        | FTArray elemTy, TConstExpr.ArrayLit(items = items) ->
            Block.tryMap (tryElemAt tryTypeName elemTy) items
            |> ValueOption.map (fun elems ->
                bytes (fun b ->
                    b.WriteUInt32(uint32 elems.Length)

                    for elem in elems do
                        b.WriteBytes elem
                )
            )
        | _, TConstExpr.ArrayLit _ -> ValueNone
        | _ -> tryValueElem tryTypeName declared (TConstExpr.result e)

    /// A named argument's segment: `0x54` for a property or `0x53` for a field, the member's
    /// `FieldOrPropType`, its SerString name, then the `Elem` at the member's type.
    let private tryNamedSegment
        (tryTypeName: FrozenType -> string voption)
        (m: TAttributeMember, e: TConstExpr)
        : ImmutableArray<byte> voption =
        let kind =
            match m with
            | TAttributeMember.Property _ -> 0x54uy
            | TAttributeMember.Field _ -> 0x53uy

        match tryFieldOrPropType tryTypeName m.Ty, tryElemAt tryTypeName m.Ty e with
        | ValueSome fieldOrPropType, ValueSome value ->
            ValueSome(
                bytes (fun b ->
                    b.WriteByte kind
                    b.WriteBytes fieldOrPropType
                    b.WriteSerializedString m.Name
                    b.WriteBytes value
                )
            )
        | _ -> ValueNone

    /// The blob: prolog `0x0001`, the fixed arguments in the constructor's parameter order,
    /// each at its parameter's type, the named-argument count, then the named arguments in
    /// written order. `ValueNone` where any argument is unencodable.
    let tryEncode
        (tryTypeName: FrozenType -> string voption)
        (ctorParams: Block<FrozenType>)
        (args: Block<TAttributeArg>)
        : BlobBuilder voption =
        let fixedArg (i: int) (paramTy: FrozenType) : ImmutableArray<byte> voption =
            args
            |> Block.tryFind (fun a -> a.Target = TAttributeArgTarget.Parameter i)
            |> ValueOption.bind (fun a -> tryElemAt tryTypeName paramTy a.Expr)

        let named =
            Block.ofList
                [
                    for a in args do
                        match a.Target with
                        | TAttributeArgTarget.Member m -> yield m, a.Expr
                        | TAttributeArgTarget.Parameter _ -> ()
                ]

        match Block.tryMap id (Block.mapi fixedArg ctorParams), Block.tryMap (tryNamedSegment tryTypeName) named with
        | ValueSome fixedSegments, ValueSome namedSegments ->
            let b = BlobBuilder()
            b.WriteUInt16 1us

            for segment in fixedSegments do
                b.WriteBytes segment

            b.WriteUInt16(uint16 namedSegments.Length)

            for segment in namedSegments do
                b.WriteBytes segment

            ValueSome b
        | _ -> ValueNone
