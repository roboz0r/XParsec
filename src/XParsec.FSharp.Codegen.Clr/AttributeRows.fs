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
    /// `float32`, `float`, `string`, and `0x50` for `Vesper.Type`. `ValueNone` for every other key.
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

    /// One value's `Elem` at its type `ty`: a `Type` is the SerString of the type's name, an
    /// array its `uint32` count then each item's `Elem`. `ValueNone` for `null`, a
    /// string-valued (TS) enum, and a scalar or type outside the `Elem` encoding.
    let rec private tryElem
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
        | FTArray elemTy, TConstResult.ArrayVal items ->
            Block.tryMap (tryElem tryTypeName elemTy) items
            |> ValueOption.map (fun elems ->
                bytes (fun b ->
                    b.WriteUInt32(uint32 elems.Length)

                    for elem in elems do
                        b.WriteBytes elem
                )
            )
        | _, TConstResult.Null
        | _, TConstResult.ArrayVal _ -> ValueNone

    [<RequireQualifiedAccess>]
    type private ArgKind =
        | Positional
        | Named

    /// One argument's finished bytes: a positional argument's `Elem`; a named PROPERTY
    /// argument's `0x54`, `FieldOrPropType`, SerString name, then `Elem`.
    type private EncodedArg =
        {
            Kind: ArgKind
            Bytes: ImmutableArray<byte>
        }

    /// `ValueNone` where the value has no `Elem` encoding, or a named argument's type has no
    /// `FieldOrPropType` spelling (a string-valued TS enum). A positional argument's type is
    /// the ctor's parameter type and is unwritten.
    let private tryEncodeArg (tryTypeName: FrozenType -> string voption) (a: TAttributeArg) : EncodedArg voption =
        let ty = TConstExpr.ty a.Expr

        tryElem tryTypeName ty (TConstExpr.result a.Expr)
        |> ValueOption.bind (fun value ->
            match a.Name with
            | ValueNone ->
                ValueSome
                    {
                        Kind = ArgKind.Positional
                        Bytes = value
                    }
            | ValueSome name ->
                tryFieldOrPropType tryTypeName ty
                |> ValueOption.map (fun fieldOrPropType ->
                    {
                        Kind = ArgKind.Named
                        Bytes =
                            bytes (fun b ->
                                b.WriteByte 0x54uy
                                b.WriteBytes fieldOrPropType
                                b.WriteSerializedString name
                                b.WriteBytes value
                            )
                    }
                )
        )

    /// The blob: prolog `0x0001`, the positional arguments in written order, the
    /// named-argument count, then the named arguments in written order. `ValueNone` where
    /// any argument is unencodable.
    let tryEncode (tryTypeName: FrozenType -> string voption) (args: Block<TAttributeArg>) : BlobBuilder voption =
        Block.tryMap (tryEncodeArg tryTypeName) args
        |> ValueOption.map (fun encoded ->
            let named = encoded |> Block.filter (fun a -> a.Kind = ArgKind.Named)
            let b = BlobBuilder()
            b.WriteUInt16 1us

            for a in encoded do
                if a.Kind = ArgKind.Positional then
                    b.WriteBytes a.Bytes

            b.WriteUInt16(uint16 named.Length)

            for a in named do
                b.WriteBytes a.Bytes

            b
        )
