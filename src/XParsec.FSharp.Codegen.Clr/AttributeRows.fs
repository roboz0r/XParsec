namespace XParsec.FSharp.Codegen.Clr

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

    /// One scalar's `FieldOrPropType` byte and its `Elem` write. `ValueNone` ⇒ pointer-width
    /// integrals, `decimal` (fsc lowers it to `DecimalConstantAttribute`) and `unit` have no
    /// `Elem` encoding.
    let private tryScalarElem (v: TConstValue) : struct (byte * (BlobBuilder -> unit)) voption =
        match v with
        | TConstValue.Bool x -> ValueSome(struct (0x02uy, (fun b -> b.WriteBoolean x)))
        | TConstValue.Char c -> ValueSome(struct (0x03uy, (fun b -> b.WriteUInt16(uint16 c))))
        | TConstValue.Integral v ->
            match v with
            | IntValue.SByte n -> ValueSome(struct (0x04uy, (fun b -> b.WriteSByte n)))
            | IntValue.Byte n -> ValueSome(struct (0x05uy, (fun b -> b.WriteByte n)))
            | IntValue.Int16 n -> ValueSome(struct (0x06uy, (fun b -> b.WriteInt16 n)))
            | IntValue.UInt16 n -> ValueSome(struct (0x07uy, (fun b -> b.WriteUInt16 n)))
            | IntValue.Int32 n -> ValueSome(struct (0x08uy, (fun b -> b.WriteInt32 n)))
            | IntValue.UInt32 n -> ValueSome(struct (0x09uy, (fun b -> b.WriteUInt32 n)))
            | IntValue.Int64 n -> ValueSome(struct (0x0Auy, (fun b -> b.WriteInt64 n)))
            | IntValue.UInt64 n -> ValueSome(struct (0x0Buy, (fun b -> b.WriteUInt64 n)))
            | IntValue.NativeInt _
            | IntValue.UNativeInt _ -> ValueNone
        | TConstValue.Float32 f -> ValueSome(struct (0x0Cuy, (fun b -> b.WriteSingle f)))
        | TConstValue.Float f -> ValueSome(struct (0x0Duy, (fun b -> b.WriteDouble f)))
        | TConstValue.String s -> ValueSome(struct (0x0Euy, (fun b -> b.WriteSerializedString s)))
        | TConstValue.Decimal _
        | TConstValue.Unit -> ValueNone

    /// One result's `FieldOrPropType` byte and its `Elem` write. A `Type` (`0x50`) carries
    /// the SerString of the type's name. `ValueNone` for `null`, an array-valued argument,
    /// and a scalar or type with no `Elem` encoding.
    let private tryElem
        (tryTypeName: FrozenType -> string voption)
        (r: TConstResult)
        : struct (byte * (BlobBuilder -> unit)) voption =
        match r with
        | TConstResult.Scalar v -> tryScalarElem v
        | TConstResult.TypeVal t ->
            tryTypeName t
            |> ValueOption.map (fun name -> struct (0x50uy, (fun (b: BlobBuilder) -> b.WriteSerializedString name)))
        | TConstResult.Null
        | TConstResult.ArrayVal _ -> ValueNone

    /// One argument judged encodable before anything is written: a row is all-or-nothing.
    [<NoEquality; NoComparison>]
    type private EncodableArg =
        {
            Name: string voption
            /// `ValueSome` ⇒ the named-argument `FieldOrPropType` is `0x55` + this SerString
            /// (II.23.3's enum form) instead of `TypeByte`.
            EnumFullName: string voption
            TypeByte: byte
            WriteValue: BlobBuilder -> unit
        }

    /// A POSITIONAL enum-typed argument needs no enum name: the fixed-argument encoding
    /// follows the ctor's parameter type. `ValueNone` where the value has no `Elem`
    /// encoding, or a named argument is typed by a string-valued (TS) enum.
    let private tryClassify (tryTypeName: FrozenType -> string voption) (a: TAttributeArg) : EncodableArg voption =
        match tryElem tryTypeName (TConstExpr.result a.Expr) with
        | ValueNone -> ValueNone
        | ValueSome(struct (tyByte, write)) ->
            let arg =
                {
                    Name = a.Name
                    EnumFullName = ValueNone
                    TypeByte = tyByte
                    WriteValue = write
                }

            match a.Name, a.EnumKey with
            | _, ValueNone
            | ValueNone, ValueSome _ -> ValueSome arg
            | ValueSome _, ValueSome key ->
                match a.Value with
                // An enum value serialises at its underlying integral width; a string-valued
                // enum (a TS enum) has no CLR encoding.
                | ValueSome(TConstValue.Integral _) ->
                    tryTypeName (FTEnum key)
                    |> ValueOption.map (fun n -> { arg with EnumFullName = ValueSome n })
                | _ -> ValueNone

    /// The blob: prolog `0x0001`, the positional arguments in written order, the
    /// named-argument count, then each named PROPERTY argument (`0x54`, `FieldOrPropType`,
    /// SerString name, value). `ValueNone` where any argument is unencodable.
    let tryEncode (tryTypeName: FrozenType -> string voption) (args: Block<TAttributeArg>) : BlobBuilder voption =
        // Classify every argument before writing: a partially-written blob is never returned.
        let classified = [ for a in args -> tryClassify tryTypeName a ]

        if List.exists ValueOption.isNone classified then
            ValueNone
        else
            let classified = List.map ValueOption.get classified
            let b = BlobBuilder()
            b.WriteUInt16 1us

            let named =
                classified
                |> List.choose (fun a ->
                    match a.Name with
                    | ValueSome n -> Some(n, a)
                    | ValueNone -> None
                )

            for a in classified do
                if a.Name.IsNone then
                    a.WriteValue b

            b.WriteUInt16(uint16 (List.length named))

            for (name, a) in named do
                b.WriteByte 0x54uy

                match a.EnumFullName with
                | ValueSome enumName ->
                    b.WriteByte 0x55uy
                    b.WriteSerializedString enumName
                | ValueNone -> b.WriteByte a.TypeByte

                b.WriteSerializedString name
                a.WriteValue b

            ValueSome b
