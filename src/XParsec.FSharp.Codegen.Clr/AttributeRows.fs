namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
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

/// Why an argument list has no II.23.3 blob; the caller emits no row.
[<RequireQualifiedAccess>]
type AttributeBlobRejection =
    /// A value outside the encodable constant domain: a pointer-width integral, `decimal`
    /// (fsc lowers it to `DecimalConstantAttribute`), `unit`, or a string-valued (TS) enum.
    | UnencodableValue
    /// A named argument typed by an enum with no full name usable in this assembly's blob:
    /// II.23.3's enum SerString would need an assembly-qualified name for a
    /// referenced-assembly enum.
    | ForeignEnum of enumKey: TypeKey

/// ECMA-335 II.23.3 `CustomAttrib` blob encoding over the frozen constant-folded arguments.
module internal AttributeBlob =

    /// One value's `FieldOrPropType` byte and its `Elem` write. `ValueNone` ⇒ pointer-width
    /// integrals, `decimal` (fsc lowers it to `DecimalConstantAttribute`) and `unit` have no
    /// `Elem` encoding.
    let private tryElem (v: TConstValue) : struct (byte * (BlobBuilder -> unit)) voption =
        match v with
        | TConstValue.Bool x -> ValueSome(struct (0x02uy, (fun b -> b.WriteBoolean x)))
        | TConstValue.Char c -> ValueSome(struct (0x03uy, (fun b -> b.WriteUInt16(uint16 c))))
        | TConstValue.Integral(kind, bits) ->
            match kind with
            | IntKind.SByte -> ValueSome(struct (0x04uy, (fun b -> b.WriteByte(byte bits))))
            | IntKind.Byte -> ValueSome(struct (0x05uy, (fun b -> b.WriteByte(byte bits))))
            | IntKind.Int16 -> ValueSome(struct (0x06uy, (fun b -> b.WriteUInt16(uint16 bits))))
            | IntKind.UInt16 -> ValueSome(struct (0x07uy, (fun b -> b.WriteUInt16(uint16 bits))))
            | IntKind.Int32 -> ValueSome(struct (0x08uy, (fun b -> b.WriteUInt32(uint32 bits))))
            | IntKind.UInt32 -> ValueSome(struct (0x09uy, (fun b -> b.WriteUInt32(uint32 bits))))
            | IntKind.Int64 -> ValueSome(struct (0x0Auy, (fun b -> b.WriteUInt64(uint64 bits))))
            | IntKind.UInt64 -> ValueSome(struct (0x0Buy, (fun b -> b.WriteUInt64(uint64 bits))))
            | IntKind.NativeInt
            | IntKind.UNativeInt -> ValueNone
        | TConstValue.Float32 f -> ValueSome(struct (0x0Cuy, (fun b -> b.WriteSingle f)))
        | TConstValue.Float f -> ValueSome(struct (0x0Duy, (fun b -> b.WriteDouble f)))
        | TConstValue.String s -> ValueSome(struct (0x0Euy, (fun b -> b.WriteSerializedString s)))
        | TConstValue.Decimal _
        | TConstValue.Unit -> ValueNone

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
    /// follows the ctor's parameter type, so the value's bytes stand alone. Only a named
    /// argument writes the `0x55` enum form, so only there is `tryEnumFullName` consulted.
    let private tryClassify
        (tryEnumFullName: TypeKey -> string voption)
        (a: TAttributeArg)
        : Result<EncodableArg, AttributeBlobRejection> =
        match tryElem a.Value with
        | ValueNone -> Error AttributeBlobRejection.UnencodableValue
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
            | ValueNone, ValueSome _ -> Ok arg
            | ValueSome _, ValueSome key ->
                match a.Value with
                // An enum value serialises at its underlying integral width; a string-valued
                // enum (a TS enum) has no CLR encoding.
                | TConstValue.Integral _ ->
                    match tryEnumFullName key with
                    | ValueSome n -> Ok { arg with EnumFullName = ValueSome n }
                    | ValueNone -> Error(AttributeBlobRejection.ForeignEnum key)
                | _ -> Error AttributeBlobRejection.UnencodableValue

    /// The blob: prolog `0x0001`, the positional arguments in written order, the
    /// named-argument count, then each named PROPERTY argument (`0x54`, `FieldOrPropType`,
    /// SerString name, value). `tryEnumFullName` supplies the II.23.3 enum-type SerString
    /// for a named enum-typed argument; `ValueNone` there rejects as `ForeignEnum`.
    let tryEncode
        (tryEnumFullName: TypeKey -> string voption)
        (args: EqArray<TAttributeArg>)
        : Result<BlobBuilder, AttributeBlobRejection> =
        // Classify every argument before writing: a partially-written blob is never returned.
        let rec classifyAll acc rest =
            match rest with
            | [] -> Ok(List.rev acc)
            | a :: rest ->
                match tryClassify tryEnumFullName a with
                | Ok e -> classifyAll (e :: acc) rest
                | Error r -> Error r

        match classifyAll [] (EqArray.toList args) with
        | Error r -> Error r
        | Ok classified ->
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

            Ok b
