namespace XParsec.FSharp.Codegen.Common

open System.Reflection.Metadata.Ecma335

/// IL representation strings for Vesper primitives (`"int"` → `"System.Int32"`).
/// The live source of a primitive's repr is the `(# "..." #)` of its `.fs`, read
/// through `ClrEnv.TryPrimitiveRepr` (own-unit intrinsics → the provider's harvested
/// forward `{canon → platform}` map). `defaults` is NO LONGER on the codegen path;
/// its SOLE remaining consumer is `MetadataSymbols.reprToName` (the BCL→canon reverse
/// map), which T8 step 1.5 retires in favour of `IntrinsicReverseCanon` — at which
/// point `defaults` is deleted. Do not add a new consumer.
module IntrinsicRepr =

    /// Legacy repr map — see the module note. Only `reprToName` reads it now.
    let defaults: Map<string, string> =
        Map
            [
                "int", "System.Int32"
                // Canonical `uint32`; the front end expands the `uint` abbreviation.
                // `reprToName` (this map inverted) needs one name per repr.
                "uint32", "System.UInt32"
                "int64", "System.Int64"
                "uint64", "System.UInt64"
                "sbyte", "System.SByte"
                "byte", "System.Byte"
                "int16", "System.Int16"
                "uint16", "System.UInt16"
                "float", "System.Double"
                "float32", "System.Single"
                "bool", "System.Boolean"
                "char", "System.Char"
                "decimal", "System.Decimal"
                "string", "System.String"
                "unit", "System.ValueTuple"
            ]

    /// Encode a primitive value type directly onto `te`. Returns `false` for
    /// `System.Decimal` (needs a `TypeRef` the provider holds) and unknown reprs.
    let tryEncodeValueType (te: SignatureTypeEncoder) (repr: string) : bool =
        match repr with
        | "System.Int32" ->
            te.Int32()
            true
        | "System.UInt32" ->
            te.UInt32()
            true
        | "System.Int64" ->
            te.Int64()
            true
        | "System.UInt64" ->
            te.UInt64()
            true
        | "System.SByte" ->
            te.SByte()
            true
        | "System.Byte" ->
            te.Byte()
            true
        | "System.Int16" ->
            te.Int16()
            true
        | "System.UInt16" ->
            te.UInt16()
            true
        | "System.Double" ->
            te.Double()
            true
        | "System.Single" ->
            te.Single()
            true
        | "System.Boolean" ->
            te.Boolean()
            true
        | "System.Char" ->
            te.Char()
            true
        | "System.String" ->
            te.String()
            true
        | _ -> false
