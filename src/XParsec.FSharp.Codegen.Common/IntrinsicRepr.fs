namespace XParsec.FSharp.Codegen.Common

open System.Reflection.Metadata.Ecma335

/// SRM IL-encoding of Vesper primitive representations (`"System.Int32"` → `te.Int32()`).
/// This is pure System.Reflection.Metadata knowledge, NOT a Vesper repr table: the
/// canon → platform mapping (`"int"` → `"System.Int32"`) lives in the `.fs` `(# … #)`
/// declarations and is read through `ClrEnv.TryPrimitiveRepr` (own-unit intrinsics →
/// the provider's harvested forward `{canon → platform}` map). The metadata leaf
/// canonicalizes the reverse direction through the harvested `{platform → canon}` map.
module IntrinsicRepr =

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
