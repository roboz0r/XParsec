namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata.Ecma335

/// SRM IL-encoding of Vesper primitive representations (`"System.Int32"` → `te.Int32()`).
/// This is pure System.Reflection.Metadata knowledge, NOT a Vesper repr table: the
/// canon → platform mapping (`"int"` → `"System.Int32"`) lives in the `.fs` `(# … #)`
/// declarations and is read through `ClrEnv.TryPrimitiveRepr` (own-file intrinsics →
/// the provider's extracted forward `{canon → platform}` map). The metadata leaf
/// canonicalizes the reverse direction through the extracted `{platform → canon}` map.
module IntrinsicRepr =

    /// The directly-encodable primitive value-type reprs, paired with the
    /// `SignatureTypeEncoder` call that writes each. The SINGLE source of "which BCL
    /// value types the IL encoder knows by repr string" — both `tryEncodeValueType`
    /// (the writer) and `isEncodableValueType` (the pure conformance predicate) read
    /// it, so they can never disagree. A new scalar primitive becomes encodable by
    /// adding one entry here. NOTE: `System.Decimal` (needs a `TypeRef`) and
    /// `System.ValueTuple` (`unit`) are deliberately ABSENT — the encoder writes them
    /// via their own `TypeRef`-backed arms (`ClrEncoder`'s `eDecimal`/`eValueTuple`),
    /// not as direct value types.
    ///
    /// A key is the repr string the `.fs` declares, verbatim — usually a BCL name, but
    /// the pointer-width pair is spelled in IL signature syntax (`prim-types-nativeint.fs`:
    /// `type nativeint = (# "native int" #)`), because `native int` / `unsigned native int`
    /// ARE the ECMA-335 element types (`ELEMENT_TYPE_I` / `_U`) — not a nominal struct that
    /// happens to be pointer-sized. `IntPtr()` / `UIntPtr()` write exactly those tags.
    let private valueTypeWriters: Map<string, SignatureTypeEncoder -> unit> =
        Map
            [
                "System.Int32", (fun te -> te.Int32())
                "System.UInt32", (fun te -> te.UInt32())
                "System.Int64", (fun te -> te.Int64())
                "System.UInt64", (fun te -> te.UInt64())
                "System.SByte", (fun te -> te.SByte())
                "System.Byte", (fun te -> te.Byte())
                "System.Int16", (fun te -> te.Int16())
                "System.UInt16", (fun te -> te.UInt16())
                "System.Double", (fun te -> te.Double())
                "System.Single", (fun te -> te.Single())
                "System.Boolean", (fun te -> te.Boolean())
                "System.Char", (fun te -> te.Char())
                "System.String", (fun te -> te.String())
                "native int", (fun te -> te.IntPtr())
                "unsigned native int", (fun te -> te.UIntPtr())
            ]

    /// Encode a primitive value type directly onto `te`. Returns `false` for
    /// `System.Decimal` (needs a `TypeRef` the provider holds) and unknown reprs.
    let tryEncodeValueType (te: SignatureTypeEncoder) (repr: string) : bool =
        match Map.tryFind repr valueTypeWriters with
        | Some write ->
            write te
            true
        | None -> false

    /// True iff `repr` is a primitive value type the IL encoder writes DIRECTLY
    /// (`tryEncodeValueType` would succeed) — the pure, encoder-free predicate the
    /// repr-encodability conformance check reads (T8 Step 1). Excludes `System.Decimal`
    /// / `System.ValueTuple`, which the encoder handles via their own `TypeRef`-backed
    /// arms; a conformance check that admits those adds them explicitly.
    let isEncodableValueType (repr: string) : bool = Map.containsKey repr valueTypeWriters
