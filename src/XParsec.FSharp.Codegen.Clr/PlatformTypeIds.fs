namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

/// SRM IL-encoding of primitive platform type ids (`"System.Int32"` → `te.Int32()`).
/// The canon → type id mapping (`"int"` → `"System.Int32"`) lives in the `.fs`
/// `(# … #)` declarations, not here.
module PlatformTypeIds =

    /// What a value of an ECMA-335 primitive element type holds.
    [<RequireQualifiedAccess>]
    type private ElementKind =
        | Scalar
        | Reference

    /// A key is the type id the `.fs` declares verbatim, usually a BCL name, but the
    /// pointer-width pair is IL signature syntax (`type nativeint = (# "native int" #)`)
    /// because `native int` / `unsigned native int` ARE ECMA-335 element types.
    let private elementTypes: Map<string, ElementKind * (SignatureTypeEncoder -> unit)> =
        Map
            [
                "System.Int32", (ElementKind.Scalar, (fun te -> te.Int32()))
                "System.UInt32", (ElementKind.Scalar, (fun te -> te.UInt32()))
                "System.Int64", (ElementKind.Scalar, (fun te -> te.Int64()))
                "System.UInt64", (ElementKind.Scalar, (fun te -> te.UInt64()))
                "System.SByte", (ElementKind.Scalar, (fun te -> te.SByte()))
                "System.Byte", (ElementKind.Scalar, (fun te -> te.Byte()))
                "System.Int16", (ElementKind.Scalar, (fun te -> te.Int16()))
                "System.UInt16", (ElementKind.Scalar, (fun te -> te.UInt16()))
                "System.Double", (ElementKind.Scalar, (fun te -> te.Double()))
                "System.Single", (ElementKind.Scalar, (fun te -> te.Single()))
                "System.Boolean", (ElementKind.Scalar, (fun te -> te.Boolean()))
                "System.Char", (ElementKind.Scalar, (fun te -> te.Char()))
                "System.String", (ElementKind.Reference, (fun te -> te.String()))
                "native int", (ElementKind.Scalar, (fun te -> te.IntPtr()))
                "unsigned native int", (ElementKind.Scalar, (fun te -> te.UIntPtr()))
            ]

    /// Encode a primitive value type directly onto `te`. Returns `false` for type ids that
    /// need a `TypeRef` (`System.Decimal`, `System.ValueTuple`) and for unknown ids.
    let tryEncodeValueType (te: SignatureTypeEncoder) (typeId: PlatformTypeId) : bool =
        match Map.tryFind typeId.Value elementTypes with
        | Some(_, write) ->
            write te
            true
        | None -> false

    /// True iff `typeId` is a primitive value type the IL encoder writes DIRECTLY. The
    /// encoder-free form, callable without a `SignatureTypeEncoder`.
    let isEncodableValueType (typeId: PlatformTypeId) : bool =
        Map.containsKey typeId.Value elementTypes

    /// The scalar value types written through a `TypeRef` rather than an element type.
    /// `System.ValueTuple` is the zero-field struct `unit` binds to.
    let private typeRefScalars: Set<string> =
        Set [ "System.Decimal"; "System.ValueTuple" ]

    /// True iff `typeId` alone settles a value as unmanaged: a scalar element type, a
    /// `TypeRef` scalar, or a pointer in IL signature syntax. The pointer ids are `void*`
    /// (`voidptr`) and `!0*` (`ilsigptr<'T>`), from `prim-types-nativeint.clr.fs`.
    let isUnmanagedScalar (typeId: PlatformTypeId) : bool =
        match Map.tryFind typeId.Value elementTypes with
        | Some(ElementKind.Scalar, _) -> true
        | Some(ElementKind.Reference, _) -> false
        | None -> typeRefScalars.Contains typeId.Value || typeId.Value.EndsWith "*"

    /// True iff `typeId` is IL array syntax: `!0[]` (`'T[]`, `prim-types-array.fs`) or
    /// `!0[0 ..., 0 ...]` (the rank-n `[,]` family, `prim-types-nd-array.clr.fs`). A
    /// reference whatever the element.
    let isArray (typeId: PlatformTypeId) : bool = typeId.Value.EndsWith "]"
