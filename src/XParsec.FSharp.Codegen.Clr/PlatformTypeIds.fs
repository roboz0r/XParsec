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

    type private ElementType =
        {
            Kind: ElementKind
            ReflectionName: string
            Write: SignatureTypeEncoder -> unit
        }

    let private elementType (kind: ElementKind) (reflectionName: string) (write: SignatureTypeEncoder -> unit) =
        {
            Kind = kind
            ReflectionName = reflectionName
            Write = write
        }

    /// A key is the type id the `.fs` declares verbatim, usually a BCL name, but the
    /// pointer-width pair is IL signature syntax (`type nativeint = (# "native int" #)`)
    /// because `native int` / `unsigned native int` ARE ECMA-335 element types.
    let private elementTypes: Map<string, ElementType> =
        Map
            [
                "System.Int32", elementType ElementKind.Scalar "System.Int32" (fun te -> te.Int32())
                "System.UInt32", elementType ElementKind.Scalar "System.UInt32" (fun te -> te.UInt32())
                "System.Int64", elementType ElementKind.Scalar "System.Int64" (fun te -> te.Int64())
                "System.UInt64", elementType ElementKind.Scalar "System.UInt64" (fun te -> te.UInt64())
                "System.SByte", elementType ElementKind.Scalar "System.SByte" (fun te -> te.SByte())
                "System.Byte", elementType ElementKind.Scalar "System.Byte" (fun te -> te.Byte())
                "System.Int16", elementType ElementKind.Scalar "System.Int16" (fun te -> te.Int16())
                "System.UInt16", elementType ElementKind.Scalar "System.UInt16" (fun te -> te.UInt16())
                "System.Double", elementType ElementKind.Scalar "System.Double" (fun te -> te.Double())
                "System.Single", elementType ElementKind.Scalar "System.Single" (fun te -> te.Single())
                "System.Boolean", elementType ElementKind.Scalar "System.Boolean" (fun te -> te.Boolean())
                "System.Char", elementType ElementKind.Scalar "System.Char" (fun te -> te.Char())
                "System.String", elementType ElementKind.Reference "System.String" (fun te -> te.String())
                "native int", elementType ElementKind.Scalar "System.IntPtr" (fun te -> te.IntPtr())
                "unsigned native int", elementType ElementKind.Scalar "System.UIntPtr" (fun te -> te.UIntPtr())
            ]

    /// Encode a primitive value type directly onto `te`. Returns `false` for type ids that
    /// need a `TypeRef` (`System.Decimal`, `System.ValueTuple`) and for unknown ids.
    let tryEncodeValueType (te: SignatureTypeEncoder) (typeId: PlatformTypeId) : bool =
        match Map.tryFind typeId.Value elementTypes with
        | Some e ->
            e.Write te
            true
        | None -> false

    /// A primitive element type's reflection name (`native int` → `System.IntPtr`);
    /// `ValueNone` for a type id written through a `TypeRef`.
    let tryReflectionName (typeId: PlatformTypeId) : string voption =
        match Map.tryFind typeId.Value elementTypes with
        | Some e -> ValueSome e.ReflectionName
        | None -> ValueNone

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
        | Some { Kind = ElementKind.Scalar } -> true
        | Some { Kind = ElementKind.Reference } -> false
        | None -> typeRefScalars.Contains typeId.Value || typeId.Value.EndsWith "*"

    /// True iff `typeId` is IL array syntax: `!0[]` (`'T[]`, `prim-types-array.fs`) or
    /// `!0[0 ..., 0 ...]` (the rank-n `[,]` family, `prim-types-nd-array.clr.fs`). A
    /// reference whatever the element.
    let isArray (typeId: PlatformTypeId) : bool = typeId.Value.EndsWith "]"
