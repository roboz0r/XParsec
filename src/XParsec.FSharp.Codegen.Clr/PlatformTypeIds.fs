namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

/// SRM IL-encoding of primitive platform type ids (`"System.Int32"` → `te.Int32()`).
/// The canon → type id mapping (`"int"` → `"System.Int32"`) lives in the `.fs`
/// `(# … #)` declarations, not here.
module PlatformTypeIds =

    /// A key is the type id the `.fs` declares verbatim, usually a BCL name, but the
    /// pointer-width pair is IL signature syntax (`type nativeint = (# "native int" #)`)
    /// because `native int` / `unsigned native int` ARE ECMA-335 element types.
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

    /// Encode a primitive value type directly onto `te`. Returns `false` for type ids that
    /// need a `TypeRef` (`System.Decimal`, `System.ValueTuple`) and for unknown ids.
    let tryEncodeValueType (te: SignatureTypeEncoder) (typeId: PlatformTypeId) : bool =
        match Map.tryFind typeId.Value valueTypeWriters with
        | Some write ->
            write te
            true
        | None -> false

    /// True iff `typeId` is a primitive value type the IL encoder writes DIRECTLY. The
    /// encoder-free form, for a caller that has no `SignatureTypeEncoder` to hand.
    let isEncodableValueType (typeId: PlatformTypeId) : bool =
        Map.containsKey typeId.Value valueTypeWriters
