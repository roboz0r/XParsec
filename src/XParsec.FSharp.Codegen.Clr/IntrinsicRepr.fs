namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata.Ecma335

/// SRM IL-encoding of primitive repr strings (`"System.Int32"` → `te.Int32()`).
/// The canon → repr mapping (`"int"` → `"System.Int32"`) lives in the `.fs`
/// `(# … #)` declarations, not here.
module IntrinsicRepr =

    /// A key is the repr string the `.fs` declares verbatim, usually a BCL name, but the
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

    /// Encode a primitive value type directly onto `te`. Returns `false` for reprs that
    /// need a `TypeRef` (`System.Decimal`, `System.ValueTuple`) and for unknown reprs.
    let tryEncodeValueType (te: SignatureTypeEncoder) (repr: string) : bool =
        match Map.tryFind repr valueTypeWriters with
        | Some write ->
            write te
            true
        | None -> false

    /// True iff `repr` is a primitive value type the IL encoder writes DIRECTLY. The
    /// encoder-free form, for a caller that has no `SignatureTypeEncoder` to hand.
    let isEncodableValueType (repr: string) : bool = Map.containsKey repr valueTypeWriters
