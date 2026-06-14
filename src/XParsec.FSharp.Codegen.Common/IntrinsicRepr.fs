namespace XParsec.FSharp.Codegen.Common

open System.Reflection.Metadata.Ecma335

/// The intrinsic-representation rekey: a Vesper primitive resolves to
/// `TyConst name`, and the backend keys the emitted IL type off the
/// *representation string* `name` maps to (`"int"` → `"System.Int32"` → `i4`)
/// rather than the Vesper name. The map flows from each file's
/// `type x = (# "..." #)` intrinsics (`TastFile.IntrinsicReprTypes`) overlaid on
/// the built-in `defaults`, so a platform author retargets a primitive by
/// editing one `.fs` line.
///
/// Lives in `Codegen.Common` because both the CLR backend (emission) and the
/// shared `MetadataSymbols` resolver (mapping a BCL nominal back to its Vesper
/// primitive — the reverse direction) read this one map.
module IntrinsicRepr =

    /// `unit` maps to the zero-field BCL struct `System.ValueTuple` — its
    /// `prim-types-min.fs` binding. Listed in `defaults` (not only that file's own
    /// `IntrinsicReprTypes`) so a package that merely *mentions* `unit` without
    /// redeclaring it — every consumer of `Vesper.Core` — still resolves it,
    /// exactly as `int`/`bool` do. `FSharp.Core.Unit` is gone from the general
    /// path (it lingers only on the cold-printf interop island).
    let defaults: Map<string, string> =
        Map
            [
                "int", "System.Int32"
                "int64", "System.Int64"
                "byte", "System.Byte"
                "float", "System.Double"
                "float32", "System.Single"
                "bool", "System.Boolean"
                "char", "System.Char"
                "decimal", "System.Decimal"
                "string", "System.String"
                "unit", "System.ValueTuple"
            ]

    /// A file entry wins over the defaults, so retargeting is one `.fs` edit.
    let merge (fileMap: Map<string, string>) : Map<string, string> =
        Map.fold (fun acc k v -> Map.add k v acc) defaults fileMap

    /// Value types needing no external reference (every primitive except
    /// `System.Decimal`, whose `TypeRef` only the provider holds). `false` when
    /// `repr` isn't one of these — the caller decides whether that's an error.
    let tryEncodeValueType (te: SignatureTypeEncoder) (repr: string) : bool =
        match repr with
        | "System.Int32" ->
            te.Int32()
            true
        | "System.Int64" ->
            te.Int64()
            true
        | "System.Byte" ->
            te.Byte()
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
