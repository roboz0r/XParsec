namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

// The mutable method-body emission state `Il`. Codegen emits through one surface:
// the untyped depth-tracked `Cil.emit*` helpers, which the reified `IlIr` buffer
// (`IlIr.lower`) replays. `Il` tracks peak stack depth so finalisation hands
// `maxStack` to `AddMethodBody` with no separate pass.

/// Wraps the SRM `InstructionEncoder` (which holds the code + control-flow
/// builders by reference, so copying the struct is free and writes land in the
/// same buffers) and accumulates the peak stack depth.
type Il(encoder: InstructionEncoder) =
    let mutable depth = 0
    let mutable maxDepth = 0
    let locals = ResizeArray<SemType>()

    member _.Encoder = encoder

    member _.Depth = depth

    /// The `maxStack` for `AddMethodBody`.
    member _.MaxStack = maxDepth

    member _.Locals = locals

    member _.DeclareLocal(ty: SemType) : int =
        let slot = locals.Count
        locals.Add ty
        slot

    member _.Adjust(delta: int) =
        depth <- depth + delta

        if depth > maxDepth then
            maxDepth <- depth

    /// Restore the logical stack depth to a saved value. At a branch merge the
    /// arms each leave the same depth while the linear tracker has counted only
    /// one, so a caller resets to the arms' shared base before the next arm.
    /// Never lowers `maxDepth` (it has already seen the peak).
    member _.SetDepth(d: int) = depth <- d

/// The intrinsic-representation rekey: a Vesper primitive resolves to
/// `TyConst name`, and the backend keys the emitted IL type off the
/// *representation string* `name` maps to (`"int"` → `"System.Int32"` → `i4`)
/// rather than the Vesper name. The map flows from each file's
/// `type x = (# "..." #)` intrinsics (`TastFile.IntrinsicReprTypes`) overlaid on
/// the built-in `defaults`, so a platform author retargets a primitive by
/// editing one `.fs` line.
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
