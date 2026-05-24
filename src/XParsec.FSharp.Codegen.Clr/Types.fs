namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

// The method-body DSL fundamentals: the phantom stack-depth markers, the
// mutable emission state `Il`, and the `Op<'in,'out>` instruction alias.

/// Phantom stack-depth markers. `E S` is the empty stack, `E S S` one element,
/// and so on. Always `null` at runtime — only the nesting, checked by the type
/// system, is load-bearing.
[<AllowNullLiteral>]
type E = class end

[<AllowNullLiteral>]
type S<'a> = class end

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

type Op<'stackin, 'stackout> = S<'stackin> -> S<'stackout> -> Il -> unit

/// An `Op` that also yields a value the emission produced — a declared local's
/// slot index — for a dependent continuation to consume via the `cil` CE's
/// `let!`. Such producers are stack-neutral (`'stackin = 'stackout`).
type OpV<'stackin, 'stackout, 'a> = S<'stackin> -> S<'stackout> -> Il -> 'a

/// The intrinsic-representation rekey: a Vesper primitive resolves to
/// `TyConst name`, and the backend keys the emitted IL type off the
/// *representation string* `name` maps to (`"int"` → `"System.Int32"` → `i4`)
/// rather than the Vesper name. The map flows from each file's
/// `type x = (# "..." #)` intrinsics (`TastFile.IntrinsicReprTypes`) overlaid on
/// the built-in `defaults`, so a platform author retargets a primitive by
/// editing one `.fs` line.
module IntrinsicRepr =

    /// `unit` is omitted: the backend still maps it to `FSharp.Core.Unit` (a name
    /// arm in `encodeType`), not to its `prim-types-min` `System.ValueTuple` binding.
    let defaults: Map<string, string> =
        Map
            [
                "int", "System.Int32"
                "int64", "System.Int64"
                "byte", "System.Byte"
                "float", "System.Double"
                "bool", "System.Boolean"
                "char", "System.Char"
                "decimal", "System.Decimal"
                "string", "System.String"
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
