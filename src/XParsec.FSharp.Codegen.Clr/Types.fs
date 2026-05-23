namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

// The fundamental types of the method-body DSL (LicenseToCIL's `Stack.fs` /
// `Types.fs`): the phantom stack-depth markers, the mutable emission state
// `Il`, and the `Op<'in,'out>` instruction alias. The opcode catalogue
// (`module Cil`) and the `cil { }` CE build on these in `Cil.fs` /
// `CilBuilder.fs`. See [codegen-clr-plan](../XParsec.FSharp.SemanticAnalysis/docs/codegen-clr-plan.md) §A.

/// Phantom stack-depth markers (LicenseToCIL `Stack.fs`). `E S` is the empty
/// stack, `E S S` one element, and so on. Always `null` at runtime — only the
/// nesting, checked by the type system, is load-bearing.
[<AllowNullLiteral>]
type E = class end

[<AllowNullLiteral>]
type S<'a> = class end

/// Mutable emission state threaded through every op. Wraps the SRM
/// `InstructionEncoder` (which itself holds the code + control-flow builders by
/// reference, so copying the struct is free and writes land in the same
/// buffers) and accumulates the peak stack depth.
type Il(encoder: InstructionEncoder) =
    let mutable depth = 0
    let mutable maxDepth = 0
    let locals = ResizeArray<SemType>()

    member _.Encoder = encoder

    /// Current logical stack depth.
    member _.Depth = depth

    /// Peak stack depth seen so far — the `maxStack` for `AddMethodBody`.
    member _.MaxStack = maxDepth

    /// The declared locals in slot order — drives the local-variable
    /// signature `buildBody` hands to `AddMethodBody`.
    member _.Locals = locals

    /// Declare a fresh local of `ty`, returning its slot index for
    /// `ldloc` / `stloc`.
    member _.DeclareLocal(ty: SemType) : int =
        let slot = locals.Count
        locals.Add ty
        slot

    /// Apply a net stack-depth delta (e.g. `+1` for a load, `-1` for `pop`).
    member _.Adjust(delta: int) =
        depth <- depth + delta

        if depth > maxDepth then
            maxDepth <- depth

/// `Op<'stackin,'stackout>` — an instruction (or sequence) that transforms the
/// phantom stack state `'stackin` into `'stackout` while emitting into `Il`.
type Op<'stackin, 'stackout> = S<'stackin> -> S<'stackout> -> Il -> unit

/// The intrinsic-representation rekey (docs/selfhost-handoff.md G7). A Vesper
/// primitive resolves to `TyConst name`; the backend keys the emitted IL type
/// off the *representation string* `name` maps to (`"int"` → `"System.Int32"` →
/// `i4`) rather than the Vesper name. The map flows from each file's
/// `type x = (# "..." #)` intrinsics (`TastFile.IntrinsicReprTypes`) overlaid on
/// the built-in `defaults`, so a platform author retargets a primitive by
/// editing one `.fs` line.
module IntrinsicRepr =

    /// Vesper primitive name → IL representation string, matching the
    /// `(# "..." #)` intrinsics declared in `src/Vesper.Core/prim-types-min.fs`.
    /// The front-end resolves these names to `TyConst name` directly (its
    /// built-in primitive set), so a normal program's `TastFile.IntrinsicReprTypes`
    /// is empty and these defaults supply the representation. `unit` is omitted:
    /// the current backend still maps it to `FSharp.Core.Unit` (a name arm in
    /// `encodeType`), not to its `prim-types-min` `System.ValueTuple` binding.
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

    /// Overlay a file's intrinsic bindings on the defaults — a file entry
    /// (`type int = (# "System.Int64" #)`) wins, so retargeting is one `.fs` edit.
    let merge (fileMap: Map<string, string>) : Map<string, string> =
        Map.fold (fun acc k v -> Map.add k v acc) defaults fileMap

    /// Emit a representation string that maps to a value type needing no external
    /// reference (every primitive except `System.Decimal`, whose `TypeRef` only
    /// the provider holds). Returns `false` when `repr` isn't one of these — the
    /// caller decides whether that's an error or has its own fallback.
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
