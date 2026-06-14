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
    let locals = ResizeArray<FrozenType>()

    member _.Encoder = encoder

    member _.Depth = depth

    /// The `maxStack` for `AddMethodBody`.
    member _.MaxStack = maxDepth

    member _.Locals = locals

    member _.DeclareLocal(ty: FrozenType) : int =
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
