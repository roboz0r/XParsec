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
