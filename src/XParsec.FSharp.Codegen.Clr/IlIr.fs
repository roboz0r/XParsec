namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis

// A reified IL instruction buffer that both the dynamic TAST walker and the
// fixed per-type templates build, lowered to bytes by one pass. The textbook
// "construct an IR, then emit" split (what F#'s own `AbstractIL` does) rather
// than emitting straight to the encoder:
//
//   * one structure for both producers (the walker and the templates),
//   * a `verify` pass that stack-balance-checks the *dynamic* walker's output —
//     a capability the phantom-typed `Op` structurally cannot have, since the
//     walker's arm shapes are runtime-determined,
//   * centralised merge-point depth bookkeeping (the `il.SetDepth` resets the
//     hand-written emitters scattered through their bodies move into `analyze`).
//
// `lower` replays the buffer through the existing public `Cil.emit*` helpers, so
// the emitted bytes are identical to what a hand-written emitter produces.

/// The reified instruction. Branch targets are `LabelId`s (ints) minted by the
/// builder; locals are `LocalId`s = index into `ILBody.Locals`. Tokenful ops
/// carry their `EntityHandle`; stack ops split `Bin` (net −1) / `Un` (net 0).
[<RequireQualifiedAccess>]
type ILInstr =
    | Ldarg of int
    | Ldloc of int
    | Stloc of int
    | Ldloca of int
    | LdcI4 of int
    | LdcR8 of double
    | Ldstr of UserStringHandle
    | Ldnull
    | Dup
    | Pop
    | Ldfld of EntityHandle
    | Stfld of EntityHandle
    | Isinst of EntityHandle
    | Newobj of EntityHandle * argc: int
    | Call of EntityHandle * argc: int * pushes: int
    | Callvirt of EntityHandle * argc: int * pushes: int
    /// A binary stack op (net −1): `ceq`, `add`, `clt`, … (`bne.un` is `BneUn`).
    | Bin of ILOpCode
    /// A unary stack op (net 0): `neg`, `not`, `conv.*`.
    | Un of ILOpCode
    /// An opaque provider recipe (`App` / `New` / `UnionCons` / `Invoke`): the
    /// provider owns the emission, `lower` replays it via `recipe.Emit`. `analyze`
    /// treats it as a single op whose net delta is the recipe's carried
    /// `Pushes - ArgCount` — sound at the boundary without seeing inside it.
    | Recipe of CallRecipe
    | Mark of int
    | Br of int
    | Brfalse of int
    | Brtrue of int
    | BneUn of int
    | Beq of int
    /// `throw` — a terminator (like `Ret`); pops the exception object and never
    /// falls through.
    | Throw
    | Ret

// A method body is built by sequential appends, then scanned twice (`analyze`,
// then `lower`) and discarded — no random access, no mid-stream splicing. So the
// buffer is a contiguous growable array (`ResizeArray`, amortized-O(1) append +
// cache-friendly scan), NOT an `ILInstr list` (O(n)-append / pointer-chasing scan).
// Label ids are dense (0..LabelCount-1), so the per-label side tables in `analyze`
// / `lower` are plain `int[]` / `LabelHandle[]` rather than `Map` / `Dictionary`.
// TODO(perf, measure-first): `ILInstr` is a *reference* DU, so every appended
// instruction is a heap allocation and the scan pointer-chases. If this buffer
// proves hot, benchmark a `[<Struct>]` `ILInstr` in `ResizeArray<struct-ILInstr>`
// (contiguous, alloc-free) — but this project's DU→struct spikes have regressed
// before (see [[feedback_errortype_spike_failed]],
// [[feedback_struct_union_aux_not_worth_it]], [[feedback_struct_value_size_cost]]),
// so measure against a real workload, don't assume. See ilir-migration-plan.md.
type ILBody =
    {
        Locals: ResizeArray<SemType>
        Instrs: ResizeArray<ILInstr>
        /// Count of distinct labels minted (ids are dense `0..LabelCount-1`), so the
        /// scan/lower passes size their per-label arrays directly.
        LabelCount: int
    }

/// The straight-line stack delta of an instruction as the *builder* tracks it,
/// mirroring the `il.Adjust` each `Cil.emit*` performs (so a builder's running
/// `Depth` equals what the eager emitter's `Il.Depth` was at the same point).
/// Distinct from `IlIr.analyze`'s control-flow walk: here a conditional/compare
/// branch pops its operands (−1 / −2) on the fall-through path and `Throw` pops
/// the exception object, because the builder follows the linear path the walker
/// is currently emitting — merges are handled by the walker's explicit
/// `SetDepth`, and re-derived independently by `analyze` for lowering.
module private InstrDelta =
    let ofInstr (i: ILInstr) : int =
        match i with
        | ILInstr.Ldarg _
        | ILInstr.Ldloc _
        | ILInstr.Ldloca _
        | ILInstr.LdcI4 _
        | ILInstr.LdcR8 _
        | ILInstr.Ldstr _
        | ILInstr.Ldnull
        | ILInstr.Dup -> 1
        | ILInstr.Stloc _
        | ILInstr.Pop -> -1
        | ILInstr.Ldfld _
        | ILInstr.Isinst _
        | ILInstr.Un _ -> 0
        | ILInstr.Stfld _ -> -2
        | ILInstr.Newobj(_, argc) -> 1 - argc
        | ILInstr.Call(_, argc, pushes)
        | ILInstr.Callvirt(_, argc, pushes) -> pushes - argc
        | ILInstr.Recipe recipe -> recipe.Pushes - recipe.ArgCount
        | ILInstr.Bin _ -> -1
        | ILInstr.Brfalse _
        | ILInstr.Brtrue _ -> -1
        | ILInstr.BneUn _
        | ILInstr.Beq _ -> -2
        | ILInstr.Throw -> -1
        | ILInstr.Mark _
        | ILInstr.Br _
        | ILInstr.Ret -> 0

/// Mutable builder: mints label ids + local slots and accumulates instructions —
/// the equivalent of appending to the body during a structural walk. It also
/// tracks a running operand-stack `Depth` (and offers `SetDepth` for branch
/// merges), exactly as `Il` does for the eager emitter — the walker reads it to
/// know how many values a statement left to discard.
type IlBuilder() =
    let locals = ResizeArray<SemType>()
    let instrs = ResizeArray<ILInstr>()
    let mutable nextLabel = 0
    let mutable depth = 0

    member _.Local(ty: SemType) : int =
        let i = locals.Count
        locals.Add ty
        i

    member _.Label() : int =
        let l = nextLabel
        nextLabel <- nextLabel + 1
        l

    member _.Add(i: ILInstr) : unit =
        instrs.Add i
        depth <- depth + InstrDelta.ofInstr i

    /// The current operand-stack depth on the linear path being built.
    member _.Depth = depth

    /// Restore the logical depth to a saved value at a branch merge — the
    /// builder-time twin of `Il.SetDepth` (the buffer's `Mark` carries no depth;
    /// `analyze` re-derives the merge depth for lowering).
    member _.SetDepth(d: int) : unit = depth <- d

    member _.Body: ILBody =
        // Hand back the live buffers (no copy): a body is built once, scanned by
        // `analyze`/`lower`, then discarded — the builder is not reused after.
        {
            Locals = locals
            Instrs = instrs
            LabelCount = nextLabel
        }

module IlIr =

    /// The stack delta of a straight-line op (control flow is handled by the walk).
    let private straightDelta (i: ILInstr) : int =
        match i with
        | ILInstr.Ldarg _
        | ILInstr.Ldloc _
        | ILInstr.Ldloca _
        | ILInstr.LdcI4 _
        | ILInstr.LdcR8 _
        | ILInstr.Ldstr _
        | ILInstr.Ldnull
        | ILInstr.Dup -> 1
        | ILInstr.Stloc _
        | ILInstr.Pop -> -1
        | ILInstr.Ldfld _
        | ILInstr.Isinst _
        | ILInstr.Un _ -> 0
        | ILInstr.Stfld _ -> -2
        | ILInstr.Newobj(_, argc) -> 1 - argc
        | ILInstr.Call(_, argc, pushes)
        | ILInstr.Callvirt(_, argc, pushes) -> pushes - argc
        | ILInstr.Recipe recipe -> recipe.Pushes - recipe.ArgCount
        | ILInstr.Bin _ -> -1
        | ILInstr.Mark _
        | ILInstr.Br _
        | ILInstr.Brfalse _
        | ILInstr.Brtrue _
        | ILInstr.BneUn _
        | ILInstr.Beq _
        | ILInstr.Throw
        | ILInstr.Ret -> 0

    type Analysis =
        {
            MaxStack: int
            /// Operand-stack depth at each label, indexed by `LabelId` (dense). `-1`
            /// = a label whose depth is not yet known (a real depth is always ≥ 0).
            LabelDepths: int[]
        }

    /// Abstract-stack walk over the buffer: the `maxStack` + the operand-stack
    /// depth at every label, or the first imbalance. Depth `ValueNone` = unreachable
    /// (right after a terminator, until the next `Mark` resolves the depth from a
    /// branch that targets it). A label reached at two different depths — the bug
    /// the hand-written `il.SetDepth` only *assumes* away — is a hard error. This is
    /// the capability the phantom-typed `Op` can't provide, because it runs over the
    /// already-built buffer regardless of how dynamically it was produced.
    let analyze (body: ILBody) : Result<Analysis, string> =
        let labelDepths = Array.create body.LabelCount -1
        let mutable maxStack = 0
        let mutable cur = ValueSome 0
        let mutable err = None

        let note l d =
            if labelDepths.[l] = -1 then
                labelDepths.[l] <- d
            elif labelDepths.[l] <> d && err.IsNone then
                err <- Some(sprintf "label %d reached at depth %d and %d" l labelDepths.[l] d)

        for i in body.Instrs do
            if err.IsNone then
                match i with
                | ILInstr.Mark l ->
                    match cur with
                    | ValueSome d ->
                        note l d
                        cur <- ValueSome d
                    | ValueNone ->
                        if labelDepths.[l] = -1 then
                            // A *dead* merge point: marked while the path is
                            // unreachable (after a terminator) and never targeted
                            // by any branch — e.g. the never-taken `nextLabel` of
                            // an irrefutable final `match` arm and the unreachable
                            // `match`-failure tail after it. The eager emitter reset
                            // the depth here with the walker's manual `SetDepth`;
                            // with that gone the entry depth is unconstrained (CLI
                            // §III: unreachable code's stack state is free), so take
                            // the base (0) — exactly the eager value for a
                            // return/statement-position match/if, and valid
                            // otherwise (dead code only ever inflates `maxStack`).
                            labelDepths.[l] <- 0
                            cur <- ValueSome 0
                        else
                            cur <- ValueSome labelDepths.[l]
                | ILInstr.Br l ->
                    (match cur with
                     | ValueSome d -> note l d
                     | ValueNone -> ())

                    cur <- ValueNone
                | ILInstr.Throw
                | ILInstr.Ret -> cur <- ValueNone
                | ILInstr.Brfalse l
                | ILInstr.Brtrue l ->
                    match cur with
                    | ValueSome d when d >= 1 ->
                        note l (d - 1)
                        cur <- ValueSome(d - 1)
                    | ValueSome _ -> err <- Some "stack underflow at conditional branch"
                    | ValueNone -> err <- Some "unreachable conditional branch"
                | ILInstr.BneUn l
                | ILInstr.Beq l ->
                    match cur with
                    | ValueSome d when d >= 2 ->
                        note l (d - 2)
                        cur <- ValueSome(d - 2)
                    | ValueSome _ -> err <- Some "stack underflow at compare-branch"
                    | ValueNone -> err <- Some "unreachable compare-branch"
                | _ ->
                    match cur with
                    | ValueSome d ->
                        let d' = d + straightDelta i

                        if d' < 0 then
                            err <- Some(sprintf "stack underflow at %A" i)
                        else
                            if d' > maxStack then
                                maxStack <- d'

                            cur <- ValueSome d'
                    | ValueNone -> err <- Some(sprintf "unreachable instruction %A" i)

        match err with
        | Some e -> Result.Error e
        | None ->
            Result.Ok
                {
                    MaxStack = maxStack
                    LabelDepths = labelDepths
                }

    /// `analyze` keeping only `maxStack` (or the imbalance) — the verifier.
    let verify (body: ILBody) : Result<int, string> =
        analyze body |> Result.map (fun a -> a.MaxStack)

    /// Replay the buffer into the existing depth-tracked `Il` via the public
    /// `Cil.emit*` helpers — so the bytes are identical to a hand-written emitter
    /// while the depth/label bookkeeping is centralised: each `Mark` resets the
    /// linear tracker to the depth `analyze` computed (replacing the scattered
    /// manual `il.SetDepth`). Locals declare in order, so `LocalId` == slot index.
    let lower (body: ILBody) (il: Il) : unit =
        // `analyze` is both load-bearing (its `LabelDepths` drive the per-`Mark`
        // depth reset below) and the **standing balance guard**: every production
        // method body is built into an `ILBody` and lowered here, so this single
        // hand-off verifies stack balance for all of codegen and fails the build on
        // any imbalance an underflow / depth-conflict represents — the safety net
        // the phantom-typed `Op` could never give the dynamic walker. (Hand-written
        // typed-`Op` bodies keep their own compile-time stack check instead.)
        let analysis =
            match analyze body with
            | Result.Ok a -> a
            | Result.Error e -> failwithf "IlIr.lower: unbalanced body: %s" e

        for ty in body.Locals do
            il.DeclareLocal ty |> ignore

        // Dense label ids → reserve every handle up front (positions are fixed at
        // `markLabel`; every minted label is marked by construction).
        let labels = Array.init body.LabelCount (fun _ -> Cil.defineLabel il)

        for i in body.Instrs do
            match i with
            | ILInstr.Ldarg n -> Cil.emitLdarg il n
            | ILInstr.Ldloc n -> Cil.emitLdloc il n
            | ILInstr.Stloc n -> Cil.emitStloc il n
            | ILInstr.Ldloca n -> Cil.emitLdloca il n
            | ILInstr.LdcI4 n -> Cil.emitLdcI4 il n
            | ILInstr.LdcR8 x -> Cil.emitLdcR8 il x
            | ILInstr.Ldstr h -> Cil.emitLdstr il h
            | ILInstr.Ldnull -> Cil.emitLdnull il
            | ILInstr.Dup -> Cil.emitDup il
            | ILInstr.Pop -> Cil.emitPop il
            | ILInstr.Ldfld f -> Cil.emitLdfld il f
            | ILInstr.Stfld f -> Cil.emitStfld il f
            | ILInstr.Isinst t -> Cil.emitIsinst il t
            | ILInstr.Newobj(c, argc) -> Cil.emitNewobj il c argc
            | ILInstr.Call(m, argc, pushes) -> Cil.emitCall il m argc pushes
            | ILInstr.Callvirt(m, argc, pushes) -> Cil.emitCallvirt il m argc pushes
            | ILInstr.Recipe recipe ->
                recipe.Emit il
                il.Adjust(recipe.Pushes - recipe.ArgCount)
            | ILInstr.Bin code -> Cil.emitIntrinsicValueOp il code 2
            | ILInstr.Un code -> Cil.emitIntrinsicValueOp il code 1
            | ILInstr.Mark l ->
                il.SetDepth analysis.LabelDepths.[l]
                Cil.markLabel il labels.[l]
            | ILInstr.Br l -> Cil.emitBr il labels.[l]
            | ILInstr.Brfalse l -> Cil.emitBrFalse il labels.[l]
            | ILInstr.Brtrue l -> Cil.emitBrTrue il labels.[l]
            | ILInstr.BneUn l -> Cil.emitBneUn il labels.[l]
            | ILInstr.Beq l -> Cil.emitBeq il labels.[l]
            | ILInstr.Throw -> Cil.emitThrow il
            | ILInstr.Ret -> Cil.emitRet il
