namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

// Emits over SRM's `InstructionEncoder` (deterministic, PE-to-disk capable)
// rather than `System.Reflection.Emit.ILGenerator`.
//
// Two surfaces share one `Il`: typed `Op<'in,'out>` combinators whose stack
// shape is statically known (stack-balance errors are compile-time), and
// untyped depth-tracked `emit*` helpers for the TAST walker, which dispatches
// dynamically and so can't keep the phantom types across the provider boundary.
// `Il` tracks peak stack depth, so finalisation hands `maxStack` to
// `AddMethodBody` with no separate analysis pass.

[<RequireQualifiedAccess>]
module Cil =

    // ---- Combinators ----

    let zero<'x> : Op<'x, 'x> = fun _ _ _ -> ()

    /// The second op is delayed so the `cil` CE can thread it.
    let inline combine (a: Op<'i, 'm>) (b: unit -> Op<'m, 'o>) : Op<'i, 'o> =
        fun sin sout il ->
            a sin null il
            (b ()) null sout il

    /// Sequences a value-producing op into a continuation that consumes the
    /// value — `let!` in the `cil` CE. The `Il`-derived value (a slot index, a
    /// label) isn't known until emit time, so it can't be a plain `let`.
    let inline bind (m: OpV<'i, 'm, 'a>) (f: 'a -> Op<'m, 'o>) : Op<'i, 'o> =
        fun sin sout il ->
            let a = m sin null il
            (f a) null sout il

    // ---- Typed opcode surface (statically stack-checked) ----

    let ldarg (n: int) : Op<'x, 'x S> =
        fun _ _ il ->
            il.Encoder.LoadArgument(n)
            il.Adjust 1

    let ldcI4 (n: int) : Op<'x, 'x S> =
        fun _ _ il ->
            il.Encoder.LoadConstantI4(n)
            il.Adjust 1

    let ldstr (h: UserStringHandle) : Op<'x, 'x S> =
        fun _ _ il ->
            il.Encoder.LoadString(h)
            il.Adjust 1

    let add<'x> : Op<'x S S, 'x S> =
        fun _ _ il ->
            il.Encoder.OpCode(ILOpCode.Add)
            il.Adjust -1

    let sub<'x> : Op<'x S S, 'x S> =
        fun _ _ il ->
            il.Encoder.OpCode(ILOpCode.Sub)
            il.Adjust -1

    let mul<'x> : Op<'x S S, 'x S> =
        fun _ _ il ->
            il.Encoder.OpCode(ILOpCode.Mul)
            il.Adjust -1

    /// Declares a fresh local of `ty` and yields its slot index for `let!` to
    /// thread into the `stloc`/`ldloc` that use it. Emits nothing — stack-neutral.
    let declareLocal (ty: SemType) : OpV<'x, 'x, int> = fun _ _ il -> il.DeclareLocal ty

    let stloc (n: int) : Op<'x S, 'x> =
        fun _ _ il ->
            il.Encoder.StoreLocal(n)
            il.Adjust -1

    let ldloc (n: int) : Op<'x, 'x S> =
        fun _ _ il ->
            il.Encoder.LoadLocal(n)
            il.Adjust 1

    let newobj1 (ctor: EntityHandle) : Op<'x S, 'x S> =
        fun _ _ il ->
            il.Encoder.OpCode(ILOpCode.Newobj)
            il.Encoder.Token(ctor)
            il.Adjust 0

    let call1 (m: EntityHandle) : Op<'x S, 'x S> = fun _ _ il -> il.Encoder.Call(m)

    /// SRM has no `Callvirt` helper, so the opcode + token are emitted by hand.
    let callvirt1 (m: EntityHandle) : Op<'x S S, 'x S> =
        fun _ _ il ->
            il.Encoder.OpCode(ILOpCode.Callvirt)
            il.Encoder.Token(m)
            il.Adjust -1

    let pop<'x> : Op<'x S, 'x> =
        fun _ _ il ->
            il.Encoder.OpCode(ILOpCode.Pop)
            il.Adjust -1

    let ret<'y> : Op<E S, 'y> =
        fun _ _ il ->
            il.Encoder.OpCode(ILOpCode.Ret)
            il.Adjust -1

    let retVoid<'y> : Op<E, 'y> = fun _ _ il -> il.Encoder.OpCode(ILOpCode.Ret)

    // ---- Structured branching (typed) ----
    //
    // Labels stay private to these combinators rather than being exposed as
    // ops: a branch is only stack-safe when its arms merge to the same shape,
    // and a structured form makes that an invariant of the type instead of a
    // rule the caller has to uphold. Both arms run from the same in-stack and
    // are typed `Op<'i,'o>`, so an unbalanced branch simply won't compile.

    /// `cond` leaves a bool; when true run `whenTrue`, else fall through. With no
    /// else arm the skip path and the through path merge, so `whenTrue` must be
    /// stack-neutral (`Op<'i,'i>`) — the type enforces it. Emits
    /// `<cond>; brfalse end; <whenTrue>; end:`; no depth reset is needed because
    /// the body leaves the post-`brfalse` depth unchanged.
    let ifThen (cond: Op<'i, 'i S>) (whenTrue: Op<'i, 'i>) : Op<'i, 'i> =
        fun _ _ il ->
            cond null null il
            let endLabel = il.Encoder.DefineLabel()
            il.Encoder.Branch(ILOpCode.Brfalse, endLabel)
            il.Adjust -1
            whenTrue null null il
            il.Encoder.MarkLabel endLabel

    /// `cond` leaves a bool; when true run `whenTrue`, else `whenFalse`. Both
    /// arms must leave the same stack shape `'o`. Emits
    /// `<cond>; brfalse else; <whenTrue>; br end; else: <whenFalse>; end:`,
    /// resetting the linear depth tracker to the post-`brfalse` base before the
    /// else arm (both arms leave one extra slot but the tracker follows only one)
    /// — see `Il.SetDepth`.
    let ifThenElse (cond: Op<'i, 'i S>) (whenTrue: Op<'i, 'o>) (whenFalse: Op<'i, 'o>) : Op<'i, 'o> =
        fun _ _ il ->
            cond null null il
            let elseLabel = il.Encoder.DefineLabel()
            let endLabel = il.Encoder.DefineLabel()
            il.Encoder.Branch(ILOpCode.Brfalse, elseLabel)
            il.Adjust -1
            let baseDepth = il.Depth
            whenTrue null null il
            il.Encoder.Branch(ILOpCode.Br, endLabel)
            il.SetDepth baseDepth
            il.Encoder.MarkLabel elseLabel
            whenFalse null null il
            il.Encoder.MarkLabel endLabel

    /// `while <cond> do <body>`: emits `br test; body: <body>; test: <cond>;
    /// brtrue body`. `cond` leaves a bool and `body` is stack-neutral, so the
    /// loop leaves the stack unchanged. The `br`/`brtrue` chain is naturally
    /// depth-balanced, so no `SetDepth` reset is needed.
    let whileLoop (cond: Op<'i, 'i S>) (body: Op<'i, 'i>) : Op<'i, 'i> =
        fun _ _ il ->
            let testLabel = il.Encoder.DefineLabel()
            let bodyLabel = il.Encoder.DefineLabel()
            il.Encoder.Branch(ILOpCode.Br, testLabel)
            il.Encoder.MarkLabel bodyLabel
            body null null il
            il.Encoder.MarkLabel testLabel
            cond null null il
            il.Encoder.Branch(ILOpCode.Brtrue, bodyLabel)
            il.Adjust -1

    // ---- Untyped depth-tracked helpers (for the dynamic TAST walker) ----

    let emitLdstr (il: Il) (h: UserStringHandle) : unit =
        il.Encoder.LoadString(h)
        il.Adjust 1

    let emitLdcI4 (il: Il) (n: int) : unit =
        il.Encoder.LoadConstantI4(n)
        il.Adjust 1

    let emitLdcR8 (il: Il) (x: double) : unit =
        il.Encoder.LoadConstantR8(x)
        il.Adjust 1

    let emitNewobj (il: Il) (ctor: EntityHandle) (argc: int) : unit =
        il.Encoder.OpCode(ILOpCode.Newobj)
        il.Encoder.Token(ctor)
        il.Adjust(1 - argc)

    let emitCall (il: Il) (m: EntityHandle) (argc: int) (pushes: int) : unit =
        il.Encoder.Call(m)
        il.Adjust(pushes - argc)

    let emitStloc (il: Il) (n: int) : unit =
        il.Encoder.StoreLocal(n)
        il.Adjust -1

    let emitLdloc (il: Il) (n: int) : unit =
        il.Encoder.LoadLocal(n)
        il.Adjust 1

    /// Managed pointer to local slot `n` — the receiver for a value-type
    /// instance call (the `Vesper.Formatter` ref-struct handler) / its in-place `.ctor`.
    let emitLdloca (il: Il) (n: int) : unit =
        il.Encoder.LoadLocalAddress(n)
        il.Adjust 1

    /// The `unit` value: `()` is the null `Unit`.
    let emitLdnull (il: Il) : unit =
        il.Encoder.OpCode(ILOpCode.Ldnull)
        il.Adjust 1

    let emitLdarg (il: Il) (n: int) : unit =
        il.Encoder.LoadArgument(n)
        il.Adjust 1

    let emitLdfld (il: Il) (field: EntityHandle) : unit =
        il.Encoder.OpCode(ILOpCode.Ldfld)
        il.Encoder.Token(field)
        il.Adjust 0

    let emitStfld (il: Il) (field: EntityHandle) : unit =
        il.Encoder.OpCode(ILOpCode.Stfld)
        il.Encoder.Token(field)
        il.Adjust -2

    let emitPop (il: Il) : unit =
        il.Encoder.OpCode(ILOpCode.Pop)
        il.Adjust -1

    let emitDup (il: Il) : unit =
        il.Encoder.OpCode(ILOpCode.Dup)
        il.Adjust 1

    let emitRet (il: Il) : unit = il.Encoder.OpCode(ILOpCode.Ret)

    // ---- Branching ----

    /// `buildBody`'s `ControlFlowBuilder` resolves the offset once `markLabel` places the target.
    let defineLabel (il: Il) : LabelHandle = il.Encoder.DefineLabel()

    let markLabel (il: Il) (label: LabelHandle) : unit = il.Encoder.MarkLabel(label)

    let emitBr (il: Il) (label: LabelHandle) : unit = il.Encoder.Branch(ILOpCode.Br, label)

    let emitBrFalse (il: Il) (label: LabelHandle) : unit =
        il.Encoder.Branch(ILOpCode.Brfalse, label)
        il.Adjust -1

    let emitBrTrue (il: Il) (label: LabelHandle) : unit =
        il.Encoder.Branch(ILOpCode.Brtrue, label)
        il.Adjust -1

    let emitBneUn (il: Il) (label: LabelHandle) : unit =
        il.Encoder.Branch(ILOpCode.Bne_un, label)
        il.Adjust -2

    let emitBeq (il: Il) (label: LabelHandle) : unit =
        il.Encoder.Branch(ILOpCode.Beq, label)
        il.Adjust -2

    /// Terminates the path; the depth tracker still settles to the post-pop
    /// value for any merge that follows.
    let emitThrow (il: Il) : unit =
        il.Encoder.OpCode(ILOpCode.Throw)
        il.Adjust -1

    // ---- Finalisation ----

    /// `maxStack` comes from the tracked peak depth — no separate pass.
    /// `encodeLocals` is invoked only when locals exist, so callers with no
    /// provider (hand-written bodies) can pass any encoder.
    let buildBody
        (encodeLocals: SemType list -> StandaloneSignatureHandle)
        (bodyStream: MethodBodyStreamEncoder)
        (emit: Il -> unit)
        : int =
        let code = BlobBuilder()
        let flow = ControlFlowBuilder()
        let enc = InstructionEncoder(code, flow)
        let il = Il(enc)
        emit il

        let localSig =
            if il.Locals.Count = 0 then
                Unchecked.defaultof<StandaloneSignatureHandle>
            else
                encodeLocals (List.ofSeq il.Locals)

        bodyStream.AddMethodBody(enc, il.MaxStack, localSig)
