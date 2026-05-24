namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

// The opcode catalogue + body finalisation (LicenseToCIL's `Ops.fs`), over the
// `E` / `S` / `Il` / `Op` types from `Types.fs`. Reimplements the emitter over
// `System.Reflection.Metadata`'s `InstructionEncoder` rather than
// `System.Reflection.Emit.ILGenerator` (deterministic, PE-to-disk capable).
// See [codegen-clr-plan](../XParsec.FSharp.SemanticAnalysis/docs/codegen-clr-plan.md) §A.
//
// Two surfaces share one emitter:
//   * Typed `Op<'in,'out>` combinators (`ldarg`, `add`, `ret`, …) for
//     hand-written bodies whose stack shape is statically known — the unit
//     tests exercise these, and stack-balance errors are compile-time.
//   * Untyped depth-tracked helpers (`emit*`) for the TAST walker, which
//     dispatches dynamically (compiled-name → recipe) and so can't keep the
//     phantom types across the provider boundary.
//
// Both run against the same `Il`, which tracks current/peak stack depth so the
// finaliser hands `maxStack` to `AddMethodBody` with no separate analysis pass.

[<RequireQualifiedAccess>]
module Cil =

    // ---- Combinators (LicenseToCIL `Ops`/`CILBuilder` shape) ----

    /// Emit nothing; preserve stack state.
    let zero<'x> : Op<'x, 'x> = fun _ _ _ -> ()

    /// Sequence two ops. The second is delayed so the `cil` CE can thread it.
    let inline combine (a: Op<'i, 'm>) (b: unit -> Op<'m, 'o>) : Op<'i, 'o> =
        fun sin sout il ->
            a sin null il
            (b ()) null sout il

    // ---- Typed opcode surface (statically stack-checked) ----

    /// `[_ -> _, arg]` Load argument `n`.
    let ldarg (n: int) : Op<'x, 'x S> =
        fun _ _ il ->
            il.Encoder.LoadArgument(n)
            il.Adjust 1

    /// `[_ -> _, value]` Load an int32 constant.
    let ldcI4 (n: int) : Op<'x, 'x S> =
        fun _ _ il ->
            il.Encoder.LoadConstantI4(n)
            il.Adjust 1

    /// `[_ -> _, str]` Load a string literal.
    let ldstr (h: UserStringHandle) : Op<'x, 'x S> =
        fun _ _ il ->
            il.Encoder.LoadString(h)
            il.Adjust 1

    /// `[_, v1, v2 -> _, result]` Add the top two values.
    let add<'x> : Op<'x S S, 'x S> =
        fun _ _ il ->
            il.Encoder.OpCode(ILOpCode.Add)
            il.Adjust -1

    /// `[_, v1, v2 -> _, result]` Subtract the top value from the one beneath.
    let sub<'x> : Op<'x S S, 'x S> =
        fun _ _ il ->
            il.Encoder.OpCode(ILOpCode.Sub)
            il.Adjust -1

    /// `[_, v1, v2 -> _, result]` Multiply the top two values.
    let mul<'x> : Op<'x S S, 'x S> =
        fun _ _ il ->
            il.Encoder.OpCode(ILOpCode.Mul)
            il.Adjust -1

    /// `[_, value -> _]` Store the top value into local slot `n`.
    let stloc (n: int) : Op<'x S, 'x> =
        fun _ _ il ->
            il.Encoder.StoreLocal(n)
            il.Adjust -1

    /// `[_ -> _, value]` Load local slot `n`.
    let ldloc (n: int) : Op<'x, 'x S> =
        fun _ _ il ->
            il.Encoder.LoadLocal(n)
            il.Adjust 1

    /// `[_, arg -> _, object]` Construct via a 1-argument constructor.
    let newobj1 (ctor: EntityHandle) : Op<'x S, 'x S> =
        fun _ _ il ->
            il.Encoder.OpCode(ILOpCode.Newobj)
            il.Encoder.Token(ctor)
            il.Adjust 0

    /// `[_, arg -> _, result]` Call a 1-argument method returning a value.
    let call1 (m: EntityHandle) : Op<'x S, 'x S> = fun _ _ il -> il.Encoder.Call(m)

    /// `[_, receiver, arg -> _, result]` Virtual call of a 1-argument
    /// instance method returning a value (`FSharpFunc.Invoke`). SRM has no
    /// `Callvirt` helper, so the opcode + token are emitted by hand.
    let callvirt1 (m: EntityHandle) : Op<'x S S, 'x S> =
        fun _ _ il ->
            il.Encoder.OpCode(ILOpCode.Callvirt)
            il.Encoder.Token(m)
            il.Adjust -1

    /// `[_, value -> _]` Discard the top value.
    let pop<'x> : Op<'x S, 'x> =
        fun _ _ il ->
            il.Encoder.OpCode(ILOpCode.Pop)
            il.Adjust -1

    /// `[retVal -> ]` Return a value.
    let ret<'y> : Op<E S, 'y> =
        fun _ _ il ->
            il.Encoder.OpCode(ILOpCode.Ret)
            il.Adjust -1

    /// `[ -> ]` Return without a value.
    let retVoid<'y> : Op<E, 'y> = fun _ _ il -> il.Encoder.OpCode(ILOpCode.Ret)

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

    /// `newobj` of a constructor taking `argc` arguments (already pushed):
    /// pops `argc`, pushes the constructed object.
    let emitNewobj (il: Il) (ctor: EntityHandle) (argc: int) : unit =
        il.Encoder.OpCode(ILOpCode.Newobj)
        il.Encoder.Token(ctor)
        il.Adjust(1 - argc)

    /// `call` of a method taking `argc` arguments (already pushed). `pushes`
    /// is the number of result values left on the stack (0 or 1).
    let emitCall (il: Il) (m: EntityHandle) (argc: int) (pushes: int) : unit =
        il.Encoder.Call(m)
        il.Adjust(pushes - argc)

    let emitStloc (il: Il) (n: int) : unit =
        il.Encoder.StoreLocal(n)
        il.Adjust -1

    let emitLdloc (il: Il) (n: int) : unit =
        il.Encoder.LoadLocal(n)
        il.Adjust 1

    /// `ldloca` — push a managed pointer to local slot `n`. The receiver for a
    /// value-type instance call (the `Vesper.Formatter` ref-struct handler) and
    /// for invoking its `.ctor` in place.
    let emitLdloca (il: Il) (n: int) : unit =
        il.Encoder.LoadLocalAddress(n)
        il.Adjust 1

    /// `ldnull` — push a null reference. Used as the `unit` value (`()` is the
    /// null `Unit`) a unit-typed expression yields.
    let emitLdnull (il: Il) : unit =
        il.Encoder.OpCode(ILOpCode.Ldnull)
        il.Adjust 1

    /// Load argument `n` (`ldarg`). In a closure `Invoke` body `ldarg.0` is
    /// `this` and `ldarg.1` the single applied parameter; a `.ctor` body reads
    /// its capture parameters at `ldarg.1` onward.
    let emitLdarg (il: Il) (n: int) : unit =
        il.Encoder.LoadArgument(n)
        il.Adjust 1

    /// `ldfld` — pop the object reference, push the field value (net 0). Reads
    /// a closure's captured value off `this`.
    let emitLdfld (il: Il) (field: EntityHandle) : unit =
        il.Encoder.OpCode(ILOpCode.Ldfld)
        il.Encoder.Token(field)
        il.Adjust 0

    /// `stfld` — pop the object reference and the value (net -2). Stores a
    /// capture into a closure field from its `.ctor`.
    let emitStfld (il: Il) (field: EntityHandle) : unit =
        il.Encoder.OpCode(ILOpCode.Stfld)
        il.Encoder.Token(field)
        il.Adjust -2

    let emitPop (il: Il) : unit =
        il.Encoder.OpCode(ILOpCode.Pop)
        il.Adjust -1

    /// `dup` — duplicate the top value (net +1). A union factory dups the freshly
    /// constructed object once per field-store, keeping the object on the stack
    /// to return.
    let emitDup (il: Il) : unit =
        il.Encoder.OpCode(ILOpCode.Dup)
        il.Adjust 1

    let emitRet (il: Il) : unit = il.Encoder.OpCode(ILOpCode.Ret)

    // ---- Branching (the SRM `ControlFlowBuilder` fixes up label offsets) ----

    /// Reserve a branch target. Place it later in the stream with `markLabel`;
    /// `buildBody`'s `ControlFlowBuilder` resolves the forward/backward offset.
    let defineLabel (il: Il) : LabelHandle = il.Encoder.DefineLabel()

    /// Place a previously-defined label at the current instruction position.
    let markLabel (il: Il) (label: LabelHandle) : unit = il.Encoder.MarkLabel(label)

    /// `br` — unconditional branch (no stack change).
    let emitBr (il: Il) (label: LabelHandle) : unit = il.Encoder.Branch(ILOpCode.Br, label)

    /// `brfalse` — pop the top value, branch if it is zero / null.
    let emitBrFalse (il: Il) (label: LabelHandle) : unit =
        il.Encoder.Branch(ILOpCode.Brfalse, label)
        il.Adjust -1

    /// `brtrue` — pop the top value, branch if it is non-zero / non-null.
    let emitBrTrue (il: Il) (label: LabelHandle) : unit =
        il.Encoder.Branch(ILOpCode.Brtrue, label)
        il.Adjust -1

    /// `bne.un` — pop the top two values, branch if they are not equal
    /// (unordered: NaN counts as unequal). The "skip this arm" test for a
    /// literal-pattern match.
    let emitBneUn (il: Il) (label: LabelHandle) : unit =
        il.Encoder.Branch(ILOpCode.Bne_un, label)
        il.Adjust -2

    /// `beq` — pop the top two values, branch if they are equal.
    let emitBeq (il: Il) (label: LabelHandle) : unit =
        il.Encoder.Branch(ILOpCode.Beq, label)
        il.Adjust -2

    /// `throw` — pop the exception object and raise it. Terminates the flow, so
    /// nothing executes after it on this path (the depth tracker still settles
    /// to the post-pop value for any merge that follows).
    let emitThrow (il: Il) : unit =
        il.Encoder.OpCode(ILOpCode.Throw)
        il.Adjust -1

    // ---- Finalisation ----

    /// Run an untyped emitter against a fresh body and append it to the method
    /// body stream, returning the body's offset for the `MethodDefinition`
    /// row. `maxStack` comes from the tracked peak depth — no separate pass.
    /// `encodeLocals` turns the body's declared locals into the standalone
    /// local-variable signature; it is only invoked when locals exist, so
    /// callers with no provider (hand-written bodies) can pass any encoder.
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
