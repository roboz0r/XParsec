namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

// Depth tracking lives here: each `emit*` adjusts `Il` by its own stack effect,
// so a caller never adjusts as well.

[<RequireQualifiedAccess>]
module Cil =

    let emitLdstr (il: Il) (h: UserStringHandle) : unit =
        il.Encoder.LoadString(h)
        il.Adjust 1

    let emitLdcI4 (il: Il) (n: int) : unit =
        il.Encoder.LoadConstantI4(n)
        il.Adjust 1

    let emitLdcI8 (il: Il) (n: int64) : unit =
        il.Encoder.LoadConstantI8(n)
        il.Adjust 1

    let emitLdcR4 (il: Il) (x: single) : unit =
        il.Encoder.LoadConstantR4(x)
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

    /// Managed pointer to local slot `n`, used as the `this` pointer for a value-type
    /// instance call, or as the target of an in-place `initobj` / `.ctor`.
    let emitLdloca (il: Il) (n: int) : unit =
        il.Encoder.LoadLocalAddress(n)
        il.Adjust 1

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

    // `ldflda` — like `ldfld` but pushes the field's *address*, so a member call on
    // a value-type field mutates it in place instead of a copy.
    let emitLdflda (il: Il) (field: EntityHandle) : unit =
        il.Encoder.OpCode(ILOpCode.Ldflda)
        il.Encoder.Token(field)
        il.Adjust 0

    let emitStfld (il: Il) (field: EntityHandle) : unit =
        il.Encoder.OpCode(ILOpCode.Stfld)
        il.Encoder.Token(field)
        il.Adjust -2

    let emitLdsfld (il: Il) (field: EntityHandle) : unit =
        il.Encoder.OpCode(ILOpCode.Ldsfld)
        il.Encoder.Token(field)
        il.Adjust 1

    let emitStsfld (il: Il) (field: EntityHandle) : unit =
        il.Encoder.OpCode(ILOpCode.Stsfld)
        il.Encoder.Token(field)
        il.Adjust -1

    let emitPop (il: Il) : unit =
        il.Encoder.OpCode(ILOpCode.Pop)
        il.Adjust -1

    let emitDup (il: Il) : unit =
        il.Encoder.OpCode(ILOpCode.Dup)
        il.Adjust 1

    /// `isinst <type>` — replaces the object reference on the stack with the same
    /// reference typed as `t`, or `null` when it isn't a `t`.
    let emitIsinst (il: Il) (t: EntityHandle) : unit =
        il.Encoder.OpCode(ILOpCode.Isinst)
        il.Encoder.Token(t)
        il.Adjust 0

    /// `castclass <type>` — checked reference downcast: throws
    /// `InvalidCastException` when the object isn't a `t`, else retypes the reference.
    let emitCastclass (il: Il) (t: EntityHandle) : unit =
        il.Encoder.OpCode(ILOpCode.Castclass)
        il.Encoder.Token(t)
        il.Adjust 0

    /// `box <type>` — boxes the value type `t` on the stack into an object reference.
    let emitBox (il: Il) (t: EntityHandle) : unit =
        il.Encoder.OpCode(ILOpCode.Box)
        il.Encoder.Token(t)
        il.Adjust 0

    /// `unbox.any <type>` — unboxes a boxed `t`; for a ref type, behaves as `castclass`.
    let emitUnboxAny (il: Il) (t: EntityHandle) : unit =
        il.Encoder.OpCode(ILOpCode.Unbox_any)
        il.Encoder.Token(t)
        il.Adjust 0

    /// `initobj <type>` — zero-initialise the value-type instance addressed by the
    /// managed pointer on the stack, popping it (net −1).
    let emitInitobj (il: Il) (t: EntityHandle) : unit =
        il.Encoder.OpCode(ILOpCode.Initobj)
        il.Encoder.Token(t)
        il.Adjust -1

    /// SRM has no `Callvirt` helper, so the opcode + token go out by hand. `argc`
    /// includes the `this` pointer.
    let emitCallvirt (il: Il) (m: EntityHandle) (argc: int) (pushes: int) : unit =
        il.Encoder.OpCode(ILOpCode.Callvirt)
        il.Encoder.Token(m)
        il.Adjust(pushes - argc)

    /// `constrained. <type>` — prefix making the following `callvirt` dispatch on a
    /// value-type `this` pointer (a managed pointer) without boxing. Net 0: a prefix emits
    /// no operand traffic of its own, the paired `callvirt` does the adjust.
    let emitConstrained (il: Il) (t: EntityHandle) : unit =
        il.Encoder.OpCode(ILOpCode.Constrained)
        il.Encoder.Token(t)
        il.Adjust 0

    /// `newarr <elem>` — allocate a 1-D zero-based array of `elem`: pops the element
    /// count, pushes the array reference.
    let emitNewarr (il: Il) (t: EntityHandle) : unit =
        il.Encoder.OpCode(ILOpCode.Newarr)
        il.Encoder.Token(t)
        il.Adjust 0

    /// `ldelem <elem>` — pops the array reference and the index, pushes the element.
    /// The token-carrying form, so it serves any element type.
    let emitLdelem (il: Il) (t: EntityHandle) : unit =
        il.Encoder.OpCode(ILOpCode.Ldelem)
        il.Encoder.Token(t)
        il.Adjust -1

    /// `stelem <elem>` — pops the array reference, the index and the value (net −3).
    /// The token-carrying form, so it serves any element type.
    let emitStelem (il: Il) (t: EntityHandle) : unit =
        il.Encoder.OpCode(ILOpCode.Stelem)
        il.Encoder.Token(t)
        il.Adjust -3

    /// `ldobj <type>` — pops a managed pointer, pushes the value it points to. The
    /// deref behind a by-ref return: `span.[i]` is `call get_Item` (yields `T&`)
    /// then `ldobj T`.
    let emitLdobj (il: Il) (t: EntityHandle) : unit =
        il.Encoder.OpCode(ILOpCode.Ldobj)
        il.Encoder.Token(t)
        il.Adjust 0

    /// `ldlen` — pops the array reference, pushes its length as a native int.
    let emitLdlen (il: Il) : unit =
        il.Encoder.OpCode(ILOpCode.Ldlen)
        il.Adjust 0

    let emitRet (il: Il) : unit = il.Encoder.OpCode(ILOpCode.Ret)

    /// Map an F# inline-IL mnemonic to its `ILOpCode`: an operator body `(# "ceq" … #)`
    /// lowers to a `TExpr.ILIntrinsic` carrying the string. `ValueNone` outside the
    /// pop-n-push-1 ops, because a branch or a load/store needs an operand this can't carry.
    let tryOpCodeOfMnemonic (mnemonic: string) : ILOpCode voption =
        match mnemonic with
        | "ceq" -> ValueSome ILOpCode.Ceq
        | "cgt" -> ValueSome ILOpCode.Cgt
        | "cgt.un" -> ValueSome ILOpCode.Cgt_un
        | "clt" -> ValueSome ILOpCode.Clt
        | "clt.un" -> ValueSome ILOpCode.Clt_un
        | "add" -> ValueSome ILOpCode.Add
        | "add.ovf" -> ValueSome ILOpCode.Add_ovf
        | "add.ovf.un" -> ValueSome ILOpCode.Add_ovf_un
        | "sub" -> ValueSome ILOpCode.Sub
        | "sub.ovf" -> ValueSome ILOpCode.Sub_ovf
        | "sub.ovf.un" -> ValueSome ILOpCode.Sub_ovf_un
        | "mul" -> ValueSome ILOpCode.Mul
        | "mul.ovf" -> ValueSome ILOpCode.Mul_ovf
        | "mul.ovf.un" -> ValueSome ILOpCode.Mul_ovf_un
        | "div" -> ValueSome ILOpCode.Div
        | "div.un" -> ValueSome ILOpCode.Div_un
        | "rem" -> ValueSome ILOpCode.Rem
        | "rem.un" -> ValueSome ILOpCode.Rem_un
        | "neg" -> ValueSome ILOpCode.Neg
        | "and" -> ValueSome ILOpCode.And
        | "or" -> ValueSome ILOpCode.Or
        | "xor" -> ValueSome ILOpCode.Xor
        | "not" -> ValueSome ILOpCode.Not
        | "shl" -> ValueSome ILOpCode.Shl
        | "shr" -> ValueSome ILOpCode.Shr
        | "shr.un" -> ValueSome ILOpCode.Shr_un
        | "conv.i1" -> ValueSome ILOpCode.Conv_i1
        | "conv.i2" -> ValueSome ILOpCode.Conv_i2
        | "conv.i4" -> ValueSome ILOpCode.Conv_i4
        | "conv.i8" -> ValueSome ILOpCode.Conv_i8
        | "conv.u1" -> ValueSome ILOpCode.Conv_u1
        | "conv.u2" -> ValueSome ILOpCode.Conv_u2
        | "conv.u4" -> ValueSome ILOpCode.Conv_u4
        | "conv.u8" -> ValueSome ILOpCode.Conv_u8
        | "conv.i" -> ValueSome ILOpCode.Conv_i
        | "conv.u" -> ValueSome ILOpCode.Conv_u
        | "conv.r4" -> ValueSome ILOpCode.Conv_r4
        | "conv.r8" -> ValueSome ILOpCode.Conv_r8
        | "conv.r.un" -> ValueSome ILOpCode.Conv_r_un
        | _ -> ValueNone

    /// The `argCount` operands are already on the stack and the op leaves exactly one
    /// result, so the delta is `1 - argCount`: `-1` for `add`, `0` for `conv.i4`.
    let emitIntrinsicValueOp (il: Il) (code: ILOpCode) (argCount: int) : unit =
        il.Encoder.OpCode code
        il.Adjust(1 - argCount)

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

    /// `switch` over `targets`, pops the `int32` selector.
    let emitSwitch (il: Il) (targets: LabelHandle list) : unit =
        let s = il.Encoder.Switch(List.length targets)

        for label in targets do
            s.Branch label

        il.Adjust -1

    /// Pops the exception object. The path terminates here, so the tracked depth
    /// matters only until the next `Mark` resets it.
    let emitThrow (il: Il) : unit =
        il.Encoder.OpCode(ILOpCode.Throw)
        il.Adjust -1

    /// `leave <label>` — the only legal exit from a protected region. The runtime
    /// clears the whole evaluation stack, so no adjustment: the path terminates and
    /// the next `Mark` resets the depth.
    let emitLeave (il: Il) (label: LabelHandle) : unit =
        il.Encoder.Branch(ILOpCode.Leave, label)

    /// `endfinally` — terminator closing a finally handler; the depth is already 0.
    let emitEndFinally (il: Il) : unit = il.Encoder.OpCode(ILOpCode.Endfinally)

    /// `maxStack` is the peak depth `Il` tracked while `emit` ran, so no separate pass
    /// computes it.
    let buildBody
        (encodeLocals: FrozenType list -> StandaloneSignatureHandle)
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
