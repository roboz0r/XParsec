namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

// Emits over SRM's `InstructionEncoder` (deterministic, PE-to-disk capable)
// rather than `System.Reflection.Emit.ILGenerator`.
//
// One emission surface: untyped depth-tracked `emit*` helpers over a shared
// `Il`. The dynamic TAST walker and the per-type templates build a reified
// `IlIr` buffer; `IlIr.lower` replays it through these helpers (so the bytes are
// what a hand-written emitter produces). `Il` tracks peak stack depth, so
// finalisation hands `maxStack` to `AddMethodBody` with no separate pass.

[<RequireQualifiedAccess>]
module Cil =

    // ---- Untyped depth-tracked helpers (replayed by `IlIr.lower`) ----

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
    /// reference typed as `t` (or `null` when it isn't a `t`). Net stack-neutral.
    let emitIsinst (il: Il) (t: EntityHandle) : unit =
        il.Encoder.OpCode(ILOpCode.Isinst)
        il.Encoder.Token(t)
        il.Adjust 0

    /// `callvirt` against a metadata handle with the receiver + args already on
    /// the stack. SRM has no `Callvirt` helper, so the opcode + token are emitted
    /// by hand; depth adjusts by `pushes - argc` (`argc` includes the receiver).
    let emitCallvirt (il: Il) (m: EntityHandle) (argc: int) (pushes: int) : unit =
        il.Encoder.OpCode(ILOpCode.Callvirt)
        il.Encoder.Token(m)
        il.Adjust(pushes - argc)

    let emitRet (il: Il) : unit = il.Encoder.OpCode(ILOpCode.Ret)

    // ---- Inline-IL value ops (the value-level `(# "op" args : ty #)`) ----

    /// Map an F# inline-IL mnemonic (`"ceq"`, `"add"`, `"conv.i2"`) to its
    /// `ILOpCode`. The value-level sibling of the type-level intrinsic repr map
    /// (`IntrinsicRepr`): an operator `.fs` body (`(=)` → `ceq`, `(<)` → `clt`,
    /// `(+)` → `add`) lowers to `TExpr.ILIntrinsic` carrying the mnemonic, and
    /// codegen interprets it here rather than special-casing the operator name.
    /// Scoped to the stack-balanced single-result ops the equality / comparison /
    /// arithmetic surface needs; `ValueNone` for anything else (operand-bearing
    /// branches, loads/stores) the simple value-op emitter can't model. Mnemonics
    /// are lower-case with `.`-separated suffixes, matching F# `(# … #)` syntax.
    let tryOpCodeOfMnemonic (mnemonic: string) : ILOpCode voption =
        match mnemonic with
        // Comparison (each leaves an int32 bool).
        | "ceq" -> ValueSome ILOpCode.Ceq
        | "cgt" -> ValueSome ILOpCode.Cgt
        | "cgt.un" -> ValueSome ILOpCode.Cgt_un
        | "clt" -> ValueSome ILOpCode.Clt
        | "clt.un" -> ValueSome ILOpCode.Clt_un
        // Arithmetic.
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
        // Bitwise / shift.
        | "and" -> ValueSome ILOpCode.And
        | "or" -> ValueSome ILOpCode.Or
        | "xor" -> ValueSome ILOpCode.Xor
        | "not" -> ValueSome ILOpCode.Not
        | "shl" -> ValueSome ILOpCode.Shl
        | "shr" -> ValueSome ILOpCode.Shr
        | "shr.un" -> ValueSome ILOpCode.Shr_un
        // Conversions (each pops one, pushes one).
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

    /// Emit a value-producing inline-IL op whose `argCount` operands are already
    /// on the stack (pushed by the caller). Every op in `tryOpCodeOfMnemonic`'s
    /// scope leaves exactly one result, so the net stack delta is `1 - argCount`
    /// (binary ops `-1`, unary conversions `0`).
    let emitIntrinsicValueOp (il: Il) (code: ILOpCode) (argCount: int) : unit =
        il.Encoder.OpCode code
        il.Adjust(1 - argCount)

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

    /// `leave <label>` — the only legal exit from a protected region. The runtime
    /// clears the entire evaluation stack as a side effect; from the linear
    /// emitter's POV the path terminates, so the depth tracker isn't adjusted
    /// (the next reachable Mark resets it from `analyze`'s LabelDepths).
    let emitLeave (il: Il) (label: LabelHandle) : unit =
        il.Encoder.Branch(ILOpCode.Leave, label)

    /// `endfinally` — terminator inside a finally handler. Depth at this point
    /// is 0 (CLI requirement); no adjustment needed.
    let emitEndFinally (il: Il) : unit = il.Encoder.OpCode(ILOpCode.Endfinally)

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
