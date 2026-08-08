namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

// Every method body is built as an `ILBody` — by the TAST walker or by a fixed
// template — then scanned twice: `analyze` derives `maxStack` and the depth at
// each label, `lower` replays the buffer through the `Cil.emit*` helpers.

/// The reified instruction. Branch targets are label ids (ints) minted by the
/// builder; locals are indices into `ILBody.Locals`. Tokenful ops carry their
/// `EntityHandle`; stack ops split `Bin` (net −1) / `Un` (net 0).
[<RequireQualifiedAccess>]
type ILInstr =
    | Ldarg of int
    | Ldloc of int
    | Stloc of int
    | Ldloca of int
    | LdcI4 of int
    | LdcI8 of int64
    | LdcR4 of single
    | LdcR8 of double
    | Ldstr of UserStringHandle
    | Ldnull
    | Dup
    | Pop
    | Ldfld of EntityHandle
    | Ldflda of EntityHandle
    | Stfld of EntityHandle
    /// `ldsfld` — load a static field (net +1). Class `static let` backing fields.
    | Ldsfld of EntityHandle
    /// `stsfld` — store a static field (net −1). Emitted in a synthesised
    /// `.cctor` to seed each `static let` field.
    | Stsfld of EntityHandle
    | Isinst of EntityHandle
    /// `castclass <type>` — checked reference downcast (`:?>` on a ref type). Net 0.
    | Castclass of EntityHandle
    /// `box <type>` — box a value type into `object` (`:>` from a value type). Net 0.
    | Box of EntityHandle
    /// `unbox.any <type>` — unbox / checked cast to a value type (`:?>`). Net 0.
    | UnboxAny of EntityHandle
    /// `initobj <type>` — zero-initialise the value-type instance whose managed
    /// pointer is on the stack (net −1: pops the address). How `()` is materialised:
    /// `unit` is a zero-field struct, so `ldloca; initobj; ldloc` yields the value.
    | Initobj of EntityHandle
    /// `constrained. <type>` — prefix on the next `callvirt`, dispatching a value-type
    /// `this` pointer (managed pointer) without boxing. Net 0 (the `callvirt` adjusts).
    | Constrained of EntityHandle
    /// `newarr <elem>` — allocate a 1-D zero-based array of `elem`; pops the
    /// element count, pushes the array reference (net 0).
    | Newarr of EntityHandle
    /// `ldelem <elem>` — pops the array reference and the index, pushes the
    /// element (net −1).
    | Ldelem of EntityHandle
    /// `stelem <elem>` — pops the array reference, the index and the value (net −3).
    | Stelem of EntityHandle
    /// `ldlen` — pops the array reference, pushes its length as a native int (net 0).
    /// `arr.Length` narrows that to int32 with a following `conv.i4`.
    | Ldlen
    /// `ldobj <type>` — pops a managed pointer, pushes the value it points to (net 0).
    /// The deref behind a by-ref return: `span.[i]` is `call get_Item` (yields `T&`)
    /// then `ldobj T`.
    | Ldobj of EntityHandle
    | Newobj of EntityHandle * argc: int
    | Call of EntityHandle * argc: int * pushes: int
    | Callvirt of EntityHandle * argc: int * pushes: int
    /// A binary stack op (net −1): `ceq`, `add`, `clt`, … (`bne.un` is `BneUn`).
    | Bin of ILOpCode
    /// A unary stack op (net 0): `neg`, `not`, `conv.*`.
    | Un of ILOpCode
    /// An opaque provider recipe: the provider owns the emission, `lower` replays it
    /// via `recipe.Emit`. `analyze` treats it as one op of net
    /// `Pushes - Arity.FlatArgCount`, without seeing inside it.
    | Recipe of CallRecipe
    | Mark of int
    | Br of int
    | Brfalse of int
    | Brtrue of int
    | BneUn of int
    | Beq of int
    /// `throw` — pops the exception object; a terminator, like `Ret`.
    | Throw
    | Ret
    /// Pseudo-mark opening a try region: emits no IL, `lower` mints and marks the
    /// internal `tryStart` label.
    | Try
    /// Pseudo-mark closing the try body and opening a catch handler for `exn`. CLI
    /// pushes the exception object at handler entry, so the depth there is 1.
    | BeginCatch of exn: EntityHandle
    /// Pseudo-mark closing the try body and opening a finally handler. CLI runs the
    /// handler with an empty evaluation stack, so the depth there is 0.
    | BeginFinally
    /// Pseudo-mark closing a catch handler, paired with the most recent `BeginCatch`.
    /// A finally is closed by `EndFinally` instead, which is also its terminator.
    | EndCatch
    /// `leave <label>` — the only legal exit from a protected region. The runtime
    /// clears the evaluation stack, so the target sees depth 0 whatever the depth
    /// at the source. Like `Br`, the fall-through path is unreachable.
    | Leave of int
    /// `endfinally` — the terminator closing a finally handler.
    | EndFinally

type ILBody =
    {
        Locals: ResizeArray<FrozenType>
        Instrs: ResizeArray<ILInstr>
        /// Count of labels minted. Ids are dense `0 .. LabelCount-1`, so the two
        /// scans index plain arrays rather than a map.
        LabelCount: int
    }

/// The stack delta along the linear path the builder is appending to: a branch pops
/// its operands here (−1 / −2) and `Throw` pops the exception object, where
/// `straightDelta` scores both 0 because its walk follows the branch instead.
module private InstrDelta =
    let ofInstr (i: ILInstr) : int =
        match i with
        | ILInstr.Ldarg _
        | ILInstr.Ldloc _
        | ILInstr.Ldloca _
        | ILInstr.LdcI4 _
        | ILInstr.LdcI8 _
        | ILInstr.LdcR4 _
        | ILInstr.LdcR8 _
        | ILInstr.Ldstr _
        | ILInstr.Ldnull
        | ILInstr.Dup -> 1
        | ILInstr.Stloc _
        | ILInstr.Pop -> -1
        | ILInstr.Ldfld _
        | ILInstr.Ldflda _
        | ILInstr.Isinst _
        | ILInstr.Castclass _
        | ILInstr.Box _
        | ILInstr.UnboxAny _
        | ILInstr.Constrained _
        | ILInstr.Newarr _
        | ILInstr.Ldlen
        | ILInstr.Un _ -> 0
        | ILInstr.Ldsfld _ -> 1
        | ILInstr.Stsfld _ -> -1
        | ILInstr.Stfld _ -> -2
        | ILInstr.Initobj _ -> -1
        | ILInstr.Ldelem _ -> -1
        | ILInstr.Stelem _ -> -3
        | ILInstr.Ldobj _ -> 0
        | ILInstr.Newobj(_, argc) -> 1 - argc
        | ILInstr.Call(_, argc, pushes)
        | ILInstr.Callvirt(_, argc, pushes) -> pushes - argc
        | ILInstr.Recipe recipe -> recipe.Pushes - recipe.Arity.FlatArgCount
        | ILInstr.Bin _ -> -1
        | ILInstr.Brfalse _
        | ILInstr.Brtrue _ -> -1
        | ILInstr.BneUn _
        | ILInstr.Beq _ -> -2
        | ILInstr.Throw -> -1
        | ILInstr.Mark _
        | ILInstr.Br _
        | ILInstr.Ret -> 0
        // The pseudo-marks emit no IL; `Leave` clears the whole stack rather than
        // popping a fixed count, so the caller resets the depth with `SetDepth`.
        | ILInstr.Try
        | ILInstr.BeginCatch _
        | ILInstr.BeginFinally
        | ILInstr.EndCatch
        | ILInstr.Leave _
        | ILInstr.EndFinally -> 0

/// Mints label ids and local slots, accumulates instructions, and tracks the
/// running operand-stack `Depth` — which the walker reads to know how many values
/// a statement left behind to discard.
type IlBuilder() =
    let locals = ResizeArray<FrozenType>()
    let instrs = ResizeArray<ILInstr>()
    let mutable nextLabel = 0
    let mutable depth = 0

    member _.Local(ty: FrozenType) : int =
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

    /// Restore the depth to a saved value at a branch merge, where the linear count
    /// has followed only one arm. `Mark` carries no depth of its own; `analyze`
    /// re-derives the merge depth for lowering.
    member _.SetDepth(d: int) : unit = depth <- d

    member _.Body: ILBody =
        // The live buffers, not a copy: the builder is not used again.
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
        | ILInstr.LdcI8 _
        | ILInstr.LdcR4 _
        | ILInstr.LdcR8 _
        | ILInstr.Ldstr _
        | ILInstr.Ldnull
        | ILInstr.Dup -> 1
        | ILInstr.Stloc _
        | ILInstr.Pop -> -1
        | ILInstr.Ldfld _
        | ILInstr.Ldflda _
        | ILInstr.Isinst _
        | ILInstr.Castclass _
        | ILInstr.Box _
        | ILInstr.UnboxAny _
        | ILInstr.Constrained _
        | ILInstr.Newarr _
        | ILInstr.Ldlen
        | ILInstr.Ldobj _
        | ILInstr.Un _ -> 0
        | ILInstr.Ldsfld _ -> 1
        | ILInstr.Stsfld _ -> -1
        | ILInstr.Stfld _ -> -2
        | ILInstr.Initobj _ -> -1
        | ILInstr.Ldelem _ -> -1
        | ILInstr.Stelem _ -> -3
        | ILInstr.Newobj(_, argc) -> 1 - argc
        | ILInstr.Call(_, argc, pushes)
        | ILInstr.Callvirt(_, argc, pushes) -> pushes - argc
        | ILInstr.Recipe recipe -> recipe.Pushes - recipe.Arity.FlatArgCount
        | ILInstr.Bin _ -> -1
        | ILInstr.Mark _
        | ILInstr.Br _
        | ILInstr.Brfalse _
        | ILInstr.Brtrue _
        | ILInstr.BneUn _
        | ILInstr.Beq _
        | ILInstr.Throw
        | ILInstr.Ret
        | ILInstr.Try
        | ILInstr.BeginCatch _
        | ILInstr.BeginFinally
        | ILInstr.EndCatch
        | ILInstr.Leave _
        | ILInstr.EndFinally -> 0

    type Analysis =
        {
            MaxStack: int
            /// Operand-stack depth at each label, indexed by `LabelId` (dense). `-1`
            /// = a label whose depth is not yet known (a real depth is always ≥ 0).
            LabelDepths: int[]
        }

    /// Abstract-stack walk over the buffer: the `maxStack` and the depth at every
    /// label, or the first imbalance. `ValueNone` = unreachable, from a terminator
    /// until the next `Mark`. A label reached at two depths is a hard error.
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
                            // A dead merge point: marked while unreachable and never
                            // targeted — the never-taken `nextLabel` of an irrefutable
                            // final `match` arm. CLI §III leaves its depth free, so 0.
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
                | ILInstr.Leave l ->
                    // `leave` clears the evaluation stack, so the target sees depth 0
                    // whatever the source depth. A dead `Leave` notes nothing.
                    (match cur with
                     | ValueSome _ -> note l 0
                     | ValueNone -> ())

                    cur <- ValueNone
                | ILInstr.EndFinally -> cur <- ValueNone
                | ILInstr.Try
                | ILInstr.EndCatch ->
                    // Region bookkeeping only: `Try` opens at the depth already
                    // reached, and by `EndCatch` the handler has left or thrown.
                    ()
                | ILInstr.BeginFinally ->
                    // CLI: a finally handler is entered with an empty stack.
                    cur <- ValueSome 0
                | ILInstr.BeginCatch _ ->
                    // CLI: the runtime pushes the exception object at handler entry,
                    // so bump `maxStack` — no `straightDelta` accounts for it.
                    cur <- ValueSome 1

                    if maxStack < 1 then
                        maxStack <- 1
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

    let verify (body: ILBody) : Result<int, string> =
        analyze body |> Result.map (fun a -> a.MaxStack)

    /// Replay the buffer into `il` through the `Cil.emit*` helpers. Each `Mark`
    /// resets the linear depth tracker to what `analyze` computed for that label.
    /// Locals are declared in order, so a local's id is its slot index.
    let lower (body: ILBody) (il: Il) : unit =
        // Every method body is lowered here, so this call is also codegen's standing
        // stack-balance check: an underflow or a depth conflict fails the build.
        let analysis =
            match analyze body with
            | Result.Ok a -> a
            | Result.Error e -> failwithf "IlIr.lower: unbalanced body: %s" e

        for ty in body.Locals do
            il.DeclareLocal ty |> ignore

        // Ids are dense, so reserve every handle up front; `markLabel` fixes positions.
        let labels = Array.init body.LabelCount (fun _ -> Cil.defineLabel il)

        // A region is recorded on the `ControlFlowBuilder` by mating four handles:
        // tryStart, tryEnd, handlerStart, handlerEnd. The pseudo-marks carry no label
        // of their own, so `lower` mints them; the stack nests regions LIFO.
        let cfb =
            match il.Encoder.ControlFlowBuilder with
            | null -> failwith "IlIr.lower: InstructionEncoder has no ControlFlowBuilder"
            | b -> b

        let regionStack = ResizeArray<LabelHandle * LabelHandle * ILInstr>()

        for i in body.Instrs do
            match i with
            | ILInstr.Ldarg n -> Cil.emitLdarg il n
            | ILInstr.Ldloc n -> Cil.emitLdloc il n
            | ILInstr.Stloc n -> Cil.emitStloc il n
            | ILInstr.Ldloca n -> Cil.emitLdloca il n
            | ILInstr.LdcI4 n -> Cil.emitLdcI4 il n
            | ILInstr.LdcI8 n -> Cil.emitLdcI8 il n
            | ILInstr.LdcR4 x -> Cil.emitLdcR4 il x
            | ILInstr.LdcR8 x -> Cil.emitLdcR8 il x
            | ILInstr.Ldstr h -> Cil.emitLdstr il h
            | ILInstr.Ldnull -> Cil.emitLdnull il
            | ILInstr.Dup -> Cil.emitDup il
            | ILInstr.Pop -> Cil.emitPop il
            | ILInstr.Ldfld f -> Cil.emitLdfld il f
            | ILInstr.Ldflda f -> Cil.emitLdflda il f
            | ILInstr.Stfld f -> Cil.emitStfld il f
            | ILInstr.Ldsfld f -> Cil.emitLdsfld il f
            | ILInstr.Stsfld f -> Cil.emitStsfld il f
            | ILInstr.Isinst t -> Cil.emitIsinst il t
            | ILInstr.Castclass t -> Cil.emitCastclass il t
            | ILInstr.Box t -> Cil.emitBox il t
            | ILInstr.UnboxAny t -> Cil.emitUnboxAny il t
            | ILInstr.Initobj t -> Cil.emitInitobj il t
            | ILInstr.Constrained t -> Cil.emitConstrained il t
            | ILInstr.Newarr t -> Cil.emitNewarr il t
            | ILInstr.Ldelem t -> Cil.emitLdelem il t
            | ILInstr.Stelem t -> Cil.emitStelem il t
            | ILInstr.Ldobj t -> Cil.emitLdobj il t
            | ILInstr.Ldlen -> Cil.emitLdlen il
            | ILInstr.Newobj(c, argc) -> Cil.emitNewobj il c argc
            | ILInstr.Call(m, argc, pushes) -> Cil.emitCall il m argc pushes
            | ILInstr.Callvirt(m, argc, pushes) -> Cil.emitCallvirt il m argc pushes
            | ILInstr.Recipe recipe ->
                recipe.Emit il
                il.Adjust(recipe.Pushes - recipe.Arity.FlatArgCount)
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
            | ILInstr.Try ->
                let tryStart = Cil.defineLabel il
                Cil.markLabel il tryStart
                // The handler start and kind are unknown until the begin-handler
                // instruction overwrites this entry.
                regionStack.Add(tryStart, Unchecked.defaultof<LabelHandle>, ILInstr.Try)
            | ILInstr.BeginFinally ->
                if regionStack.Count = 0 then
                    failwith "IlIr.lower: BeginFinally without a matching Try"

                let top = regionStack.Count - 1
                let tryStart, _, _ = regionStack.[top]
                let handlerStart = Cil.defineLabel il
                Cil.markLabel il handlerStart
                il.SetDepth 0 // CLI: finally handler entered with empty stack.
                regionStack.[top] <- (tryStart, handlerStart, ILInstr.BeginFinally)
            | ILInstr.BeginCatch h ->
                if regionStack.Count = 0 then
                    failwith "IlIr.lower: BeginCatch without a matching Try"

                let top = regionStack.Count - 1
                let tryStart, _, _ = regionStack.[top]
                let handlerStart = Cil.defineLabel il
                Cil.markLabel il handlerStart
                // The runtime pushes the exception object at handler entry; adjusting
                // rather than setting 1 is what lets `Il.MaxStack` see it.
                il.SetDepth 0
                il.Adjust 1
                regionStack.[top] <- (tryStart, handlerStart, ILInstr.BeginCatch h)
            | ILInstr.EndFinally ->
                if regionStack.Count = 0 then
                    failwith "IlIr.lower: EndFinally without a matching Try/BeginFinally"

                Cil.emitEndFinally il
                let handlerEnd = Cil.defineLabel il
                Cil.markLabel il handlerEnd
                let top = regionStack.Count - 1
                let tryStart, handlerStart, kind = regionStack.[top]
                regionStack.RemoveAt top

                match kind with
                | ILInstr.BeginFinally -> cfb.AddFinallyRegion(tryStart, handlerStart, handlerStart, handlerEnd)
                | _ -> failwithf "IlIr.lower: EndFinally closing a non-finally region (%A)" kind
            | ILInstr.EndCatch ->
                if regionStack.Count = 0 then
                    failwith "IlIr.lower: EndCatch without a matching Try/BeginCatch"

                let handlerEnd = Cil.defineLabel il
                Cil.markLabel il handlerEnd
                let top = regionStack.Count - 1
                let tryStart, handlerStart, kind = regionStack.[top]
                regionStack.RemoveAt top

                match kind with
                | ILInstr.BeginCatch exn -> cfb.AddCatchRegion(tryStart, handlerStart, handlerStart, handlerEnd, exn)
                | _ -> failwithf "IlIr.lower: EndCatch closing a non-catch region (%A)" kind
            | ILInstr.Leave l -> Cil.emitLeave il labels.[l]

        if regionStack.Count <> 0 then
            failwithf "IlIr.lower: %d exception region(s) left open at end of body" regionStack.Count
