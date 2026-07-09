namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis
open EmitTypes
open EmitLower
open EmitResolve
open EmitPattern
open EmitDispatch

/// Inline-IL intrinsics (`newarr` / `ldelem` / `stelem` / `box` / `ldlen` and the
/// generic operator opcodes) plus the runtime type operators (`:>` / `:?>` /
/// `:?`) and the unresolved `StaticOptimization` fallback.
module EmitIntrinsic =

    let buildILIntrinsic (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.ILIntrinsic("newarr", operand, args, _, _) ->
            // `Array.zeroCreate count` — push the count, then `newarr <elem>`.
            // The element type rides `typeOperand` (Freeze recovered it from the
            // result array type).
            for a in args do
                recur env b a

            match operand with
            | ValueSome elem -> b.Add(ILInstr.Newarr(env.Provider.TypeToken elem))
            | ValueNone -> failwith "Emit: 'newarr' without an element type operand"
        | TExprG.ILIntrinsic("ldelem", operand, args, _, _) ->
            // `arr.[i]` — push the array then the index, then `ldelem <elem>`.
            for a in args do
                recur env b a

            match operand with
            | ValueSome elem -> b.Add(ILInstr.Ldelem(env.Provider.TypeToken elem))
            | ValueNone -> failwith "Emit: 'ldelem' without an element type operand"
        | TExprG.ILIntrinsic("stelem", operand, args, _, _) ->
            // `arr.[i] <- v` — push the array, the index, then the value, then
            // `stelem <elem>`. The element type rides `typeOperand` (Freeze
            // recovered it from the value operand).
            for a in args do
                recur env b a

            match operand with
            | ValueSome elem -> b.Add(ILInstr.Stelem(env.Provider.TypeToken elem))
            | ValueNone -> failwith "Emit: 'stelem' without an element type operand"

            // The store is a `unit` expression but `stelem` leaves nothing on the
            // stack; reify the `unit` value so it behaves like every other unit
            // expression (`for`, `()` literal) — a function body that is a bare
            // `arr.[i] <- v` must leave the unit return value for `ret`.
            EmitTypes.buildUnitValue env b
        | TExprG.ILIntrinsic("ldobj", operand, args, _, _) ->
            // `span.[i]` byref-return deref — emit the arg (the `call get_Item`,
            // which leaves a managed pointer `T&` on the stack), then `ldobj <elem>`
            // to load the pointed-to element value. The element type rides
            // `typeOperand` (Freeze set it to the value-position result type).
            for a in args do
                recur env b a

            match operand with
            | ValueSome elem -> b.Add(ILInstr.Ldobj(env.Provider.TypeToken elem))
            | ValueNone -> failwith "Emit: 'ldobj' without an element type operand"
        | TExprG.ILIntrinsic("ldloca", _, args, _, _) ->
            // `&local` (managed address-of) — push the address of the mutable local
            // so a BCL `out`/`ref` parameter can write through it. The sole operand
            // is the local `Var`; emit `ldloca <slot>` rather than recurring (which
            // would `ldloc` the value). Mirrors the struct-receiver address dispatch
            // in `EmitCall`.
            match EqArray.toList args with
            | [ TExprG.Var(binding, _, _) ] when env.Slots.ContainsKey binding ->
                b.Add(ILInstr.Ldloca env.Slots.[binding])
            | [ other ] -> failwithf "Emit: address-of (&) requires an addressable mutable local, got %A" other
            | _ -> failwith "Emit: 'ldloca' intrinsic expects exactly one operand"
        | TExprG.ILIntrinsic("box", operand, args, _, _) ->
            // `box value` — push the value, then `box <T>`. The boxed type rides
            // `typeOperand` (Freeze recovered it from the argument's static type).
            // Identical instruction to the value-type `:>`-upcast path above; the
            // runtime treats `box` on a reference type as a no-op.
            for a in args do
                recur env b a

            match operand with
            | ValueSome elem -> b.Add(ILInstr.Box(env.Provider.TypeToken elem))
            | ValueNone -> failwith "Emit: 'box' without a type operand"
        | TExprG.ILIntrinsic("ilzero", operand, _, retTy, _) ->
            // `Unchecked.defaultof<'T>` — the default value of a type. Universal generic
            // form: zero a fresh scratch local and load it (`ldloca; initobj; ldloc`).
            // `initobj` yields null for a reference type and all-zeroes for a value type,
            // so it is valid for an unconstrained typar 'T (the `Seq.reduce` seed) as well
            // as a concrete instantiation. Mirrors the parameterless value-type
            // construction path in `EmitConstruct`.
            let ty =
                match operand with
                | ValueSome t -> t
                | ValueNone -> retTy

            let slot = b.Local ty
            b.Add(ILInstr.Ldloca slot)
            b.Add(ILInstr.Initobj(env.Provider.TypeToken ty))
            b.Add(ILInstr.Ldloc slot)
        | TExprG.ILIntrinsic("ldlen", _, args, _, _) ->
            // `arr.Length` — push the array, `ldlen` (native int), then `conv.i4`
            // to narrow to the int32 F# `.Length` returns.
            for a in args do
                recur env b a

            b.Add ILInstr.Ldlen
            b.Add(ILInstr.Un ILOpCode.Conv_i4)
        | TExprG.ILIntrinsic(opCode, _, args, _, _) ->
            // Push each operand, then append the mapped opcode. The dispatch
            // (which opcode for which operator/primitive) lives in the operator
            // `.fs` body this node was lowered from, not here — codegen only
            // interprets the IL.
            for a in args do
                recur env b a

            // `throw` is terminal — it pops the exception and ends the path,
            // so it doesn't fit `tryOpCodeOfMnemonic`'s balanced-result shape.
            // Tolerated in value position the same way a non-exhaustive `match`
            // fallthrough is (`buildMatchFailure`): `Throw` never returns, so
            // no result is left on the stack.
            if opCode = "throw" then
                if args.Length <> 1 then
                    failwithf "Emit: %d-ary inline-IL instruction 'throw' is out of scope" args.Length

                b.Add ILInstr.Throw
            elif opCode = "" then
                // Empty-mnemonic reinterpret cast, e.g. `(# "" value : uint32 #)` —
                // F#'s sign-only int32↔uint32 conversion, a stack no-op per
                // ECMA-335 III §1.5 (the two share one 32-bit slot). The operand is
                // already pushed; emit nothing.
                if args.Length <> 1 then
                    failwithf "Emit: %d-ary empty inline-IL reinterpret is out of scope" args.Length
            else
                match Cil.tryOpCodeOfMnemonic opCode with
                | ValueSome code ->
                    match args.Length with
                    | 2 -> b.Add(ILInstr.Bin code)
                    | 1 -> b.Add(ILInstr.Un code)
                    | n -> failwithf "Emit: %d-ary inline-IL instruction '%s' is out of scope" n opCode
                | ValueNone -> failwithf "Emit: unsupported inline-IL instruction '%s'" opCode
        | _ -> failwith "EmitIntrinsic.buildILIntrinsic: unreachable"

    let buildStaticOptimization (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.StaticOptimization(_, def, _, _) ->
            // Reaching codegen unresolved means the function was never
            // inline-expanded against a concrete operand type (used as a
            // first-class value, or declared without `inline`). F#'s semantics
            // fall back to the leading (dynamic) expression in that case.
            recur env b def
        | _ -> failwith "EmitIntrinsic.buildStaticOptimization: unreachable"

    let buildUpcast (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.Upcast(source, _, _) ->
            // `e :> T`: a reference-type source is already usable as its base —
            // the JIT erases the cast, so emit nothing. A value-type source must
            // be boxed to reach `obj` / an interface; a *generic typar* source
            // (`(x: 'T) :> obj`) must also `box` — a JIT no-op for a reference
            // instantiation but mandatory IL (matching `boxArgIntoObjParam`).
            recur env b source
            let srcTy = typeOfExpr source

            match srcTy with
            | FTTypar _ -> b.Add(ILInstr.Box(env.Provider.TypeToken srcTy))
            | _ when isValueType env srcTy -> b.Add(ILInstr.Box(env.Provider.TypeToken srcTy))
            | _ -> ()
        | _ -> failwith "EmitIntrinsic.buildUpcast: unreachable"

    let buildDowncast (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.Downcast(source, ty, _) ->
            // `e :?> T`: `unbox.any` for a value-type target, `castclass` for a
            // reference-type one. Both throw `InvalidCastException` at runtime on
            // a real mismatch.
            recur env b source
            let token = env.Provider.TypeToken ty

            if isValueType env ty then
                b.Add(ILInstr.UnboxAny token)
            else
                b.Add(ILInstr.Castclass token)
        | _ -> failwith "EmitIntrinsic.buildDowncast: unreachable"

    let buildTypeTest (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.TypeTest(source, testTy, _, _) ->
            // `e :? T` → `isinst T; ldnull; cgt.un` — a non-null `isinst` result
            // (the value really is a `T`) compares greater-than null, yielding 1.
            recur env b source
            b.Add(ILInstr.Isinst(env.Provider.TypeToken testTy))
            b.Add ILInstr.Ldnull
            b.Add(ILInstr.Bin ILOpCode.Cgt_un)
        | _ -> failwith "EmitIntrinsic.buildTypeTest: unreachable"
