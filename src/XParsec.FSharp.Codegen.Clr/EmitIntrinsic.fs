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
/// generic operator opcodes) and the runtime type operators (`:>` / `:?>` / `:?`).
module EmitIntrinsic =

    let buildILIntrinsic (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let operand = TastAccessor.exprILIntrinsicTypeOperand e
        let args = TastAccessor.exprChildren e

        match TastAccessor.exprILIntrinsicOpCode e with
        | "newarr" ->
            // `Array.zeroCreate count` — push the count, then `newarr <elem>` (the
            // element type is carried on `typeOperand`).
            for a in args do
                recur env b a

            match operand with
            | ValueSome elem -> b.Add(ILInstr.Newarr(env.Provider.TypeToken elem))
            | ValueNone -> failwith "Emit: 'newarr' without an element type operand"
        | "ldelem" ->
            // `arr.[i]` — push the array then the index, then `ldelem <elem>`.
            for a in args do
                recur env b a

            match operand with
            | ValueSome elem -> b.Add(ILInstr.Ldelem(env.Provider.TypeToken elem))
            | ValueNone -> failwith "Emit: 'ldelem' without an element type operand"
        | "stelem" ->
            // `arr.[i] <- v` — push the array, the index, then the value, then
            // `stelem <elem>`.
            for a in args do
                recur env b a

            match operand with
            | ValueSome elem -> b.Add(ILInstr.Stelem(env.Provider.TypeToken elem))
            | ValueNone -> failwith "Emit: 'stelem' without an element type operand"

            // `stelem` leaves nothing on the stack, but the store is a `unit`
            // expression: a body that is a bare `arr.[i] <- v` must leave the unit
            // value for `ret`.
            EmitTypes.buildUnitValue env b
        | "ldobj" ->
            // `span.[i]` byref-return deref — the arg (a `call get_Item`) leaves a
            // managed pointer `T&`, then `ldobj <elem>` loads the pointed-to value.
            for a in args do
                recur env b a

            match operand with
            | ValueSome elem -> b.Add(ILInstr.Ldobj(env.Provider.TypeToken elem))
            | ValueNone -> failwith "Emit: 'ldobj' without an element type operand"
        | "ldloca" ->
            // `&local` (managed address-of) — push the address so a BCL `out`/`ref`
            // parameter can write through it. The sole operand is the local `Var`;
            // emit `ldloca <slot>` rather than recurring, which would `ldloc` the value.
            match args with
            | [| var |] ->
                match var with
                | LocalSlot env slot -> b.Add(ILInstr.Ldloca slot)
                | _ -> failwithf "Emit: address-of (&) requires an addressable mutable local, got %A" var
            | _ -> failwith "Emit: 'ldloca' intrinsic expects exactly one operand"
        | "box" ->
            // `box value` — push the value, then `box <T>` (the boxed type is carried
            // on `typeOperand`).
            for a in args do
                recur env b a

            match operand with
            | ValueSome elem -> b.Add(ILInstr.Box(env.Provider.TypeToken elem))
            | ValueNone -> failwith "Emit: 'box' without a type operand"
        | "ilzero" ->
            // `Unchecked.defaultof<'T>` — zero a fresh scratch local and load it
            // (`ldloca; initobj; ldloc`). `initobj` yields null for a reference type and
            // all-zeroes for a value type, so an unconstrained typar `'T` works too.
            let ty =
                match operand with
                | ValueSome t -> t
                | ValueNone -> TastAccessor.exprTy e

            let slot = b.Local ty
            b.Add(ILInstr.Ldloca slot)
            b.Add(ILInstr.Initobj(env.Provider.TypeToken ty))
            b.Add(ILInstr.Ldloc slot)
        | "ldlen" ->
            // `arr.Length` — push the array, `ldlen` (native int), then `conv.i4`
            // to narrow to the int32 F# `.Length` returns.
            for a in args do
                recur env b a

            b.Add ILInstr.Ldlen
            b.Add(ILInstr.Un ILOpCode.Conv_i4)
        | opCode ->
            for a in args do
                recur env b a

            // `throw` pops the exception and never returns, so it leaves no result and
            // doesn't fit `tryOpCodeOfMnemonic`'s balanced-result shape.
            if opCode = "throw" then
                if args.Length <> 1 then
                    failwithf "Emit: %d-ary inline-IL instruction 'throw' is out of scope" args.Length

                b.Add ILInstr.Throw
            elif opCode = "" then
                // Empty-mnemonic reinterpret cast, e.g. `(# "" value : uint32 #)` —
                // F#'s sign-only int32↔uint32 conversion, a stack no-op per ECMA-335
                // III §1.5. The operand is already pushed; emit nothing.
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

    let buildStaticOptimization (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        // Reaching codegen unresolved means the function was never inline-expanded
        // against a concrete operand type (used as a first-class value, or declared
        // without `inline`). F# falls back to the leading (dynamic) expression.
        recur env b (TastAccessor.exprStaticOptimizationDefault e)

    let buildUpcast (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        // `e :> T`: a reference-type source is already usable as its base, so emit
        // nothing. A value-type source must `box` to reach `obj` / an interface; a
        // generic typar source (`(x: 'T) :> obj`) must `box` too.
        let source = TastAccessor.exprChild e 0
        recur env b source
        let srcTy = typeOfExpr source

        match srcTy with
        | FTTypar _ -> b.Add(ILInstr.Box(env.Provider.TypeToken srcTy))
        | _ when isValueType env srcTy -> b.Add(ILInstr.Box(env.Provider.TypeToken srcTy))
        | _ -> ()

    let buildDowncast (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        // `e :?> T`: `unbox.any` for a value-type target, `castclass` for a
        // reference-type one. Both throw `InvalidCastException` on a real mismatch.
        let source = TastAccessor.exprChild e 0
        let ty = TastAccessor.exprTy e
        recur env b source
        let token = env.Provider.TypeToken ty

        if isValueType env ty then
            b.Add(ILInstr.UnboxAny token)
        else
            b.Add(ILInstr.Castclass token)

    let buildTypeTest (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        // `e :? T` → `isinst T; ldnull; cgt.un` — a non-null `isinst` result
        // (the value really is a `T`) compares greater-than null, yielding 1.
        let source = TastAccessor.exprChild e 0
        let testTy = TastAccessor.exprTypeTestTestTy e
        recur env b source
        b.Add(ILInstr.Isinst(env.Provider.TypeToken testTy))
        b.Add ILInstr.Ldnull
        b.Add(ILInstr.Bin ILOpCode.Cgt_un)
