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

/// Iteration constructs: `for x in src`, `for i = a to b`, `while`. Each is a unit
/// expression leaving one reified `unit`.
module EmitLoops =

    /// The resolved shape of a `for x in src` enumerator walk. No `Dispose` handle:
    /// disposal always goes through the `System.IDisposable::Dispose` interface slot,
    /// minted once by the shared emitter.
    type private EnumeratorLoop =
        {
            ElemTy: FrozenType
            EnumeratorTy: FrozenType
            GetEnumerator: EntityHandle
            MoveNext: EntityHandle
            Current: EntityHandle
            IsValueType: bool
            Disposable: bool
            // Select `constrained. <Source> callvirt` over a by-address `call` for
            // `GetEnumerator`. Read only when the source is addressed.
            GetEnumeratorViaInterface: bool
            // The source is a generic typar reached through a custom seq interface.
            GetEnumViaConstrained: bool
            // The enumerator `E` is itself a generic typar — its `MoveNext` / `Current`
            // go `constrained. <E> callvirt`, addressing the slot for struct and class.
            MembersViaConstrained: bool
        }

    /// Mint the interface method-slot handle for a `constrained. callvirt` for-in
    /// dispatch. A *generic* interface is routed so the slot lands on the instantiated
    /// `TypeSpec` (`IStructSeq`1<!E>`); a non-generic one uses its `Def` handle.
    let private constrainedSlot
        (env: EmitEnv)
        (ifaceKey: TypeKey)
        (ifaceArgs: EqArray<FrozenType>)
        (memberName: string)
        : EntityHandle =
        let iface =
            match env.Interfaces.TryGetValue(SymbolKey.Type ifaceKey) with
            | true, i -> i
            | false, _ -> failwithf "EmitLoops: constrained for-in on unregistered interface '%A'" ifaceKey

        let m =
            match iface.Members.TryGetValue memberName with
            | true, candidates -> pickOverload memberName candidates []
            | false, _ -> failwithf "EmitLoops: interface '%A' has no emitted member '%s'" ifaceKey memberName

        EmitResolve.memberRef
            env
            iface.Typars
            ifaceKey
            (EqArray.toList ifaceArgs)
            (UserMemberKind.Member(m.MetaName, false, m.MethodTyparCount, m.ParamTys, m.RetTy))
            m.Handle

    /// Interface-slot disposal, so a `callvirt` reaches `E`'s implementation even when
    /// `E`'s own member table carries no `Dispose` row.
    let private mintDisposeHandle (env: EmitEnv) : EntityHandle =
        env.Provider.ExternalMemberRef(
            SymbolKeyOps.memberKey
                (SymbolKeyOps.typeKeyOf "System" "IDisposable")
                "Dispose"
                EqArray.empty
                0
                MemberKind.Method,
            false,
            false,
            FTFun(FTConst(RuntimeNames.unitKey, EqArray.empty), FTConst(RuntimeNames.unitKey, EqArray.empty))
        )

    /// `MoveNext` / `Current` for an external enumerator `E`: the refs go against the
    /// declaring instantiation `enumeratorTy`, since a T-free `MoveNext(): bool`
    /// signature cannot recover it.
    let private externalEnumMembers
        (env: EmitEnv)
        (enumeratorTy: FrozenType)
        (mnKey: SymbolKey)
        (curKey: SymbolKey)
        (elemTy: FrozenType)
        : EntityHandle * EntityHandle =
        let mn =
            env.Provider.ExternalMemberRefOn(
                mnKey,
                enumeratorTy,
                false,
                false,
                FTFun(FTConst(RuntimeNames.unitKey, EqArray.empty), FTConst(RuntimeNames.boolKey, EqArray.empty))
            )

        let cur =
            env.Provider.ExternalMemberRefOn(curKey, enumeratorTy, true, false, elemTy)

        mn, cur

    /// `let e = src.GetEnumerator()`, then `while e.MoveNext() do (bind e.Current →
    /// pat); body`, wrapped in `try … finally e.Dispose()` when `loop.Disposable`.
    /// Leaves the reified `unit`.
    let private emitEnumeratorLoop
        (recur: Recur)
        (env: EmitEnv)
        (b: IlBuilder)
        (loop: EnumeratorLoop)
        (pat: TastAccessor.PatId)
        (source: TastAccessor.ExprId)
        (body: TastAccessor.ExprId)
        : unit =
        // Only the interface `Dispose` call needs a `constrained.` token; a struct's
        // `MoveNext` / `Current` are its own members, reached by a plain `call`.
        let constrainedTok =
            if loop.IsValueType then
                ValueSome(env.Provider.TypeToken loop.EnumeratorTy)
            else
                ValueNone

        let enumSlot = b.Local loop.EnumeratorTy

        // Object argument for a member call on `E`: a struct or constrained typar by address
        // (`ldloca`), a reference enumerator by value (`ldloc`).
        let loadEnumObjArg () =
            if loop.IsValueType || loop.MembersViaConstrained then
                b.Add(ILInstr.Ldloca enumSlot)
            else
                b.Add(ILInstr.Ldloc enumSlot)

        // A concrete struct `E`'s `MoveNext` / `Current` are non-virtual, so a `call`.
        let callEnumMember (handle: EntityHandle) =
            if loop.MembersViaConstrained then
                b.Add(ILInstr.Constrained(env.Provider.TypeToken loop.EnumeratorTy))
                b.Add(ILInstr.Callvirt(handle, 1, 1))
            elif loop.IsValueType then
                b.Add(ILInstr.Call(handle, 1, 1))
            else
                b.Add(ILInstr.Callvirt(handle, 1, 1))

        let sourceTy = typeOfExpr source

        // The `GetEnumerator` object arg is spilled and addressed for a value-type source
        // (a method call on a value) and for a constrained-typar source — an `FTTypar`
        // is not statically a value type, but `constrained. callvirt` needs its address.
        if EmitPattern.isValueType env sourceTy || loop.GetEnumViaConstrained then
            recur env b source
            let srcSlot = b.Local sourceTy
            b.Add(ILInstr.Stloc srcSlot)
            b.Add(ILInstr.Ldloca srcSlot)

            if loop.GetEnumeratorViaInterface then
                b.Add(ILInstr.Constrained(env.Provider.TypeToken sourceTy))
                b.Add(ILInstr.Callvirt(loop.GetEnumerator, 1, 1))
            else
                b.Add(ILInstr.Call(loop.GetEnumerator, 1, 1))
        else
            recur env b source
            b.Add(ILInstr.Callvirt(loop.GetEnumerator, 1, 1))

        b.Add(ILInstr.Stloc enumSlot)

        let xSlot = b.Local loop.ElemTy
        let loopStart = b.Label()
        let loopEnd = b.Label()
        let endLabel = b.Label()

        if loop.Disposable then
            b.Add ILInstr.Try

        b.Add(ILInstr.Mark loopStart)
        loadEnumObjArg ()
        callEnumMember loop.MoveNext
        b.Add(ILInstr.Brfalse loopEnd)
        // `x = e.Current`, then the unit-typed body whose value is discarded.
        loadEnumObjArg ()
        callEnumMember loop.Current
        b.Add(ILInstr.Stloc xSlot)
        bindPattern env b xSlot pat
        recur env b body
        b.Add ILInstr.Pop
        b.Add(ILInstr.Br loopStart)
        b.Add(ILInstr.Mark loopEnd)

        if loop.Disposable then
            let dispHandle = mintDisposeHandle env
            b.Add(ILInstr.Leave endLabel)
            b.Add ILInstr.BeginFinally
            b.SetDepth 0

            if loop.IsValueType then
                // A struct value is never null, and `brfalse` on a value is invalid IL,
                // so dispose unconditionally. `IDisposable.Dispose` returns a real
                // `void`, so the callvirt consumes only the object arg — nothing to pop.
                b.Add(ILInstr.Ldloca enumSlot)

                match constrainedTok with
                | ValueSome t -> b.Add(ILInstr.Constrained t)
                | ValueNone -> ()

                b.Add(ILInstr.Callvirt(dispHandle, 1, 0))
            else
                // Reference enumerator: null-checked `callvirt` disposal.
                let skipLabel = b.Label()
                b.Add(ILInstr.Ldloc enumSlot)
                b.Add(ILInstr.Brfalse skipLabel)
                b.Add(ILInstr.Ldloc enumSlot)
                b.Add(ILInstr.Callvirt(dispHandle, 1, 0))
                b.Add(ILInstr.Mark skipLabel)

            b.Add ILInstr.EndFinally
            b.SetDepth 0
            b.Add(ILInstr.Mark endLabel)
        else
            // `E` is not `IDisposable` — no `try … finally` region at all.
            b.Add(ILInstr.Mark endLabel)

        // `for` is a unit expression — leave the single reified `unit` value.
        EmitTypes.buildUnitValue env b

    let buildForIn (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let view = TastAccessor.exprForIn e
        let pat = view.Pat
        let source = view.Source
        let body = view.Body
        let enumerator = view.Enumerator
        let elemTy = typeOfPat pat

        match enumerator with
        | ForInEnumeratorG.Pattern(enumeratorTy, getEnum, members, isValueType, dispose) ->
            // Duck-typed `GetEnumerator()` — C#'s non-boxing `foreach`: the source
            // exposes a concrete `E` with `MoveNext(): bool` and a `Current` property
            // without implementing `IEnumerable<'T>`, so the loop walks `E` directly.
            let geHandle =
                match getEnum with
                | ForInGetEnumG.External geKey ->
                    env.Provider.ExternalMemberRef(
                        geKey,
                        false,
                        false,
                        FTFun(FTConst(RuntimeNames.unitKey, EqArray.empty), enumeratorTy)
                    )
                | ForInGetEnumG.Local -> fst (resolveInstanceMember env (typeOfExpr source) "GetEnumerator" [])
                | ForInGetEnumG.ConstrainedInterface(ifaceKey, ifaceArgs) ->
                    constrainedSlot env ifaceKey ifaceArgs "GetEnumerator"

            let mnHandle, curHandle =
                match members with
                | ForInEnumMembersG.External(mnKey, curKey) -> externalEnumMembers env enumeratorTy mnKey curKey elemTy
                | ForInEnumMembersG.Local ->
                    fst (resolveInstanceMember env enumeratorTy "MoveNext" []),
                    fst (resolveInstanceMember env enumeratorTy "Current" [])
                | ForInEnumMembersG.ConstrainedInterface(ifaceKey, ifaceArgs) ->
                    constrainedSlot env ifaceKey ifaceArgs "MoveNext", constrainedSlot env ifaceKey ifaceArgs "Current"

            let getEnumViaConstrained =
                match getEnum with
                | ForInGetEnumG.ConstrainedInterface _ -> true
                | _ -> false

            let membersViaConstrained =
                match members with
                | ForInEnumMembersG.ConstrainedInterface _ -> true
                | _ -> false

            emitEnumeratorLoop
                recur
                env
                b
                {
                    ElemTy = elemTy
                    EnumeratorTy = enumeratorTy
                    GetEnumerator = geHandle
                    MoveNext = mnHandle
                    Current = curHandle
                    IsValueType = isValueType
                    Disposable = dispose
                    GetEnumeratorViaInterface = getEnumViaConstrained
                    GetEnumViaConstrained = getEnumViaConstrained
                    MembersViaConstrained = membersViaConstrained
                }
                pat
                source
                body
        | ForInEnumeratorG.Interface ->
            // `for x in src` over an `IEnumerable<'T>`. Each member ref is minted
            // against its DECLARING interface (`IEnumerable`1` / `IEnumerator` /
            // `IEnumerator`1`), so a `callvirt` dispatches to the collection's impl.
            let enumTy =
                FTClass(SymbolKeyOps.typeKeyOf "System.Collections.Generic" "IEnumerator`1", EqArray.singleton elemTy)

            let geKey =
                SymbolKeyOps.memberKey
                    (SymbolKeyOps.typeKeyOf "System.Collections.Generic" "IEnumerable`1")
                    "GetEnumerator"
                    EqArray.empty
                    0
                    MemberKind.Method

            let geHandle =
                env.Provider.ExternalMemberRef(
                    geKey,
                    false,
                    false,
                    FTFun(FTConst(RuntimeNames.unitKey, EqArray.empty), enumTy)
                )

            let mnKey =
                SymbolKeyOps.memberKey
                    (SymbolKeyOps.typeKeyOf "System.Collections" "IEnumerator")
                    "MoveNext"
                    EqArray.empty
                    0
                    MemberKind.Method

            let mnHandle =
                env.Provider.ExternalMemberRef(
                    mnKey,
                    false,
                    false,
                    FTFun(FTConst(RuntimeNames.unitKey, EqArray.empty), FTConst(RuntimeNames.boolKey, EqArray.empty))
                )

            let curKey =
                SymbolKeyOps.memberKey
                    (SymbolKeyOps.typeKeyOf "System.Collections.Generic" "IEnumerator`1")
                    "Current"
                    EqArray.empty
                    0
                    MemberKind.Property

            let curHandle = env.Provider.ExternalMemberRef(curKey, true, false, elemTy)

            // An `IEnumerator<'T>` is always a reference and always `IDisposable`.
            emitEnumeratorLoop
                recur
                env
                b
                {
                    ElemTy = elemTy
                    EnumeratorTy = enumTy
                    GetEnumerator = geHandle
                    MoveNext = mnHandle
                    Current = curHandle
                    IsValueType = false
                    Disposable = true
                    GetEnumeratorViaInterface = true
                    GetEnumViaConstrained = false
                    MembersViaConstrained = false
                }
                pat
                source
                body

    let buildForTo (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let view = TastAccessor.exprForTo e
        let var = view.Var
        let startExpr = view.StartExpr
        let endExpr = view.EndExpr
        let body = view.Body
        // `a`/`b` are evaluated once, into the loop variable and a hidden limit local.
        // The loop exits *before* the increment when `i = limit`, so the last iteration
        // cannot overflow `i + 1` at `b = Int32.MaxValue`.
        let intTy = FTConst(RuntimeNames.intKey, EqArray.empty)
        let iSlot = b.Local intTy
        let limitSlot = b.Local intTy
        env.Slots.[var] <- iSlot

        recur env b startExpr
        b.Add(ILInstr.Stloc iSlot)
        recur env b endExpr
        b.Add(ILInstr.Stloc limitSlot)

        let loopBody = b.Label()
        let loopEnd = b.Label()
        let baseDepth = b.Depth

        // `i > limit` (signed) → exit before the first iteration on an empty range.
        b.Add(ILInstr.Ldloc iSlot)
        b.Add(ILInstr.Ldloc limitSlot)
        b.Add(ILInstr.Bin ILOpCode.Cgt)
        b.Add(ILInstr.Brtrue loopEnd)

        b.Add(ILInstr.Mark loopBody)
        recur env b body

        while b.Depth > baseDepth do
            b.Add ILInstr.Pop

        // `i = limit` → done (skips the increment that would overflow at MaxValue).
        b.Add(ILInstr.Ldloc iSlot)
        b.Add(ILInstr.Ldloc limitSlot)
        b.Add(ILInstr.Beq loopEnd)
        b.Add(ILInstr.Ldloc iSlot)
        b.Add(ILInstr.LdcI4 1)
        b.Add(ILInstr.Bin ILOpCode.Add)
        b.Add(ILInstr.Stloc iSlot)
        b.Add(ILInstr.Br loopBody)
        b.SetDepth baseDepth
        b.Add(ILInstr.Mark loopEnd)
        // `for` is a unit expression — leave the single reified `unit` value.
        EmitTypes.buildUnitValue env b

    let buildWhile (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let view = TastAccessor.exprWhile e
        let cond = view.Cond
        let body = view.Body
        // The body is a unit statement whose value is discarded each iteration, popped
        // back to the loop-top base. The depth tracker is reset to that base before the
        // exit label, so post-loop discards stay correct across the back-edge.
        let loopStart = b.Label()
        let loopEnd = b.Label()
        let baseDepth = b.Depth
        b.Add(ILInstr.Mark loopStart)
        recur env b cond
        b.Add(ILInstr.Brfalse loopEnd)
        recur env b body

        while b.Depth > baseDepth do
            b.Add ILInstr.Pop

        b.Add(ILInstr.Br loopStart)
        b.SetDepth baseDepth
        b.Add(ILInstr.Mark loopEnd)
        EmitTypes.buildUnitValue env b
