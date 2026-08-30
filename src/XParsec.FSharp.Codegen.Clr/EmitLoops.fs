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

    /// How the `GetEnumerator` object argument is loaded and its call dispatched.
    type private SourceDispatch =
        /// The evaluated source is the object argument to a `callvirt`.
        | SourceByValue
        /// The source is spilled to a local, whose address is the object argument to a `call`.
        | SourceByAddress
        /// The source is spilled to a local, whose address is the object argument to a
        /// `constrained. <Source> callvirt`.
        | SourceByAddressConstrained

    /// How `MoveNext` / `Current` / `Dispose` reach the enumerator `E`.
    type private EnumeratorDispatch =
        /// A reference `E`: `ldloc` then `callvirt`, with disposal guarded by a null check.
        | EnumRefByValue
        /// A concrete struct `E`: `ldloca` then `call`, with unconditional disposal through
        /// `constrained. <E> callvirt` on the carried token.
        | EnumStructByAddress of disposeConstrainedTok: EntityHandle
        /// A typar `E`: `ldloca` then `constrained. <E> callvirt` for every member, with
        /// unconditional disposal.
        | EnumTyparConstrained

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
            SourceDispatch: SourceDispatch
            EnumeratorDispatch: EnumeratorDispatch
            Disposable: bool
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
            match env.Interfaces.TryGetValue ifaceKey with
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
        let enumSlot = b.Local loop.EnumeratorTy

        let loadEnumObjArg () =
            match loop.EnumeratorDispatch with
            | EnumRefByValue -> b.Add(ILInstr.Ldloc enumSlot)
            | EnumStructByAddress _
            | EnumTyparConstrained -> b.Add(ILInstr.Ldloca enumSlot)

        let callEnumMember (handle: EntityHandle) =
            match loop.EnumeratorDispatch with
            | EnumRefByValue -> b.Add(ILInstr.Callvirt(handle, 1, 1))
            | EnumStructByAddress _ -> b.Add(ILInstr.Call(handle, 1, 1))
            | EnumTyparConstrained ->
                b.Add(ILInstr.Constrained(env.Provider.TypeToken loop.EnumeratorTy))
                b.Add(ILInstr.Callvirt(handle, 1, 1))

        let sourceTy = typeOfExpr source

        let spillSourceAddress () =
            recur env b source
            let srcSlot = b.Local sourceTy
            b.Add(ILInstr.Stloc srcSlot)
            b.Add(ILInstr.Ldloca srcSlot)

        match loop.SourceDispatch with
        | SourceByValue ->
            recur env b source
            b.Add(ILInstr.Callvirt(loop.GetEnumerator, 1, 1))
        | SourceByAddress ->
            spillSourceAddress ()
            b.Add(ILInstr.Call(loop.GetEnumerator, 1, 1))
        | SourceByAddressConstrained ->
            spillSourceAddress ()
            b.Add(ILInstr.Constrained(env.Provider.TypeToken sourceTy))
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

            // `IDisposable.Dispose` returns a real `void`, so the `callvirt` consumes only the
            // object arg, leaving nothing to pop.
            let disposeAddressed (tok: EntityHandle) =
                b.Add(ILInstr.Ldloca enumSlot)
                b.Add(ILInstr.Constrained tok)
                b.Add(ILInstr.Callvirt(dispHandle, 1, 0))

            match loop.EnumeratorDispatch with
            | EnumRefByValue ->
                let skipLabel = b.Label()
                b.Add(ILInstr.Ldloc enumSlot)
                b.Add(ILInstr.Brfalse skipLabel)
                b.Add(ILInstr.Ldloc enumSlot)
                b.Add(ILInstr.Callvirt(dispHandle, 1, 0))
                b.Add(ILInstr.Mark skipLabel)
            | EnumStructByAddress tok -> disposeAddressed tok
            | EnumTyparConstrained -> disposeAddressed (env.Provider.TypeToken loop.EnumeratorTy)

            b.Add ILInstr.EndFinally
            b.SetDepth 0
            b.Add(ILInstr.Mark endLabel)
        else
            // `E` is not `IDisposable`, so no `try … finally` region at all.
            b.Add(ILInstr.Mark endLabel)

        // `for` is a unit expression, so leave the single reified `unit` value.
        EmitTypes.buildUnitValue env b

    let buildForIn (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let view = TastAccessor.exprForIn e
        let pat = view.Pat
        let source = view.Source
        let body = view.Body
        let enumerator = view.Enumerator
        let elemTy = typeOfPat pat

        match enumerator with
        | ForInEnumeratorG.Pattern {
                                       EnumeratorTy = enumeratorTy
                                       GetEnumerator = getEnum
                                       Members = members
                                       IsValueType = isValueType
                                       Dispose = dispose
                                   } ->
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
                | ForInGetEnumG.Local -> fst (resolveInstanceMember env (nominalOfExpr source) "GetEnumerator" [])
                | ForInGetEnumG.ConstrainedInterface(ifaceKey, ifaceArgs) ->
                    constrainedSlot env ifaceKey ifaceArgs "GetEnumerator"

            let mnHandle, curHandle =
                match members with
                | ForInEnumMembersG.External(mnKey, curKey) -> externalEnumMembers env enumeratorTy mnKey curKey elemTy
                | ForInEnumMembersG.Local ->
                    // A duck-typed enumerator emitted in this assembly is one of its own
                    // nominals, so `MoveNext` / `Current` resolve off the type's key.
                    let en = FrozenNominal.ofFrozen "a `for … in` enumerator" enumeratorTy

                    fst (resolveInstanceMember env en "MoveNext" []), fst (resolveInstanceMember env en "Current" [])
                | ForInEnumMembersG.ConstrainedInterface(ifaceKey, ifaceArgs) ->
                    constrainedSlot env ifaceKey ifaceArgs "MoveNext", constrainedSlot env ifaceKey ifaceArgs "Current"

            let sourceDispatch =
                match getEnum with
                | ForInGetEnumG.ConstrainedInterface _ -> SourceByAddressConstrained
                | _ when EmitPattern.isValueType env (typeOfExpr source) -> SourceByAddress
                | _ -> SourceByValue

            let enumeratorDispatch =
                match members with
                | ForInEnumMembersG.ConstrainedInterface _ -> EnumTyparConstrained
                | _ when isValueType -> EnumStructByAddress(env.Provider.TypeToken enumeratorTy)
                | _ -> EnumRefByValue

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
                    SourceDispatch = sourceDispatch
                    EnumeratorDispatch = enumeratorDispatch
                    Disposable = dispose
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

            // A struct source reaches `IEnumerable`1::GetEnumerator` through its own
            // implementation, so the interface slot is addressed and `constrained.`.
            let sourceDispatch =
                if EmitPattern.isValueType env (typeOfExpr source) then
                    SourceByAddressConstrained
                else
                    SourceByValue

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
                    SourceDispatch = sourceDispatch
                    EnumeratorDispatch = EnumRefByValue
                    Disposable = true
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
        // `for` is a unit expression, so leave the single reified `unit` value.
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
