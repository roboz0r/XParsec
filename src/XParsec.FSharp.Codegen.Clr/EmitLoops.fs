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

/// Iteration constructs: `for x in src`, `for i = a to b`, and `while`. Each is a
/// unit expression leaving one reified `unit`. The duck-typed `for-in` arm binds a
/// local `isValueType` *bool* off the enumerator descriptor that deliberately
/// shadows `EmitPattern.isValueType` — preserved from the single-module original.
module EmitLoops =

    /// The resolved shape of a `for x in src` enumerator walk: element and
    /// enumerator types, the three handles that drive it (`GetEnumerator` /
    /// `MoveNext` / `get_Current`), whether the enumerator is an unboxed struct
    /// (address-based dispatch, no null-check), and whether `E` is `IDisposable`
    /// (`false` ⇒ no `try`/`finally`). Disposal is always the
    /// `System.IDisposable::Dispose` interface slot, so no handle is carried —
    /// `emitEnumeratorLoop` mints it once (`mintDisposeHandle`). The `for-in` arms
    /// differ only in how they *resolve* the three drive handles, then share
    /// `emitEnumeratorLoop`.
    type private EnumeratorLoop =
        {
            ElemTy: FrozenType
            EnumeratorTy: FrozenType
            GetEnumerator: EntityHandle
            MoveNext: EntityHandle
            Current: EntityHandle
            IsValueType: bool
            Disposable: bool
            // Whether `GetEnumerator` is dispatched through the `IEnumerable<'T>`
            // interface slot (`Interface` arm) rather than the source's own concrete
            // method (`Pattern` arm). Only consulted for a *value-type* source, where
            // it selects `constrained. <Source> callvirt` over a by-address `call`.
            GetEnumeratorViaInterface: bool
            // Rung-3: the source is a *generic typar* (or a value reached only through
            // a custom seq interface) — `GetEnumerator` dispatches via
            // `constrained. <Source> callvirt iface::GetEnumerator`, so the source
            // receiver is addressed (`ldloca`) regardless of `EmitPattern.isValueType`
            // (an `FTTypar` is not statically a value type).
            GetEnumViaConstrained: bool
            // Rung-3: the enumerator `E` is itself a *generic typar* — its `MoveNext` /
            // `Current` dispatch via `constrained. <E> callvirt iface::…`, addressing
            // the enumerator slot and dispatching virtually for both struct and class.
            MembersViaConstrained: bool
        }

    /// Mint an interface method-slot handle for a `constrained. callvirt` for-in
    /// dispatch (rung-3): look the abstract slot up by name in the `EmittedInterface`
    /// registry (Wall A's `env.Interfaces`) and route a *generic* interface through
    /// `EmitResolve.memberRef` so the slot lands on the instantiated interface
    /// `TypeSpec` (`IStructSeq`1<!E>`). A non-generic interface (empty `Typars`) uses
    /// the slot's `Def` handle directly. Mirrors `EmitMember`'s `CallVia.Interface`
    /// slot resolution; for-in members (`GetEnumerator`/`MoveNext`/`Current`) take no
    /// arguments, so overload picking sees an empty arg-type list.
    let private constrainedSlot
        (env: EmitEnv)
        (ifaceKey: SymbolKey)
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
            (UserMemberKind.ClassMember(ClassMember.Member(m.MetaName, false, m.MethodTyparCount, m.ParamTys, m.RetTy)))
            m.Handle

    /// The `System.IDisposable::Dispose` handle — disposal for *every* `for … in`
    /// arm goes through the interface slot (a `callvirt`, or `constrained. callvirt`
    /// for a struct `E`, dispatches to the enumerator's impl), so each arm minted the
    /// identical handle. Minted once here.
    let private mintDisposeHandle (env: EmitEnv) : EntityHandle =
        env.Provider.ExternalMemberRef(
            SymbolKey.MemberKey(
                SymbolKey.TypeKey(None, "System", "IDisposable"),
                "Dispose",
                EqArray.empty,
                MemberKind.Method
            ),
            false,
            false,
            FTFun(
                FTConst(BuiltinTypes.intrinsicKey "unit", EqArray.empty),
                FTConst(BuiltinTypes.intrinsicKey "unit", EqArray.empty)
            )
        )

    /// The `MoveNext` / `Current` handles for an external enumerator `E` (the
    /// duck-typed and Gap 3 hybrid arms): both members are declared on `E` itself,
    /// so their refs come from `ExternalMemberRefOn` against the declaring
    /// instantiation `enumeratorTy` (not recoverable from a T-free `MoveNext(): bool`).
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
                FTFun(
                    FTConst(BuiltinTypes.intrinsicKey "unit", EqArray.empty),
                    FTConst(BuiltinTypes.intrinsicKey "bool", EqArray.empty)
                )
            )

        let cur =
            env.Provider.ExternalMemberRefOn(curKey, enumeratorTy, true, false, elemTy)

        mn, cur

    /// Emit the enumerator loop for `loop`:
    ///
    ///   let e = src.GetEnumerator()
    ///   try                                   // only when `Dispose` is set
    ///     while e.MoveNext() do (bind e.Current → pat); body
    ///   finally e.Dispose()                   // null-checked for a reference E,
    ///                                         // `constrained.` for a struct E
    ///
    /// A struct enumerator dispatches by address (`ldloca` + a direct `call`) with
    /// no null-check (a struct value is never null); a reference enumerator loads by
    /// value and `callvirt`s. The `MoveNext` / `Current` calls on a struct are a
    /// plain `call` — its concrete value type is statically known, exactly as the
    /// F# compiler lowers `for x in struct-enumerator` — *not* `constrained.
    /// callvirt`: those members are ordinary (non-virtual) instance methods on `E`,
    /// and a `constrained. callvirt` to a non-virtual struct `MethodDef` mis-dispatches
    /// (it walks an uninitialised receiver). `Dispose` is the lone exception — it is
    /// reached through the `IDisposable` interface slot, so it keeps `constrained.
    /// callvirt` (interface dispatch on the boxed-or-addressed receiver). The loop
    /// variable's type is the iterated element type — a tuple binder (`for (k, v) in
    /// pairs`) `bindPattern`s the element, a simple binder aliases it. `for` is a
    /// unit expression, so the single reified `unit` value is left on the stack.
    let private emitEnumeratorLoop
        (recur: Recur)
        (env: EmitEnv)
        (b: IlBuilder)
        (loop: EnumeratorLoop)
        (pat: Frozen.TPat)
        (source: Frozen.TExpr)
        (body: Frozen.TExpr)
        : unit =
        // The `constrained.` token for a struct enumerator's interface `Dispose` —
        // `TypeToken` routes through the value-type-aware encoder, so `E` lands as a
        // value type. Only the `Dispose` interface call needs it; the concrete-type
        // `MoveNext` / `Current` calls are a plain `call` (see the doc comment).
        let constrainedTok =
            if loop.IsValueType then
                ValueSome(env.Provider.TypeToken loop.EnumeratorTy)
            else
                ValueNone

        let enumSlot = b.Local loop.EnumeratorTy

        // Load the enumerator as the receiver for a member call on its own type: a
        // struct by address (`ldloca`, for the by-address `call`), a reference by
        // value (`ldloc`, for the `callvirt`).
        let loadEnumReceiver () =
            // A struct (or a constrained typar, rung-3) is dispatched by address; a
            // reference enumerator by value.
            if loop.IsValueType || loop.MembersViaConstrained then
                b.Add(ILInstr.Ldloca enumSlot)
            else
                b.Add(ILInstr.Ldloc enumSlot)

        // Dispatch a member declared on `E` (`MoveNext` / `Current`): a direct `call`
        // for a concrete struct (type known), a plain `callvirt` for a reference, and
        // `constrained. <E> callvirt` when `E` is a generic typar (rung-3) — the JIT
        // dispatches a struct typar by address (no box) and a class typar by reference.
        let callEnumMember (handle: EntityHandle) =
            if loop.MembersViaConstrained then
                b.Add(ILInstr.Constrained(env.Provider.TypeToken loop.EnumeratorTy))
                b.Add(ILInstr.Callvirt(handle, 1, 1))
            elif loop.IsValueType then
                b.Add(ILInstr.Call(handle, 1, 1))
            else
                b.Add(ILInstr.Callvirt(handle, 1, 1))

        // Load the source as the `GetEnumerator` receiver. A reference source is
        // pushed by value and `callvirt`-ed. A *value-type* source (Gap 1: a struct
        // `MapSeq`/`ArraySeq`, or any `[<Struct>]` collection) is a method call on a
        // value, so it must be addressed exactly like the enumerator receiver: spill
        // to a local and `ldloca`, then dispatch its own concrete `GetEnumerator` with
        // a direct `call` (`Pattern` arm), or — when the only enumerable surface is the
        // `IEnumerable<'T>` slot — a `constrained. <Source> callvirt` (`Interface` arm).
        let sourceTy = typeOfExpr source

        // The source receiver is addressed (`ldloca`) when it is a value-type
        // collection OR a rung-3 constrained-typar source (an `FTTypar` is not
        // statically a value type, but `constrained. callvirt` needs its address).
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
        loadEnumReceiver ()
        callEnumMember loop.MoveNext
        b.Add(ILInstr.Brfalse loopEnd)
        // `x = e.Current`, then the unit-typed body whose value is discarded.
        loadEnumReceiver ()
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
                // A struct value is never null — no `brfalse` (invalid IL on a
                // value). Dispose via `constrained. <E>` callvirt on the address;
                // the external `IDisposable.Dispose` carries a real `void` return,
                // so the callvirt consumes only the receiver (no `pop`).
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
            // `E` is not `IDisposable` — no `try … finally` region at all (C#
            // parity); the plain `while` simply falls through.
            b.Add(ILInstr.Mark endLabel)

        // `for` is a unit expression — leave the single reified `unit` value.
        EmitTypes.buildUnitValue env b

    let buildForIn (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        // Matched in two layers so the inner `match` over `ForInEnumeratorG` is
        // *compiler-exhaustive* — a new enumerator kind fails the build here rather
        // than silently falling through to a wildcard. The outer `_` only guards the
        // (unreachable) non-`ForIn` dispatch.
        | TExprG.ForIn(pat, source, body, enumerator, _, _) ->
            let elemTy = typeOfPat pat

            match enumerator with
            | ForInEnumeratorG.Pattern(enumeratorTy, getEnum, members, isValueType, dispose) ->
                // Duck-typed `GetEnumerator()` — C#'s non-boxing `foreach`. The source
                // exposes a public `GetEnumerator()` returning a concrete enumerator
                // `E` (`enumeratorTy`) with `MoveNext(): bool` and a `Current`
                // property, without implementing `IEnumerable<'T>`. The loop walks `E`
                // directly — by address with no allocation when `E` is a struct
                // (`isValueType`). The two resolution axes are independent:
                //   • `getEnum`  — how to ref the source's `GetEnumerator`: `External`
                //     mints it from the carried key (`ExternalMemberRef`, return
                //     recovers the source instantiation); `Local` resolves it off the
                //     source expression's type (`resolveInstanceMember`).
                //   • `members`  — how to ref `E`'s `MoveNext` / `Current`: `External`
                //     mints them via `ExternalMemberRefOn` against `enumeratorTy` (a
                //     T-free `MoveNext(): bool` can't recover the declaring type);
                //     `Local` resolves them off the user `E` `TypeDef`.
                // `dispose` is `true` only when `E : IDisposable` (the shared emitter
                // mints the `System.IDisposable::Dispose` slot — a `callvirt`, or
                // `constrained.` for a struct `E`, dispatches to the impl even when its
                // slot is absent from `E`'s member table).
                let geHandle =
                    match getEnum with
                    | ForInGetEnumG.External geKey ->
                        env.Provider.ExternalMemberRef(
                            geKey,
                            false,
                            false,
                            FTFun(FTConst(BuiltinTypes.intrinsicKey "unit", EqArray.empty), enumeratorTy)
                        )
                    | ForInGetEnumG.Local -> fst (resolveInstanceMember env (typeOfExpr source) "GetEnumerator" [])
                    // Rung-3: a generic-typar source — `GetEnumerator` is the custom
                    // seq interface's abstract slot, dispatched `constrained. callvirt`.
                    | ForInGetEnumG.ConstrainedInterface(ifaceKey, ifaceArgs) ->
                        constrainedSlot env ifaceKey ifaceArgs "GetEnumerator"

                let mnHandle, curHandle =
                    match members with
                    | ForInEnumMembersG.External(mnKey, curKey) ->
                        externalEnumMembers env enumeratorTy mnKey curKey elemTy
                    | ForInEnumMembersG.Local ->
                        fst (resolveInstanceMember env enumeratorTy "MoveNext" []),
                        fst (resolveInstanceMember env enumeratorTy "Current" [])
                    // Rung-3: a generic-typar enumerator `E` — `MoveNext` / `Current`
                    // are the enumerator interface's abstract slots.
                    | ForInEnumMembersG.ConstrainedInterface(ifaceKey, ifaceArgs) ->
                        constrainedSlot env ifaceKey ifaceArgs "MoveNext",
                        constrainedSlot env ifaceKey ifaceArgs "Current"

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
                        // A constrained `GetEnumerator` is dispatched `constrained.
                        // callvirt` on the addressed source receiver.
                        GetEnumeratorViaInterface = getEnumViaConstrained
                        GetEnumViaConstrained = getEnumViaConstrained
                        MembersViaConstrained = membersViaConstrained
                    }
                    pat
                    source
                    body
            | ForInEnumeratorG.Interface ->
                // `for x in src do body` over an `IEnumerable<'T>`. Lowered to the
                // standard enumerator loop through the interface slots, so the same
                // shape drives any BCL collection (and, later, a user `seq`):
                //
                //   let e = (src).GetEnumerator()            // IEnumerable<T>::GetEnumerator → IEnumerator<T>
                //   try
                //     while e.MoveNext() do                  // IEnumerator::MoveNext
                //       let x = e.Current                    // IEnumerator<T>::get_Current
                //       body
                //   finally
                //     if e <> null then e.Dispose()          // IDisposable::Dispose
                //
                // The four member refs are minted from hand-built `SymbolKey`s against
                // the well-known interface types — the declaring type of each slot,
                // not the source's concrete type — so a `callvirt` dispatches to the
                // collection's implementation. `ExternalMemberRef` recovers the
                // instantiation (`!0` → `elemTy`) from the supplied member type. The
                // IL-IR exception region is the same `Try` / `BeginFinally` /
                // `EndFinally` shape as `TExprG.Use`'s disposal.
                let enumTy =
                    FTClass(
                        SymbolKey.TypeKey(None, "System.Collections.Generic", "IEnumerator`1"),
                        EqArray.singleton elemTy
                    )

                let geKey =
                    SymbolKey.MemberKey(
                        SymbolKey.TypeKey(None, "System.Collections.Generic", "IEnumerable`1"),
                        "GetEnumerator",
                        EqArray.empty,
                        MemberKind.Method
                    )

                let geHandle =
                    env.Provider.ExternalMemberRef(
                        geKey,
                        false,
                        false,
                        FTFun(FTConst(BuiltinTypes.intrinsicKey "unit", EqArray.empty), enumTy)
                    )

                let mnKey =
                    SymbolKey.MemberKey(
                        SymbolKey.TypeKey(None, "System.Collections", "IEnumerator"),
                        "MoveNext",
                        EqArray.empty,
                        MemberKind.Method
                    )

                let mnHandle =
                    env.Provider.ExternalMemberRef(
                        mnKey,
                        false,
                        false,
                        FTFun(
                            FTConst(BuiltinTypes.intrinsicKey "unit", EqArray.empty),
                            FTConst(BuiltinTypes.intrinsicKey "bool", EqArray.empty)
                        )
                    )

                let curKey =
                    SymbolKey.MemberKey(
                        SymbolKey.TypeKey(None, "System.Collections.Generic", "IEnumerator`1"),
                        "Current",
                        EqArray.empty,
                        MemberKind.Property
                    )

                let curHandle = env.Provider.ExternalMemberRef(curKey, true, false, elemTy)

                // The interface enumerator is always a reference `IEnumerator<'T>` and
                // always `IDisposable` — so `IsValueType = false` and `Disposable = true`
                // (the null-checked disposal `emitEnumeratorLoop` emits for a reference
                // enumerator, through the `System.IDisposable::Dispose` slot it mints).
                // That `Dispose` returns a real `void`, so the callvirt consumes only
                // the receiver — exactly what the shared emitter expects.
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
        | _ -> failwith "EmitLoops.buildForIn: unreachable"

    let buildForTo (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.ForTo(var, startExpr, endExpr, body, _, _) ->
            // `for i = a to b do body` — a unit expression. `a`/`b` are evaluated
            // once (F# semantics) into the loop-variable and a hidden limit local;
            // the loop is exited *before* the increment when `i = limit`, so the
            // final iteration runs without `i+1` overflowing (the standard F#
            // lowering — matters at `b = Int32.MaxValue`). Shape:
            //   i = a; limit = b
            //   if i > limit goto loopEnd          // empty/degenerate range
            //   loopBody: body; pop…
            //             if i = limit goto loopEnd // last iteration, no overflow
            //             i = i + 1; goto loopBody
            //   loopEnd:
            let intTy = FTConst(BuiltinTypes.intrinsicKey "int", EqArray.empty)
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
        | _ -> failwith "EmitLoops.buildForTo: unreachable"

    let buildWhile (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.While(cond, body, _, _) ->
            // `while <cond> do <body>` — a unit expression. Shape:
            //   loopStart: <cond>; brfalse loopEnd; <body>; pop…; br loopStart; loopEnd:
            // The condition leaves a `bool` the `brfalse` consumes; the body is a
            // unit statement whose value is discarded each iteration (popped back to
            // the loop-top base, as `Sequential` does). The depth tracker is reset to
            // that base before the exit label so post-loop statement discards stay
            // correct — `IlIr.analyze` re-derives the buffer's merge depths across the
            // back-edge. `while` itself leaves the single reified `unit`.
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
        | _ -> failwith "EmitLoops.buildWhile: unreachable"
