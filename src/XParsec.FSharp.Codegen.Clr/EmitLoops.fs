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
    /// (address-based dispatch, no null-check), and the optional `Dispose` handle
    /// (`ValueNone` ⇒ `E` is not `IDisposable`, so no `try`/`finally`). Both
    /// `for-in` arms — the §4.4 duck-typed walk and the §4.2 interface walk —
    /// differ only in how they *resolve* these handles, then share
    /// `emitEnumeratorLoop`.
    type private EnumeratorLoop =
        {
            ElemTy: FrozenType
            EnumeratorTy: FrozenType
            GetEnumerator: EntityHandle
            MoveNext: EntityHandle
            Current: EntityHandle
            IsValueType: bool
            Dispose: EntityHandle voption
        }

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
            if loop.IsValueType then
                b.Add(ILInstr.Ldloca enumSlot)
            else
                b.Add(ILInstr.Ldloc enumSlot)

        // Dispatch a member declared on `E` itself (`MoveNext` / `Current`): a direct
        // `call` for a struct (concrete type known), a `callvirt` for a reference.
        let callEnumMember (handle: EntityHandle) =
            if loop.IsValueType then
                b.Add(ILInstr.Call(handle, 1, 1))
            else
                b.Add(ILInstr.Callvirt(handle, 1, 1))

        recur env b source
        b.Add(ILInstr.Callvirt(loop.GetEnumerator, 1, 1))
        b.Add(ILInstr.Stloc enumSlot)

        let xSlot = b.Local loop.ElemTy
        let loopStart = b.Label()
        let loopEnd = b.Label()
        let endLabel = b.Label()

        match loop.Dispose with
        | ValueSome _ -> b.Add ILInstr.Try
        | ValueNone -> ()

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

        match loop.Dispose with
        | ValueSome dispHandle ->
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
                // Reference enumerator: the §4.2 null-checked `callvirt` disposal.
                let skipLabel = b.Label()
                b.Add(ILInstr.Ldloc enumSlot)
                b.Add(ILInstr.Brfalse skipLabel)
                b.Add(ILInstr.Ldloc enumSlot)
                b.Add(ILInstr.Callvirt(dispHandle, 1, 0))
                b.Add(ILInstr.Mark skipLabel)

            b.Add ILInstr.EndFinally
            b.SetDepth 0
            b.Add(ILInstr.Mark endLabel)
        | ValueNone ->
            // `E` is not `IDisposable` — no `try … finally` region at all (C#
            // parity); the plain `while` simply falls through.
            b.Add(ILInstr.Mark endLabel)

        // `for` is a unit expression — leave the single reified `unit` value.
        EmitTypes.buildUnitValue env b

    let buildForIn (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.ForIn(pat,
                       source,
                       body,
                       ForInEnumeratorG.DuckTyped(enumeratorTy, geKey, mnKey, curKey, isValueType, disposeOpt),
                       _) ->
            let elemTy = typeOfPat pat
            // §4.4 duck-typed / pattern-based `GetEnumerator()` — C#'s non-boxing
            // `foreach`. The source exposes a public `GetEnumerator()` returning an
            // enumerator type `E` (`List`1+Enumerator<int>`) with `MoveNext(): bool`
            // and a `Current` property, *without* implementing `IEnumerable<'T>`. The
            // loop walks `E` directly — by value with no allocation when `E` is a
            // struct (`isValueType`). Unlike the §4.2 interface path, `MoveNext` /
            // `Current` are declared on `E` itself, so their refs come from
            // `ExternalMemberRefOn` (the declaring instantiation is `enumeratorTy`,
            // not recoverable from a T-free `MoveNext(): bool`). `GetEnumerator` is on
            // the (reference) source, so its ref recovers normally (its return
            // mentions the typar). `disposeOpt` is `ValueSome` only when `E :
            // IDisposable`.
            let geHandle =
                env.Provider.ExternalMemberRef(geKey, false, false, FTFun(FTConst("unit", EqArray.empty), enumeratorTy))

            let mnHandle =
                env.Provider.ExternalMemberRefOn(
                    mnKey,
                    enumeratorTy,
                    false,
                    false,
                    FTFun(FTConst("unit", EqArray.empty), FTConst("bool", EqArray.empty))
                )

            let curHandle =
                env.Provider.ExternalMemberRefOn(curKey, enumeratorTy, true, false, elemTy)

            let dispose =
                disposeOpt
                |> ValueOption.map (fun dispKey ->
                    env.Provider.ExternalMemberRef(
                        dispKey,
                        false,
                        false,
                        FTFun(FTConst("unit", EqArray.empty), FTConst("unit", EqArray.empty))
                    )
                )

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
                    Dispose = dispose
                }
                pat
                source
                body
        | TExprG.ForIn(pat, source, body, ForInEnumeratorG.UserDuckTyped(enumeratorTy, isValueType, dispose), _) ->
            let elemTy = typeOfPat pat
            // Gap 2 pure-pattern user variant: a project-local source class with a
            // pattern `GetEnumerator()` whose enumerator `E` is itself a user class
            // exposing `MoveNext(): bool` / `Current` (no `IEnumerable<'T>`). Every
            // member lives on a user `TypeDef`, so — unlike the external §4.4 arm —
            // the three handles come from the project-local member machinery
            // (`resolveInstanceMember`, which routes a generic receiver through a
            // `UserGenericMemberRef`/`TypeSpec`). When `E` is a `[<Struct>]` enumerator
            // (`isValueType`) the shared emitter walks it by address
            // (`ldloca` + `constrained. <E>`), no boxing; a reference `E` walks by
            // value.
            let geHandle, _ = resolveInstanceMember env (typeOfExpr source) "GetEnumerator"
            let mnHandle, _ = resolveInstanceMember env enumeratorTy "MoveNext"
            let curHandle, _ = resolveInstanceMember env enumeratorTy "Current"

            // When `E : IDisposable` the front end set `dispose`, so emit the
            // null-checked `finally` disposal. Rather than resolve the user's own
            // `Dispose` member (an interface-impl slot may not live in the member
            // table), mint `System.IDisposable::Dispose` exactly as the §4.2
            // interface arm does — a `callvirt` on the interface slot dispatches to
            // the user impl. For a struct `E` the shared emitter disposes via
            // `ldloca` + `constrained. <E>` (no null-check); a reference `E` takes the
            // null-checked disposal path.
            let disposeHandle =
                if dispose then
                    let dispKey =
                        SymbolKey.MemberKey(
                            SymbolKey.TypeKey(None, "System", "IDisposable"),
                            "Dispose",
                            EqArray.empty,
                            MemberKind.Method
                        )

                    ValueSome(
                        env.Provider.ExternalMemberRef(
                            dispKey,
                            false,
                            false,
                            FTFun(FTConst("unit", EqArray.empty), FTConst("unit", EqArray.empty))
                        )
                    )
                else
                    ValueNone

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
                    Dispose = disposeHandle
                }
                pat
                source
                body
        | TExprG.ForIn(pat, source, body, _, _) ->
            let elemTy = typeOfPat pat
            // `for x in src do body` over an `IEnumerable<'T>` (B-6). Lowered to the standard enumerator
            // loop through the *interface* slots, so the same shape drives any BCL
            // collection (and, later, a user `seq`):
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
            // the well-known interface types — the *declaring* type of each slot,
            // not the source's concrete type — so a `callvirt` dispatches to the
            // collection's implementation. `ExternalMemberRef` recovers the
            // instantiation (`!0` → `elemTy`) from the supplied member type. The
            // IL-IR exception region (H5) is the same `Try` / `BeginFinally` /
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
                env.Provider.ExternalMemberRef(geKey, false, false, FTFun(FTConst("unit", EqArray.empty), enumTy))

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
                    FTFun(FTConst("unit", EqArray.empty), FTConst("bool", EqArray.empty))
                )

            let curKey =
                SymbolKey.MemberKey(
                    SymbolKey.TypeKey(None, "System.Collections.Generic", "IEnumerator`1"),
                    "Current",
                    EqArray.empty,
                    MemberKind.Property
                )

            let curHandle = env.Provider.ExternalMemberRef(curKey, true, false, elemTy)

            let dispKey =
                SymbolKey.MemberKey(
                    SymbolKey.TypeKey(None, "System", "IDisposable"),
                    "Dispose",
                    EqArray.empty,
                    MemberKind.Method
                )

            let dispHandle =
                env.Provider.ExternalMemberRef(
                    dispKey,
                    false,
                    false,
                    FTFun(FTConst("unit", EqArray.empty), FTConst("unit", EqArray.empty))
                )

            // The interface enumerator is always a reference `IEnumerator<'T>` and
            // always `IDisposable` — so `IsValueType = false` and `Dispose` is always
            // present (the null-checked disposal `emitEnumeratorLoop` emits for a
            // reference enumerator). `Dispose` here returns a real `void` (the §4.2
            // void-return fix encoded in `ClrExternalMembers`), so the callvirt
            // consumes only the receiver — exactly what the shared emitter expects.
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
                    Dispose = ValueSome dispHandle
                }
                pat
                source
                body
        | _ -> failwith "EmitLoops.buildForIn: unreachable"

    let buildForTo (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.ForTo(var, startExpr, endExpr, body, _) ->
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
            let intTy = FTConst("int", EqArray.empty)
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
        | TExprG.While(cond, body, _) ->
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
