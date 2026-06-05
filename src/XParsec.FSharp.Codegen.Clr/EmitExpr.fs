namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis
open EmitTypes
open EmitLower
open EmitResolve

module EmitExpr =
    /// Load a variable for the current method: a method parameter (`ldarg.i`),
    /// the recursive self of a closure (`this`, `ldarg.0`), a capture
    /// (`ldarg.0; ldfld`), or a local slot (`ldloc`).
    let private buildVarLoad (env: EmitEnv) (b: IlBuilder) (key: NodeKey) : unit =
        match env.Args.TryGetValue key with
        | true, i -> b.Add(ILInstr.Ldarg i)
        | false, _ ->

            match env.SelfKey with
            | ValueSome s when s = key -> b.Add(ILInstr.Ldarg 0) // `this` — the recursive self
            | _ ->
                match env.CaptureFields.TryGetValue key with
                | true, field ->
                    b.Add(ILInstr.Ldarg 0)
                    b.Add(ILInstr.Ldfld field)
                | false, _ ->
                    match env.Slots.TryGetValue key with
                    | true, slot -> b.Add(ILInstr.Ldloc slot)
                    | false, _ -> failwithf "Emit: no binding for variable %O" key

    /// Test a pattern against the value already stored in local `scrutSlot`:
    /// branch to `nextLabel` on mismatch, and bind any pattern variables. A
    /// `Const` compares (`bne.un` skips the arm); `Wildcard` / `NamedSimple`
    /// always match (the latter aliases its binding to `scrutSlot`, so
    /// `emitVarLoad` resolves it to the same local — no copy). Union / tuple /
    /// record patterns land in later rung-2 slices.
    let rec private buildMatchTest
        (env: EmitEnv)
        (b: IlBuilder)
        (scrutSlot: int)
        (nextLabel: int)
        (pat: Frozen.TPat)
        : unit =
        // `ldfld` a field of the scrutinee into a fresh local, then test its
        // sub-pattern against that local (a named sub-pattern just aliases it).
        let extractField (fieldRef: EntityHandle) (subPat: Frozen.TPat) =
            let fldSlot = b.Local(typeOfPat subPat)
            b.Add(ILInstr.Ldloc scrutSlot)
            b.Add(ILInstr.Ldfld fieldRef)
            b.Add(ILInstr.Stloc fldSlot)
            buildMatchTest env b fldSlot nextLabel subPat

        match pat with
        | TPatG.Wildcard _ -> ()
        | TPatG.NamedSimple(binding, _) -> env.Slots.[binding] <- scrutSlot
        | TPatG.Const(value, _) ->
            b.Add(ILInstr.Ldloc scrutSlot)

            match value with
            | TConstValue.Int n -> b.Add(ILInstr.LdcI4 n)
            | TConstValue.Bool v -> b.Add(ILInstr.LdcI4(if v then 1 else 0))
            | TConstValue.Byte n -> b.Add(ILInstr.LdcI4(int n))
            | TConstValue.Char c -> b.Add(ILInstr.LdcI4(int c))
            | other -> failwithf "Emit: match on constant %A is out of scope" other

            b.Add(ILInstr.BneUn nextLabel)
        | TPatG.Union(caseName, subPats, ty) ->
            // Local union table keys by the nominal `SymbolKey`; the external union
            // provider lookups take the qualified compiled name derived from it (Phase 6D).
            let key, tyArgs = nominalShape "union pattern" ty
            let qualName = SymbolKeyOps.qualifiedName key

            // The discriminator field + its value for this case, and a per-index
            // field-ref source, resolved from either the local emitted union or a
            // *referenced-package* one (`match o with Some x -> …`)
            // The emit sequence below is identical for both — only the
            // handle source differs (local `Def`/`MemberRef` tokens vs the provider's
            // refs minted off the external union shape, which keeps the field names +
            // declaration-order tagging in lockstep with the union emitter).
            let tagRef, tagValue, fieldRef =
                match env.Unions.TryGetValue key with
                | true, u ->
                    let c = u.Cases.[caseName]

                    // Tag / field access is a `Def` token for a monomorphic union, but
                    // a `MemberRef` on the instantiated `TypeSpec` for a generic one
                    // (`List<int>::_tag` etc.) — see `EmittedUnion.Typars`
                    let tagRef =
                        memberRef env u.Typars key tyArgs (UserMemberKind.UnionMember UnionMember.Tag) u.TagField

                    let fieldRef i =
                        memberRef
                            env
                            u.Typars
                            key
                            tyArgs
                            (UserMemberKind.UnionMember(UnionMember.Field(caseName, i)))
                            c.Fields.[i]

                    tagRef, c.Tag, fieldRef
                | false, _ ->
                    match env.Provider.ExternalUnionTag(qualName, tyArgs, caseName) with
                    | ValueSome(tagRef, tagValue) ->
                        let fieldRef i =
                            match env.Provider.ExternalUnionCaseField(qualName, tyArgs, caseName, i) with
                            | ValueSome(fieldRef, _) -> fieldRef
                            | ValueNone ->
                                failwithf "Emit: external union '%s' case '%s' has no field %d" qualName caseName i

                        tagRef, tagValue, fieldRef
                    | ValueNone -> failwithf "Emit: no emitted union for match on '%s'" qualName

            // Skip the arm unless `scrut._tag = case.Tag`.
            b.Add(ILInstr.Ldloc scrutSlot)
            b.Add(ILInstr.Ldfld tagRef)
            b.Add(ILInstr.LdcI4 tagValue)
            b.Add(ILInstr.BneUn nextLabel)

            subPats
            |> EqArray.iteri (fun i subPat ->
                match subPat with
                | TPatG.Wildcard _ -> ()
                | _ -> extractField (fieldRef i) subPat
            )
        | TPatG.Record(fields, ty) ->
            // A record pattern never fails on shape (no tag to compare): for each
            // named sub-pattern, `ldfld` the field into a fresh local and recurse
            // — only the sub-patterns themselves can branch to `nextLabel`. A
            // wildcard sub-pattern is skipped (it would always match), exactly
            // like the union arm above.
            let key, tyArgs = nominalShape "record pattern" ty

            match env.Records.TryGetValue key with
            | true, r ->
                for (fieldName, subPat) in fields do
                    match subPat with
                    | TPatG.Wildcard _ -> ()
                    | _ ->
                        match r.Fields |> List.tryFind (fun (n, _, _) -> n = fieldName) with
                        | Some(_, handle, _) ->
                            let fieldRef =
                                memberRef
                                    env
                                    r.Typars
                                    key
                                    tyArgs
                                    (UserMemberKind.RecordMember(RecordMember.Field fieldName))
                                    handle

                            extractField fieldRef subPat
                        | None -> failwithf "Emit: record '%A' has no field '%s'" key fieldName
            | false, _ -> failwithf "Emit: no emitted record for pattern on '%A'" key
        | other -> failwithf "Emit: match pattern is out of scope: %A" other

    /// The fallthrough a `match` reaches when no arm matched — `throw new
    /// System.Exception("…")`. An exhaustive match never reaches it at runtime,
    /// but it keeps the emitted IL well-formed (and gives a non-exhaustive one
    /// defined behaviour).
    let private buildMatchFailure (env: EmitEnv) (b: IlBuilder) : unit =
        b.Add(ILInstr.Ldstr(env.Ctx.UserString "The match cases were incomplete"))
        b.Add(ILInstr.Newobj(env.Provider.ExceptionCtor, 1))
        b.Add ILInstr.Throw

    /// The cold printf path (`printfn "%A"` …). Its `PrintFormatLine` recipe leaves
    /// an FSharp.Core `FSharpFunc` printer on the stack, applied via
    /// `FSharpFunc::Invoke` rather than `Vesper.Fun::Invoke`. Identity is keyed on
    /// the resolved `SymbolKey` (stamped by `Resolution.ExternalValue`), so a user
    /// `module MyMod = let printfn x = x` (project-local key) is correctly *not*
    /// treated as printf. The name fallback only fires for bare `"printfn"` from
    /// unkeyed call sites (test mocks / pre-key-pipeline paths).
    let private isColdPrintf (key: SymbolKey voption) (name: string) : bool =
        match key with
        | ValueSome k when PrintfSpec.isCanonicalPrintfn k -> true
        | _ -> name = "printfn"

    /// Whether a (zonked) `FrozenType` is a CLR value type — drives the box vs
    /// no-op choice on `:>` and the `unbox.any` vs `castclass` choice on `:?>`.
    /// User records / unions / classes are reference types (rung 2); the BCL
    /// primitives bound as `TyConst` are value types. `string` / `obj` are
    /// reference types despite being `TyConst`.
    let private isValueType (ty: FrozenType) : bool =
        match ty with
        | FTConst(n, _) ->
            match n with
            | "int"
            | "int64"
            | "int16"
            | "byte"
            | "sbyte"
            | "uint16"
            | "uint32"
            | "uint64"
            | "nativeint"
            | "unativeint"
            | "float"
            | "float32"
            | "single"
            | "double"
            | "bool"
            | "char"
            | "decimal" -> true
            | _ -> false
        | _ -> false

    let rec buildExpr (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.Const(TConstValue.String s, _) -> b.Add(ILInstr.Ldstr(env.Ctx.UserString s))
        | TExprG.Const(TConstValue.Int n, _) -> b.Add(ILInstr.LdcI4 n)
        | TExprG.Const(TConstValue.Int64 n, _) -> b.Add(ILInstr.LdcI8 n)
        | TExprG.Const(TConstValue.Bool v, _) -> b.Add(ILInstr.LdcI4(if v then 1 else 0))
        | TExprG.Const(TConstValue.Byte n, _) -> b.Add(ILInstr.LdcI4(int n))
        | TExprG.Const(TConstValue.Float x, _) -> b.Add(ILInstr.LdcR8 x)
        | TExprG.Const(TConstValue.Float32 x, _) -> b.Add(ILInstr.LdcR4 x)
        | TExprG.Const(TConstValue.Char c, _) -> b.Add(ILInstr.LdcI4(int c))
        | TExprG.Const(TConstValue.Decimal d, _) ->
            // Materialise via `Decimal..ctor(lo, mid, hi, isNegative, scale)` from
            // the value's bit representation — the same shape F#/Roslyn emit.
            let bits = System.Decimal.GetBits d
            let flags = bits.[3]
            b.Add(ILInstr.LdcI4 bits.[0]) // lo
            b.Add(ILInstr.LdcI4 bits.[1]) // mid
            b.Add(ILInstr.LdcI4 bits.[2]) // hi
            b.Add(ILInstr.LdcI4(if flags < 0 then 1 else 0)) // sign (high bit of flags)
            b.Add(ILInstr.LdcI4((flags >>> 16) &&& 0xFF)) // scale
            b.Add(ILInstr.Newobj(env.Provider.DecimalCtor, 5))
        | TExprG.Const(TConstValue.Unit, _) ->
            // `()` literal — reify the `unit` value (a zero-field `System.ValueTuple`
            // struct, not FSharp.Core's null `Unit`). Pushed when a closure
            // invocation needs a unit arg (`c ()`) or a unit value is otherwise
            // reified — F3 (Phase 2 §1 mkCounter pattern).
            EmitTypes.buildUnitValue env b

        | TExprG.Var(binding, _) -> buildVarLoad env b binding

        | TExprG.Let(TPatG.NamedSimple(binding, ty), value, body, _) ->
            let slot = b.Local ty
            env.Slots.[binding] <- slot
            buildExpr env b value
            b.Add(ILInstr.Stloc slot)
            buildExpr env b body
        | TExprG.Let(pat, _, _, _) -> failwithf "Emit: destructuring let-binding is out of scope: %A" pat

        | TExprG.Use(TPatG.NamedSimple(binding, varTy), value, body, dispose, _) ->
            // `use x = value in body` → `let x = value in try body finally if x <> null
            // then x.Dispose()` (B-5, vesper-set-sprint-phase-4 §4.1). The IL-IR
            // exception-region pseudo-marks (`Try` / `BeginFinally` / `EndFinally`,
            // H5) carry the region; `IlIr.lower` replays them into a proper
            // `try`/`finally`.
            //
            // A protected region can't carry an evaluation-stack value across its
            // `leave`, so the body's result is parked in a local inside the `try`
            // and reloaded after the finally as the expression's value (works for a
            // unit body too — `Unit` is `null`, parked and reloaded like any value).
            // The disposal is guarded by a null check so a null binder is a no-op
            // like F#'s `use`. `dispose` selects the path (§4.3): `ValueNone` is the
            // duck-typed direct `x.Dispose()` call on a project-local binder (no
            // `IDisposable` upcast, §4.1); `ValueSome key` disposes an external (BCL)
            // binder through the keyed `Dispose` member the front end resolved.
            //
            // `use` is a statement-position binding, so the surrounding stack is
            // empty here: the region opens at depth 0 and the final `ldloc` leaves
            // exactly the one result value.
            //
            // Both disposal paths below guard the call with a reference null check
            // (`ldloc; brfalse`) and dispatch via `callvirt` — correct only for a
            // *reference* binder. A value-type binder would need no null check (a
            // struct can't be null) and an address-based `ldloca` + `call` /
            // `constrained. callvirt` to avoid boxing the receiver; emitting `brfalse`
            // on a loaded struct is in fact invalid IL. Vesper has no value types
            // yet, so this can't be reached today — but fail fast rather than emit
            // bad IL if a primitive-typed binder ever slips through. A BCL *struct*
            // disposable (e.g. a struct enumerator) reads as `TyClass` and is
            // indistinguishable from a class here; supporting it is Step 4.4's job
            // (it must record struct-ness on the node — the provider's
            // `ExternalClassShape` doesn't surface it).
            if isValueType varTy then
                failwithf
                    "Emit: `use` over a value-type binder is out of scope (vesper-set-sprint-phase-4 §4.4): %A"
                    varTy

            let slot = b.Local varTy
            env.Slots.[binding] <- slot
            buildExpr env b value
            b.Add(ILInstr.Stloc slot)

            let resultSlot = b.Local(typeOfExpr body)
            let endLabel = b.Label()
            let skipLabel = b.Label()

            b.Add ILInstr.Try
            buildExpr env b body
            b.Add(ILInstr.Stloc resultSlot)
            b.Add(ILInstr.Leave endLabel)

            b.Add ILInstr.BeginFinally
            b.SetDepth 0
            b.Add(ILInstr.Ldloc slot)
            b.Add(ILInstr.Brfalse skipLabel)

            match dispose with
            | ValueNone ->
                // Reuse the standard instance-call path for `x.Dispose()`: it resolves
                // the member handle and emits the `callvirt`. `Dispose` returns unit
                // (one `Unit` value), popped so the finally handler ends empty-stacked.
                let disposeKey =
                    SymbolKey.MemberKey(
                        fst (nominalShape "use-dispose receiver" varTy),
                        "Dispose",
                        EqArray.empty,
                        MemberKind.Method
                    )

                buildExpr
                    env
                    b
                    (TExprG.MethodCall(
                        TExprG.Var(binding, varTy),
                        disposeKey,
                        CallVia.Self,
                        EqArray.empty,
                        FTConst("unit", EqArray.empty)
                    ))

                b.Add ILInstr.Pop
            | ValueSome key ->
                // External (BCL) binder (§4.3): dispose through the keyed `Dispose`
                // the front end resolved (the type's own `Dispose`, or
                // `System.IDisposable`'s), minted as an `ExternalMemberRef` `callvirt`
                // — the same machinery the §4.2 for-in disposal uses. The external
                // member carries a real `void` return (the §4.2 fix), so it pushes
                // nothing: a receiver-only `callvirt`, no `pop`.
                let dispHandle =
                    env.Provider.ExternalMemberRef(
                        key,
                        false,
                        false,
                        FTFun(FTConst("unit", EqArray.empty), FTConst("unit", EqArray.empty))
                    )

                b.Add(ILInstr.Ldloc slot)
                b.Add(ILInstr.Callvirt(dispHandle, 1, 0))

            b.SetDepth 0
            b.Add(ILInstr.Mark skipLabel)
            b.Add ILInstr.EndFinally

            b.SetDepth 0
            b.Add(ILInstr.Mark endLabel)
            b.Add(ILInstr.Ldloc resultSlot)
        | TExprG.Use(pat, _, _, _, _) -> failwithf "Emit: destructuring use-binding is out of scope: %A" pat

        | TExprG.ForIn(TPatG.NamedSimple(binding, elemTy),
                       source,
                       body,
                       ForInEnumeratorG.DuckTyped(enumeratorTy, geKey, mnKey, curKey, isValueType, disposeOpt),
                       _) ->
            // §4.4 duck-typed / pattern-based `GetEnumerator()` — C#'s non-boxing
            // `foreach`. The source exposes a public `GetEnumerator()` returning an
            // enumerator type `E` (`List`1+Enumerator<int>`) with `MoveNext(): bool`
            // and a `Current` property, *without* implementing `IEnumerable<'T>`. The
            // loop walks `E` directly — by value with no allocation when `E` is a
            // struct (`isValueType`):
            //
            //   let e = (src).GetEnumerator()
            //   try                                  // only when E : IDisposable
            //     while e.MoveNext() do e.Current → body
            //   finally e.Dispose()                  // only when E : IDisposable
            //
            // Unlike the §4.2 interface path, `MoveNext` / `Current` are declared on
            // `E` itself, so their refs come from `ExternalMemberRefOn` (the
            // declaring instantiation is `enumeratorTy`, not recoverable from a
            // T-free `MoveNext(): bool`). A struct `E` dispatches via `ldloca` +
            // `constrained. <E>` callvirt (no box, no null-check — a struct value is
            // never null); a reference `E` uses `ldloc` + `callvirt` with the
            // §4.2-style null-checked `Dispose`. `GetEnumerator` is on the (reference)
            // source, so its ref recovers normally (its return mentions the typar).
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

            // The `constrained.` token for a struct enumerator — `TypeToken` routes
            // through the (now value-type-aware) encoder, so `E` lands as a value type.
            let constrainedTok =
                if isValueType then
                    ValueSome(env.Provider.TypeToken enumeratorTy)
                else
                    ValueNone

            let enumSlot = b.Local enumeratorTy
            buildExpr env b source
            b.Add(ILInstr.Callvirt(geHandle, 1, 1))
            b.Add(ILInstr.Stloc enumSlot)

            let xSlot = b.Local elemTy
            env.Slots.[binding] <- xSlot

            // Load the enumerator as the receiver for a member call: a struct by
            // address (+ `constrained.`), a reference by value.
            let loadEnumReceiver () =
                if isValueType then
                    b.Add(ILInstr.Ldloca enumSlot)

                    match constrainedTok with
                    | ValueSome t -> b.Add(ILInstr.Constrained t)
                    | ValueNone -> ()
                else
                    b.Add(ILInstr.Ldloc enumSlot)

            let loopStart = b.Label()
            let loopEnd = b.Label()
            let endLabel = b.Label()

            match disposeOpt with
            | ValueSome _ -> b.Add ILInstr.Try
            | ValueNone -> ()

            b.Add(ILInstr.Mark loopStart)
            loadEnumReceiver ()
            b.Add(ILInstr.Callvirt(mnHandle, 1, 1))
            b.Add(ILInstr.Brfalse loopEnd)
            // `x = e.Current`, then the unit-typed body whose value is discarded.
            loadEnumReceiver ()
            b.Add(ILInstr.Callvirt(curHandle, 1, 1))
            b.Add(ILInstr.Stloc xSlot)
            buildExpr env b body
            b.Add ILInstr.Pop
            b.Add(ILInstr.Br loopStart)
            b.Add(ILInstr.Mark loopEnd)

            match disposeOpt with
            | ValueSome dispKey ->
                let dispHandle =
                    env.Provider.ExternalMemberRef(
                        dispKey,
                        false,
                        false,
                        FTFun(FTConst("unit", EqArray.empty), FTConst("unit", EqArray.empty))
                    )

                b.Add(ILInstr.Leave endLabel)
                b.Add ILInstr.BeginFinally
                b.SetDepth 0

                if isValueType then
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
        | TExprG.ForIn(TPatG.NamedSimple(binding, elemTy), source, body, _, _) ->
            // `for x in src do body` over an `IEnumerable<'T>` (B-6,
            // vesper-set-sprint-phase-4 §4.2). Lowered to the standard enumerator
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

            // `e = src.GetEnumerator()` — at statement position, so the stack is
            // empty here and the enumerator local is the only live value.
            let enumSlot = b.Local enumTy
            buildExpr env b source
            b.Add(ILInstr.Callvirt(geHandle, 1, 1))
            b.Add(ILInstr.Stloc enumSlot)

            let xSlot = b.Local elemTy
            env.Slots.[binding] <- xSlot

            let loopStart = b.Label()
            let loopEnd = b.Label()
            let skipLabel = b.Label()
            let endLabel = b.Label()

            b.Add ILInstr.Try
            b.Add(ILInstr.Mark loopStart)
            b.Add(ILInstr.Ldloc enumSlot)
            b.Add(ILInstr.Callvirt(mnHandle, 1, 1))
            b.Add(ILInstr.Brfalse loopEnd)
            // `x = e.Current`, then the unit-typed body whose value is discarded.
            b.Add(ILInstr.Ldloc enumSlot)
            b.Add(ILInstr.Callvirt(curHandle, 1, 1))
            b.Add(ILInstr.Stloc xSlot)
            buildExpr env b body
            b.Add ILInstr.Pop
            b.Add(ILInstr.Br loopStart)
            b.Add(ILInstr.Mark loopEnd)
            b.Add(ILInstr.Leave endLabel)

            b.Add ILInstr.BeginFinally
            b.SetDepth 0
            b.Add(ILInstr.Ldloc enumSlot)
            b.Add(ILInstr.Brfalse skipLabel)
            b.Add(ILInstr.Ldloc enumSlot)
            // `IDisposable.Dispose()` returns `void` (pushes nothing — the
            // external void return encoded in `ClrExternalMembers`), so the
            // `callvirt` consumes only the receiver and leaves the finally
            // handler empty-stacked; no `pop` of a phantom result.
            b.Add(ILInstr.Callvirt(dispHandle, 1, 0))
            b.Add(ILInstr.Mark skipLabel)
            b.Add ILInstr.EndFinally

            b.SetDepth 0
            b.Add(ILInstr.Mark endLabel)
            // `for` is a unit expression — leave the single reified `unit` value.
            EmitTypes.buildUnitValue env b
        | TExprG.ForIn(pat, _, _, _, _) -> failwithf "Emit: destructuring for-in binding is out of scope: %A" pat

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

            buildExpr env b startExpr
            b.Add(ILInstr.Stloc iSlot)
            buildExpr env b endExpr
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
            buildExpr env b body

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

        | TExprG.Sequential(items, _) ->
            // Every item but the last is a unit-typed statement: emit it and
            // discard whatever value it leaves (popping back to the pre-item
            // depth); the last item leaves the sequence's result.
            let n = items.Length

            items
            |> EqArray.iteri (fun i it ->
                if i = n - 1 then
                    buildExpr env b it
                else
                    let baseDepth = b.Depth
                    buildExpr env b it

                    while b.Depth > baseDepth do
                        b.Add ILInstr.Pop
            )

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
            buildExpr env b cond
            b.Add(ILInstr.Brfalse loopEnd)
            buildExpr env b body

            while b.Depth > baseDepth do
                b.Add ILInstr.Pop

            b.Add(ILInstr.Br loopStart)
            b.SetDepth baseDepth
            b.Add(ILInstr.Mark loopEnd)
            EmitTypes.buildUnitValue env b

        | TExprG.IfThenElse(cond, thenExpr, elseExpr, _) ->
            // `<cond>; brfalse else; <then>; br end; else: <else>; end:`. Both
            // arms leave one value; the builder's linear depth tracker (which
            // follows only the then-arm) is reset to the post-`brfalse` base
            // before the else-arm so subsequent statement-discards stay correct —
            // the *buffer's* merge depths are re-derived by `IlIr.analyze`.
            let elseLabel = b.Label()
            let endLabel = b.Label()
            buildExpr env b cond
            b.Add(ILInstr.Brfalse elseLabel)
            let baseDepth = b.Depth
            buildExpr env b thenExpr
            b.Add(ILInstr.Br endLabel)
            b.SetDepth baseDepth
            b.Add(ILInstr.Mark elseLabel)
            buildExpr env b elseExpr
            b.Add(ILInstr.Mark endLabel)

        | TExprG.Match(scrutinee, arms, _) ->
            // Evaluate the scrutinee once into a local, then test each arm in
            // order: on a mismatch branch to the next arm; on a match (and a
            // passing guard) emit the body and branch to the shared end. The
            // builder's depth tracker is reset to the post-scrutinee base before
            // each arm and before the end label (every body leaves one result);
            // `IlIr.analyze` re-derives the buffer's merge depths.
            let scrutSlot = b.Local(typeOfExpr scrutinee)
            buildExpr env b scrutinee
            b.Add(ILInstr.Stloc scrutSlot)
            let baseDepth = b.Depth
            let endLabel = b.Label()

            for arm in arms do
                let nextLabel = b.Label()
                buildMatchTest env b scrutSlot nextLabel arm.Pat

                match arm.Guard with
                | Some g ->
                    buildExpr env b g
                    b.Add(ILInstr.Brfalse nextLabel)
                | None -> ()

                buildExpr env b arm.Body
                b.Add(ILInstr.Br endLabel)
                b.SetDepth baseDepth
                b.Add(ILInstr.Mark nextLabel)

            buildMatchFailure env b
            b.SetDepth(baseDepth + 1)
            b.Add(ILInstr.Mark endLabel)

        | TExprG.Lambda _ ->
            // A function value: construct its closure. Captures are pushed via
            // the *current* resolver (a local in `Main`, the param or a capture
            // inside an enclosing closure), then `newobj` its ctor.
            //
            // A *generic* closure (function-representation-plan §Generic closures, C3) routes the `Newobj`
            // through a `MemberRef` on `<closure>$n<args>`, where `args` is the
            // closure's typars zonked at the call site (`!!i` inside the
            // enclosing static method's body, `!i` inside an enclosing closure's
            // `Invoke`) — both encodings reference the same TypeVar roots, and
            // the parent's `TypeSpec` captures the use-site instantiation.
            match env.ClosureByNode.TryGetValue e with
            | true, closure ->
                for (k, _) in closure.Captures do
                    buildVarLoad env b k

                let ctorHandle =
                    if closure.Typars = 0 then
                        match env.CtorHandleByNode.TryGetValue e with
                        | true, ctor -> ctor
                        | false, _ ->
                            failwith "Emit: closure constructor not yet emitted (leaves-first ordering broken)"
                    else
                        // The closure's self-instantiation over its own typars: the
                        // enclosing method's `FTTypar(Method, i)`, encoded under the
                        // ambient closure mode at this construction site (`!!i` in a
                        // static-method body, `!i` inside an enclosing closure).
                        env.Provider.UserClosureMemberRef(
                            closure.Name,
                            [ for i in 0 .. closure.Typars - 1 -> FTTypar(TyparAxis.Method, i) ],
                            ClosureMember.Ctor
                        )

                b.Add(ILInstr.Newobj(ctorHandle, List.length closure.Captures))
            | false, _ -> failwith "Emit: a Lambda value was not discovered as a closure"

        | TExprG.New(className, args, ty) ->
            for a in args do
                buildExpr env b a

            let tyArgs =
                match ty with
                | FTClass(_, xs) -> EqArray.toList xs
                | _ -> []

            // Call-site arg types let the external-ctor path disambiguate ctor
            // overloads (v1 picker is arity-only — see `ClrProvider.externalCtor`).
            let argTypes = [ for a in args -> typeOfExpr a ]

            // A project-local class is identified by the nominal `SymbolKey` on the
            // construction's `TyClass` result type; an external ctor (no `TyClass`
            // result, or a key not in `env.Classes`) routes through the provider by
            // `className` (Phase 6D).
            let localClass =
                match ty with
                | FTClass(k, _) ->
                    match env.Classes.TryGetValue k with
                    | true, c -> ValueSome(k, c)
                    | _ -> ValueNone
                | _ -> ValueNone

            match localClass with
            | ValueSome(classKey, c) ->
                // A user class emitted into this assembly (vesper-set-sprint-plan
                // Phase 1 / B-1). The primary ctor's arity equals its field count;
                // a different arg count selects a secondary ctor (B-11) by arity —
                // F# forbids two ctors of the same signature, so arity is a key.
                let argCount = args.Length

                if argCount = List.length c.Fields then
                    // Primary. Monomorphic: the ctor's `Def` token directly.
                    // Generic: a `MemberRef` on the receiver's instantiated
                    // `TypeSpec` (`Box<int>::.ctor`), as the generic-record path.
                    let ctorRef =
                        memberRef env c.Typars classKey tyArgs (UserMemberKind.ClassMember ClassMember.Ctor) c.Ctor

                    b.Add(ILInstr.Newobj(ctorRef, argCount))
                else
                    match c.SecondaryCtors |> List.tryFind (fun (a, _) -> a = argCount) with
                    | Some(_, h) when List.isEmpty c.Typars -> b.Add(ILInstr.Newobj(h, argCount))
                    | Some _ ->
                        // Generic secondary-ctor *call sites* need a `MemberRef` on
                        // the instantiated `TypeSpec`; the secondary-ctor *bodies*
                        // already emit. Deferred until a `ClassMember.SecondaryCtor`
                        // ref variant lands.
                        failwithf "Emit: generic secondary-constructor call sites not yet supported ('%s')" className
                    | None -> failwithf "Emit: no constructor of arity %d on class '%s'" argCount className
            | ValueNone ->
                match env.Provider.TryEmitCtor(className, tyArgs, argTypes) with
                | ValueSome recipe -> b.Add(ILInstr.Newobj(recipe.Handle, recipe.ArgCount))
                | ValueNone -> failwithf "Emit: no constructor recipe for '%s'" className

        | TExprG.App _ -> buildAppCall env b e

        | TExprG.RecordCons(srcFields, ty) ->
            // The source-order initialiser list (`{ Y = …; X = … }`) is reordered
            // to the type's *declaration* order before the ctor is invoked:
            // the ctor's parameter slots correspond to
            // declaration order so the field-store sequence in `buildRecordCtor`
            // lines up. A generic record's `.ctor` is a `MemberRef` on its own
            // `TypeSpec` (`Box\`1<!0>::.ctor`), exactly like a generic union's
            // factory.
            let key, tyArgs = nominalShape "RecordCons" ty

            match env.Records.TryGetValue key with
            | true, r ->
                let srcMap = Map.ofSeq srcFields.Underlying

                for (fieldName, _, _) in r.Fields do
                    match Map.tryFind fieldName srcMap with
                    | Some e -> buildExpr env b e
                    | None ->
                        failwithf "Emit: record literal for '%A' is missing initialiser for field '%s'" key fieldName

                let ctor =
                    memberRef env r.Typars key tyArgs (UserMemberKind.RecordMember RecordMember.Ctor) r.Ctor

                b.Add(ILInstr.Newobj(ctor, List.length r.Fields))
            | false, _ ->
                let qualName = SymbolKeyOps.qualifiedName key
                // Records-handoff Phase 2 follow-up F2: the record lives in a
                // referenced assembly (`Vesper.Ref\`1` in `Vesper.Core.dll`,
                // routed here from `RefCellPromotion`). The provider mints a
                // `MemberRef` on its instantiated `TypeSpec`; field arguments are
                // pushed in source order (the contract layer's field order is
                // also the declaration order, which matches the ctor's parameter
                // layout, so no reorder is required for the supported one-field
                // `Ref<'T>` shape — multi-field external records will revisit).
                let fieldNames = [ for (n, _) in srcFields -> n ]

                match env.Provider.TryEmitRecordCons(qualName, tyArgs, fieldNames) with
                | ValueSome recipe ->
                    for (_, e) in srcFields do
                        buildExpr env b e

                    b.Add(ILInstr.Newobj(recipe.Handle, recipe.ArgCount))
                | ValueNone -> failwithf "Emit: no emitted record for '%s'" qualName

        | TExprG.FieldGet(receiver, name, _) ->
            // `r.X` — load the receiver and `ldfld` the field. The field handle is
            // a `Def` token for a monomorphic record, a `MemberRef` on the receiver's
            // `TypeSpec` for a generic one (`resolveRecordField`). A
            // referenced-assembly record (F2) routes through the provider.
            let handle = resolveRecordField env (typeOfExpr receiver) name
            buildExpr env b receiver
            b.Add(ILInstr.Ldfld handle)

        | TExprG.Assignment(TExprG.Var(binding, _), value, _) ->
            // `x <- v` on a non-promoted `mutable` local — store into its slot.
            // (A `HeapShared` mutable local was already rewritten by
            // `RefCellPromotion` into a `contents` FieldSet, so any `Assignment`
            // surviving to codegen targets a plain stack local.) Unit-typed, so
            // reify `unit` for the consumer — same convention as `FieldSet`.
            match env.Slots.TryGetValue binding with
            | true, slot ->
                buildExpr env b value
                b.Add(ILInstr.Stloc slot)
                EmitTypes.buildUnitValue env b
            | false, _ -> failwithf "Emit: assignment to a variable with no local slot: %O" binding

        | TExprG.FieldSet(receiver, name, value, _) ->
            // `r.X <- v` on a `mutable` field. Validation has rejected the
            // immutable case before we reach here. `stfld` consumes both pushes
            // and leaves nothing on the stack, but a `FieldSet` is *unit-typed*
            // — every consumer (`Sequential` middle items, the body of a
            // unit-returning closure / static method) expects a unit value to be
            // present. Reify the `unit` value to keep the IL verifier happy when
            // the body is just a FieldSet (`fun () -> n <- n + 1`, F3 §1).
            let handle = resolveRecordField env (typeOfExpr receiver) name
            buildExpr env b receiver
            buildExpr env b value
            b.Add(ILInstr.Stfld handle)
            EmitTypes.buildUnitValue env b

        | TExprG.RecordClone(source, overrides, ty) ->
            // `{ r with X = v; … }` — evaluate `r` into a local, then per
            // declaration-order field: push the override expression if it's in
            // the override list, else `ldloc; ldfld` from the saved source. Then
            // `newobj` the ctor. Direct field reads (no `MemberwiseClone`) keeps
            // it BCL-only and works identically for a generic record.
            let key, tyArgs = nominalShape "RecordClone" ty

            match env.Records.TryGetValue key with
            | true, r ->
                let overrideMap = Map.ofSeq overrides.Underlying
                let srcSlot = b.Local ty
                buildExpr env b source
                b.Add(ILInstr.Stloc srcSlot)

                for (fieldName, handle, _) in r.Fields do
                    match Map.tryFind fieldName overrideMap with
                    | Some e -> buildExpr env b e
                    | None ->
                        let fieldRef =
                            memberRef
                                env
                                r.Typars
                                key
                                tyArgs
                                (UserMemberKind.RecordMember(RecordMember.Field fieldName))
                                handle

                        b.Add(ILInstr.Ldloc srcSlot)
                        b.Add(ILInstr.Ldfld fieldRef)

                let ctor =
                    memberRef env r.Typars key tyArgs (UserMemberKind.RecordMember RecordMember.Ctor) r.Ctor

                b.Add(ILInstr.Newobj(ctor, List.length r.Fields))
            | false, _ -> failwithf "Emit: no emitted record for '%A'" key

        | TExprG.UnionCons(caseName, args, ty) ->
            // Local union table keys by the nominal `SymbolKey`; the provider's
            // cons recipe (FSharp.Core / Vesper list) selects on the same key.
            let key, tyArgs = nominalShape "UnionCons" ty
            let qualName = SymbolKeyOps.qualifiedName key

            for a in args do
                buildExpr env b a

            match env.Unions.TryGetValue key with
            | true, u ->
                // Our own emitted union: `call` the case's static factory (the
                // fields are already on the stack in declaration order). A
                // monomorphic factory is a `Def` token; a generic one is a
                // `MemberRef` on the instantiated `TypeSpec` (`List<int>::Cons`).
                let factoryRef =
                    memberRef
                        env
                        u.Typars
                        key
                        tyArgs
                        (UserMemberKind.UnionMember(UnionMember.Factory caseName))
                        u.Cases.[caseName].Factory

                b.Add(ILInstr.Call(factoryRef, args.Length, 1))
            | false, _ ->
                // The provider's special-case (FSharp.Core list) for `[]` / `::`.
                match env.Provider.TryEmitUnionCons(key, caseName, tyArgs) with
                | ValueSome recipe -> b.Add(ILInstr.Recipe recipe)
                | ValueNone -> failwithf "Emit: no union-cons recipe for %s.%s" qualName caseName

        | TExprG.PropertyGet(receiver, key, via, _) ->
            let name = SymbolKeyOps.simpleName key
            // Instance property read: load the receiver, then dispatch.
            // Unions/records are sealed (rung 2) so `call` is safe and avoids
            // the null check. User classes (vesper-set-sprint-plan §1.7 /
            // B-1) emit `callvirt` uniformly — non-`override` members would
            // accept `call`, but `callvirt` is the safer default per the plan.
            // `base.X` (`CallVia.Base`) is non-virtual: `receiverTy` is already
            // the parent type (so the slot resolves to the parent's getter), and
            // `call` skips virtual dispatch so an `override` doesn't recurse.
            let receiverTy = typeOfExpr receiver
            let handle = resolveInstanceMember env receiverTy name
            buildExpr env b receiver

            match via, receiverTy with
            | CallVia.Self, FTClass _ -> b.Add(ILInstr.Callvirt(handle, 1, 1))
            | _ -> b.Add(ILInstr.Call(handle, 1, 1))

        | TExprG.MethodCall(receiver, key, via, args, _) ->
            let name = SymbolKeyOps.simpleName key
            // Instance method call: receiver then args. Unions/records use
            // `call` (sealed, no virtual dispatch needed). User classes
            // (vesper-set-sprint-plan §1.7 / B-1) emit `callvirt` uniformly
            // for safety. `base.M(...)` (`CallVia.Base`) emits `call` against the
            // parent's slot (`receiverTy` is the parent type), so an `override`
            // body calling `base.M()` invokes the parent — not itself.
            let receiverTy = typeOfExpr receiver
            let handle = resolveInstanceMember env receiverTy name
            buildExpr env b receiver

            for a in args do
                buildExpr env b a

            match via, receiverTy with
            | CallVia.Self, FTClass _ -> b.Add(ILInstr.Callvirt(handle, 1 + args.Length, 1))
            | _ -> b.Add(ILInstr.Call(handle, 1 + args.Length, 1))

        | TExprG.StaticPropertyGet(key, _) ->
            let handle = resolveStaticMember env key
            b.Add(ILInstr.Call(handle, 0, 1))

        | TExprG.StaticFieldGet(declKey, name, _) ->
            let handle = resolveStaticField env declKey name
            b.Add(ILInstr.Ldsfld handle)

        | TExprG.StaticMethodCall(key, args, _) ->
            let handle = resolveStaticMember env key

            for a in args do
                buildExpr env b a

            b.Add(ILInstr.Call(handle, args.Length, 1))

        | TExprG.ExternalMember(receiver, key, _, true, ty) ->
            // A standalone external *property* get (P4): a static one (`call
            // get_<name>()`) or an instance one reached as the receiver of an outer
            // access (`<receiver>; callvirt get_<name>()`). The keyed member ref is
            // minted from the node's `SymbolKey`; an instance access on an external
            // union/record receiver goes through `ExternalMemberRefOn` (the parent +
            // arity come off the receiver type, not the bare contract name).
            match receiver with
            | ValueNone ->
                let handle = env.Provider.ExternalMemberRef(key, true, true, ty)
                b.Add(ILInstr.Call(handle, 0, 1))
            | ValueSome r ->
                let handle = externalInstanceMemberRef env key (typeOfExpr r) true (ty)
                buildExpr env b r
                b.Add(ILInstr.Callvirt(handle, 1, 1))

        | TExprG.ExternalMember(_, _, _, false, _) ->
            // An external method used as a first-class value (a method group, not
            // applied) needs closure synthesis — out of scope. Applied methods are
            // handled as an `App` head above.
            failwith "Emit: external method used as a first-class value is out of scope"

        | TExprG.Format(sink, segments, _) -> EmitFormat.buildFormat buildExpr env b sink segments

        | TExprG.ILIntrinsic("newarr", operand, args, _) ->
            // `Array.zeroCreate count` — push the count, then `newarr <elem>`.
            // The element type rides `typeOperand` (Freeze recovered it from the
            // result array type).
            for a in args do
                buildExpr env b a

            match operand with
            | ValueSome elem -> b.Add(ILInstr.Newarr(env.Provider.TypeToken elem))
            | ValueNone -> failwith "Emit: 'newarr' without an element type operand"

        | TExprG.ILIntrinsic("ldelem", operand, args, _) ->
            // `arr.[i]` — push the array then the index, then `ldelem <elem>`.
            for a in args do
                buildExpr env b a

            match operand with
            | ValueSome elem -> b.Add(ILInstr.Ldelem(env.Provider.TypeToken elem))
            | ValueNone -> failwith "Emit: 'ldelem' without an element type operand"

        | TExprG.ILIntrinsic("ldlen", _, args, _) ->
            // `arr.Length` — push the array, `ldlen` (native int), then `conv.i4`
            // to narrow to the int32 F# `.Length` returns.
            for a in args do
                buildExpr env b a

            b.Add ILInstr.Ldlen
            b.Add(ILInstr.Un ILOpCode.Conv_i4)

        | TExprG.ILIntrinsic(opCode, _, args, _) ->
            // Push each operand, then append the mapped opcode. The dispatch
            // (which opcode for which operator/primitive) lives in the operator
            // `.fs` body this node was lowered from, not here — codegen only
            // interprets the IL. See docs/operators-plan.md.
            for a in args do
                buildExpr env b a

            // `throw` is terminal — it pops the exception and ends the path,
            // so it doesn't fit `tryOpCodeOfMnemonic`'s balanced-result shape.
            // Tolerated in value position the same way a non-exhaustive `match`
            // fallthrough is (`buildMatchFailure`): `Throw` never returns, so
            // no result is left on the stack.
            if opCode = "throw" then
                if args.Length <> 1 then
                    failwithf "Emit: %d-ary inline-IL instruction 'throw' is out of scope" args.Length

                b.Add ILInstr.Throw
            else
                match Cil.tryOpCodeOfMnemonic opCode with
                | ValueSome code ->
                    match args.Length with
                    | 2 -> b.Add(ILInstr.Bin code)
                    | 1 -> b.Add(ILInstr.Un code)
                    | n -> failwithf "Emit: %d-ary inline-IL instruction '%s' is out of scope" n opCode
                | ValueNone -> failwithf "Emit: unsupported inline-IL instruction '%s'" opCode

        | TExprG.StaticOptimization(_, def, _) ->
            // Reaching codegen unresolved means the function was never
            // inline-expanded against a concrete operand type (used as a
            // first-class value, or declared without `inline`). F#'s semantics
            // fall back to the leading (dynamic) expression in that case.
            buildExpr env b def

        | TExprG.Upcast(source, _) ->
            // `e :> T`: a reference-type source is already usable as its base —
            // the JIT erases the cast, so emit nothing. A value-type source must
            // be boxed to reach `obj` / an interface.
            buildExpr env b source
            let srcTy = typeOfExpr source

            if isValueType srcTy then
                b.Add(ILInstr.Box(env.Provider.TypeToken srcTy))

        | TExprG.Downcast(source, ty) ->
            // `e :?> T`: `unbox.any` for a value-type target, `castclass` for a
            // reference-type one. Both throw `InvalidCastException` at runtime on
            // a real mismatch.
            buildExpr env b source
            let token = env.Provider.TypeToken ty

            if isValueType ty then
                b.Add(ILInstr.UnboxAny token)
            else
                b.Add(ILInstr.Castclass token)

        | TExprG.TypeTest(source, testTy, _) ->
            // `e :? T` → `isinst T; ldnull; cgt.un` — a non-null `isinst` result
            // (the value really is a `T`) compares greater-than null, yielding 1.
            buildExpr env b source
            b.Add(ILInstr.Isinst(env.Provider.TypeToken testTy))
            b.Add ILInstr.Ldnull
            b.Add(ILInstr.Bin ILOpCode.Cgt_un)

        | other -> failwithf "Emit: unsupported expression: %A" other

    /// Lower a `TExprG.App` chain. Split out of `buildExpr` so the upcoming
    /// class-spine work (B-1 `New(className, args)`, B-9 `Raise`, B-4
    /// `:>`/`:?`/`:?>`) can grow App-head shapes near here instead of inside a
    /// 600-line `buildExpr` match (vesper-set-sprint-plan §0.2 / M2). The head
    /// dispatch is shape-by-shape:
    /// - `TExprG.External(name, key, _)` — a provider-resolved call. The
    ///   recipe's generic instantiation is read from the head's full curried
    ///   type. `key` (the Freeze-stamped `SymbolKey.ValueKey`) lets codegen
    ///   route by identity, not name (Phase 0 §0.1).
    /// - `TExprG.Var k` where `env.StaticMethods.ContainsKey k` — a top-level
    ///   function emitted as a static method (P3b); generic instantiations are
    ///   recovered by matching declared param types against the actual arg
    ///   types (R3).
    /// - `TExprG.ExternalMember(receiver, key, name, false, memberTy)` — an
    ///   external method call (P4); tupled per .NET convention, so the call
    ///   consumes one spine element (the arg list) and the param count comes
    ///   from the key's `argSig` length.
    /// - otherwise — the head is itself a function value (a closure local or a
    ///   partially applied result); emit it, then `Invoke` each arg.
    and private buildAppCall (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        let head, spineArgs = TastWalk.collectSpine [] e

        match head with
        | TExprG.External(name, key, _) ->
            // The recipe reads its generic instantiation from the head's
            // full curried type (`fnTy`). `key` is the resolved
            // `SymbolKey.ValueKey` stamped by Freeze when the front-end
            // resolved the name through the symbol provider — codegen
            // routes by identity, not name suffix
            // (vesper-set-sprint-plan §0.1 / M1).
            match env.Provider.TryEmitCall(name, key, typeOfExpr head) with
            | ValueSome recipe ->
                let leading, rest = List.splitAt recipe.ArgCount spineArgs

                for (a, _) in leading do
                    buildExpr env b a

                b.Add(ILInstr.Recipe recipe)

                // Whatever the recipe left on the stack — a function value
                // the rest of the spine is applied to.
                let funcTy =
                    match List.tryLast leading with
                    | Some(_, ty) -> ty
                    | None -> typeOfExpr head

                // The cold printf printer is an FSharp.Core `FSharpFunc`, so it
                // is applied via `FSharpFunc::Invoke`; every other recipe result
                // is a native `Vesper.Fun` (R1).
                if isColdPrintf key name then
                    foldInvokeFSharpFunc env b funcTy rest
                else
                    foldInvoke env b funcTy rest
            | ValueNone -> failwithf "Emit: no call recipe for external '%s'" name

        | TExprG.Var(k, _) when env.StaticMethods.ContainsKey k ->
            // A top-level function emitted as a static method (P3b): `call`
            // it with the first `Arity` args (always present — a non-saturated
            // use would have escaped to a closure, see `collectStaticFns`),
            // then `Invoke` the result with any remainder. A *generic* static
            // method (R3) `call`s a `MethodSpec` instantiating it — recovered
            // by matching its declared parameter types against the actual
            // argument types (recursion yields the method's own typars ⇒ `!!i`).
            let sm = env.StaticMethods.[k]
            let leading, rest = List.splitAt sm.Arity spineArgs

            for (a, _) in leading do
                buildExpr env b a

            let callHandle =
                if sm.Typars = 0 then
                    sm.Handle
                else
                    // Each spine arg's *own* type (`collectSpine` pairs it with
                    // the application's *result* type instead), matched against
                    // the declared parameter types to recover the instantiation
                    // (by `FTTypar(Method, i)` index).
                    let actualTys = leading |> List.map (fun (a, _) -> typeOfExpr a)
                    let inst = matchInstantiation sm.Typars sm.ParamTys actualTys
                    env.Provider.StaticFnMethodSpec(sm.Handle, inst)

            b.Add(ILInstr.Call(callHandle, sm.Arity, 1))
            foldInvoke env b sm.ResultTy rest

        | TExprG.ExternalMember(receiver, key, name, false, memberTy) ->
            // An external instance/static *method* call (P4): push the receiver
            // (instance only) beneath the arguments, then `call` (static) /
            // `callvirt` (instance) the keyed member ref. A .NET method is
            // **tupled** (`m(a, b)` = one application to `(a, b)`), so the call
            // consumes a single spine element — the argument list — and the
            // parameter count comes from the chosen key's `argSig` length
            // (authoritative: `memberTy` alone can't tell a flattened 2-param
            // method from a genuine single `(int*int)` param). A literal
            // `TExprG.Tuple` argument is pushed element-wise
            // (no tuple object is constructed).
            let isStatic = ValueOption.isNone receiver

            let argCount =
                match key with
                | SymbolKey.MemberKey(_, _, argSig, _) -> argSig.Length
                | other -> failwithf "Emit: ExternalMember key is not a MemberKey: %A" other

            // The method consumes one spine element (its argument list); any
            // remainder is further application of the result (rare).
            let argList, rest =
                match spineArgs with
                | first :: more -> ValueSome first, more
                | [] -> ValueNone, []

            match receiver with
            | ValueSome r -> buildExpr env b r
            | ValueNone -> ()

            let pushedArgs =
                match argList with
                | ValueNone -> 0 // no argument supplied (a 0-param method)
                | ValueSome(argExpr, _) ->
                    if argCount >= 2 then
                        match argExpr with
                        | TExprG.Tuple(elems, _) when elems.Length = argCount ->
                            for el in elems do
                                buildExpr env b el

                            argCount
                        | _ ->
                            failwithf
                                "Emit: external member '%s' expects %d tupled arguments but the argument is not a literal %d-tuple"
                                name
                                argCount
                                argCount
                    elif argCount = 1 then
                        buildExpr env b argExpr
                        1
                    else
                        // argCount = 0: a `unit → ret` method; the lone arg is
                        // `()`, which has no IL value to push.
                        0

            let handle =
                match receiver with
                | ValueSome r -> externalInstanceMemberRef env key (typeOfExpr r) false (memberTy)
                | ValueNone -> env.Provider.ExternalMemberRef(key, false, true, memberTy)

            let total = (if isStatic then 0 else 1) + pushedArgs

            if isStatic then
                b.Add(ILInstr.Call(handle, total, 1))
            else
                b.Add(ILInstr.Callvirt(handle, total, 1))

            // A method returning a function value applied further (rare): the
            // result type is the consumed `App` node's type.
            let resultTy =
                match argList with
                | ValueSome(_, ty) -> ty
                | ValueNone -> typeOfExpr head

            foldInvoke env b resultTy rest

        | _ ->
            // The head is itself a function value (a closure local or a
            // partially applied result): emit it, then `Invoke` each arg.
            buildExpr env b head
            foldInvoke env b (typeOfExpr head) spineArgs

    /// Apply each remaining argument to the function value on the stack,
    /// threading the running function type. `tryInvoke` chooses the invocation
    /// recipe per arg (`Vesper.Fun::Invoke` vs `FSharpFunc::Invoke`); `what`
    /// names the function kind for the failure diagnostic.
    and private foldInvokeWith
        (tryInvoke: FrozenType -> CallRecipe voption)
        (what: string)
        (env: EmitEnv)
        (b: IlBuilder)
        (funcTy0: FrozenType)
        (args: (Frozen.TExpr * FrozenType) list)
        : unit =
        let mutable funcTy = funcTy0

        for (arg, resTy) in args do
            match tryInvoke funcTy with
            | ValueSome recipe ->
                buildExpr env b arg
                b.Add(ILInstr.Recipe recipe)
                funcTy <- resTy
            | ValueNone -> failwithf "Emit: cannot apply argument to %s value of type %A" what funcTy

    /// Apply remaining arguments to a native `Vesper.Fun` value via its `Invoke`.
    and private foldInvoke
        (env: EmitEnv)
        (b: IlBuilder)
        (funcTy0: FrozenType)
        (args: (Frozen.TExpr * FrozenType) list)
        : unit =
        foldInvokeWith env.Provider.TryEmitInvoke "Vesper.Fun" env b funcTy0 args

    /// Apply a curried FSharp.Core `FSharpFunc` value (the cold printf printer)
    /// argument by argument via `FSharpFunc::Invoke` — the FSharpFunc twin of
    /// `foldInvoke` (R1; retargeted with the printf engine, handoff §R9).
    and private foldInvokeFSharpFunc
        (env: EmitEnv)
        (b: IlBuilder)
        (funcTy0: FrozenType)
        (args: (Frozen.TExpr * FrozenType) list)
        : unit =
        foldInvokeWith env.Provider.TryEmitFSharpFuncInvoke "FSharpFunc" env b funcTy0 args

    /// Emit an expression as a statement: evaluate it and discard any value.
    let buildStatement (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        buildExpr env b e

        while b.Depth > 0 do
            b.Add ILInstr.Pop
