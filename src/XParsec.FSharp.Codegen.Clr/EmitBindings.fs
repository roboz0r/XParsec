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

/// `let` / `use` bindings. Both recurse into `buildExpr` (passed as `recur`); the
/// `use`-dispose path even builds a synthetic `MethodCall` and re-enters through
/// `recur`, so no direct cross-arm call is needed.
module EmitBindings =

    let buildLet (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.Let(TPatG.NamedSimple(binding, ty, _), value, body, _, _) ->
            let slot = b.Local ty
            env.Slots.[binding] <- slot
            recur env b value
            b.Add(ILInstr.Stloc slot)
            recur env b body
        | TExprG.Let(pat, value, body, _, _) ->
            // A destructuring `let pat = value in body` (e.g. `let a, b = (1, 2)`).
            // Evaluate the scrutinee once into a temp, then `bindPattern` (irrefutable)
            // pulls each leaf binding out of it before the body runs.
            let slot = b.Local(typeOfExpr value)
            recur env b value
            b.Add(ILInstr.Stloc slot)
            bindPattern env b slot pat
            recur env b body
        | _ -> failwith "EmitBindings.buildLet: unreachable"

    let buildUse (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.Use((TPatG.NamedSimple _ | TPatG.Wildcard _) as pat, value, body, dispose, _, tok) ->
            // `use x = value` (named) or `use _ = value` (wildcard). A `_` binder
            // still parks the value in a local — it is the resource the `finally`
            // disposes — but gives the body no name to reference it, so its slot is
            // keyed off a synthetic placeholder. Everything downstream (slot, null
            // check, disposal) is identical for both.
            let binding, varTy =
                match pat with
                | TPatG.NamedSimple(b, ty, _) -> b, ty
                | TPatG.Wildcard(ty, _) -> mintUseBinderKey (), ty
                | _ -> failwith "Emit: unreachable — outer match admits only NamedSimple / Wildcard"
            // `use x = value in body` → `let x = value in try body finally if x <> null
            // then x.Dispose()`. The IL-IR exception-region pseudo-marks (`Try` /
            // `BeginFinally` / `EndFinally`) carry the region; `IlIr.lower` replays
            // them into a proper `try`/`finally`.
            //
            // A protected region can't carry an evaluation-stack value across its
            // `leave`, so the body's result is parked in a local inside the `try`
            // and reloaded after the finally as the expression's value (works for a
            // unit body too — `Unit` is `null`, parked and reloaded like any value).
            // The disposal is guarded by a null check so a null binder is a no-op
            // like F#'s `use`. `dispose` (see `Disposal`) selects the path: the
            // capability's slot — on the CLR, the resolved `IDisposable::Dispose`
            // interface member — or the binder's own pattern `Dispose()`.
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
            // indistinguishable from a class here; supporting it requires recording
            // struct-ness on the node — the provider's `ExternalClassShape` doesn't
            // surface it.
            if isValueType env varTy then
                failwithf "Emit: `use` over a value-type binder is out of scope: %A" varTy

            let slot = b.Local varTy
            env.Slots.[binding] <- slot
            recur env b value
            b.Add(ILInstr.Stloc slot)

            let resultSlot = b.Local(typeOfExpr body)
            let endLabel = b.Label()
            let skipLabel = b.Label()

            b.Add ILInstr.Try
            recur env b body
            b.Add(ILInstr.Stloc resultSlot)
            b.Add(ILInstr.Leave endLabel)

            b.Add ILInstr.BeginFinally
            b.SetDepth 0
            b.Add(ILInstr.Ldloc slot)
            b.Add(ILInstr.Brfalse skipLabel)

            // Dispose through a local instance `x.Dispose()`: reuse the standard
            // instance-call path (`CallVia.Self` `MethodCall`), which resolves the member
            // handle and emits the `callvirt`. `Dispose` returns unit (one `Unit` value),
            // popped so the finally handler ends empty-stacked.
            let emitLocalDispose (disposeKey: SymbolKey) =
                recur
                    env
                    b
                    (TExprG.MethodCall(
                        TExprG.Var(binding, varTy, tok),
                        disposeKey,
                        CallVia.Self,
                        EqArray.empty,
                        FTConst(RuntimeNames.unitKey, EqArray.empty),
                        tok
                    ))

                b.Add ILInstr.Pop

            // Dispose through a keyed `Dispose` on an EXTERNAL type, minted as an
            // `ExternalMemberRef` `callvirt`. The external member carries a real `void`
            // return, so it pushes nothing: a receiver-only `callvirt`, no `pop`.
            let emitExternalDispose (disposeKey: SymbolKey) =
                let dispHandle =
                    env.Provider.ExternalMemberRef(
                        disposeKey,
                        false,
                        false,
                        FTFun(
                            FTConst(RuntimeNames.unitKey, EqArray.empty),
                            FTConst(RuntimeNames.unitKey, EqArray.empty)
                        )
                    )

                b.Add(ILInstr.Ldloc slot)
                b.Add(ILInstr.Callvirt(dispHandle, 1, 0))

            // Locality decides the call shape, in both disposal paths: an
            // `ExternalMemberRef` would fault on a project-local handle (mirrors
            // `EmitMember`'s `env.Unions`/`env.Classes` test), so a local type is always
            // disposed through the `CallVia.Self` path on its OWN `Dispose` method.
            let isLocalType (key: TypeKey) =
                env.Classes.ContainsKey(SymbolKey.Type key)
                || env.Unions.ContainsKey(SymbolKey.Type key)

            let isLocalBinder =
                match TastLower.receiverShape varTy with
                | ValueSome(headKey, _) -> isLocalType headKey
                | ValueNone -> false

            // `Disposal.ViaOwnMember` carries a member key by construction (both producers —
            // the external own-`Dispose` probe and the ref-struct carve-out — mint one), so
            // the question is only WHERE that member's type lives, never what kind of key it is.
            let isLocalDisposeKey (key: SymbolKey) =
                isLocalType (SymbolKeyOps.declTypeKeyOf "Emit: use-dispose member" key)

            match dispose with
            // The binder implements the disposal capability. A LOCAL impl disposes through
            // its own `Dispose` method; an EXTERNAL one through the capability's interface
            // slot (the key the front end resolved) — the type's own `Dispose` may not even
            // exist on it (`MemoryStream` inherits `Stream.Dispose`).
            | Disposal.ViaCapability _ when isLocalBinder ->
                emitLocalDispose (
                    SymbolKeyOps.memberKey
                        (nominalTypeKey "use-dispose receiver" varTy)
                        "Dispose"
                        EqArray.empty
                        MemberKind.Method
                )
            | Disposal.ViaCapability slot -> emitExternalDispose slot
            // The carve-out: an own pattern `Dispose()`, called directly. The project-local
            // `[<IsByRefLike>]` ref struct (`Infer.tryRefStructOwnDispose`) lands here too.
            | Disposal.ViaOwnMember key when isLocalDisposeKey key -> emitLocalDispose key
            | Disposal.ViaOwnMember key -> emitExternalDispose key
            | Disposal.Unresolved ->
                failwithf
                    "Emit: `use` over a binder with no resolved disposal (%A) — Unification reported an error, so this file should never have reached codegen"
                    varTy

            b.SetDepth 0
            b.Add(ILInstr.Mark skipLabel)
            b.Add ILInstr.EndFinally

            b.SetDepth 0
            b.Add(ILInstr.Mark endLabel)
            b.Add(ILInstr.Ldloc resultSlot)
        | TExprG.Use(pat, _, _, _, _, _) ->
            // Only a genuinely destructuring `use` (tuple / record / union / const)
            // can reach here — `NamedSimple` and `Wildcard` are handled above. Such
            // a pattern is rejected up front (`Validation.checkUseBindings` — "Only
            // simple variable patterns can be bound in 'use' expressions"), since the
            // bound value is what gets disposed. Kept as a defensive invariant guard.
            failwithf "Emit: destructuring use-binding should have been rejected by Validation: %A" pat
        | _ -> failwith "EmitBindings.buildUse: unreachable"
