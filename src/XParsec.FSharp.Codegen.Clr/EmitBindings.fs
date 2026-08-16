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

/// `let` / `use` bindings and the source-level `try`/`finally`.
module EmitBindings =

    /// Emit a `try Body finally …` protected region, returning the body's value.
    /// `leave` clears the evaluation stack, so the body's result is parked in a local
    /// inside the `try` and reloaded after; `emitFinally` must leave the stack empty.
    let private buildTryFinallyRegion
        (recur: Recur)
        (env: EmitEnv)
        (b: IlBuilder)
        (body: TastAccessor.ExprId)
        (emitFinally: unit -> unit)
        : unit =
        let resultSlot = b.Local(typeOfExpr body)
        let endLabel = b.Label()

        b.Add ILInstr.Try
        recur env b body
        b.Add(ILInstr.Stloc resultSlot)
        b.Add(ILInstr.Leave endLabel)

        b.Add ILInstr.BeginFinally
        b.SetDepth 0
        emitFinally ()
        b.SetDepth 0
        b.Add ILInstr.EndFinally

        b.SetDepth 0
        b.Add(ILInstr.Mark endLabel)
        b.Add(ILInstr.Ldloc resultSlot)

    let buildLet (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let view = TastAccessor.exprLet e

        match TastAccessor.patBoundVar view.Pattern with
        | ValueSome boundVar ->
            // A simple `let x = value in body` bound variable: park the value in `x`'s slot.
            let slot = b.Local(TastAccessor.patTy view.Pattern)
            env.Slots.[boundVar] <- slot
            recur env b view.Value
            b.Add(ILInstr.Stloc slot)
            recur env b view.Body
        | ValueNone ->
            // A destructuring `let a, b = (1, 2) in body`: park the scrutinee in a temp,
            // then `bindPattern` pulls each bound variable out of it before the body runs.
            let slot = b.Local(typeOfExpr view.Value)
            recur env b view.Value
            b.Add(ILInstr.Stloc slot)
            bindPattern env b slot view.Pattern
            recur env b view.Body

    let buildUse (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let view = TastAccessor.exprUse e
        let pat = view.Pattern

        match TastAccessor.patKind pat with
        | PatShape.NamedSimple
        | PatShape.Wildcard ->
            // `use x = value` or `use _ = value`. A `_` still parks the value under a synthetic
            // placeholder key, because it is the resource the `finally` disposes.
            let tok = TastAccessor.exprTok e
            let varTy = TastAccessor.patTy pat

            let boundVar =
                match TastAccessor.patBoundVar pat with
                | ValueSome b -> b
                | ValueNone -> TastPoolBuilder.mintBoundVar pat.Pool
            // `use x = v in body` → `let x = v in try body finally if x <> null then
            // x.Dispose()`, so a null `x` disposes nothing. `view.Dispose` names the member:
            // the capability's interface slot, or `x`'s own `Dispose()`.

            // Both disposal paths below `brfalse` the loaded `x` and `callvirt` it, so both
            // need a reference type. `brfalse` on a loaded struct is invalid IL, and a struct
            // object arg would need `ldloca` + `constrained. callvirt`.
            if isValueType env varTy then
                failwithf "Emit: `use` over a value-type bound variable is out of scope: %A" varTy

            let slot = b.Local varTy
            env.Slots.[boundVar] <- slot
            recur env b view.Value
            b.Add(ILInstr.Stloc slot)

            // `x.Dispose()` through the standard instance-call path, which resolves the
            // member handle and emits the `callvirt`; its `Unit` result is popped.
            let emitLocalDispose (disposeKey: SymbolKey) =
                let pool = view.Value.Pool

                recur
                    env
                    b
                    (TastAccessor.mintMethodCall
                        (TastAccessor.mintVar pool boundVar varTy tok)
                        disposeKey
                        CallVia.Self
                        [||]
                        (FTConst(RuntimeNames.unitKey, EqArray.empty))
                        tok)

                b.Add ILInstr.Pop

            // A keyed `Dispose` on an EXTERNAL type. Its real `void` return pushes
            // nothing, so this is an object-arg-only `callvirt` with no `pop`.
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

            // Locality decides the call shape in both disposal paths: a type emitted into
            // this assembly is disposed through its OWN `Dispose`, never a `MemberRef`.
            let isLocalType (key: TypeKey) =
                env.Classes.ContainsKey key || env.Unions.ContainsKey key

            let localNominal =
                FrozenNominal.TryOfFrozen varTy
                |> ValueOption.filter (fun n -> isLocalType n.Key)

            let isLocalDisposeKey (key: SymbolKey) =
                isLocalType (SymbolKeyOps.declTypeKeyOf "Emit: use-dispose member" key)

            // The finally handler: `if x <> null then x.Dispose()`, each disposal path
            // ending balanced so the handler leaves the stack empty.
            let skipLabel = b.Label()

            let emitDisposeFinally () =
                b.Add(ILInstr.Ldloc slot)
                b.Add(ILInstr.Brfalse skipLabel)

                match view.Dispose with
                | Disposal.ViaCapability slot ->
                    // A LOCAL capability impl disposes through its own `Dispose` method; an
                    // EXTERNAL one through the capability's interface slot, since the type's own
                    // `Dispose` may not exist on it (`MemoryStream` inherits `Stream.Dispose`).
                    match localNominal with
                    | ValueSome n ->
                        emitLocalDispose (SymbolKeyOps.memberKey n.Key "Dispose" EqArray.empty 0 MemberKind.Method)
                    | ValueNone -> emitExternalDispose slot
                // The carve-out: an own pattern `Dispose()`, called directly.
                | Disposal.ViaOwnMember key when isLocalDisposeKey key -> emitLocalDispose key
                | Disposal.ViaOwnMember key -> emitExternalDispose key
                | Disposal.Unresolved ->
                    failwithf
                        "Emit: `use` over a bound variable with no resolved disposal (%A). Disposal is unresolved only where Unification reported an error, so this file should never have reached codegen"
                        varTy

                b.SetDepth 0
                b.Add(ILInstr.Mark skipLabel)

            buildTryFinallyRegion recur env b view.Body emitDisposeFinally
        | _ ->
            // A destructuring `use` is rejected up front by Validation ("Only simple
            // variable patterns can be bound in 'use' expressions"), since the bound
            // value is what gets disposed.
            failwithf "Emit: destructuring use-binding should have been rejected by Validation: %A" pat

    let buildTryFinally (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let view = TastAccessor.exprTryFinally e

        // `Cleanup` types as unit and runs purely for effect, so discard whatever it
        // leaves: the handler must end empty-stacked.
        let emitCleanupFinally () =
            recur env b view.Cleanup

            while b.Depth > 0 do
                b.Add ILInstr.Pop

        buildTryFinallyRegion recur env b view.Body emitCleanupFinally
