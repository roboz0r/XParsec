namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Vesper
open XParsec.FSharp.SemanticAnalysis
open EmitTypes
open EmitLower
open EmitResolve
open EmitPattern
open EmitDispatch

/// `let` / `use` bindings and the source-level `try`/`finally`.
module EmitBindings =

    /// Emit a `try Body finally …` protected region. A value-position body's result is
    /// parked in a local inside the `try` and reloaded after the `leave`, which clears the
    /// evaluation stack; `emitFinally` must leave the stack empty.
    let private buildTryFinallyRegion
        (recur: RecurAt)
        (pos: ExprPos)
        (env: EmitEnv)
        (b: IlBuilder)
        (body: TastAccessor.ExprId)
        (emitFinally: unit -> unit)
        : unit =
        let resultSlot =
            match pos with
            | ExprPos.Value -> ValueSome(b.Local(typeOfExpr body))
            | ExprPos.Statement -> ValueNone

        let endLabel = b.Label()

        b.Add ILInstr.Try
        recur pos env b body
        resultSlot |> ValueOption.iter (fun slot -> b.Add(ILInstr.Stloc slot))
        b.Add(ILInstr.Leave endLabel)

        b.Add ILInstr.BeginFinally
        b.SetDepth 0
        emitFinally ()
        b.SetDepth 0
        b.Add ILInstr.EndFinally

        b.SetDepth 0
        b.Add(ILInstr.Mark endLabel)
        resultSlot |> ValueOption.iter (fun slot -> b.Add(ILInstr.Ldloc slot))

    let buildLet (recur: RecurAt) (pos: ExprPos) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let view = TastAccessor.exprLet e
        let m = view.Binding

        match TastAccessor.patBoundVar m.Pattern with
        // A generalised local lifted to a generic static method: its value is that method's
        // body, so the `let` binds no slot and only its body runs here.
        | ValueSome boundVar when env.LiftedLocals.ContainsKey boundVar -> recur pos env b view.Body
        | ValueSome boundVar ->
            // A simple `let x = value in body` bound variable: park the value in `x`'s slot.
            let slot = b.Local m.Ty
            env.Slots.[boundVar] <- slot
            recur ExprPos.Value env b m.Value
            b.Add(ILInstr.Stloc slot)
            recur pos env b view.Body
        | ValueNone ->
            // A destructuring `let a, b = (1, 2) in body`: park the scrutinee in a temp,
            // then `bindPattern` pulls each bound variable out of it before the body runs.
            let slot = b.Local(typeOfExpr m.Value)
            recur ExprPos.Value env b m.Value
            b.Add(ILInstr.Stloc slot)
            bindPattern env b slot m.Pattern
            recur pos env b view.Body

    /// `let rec a … and b … in body`. Slots for every member precede every member's value, and
    /// each closure's back-patched sibling captures are stored once the whole group is bound.
    /// Discovery rejects a non-function member, so every member's value is a closure.
    let buildLetGroup (recur: RecurAt) (pos: ExprPos) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let view = TastAccessor.exprLetGroup e

        let slots =
            view.Members
            |> Array.map (fun m ->
                let slot = b.Local m.Ty
                env.Slots.[TastAccessor.letGroupMemberKey m] <- slot
                slot
            )

        view.Members
        |> Array.iteri (fun i m ->
            recur ExprPos.Value env b m.Value
            b.Add(ILInstr.Stloc slots.[i])
        )

        view.Members
        |> Array.iteri (fun i m ->
            let c = env.ClosureByNode.[m.Value]

            c.Captures
            |> List.iteri (fun j cap ->
                match cap.Fill with
                | CaptureFill.BackPatched ->
                    b.Add(ILInstr.Ldloc slots.[i])
                    b.Add(ILInstr.Castclass(closureToken env c ClosureToken.Type))
                    buildVarLoad env b cap.Key
                    b.Add(ILInstr.Stfld(closureToken env c (ClosureToken.CaptureField j)))
                | CaptureFill.ByCtor -> ()
            )
        )

        recur pos env b view.Body

    let buildUse (recur: RecurAt) (pos: ExprPos) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
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
            // x.Dispose()`, so a null `x` disposes nothing. `view.Dispose` identifies the member:
            // the capability's interface slot, or `x`'s own `Dispose()`.

            // Both disposal paths below `brfalse` the loaded `x` and `callvirt` it, so both
            // need a reference type. `brfalse` on a loaded struct is invalid IL, and a struct
            // object arg would need `ldloca` + `constrained. callvirt`.
            if isValueType env varTy then
                failwithf "Emit: `use` over a value-type bound variable is out of scope: %A" varTy

            let slot = b.Local varTy
            env.Slots.[boundVar] <- slot
            recur ExprPos.Value env b view.Value
            b.Add(ILInstr.Stloc slot)

            // `x.Dispose()` through the standard instance-call path, which resolves the
            // member handle and emits the `callvirt`. Statement position, so the call
            // leaves the stack as it was.
            let emitLocalDispose (disposeKey: SymbolKey) =
                let pool = view.Value.Pool

                recur
                    ExprPos.Statement
                    env
                    b
                    (TastAccessor.mintMethodCall
                        (TastAccessor.mintVar pool boundVar varTy tok)
                        disposeKey
                        CallVia.Self
                        [||]
                        (RuntimeNames.unitTy)
                        tok)

            // A keyed `Dispose` on an EXTERNAL type. Its real `void` return pushes
            // nothing, so this is an object-arg-only `callvirt` with no `pop`.
            let emitExternalDispose (disposeKey: SymbolKey) =
                let dispHandle =
                    env.Provider.ExternalMemberRef(
                        disposeKey,
                        false,
                        false,
                        FTFun(RuntimeNames.unitTy, RuntimeNames.unitTy)
                    )

                b.Add(ILInstr.Ldloc slot)
                b.Add(ILInstr.Callvirt(dispHandle, 1, 0))

            // Locality decides the call shape in both disposal paths: a type emitted into
            // this assembly is disposed through its OWN `Dispose`, never a `MemberRef`.
            let isLocalType (key: TypeKey) =
                env.Classes.ContainsKey key || env.Unions.ContainsKey key

            let localNominal =
                FrozenNominal.tryOfFrozen varTy
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
                        emitLocalDispose (SymbolKeyOps.memberKey n.Key "Dispose" Block.empty 0 MemberKind.Method)
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

            buildTryFinallyRegion recur pos env b view.Body emitDisposeFinally
        | _ ->
            // A destructuring `use` is rejected up front by Validation ("Only simple
            // variable patterns can be bound in 'use' expressions"), since the bound
            // value is what gets disposed.
            failwithf "Emit: destructuring use-binding should have been rejected by Validation: %A" pat

    let buildTryFinally (recur: RecurAt) (pos: ExprPos) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let view = TastAccessor.exprTryFinally e

        // `Cleanup` types as unit and runs purely for effect. The handler must end
        // empty-stacked.
        let emitCleanupFinally () =
            recur ExprPos.Statement env b view.Cleanup

        buildTryFinallyRegion recur pos env b view.Body emitCleanupFinally
