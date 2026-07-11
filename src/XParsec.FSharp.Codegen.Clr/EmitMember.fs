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

/// Field / property / method access — instance and static, project-local and
/// external. The struct-receiver address helper lives here because only the
/// instance property/method arms need it.
module EmitMember =

    /// Load a value-type receiver as a managed pointer (`this` byref) for an
    /// address-based member call. A method
    /// / property call on an *unboxed* struct needs the receiver **address**, not
    /// its value: a `let`/slot-bound local is addressed in place (`ldloca slot`)
    /// so a mutating member persists; the `this`/self receiver of a struct
    /// instance method is *already* a managed pointer (`ldarg.0` is the byref
    /// `this`), so it is loaded directly — spilling it would copy the struct and
    /// a mutating self-call (`this.AppendLiteral …`) would not persist; any other
    /// receiver expression (an arg, a capture, a nested call) is spilled to a
    /// fresh temp and addressed there.
    /// Leaves the address on the stack; the caller pushes args then `constrained.
    /// <recvTy>` immediately before the `callvirt`.
    let rec private loadStructReceiverAddr
        (recur: Recur)
        (env: EmitEnv)
        (b: IlBuilder)
        (receiver: Frozen.TExpr)
        (receiverTy: FrozenType)
        : unit =
        match receiver with
        | TExprG.Var(binding, _, _) when env.Slots.ContainsKey binding -> b.Add(ILInstr.Ldloca env.Slots.[binding])
        | TExprG.Var(binding, _, _) when env.SelfKey = ValueSome binding -> b.Add(ILInstr.Ldarg 0)
        // A struct-typed *field* receiver (`this.Source.MoveNext()`): address the
        // field in place with `ldflda` so a mutating member call persists — spilling
        // the field's *value* to a temp (the fall-through below) would mutate a copy.
        // The parent is itself addressed when it's a struct (recurse — chains
        // `this.a.b.M()`) or loaded by value when it's a reference type; `ldflda`
        // accepts either an object ref or a managed pointer. A struct returned by a
        // *property* still falls through to the spill (a getter yields a copy — there
        // is no in-place location to address, matching F#'s copy semantics).
        | TExprG.FieldGet(parent, name, _, _) ->
            let parentTy = typeOfExpr parent
            let fldHandle = resolveRecordField env parentTy name

            if isValueType env parentTy then
                loadStructReceiverAddr recur env b parent parentTy
            else
                recur env b parent

            b.Add(ILInstr.Ldflda fldHandle)
        | _ ->
            recur env b receiver
            let tmp = b.Local receiverTy
            b.Add(ILInstr.Stloc tmp)
            b.Add(ILInstr.Ldloca tmp)

    /// Emit an instance member access: load the receiver, push any arguments, then
    /// invoke `handle`. The receiver/dispatch shape is shared by `buildPropertyGet`
    /// (no arguments) and `buildMethodCall`:
    /// - `CallVia.Self` on an *unboxed struct* (`FTClass` + `isValueType`) — address
    ///   the receiver (`ldloca`, so a mutating member persists) and `call` it.
    /// - `CallVia.Self` on a *class* — `callvirt` (the safe default; a non-`override` would accept `call` too).
    /// - anything else — a non-virtual `call`: `base.M`/`base.X` (`CallVia.Base`,
    ///   `receiverTy` already the parent type) so an `override` body doesn't recurse,
    ///   and sealed union/record receivers (not `FTClass`) where no virtual dispatch
    ///   is needed.
    let private emitInstanceMember
        (recur: Recur)
        (env: EmitEnv)
        (b: IlBuilder)
        (via: CallVia<FrozenType>)
        (receiver: Frozen.TExpr)
        (receiverTy: FrozenType)
        (handle: EntityHandle)
        (args: EqArray<Frozen.TExpr>)
        (returnsUnit: bool)
        : unit =
        let isStructSelf =
            match via, receiverTy with
            | CallVia.Self, FTClass _ -> isValueType env receiverTy
            | _ -> false

        if isStructSelf then
            loadStructReceiverAddr recur env b receiver receiverTy
        else
            recur env b receiver

        // The value→`obj` box for an `obj` parameter is now an explicit `Upcast`
        // node synthesised at Elaborate — codegen just pushes each argument.
        for a in args do
            recur env b a

        let operands = 1 + args.Length

        // A `unit`-returning instance method is emitted `void` (`NominalEmit`'s
        // `returnsVoid`): it pushes nothing, so the call declares 0 results and a
        // `unit` value is reified afterward for a value-position consumer — the same
        // `unit → void` convention the external-call path (`EmitCall`) uses. A
        // property get is never `unit`, so `buildPropertyGet` passes `false`.
        let resultCount = if returnsUnit then 0 else 1

        match via, receiverTy with
        | CallVia.Self, FTClass _ when not isStructSelf -> b.Add(ILInstr.Callvirt(handle, operands, resultCount))
        | _ -> b.Add(ILInstr.Call(handle, operands, resultCount))

        if returnsUnit then
            EmitTypes.buildUnitValue env b

    /// Rung-3 Wall C: a member access on a value whose type is a generic typar
    /// constrained to an interface (`x : 'T when 'T :> IFace`). The receiver is an
    /// `FTTypar`, not a nominal — so `resolveInstanceMember` (which destructures a
    /// nominal head) can't be used. Instead the abstract slot is resolved directly off
    /// the member key's declaring interface (recorded by Wall B's `TyparInterfaceCall`
    /// side-table) and the call is dispatched with a `constrained. <typar> callvirt`:
    /// the JIT then dispatches a *struct* typar by address (no box) and a *class* typar
    /// by reference — the zero-alloc behaviour rung 3 needs. Sound because the slot is
    /// an interface (virtual) member, so the non-virtual-struct-method guardrail does
    /// not bite. Shared by `buildMethodCall` (args present) and `buildPropertyGet` (a
    /// 0-argument access → a `get_<name>` getter slot).
    let private emitConstrainedInterfaceCall
        (recur: Recur)
        (env: EmitEnv)
        (b: IlBuilder)
        (receiver: Frozen.TExpr)
        (key: SymbolKey)
        (ifaceArgs: EqArray<FrozenType>)
        (args: EqArray<Frozen.TExpr>)
        (ty: FrozenType)
        : unit =
        let receiverTy = typeOfExpr receiver
        let name = SymbolKeyOps.simpleName key
        let argTys = [ for a in args -> typeOfExpr a ]

        let ifaceKey =
            match key with
            | SymbolKey.MemberKey(decl, _, _, _) -> decl
            | _ -> failwithf "EmitMember: CallVia.Interface member key is not a MemberKey: %A" key

        // The abstract slot the `constrained. callvirt` targets. A *project-local*
        // interface (`'T :> IFace`, rung 3) is in `env.Interfaces`, so the slot is
        // minted off its emitted member. An *external* interface (`'T :>
        // Vesper.Fun<int,int>`, rung 4) is not registered locally — mint the slot
        // via the provider against the interface's instantiated `TypeSpec`, exactly
        // as `EmitResolve.resolveExternalMember` does for a grounded receiver.
        let slotHandle =
            match env.Interfaces.TryGetValue ifaceKey with
            | true, iface ->
                let m =
                    match iface.Members.TryGetValue name with
                    | true, candidates -> pickOverload name candidates argTys
                    | false, _ -> failwithf "EmitMember: interface '%A' has no emitted member '%s'" ifaceKey name

                // For a *generic* interface (`'S :> IStructSeq<'E>`), the abstract slot
                // lives on the instantiated interface `TypeSpec` (`IStructSeq`1<!E>`), not
                // on the bare generic definition — so mint a `MemberRef` against the
                // instantiation Wall B threaded onto `CallVia.Interface`. A non-generic
                // interface (empty `iface.Typars`) uses the slot's `Def` handle directly.
                EmitResolve.memberRef
                    env
                    iface.Typars
                    ifaceKey
                    (EqArray.toList ifaceArgs)
                    (UserMemberKind.Member(m.MetaName, false, m.MethodTyparCount, m.ParamTys, m.RetTy))
                    m.Handle
            | false, _ ->
                // External interface: encode the declaring type from the interface key
                // + the instantiation `CallVia.Interface` carries, and recover the slot
                // signature from the provider's metadata. `memberTy` is the access's
                // instantiated curried shape (`arg → … → ret`) so the method-axis (if
                // any) is recoverable; a property slot drops to its value type.
                let ifaceTy = FTClass(ifaceKey, ifaceArgs)

                let memberTy = EmitResolve.curriedFun argTys ty

                env.Provider.ExternalMemberRefOn(key, ifaceTy, false, false, memberTy)

        // A `unit`-returning instance method is emitted `void` (`NominalEmit`).
        let returnsUnit =
            match ty with
            | FTConst(key, _) when SymbolKeyOps.simpleName key = "unit" -> true
            | _ -> false

        let resultCount = if returnsUnit then 0 else 1
        let operands = 1 + args.Length

        // Receiver by *address* — `loadStructReceiverAddr` addresses a slot/self/field
        // in place and spills any other receiver (a static-fn arg `Ldarg i`) to a
        // temp it `ldloca`s; `constrained.` needs that managed pointer for both struct
        // and class typars.
        loadStructReceiverAddr recur env b receiver receiverTy

        for a in args do
            recur env b a

        b.Add(ILInstr.Constrained(env.Provider.TypeToken receiverTy))
        b.Add(ILInstr.Callvirt(slotHandle, operands, resultCount))

        if returnsUnit then
            EmitTypes.buildUnitValue env b

    let buildFieldGet (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.FieldGet(receiver, name, _, _) ->
            // `r.X` — load the receiver and `ldfld` the field. The field handle is
            // a `Def` token for a monomorphic record, a `MemberRef` on the receiver's
            // `TypeSpec` for a generic one (`resolveRecordField`). A
            // referenced-assembly record routes through the provider.
            let handle = resolveRecordField env (typeOfExpr receiver) name
            recur env b receiver
            b.Add(ILInstr.Ldfld handle)
        | _ -> failwith "EmitMember.buildFieldGet: unreachable"

    let buildAssignment (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.Assignment(TExprG.Var(binding, _, _), value, _, _) ->
            // `x <- v` on a non-promoted `mutable` local — store into its slot.
            // (A `HeapShared` mutable local was already rewritten by
            // `RefCellPromotion` into a `contents` FieldSet, so any `Assignment`
            // surviving to codegen targets a plain stack local.) Unit-typed, so
            // reify `unit` for the consumer — same convention as `FieldSet`.
            match env.Slots.TryGetValue binding with
            | true, slot ->
                recur env b value
                b.Add(ILInstr.Stloc slot)
                EmitTypes.buildUnitValue env b
            | false, _ -> failwithf "Emit: assignment to a variable with no local slot: %O" binding
        | _ -> failwith "EmitMember.buildAssignment: unreachable"

    let buildFieldSet (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.FieldSet(receiver, name, value, _, _) ->
            // `r.X <- v` on a `mutable` field. Validation has rejected the
            // immutable case before we reach here. `stfld` consumes both pushes
            // and leaves nothing on the stack, but a `FieldSet` is *unit-typed*
            // — every consumer (`Sequential` middle items, the body of a
            // unit-returning closure / static method) expects a unit value to be
            // present. Reify the `unit` value to keep the IL verifier happy when
            // the body is just a FieldSet.
            let handle = resolveRecordField env (typeOfExpr receiver) name
            recur env b receiver
            recur env b value
            b.Add(ILInstr.Stfld handle)
            EmitTypes.buildUnitValue env b
        | _ -> failwith "EmitMember.buildFieldSet: unreachable"

    let buildPropertyGet (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.PropertyGet(receiver, key, CallVia.Interface ifaceArgs, ty, _) ->
            // Rung-3: an instance *property* read on a typar receiver constrained to an
            // interface (`this.Source.Current` where `Source : 'E :> IStructEnumerator`).
            // A 0-argument constrained interface access — the getter slot is `get_<name>`
            // in the interface registry. Shared `constrained. callvirt` path with the
            // method case.
            emitConstrainedInterfaceCall recur env b receiver key ifaceArgs EqArray.empty ty
        | TExprG.PropertyGet(receiver, key, via, _, _) ->
            // Instance property read — a 0-argument instance member access; the
            // receiver/dispatch shape is shared with `buildMethodCall`.
            let receiverTy = typeOfExpr receiver
            // A property is never a generic method, so the resolved member metadata
            // is unused here (`MethodTyparCount` is always 0 for a `get_<name>`).
            // A property get is a 0-argument access — no overload args to match.
            let handle, _ =
                resolveInstanceMember env receiverTy (SymbolKeyOps.simpleName key) []
            // A property get is never `unit`-returning, so it always yields a value.
            emitInstanceMember recur env b via receiver receiverTy handle EqArray.empty false
        | _ -> failwith "EmitMember.buildPropertyGet: unreachable"

    let buildMethodCall (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.MethodCall(receiver, key, CallVia.Interface ifaceArgs, args, ty, _) ->
            emitConstrainedInterfaceCall recur env b receiver key ifaceArgs args ty
        | TExprG.MethodCall(receiver, key, via, args, ty, _) ->
            // Instance method call — the same receiver/dispatch shape as
            // `buildPropertyGet`, with the call's arguments pushed between the
            // receiver and the `call`/`callvirt`.
            let receiverTy = typeOfExpr receiver
            let argTys = [ for a in args -> typeOfExpr a ]

            let handle0, m =
                resolveInstanceMember env receiverTy (SymbolKeyOps.simpleName key) argTys

            // A *generic instance method*: the
            // member-ref already carries the `GENERIC` header (its `'U` rides `!!i`),
            // so the call must wrap it in a `MethodSpec`. The node carries no method
            // type args, so recover them by structurally matching the member's declared
            // curried signature (declaring-/method-axis markers) against the call's
            // actual argument + result types — the instance analogue of the
            // generic-static-fn `MethodSpec` recovery (`EmitCall`).
            let handle =
                if m.MethodTyparCount = 0 then
                    handle0
                else
                    let declArity =
                        match receiverShape receiverTy with
                        | ValueSome(_, rargs) -> List.length rargs
                        | ValueNone -> 0

                    let _, methodArgs =
                        recoverMemberInst env m declArity [ for a in args -> typeOfExpr a ] ty

                    env.Provider.StaticFnMethodSpec(handle0, methodArgs)

            // A `unit`-returning instance method is emitted `void` (`NominalEmit`):
            // detect it from the call's result type so the call declares 0 results.
            let returnsUnit =
                match ty with
                | FTConst(key, _) when SymbolKeyOps.simpleName key = "unit" -> true
                | _ -> false

            emitInstanceMember recur env b via receiver receiverTy handle args returnsUnit
        | _ -> failwith "EmitMember.buildMethodCall: unreachable"

    let buildStaticPropertyGet (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.StaticPropertyGet(key, ty, _) ->
            let handle = resolveStaticMember env key [] ty
            b.Add(ILInstr.Call(handle, 0, 1))
        | _ -> failwith "EmitMember.buildStaticPropertyGet: unreachable"

    let buildStaticFieldGet (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.StaticFieldGet(declKey, name, _, _) ->
            // A numeric enum case (`E.A`) pushes its underlying integer constant — the
            // enum value IS that integer (its `literal` field is metadata-only, so
            // `ldsfld` would throw `MissingFieldException`). A class `static let`
            // backing field is a real `ldsfld`.
            match tryResolveEnumCaseLoad env declKey name with
            | ValueSome instr -> b.Add instr
            | ValueNone -> b.Add(ILInstr.Ldsfld(resolveStaticField env declKey name))
        | _ -> failwith "EmitMember.buildStaticFieldGet: unreachable"

    let buildStaticMethodCall (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.StaticMethodCall(key, args, ty, _) ->
            // The declaring type of the static member. When it is a project-local
            // class/union the emitted tables carry it; when it lives in a referenced
            // package it does not — a *consumer*'s SRTP `+` / `-` dispatching to an
            // imported type's static operator (`Vesper.Set`'s `op_Addition`) reaches
            // here. `Inline.resolveTraitCall` always mints a local-shaped
            // `StaticMethodCall`; route the external case through the external
            // member-ref path instead of failing in `resolveStaticMember`.
            let isLocal =
                match key with
                | SymbolKey.MemberKey(declKey, _, _, _) ->
                    env.Unions.ContainsKey declKey || env.Classes.ContainsKey declKey
                | _ -> true

            let handle =
                if isLocal then
                    resolveStaticMember env key [ for a in args -> typeOfExpr a ] ty
                else
                    // Reconstruct the member's .NET-tupled signature from the pushed
                    // args + result so `ExternalMemberRef` can recover the declaring
                    // instantiation (`Set<int>` from `op_Addition`'s open `Set<!0>`).
                    let argTys = [ for a in args -> typeOfExpr a ]

                    let paramTy =
                        match argTys with
                        | [] -> FTConst(RuntimeNames.unitKey, EqArray.empty)
                        | [ single ] -> single
                        | many -> FTTuple(EqArray.ofList many)

                    env.Provider.ExternalMemberRef(key, false, true, FTFun(paramTy, ty))
            // Obj-parameter boxes are explicit `Upcast` nodes from Elaborate; push raw.
            for a in args do
                recur env b a

            // A `unit`-returning static member is emitted `void` (Step B,
            // "void everywhere") — and the external member-ref encoder already maps
            // a `unit` return to `void` — so the `call` declares 0 results and a
            // value-position consumer reifies a `unit` afterward, exactly as the
            // instance path (`emitInstanceMember`) does.
            let returnsUnit =
                match ty with
                | FTConst(key, _) when SymbolKeyOps.simpleName key = "unit" -> true
                | _ -> false

            let resultCount = if returnsUnit then 0 else 1
            b.Add(ILInstr.Call(handle, args.Length, resultCount))

            if returnsUnit then
                EmitTypes.buildUnitValue env b
        | _ -> failwith "EmitMember.buildStaticMethodCall: unreachable"

    let buildExternalMember (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.ExternalMember(receiver, key, _, MemberStorage.Field, ty, _) ->
            // A genuine external public field — read via `ldsfld` (static, e.g.
            // `String.Empty`) or `ldfld` over the pushed receiver (instance, e.g. a
            // `ValueTuple`'s `Item1`), against a field token (not a `get_X` accessor).
            match receiver with
            | ValueNone ->
                let handle = env.Provider.ExternalFieldRef(key, ValueNone, ty)
                b.Add(ILInstr.Ldsfld handle)
            | ValueSome r ->
                let receiverTy = typeOfExpr r
                let handle = env.Provider.ExternalFieldRef(key, ValueSome receiverTy, ty)

                // An unboxed value-type receiver is reached by address (as the property
                // getter arm does); `ldfld` then reads the field off that managed pointer.
                if isValueType env receiverTy then
                    loadStructReceiverAddr recur env b r receiverTy
                else
                    recur env b r

                b.Add(ILInstr.Ldfld handle)
        | TExprG.ExternalMember(receiver, key, _, MemberStorage.Property, ty, _) ->
            // A standalone external *property* get: a static one (`call
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
                let receiverTy = typeOfExpr r
                let handle = externalInstanceMemberRef env key receiverTy true (ty)

                // A property getter on an *unboxed* value-type receiver (`span.Length`,
                // any external struct) is reached by address + non-virtual `call`, not
                // by value + `callvirt` (the verifier rejects the latter — a ref struct
                // can't be boxed). Same dispatch as `emitInstanceMember`'s struct self.
                if isValueType env receiverTy then
                    loadStructReceiverAddr recur env b r receiverTy
                    b.Add(ILInstr.Call(handle, 1, 1))
                else
                    recur env b r
                    b.Add(ILInstr.Callvirt(handle, 1, 1))
        | TExprG.ExternalMember(_, _, _, MemberStorage.Method, _, _) ->
            // An external method used as a first-class value (a method group, not
            // applied) needs closure synthesis — out of scope. Applied methods are
            // handled as an `App` head above.
            failwith "Emit: external method used as a first-class value is out of scope"
        | _ -> failwith "EmitMember.buildExternalMember: unreachable"
