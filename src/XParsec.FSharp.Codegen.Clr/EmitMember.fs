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

/// Field / property / method access, instance and static, project-local and external.
module EmitMember =

    /// Load an unboxed value-type object argument as its `this` pointer, so a mutating call
    /// persists rather than mutating a copy: a slot-bound local is addressed in place
    /// (`ldloca slot`), a struct `this` already IS a byref (`ldarg.0`), anything else spills.
    let rec private loadStructThisPtr
        (recur: Recur)
        (env: EmitEnv)
        (b: IlBuilder)
        (objArg: TastAccessor.ExprId)
        (objArgTy: FrozenType)
        : unit =
        match objArg with
        | LocalSlot env slot -> b.Add(ILInstr.Ldloca slot)
        | TastAccessor.EVar k when env.SelfKey = ValueSome k -> b.Add(ILInstr.Ldarg 0)
        // A struct-typed FIELD object argument (`this.Source.MoveNext()`) is addressed with
        // `ldflda`; the parent recurses when it is itself a struct (`this.a.b.M()`). A struct
        // from a PROPERTY falls through to the spill because a getter yields a copy with no
        // location.
        | TastAccessor.EFieldGet fieldGet ->
            let parent = fieldGet.ObjArg
            let name = fieldGet.FieldName
            let parentTy = typeOfExpr parent
            let fldHandle = resolveRecordField env parentTy name

            if isValueType env parentTy then
                loadStructThisPtr recur env b parent parentTy
            else
                recur env b parent

            b.Add(ILInstr.Ldflda fldHandle)
        | _ ->
            recur env b objArg
            let tmp = b.Local objArgTy
            b.Add(ILInstr.Stloc tmp)
            b.Add(ILInstr.Ldloca tmp)

    /// Emit an instance member access: load the object argument, push args, invoke `handle`. A
    /// `Self` object argument is `callvirt`ed for a class, addressed + `call`ed for a struct;
    /// `Base` and sealed union/record ones take a non-virtual `call`, so `base.M` cannot recurse.
    let private emitInstanceMember
        (recur: Recur)
        (env: EmitEnv)
        (b: IlBuilder)
        (via: CallVia<FrozenType>)
        (objArg: TastAccessor.ExprId)
        (objArgTy: FrozenType)
        (handle: EntityHandle)
        (args: EqArray<TastAccessor.ExprId>)
        (returnsUnit: bool)
        : unit =
        let isStructSelf =
            match via, objArgTy with
            | CallVia.Self, FTClass _ -> isValueType env objArgTy
            | _ -> false

        if isStructSelf then
            loadStructThisPtr recur env b objArg objArgTy
        else
            recur env b objArg

        // The value→`obj` box is an explicit `Upcast` node from Elaborate; push each arg raw.
        for a in args do
            recur env b a

        let operands = 1 + args.Length

        // A `unit`-returning instance method is emitted `void`: it pushes nothing, so the call
        // declares 0 results and a `unit` value is reified afterward for the consumer.
        let resultCount = if returnsUnit then 0 else 1

        match via, objArgTy with
        | CallVia.Self, FTClass _ when not isStructSelf -> b.Add(ILInstr.Callvirt(handle, operands, resultCount))
        | _ -> b.Add(ILInstr.Call(handle, operands, resultCount))

        if returnsUnit then
            EmitTypes.buildUnitValue env b

    /// A member access through an interface-constrained typar (`x : 'T when 'T :> IFace`). The
    /// object argument is an `FTTypar`, not a nominal, so the slot comes off the key's declaring
    /// interface, dispatched `constrained. <typar> callvirt`, so a struct typar goes by address
    /// with no box.
    let private emitConstrainedInterfaceCall
        (recur: Recur)
        (env: EmitEnv)
        (b: IlBuilder)
        (objArg: TastAccessor.ExprId)
        (key: SymbolKey)
        (ifaceArgs: EqArray<FrozenType>)
        (args: EqArray<TastAccessor.ExprId>)
        (ty: FrozenType)
        : unit =
        let objArgTy = typeOfExpr objArg
        // A member name carries no arity, so the display projection IS the emitted CLR name.
        let (DisplayName name) = SymbolKeyOps.simpleName key
        let argTys = [ for a in args -> typeOfExpr a ]

        let ifaceKey = SymbolKeyOps.declTypeKeyOf "EmitMember: CallVia.Interface member" key

        // The abstract slot the `constrained. callvirt` targets. A project-local interface is
        // in `env.Interfaces`; an external one (`'T :> Vesper.Fun<int,int>`) is not registered
        // locally, so its slot is minted against the interface's instantiated `TypeSpec`.
        let slotHandle =
            match env.Interfaces.TryGetValue(SymbolKey.Type ifaceKey) with
            | true, iface ->
                let m =
                    match iface.Members.TryGetValue name with
                    | true, candidates -> pickOverload name candidates argTys
                    | false, _ -> failwithf "EmitMember: interface '%A' has no emitted member '%s'" ifaceKey name

                // A generic interface's abstract slot lives on the instantiated `TypeSpec`
                // (`IStructSeq<int>`), not the bare definition, so mint a `MemberRef` at the
                // `ifaceArgs` the node carries. A non-generic one uses the `Def` handle.
                EmitResolve.memberRef
                    env
                    iface.Typars
                    ifaceKey
                    (EqArray.toList ifaceArgs)
                    (UserMemberKind.Member(m.MetaName, false, m.MethodTyparCount, m.ParamTys, m.RetTy))
                    m.Handle
            | false, _ ->
                // `memberTy` is the access's instantiated curried shape (`arg → … → ret`), so
                // the method axis is recoverable from the provider's metadata; a property
                // slot drops to its bare value type.
                let ifaceTy = FTClass(ifaceKey, ifaceArgs)

                let memberTy = EmitResolve.curriedFun argTys ty

                env.Provider.ExternalMemberRefOn(key, ifaceTy, false, false, memberTy)

        // A `unit`-returning instance method is emitted `void`.
        let returnsUnit =
            match ty with
            | FTUnit -> true
            | _ -> false

        let resultCount = if returnsUnit then 0 else 1
        let operands = 1 + args.Length

        // `constrained.` needs a managed pointer for both struct and class typars.
        loadStructThisPtr recur env b objArg objArgTy

        for a in args do
            recur env b a

        b.Add(ILInstr.Constrained(env.Provider.TypeToken objArgTy))
        b.Add(ILInstr.Callvirt(slotHandle, operands, resultCount))

        if returnsUnit then
            EmitTypes.buildUnitValue env b

    let buildFieldGet (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let view = TastAccessor.exprFieldGet e
        let objArg = view.ObjArg
        let name = view.FieldName
        // `r.X` — load the object argument and `ldfld` the field.
        let handle = resolveRecordField env (typeOfExpr objArg) name
        recur env b objArg
        b.Add(ILInstr.Ldfld handle)

    let buildAssignment (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let view = TastAccessor.exprAssignment e

        match view.Lhs with
        | TastAccessor.EVar binding ->
            let value = view.Rhs
            // `x <- v` on a non-promoted `mutable` local — store into its slot. A heap-shared
            // one was already rewritten into a `contents` `FieldSet`, so an `Assignment`
            // surviving to codegen targets a plain stack local. Unit-typed, so reify `unit`.
            match env.Slots.TryGetValue binding with
            | true, slot ->
                recur env b value
                b.Add(ILInstr.Stloc slot)
                EmitTypes.buildUnitValue env b
            | false, _ -> failwithf "Emit: assignment to a variable with no local slot: %O" binding
        | _ -> failwith "Emit: assignment lhs is not a mutable-local Var (front end should have rejected it)"

    let buildFieldSet (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let view = TastAccessor.exprFieldSet e
        let objArg = view.ObjArg
        let name = view.FieldName
        let value = view.Value
        // `r.X <- v` on a `mutable` field. `stfld` consumes both pushes and leaves nothing, but
        // a `FieldSet` is UNIT-TYPED and a `Sequential` middle item or a unit-returning body
        // expects a value present, so reify `unit` to keep the IL verifier happy.
        let handle = resolveRecordField env (typeOfExpr objArg) name
        recur env b objArg
        recur env b value
        b.Add(ILInstr.Stfld handle)
        EmitTypes.buildUnitValue env b

    let buildPropertyGet (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let view = TastAccessor.exprPropertyGet e
        let objArg = view.ObjArg
        let key = view.Key

        match view.Via with
        | CallVia.Interface ifaceArgs ->
            // A property read through an interface-constrained typar (`this.Source.Current`)
            // is a 0-argument constrained access; the slot is the interface's `get_<name>`.
            let ty = TastAccessor.exprTy e
            emitConstrainedInterfaceCall recur env b objArg key ifaceArgs EqArray.empty ty
        | via ->
            let objArgTy = typeOfExpr objArg
            // A property is never a generic method and takes no arguments, so the resolved
            // member metadata is unused and there are no overload args to match.
            let (DisplayName memberName) = SymbolKeyOps.simpleName key
            let handle, _ = resolveInstanceMember env objArgTy memberName []
            // A property get is never `unit`-returning, so it always yields a value.
            emitInstanceMember recur env b via objArg objArgTy handle EqArray.empty false

    let buildMethodCall (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let view = TastAccessor.exprMethodCall e
        let objArg = view.ObjArg
        let key = view.Key
        // `MethodCallView.Args` is ONLY the args, whereas `exprChildren` merges the object arg in.
        let args = view.Args
        let ty = TastAccessor.exprTy e

        match view.Via with
        | CallVia.Interface ifaceArgs -> emitConstrainedInterfaceCall recur env b objArg key ifaceArgs args ty
        | via ->
            let objArgTy = typeOfExpr objArg
            let argTys = [ for a in args -> typeOfExpr a ]

            let (DisplayName memberName) = SymbolKeyOps.simpleName key
            let handle0, m = resolveInstanceMember env objArgTy memberName argTys

            // A generic instance method's member-ref already carries the `GENERIC` header (its
            // `'U` rides `!!i`), so the call must wrap it in a `MethodSpec`. The node carries no
            // method type args, so recover them from the declared signature.
            let handle =
                if m.MethodTyparCount = 0 then
                    handle0
                else
                    let declTyparArity =
                        match objArgShape objArgTy with
                        | ValueSome(_, rargs) -> List.length rargs
                        | ValueNone -> 0

                    let _, methodArgs = recoverMemberInst env m declTyparArity argTys ty

                    env.Provider.StaticFnMethodSpec(handle0, methodArgs)

            // A `unit`-returning instance method is emitted `void`, so it declares 0 results.
            let returnsUnit =
                match ty with
                | FTUnit -> true
                | _ -> false

            emitInstanceMember recur env b via objArg objArgTy handle args returnsUnit

    let buildStaticPropertyGet (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let key = TastAccessor.exprStaticPropertyGetKey e
        let ty = TastAccessor.exprTy e
        let handle = resolveStaticMember env key [] ty
        b.Add(ILInstr.Call(handle, 0, 1))

    let buildStaticFieldGet (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let view = TastAccessor.exprStaticFieldGet e
        let declKey = view.Key
        let name = view.FieldName
        // A numeric enum case (`E.A`) IS its integer at runtime and its `literal` field is
        // metadata-only, so `ldsfld` would throw `MissingFieldException`; push the constant
        // instead. A class `static let` backing field is a real `ldsfld`.
        match tryResolveEnumCaseLoad env declKey name with
        | ValueSome instr -> b.Add instr
        | ValueNone -> b.Add(ILInstr.Ldsfld(resolveStaticField env declKey name))

    let buildStaticFieldSet (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let view = TastAccessor.exprStaticFieldSet e
        let declKey = view.Key
        let name = view.FieldName
        let value = view.Value
        // `x <- v` on a `static let mutable` backing field. `stsfld` leaves nothing on the
        // stack, but the write is UNIT-TYPED, so reify a unit value for the consumer.
        recur env b value
        b.Add(ILInstr.Stsfld(resolveStaticField env declKey name))
        EmitTypes.buildUnitValue env b

    let buildStaticMethodCall (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let key = TastAccessor.exprStaticMethodCallKey e
        // A `StaticMethodCall`'s arguments ARE its `exprChildren` (no object arg to merge).
        let args = TastAccessor.exprChildren e
        let ty = TastAccessor.exprTy e
        // A consumer's SRTP `+` dispatching to an imported type's static operator
        // (`Vesper.Set`'s `op_Addition`) reaches here as a LOCAL-shaped `StaticMethodCall`, so
        // route the external case out rather than let the local resolve fail on it.
        let isLocal =
            let declKey =
                SymbolKey.Type(SymbolKeyOps.declTypeKeyOf "EmitMember: static member call" key)

            env.Unions.ContainsKey declKey || env.Classes.ContainsKey declKey

        let handle =
            if isLocal then
                resolveStaticMember env key [ for a in args -> typeOfExpr a ] ty
            else
                // Reconstruct the member's .NET-tupled signature from the pushed args + result,
                // so the ref can recover `Set<int>` from `op_Addition`'s open `Set<!0>`.
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

        // A `unit`-returning static member is emitted `void`, and the external member-ref
        // encoder maps a `unit` return to `void` too, so the `call` declares 0 results and a
        // value-position consumer reifies a `unit` afterward.
        let returnsUnit =
            match ty with
            | FTUnit -> true
            | _ -> false

        let resultCount = if returnsUnit then 0 else 1
        b.Add(ILInstr.Call(handle, args.Length, resultCount))

        if returnsUnit then
            EmitTypes.buildUnitValue env b

    let buildExternalMember (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let view = TastAccessor.exprExternalMember e
        let objArg = view.ObjArg
        let key = view.Key
        let ty = TastAccessor.exprTy e

        match view.Storage with
        | MemberStorage.Field ->
            // A genuine external public FIELD, against a field token and not a `get_X`
            // accessor: `ldsfld` for a static one (`String.Empty`), `ldfld` over the pushed
            // object argument for an instance one (a `ValueTuple`'s `Item1`).
            match objArg with
            | ValueNone ->
                let handle = env.Provider.ExternalFieldRef(key, ValueNone, ty)
                b.Add(ILInstr.Ldsfld handle)
            | ValueSome r ->
                let objArgTy = typeOfExpr r
                let handle = env.Provider.ExternalFieldRef(key, ValueSome objArgTy, ty)

                // An unboxed value-type object argument is reached by address; `ldfld` then
                // reads the field off that managed pointer.
                if isValueType env objArgTy then
                    loadStructThisPtr recur env b r objArgTy
                else
                    recur env b r

                b.Add(ILInstr.Ldfld handle)
        | MemberStorage.Property ->
            // A standalone external PROPERTY get: static (`call get_<name>()`) or instance
            // (`<objArg>; callvirt get_<name>()`). An external union/record object argument
            // routes through `ExternalMemberRefOn`, whose parent + arity come off its type.
            match objArg with
            | ValueNone ->
                let handle = env.Provider.ExternalMemberRef(key, true, true, ty)
                b.Add(ILInstr.Call(handle, 0, 1))
            | ValueSome r ->
                let objArgTy = typeOfExpr r
                let handle = externalInstanceMemberRef env key objArgTy true (ty)

                // A getter on an unboxed value-type object arg (`span.Length`) is reached by
                // address + non-virtual `call`, not by value + `callvirt`: the latter boxes,
                // and a ref struct cannot be boxed.
                if isValueType env objArgTy then
                    loadStructThisPtr recur env b r objArgTy
                    b.Add(ILInstr.Call(handle, 1, 1))
                else
                    recur env b r
                    b.Add(ILInstr.Callvirt(handle, 1, 1))
        | MemberStorage.Method ->
            // A method group needs closure synthesis, which is out of scope. An APPLIED external
            // method never reaches here; it is handled as an `App`'s applied function.
            failwith "Emit: external method used as a first-class value is out of scope"
