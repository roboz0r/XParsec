namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaborateNominals

// The implicit value→`obj` upcast. A value or open typar passed to an `obj`
// parameter type-checks *without grounding* the typar, so the box that upcast implies
// is made explicit here, at Elaborate, as a `TExpr.Upcast(arg, obj)` node.

module internal ElaborateObjArgs =

    let objTy: SemType = TyConst(RuntimeNames.objKey, EqArray.empty)

    let private isObjTy (store: TypeStore) (t: SemType) : bool =
        UnificationEngine.isObjType (Unification.zonk store t)

    /// Wrap an argument passed to parameter `paramTy` in an explicit obj-`Upcast` when
    /// the parameter is the universal `obj` slot and the argument is not already obj. A
    /// tupled parameter facing a `TExpr.Tuple` argument wraps element-wise.
    let rec wrapObjArg (store: TypeStore) (paramTy: SemType) (arg: TExpr) : TExpr =
        match Unification.zonk store paramTy with
        | TyTuple ptys ->
            match arg with
            | TExpr.Tuple(elems, tupTy, tupTok) when ptys.Length = elems.Length ->
                TExpr.Tuple(
                    EqArray.ofSeq (seq { for i in 0 .. elems.Length - 1 -> wrapObjArg store ptys.[i] elems.[i] }),
                    tupTy,
                    tupTok
                )
            | _ -> arg
        | zParam when UnificationEngine.isObjType zParam && not (isObjTy store (TastWalk.exprTy arg)) ->
            TExpr.Upcast(arg, objTy, TastWalk.exprTok arg)
        | _ -> arg

    /// Positions past the supplied `paramTys`, and every position when it is empty (an
    /// external ctor, an unknown member), are left unwrapped.
    let wrapObjArgsEq (store: TypeStore) (paramTys: SemType list) (args: EqArray<TExpr>) : EqArray<TExpr> =
        if List.isEmpty paramTys then
            args
        else
            let ptys = List.toArray paramTys

            EqArray.ofSeq (
                seq {
                    for i in 0 .. args.Length - 1 ->
                        if i < ptys.Length then
                            wrapObjArg store ptys.[i] args.[i]
                        else
                            args.[i]
                }
            )

    /// The declared parameter SemType for an external method call, off the recorded
    /// signature's `TyFun` domain. The call node's own SemType will not do: obj-absorption
    /// leaves its argument position an un-grounded typar, so the `obj` slot shows only here.
    let externalMethodParamTy (ctx: PassContext) (fnKey: NodeKey) : SemType voption =
        match ctx.Resolution.ExternalAccess.TryGetValue fnKey with
        | ValueSome info ->
            match Unification.zonk ctx.Store info.Signature with
            | TyFun(dom, _) -> ValueSome dom
            | _ -> ValueNone
        | ValueNone -> ValueNone

    /// The `obj`-slot model for an applied function: an external .NET method reads it off
    /// its recorded declared signature. `ValueNone` for anything else, whose `obj` slots
    /// come from its own function-type domain at the call site.
    let externalFnDom (ctx: PassContext) (fnKey: NodeKey) (fnT: TExpr) : SemType voption =
        match fnT with
        | TExpr.ExternalMember(_, _, _, MemberStorage.Method, _, _, _) -> externalMethodParamTy ctx fnKey
        | _ -> ValueNone

    /// A tupled member `M(a, b)` carries ONE `TyTuple` parameter, but its call arguments
    /// arrive already flattened to two, so the tuple is expanded element-wise here to keep
    /// the two index spaces aligned.
    let private flatMemberParams (store: TypeStore) (memberTy: SemType) : SemType list =
        let rec peelFuns t =
            match Unification.zonk store t with
            | TyFun(a, b) ->
                let ps, r = peelFuns b
                a :: ps, r
            | other -> [], other

        match peelFuns memberTy with
        | [ single ], _ ->
            match Unification.zonk store single with
            | TyTuple elems -> EqArray.toList elems
            | other -> [ other ]
        | ps, _ -> ps

    /// The declared parameter types of the project-local member `key` names, selected by the
    /// key's OWN argSig so one of two same-name overloads cannot answer for the other. Empty
    /// for an external or unresolved member, whose call still emits, just unwrapped.
    let memberParamTys (ctx: PassContext) (key: SymbolKey) : SemType list =
        match key with
        | SymbolKey.Member mk ->
            match TypeRegistry.tryNominalByKey ctx.Types mk.Decl with
            | ValueSome decl ->
                let chosen =
                    match decl.Members |> Array.filter (fun m -> m.Name = mk.Name) with
                    | [| only |] -> Some only
                    | overloads ->
                        overloads
                        |> Array.tryFind (fun m ->
                            UnificationInferOverload.freezeUserMemberArgSig ctx.Store decl.TypeParams m = mk.ArgSig
                        )

                match chosen with
                | Some m -> flatMemberParams ctx.Store m.Type
                | None -> []
            | ValueNone -> []
        | _ -> []

    /// The primary ctor when `argCount` matches its parameter count, else the secondary
    /// ctor of that arity. Empty for an external ctor, which has no local param model.
    let ctorParamTys (ctx: PassContext) (classTy: SemType) (argCount: int) : SemType list =
        match classTy with
        | LocalClass ctx info ->
            if argCount = info.CtorParams.Length then
                [ for p in info.CtorParams -> p.Type ]
            else
                match info.SecondaryCtors |> Array.tryFind (fun sc -> sc.Params.Length = argCount) with
                | Some sc -> [ for p in sc.Params -> p.Type ]
                | None -> []
        | _ -> []

    /// The declared SemType of a record field, for boxing a value assigned to an `obj`
    /// field. The external / cross-file arm is load-bearing: inference COERCES a value into
    /// an `obj` field, so without the box a cross-file `{ X = v }` emits invalid IL.
    let recordFieldTy (ctx: PassContext) (recordTy: SemType) (fieldName: string) : SemType voption =
        match recordTy with
        | LocalRecord ctx info ->
            match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
            | Some f -> ValueSome f.Type
            | None -> ValueNone
        | _ ->
            match Unification.zonk ctx.Store recordTy with
            | TyRecord(key, args) ->
                match ctx.Provider.TryLookupType(SymbolKey.Type key) with
                | ValueSome(ExternalTypeShape.Record(_, fieldShapes, _)) ->
                    match fieldShapes |> EqArray.tryFind (fun f -> f.Name = fieldName) with
                    | ValueSome f -> ValueSome(FrozenTypeBridge.instantiateDeclaring f.Frozen (args.AsSpan().ToArray()))
                    | ValueNone -> ValueNone
                | _ -> ValueNone
            | _ -> ValueNone

    /// Field SemTypes of a union case, in declaration order, for boxing a value assigned to
    /// an `obj` case field. Empty for an external union.
    let unionCaseFieldTys (ctx: PassContext) (unionTy: SemType) (caseName: string) : SemType list =
        match unionTy with
        | LocalUnion ctx info ->
            match info.Cases |> Array.tryFind (fun c -> c.Name = caseName) with
            | Some c -> List.ofArray c.Fields
            | None -> []
        | _ -> []
