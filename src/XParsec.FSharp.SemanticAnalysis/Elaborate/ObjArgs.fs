namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaborateNominals

// The implicit value→`obj` upcast, and the per-site parameter models that feed it.
//
// The front end accepts a value / open typar flowing into an `obj` parameter
// *without grounding* the typar (Engine's obj-absorption rule). The box that
// upcast implies is made explicit here, at Elaborate, as a `TExpr.Upcast(arg,
// obj)` node — codegen's existing `buildUpcast` handler materialises the box
// (`box` for a value/typar source, a JIT no-op for a reference one). This is
// the single home for the box policy; codegen no longer re-derives it per
// emit site. The `*ParamTys` / `*FieldTy` producers supply each call/ctor/cons site's
// per-argument parameter SemTypes; `wrapObjArg`/`wrapObjArgsEq` apply the rule.

module internal ElaborateObjArgs =

    /// `obj` SemType for a synthesised `Upcast` target.
    let objTy: SemType = TyConst(RuntimeNames.objKey, EqArray.empty)

    let private isObjTy (store: TypeStore) (t: SemType) : bool =
        UnificationEngine.isObjType (Unification.zonk store t)

    /// Wrap an argument flowing into parameter `paramTy` in an explicit
    /// obj-`Upcast` when the parameter is the universal `obj` slot and the
    /// argument is not already obj (the latter only for tree cleanliness — an
    /// `Upcast(obj, obj)` would emit nothing anyway). A *tupled* multi-parameter
    /// slot — an external .NET method's flattened argument list arriving as a
    /// single `TExpr.Tuple` — wraps element-wise.
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
            // The box wraps an existing argument node; anchor the synthesised
            // `Upcast` at that argument's own source token.
            TExpr.Upcast(arg, objTy, TastWalk.exprTok arg)
        | _ -> arg

    /// Apply `wrapObjArg` per position over an arity-flattened argument array.
    /// Positions past the supplied `paramTys` (or an empty model — an external
    /// ctor / unknown member) are left raw.
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

    /// The declared parameter SemType (the `obj`-slot model) for an external
    /// method call, read from the `ResolvedExternalMember.Signature` Unification
    /// recorded at `fnKey` — a method's `TyFun(param → … → ret)` domain, fed
    /// straight to `wrapObjArg` (a multi-parameter method's domain is a `TyTuple`,
    /// which `wrapObjArg` wraps element-wise). This — not the call node's own
    /// SemType — is the box source for an external method: a deferred dot-access
    /// (`comparer.GetHashCode(x)`, receiver grounded only after the body) is typed
    /// by `unifyAppliedSig`, which leaves the node's argument position as the
    /// *un-grounded* argument typar (the obj-absorption rule never grounds
    /// `'T → obj`), so the `obj` slot is visible only on the recorded declared
    /// signature. `ValueNone` for a property (no `TyFun` domain) or a missing
    /// record (the call still emits — just unwrapped).
    let externalMethodParamTy (ctx: PassContext) (fnKey: NodeKey) : SemType voption =
        match ctx.Resolution.ExternalAccess.TryGetValue fnKey with
        | ValueSome info ->
            match Unification.zonk ctx.Store info.Signature with
            | TyFun(dom, _) -> ValueSome dom
            | _ -> ValueNone
        | ValueNone -> ValueNone

    /// The `obj`-slot model for a *residual* application head (the argument fold and
    /// the single-`HighPrecedenceApp` arm share this probe): an external .NET
    /// method head reads it off the recorded declared signature
    /// (`externalMethodParamTy`), since its node SemType is the un-grounded applied
    /// shape, not the function type. `ValueNone` for any non-external head, whose
    /// `obj` slots read off its function-type domain at the call site instead.
    let externalHeadDom (ctx: PassContext) (fnKey: NodeKey) (fnT: TExpr) : SemType voption =
        match fnT with
        | TExpr.ExternalMember(_, _, _, MemberStorage.Method, _, _) -> externalMethodParamTy ctx fnKey
        | _ -> ValueNone

    /// Per-argument parameter SemTypes for a *member* call, flattened to the
    /// arity-flattened argument list. A tupled member `M(a, b)` carries a single
    /// `TyTuple` parameter; `peelCtorArgs` flattens its call args to two, so the
    /// tuple is expanded element-wise here to keep the indices aligned.
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

    /// Parameter SemTypes for an instance/static member call resolved to
    /// `declKey.memberName`; empty when the member is unresolved (the call still
    /// emits — just unwrapped).
    let memberParamTys (ctx: PassContext) (declKey: TypeKey) (memberName: string) : SemType list =
        match tryNominalMemberByKey ctx declKey memberName with
        | ValueSome(_, m) -> flatMemberParams ctx.Store m.Type
        | ValueNone -> []

    /// Constructor parameter SemTypes for a project-local class construction of
    /// the given arity: the primary ctor when the arity matches its field count,
    /// else the arity-selected secondary ctor. Empty for an external ctor (no
    /// local param model — the provider recipe boxes).
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

    /// The declared SemType of a record field, for boxing a value flowing into an
    /// `obj` field. Answers for a LOCAL record (registry field model) AND an
    /// external / cross-file one (provider field shapes, instantiated at the receiver's
    /// type args), so `wrapObjArg` boxes an `obj`-typed field of a cross-file record
    /// exactly as it does a local one. The external arm is load-bearing: field-init now
    /// COERCES a value into an `obj` field (`InferRecordAccess`'s `unifyArg`), so a
    /// cross-file `{ X = v }` type-checks — without the box here it would emit invalid IL.
    /// `ValueNone` when the field is unknown or the shape is not a provider record.
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
                    match fieldShapes |> Array.tryFind (fun f -> f.Name = fieldName) with
                    | Some f -> ValueSome(FrozenTypeBridge.instantiateDeclaring f.Frozen (args.AsSpan().ToArray()))
                    | None -> ValueNone
                | _ -> ValueNone
            | _ -> ValueNone

    /// Field SemTypes of a union case, in declaration order — for boxing a
    /// value-typed argument flowing into an `obj` case field (the union-cons obj
    /// gap codegen could not close: `EmittedCase.Fields` carries only handles, not
    /// the field types Elaborate has here). Empty for an external union.
    let unionCaseFieldTys (ctx: PassContext) (unionTy: SemType) (caseName: string) : SemType list =
        match unionTy with
        | LocalUnion ctx info ->
            match info.Cases |> Array.tryFind (fun c -> c.Name = caseName) with
            | Some c -> List.ofArray c.Fields
            | None -> []
        | _ -> []
