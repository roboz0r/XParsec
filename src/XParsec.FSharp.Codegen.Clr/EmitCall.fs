namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open EmitTypes
open EmitLower
open EmitResolve
open EmitPattern
open EmitDispatch

/// Application (`f a b …`) lowering: emit the applied function, then apply the
/// residual arguments through `Invoke`.
module EmitCall =

    /// Apply the remaining arguments to a `Vesper.Fun` value through its `Invoke`.
    let foldInvoke
        (recur: Recur)
        (env: EmitEnv)
        (b: IlBuilder)
        (funcTy0: FrozenType)
        (args: TastAccessor.AppliedArg list)
        : unit =
        let mutable funcTy = funcTy0

        for a in args do
            match env.Provider.TryEmitInvoke funcTy with
            | ValueSome recipe ->
                recur env b a.Arg
                b.Add(ILInstr.Recipe recipe)
                funcTy <- a.StepResultTy
            | ValueNone -> failwithf "Emit: cannot apply argument to Vesper.Fun value of type %A" funcTy

    /// Push each step as IL, returning the pushed values' actual types in order for
    /// generic-instantiation matching. A value→`obj` box is an explicit `Upcast` node
    /// from Elaborate, so every argument pushes raw.
    let private pushFlatSteps (recur: Recur) (env: EmitEnv) (b: IlBuilder) (steps: CompiledFns.FlatStep list) =
        let actualTys = ResizeArray<FrozenType>()

        for step in steps do
            match step with
            | CompiledFns.FlatStep.Arg a ->
                actualTys.Add(typeOfExpr a)
                recur env b a
            | CompiledFns.FlatStep.TupleLiteral elems ->
                for el in elems do
                    actualTys.Add(typeOfExpr el)
                    recur env b el
            | CompiledFns.FlatStep.TupleValue(a, elemTys) ->
                let refs = env.Provider.ValueTupleRefs elemTys
                let slot = b.Local(typeOfExpr a)

                recur env b a
                b.Add(ILInstr.Stloc slot)

                elemTys
                |> List.iteri (fun i ety ->
                    actualTys.Add ety
                    b.Add(ILInstr.Ldloc slot)
                    EmitPattern.emitTupleItemLoad b refs i
                )

        List.ofSeq actualTys

    /// Flatten a saturated call's leading arguments (one per SOURCE group) to its pushed
    /// CLR values: a tupled group pushes N, a lone `()` pushes 0.
    let private flattenGroupPushes
        (recur: Recur)
        (env: EmitEnv)
        (b: IlBuilder)
        (groups: TastAccessor.ArgGroup list)
        (leading: TastAccessor.AppliedArg list)
        : FrozenType list =
        CompiledFns.flattenPlan groups (leading |> List.map (fun a -> a.Arg))
        |> pushFlatSteps recur env b

    /// `call` a flat static method whose `pushed` arguments are on the stack, reify a `unit`
    /// result for a value-position consumer, then `Invoke` the result with `rest`.
    let private callFlatStatic
        (recur: Recur)
        (pos: ExprPos)
        (env: EmitEnv)
        (b: IlBuilder)
        (handle: EntityHandle)
        (pushed: int)
        (returnsVoid: bool)
        (resultTy: FrozenType)
        (rest: TastAccessor.AppliedArg list)
        : unit =
        let result = CallResult.ofReturnsVoid returnsVoid
        b.Add(ILInstr.Call(handle, pushed, result.Pushes))
        CallResult.reify env b pos result
        foldInvoke recur env b resultTy rest

    /// Lower an `App` chain: dispatch on the applied function's shape, then apply any
    /// argument its own call did not consume through `Invoke`.
    let buildAppCall (recur: Recur) (pos: ExprPos) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let fn, appArgs = TastAccessor.collectAppChain [] e

        match fn with
        | TastAccessor.EExternal key ->
            // The recipe's generic instantiation comes from the function's curried type,
            // which is stale once an argument became a value-struct closure, because that
            // argument still encodes to the `Fun`2` INTERFACE. Rebuild from the actual types.
            let recipeFnTy =
                if
                    appArgs
                    |> List.exists (fun a -> ValueOption.isSome (closureValueType env a.Arg))
                then
                    let argTys =
                        appArgs
                        |> List.map (fun a ->
                            match closureValueType env a.Arg with
                            | ValueSome closureFt -> closureFt
                            | ValueNone -> typeOfExpr a.Arg
                        )

                    List.foldBack (fun a acc -> FTFun(a, acc)) argTys (typeOfExpr e)
                else
                    typeOfExpr fn

            match env.Provider.TryEmitCall(key, recipeFnTy) with
            | ValueSome recipe ->
                // `Grouped` splits one argument per SOURCE group and flattens each;
                // `Flat` carries an already-flat count and pushes one-to-one.
                let leading, rest =
                    match recipe.Arity with
                    | CallArity.Grouped ps ->
                        let leading, rest = List.splitAt ps.GroupCount appArgs
                        flattenGroupPushes recur env b ps.Groups leading |> ignore
                        leading, rest
                    | CallArity.Flat argCount ->
                        let leading, rest = List.splitAt argCount appArgs

                        for a in leading do
                            recur env b a.Arg

                        leading, rest

                b.Add(ILInstr.Recipe recipe)

                // A `void` recipe (`Pushes = 0`) left nothing on the stack; reify a
                // `unit` for the value-position consumer.
                if recipe.Pushes = 0 then
                    ExprPos.reifyUnit env b pos

                let funcTy =
                    match List.tryLast leading with
                    | Some a -> a.StepResultTy
                    | None -> typeOfExpr fn

                foldInvoke recur env b funcTy rest
            | ValueNone -> failwithf "Emit: no call recipe for external '%s'" (SymbolKeyOps.qualifiedBindingName key)

        | TastAccessor.EVar k when env.LiftedLocals.ContainsKey k ->
            // A generalised local lifted to a generic static method. Its instantiation is the
            // caller's own leaves for every enclosing scope, then the local's typars recovered
            // from the arguments and from the reference's type, one `->` peeled per group.
            let ll = env.LiftedLocals.[k]

            for (ck, _) in ll.Captures do
                buildVarLoad env b ck

            let leading, rest = List.splitAt ll.Params.GroupCount appArgs
            let flatActualTys = flattenGroupPushes recur env b ll.Params.Groups leading

            let _, actualResultTy =
                TastLower.peelFunDomains ll.Params.GroupCount (typeOfExpr fn)

            let own =
                TastLower.matchScopeInstantiation
                    ll.Own.Scope
                    ll.Own.Count
                    (ll.Params.Flat @ [ ll.ResultTy ])
                    (flatActualTys @ [ actualResultTy ])

            let callHandle =
                env.Provider.StaticFnMethodSpec(ll.Handle, ll.Enclosing.Instantiation @ own)

            callFlatStatic
                recur
                pos
                env
                b
                callHandle
                (List.length ll.Captures + List.length flatActualTys)
                ll.ReturnsVoid
                ll.ResultTy
                rest

        | TastAccessor.EVar k when env.StaticMethods.ContainsKey k ->
            // A top-level function emitted as a static method: `call` it with one
            // argument per SOURCE group, then `Invoke` the result with any remainder.
            let sm = env.StaticMethods.[k]
            let leading, rest = List.splitAt sm.Params.GroupCount appArgs
            let flatActualTys = flattenGroupPushes recur env b sm.Params.Groups leading

            let callHandle =
                if sm.Scheme.TyparArity = 0<_> then
                    sm.Handle
                else
                    // The instantiation is recovered by matching declared types against
                    // actual. `zeroCreate: int -> 'T[]` carries `'T` only in its result, so
                    // parameters alone may miss a typar and a saturated call matches the
                    // result too.
                    let defTys, actualTys =
                        match rest with
                        | [] -> sm.Params.Flat @ [ sm.ResultTy ], flatActualTys @ [ typeOfExpr e ]
                        | _ -> sm.Params.Flat, flatActualTys

                    let instArr = matchInstantiationPartial sm.Scheme.TyparArity defTys actualTys

                    // A value-struct closure argument in a constrained `'TF :> Fun<_,_>`
                    // slot must instantiate `!TF` with the closure's own struct, not the
                    // function type, because that encodes to the `Fun\`2` INTERFACE and boxes.
                    // A closure fills a whole group, so its group's one flat slot is the typar.
                    List.iter2
                        (fun (a: TastAccessor.AppliedArg) (_, slotTys) ->
                            match closureValueType env a.Arg, slotTys with
                            | ValueSome closureFt, [ FTFunctionTypar idx ] when idx >= 0 && idx < instArr.Length ->
                                instArr.[idx] <- ValueSome closureFt
                            | _ -> ()
                        )
                        leading
                        sm.Params.ByGroup

                    // A phantom typar — one appearing only in a constraint, like `fold`'s
                    // enumerator `'E` in `'S :> IStructSeq<'T,'E>` — survives matching as
                    // `ValueNone`; solve it from the constraint's interface witness.
                    TastLower.solvePhantomTypars sm.Scheme.Typars (tryInterfaceWitness env) instArr

                    let inst =
                        [
                            for i in 0 .. instArr.Length - 1 ->
                                match instArr.[i] with
                                | ValueSome t -> t
                                | ValueNone ->
                                    failwithf
                                        "Emit: could not infer instantiation for static-method type parameter %d (phantom-typar solve found no witness)"
                                        i
                        ]

                    env.Provider.StaticFnMethodSpec(sm.Handle, inst)

            callFlatStatic recur pos env b callHandle (List.length flatActualTys) sm.ReturnsVoid sm.ResultTy rest

        | TastAccessor.EExternalMember em when em.Storage = MemberStorage.Method ->
            let objArg = em.ObjArg
            let key = em.Key
            let name = em.MemberName
            let memberTy = typeOfExpr fn
            let isStatic = ValueOption.isNone objArg
            let widths = em.ArgGroupWidths

            let plan =
                match CompiledFns.memberCallPlan widths appArgs with
                | ValueSome plan -> plan
                | ValueNone ->
                    failwithf
                        "Emit: external member '%s' takes argument groups %A, which its %d applied arguments do not fill"
                        name
                        widths
                        (List.length appArgs)

            // An instance method on an unboxed value-type object arg (`Span<char>`, any
            // external struct) is reached by address + non-virtual `call`; by value +
            // `callvirt` the verifier rejects, and a ref struct cannot even be boxed.
            let objArgIsStruct =
                match objArg with
                | ValueSome r -> isValueType env (typeOfExpr r)
                | ValueNone -> false

            match objArg with
            | ValueSome r when objArgIsStruct ->
                match r with
                | LocalSlot env slot -> b.Add(ILInstr.Ldloca slot)
                | _ ->
                    recur env b r
                    let tmp = b.Local(typeOfExpr r)
                    b.Add(ILInstr.Stloc tmp)
                    b.Add(ILInstr.Ldloca tmp)
            | ValueSome r -> recur env b r
            | ValueNone -> ()

            let pushedArgs = plan.Steps |> pushFlatSteps recur env b |> List.length

            let handle =
                match objArg with
                | ValueSome r -> externalInstanceMemberRef env key (typeOfExpr r) false (memberTy)
                | ValueNone -> env.Provider.ExternalMemberRef(key, false, true, memberTy)

            let total = (if isStatic then 0 else 1) + pushedArgs

            // What a method returning a function value applies its residual arguments to:
            // `memberTy` with one `->` peeled per group consumed.
            let resultTy = TastLower.peelFunDomains widths.Length memberTy |> snd

            // The DECLARED `void`-ness, read from the provider rather than derived from `resultTy`.
            let result = CallResult.ofReturnsVoid (env.Provider.ExternalMemberReturnsVoid key)

            if isStatic || objArgIsStruct then
                b.Add(ILInstr.Call(handle, total, result.Pushes))
            else
                b.Add(ILInstr.Callvirt(handle, total, result.Pushes))

            CallResult.reify env b pos result

            foldInvoke recur env b resultTy plan.Residual

        | _ ->
            // The applied expression is itself a function VALUE, a closure local or a
            // partial result, so there is no call recipe to dispatch to.
            recur env b fn
            foldInvoke recur env b (typeOfExpr fn) appArgs
