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

/// Application (`f a b …`) lowering: emit the head, then apply the residual
/// arguments through `Invoke`.
module EmitCall =

    /// Apply the remaining arguments to a `Vesper.Fun` value through its `Invoke`.
    let foldInvoke
        (recur: Recur)
        (env: EmitEnv)
        (b: IlBuilder)
        (funcTy0: FrozenType)
        (args: (TastAccessor.ExprId * FrozenType * Anchor) list)
        : unit =
        let mutable funcTy = funcTy0

        for (arg, resTy, _) in args do
            match env.Provider.TryEmitInvoke funcTy with
            | ValueSome recipe ->
                recur env b arg
                b.Add(ILInstr.Recipe recipe)
                funcTy <- resTy
            | ValueNone -> failwithf "Emit: cannot apply argument to Vesper.Fun value of type %A" funcTy

    /// `[| a; b |]` reaches codegen as `ArrayModule.OfList (Cons(a, Cons(b, Nil)))`; the
    /// BCL-only path has no FSharp.Core, so emit `newarr` + `dup; ldc i; <elem>; stelem`.
    /// `false` (and nothing emitted) unless the argument is that literal cons-chain.
    let private tryEmitArrayLiteral
        (recur: Recur)
        (env: EmitEnv)
        (b: IlBuilder)
        (arrTy: FrozenType)
        (appArgs: (TastAccessor.ExprId * FrozenType * Anchor) list)
        : bool =
        let elemOf =
            match arrTy with
            | FTArray elem -> ValueSome elem
            | _ -> ValueNone

        let rec collect (acc: TastAccessor.ExprId list) (e: TastAccessor.ExprId) : TastAccessor.ExprId list option =
            match TastAccessor.exprKind e with
            | ExprShape.UnionCons ->
                match TastAccessor.exprChildren e with
                | [| x; rest |] -> collect (x :: acc) rest
                | [||] -> Some(List.rev acc)
                | _ -> None
            | _ -> None

        match elemOf, appArgs with
        | ValueSome elem, [ (chain, _, _) ] ->
            match collect [] chain with
            | Some elems ->
                let elemTok = env.Provider.TypeToken elem
                b.Add(ILInstr.LdcI4 elems.Length)
                b.Add(ILInstr.Newarr elemTok)

                elems
                |> List.iteri (fun i el ->
                    b.Add ILInstr.Dup
                    b.Add(ILInstr.LdcI4 i)
                    recur env b el
                    b.Add(ILInstr.Stelem elemTok)
                )

                true
            | None -> false
        | _ -> false

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
        (leading: (TastAccessor.ExprId * FrozenType * Anchor) list)
        : FrozenType list =
        CompiledFns.flattenPlan groups (leading |> List.map (fun (a, _, _) -> a))
        |> pushFlatSteps recur env b

    /// Lower an `App` chain: dispatch on the head's shape, then apply any argument the
    /// head's own call did not consume through `Invoke`.
    let buildAppCall (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let head, appArgs = TastAccessor.collectAppChain [] e

        match head with
        | TastAccessor.EExternal ext ->
            if
                ext.CompiledName = RuntimeNames.arrayOfListName
                && tryEmitArrayLiteral recur env b (typeOfExpr e) appArgs
            then
                ()
            else

                let name = ext.CompiledName
                let key = ext.Key
                // The recipe's generic instantiation comes from the head's curried type,
                // which is stale once an argument became a value-struct closure — that leaf
                // still encodes to the `Fun`2` INTERFACE. Rebuild from the actual types.
                let recipeFnTy =
                    if
                        appArgs
                        |> List.exists (fun (arg, _, _) -> env.ClosureValueTypeByNode.ContainsKey arg)
                    then
                        // The tuple's middle element is the partial-application RESULT type
                        // at that step, not the argument's own type.
                        let argTys =
                            appArgs
                            |> List.map (fun (arg, _, _) ->
                                match env.ClosureValueTypeByNode.TryGetValue arg with
                                | true, closureFt -> closureFt
                                | false, _ -> typeOfExpr arg
                            )

                        List.foldBack (fun a acc -> FTFun(a, acc)) argTys (typeOfExpr e)
                    else
                        typeOfExpr head

                match env.Provider.TryEmitCall(name, key, recipeFnTy) with
                | ValueSome recipe ->
                    // `Grouped` splits one argument per SOURCE group and flattens each;
                    // `Flat` carries an already-flat count and pushes one-to-one.
                    let leading, rest =
                        match recipe.Arity with
                        | CallArity.Grouped(groups, _) ->
                            let leading, rest = List.splitAt (List.length groups) appArgs
                            flattenGroupPushes recur env b groups leading |> ignore
                            leading, rest
                        | CallArity.Flat argCount ->
                            let leading, rest = List.splitAt argCount appArgs

                            for (a, _, _) in leading do
                                recur env b a

                            leading, rest

                    b.Add(ILInstr.Recipe recipe)

                    // A `void` recipe (`Pushes = 0`) left nothing on the stack; reify a
                    // `unit` for the value-position consumer.
                    if recipe.Pushes = 0 then
                        EmitTypes.buildUnitValue env b

                    // The partial-application result at the last consumed argument — the
                    // type of the value `rest` is applied to.
                    let funcTy =
                        match List.tryLast leading with
                        | Some(_, ty, _) -> ty
                        | None -> typeOfExpr head

                    foldInvoke recur env b funcTy rest
                | ValueNone -> failwithf "Emit: no call recipe for external '%s'" name

        | TastAccessor.EVar k when env.StaticMethods.ContainsKey k ->
            // A top-level function emitted as a static method: `call` it with one
            // argument per SOURCE group, then `Invoke` the result with any remainder.
            let sm = env.StaticMethods.[k]
            let leading, rest = List.splitAt (List.length sm.Groups) appArgs
            let flatActualTys = flattenGroupPushes recur env b sm.Groups leading

            let callHandle =
                if sm.Typars = 0 then
                    sm.Handle
                else
                    // The instantiation is recovered by matching declared types against
                    // actual. Parameters alone may miss a typar — `zeroCreate: int -> 'T[]`
                    // carries `'T` only in its result — so a saturated call matches that too.
                    let defTys, actualTys =
                        match rest with
                        | [] -> sm.ParamTys @ [ sm.ResultTy ], flatActualTys @ [ typeOfExpr e ]
                        | _ -> sm.ParamTys, flatActualTys

                    let instArr = matchInstantiationPartial sm.Typars defTys actualTys

                    // A value-struct closure argument in a constrained `'TF :> Fun<_,_>`
                    // slot must instantiate `!TF` with the closure's own struct, not the
                    // function type — which encodes to the `Fun\`2` INTERFACE and boxes.
                    leading
                    |> List.iteri (fun i (arg, _, _) ->
                        match env.ClosureValueTypeByNode.TryGetValue arg with
                        | true, closureFt ->
                            if i < List.length sm.ParamTys then
                                match sm.ParamTys.[i] with
                                | FTTypar(TyparAxis.Method, idx) when idx >= 0 && idx < instArr.Length ->
                                    instArr.[idx] <- ValueSome closureFt
                                | _ -> ()
                        | false, _ -> ()
                    )

                    // A phantom typar — one in no parameter and no result, like `fold`'s
                    // enumerator `'E` in `'S :> IStructSeq<'T,'E>` — survives matching as
                    // `ValueNone`; solve it from the constraint's interface witness.
                    TastLower.solvePhantomTypars sm.Typars sm.Constraints (tryInterfaceWitness env) instArr

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

            // A `unit`-returning static fn is emitted `void`: the `call` declares 0
            // results, so reify a `unit` for a value-position consumer.
            let resultCount = if sm.ReturnsVoid then 0 else 1
            b.Add(ILInstr.Call(callHandle, List.length flatActualTys, resultCount))

            if sm.ReturnsVoid then
                EmitTypes.buildUnitValue env b

            foldInvoke recur env b sm.ResultTy rest

        | TastAccessor.EExternalMember em when em.Storage = MemberStorage.Method ->
            let receiver = em.Receiver
            let key = em.Key
            let name = em.MemberName
            let memberTy = typeOfExpr head
            // A .NET method is tupled — `m(a, b)` is ONE application to `(a, b)` — so the
            // call consumes a single argument, opened to the declared width.
            let isStatic = ValueOption.isNone receiver
            let argCount = SymbolKeyOps.memberArity "Emit: external member call" key

            let argList, rest =
                match appArgs with
                | first :: more -> ValueSome first, more
                | [] -> ValueNone, []

            // An instance method on an unboxed value-type receiver (`Span<char>`, any
            // external struct) is reached by address + non-virtual `call`; by value +
            // `callvirt` the verifier rejects, and a ref struct cannot even be boxed.
            let receiverIsStruct =
                match receiver with
                | ValueSome r -> isValueType env (typeOfExpr r)
                | ValueNone -> false

            match receiver with
            | ValueSome r when receiverIsStruct ->
                match r with
                | LocalSlot env slot -> b.Add(ILInstr.Ldloca slot)
                | _ ->
                    recur env b r
                    let tmp = b.Local(typeOfExpr r)
                    b.Add(ILInstr.Stloc tmp)
                    b.Add(ILInstr.Ldloca tmp)
            | ValueSome r -> recur env b r
            | ValueNone -> ()

            let pushedArgs =
                match argList with
                | ValueNone -> 0 // no argument supplied (a 0-param method)
                | ValueSome(argExpr, _, _) ->
                    CompiledFns.tupledMemberPlan (sprintf "Emit: external member '%s'" name) argCount argExpr
                    |> pushFlatSteps recur env b
                    |> List.length

            let handle =
                match receiver with
                | ValueSome r -> externalInstanceMemberRef env key (typeOfExpr r) false (memberTy)
                | ValueNone -> env.Provider.ExternalMemberRef(key, false, true, memberTy)

            let total = (if isStatic then 0 else 1) + pushedArgs

            // For a method returning a function value applied further, the result type is
            // the consumed `App` node's type.
            let resultTy =
                match argList with
                | ValueSome(_, ty, _) -> ty
                | ValueNone -> typeOfExpr head

            // An F# `unit` return is a .NET **void** method and must declare 0 results, or
            // the statement discard underflows on a phantom value. Void-ness comes from the
            // DECLARED codomain — `Span<char>.Fill(T)`'s applied node type is not `unit`.
            let returnsVoid =
                let declaredRet =
                    match memberTy with
                    | FTFun(_, r) -> r
                    | other -> other

                match declaredRet with
                | FTUnit -> true
                | _ -> false

            let resultCount = if returnsVoid then 0 else 1

            if isStatic || receiverIsStruct then
                b.Add(ILInstr.Call(handle, total, resultCount))
            else
                b.Add(ILInstr.Callvirt(handle, total, resultCount))

            if returnsVoid then
                EmitTypes.buildUnitValue env b

            foldInvoke recur env b resultTy rest

        | _ ->
            // The head is itself a function value — a closure local or a partial result.
            recur env b head
            foldInvoke recur env b (typeOfExpr head) appArgs
