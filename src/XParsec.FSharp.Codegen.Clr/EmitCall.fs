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

/// Application (`f a b …`) lowering and the curried-invoke fold. The head
/// dispatch is shape-by-shape (provider call recipe / static method / external
/// member / function value); the residual arguments are applied through `Invoke`.
module EmitCall =

    /// Apply remaining arguments to a native `Vesper.Fun` value via its `Invoke`,
    /// threading the running function type.
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

    /// An `[| … |]` array literal reaches codegen as `ArrayModule.OfList <chain>`
    /// where `<chain>` is the literal `Cons(e0, … Cons(e_{n-1}, Nil))` ElaborateExpr
    /// built (`RuntimeNames.arrayOfListName`). On the BCL-only path FSharp.Core's
    /// `ArrayModule.OfList` is absent, so emit the array inline: `newarr`, then
    /// `dup; ldc i; <elem>; stelem` per element, leaving the array on the stack.
    /// Returns `false` (emitting nothing) unless the head is this exact literal
    /// shape — a rank-1 array result over a literal cons-chain — so any other
    /// `Array.ofList` use falls through to the recipe path (its FSharp.Core
    /// binding) untouched.
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

    /// Push one `CompiledFns.FlatStep` as IL, returning each pushed value's actual type in
    /// order (for generic-instantiation matching; an external recipe call ignores them): a
    /// scalar arg pushed raw (the `obj` box is an explicit `Upcast` node from Elaborate), a
    /// tuple literal's elements pushed directly, a tuple value spilled to a local then each
    /// `ValueTuple` `Item` field read (left-to-right order preserved).
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

    /// Flatten a saturated call's leading arguments (one per SOURCE group) to its pushed CLR
    /// values. The lone-unit-erase / literal-vs-value tuple dispatch is
    /// `CompiledFns.flattenPlan`'s, shared with the JS backend.
    let private flattenGroupPushes
        (recur: Recur)
        (env: EmitEnv)
        (b: IlBuilder)
        (groups: TastAccessor.ArgGroup list)
        (leading: (TastAccessor.ExprId * FrozenType * Anchor) list)
        : FrozenType list =
        CompiledFns.flattenPlan groups (leading |> List.map (fun (a, _, _) -> a))
        |> pushFlatSteps recur env b

    /// Lower an `App` chain. The head dispatch is shape-by-shape:
    /// - an `External` node (compiled name + key) — a provider-resolved call. The
    ///   recipe's generic instantiation is read from the head's full curried
    ///   type. `key` (the Elaborate-stamped `SymbolKey.ValueKey`) lets codegen
    ///   route by identity, not name.
    /// - a `Var` node whose binding is in `env.StaticMethods` — a top-level
    ///   function emitted as a static method; generic instantiations are
    ///   recovered by matching declared param types against the actual arg types.
    /// - an `ExternalMember` node (`MemberStorage.Method`) — an
    ///   external method call; tupled per .NET convention, so the call consumes
    ///   one argument (the arg list) and the param count comes from the
    ///   key's `argSig` length.
    /// - otherwise — the head is itself a function value (a closure local or a
    ///   partially applied result); emit it, then `Invoke` each arg.
    let buildAppCall (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let head, appArgs = TastAccessor.collectAppChain [] e

        match head with
        | TastAccessor.EExternal ext ->
            // An `[| … |]` literal lowered to `ArrayModule.OfList <cons-chain>`
            // (ElaborateExpr) is emitted directly as newarr + stelem, so the BCL-only
            // path needs no FSharp.Core. `tryEmitArrayLiteral` commits IL and returns
            // true only when the argument is that literal cons-chain; any other shape
            // emits nothing, returns false, and falls through to the recipe path below.
            if
                ext.CompiledName = RuntimeNames.arrayOfListName
                && tryEmitArrayLiteral recur env b (typeOfExpr e) appArgs
            then
                ()
            else

                let name = ext.CompiledName
                let key = ext.Key
                // The recipe reads its generic instantiation from the head's full
                // curried type. `key` is the resolved `SymbolKey.ValueKey` stamped
                // by Elaborate when the front-end resolved the name through the symbol
                // provider — codegen routes by identity, not name suffix.
                //
                // When a SOURCE-LAMBDA argument lowered to a
                // value-struct closure (an external struct-seq combinator: `StructSeq.map`
                // / `fold`), the head's frozen type is stale for the instantiation recovery
                // — its `'TFunc` leaf is the front end's function type (→ the `Fun`2`/`Fun`3`
                // INTERFACE), and a chained `'S` source slot still carries the producing
                // transformer's function type rather than its already-rewritten `<closure>$`
                // value-struct. Reconstruct the recovery type from the ACTUAL (closure-
                // rewritten) argument types + result instead, overriding each value-
                // struct-closure position with its `<closure>$` nominal — the external
                // analogue of the project-local `StaticMethods`-arm override + rewritten
                // `actualTys`. Gated on the presence of a value-struct closure so every
                // existing external call keeps the (identical) head-type recovery.
                let recipeFnTy =
                    if
                        appArgs
                        |> List.exists (fun (arg, _, _) -> env.ClosureValueTypeByNode.ContainsKey arg)
                    then
                        // NOTE the argument tuple's middle element is the partial-application
                        // RESULT type at that step, not the argument's own type — read the
                        // argument type from `typeOfExpr arg` (already closure-rewritten by
                        // `ClosureVerdictRewrite` for a chained source slot).
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
                    // The argument split keys off the SOURCE-group count when the recipe
                    // carries one (`Grouped` — an external module function with a captured
                    // `ValRepr`): one argument per source group, then each
                    // group flattened to its pushed CLR values exactly as the in-assembly
                    // static-fn arm does. `Flat` pushes every leading element one-to-one.
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

                    // A `void` recipe (`Pushes = 0`, a now-`void` external module
                    // function) left nothing on the stack; reify a `unit` for the
                    // value-position result, as every other unit-returning call does.
                    // `rest` is empty for such a call (`unit` is not applicable), so the
                    // `foldInvoke` below is a no-op.
                    if recipe.Pushes = 0 then
                        EmitTypes.buildUnitValue env b

                    // Whatever the recipe left on the stack — a function value
                    // the rest of the arguments are applied to.
                    let funcTy =
                        match List.tryLast leading with
                        | Some(_, ty, _) -> ty
                        | None -> typeOfExpr head

                    // Whatever the recipe left is a native `Vesper.Fun` — apply the
                    // rest of the arguments through its `Invoke`.
                    foldInvoke recur env b funcTy rest
                | ValueNone -> failwithf "Emit: no call recipe for external '%s'" name

        | TastAccessor.EVar k when env.StaticMethods.ContainsKey k ->
            // A top-level function emitted as a static method: `call` it with
            // the first `ParamArity` args (always present — a non-saturated use would
            // have escaped to a closure, see `collectStaticFns`), then `Invoke`
            // the result with any remainder. A generic static method `call`s a
            // `MethodSpec` instantiating it — recovered by matching its declared
            // parameter types against the actual argument types (recursion yields
            // the method's own typars ⇒ `!!i`).
            let sm = env.StaticMethods.[k]
            // The argument split is driven by the SOURCE arity (`Groups.Length`): one
            // application per source group. `flattenGroupPushes` then expands each
            // group to its flat pushed values (a tupled group → N; a lone `()` → 0),
            // so the flat CLR arg count it returns can exceed `Groups.Length`.
            let leading, rest = List.splitAt (List.length sm.Groups) appArgs
            let flatActualTys = flattenGroupPushes recur env b sm.Groups leading

            let callHandle =
                if sm.Typars = 0 then
                    sm.Handle
                else
                    // The flat pushed-argument types, matched against the (flat)
                    // declared parameter types to recover the instantiation (by
                    // `FTTypar(Method, i)` index). Parameters alone may not mention
                    // every typar — e.g. `zeroCreate: int -> 'T[]` carries `'T` only
                    // in its result — so when the call is saturated (no further
                    // `Invoke`), also match the declared result against the call's
                    // actual result. First-occurrence-wins keeps the parameter
                    // matches authoritative.
                    let defTys, actualTys =
                        match rest with
                        | [] -> sm.ParamTys @ [ sm.ResultTy ], flatActualTys @ [ typeOfExpr e ]
                        | _ -> sm.ParamTys, flatActualTys

                    // Partial recovery — a PHANTOM
                    // constraint typar (`fold`'s enumerator `'E`, in no param/result)
                    // is unrecoverable by param-matching and stays `ValueNone`; it is
                    // solved below from `sm.Constraints`. The strict failwith moved
                    // to the post-solve finalize.
                    let instArr = matchInstantiationPartial sm.Typars defTys actualTys

                    // A captureless `Stack` (value-struct) lambda
                    // argument fed a bare method-typar parameter (the constrained
                    // `'TF :> Fun<_,_>` slot) must instantiate `!TF` with the
                    // closure's own struct `TypeDef`, NOT the function type (which encodes to
                    // the `Fun\`2` INTERFACE and would force a box). `matchInstantiation`
                    // bound that typar to the `FTFun(_,_)`; override it with the
                    // closure's synthetic value-type `FrozenType` so `constrained. !TF`
                    // targets the struct → JIT devirt, no box. The discovery gate runs
                    // only on all-`GSimple` callees, so the leading argument index
                    // maps one-to-one onto the flat parameter index.
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

                    // The call-site PHANTOM-typar solve.
                    // A phantom constraint typar (`fold`'s enumerator `'E` in
                    // `'S :> IStructSeq<'T,'E>`) is in no param/result, so it is still
                    // `ValueNone`; solve it from `sm.Constraints` via the project-local
                    // interface-impl witness (`env.Classes`). The closure rides in
                    // through `'S`'s rewritten arg — collision-free, no type-equality.
                    // The same solve serves the external module-fn call
                    // (`ClrRecipes.emitExternalCall`); only `tryWitness` differs.
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

            // A `unit`-returning static fn is emitted `void`: the `call`
            // declares 0 results and a `unit` value is reified for a value-position
            // consumer — the `unit → void` convention the instance path uses. `rest`
            // is empty for a void fn (`unit` is not applicable), so `foldInvoke` is a
            // no-op there.
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
            // An external instance/static method call: push the receiver (instance
            // only) beneath the arguments, then `call` (static) / `callvirt`
            // (instance) the keyed member ref. A .NET method is tupled
            // (`m(a, b)` = one application to `(a, b)`), so the call consumes a
            // single argument — the argument list — opened to the declared width.
            let isStatic = ValueOption.isNone receiver
            let argCount = SymbolKeyOps.memberArity "Emit: external member call" key

            // The method consumes one argument (its argument list); any
            // remainder is further application of the result (rare).
            let argList, rest =
                match appArgs with
                | first :: more -> ValueSome first, more
                | [] -> ValueNone, []

            // An instance method on an unboxed value-type receiver (a `Span<char>`
            // field/local, any external struct) must be reached by address + a
            // non-virtual `call`, not by value + `callvirt` (which the verifier
            // rejects — a ref struct can't even be boxed). Mirrors the local-struct
            // dispatch in `EmitMember.emitInstanceMember`: a slot-bound local is
            // addressed in place (`ldloca`), any other receiver expression is spilled
            // to a temp and addressed there (a struct copy is fine — Span methods read
            // through the copied (ptr,len), they don't mutate the struct itself).
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

            // The value→`obj` box for an `obj` parameter is an explicit `Upcast` node from
            // Elaborate (which wraps the tuple element-wise), so each element pushes raw.
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

            // A method returning a function value applied further (rare): the
            // result type is the consumed `App` node's type.
            let resultTy =
                match argList with
                | ValueSome(_, ty, _) -> ty
                | ValueNone -> typeOfExpr head

            // An external method whose F# return is `unit` is a .NET **void**
            // method (the unit→void mapping the member-ref signature encodes, same
            // as `IDisposable.Dispose` above): the `call`/`callvirt` pushes nothing,
            // so it must declare 0 results — modelling 1 leaves a phantom value the
            // statement discard underflows on (`InvalidProgramException`). Reify the
            // `unit` value afterwards so a value-position consumer still gets one,
            // exactly like the `for` / `stelem` unit expressions.
            //
            // Void-ness is read from the member's *declared* signature codomain
            // (`memberTy` is `paramsT → retT` for a .NET method), NOT the applied
            // node's type: a void instance method on a generic value-type receiver
            // (`Span<char>.Fill(T)`) can leave the applied node type un-grounded as a
            // non-`unit` placeholder, which mis-modelled it as result-bearing (the
            // `pop` then underflowed). The declared return is authoritative.
            let returnsVoid =
                let declaredRet =
                    match memberTy with
                    | FTFun(_, r) -> r
                    | other -> other

                match declaredRet with
                | FTUnit -> true
                | _ -> false

            let resultCount = if returnsVoid then 0 else 1

            // Static → `call`; value-type instance method → `call` on the receiver
            // address (non-virtual, the verifier-legal struct dispatch); reference
            // instance method → `callvirt`.
            if isStatic || receiverIsStruct then
                b.Add(ILInstr.Call(handle, total, resultCount))
            else
                b.Add(ILInstr.Callvirt(handle, total, resultCount))

            if returnsVoid then
                EmitTypes.buildUnitValue env b

            foldInvoke recur env b resultTy rest

        | _ ->
            // The head is itself a function value (a closure local or a
            // partially applied result): emit it, then `Invoke` each arg.
            recur env b head
            foldInvoke recur env b (typeOfExpr head) appArgs
