namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open EmitTypes
open EmitLower
open EmitResolve
open EmitPattern
open EmitDispatch

/// Application (`f a b …`) lowering and the curried-invoke fold. The head
/// dispatch is shape-by-shape (provider call recipe / static method / external
/// member / function value); the residual spine is applied through `Invoke`.
module EmitCall =

    /// The cold printf path (`printfn "%A"` …). Its `PrintFormatLine` recipe leaves
    /// an FSharp.Core `FSharpFunc` printer on the stack, applied via
    /// `FSharpFunc::Invoke` rather than `Vesper.Fun::Invoke`. Identity is keyed on
    /// the resolved `SymbolKey` (stamped by `Resolution.ExternalValue`), so a user
    /// `module MyMod = let printfn x = x` (project-local key) is correctly *not*
    /// treated as printf. The name fallback only fires for bare `"printfn"` from
    /// unkeyed call sites (test mocks / pre-key-pipeline paths).
    let private isColdPrintf (key: SymbolKey voption) (name: string) : bool =
        match key with
        | ValueSome k when PrintfSpec.isCanonicalPrintfn k -> true
        | _ -> name = "printfn"

    /// Apply each remaining argument to the function value on the stack,
    /// threading the running function type. `tryInvoke` chooses the invocation
    /// recipe per arg (`Vesper.Fun::Invoke` vs `FSharpFunc::Invoke`); `what`
    /// names the function kind for the failure diagnostic.
    let private foldInvokeWith
        (recur: Recur)
        (tryInvoke: FrozenType -> CallRecipe voption)
        (what: string)
        (env: EmitEnv)
        (b: IlBuilder)
        (funcTy0: FrozenType)
        (args: (Frozen.TExpr * FrozenType * SyntaxToken) list)
        : unit =
        let mutable funcTy = funcTy0

        for (arg, resTy, _) in args do
            match tryInvoke funcTy with
            | ValueSome recipe ->
                recur env b arg
                b.Add(ILInstr.Recipe recipe)
                funcTy <- resTy
            | ValueNone -> failwithf "Emit: cannot apply argument to %s value of type %A" what funcTy

    /// Apply remaining arguments to a native `Vesper.Fun` value via its `Invoke`.
    let foldInvoke
        (recur: Recur)
        (env: EmitEnv)
        (b: IlBuilder)
        (funcTy0: FrozenType)
        (args: (Frozen.TExpr * FrozenType * SyntaxToken) list)
        : unit =
        foldInvokeWith recur env.Provider.TryEmitInvoke "Vesper.Fun" env b funcTy0 args

    /// Apply a curried FSharp.Core `FSharpFunc` value (the cold printf printer)
    /// argument by argument via `FSharpFunc::Invoke` — the FSharpFunc twin of
    /// `foldInvoke` retargeted with the printf engine.
    let private foldInvokeFSharpFunc
        (recur: Recur)
        (env: EmitEnv)
        (b: IlBuilder)
        (funcTy0: FrozenType)
        (args: (Frozen.TExpr * FrozenType * SyntaxToken) list)
        : unit =
        foldInvokeWith recur env.Provider.TryEmitFSharpFuncInvoke "FSharpFunc" env b funcTy0 args

    /// An `[| … |]` array literal reaches codegen as `ArrayModule.OfList <chain>`
    /// where `<chain>` is the literal `Cons(e0, … Cons(e_{n-1}, Nil))` FreezeExpr
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
        (spineArgs: (Frozen.TExpr * FrozenType * SyntaxToken) list)
        : bool =
        let elemOf =
            match arrTy with
            | FTConst(n, args) when n = RuntimeNames.arrayName 1 ->
                match EqArray.toList args with
                | [ elem ] -> ValueSome elem
                | _ -> ValueNone
            | _ -> ValueNone

        let rec collect (acc: Frozen.TExpr list) (e: Frozen.TExpr) : Frozen.TExpr list option =
            match e with
            | TExprG.UnionCons(_, args, _, _) ->
                match EqArray.toList args with
                | [ x; rest ] -> collect (x :: acc) rest
                | [] -> Some(List.rev acc)
                | _ -> None
            | _ -> None

        match elemOf, spineArgs with
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

    /// Lower a `TExprG.App` chain. The head dispatch is shape-by-shape:
    /// - `TExprG.External(name, key, _)` — a provider-resolved call. The
    ///   recipe's generic instantiation is read from the head's full curried
    ///   type. `key` (the Freeze-stamped `SymbolKey.ValueKey`) lets codegen
    ///   route by identity, not name.
    /// - `TExprG.Var k` where `env.StaticMethods.ContainsKey k` — a top-level
    ///   function emitted as a static method; generic instantiations are
    ///   recovered by matching declared param types against the actual arg types.
    /// - `TExprG.ExternalMember(receiver, key, name, false, memberTy)` — an
    ///   external method call; tupled per .NET convention, so the call consumes
    ///   one spine element (the arg list) and the param count comes from the
    ///   key's `argSig` length.
    /// - otherwise — the head is itself a function value (a closure local or a
    ///   partially applied result); emit it, then `Invoke` each arg.
    let buildAppCall (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        let head, spineArgs = TastWalk.collectSpine [] e

        match head with
        | TExprG.External(name, _, _, _) when
            name = RuntimeNames.arrayOfListName
            && tryEmitArrayLiteral recur env b (typeOfExpr e) spineArgs
            ->
            // Handled in the guard: an `[| … |]` literal lowered to
            // `ArrayModule.OfList <cons-chain>` (FreezeExpr) emitted directly as
            // newarr + stelem, so the BCL-only path needs no FSharp.Core. The guard
            // only commits when the spine arg is the literal cons-chain FreezeExpr
            // builds; any other shape falls through to the recipe path below.
            ()
        | TExprG.External(name, key, _, _) ->
            // The recipe reads its generic instantiation from the head's full
            // curried type. `key` is the resolved `SymbolKey.ValueKey` stamped
            // by Freeze when the front-end resolved the name through the symbol
            // provider — codegen routes by identity, not name suffix.
            match env.Provider.TryEmitCall(name, key, typeOfExpr head) with
            | ValueSome recipe ->
                let leading, rest = List.splitAt recipe.ArgCount spineArgs

                // When the recipe carries the callee's SOURCE grouping (an external
                // module function with a captured `ValRepr`, Step C), `leading` holds
                // one spine element per source group; flatten each group to its pushed
                // CLR values exactly as the in-assembly static-fn arm does (a tupled
                // group → push each element; a lone `()` group → push nothing). Without
                // `Groups`, every leading element is pushed one-to-one.
                match recipe.Groups with
                | ValueSome groups ->
                    let isLoneUnit =
                        match groups with
                        | [ ArgGroupG.GUnit _ ] -> true
                        | _ -> false

                    List.iter2
                        (fun (g: Frozen.ArgGroup) (a, _, _) ->
                            match g with
                            | ArgGroupG.GUnit _ when isLoneUnit -> () // erased — push nothing
                            | ArgGroupG.GUnit _
                            | ArgGroupG.GSimple _ -> recur env b a
                            | ArgGroupG.GTuple _ ->
                                match a with
                                | TExprG.Tuple(elems, _, _) ->
                                    for el in EqArray.toList elems do
                                        recur env b el
                                | _ ->
                                    let elemTys =
                                        match typeOfExpr a with
                                        | FTTuple xs -> EqArray.toList xs
                                        | other -> failwithf "Emit: tuple-group argument is not a tuple type: %A" other

                                    let refs = env.Provider.ValueTupleRefs elemTys
                                    let slot = b.Local(typeOfExpr a)

                                    recur env b a
                                    b.Add(ILInstr.Stloc slot)

                                    elemTys
                                    |> List.iteri (fun i _ ->
                                        b.Add(ILInstr.Ldloc slot)
                                        b.Add(ILInstr.Ldfld refs.ItemFields.[i])
                                    )
                        )
                        groups
                        leading
                | ValueNone ->
                    for (a, _, _) in leading do
                        recur env b a

                b.Add(ILInstr.Recipe recipe)

                // A `void` recipe (`Pushes = 0`, a now-`void` external module
                // function, Step B) left nothing on the stack; reify a `unit` for the
                // value-position result, as every other unit-returning call does.
                // `rest` is empty for such a call (`unit` is not applicable), so the
                // `foldInvoke` below is a no-op.
                if recipe.Pushes = 0 then
                    EmitTypes.buildUnitValue env b

                // Whatever the recipe left on the stack — a function value
                // the rest of the spine is applied to.
                let funcTy =
                    match List.tryLast leading with
                    | Some(_, ty, _) -> ty
                    | None -> typeOfExpr head

                // The cold printf printer is an FSharp.Core `FSharpFunc`, so it
                // is applied via `FSharpFunc::Invoke`; every other recipe result
                // is a native `Vesper.Fun`.
                if isColdPrintf key name then
                    foldInvokeFSharpFunc recur env b funcTy rest
                else
                    foldInvoke recur env b funcTy rest
            | ValueNone -> failwithf "Emit: no call recipe for external '%s'" name

        | TExprG.Var(k, _, _) when env.StaticMethods.ContainsKey k ->
            // A top-level function emitted as a static method: `call` it with
            // the first `Arity` args (always present — a non-saturated use would
            // have escaped to a closure, see `collectStaticFns`), then `Invoke`
            // the result with any remainder. A generic static method `call`s a
            // `MethodSpec` instantiating it — recovered by matching its declared
            // parameter types against the actual argument types (recursion yields
            // the method's own typars ⇒ `!!i`).
            let sm = env.StaticMethods.[k]
            // The spine split is driven by the SOURCE arity (`Groups.Length`): one
            // application per source group. Tuple flattening then expands a tupled
            // group's single argument into N pushed values, so the *flat* CLR arg
            // count (`sm.Arity`) can exceed `Groups.Length`.
            let leading, rest = List.splitAt (List.length sm.Groups) spineArgs

            // A LONE unit group (`let f () = …`) compiles to a parameterless method
            // (`compiledOf`'s `[GUnit] → []` erasure); its `()` argument is dropped.
            let isLoneUnit =
                match sm.Groups with
                | [ ArgGroupG.GUnit _ ] -> true
                | _ -> false

            // Flatten each source group's spine argument to its pushed CLR values,
            // recording each value's actual type for generic-instantiation matching.
            // The value→obj box for an `obj` parameter is an explicit `Upcast` node
            // from Freeze, so each scalar argument is pushed raw.
            let flatActualTys = ResizeArray<FrozenType>()
            let pushes = ResizeArray<unit -> unit>()

            List.iter2
                (fun (g: Frozen.ArgGroup) (a, _, _) ->
                    match g with
                    | ArgGroupG.GUnit _ when isLoneUnit -> () // erased — push nothing
                    | ArgGroupG.GUnit _
                    | ArgGroupG.GSimple _ ->
                        flatActualTys.Add(typeOfExpr a)
                        pushes.Add(fun () -> recur env b a)
                    | ArgGroupG.GTuple _ ->
                        // A tupled source group flattens to N flat params (full F#,
                        // one level): a literal `Tuple(a, b)` pushes each element
                        // directly; a tuple *value* spills to a local and pushes each
                        // `ValueTuple` `Item` field.
                        match a with
                        | TExprG.Tuple(elems, _, _) ->
                            for el in EqArray.toList elems do
                                flatActualTys.Add(typeOfExpr el)
                                pushes.Add(fun () -> recur env b el)
                        | _ ->
                            let elemTys =
                                match typeOfExpr a with
                                | FTTuple xs -> EqArray.toList xs
                                | other -> failwithf "Emit: tuple-group argument is not a tuple type: %A" other

                            let refs = env.Provider.ValueTupleRefs elemTys
                            let slot = b.Local(typeOfExpr a)

                            // Spill the tuple value once (pushes nothing net), then a
                            // load thunk per element preserves left-to-right order.
                            pushes.Add(fun () ->
                                recur env b a
                                b.Add(ILInstr.Stloc slot)
                            )

                            elemTys
                            |> List.iteri (fun i ety ->
                                flatActualTys.Add ety

                                pushes.Add(fun () ->
                                    b.Add(ILInstr.Ldloc slot)
                                    b.Add(ILInstr.Ldfld refs.ItemFields.[i])
                                )
                            )
                )
                sm.Groups
                leading

            for push in pushes do
                push ()

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
                    let paramActualTys = List.ofSeq flatActualTys

                    let defTys, actualTys =
                        match rest with
                        | [] -> sm.ParamTys @ [ sm.ResultTy ], paramActualTys @ [ typeOfExpr e ]
                        | _ -> sm.ParamTys, paramActualTys

                    let inst = matchInstantiation sm.Typars defTys actualTys
                    env.Provider.StaticFnMethodSpec(sm.Handle, inst)

            // A `unit`-returning static fn is emitted `void` (Step B): the `call`
            // declares 0 results and a `unit` value is reified for a value-position
            // consumer — the `unit → void` convention the instance path uses. `rest`
            // is empty for a void fn (`unit` is not applicable), so `foldInvoke` is a
            // no-op there.
            let resultCount = if sm.ReturnsVoid then 0 else 1
            b.Add(ILInstr.Call(callHandle, flatActualTys.Count, resultCount))

            if sm.ReturnsVoid then
                EmitTypes.buildUnitValue env b

            foldInvoke recur env b sm.ResultTy rest

        | TExprG.ExternalMember(receiver, key, name, false, memberTy, _) ->
            // An external instance/static method call: push the receiver (instance
            // only) beneath the arguments, then `call` (static) / `callvirt`
            // (instance) the keyed member ref. A .NET method is tupled
            // (`m(a, b)` = one application to `(a, b)`), so the call consumes a
            // single spine element — the argument list — and the parameter count
            // comes from the chosen key's `argSig` length (authoritative: `memberTy`
            // alone can't tell a flattened 2-param method from a genuine single
            // `(int*int)` param). A literal `TExprG.Tuple` argument is pushed
            // element-wise (no tuple object is constructed).
            let isStatic = ValueOption.isNone receiver

            let argSig =
                match key with
                | SymbolKey.MemberKey(_, _, argSig, _) -> argSig
                | other -> failwithf "Emit: ExternalMember key is not a MemberKey: %A" other

            let argCount = argSig.Length

            // The method consumes one spine element (its argument list); any
            // remainder is further application of the result (rare).
            let argList, rest =
                match spineArgs with
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
                | TExprG.Var(binding, _, _) when env.Slots.ContainsKey binding ->
                    b.Add(ILInstr.Ldloca env.Slots.[binding])
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
                    if argCount >= 2 then
                        // A multi-param .NET method is tupled; its arguments are
                        // pushed element-wise. The value→`obj` box for an `obj`
                        // parameter is an explicit `Upcast` node from Freeze (which
                        // wraps the tuple element-wise), so push each element raw.
                        match argExpr with
                        | TExprG.Tuple(elems, _, _) when elems.Length = argCount ->
                            for el in elems do
                                recur env b el

                            argCount
                        | _ ->
                            failwithf
                                "Emit: external member '%s' expects %d tupled arguments but the argument is not a literal %d-tuple"
                                name
                                argCount
                                argCount
                    elif argCount = 1 then
                        recur env b argExpr
                        1
                    else
                        // argCount = 0: a `unit → ret` method; the lone arg is
                        // `()`, which has no IL value to push.
                        0

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
            // spine type: a void instance method on a generic value-type receiver
            // (`Span<char>.Fill(T)`) can leave the applied node type un-grounded as a
            // non-`unit` placeholder, which mis-modelled it as result-bearing (the
            // `pop` then underflowed). The declared return is authoritative.
            let returnsVoid =
                let declaredRet =
                    match memberTy with
                    | FTFun(_, r) -> r
                    | other -> other

                match declaredRet with
                | FTConst("unit", _) -> true
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
            foldInvoke recur env b (typeOfExpr head) spineArgs
