namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
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

    /// Flatten a saturated call's leading spine (one element per SOURCE group) to its
    /// pushed CLR values, returning each pushed value's actual type in order (for
    /// generic-instantiation matching; an external recipe call ignores them). The
    /// lone-unit-erase / literal-vs-value tuple dispatch is `CompiledFns.flattenPlan`'s
    /// (shared with the JS backend); this interprets each `FlatStep` as IL: a scalar
    /// arg pushed raw (the `obj` box is an explicit `Upcast` node from Freeze), a tuple
    /// literal's elements pushed directly, a tuple value spilled to a local then each
    /// `ValueTuple` `Item` field read (left-to-right order preserved).
    let private flattenGroupPushes
        (recur: Recur)
        (env: EmitEnv)
        (b: IlBuilder)
        (groups: Frozen.ArgGroup list)
        (leading: (Frozen.TExpr * FrozenType * SyntaxToken) list)
        : FrozenType list =
        let actualTys = ResizeArray<FrozenType>()

        for step in CompiledFns.flattenPlan groups (leading |> List.map (fun (a, _, _) -> a)) do
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
                    b.Add(ILInstr.Ldfld refs.ItemFields.[i])
                )

        List.ofSeq actualTys

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
                // The spine split keys off the SOURCE-group count when the recipe
                // carries one (`Grouped` — an external module function with a captured
                // `ValRepr`, Step C): one spine element per source group, then each
                // group flattened to its pushed CLR values exactly as the in-assembly
                // static-fn arm does. `Flat` pushes every leading element one-to-one.
                let leading, rest =
                    match recipe.Arity with
                    | CallArity.Grouped(groups, _) ->
                        let leading, rest = List.splitAt (List.length groups) spineArgs
                        flattenGroupPushes recur env b groups leading |> ignore
                        leading, rest
                    | CallArity.Flat argCount ->
                        let leading, rest = List.splitAt argCount spineArgs

                        for (a, _, _) in leading do
                            recur env b a

                        leading, rest

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
            // application per source group. `flattenGroupPushes` then expands each
            // group to its flat pushed values (a tupled group → N; a lone `()` → 0),
            // so the flat CLR arg count it returns can exceed `Groups.Length`.
            let leading, rest = List.splitAt (List.length sm.Groups) spineArgs
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

                    // rung-4 §9 (Direction B): partial recovery — a PHANTOM
                    // constraint typar (`fold`'s enumerator `'E`, in no param/result)
                    // is unrecoverable by param-matching and stays `ValueNone`; it is
                    // solved below from `sm.Constraints`. The strict failwith moved
                    // to the post-solve finalize.
                    let instArr = matchInstantiationPartial sm.Typars defTys actualTys

                    // rung-4 Step C (M1): a captureless `Stack` (value-struct) lambda
                    // argument fed a bare method-typar parameter (the constrained
                    // `'TF :> Fun<_,_>` slot) must instantiate `!TF` with the
                    // closure's own struct `TypeDef`, NOT the arrow (which encodes to
                    // the `Fun\`2` INTERFACE and would force a box). `matchInstantiation`
                    // bound that typar to the arrow `FTFun(_,_)`; override it with the
                    // closure's synthetic value-type `FrozenType` so `constrained. !TF`
                    // targets the struct → JIT devirt, no box. The discovery gate runs
                    // only on all-`GSimple` callees, so the leading spine arg index
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

                    // rung-4 §9 (Direction B) — the call-site PHANTOM-typar solve.
                    // A phantom constraint typar (`fold`'s enumerator `'E` in
                    // `'S :> IStructSeq<'T,'E>`) is in no param/result, so it is still
                    // `ValueNone`. Solve it from `sm.Constraints`: for each
                    // `Coercion(ci, target)` whose CONSTRAINED typar `ci` is already
                    // resolved (e.g. `'S := instArr.[idx_S]`, ALREADY the node-key-
                    // rewritten `<closure>$`-bearing arg type), walk the constrained
                    // type's interface impl for `target`'s iface and structurally
                    // recover the typars `target` mentions (incl. `'E`) from the
                    // concrete witness. The closure rides in through `'S`'s rewritten
                    // arg — collision-free, no arrow-equality. Iterate to a fixpoint:
                    // one bound's target may mention a typar another bound just solved.
                    //
                    // The witness is AUTHORITATIVE for a bound-mentioned typar even if
                    // `matchInstantiation` already recovered it from the result: a
                    // combinator like `map` carries `'E` in BOTH its `'S :> IStructSeq<'T,'E>`
                    // bound AND its `… -> MapSeq<'S,'E,…>` result, and the RESULT occurrence
                    // can still embed a stale arrow (a chained source `s1`'s `'TFunc` buried
                    // in its frozen enumerator type), whereas the witness reads it from the
                    // source's ACTUAL `<closure>$`-bearing seq impl. So override every typar
                    // index that appears inside a Coercion target with the witness value.
                    if not (List.isEmpty sm.Constraints) then
                        // Indices the bounds can recover — those the witness may overwrite.
                        let boundMentioned = System.Collections.Generic.HashSet<int>()

                        let rec mention (t: FrozenType) =
                            match t with
                            | FTTypar(TyparAxis.Method, i) -> boundMentioned.Add i |> ignore
                            | FTFun(a, b) ->
                                mention a
                                mention b
                            | FTConst(_, xs)
                            | FTTuple xs
                            | FTRecord(_, xs)
                            | FTUnion(_, xs)
                            | FTClass(_, xs)
                            | FTOr xs -> EqArray.iter mention xs
                            | FTUnknown _
                            | FTTypar(TyparAxis.Declaring, _) -> ()

                        for c in sm.Constraints do
                            match c with
                            | FrozenConstraint.Coercion(_, target) -> mention target

                        let mutable changed = true

                        while changed do
                            changed <- false

                            for c in sm.Constraints do
                                match c with
                                | FrozenConstraint.Coercion(ci, target) ->
                                    if ci >= 0 && ci < instArr.Length then
                                        match instArr.[ci], target with
                                        | ValueSome receiver, FTClass(ifaceKey, _) ->
                                            match tryInterfaceWitness env receiver ifaceKey with
                                            | ValueSome witnessArgs ->
                                                let holes =
                                                    matchInstantiationPartial
                                                        sm.Typars
                                                        [ target ]
                                                        [ FTClass(ifaceKey, witnessArgs) ]

                                                for j in 0 .. instArr.Length - 1 do
                                                    match holes.[j] with
                                                    | ValueSome t when
                                                        instArr.[j] <> ValueSome t
                                                        && (instArr.[j] = ValueNone || boundMentioned.Contains j)
                                                        ->
                                                        instArr.[j] <- ValueSome t
                                                        changed <- true
                                                    | _ -> ()
                                            | ValueNone -> ()
                                        | _ -> ()

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

            // A `unit`-returning static fn is emitted `void` (Step B): the `call`
            // declares 0 results and a `unit` value is reified for a value-position
            // consumer — the `unit → void` convention the instance path uses. `rest`
            // is empty for a void fn (`unit` is not applicable), so `foldInvoke` is a
            // no-op there.
            let resultCount = if sm.ReturnsVoid then 0 else 1
            b.Add(ILInstr.Call(callHandle, List.length flatActualTys, resultCount))

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
