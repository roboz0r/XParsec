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

/// Object / value construction: `new`, record literals + `{ r with … }`, union
/// case construction, tuple values, and the closure for a `Lambda` value.
module EmitConstruct =

    let buildNew (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let className = TastAccessor.exprNewClassName e
        let chosenCtor = TastAccessor.exprNewChosenCtor e
        let args = TastAccessor.exprChildrenBlock e
        let ty = TastAccessor.exprTy e

        let tyArgs =
            match ty with
            | FTClass(_, xs) -> xs
            | _ -> Block.empty

        // Both ctor paths filter candidates by arity off these, then disambiguate a same-arity
        // set by type: the external one through `chosenCtor`, the local one through
        // `pickLocalCtor`.
        let argTypes = args |> Block.map typeOfExpr

        // A project-local class is the `FTClass` key that `env.Classes` knows; anything
        // else is an external ctor, resolved through the provider.
        let localClass =
            match ty with
            | FTClass(k, _) ->
                match env.Classes.TryGetValue k with
                | true, c -> ValueSome(k, c)
                | _ -> ValueNone
            | _ -> ValueNone

        let argCount = args.Length

        // Parameterless value-type construction (`Counter()`, `Span<char>()`) pushes no
        // arguments and emits no `newobj`: it is `initobj` on a zeroed scratch local.
        // `Span<char>` has no parameterless ctor to call; it is `default(Span<char>)`.
        let isInitObj =
            argCount = 0
            && (
                match localClass with
                | ValueSome(_, c) -> c.IsValueType
                | ValueNone -> isValueType env ty
            )

        if isInitObj then
            let slot = b.Local ty
            b.Add(ILInstr.Ldloca slot)
            b.Add(ILInstr.Initobj(env.Provider.TypeToken ty))
            b.Add(ILInstr.Ldloc slot)
        else
            // Resolve the construction to the instruction that consumes the pushed args,
            // THEN push, below. A value→`obj` box is an explicit `Upcast` node from
            // Elaborate, so each argument pushes raw.
            let emitNewobj =
                match localClass with
                | ValueSome(classKey, c) ->
                    let kind, handle = pickLocalCtor className c tyArgs argTypes
                    let ctorRef = memberRef env.Provider c.TypeArity classKey tyArgs kind handle
                    fun () -> b.Add(ILInstr.Newobj(ctorRef, argCount))
                | ValueNone ->
                    // An external ctor is identified by the construction's result-type key;
                    // `className` survives only for the error message.
                    fun () ->
                        match ty with
                        | FTClass(ctorKey, _) ->
                            match env.Provider.TryEmitCtor(ctorKey, chosenCtor, tyArgs, argTypes) with
                            | ValueSome recipe -> b.Add(ILInstr.Newobj(recipe.Handle, recipe.ArgCount))
                            | ValueNone -> failwithf "Emit: no constructor recipe for '%s'" className
                        // `new exn "boom"` types as the canon `FTConst`, not a class:
                        // resolve it to its platform key (`System.Exception`) and mint
                        // the ctor there.
                        | FTConst(canonKey, args) when args.IsEmpty ->
                            let recipe =
                                env.Provider.IntrinsicClassBase canonKey
                                |> ValueOption.bind (fun (struct (platformKey, _)) ->
                                    env.Provider.TryEmitCtor(platformKey, chosenCtor, tyArgs, argTypes)
                                )

                            match recipe with
                            | ValueSome recipe -> b.Add(ILInstr.Newobj(recipe.Handle, recipe.ArgCount))
                            | ValueNone -> failwithf "Emit: no constructor recipe for '%s'" className
                        | _ -> failwithf "Emit: no constructor recipe for '%s'" className

            for a in args do
                recur env b a

            emitNewobj ()

    let buildRecordCons (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let srcFields = TastAccessor.exprRecordConsFields e
        let nominal = nominalOfExpr e
        let key, tyArgs = nominal.Key, nominal.Args

        match env.Records.TryGetValue key with
        | true, r ->
            let srcMap = Map.ofSeq srcFields

            // A source-order initialiser list (`{ Y = …; X = … }`) pushes in the type's
            // DECLARATION order, which is the ctor's parameter layout.
            for f in r.Fields do
                match Map.tryFind f.Name srcMap with
                | Some e -> recur env b e
                | None -> failwithf "Emit: record literal for '%A' is missing initialiser for field '%s'" key f.Name

            let ctor =
                memberRef env.Provider r.TypeArity key tyArgs (UserMemberKind.RecordMember RecordMember.Ctor) r.Ctor

            b.Add(ILInstr.Newobj(ctor, List.length r.Fields))
        | false, _ ->
            let qualName = SymbolKeyOps.typeMetaName key
            // A record in a referenced assembly pushes in SOURCE order: the only such
            // shape supported is the one-field `Ref<'T>`, where the two coincide.
            let fieldNames = Block.ofList [ for (n, _) in srcFields -> n ]

            match env.Provider.TryEmitRecordCons(key, tyArgs, fieldNames) with
            | ValueSome recipe ->
                for (_, e) in srcFields do
                    recur env b e

                b.Add(ILInstr.Newobj(recipe.Handle, recipe.ArgCount))
            | ValueNone -> failwithf "Emit: no emitted record for '%s'" qualName

    let buildRecordClone (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let cloneView = TastAccessor.exprRecordClone e
        let source = cloneView.Source
        let overrides = cloneView.Overrides

        // `{ r with X = v }` — spill `r` to a local, then per declaration-order field push
        // the override if there is one, else `call` the getter on the saved source (its
        // address for a struct record), then `newobj`.
        let nominal = nominalOfExpr e
        let key, tyArgs = nominal.Key, nominal.Args

        match env.Records.TryGetValue key with
        | true, r ->
            let overrideMap = Map.ofSeq overrides
            let srcSlot = b.Local(FrozenNominal.ty nominal)
            recur env b source
            b.Add(ILInstr.Stloc srcSlot)

            for f in r.Fields do
                match Map.tryFind f.Name overrideMap with
                | Some e -> recur env b e
                | None ->
                    let getter = recordFieldAccessor env r key tyArgs f TAccessorRole.Getter
                    b.Add(loadSlotAsThis r.IsValueType srcSlot)
                    b.Add(ILInstr.Call(getter, 1, 1))

            let ctor =
                memberRef env.Provider r.TypeArity key tyArgs (UserMemberKind.RecordMember RecordMember.Ctor) r.Ctor

            b.Add(ILInstr.Newobj(ctor, List.length r.Fields))
        | false, _ -> failwithf "Emit: no emitted record for '%A'" key

    let buildUnionCons (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let caseName = TastAccessor.exprUnionConsCaseName e
        let args = TastAccessor.exprChildren e
        let nominal = nominalOfExpr e
        let key, tyArgs = nominal.Key, nominal.Args
        let qualName = SymbolKeyOps.typeMetaName key

        // A value assigned to a case field typed `obj` is boxed by an explicit `Upcast`
        // node from Elaborate, so push each argument raw.
        for a in args do
            recur env b a

        match env.Unions.TryGetValue key with
        | true, u ->
            // Our own emitted union: `call` the case's static factory, the fields already
            // on the stack in declaration order.
            let factoryRef =
                memberRef
                    env.Provider
                    u.TypeArity
                    key
                    tyArgs
                    (UserMemberKind.UnionMember(UnionMember.Factory caseName))
                    u.Cases.[caseName].Factory

            b.Add(ILInstr.Call(factoryRef, args.Length, 1))
        | false, _ ->
            // The provider's special-case (FSharp.Core list) for `[]` / `::`.
            match env.Provider.TryEmitUnionCons(key, caseName, tyArgs) with
            | ValueSome recipe -> b.Add(ILInstr.Recipe recipe)
            | ValueNone -> failwithf "Emit: no union-cons recipe for %s.%s" qualName caseName

    let buildTuple (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let elems = TastAccessor.exprChildren e
        let ty = TastAccessor.exprTy e

        // A standalone tuple VALUE; an argument list is flattened at the call site instead.
        // Arity ≥ 8 nests: push slots 0–6, build the REST as a nested `TRest` value, then
        // `newobj` the 8-arg `ValueTuple\`8` ctor.
        let refs = env.Provider.ValueTupleRefs(tupleElemTys ty)

        let rec buildFrom (refs: ValueTupleHandles) (start: int) : unit =
            match refs.Rest with
            | ValueSome rest ->
                for i in start .. start + 6 do
                    recur env b elems.[i]

                buildFrom rest.Nested (start + 7)
                b.Add(ILInstr.Newobj(refs.Ctor, 8))
            | ValueNone ->
                for i in start .. elems.Length - 1 do
                    recur env b elems.[i]

                b.Add(ILInstr.Newobj(refs.Ctor, elems.Length - start))

        buildFrom refs 0

    /// `[| a; b |]`: `newarr` at the element type, then `dup; ldc.i4 i; <elem>; stelem`
    /// per element, leaving the array on the stack.
    let buildArrayLit (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let elems = TastAccessor.exprChildren e

        let elemTok =
            match TastAccessor.exprTy e with
            | FTArray elem -> env.Provider.TypeToken elem
            | ty -> failwithf "Emit: array literal typed %A" ty

        b.Add(ILInstr.LdcI4 elems.Length)
        b.Add(ILInstr.Newarr elemTok)

        elems
        |> Array.iteri (fun i el ->
            b.Add ILInstr.Dup
            b.Add(ILInstr.LdcI4 i)
            recur env b el
            b.Add(ILInstr.Stelem elemTok)
        )

    let private closureOf (env: EmitEnv) (e: TastAccessor.ExprId) : Closure =
        match env.ClosureByNode.TryGetValue e with
        | true, closure -> closure
        | false, _ -> failwith "Emit: a Lambda value was not discovered as a closure"

    /// A value-struct closure is built BY VALUE with no `newobj`, leaving the struct on the
    /// stack for a constrained `!TF` slot. Value-type ctor stack discipline is address
    /// first, then captures, then `call` (which returns void), then `ldloc` the result.
    let private buildValueStructClosure
        (env: EmitEnv)
        (b: IlBuilder)
        (e: TastAccessor.ExprId)
        (closureFt: FrozenType)
        : unit =
        let closure = closureOf env e
        let emitted = env.Closures.[e]
        let slot = b.Local closureFt
        b.Add(ILInstr.Ldloca slot)

        if List.isEmpty closure.Captures then
            b.Add(ILInstr.Initobj emitted.Type)
        else
            for cap in closure.Captures do
                buildVarLoad env b cap.Key

            b.Add(ILInstr.Call(emitted.Ctor, List.length closure.Captures + 1, 0))

        b.Add(ILInstr.Ldloc slot)

    /// A heap closure: push its captures, then `newobj` its ctor. A back-patched capture is
    /// pushed as `null`.
    let private buildHeapClosure (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let closure = closureOf env e

        for cap in closure.Captures do
            match cap.Fill with
            | CaptureFill.ByCtor -> buildVarLoad env b cap.Key
            | CaptureFill.BackPatched -> b.Add ILInstr.Ldnull

        b.Add(ILInstr.Newobj(closureToken env closure ClosureToken.Ctor, List.length closure.Captures))

    /// A `Lambda` value: construct its closure by value if it is a value-struct, else a
    /// stateless one `ldsfld`s the singleton cached for it, else `newobj` on the heap.
    let buildLambda (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        match env.Closures.TryGetValue e with
        | true, { ValueType = ValueSome closureFt } -> buildValueStructClosure env b e closureFt
        | true, { CachedField = ValueSome cachedField } -> b.Add(ILInstr.Ldsfld cachedField)
        | _ -> buildHeapClosure env b e
