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

/// Object / value construction: `new`, record literals + `{ r with … }`, union
/// case construction, tuple values, and the closure for a `Lambda` value.
module EmitConstruct =

    let buildNew (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let className = TastAccessor.exprNewClassName e
        let chosenCtor = TastAccessor.exprNewChosenCtor e
        let args = TastAccessor.exprChildren e
        let ty = TastAccessor.exprTy e

        let tyArgs =
            match ty with
            | FTClass(_, xs) -> EqArray.toList xs
            | _ -> []

        // The external-ctor path filters candidates by arity off these; `chosenCtor`
        // then disambiguates a same-arity set.
        let argTypes = [ for a in args -> typeOfExpr a ]

        // A project-local class is the `FTClass` key that `env.Classes` knows; anything
        // else is an external ctor, resolved through the provider.
        let localClass =
            match ty with
            | FTClass(k, _) ->
                match env.Classes.TryGetValue(SymbolKey.Type k) with
                | true, c -> ValueSome(k, c)
                | _ -> ValueNone
            | _ -> ValueNone

        let argCount = args.Length

        // Parameterless value-type construction (`Counter()`, `Span<char>()`) pushes no
        // arguments and emits no `newobj`: it is `initobj` on a zeroed scratch local —
        // `Span<char>` has no parameterless ctor to call, it is `default(Span<char>)`.
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
                    // The primary ctor's arity equals its field count; any other arg count
                    // selects a secondary by arity — F# forbids two ctors of the same
                    // signature, so arity is a key. `type T = val …; new(…)` has no primary.
                    if c.HasPrimaryCtor && argCount = List.length c.Fields then
                        let ctorRef =
                            memberRef env c.Typars classKey tyArgs (UserMemberKind.ClassMember ClassMember.Ctor) c.Ctor

                        fun () -> b.Add(ILInstr.Newobj(ctorRef, argCount))
                    else
                        match c.SecondaryCtors |> List.tryFind (fun (a, _, _) -> a = argCount) with
                        | Some(_, _, h) when List.isEmpty c.Typars -> fun () -> b.Add(ILInstr.Newobj(h, argCount))
                        | Some(_, paramTys, h) ->
                            let ctorRef =
                                memberRef
                                    env
                                    c.Typars
                                    classKey
                                    tyArgs
                                    (UserMemberKind.ClassMember(ClassMember.SecondaryCtor paramTys))
                                    h

                            fun () -> b.Add(ILInstr.Newobj(ctorRef, argCount))
                        | None -> failwithf "Emit: no constructor of arity %d on class '%s'" argCount className
                | ValueNone ->
                    // An external ctor is identified by the construction's result-type key;
                    // `className` survives only for the error message.
                    fun () ->
                        match ty with
                        | FTClass(ctorKey, _) ->
                            match env.Provider.TryEmitCtor(SymbolKey.Type ctorKey, chosenCtor, tyArgs, argTypes) with
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
        let ty = TastAccessor.exprTy e

        let key, tyArgs = nominalShape "RecordCons" ty

        match env.Records.TryGetValue(SymbolKey.Type key) with
        | true, r ->
            let srcMap = Map.ofSeq srcFields

            // A source-order initialiser list (`{ Y = …; X = … }`) pushes in the type's
            // DECLARATION order, which is the ctor's parameter layout.
            for (fieldName, _, _) in r.Fields do
                match Map.tryFind fieldName srcMap with
                | Some e -> recur env b e
                | None -> failwithf "Emit: record literal for '%A' is missing initialiser for field '%s'" key fieldName

            let ctor =
                memberRef env r.Typars key tyArgs (UserMemberKind.RecordMember RecordMember.Ctor) r.Ctor

            b.Add(ILInstr.Newobj(ctor, List.length r.Fields))
        | false, _ ->
            let qualName = SymbolKeyOps.typeMetaName key
            // A record in a referenced assembly pushes in SOURCE order: the only such
            // shape supported is the one-field `Ref<'T>`, where the two coincide.
            let fieldNames = [ for (n, _) in srcFields -> n ]

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
        let ty = TastAccessor.exprTy e

        // `{ r with X = v }` — spill `r` to a local, then per declaration-order field push
        // the override if there is one, else `ldloc; ldfld` the saved source, then `newobj`.
        let key, tyArgs = nominalShape "RecordClone" ty

        match env.Records.TryGetValue(SymbolKey.Type key) with
        | true, r ->
            let overrideMap = Map.ofSeq overrides
            let srcSlot = b.Local ty
            recur env b source
            b.Add(ILInstr.Stloc srcSlot)

            for (fieldName, handle, _) in r.Fields do
                match Map.tryFind fieldName overrideMap with
                | Some e -> recur env b e
                | None ->
                    let fieldRef =
                        memberRef
                            env
                            r.Typars
                            key
                            tyArgs
                            (UserMemberKind.RecordMember(RecordMember.Field fieldName))
                            handle

                    b.Add(ILInstr.Ldloc srcSlot)
                    b.Add(ILInstr.Ldfld fieldRef)

            let ctor =
                memberRef env r.Typars key tyArgs (UserMemberKind.RecordMember RecordMember.Ctor) r.Ctor

            b.Add(ILInstr.Newobj(ctor, List.length r.Fields))
        | false, _ -> failwithf "Emit: no emitted record for '%A'" key

    let buildUnionCons (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let caseName = TastAccessor.exprUnionConsCaseName e
        let args = TastAccessor.exprChildren e
        let ty = TastAccessor.exprTy e

        let key, tyArgs = nominalShape "UnionCons" ty
        let qualName = SymbolKeyOps.typeMetaName key

        // A value flowing into a case field typed `obj` is boxed by an explicit `Upcast`
        // node from Elaborate, so push each argument raw.
        for a in args do
            recur env b a

        match env.Unions.TryGetValue(SymbolKey.Type key) with
        | true, u ->
            // Our own emitted union: `call` the case's static factory, the fields already
            // on the stack in declaration order.
            let factoryRef =
                memberRef
                    env
                    u.Typars
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
        // Arity ≥ 8 nests: push slots 0–6, build the tail as a nested `TRest` value, then
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

    let private closureOf (env: EmitEnv) (e: TastAccessor.ExprId) : Closure =
        match env.ClosureByNode.TryGetValue e with
        | true, closure -> closure
        | false, _ -> failwith "Emit: a Lambda value was not discovered as a closure"

    /// A value-struct closure is built BY VALUE — no `newobj` — leaving the struct on the
    /// stack for a constrained `!TF` slot. Value-type ctor stack discipline is address
    /// first, then captures, then `call` (which returns void), then `ldloc` the result.
    let private buildValueStructClosure
        (env: EmitEnv)
        (b: IlBuilder)
        (e: TastAccessor.ExprId)
        (closureFt: FrozenType)
        : unit =
        let closure = closureOf env e
        let closureHandle = env.ClosureTypeDefByNode.[e]
        let slot = b.Local closureFt
        b.Add(ILInstr.Ldloca slot)

        if List.isEmpty closure.Captures then
            b.Add(ILInstr.Initobj closureHandle)
        else
            for (k, _) in closure.Captures do
                buildVarLoad env b k

            let ctorHandle =
                match env.CtorHandleByNode.TryGetValue e with
                | true, ctor -> ctor
                | false, _ ->
                    failwith "Emit: value-struct closure constructor not yet emitted (leaves-first ordering broken)"

            b.Add(ILInstr.Call(ctorHandle, List.length closure.Captures + 1, 0))

        b.Add(ILInstr.Ldloc slot)

    /// A heap closure: push its captures, then `newobj` its ctor. A generic one routes the
    /// `Newobj` through a `MemberRef` on `<closure>$n<args>`, `args` being its typars as
    /// seen at THIS construction site.
    let private buildHeapClosure (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let closure = closureOf env e

        for (k, _) in closure.Captures do
            buildVarLoad env b k

        let ctorHandle =
            if closure.Typars = 0 then
                match env.CtorHandleByNode.TryGetValue e with
                | true, ctor -> ctor
                | false, _ -> failwith "Emit: closure constructor not yet emitted (leaves-first ordering broken)"
            else
                // The closure's typar list is the enclosing class typars (the leading
                // `DeclaringTypars` slots, encoded `!i` in a member body) followed by the
                // enclosing member's method typars (`!!j`).
                let instArgs =
                    [ for i in 0 .. closure.DeclaringTypars - 1 -> FTTypar(TyparAxis.Declaring, i) ]
                    @ [
                        for j in 0 .. closure.Typars - closure.DeclaringTypars - 1 -> FTTypar(TyparAxis.Method, j)
                    ]

                env.Provider.UserClosureMemberRef(closure.Name, instArgs, ClosureMember.Ctor)

        b.Add(ILInstr.Newobj(ctorHandle, List.length closure.Captures))

    /// A `Lambda` value: construct its closure — by value if it is a value-struct, else a
    /// stateless one `ldsfld`s the singleton cached for it, else `newobj` on the heap.
    let buildLambda (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        match env.ClosureValueTypeByNode.TryGetValue e with
        | true, closureFt -> buildValueStructClosure env b e closureFt
        | false, _ ->
            match env.CachedClosureFieldByNode.TryGetValue e with
            | true, cachedField -> b.Add(ILInstr.Ldsfld cachedField)
            | false, _ -> buildHeapClosure env b e
