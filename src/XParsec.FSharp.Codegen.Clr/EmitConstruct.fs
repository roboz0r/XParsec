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
/// case construction, tuple values, and the closure `newobj` for a `Lambda`
/// value. `buildLambda` only pushes captures (via `buildVarLoad`), so it carries
/// no `Recur` seam; the rest evaluate sub-expressions through `recur`.
module EmitConstruct =

    let buildNew (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.New(className, args, ty, _) ->
            let tyArgs =
                match ty with
                | FTClass(_, xs) -> EqArray.toList xs
                | _ -> []

            // Call-site arg types let the external-ctor path disambiguate ctor
            // overloads (v1 picker is arity-only — see `ClrProvider.externalCtor`).
            let argTypes = [ for a in args -> typeOfExpr a ]

            // A project-local class is identified by the nominal `SymbolKey` on the
            // construction's `TyClass` result type; an external ctor (no `TyClass`
            // result, or a key not in `env.Classes`) routes through the provider by
            // `className`.
            let localClass =
                match ty with
                | FTClass(k, _) ->
                    match env.Classes.TryGetValue k with
                    | true, c -> ValueSome(k, c)
                    | _ -> ValueNone
                | _ -> ValueNone

            let argCount = args.Length

            // Parameterless value-type construction (`Counter()`, `Span<char>()`) is
            // the one shape that pushes no arguments and emits no `newobj`: the
            // idiomatic CLR lowering is `initobj` on a zeroed scratch local, not a
            // `newobj` against the synthesised parameterless `.ctor` (a struct's
            // parameterless ctor only zero-inits anyway, and this avoids relying on
            // the JIT tolerating an explicit value-type `.ctor()` call). Covers both
            // a project-local struct and an *external* value type (`Span<char>` has
            // no real parameterless ctor recipe — `default(Span<char>)`). Handled
            // first so every other path shares the single push-then-construct seam.
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
                // Resolve the construction to the instruction that consumes the
                // pushed args, THEN push exactly once. Keeping the push at this
                // single seam — not inside each ctor arm — means no arm can forget
                // it (a missing push would underflow the IL stack). The value→`obj`
                // box for an `obj` parameter is now an explicit `Upcast` node from
                // Freeze, so codegen just pushes each argument raw.
                let emitNewobj =
                    match localClass with
                    | ValueSome(classKey, c) ->
                        // A user class emitted into this assembly. The primary ctor's
                        // arity equals its field count; a different arg count selects
                        // a secondary ctor by arity — F# forbids two ctors of the same
                        // signature, so arity is a key. The `val`-field form
                        // (`type T = val …; new(…) = …`) has NO primary ctor
                        // (`HasPrimaryCtor = false`), so every construction — including
                        // a 0-arg `T()` that would otherwise match the (absent) primary
                        // by field count — resolves to a secondary by arity.
                        if c.HasPrimaryCtor && argCount = List.length c.Fields then
                            // Primary. Monomorphic: the ctor's `Def` token directly.
                            // Generic: a `MemberRef` on the receiver's instantiated
                            // `TypeSpec` (`Box<int>::.ctor`), as the generic-record path.
                            let ctorRef =
                                memberRef
                                    env
                                    c.Typars
                                    classKey
                                    tyArgs
                                    (UserMemberKind.ClassMember ClassMember.Ctor)
                                    c.Ctor

                            fun () -> b.Add(ILInstr.Newobj(ctorRef, argCount))
                        else
                            match c.SecondaryCtors |> List.tryFind (fun (a, _, _) -> a = argCount) with
                            | Some(_, _, h) when List.isEmpty c.Typars -> fun () -> b.Add(ILInstr.Newobj(h, argCount))
                            | Some(_, paramTys, h) ->
                                // Generic secondary-ctor call site: a `MemberRef` on
                                // the instantiated `TypeSpec` (`OnceEnum<int>::.ctor`),
                                // keyed by the ctor's declared param signature.
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
                        // The external ctor is identified by the construction's
                        // result-type key (`ty = FTClass(key, _)` — also the
                        // `PrintfFormat` printf-literal case); `className` survives
                        // only for the error message. A `New` whose `ty` isn't a
                        // `TyClass` is a defensive CST error path the project-local
                        // arm already missed — it has no resolvable ctor.
                        fun () ->
                            match ty with
                            | FTClass(ctorKey, _) ->
                                match env.Provider.TryEmitCtor(ctorKey, tyArgs, argTypes) with
                                | ValueSome recipe -> b.Add(ILInstr.Newobj(recipe.Handle, recipe.ArgCount))
                                | ValueNone -> failwithf "Emit: no constructor recipe for '%s'" className
                            // A constructed heritable primitive (`new exn "boom"` /
                            // `new System.Exception "boom"`, canonicalized at
                            // resolution) types as the canon `FTConst`: resolve it to
                            // its platform external key (`System.Exception`) and mint
                            // the ctor there — the same resolution the `inherit exn(…)`
                            // base-ctor chain uses.
                            | FTConst(canonKey, args) when args.IsEmpty ->
                                let recipe =
                                    env.Provider.IntrinsicClassBase canonKey
                                    |> ValueOption.bind (fun (struct (platformKey, _)) ->
                                        env.Provider.TryEmitCtor(platformKey, tyArgs, argTypes)
                                    )

                                match recipe with
                                | ValueSome recipe -> b.Add(ILInstr.Newobj(recipe.Handle, recipe.ArgCount))
                                | ValueNone -> failwithf "Emit: no constructor recipe for '%s'" className
                            | _ -> failwithf "Emit: no constructor recipe for '%s'" className

                for a in args do
                    recur env b a

                emitNewobj ()
        | _ -> failwith "EmitConstruct.buildNew: unreachable"

    let buildRecordCons (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.RecordCons(srcFields, ty, _) ->
            // The source-order initialiser list (`{ Y = …; X = … }`) is reordered
            // to the type's *declaration* order before the ctor is invoked:
            // the ctor's parameter slots correspond to
            // declaration order so the field-store sequence in `buildRecordCtor`
            // lines up. A generic record's `.ctor` is a `MemberRef` on its own
            // `TypeSpec` (`Box\`1<!0>::.ctor`), exactly like a generic union's
            // factory.
            let key, tyArgs = nominalShape "RecordCons" ty

            match env.Records.TryGetValue key with
            | true, r ->
                let srcMap = Map.ofSeq srcFields.Underlying

                // Fields push in declaration order (the ctor's parameter layout).
                // A value flowing into an `obj` field is boxed by an explicit
                // `Upcast` node from Freeze, so push each initialiser raw.
                for (fieldName, _, _) in r.Fields do
                    match Map.tryFind fieldName srcMap with
                    | Some e -> recur env b e
                    | None ->
                        failwithf "Emit: record literal for '%A' is missing initialiser for field '%s'" key fieldName

                let ctor =
                    memberRef env r.Typars key tyArgs (UserMemberKind.RecordMember RecordMember.Ctor) r.Ctor

                b.Add(ILInstr.Newobj(ctor, List.length r.Fields))
            | false, _ ->
                let qualName = SymbolKeyOps.qualifiedName key
                // The record lives in a referenced assembly. The provider mints a
                // `MemberRef` on its instantiated `TypeSpec`; field arguments are
                // pushed in source order (the contract layer's field order is also
                // the declaration order, which matches the ctor's parameter layout,
                // so no reorder is required for the supported one-field `Ref<'T>`
                // shape — multi-field external records will revisit).
                let fieldNames = [ for (n, _) in srcFields -> n ]

                match env.Provider.TryEmitRecordCons(key, tyArgs, fieldNames) with
                | ValueSome recipe ->
                    for (_, e) in srcFields do
                        recur env b e

                    b.Add(ILInstr.Newobj(recipe.Handle, recipe.ArgCount))
                | ValueNone -> failwithf "Emit: no emitted record for '%s'" qualName
        | _ -> failwith "EmitConstruct.buildRecordCons: unreachable"

    let buildRecordClone (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.RecordClone(source, overrides, ty, _) ->
            // `{ r with X = v; … }` — evaluate `r` into a local, then per
            // declaration-order field: push the override expression if it's in
            // the override list, else `ldloc; ldfld` from the saved source. Then
            // `newobj` the ctor. Direct field reads (no `MemberwiseClone`) keeps
            // it BCL-only and works identically for a generic record.
            let key, tyArgs = nominalShape "RecordClone" ty

            match env.Records.TryGetValue key with
            | true, r ->
                let overrideMap = Map.ofSeq overrides.Underlying
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
        | _ -> failwith "EmitConstruct.buildRecordClone: unreachable"

    let buildUnionCons (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.UnionCons(caseName, args, ty, _) ->
            // Local union table keys by the nominal `SymbolKey`; the provider's
            // cons recipe (FSharp.Core / Vesper list) selects on the same key.
            let key, tyArgs = nominalShape "UnionCons" ty
            let qualName = SymbolKeyOps.qualifiedName key

            // A value-type arg flowing into a case field typed `obj` is boxed by an
            // explicit `Upcast` node synthesised at Freeze (which has the case field
            // SemTypes this site lacks — `EmittedCase.Fields` carries only handles),
            // so codegen just pushes each argument raw.
            for a in args do
                recur env b a

            match env.Unions.TryGetValue key with
            | true, u ->
                // Our own emitted union: `call` the case's static factory (the
                // fields are already on the stack in declaration order). A
                // monomorphic factory is a `Def` token; a generic one is a
                // `MemberRef` on the instantiated `TypeSpec` (`List<int>::Cons`).
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
        | _ -> failwith "EmitConstruct.buildUnionCons: unreachable"

    let buildTuple (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.Tuple(elems, ty, _) ->
            // A standalone tuple *value* (the argument-list case is flattened at the
            // call site instead). Push each element left-to-right, then `newobj` the
            // `System.ValueTuple` ctor, leaving the struct on the stack. Arity ≥ 8
            // nests: push slots 0–6, build the residual tail as a nested `TRest`
            // value, then `newobj` the 8-arg `ValueTuple`8` ctor. Recurse by offset
            // into `elems` to stay allocation-free.
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
        | _ -> failwith "EmitConstruct.buildTuple: unreachable"

    /// The discovered `Closure` for a `Lambda` node — every construction path needs
    /// it. Its absence is a broken invariant (discovery missed a lambda), so each
    /// caller faults rather than silently degrading.
    let private closureOf (env: EmitEnv) (e: Frozen.TExpr) : Closure =
        match env.ClosureByNode.TryGetValue e with
        | true, closure -> closure
        | false, _ -> failwith "Emit: a Lambda value was not discovered as a closure"

    /// A `Stack` (value-struct) closure is a readonly struct,
    /// constructed BY-VALUE (NO `newobj`, NO cached `ldsfld`), leaving the struct
    /// VALUE on the stack; the call site passes it into the constrained `!TF` slot,
    /// so `constrained.` devirtualises with no box.
    ///   * Captureless: a zero-field struct — `ldloca; initobj; ldloc`.
    ///   * Capturing: `initobj` only zeroes a fieldless struct, so push each
    ///     capture (in capture-field order) and `call` the value-type ctor
    ///     (`buildStructCtor` stores `ldarg.(i+1)` into field `i`), writing through
    ///     the `&slot` managed pointer. Value-type ctor stack discipline: address
    ///     first, then args, then `call` (returns void, stack empty), then `ldloc`
    ///     the now-initialised value.
    let private buildValueStructClosure (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) (closureFt: FrozenType) : unit =
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

    /// The v1 heap closure: push captures via the current resolver (a local in
    /// `Main`, the param, or a capture inside an enclosing closure), then `newobj`
    /// its ctor. A generic closure routes the `Newobj` through a `MemberRef` on
    /// `<closure>$n<args>`, where `args` is the closure's typars zonked at the call
    /// site (`!!i` inside the enclosing static method's body, `!i` inside an
    /// enclosing closure's `Invoke`) — both encodings reference the same TypeVar
    /// roots, and the parent's `TypeSpec` captures the use-site instantiation.
    let private buildHeapClosure (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        let closure = closureOf env e

        for (k, _) in closure.Captures do
            buildVarLoad env b k

        let ctorHandle =
            if closure.Typars = 0 then
                match env.CtorHandleByNode.TryGetValue e with
                | true, ctor -> ctor
                | false, _ -> failwith "Emit: closure constructor not yet emitted (leaves-first ordering broken)"
            else
                // The closure's instantiation at *this* construction site, in the
                // enclosing context's ambient. Its typar list is the enclosing class
                // typars (the leading `DeclaringTypars` slots) followed by the
                // enclosing member's method typars: a class typar is
                // `FTTypar(Declaring, i)` (encoded `!i` in a member body) and a method
                // typar `FTTypar(Method, j)` (`!!j`). A static-fn closure has
                // `DeclaringTypars = 0`, so this is `[FTTypar(Method, j)]` — the prior
                // encoding (`!!i` in a static-method body, `!i` inside an enclosing
                // closure).
                let instArgs =
                    [ for i in 0 .. closure.DeclaringTypars - 1 -> FTTypar(TyparAxis.Declaring, i) ]
                    @ [
                        for j in 0 .. closure.Typars - closure.DeclaringTypars - 1 -> FTTypar(TyparAxis.Method, j)
                    ]

                env.Provider.UserClosureMemberRef(closure.Name, instArgs, ClosureMember.Ctor)

        b.Add(ILInstr.Newobj(ctorHandle, List.length closure.Captures))

    /// A `Lambda` value: construct its closure. The three construction modes
    /// partition the closure space (`EmitTypes.closureIsCached`), so this is a flat
    /// dispatch with no sub-expression evaluation (no `Recur` seam):
    ///   * value-struct — by-value, keyed in `ClosureValueTypeByNode`;
    ///   * cached singleton — stateless heap closure `ldsfld`'d once;
    ///   * heap `newobj` (v1) — everything else.
    let buildLambda (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.Lambda _ ->
            match env.ClosureValueTypeByNode.TryGetValue e with
            | true, closureFt -> buildValueStructClosure env b e closureFt
            | false, _ ->
                match env.CachedClosureFieldByNode.TryGetValue e with
                | true, cachedField -> b.Add(ILInstr.Ldsfld cachedField)
                | false, _ -> buildHeapClosure env b e
        | _ -> failwith "EmitConstruct.buildLambda: unreachable"
