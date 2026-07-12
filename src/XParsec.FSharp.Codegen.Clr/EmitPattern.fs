namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open EmitTypes
open EmitLower
open EmitResolve

/// The recur-free core of expression emission: variable loads, tuple
/// destructuring, the match-test compiler, the irrefutable binder, and the
/// match-failure fallthrough. None of these call back into `buildExpr`, so they
/// carry no `Recur` seam and are compiled ahead of every arm-group module.
module EmitPattern =

    /// Load a variable for the current method: a method parameter (`ldarg.i`),
    /// the recursive self of a closure (`this`, `ldarg.0`), a capture
    /// (`ldarg.0; ldfld`), or a local slot (`ldloc`).
    let buildVarLoad (env: EmitEnv) (b: IlBuilder) (key: NodeKey) : unit =
        match env.Args.TryGetValue key with
        | true, i -> b.Add(ILInstr.Ldarg i)
        | false, _ ->

            match env.SelfKey with
            | ValueSome s when s = key -> b.Add(ILInstr.Ldarg 0) // `this` — the recursive self
            | _ ->
                match env.CaptureFields.TryGetValue key with
                | true, field ->
                    b.Add(ILInstr.Ldarg 0)
                    b.Add(ILInstr.Ldfld field)
                | false, _ ->
                    match env.Slots.TryGetValue key with
                    | true, slot -> b.Add(ILInstr.Ldloc slot)
                    | false, _ ->
                        // A module-level value (`let x = e` at module scope) is a
                        // `public static` field on its module holder; load it with
                        // `ldsfld`. Last arm: a
                        // module value is never an arg/self/capture/local.
                        match env.ModuleValues.TryGetValue key with
                        | true, field -> b.Add(ILInstr.Ldsfld field)
                        // Name the surrounding binder counts so a missing capture /
                        // local / arg is pinpointable rather than anonymous (a
                        // member body has `selfKey`/args; a static fn has args only;
                        // a closure `Invoke` has capture fields).
                        | false, _ ->
                            failwithf
                                "Emit: no binding for variable %O (selfKey=%A captures=%d slots=%d args=%d)"
                                key
                                env.SelfKey
                                env.CaptureFields.Count
                                env.Slots.Count
                                env.Args.Count

    /// The element types of a tuple `FrozenType`. A hard failure if the front end
    /// typed a tuple pattern / value as something other than `FTTuple` — an internal
    /// invariant break, not user error.
    let tupleElemTys (ty: FrozenType) : FrozenType list =
        match ty with
        | FTTuple xs -> EqArray.toList xs
        | other -> failwithf "Emit: expected a tuple type, got: %A" other

    /// Emit the `ldfld` chain that reads element `index` of a `ValueTuple` value
    /// already on the stack. Arity ≤ 7 is a single `Item{i+1}` load; in the nested
    /// ≥ 8 layout an index ≥ 7 loads `Rest` then chases the residual index into the
    /// nested tuple, recursively.
    let rec emitTupleItemLoad (b: IlBuilder) (refs: ValueTupleHandles) (index: int) : unit =
        match refs.Rest with
        | ValueSome rest when index >= 7 ->
            b.Add(ILInstr.Ldfld rest.RestField)
            emitTupleItemLoad b rest.Nested (index - 7)
        | _ -> b.Add(ILInstr.Ldfld refs.ItemFields.[index])

    /// Decompose a `ValueTuple` value held in local `srcSlot`: for each
    /// non-wildcard element, load its slot (`emitTupleItemLoad`, Rest-chasing for
    /// arity ≥ 8) into a fresh local and hand that to `recur` (a wildcard binds
    /// nothing, so its field load is skipped). A tuple never branches on shape, so
    /// this is the one tuple-destructuring primitive behind both the match compiler
    /// (`buildMatchTest`) and the irrefutable binder (`bindPattern`).
    let destructureTuple
        (env: EmitEnv)
        (b: IlBuilder)
        (srcSlot: int)
        (ty: FrozenType)
        (items: EqArray<Frozen.TPat>)
        (recur: int -> Frozen.TPat -> unit)
        : unit =
        let refs = env.Provider.ValueTupleRefs(tupleElemTys ty)

        items
        |> EqArray.iteri (fun i subPat ->
            match subPat with
            | TPatG.Wildcard _ -> ()
            | _ ->
                let fldSlot = b.Local(typeOfPat subPat)
                b.Add(ILInstr.Ldloc srcSlot)
                emitTupleItemLoad b refs i
                b.Add(ILInstr.Stloc fldSlot)
                recur fldSlot subPat
        )

    /// Whether a (zonked) `FrozenType` is a CLR value type — drives the box vs
    /// no-op choice on `:>` and the `unbox.any` vs `castclass` choice on `:?>`
    /// (and the type-test pattern's `isinst` bind). User records / unions /
    /// classes are reference types (rung 2); the BCL primitives bound as
    /// `TyConst` are value types. `string` / `obj` are reference types despite
    /// being `TyConst`.
    let isValueType (env: EmitEnv) (ty: FrozenType) : bool =
        match ty with
        // Numeric primitives (incl. `decimal`) share `RuntimeNames.numericTypeNames`;
        // `bool` / `char` are the two non-numeric value-type scalars. `string` / `obj`
        // are `TyConst` but reference types, so they're excluded (not in the set).
        | FTConst(key, _) ->
            let n = SymbolKeyOps.simpleName key
            RuntimeNames.numericTypeNames.Contains n || n = "bool" || n = "char"
        // A user-declared `[<Struct>]` type emitted into this assembly:
        // the `EmittedClass.IsValueType` flag drives
        // box-on-`:>` / `unbox.any`-on-`:?>` exactly as for a BCL value type. A
        // struct that lives in a *referenced* package (not in `env.Classes`) is
        // recognised the same way via the provider's external value-type flag —
        // the contract/metadata layer's `IsValueType`.
        | FTClass(key, _) ->
            match env.Classes.TryGetValue key with
            | true, c -> c.IsValueType
            | false, _ -> env.Provider.IsExternalValueType key
        | _ -> false

    /// Test a pattern against the value already stored in local `scrutSlot`:
    /// branch to `nextLabel` on mismatch, and bind any pattern variables. A
    /// `Const` compares (`bne.un` skips the arm); `Wildcard` / `NamedSimple`
    /// always match (the latter aliases its binding to `scrutSlot`, so
    /// `emitVarLoad` resolves it to the same local — no copy). Union / tuple /
    /// record patterns land in later rung-2 slices.
    let rec buildMatchTest (env: EmitEnv) (b: IlBuilder) (scrutSlot: int) (nextLabel: int) (pat: Frozen.TPat) : unit =
        // `ldfld` a field of the scrutinee into a fresh local, then test its
        // sub-pattern against that local (a named sub-pattern just aliases it).
        let extractField (fieldRef: EntityHandle) (subPat: Frozen.TPat) =
            let fldSlot = b.Local(typeOfPat subPat)
            b.Add(ILInstr.Ldloc scrutSlot)
            b.Add(ILInstr.Ldfld fieldRef)
            b.Add(ILInstr.Stloc fldSlot)
            buildMatchTest env b fldSlot nextLabel subPat

        match pat with
        | TPatG.Wildcard _ -> ()
        | TPatG.NamedSimple(binding, _, _) -> env.Slots.[binding] <- scrutSlot
        | TPatG.EnumCase(enumKey, caseName, _, _) ->
            // v1 = equality only; binds nothing (a named case is a singleton). The
            // comparison depends on the enum's repr:
            //  * NUMERIC (5a): load the scrutinee (an enum value) and the case's
            //    underlying integer constant, then `bne.un`. The scrutinee value type
            //    is treated as its underlying integer for the compare.
            //  * STRING/MIXED (5b): the wrapper holds a `string` / `obj` field; compare
            //    the scrutinee's field against the case literal via
            //    `EqualityComparer<field>.Default.Equals` — the same field-equality
            //    recipe records use (string equality for `string`, structural object
            //    equality for the boxed `obj`), NOT a `ceq` (reference) compare.
            match env.Enums.TryGetValue enumKey with
            | true,
              {
                  Repr = EmittedEnumRepr.StructEnum(isMixed, backingField, _, caseLits)
              } ->
                let fieldTy =
                    FTConst(
                        (if isMixed then
                             RuntimeNames.objKey
                         else
                             RuntimeNames.stringKey),
                        EqArray.empty
                    )

                let pushLit =
                    match caseLits.TryGetValue caseName with
                    | true, lit -> enumLiteralPush env.Provider.TypeToken env.Ctx.UserString lit
                    | false, _ -> failwithf "Emit: struct enum '%A' has no case literal for '%s'" enumKey caseName

                b.Add(ILInstr.Call(env.Provider.EqualityComparerDefault fieldTy, 0, 1))
                b.Add(ILInstr.Ldloc scrutSlot)
                b.Add(ILInstr.Ldfld backingField)
                pushLit |> List.iter b.Add
                b.Add(ILInstr.Callvirt(env.Provider.EqualityComparerEquals fieldTy, 3, 1))
                b.Add(ILInstr.Brfalse nextLabel)
            | _ ->
                let loadCase =
                    match tryResolveEnumCaseLoad env enumKey caseName with
                    | ValueSome instr -> instr
                    | ValueNone ->
                        failwithf "Emit: no emitted numeric enum carrying case '%s' for '%A'" caseName enumKey

                b.Add(ILInstr.Ldloc scrutSlot)
                b.Add loadCase
                b.Add(ILInstr.BneUn nextLabel)
        | TPatG.Null _ ->
            // `null` pattern: match only a null scrutinee. A non-null value
            // (`brtrue`) skips the arm; null falls through to the body. Binds
            // nothing.
            b.Add(ILInstr.Ldloc scrutSlot)
            b.Add(ILInstr.Brtrue nextLabel)
        | TPatG.Const(value, _, _) ->
            b.Add(ILInstr.Ldloc scrutSlot)

            match value with
            // The scrutinee slot carries the constant's own width, and so does the
            // load (`pushIntConst`, shared with the `Const` expression) — including the
            // pointer-width conversion a `nativeint` slot needs before `bne.un`
            // compares it.
            | TConstValue.Integral(w, bits) -> pushIntConst b w bits
            | TConstValue.Bool v -> b.Add(ILInstr.LdcI4(if v then 1 else 0))
            | TConstValue.Char c -> b.Add(ILInstr.LdcI4(int c))
            | other -> failwithf "Emit: match on constant %A is out of scope" other

            b.Add(ILInstr.BneUn nextLabel)
        | TPatG.Union(caseName, subPats, ty, _) ->
            // Local union table keys by the nominal `SymbolKey`; the external union
            // provider lookups take the qualified compiled name derived from it.
            let key, tyArgs = nominalShape "union pattern" ty
            let qualName = SymbolKeyOps.qualifiedName key

            // The discriminator field + its value for this case, and a per-index
            // field-ref source, resolved from either the local emitted union or a
            // *referenced-package* one (`match o with Some x -> …`)
            // The emit sequence below is identical for both — only the
            // handle source differs (local `Def`/`MemberRef` tokens vs the provider's
            // refs minted off the external union shape, which keeps the field names +
            // declaration-order tagging in lockstep with the union emitter).
            let tagRef, tagValue, fieldRef =
                match env.Unions.TryGetValue key with
                | true, u ->
                    let c = u.Cases.[caseName]

                    // Tag / field access is a `Def` token for a monomorphic union, but
                    // a `MemberRef` on the instantiated `TypeSpec` for a generic one
                    // (`List<int>::_tag` etc.) — see `EmittedUnion.Typars`
                    let tagRef =
                        memberRef env u.Typars key tyArgs (UserMemberKind.UnionMember UnionMember.Tag) u.TagField

                    let fieldRef i =
                        memberRef
                            env
                            u.Typars
                            key
                            tyArgs
                            (UserMemberKind.UnionMember(UnionMember.Field(caseName, i)))
                            c.Fields.[i]

                    tagRef, c.Tag, fieldRef
                | false, _ ->
                    match env.Provider.ExternalUnionTag(key, tyArgs, caseName) with
                    | ValueSome(tagRef, tagValue) ->
                        let fieldRef i =
                            match env.Provider.ExternalUnionCaseField(key, tyArgs, caseName, i) with
                            | ValueSome(fieldRef, _) -> fieldRef
                            | ValueNone ->
                                failwithf "Emit: external union '%s' case '%s' has no field %d" qualName caseName i

                        tagRef, tagValue, fieldRef
                    | ValueNone -> failwithf "Emit: no emitted union for match on '%s'" qualName

            // Skip the arm unless `scrut._tag = case.Tag`.
            b.Add(ILInstr.Ldloc scrutSlot)
            b.Add(ILInstr.Ldfld tagRef)
            b.Add(ILInstr.LdcI4 tagValue)
            b.Add(ILInstr.BneUn nextLabel)

            subPats
            |> EqArray.iteri (fun i subPat ->
                match subPat with
                | TPatG.Wildcard _ -> ()
                | _ -> extractField (fieldRef i) subPat
            )
        | TPatG.Record(fields, ty, _) ->
            // A record pattern never fails on shape (no tag to compare): for each
            // named sub-pattern, `ldfld` the field into a fresh local and recurse
            // — only the sub-patterns themselves can branch to `nextLabel`. A
            // wildcard sub-pattern is skipped (it would always match), exactly
            // like the union arm above.
            let key, tyArgs = nominalShape "record pattern" ty

            match env.Records.TryGetValue key with
            | true, r ->
                for (fieldName, subPat) in fields do
                    match subPat with
                    | TPatG.Wildcard _ -> ()
                    | _ ->
                        match r.Fields |> List.tryFind (fun (n, _, _) -> n = fieldName) with
                        | Some(_, handle, _) ->
                            let fieldRef =
                                memberRef
                                    env
                                    r.Typars
                                    key
                                    tyArgs
                                    (UserMemberKind.RecordMember(RecordMember.Field fieldName))
                                    handle

                            extractField fieldRef subPat
                        | None -> failwithf "Emit: record '%A' has no field '%s'" key fieldName
            | false, _ -> failwithf "Emit: no emitted record for pattern on '%A'" key
        | TPatG.Tuple(items, ty, _) ->
            // A tuple pattern never fails on shape (a `ValueTuple`n` has no tag):
            // decompose each element and recurse — only the sub-patterns can branch
            // to `nextLabel`, exactly like the union / record arms above.
            destructureTuple env b scrutSlot ty items (fun s p -> buildMatchTest env b s nextLabel p)
        | TPatG.TypeTestAs(testTy, inner, _, _) ->
            // `:? T as x` → `isinst T` then a null check: a non-`T` value yields
            // null (`brfalse` skips the arm). On a match the cast-down value is
            // stored to a `T`-typed local; for a value-type target the `isinst`
            // result is a boxed `T`, so `unbox.any` it back to the unboxed slot.
            // The inner pattern (the `as`-name) then binds against that local
            // (a `NamedSimple` just aliases it — same as the other arms).
            let token = env.Provider.TypeToken testTy
            b.Add(ILInstr.Ldloc scrutSlot)
            b.Add(ILInstr.Isinst token)

            if isValueType env testTy then
                let boxedSlot = b.Local(FTConst(RuntimeNames.objKey, EqArray.empty))
                b.Add(ILInstr.Stloc boxedSlot)
                b.Add(ILInstr.Ldloc boxedSlot)
                b.Add(ILInstr.Brfalse nextLabel)
                let valSlot = b.Local testTy
                b.Add(ILInstr.Ldloc boxedSlot)
                b.Add(ILInstr.UnboxAny token)
                b.Add(ILInstr.Stloc valSlot)
                buildMatchTest env b valSlot nextLabel inner
            else
                let castSlot = b.Local testTy
                b.Add(ILInstr.Stloc castSlot)
                b.Add(ILInstr.Ldloc castSlot)
                b.Add(ILInstr.Brfalse nextLabel)
                buildMatchTest env b castSlot nextLabel inner
        | TPatG.Or(alts, _, _) ->
            // `p1 | … | pn`: test each alternative against the same scrutinee, in
            // order. The first that matches falls through to `matchedLabel` (the
            // arm body); a failing alternative branches to the next alternative's
            // test; the last alternative's failure is the whole arm's failure
            // (`nextLabel`). Each `buildMatchTest` is depth-neutral, so no
            // `SetDepth` is needed between alternatives. Alternatives bind nothing
            // (name resolution drops or-pattern binders), so there is no binder to
            // reconcile across the join.
            let matchedLabel = b.Label()
            let n = alts.Length

            alts
            |> EqArray.iteri (fun i alt ->
                if i = n - 1 then
                    buildMatchTest env b scrutSlot nextLabel alt
                else
                    let altFail = b.Label()
                    buildMatchTest env b scrutSlot altFail alt
                    b.Add(ILInstr.Br matchedLabel)
                    b.Add(ILInstr.Mark altFail)
            )

            b.Add(ILInstr.Mark matchedLabel)

    /// Bind an *irrefutable* pattern against a value already in local `srcSlot` — the
    /// shared destructuring binder for `let` / `for-in` (and, in Step 5, a tuple
    /// lambda parameter). Unlike the match compiler's `buildMatchTest`, this never
    /// branches: a `let` / `for` pattern is assumed to match on shape. A
    /// `NamedSimple` aliases its binding directly to `srcSlot` (no copy, exactly as
    /// the match arm does); a `Tuple` `ldfld`s each `ValueTuple`n` `Item` field into
    /// a fresh local and recurses; `Wildcard` / `Const` bind nothing.
    let rec bindPattern (env: EmitEnv) (b: IlBuilder) (srcSlot: int) (pat: Frozen.TPat) : unit =
        match pat with
        | TPatG.Wildcard _ -> ()
        | TPatG.Const _ -> () // irrefutable in a binding position — no compare, no bind
        | TPatG.NamedSimple(binding, _, _) -> env.Slots.[binding] <- srcSlot
        | TPatG.Tuple(items, ty, _) -> destructureTuple env b srcSlot ty items (bindPattern env b)
        | other -> failwithf "Emit: destructuring pattern is out of scope: %A" other

    /// The fallthrough a `match` reaches when no arm matched — `throw new
    /// System.Exception("…")`. An exhaustive match never reaches it at runtime,
    /// but it keeps the emitted IL well-formed (and gives a non-exhaustive one
    /// defined behaviour).
    let buildMatchFailure (env: EmitEnv) (b: IlBuilder) : unit =
        b.Add(ILInstr.Ldstr(env.Ctx.UserString "The match cases were incomplete"))
        b.Add(ILInstr.Newobj(env.Provider.ExceptionCtor, 1))
        b.Add ILInstr.Throw
