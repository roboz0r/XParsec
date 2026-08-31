namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open EmitTypes
open EmitLower
open EmitResolve

/// How the CLR holds a value of a given type.
[<RequireQualifiedAccess>]
type ClrRepr =
    /// A value type, flat in place.
    | Value
    /// A reference to a heap cell.
    | Reference
    /// An open type parameter, whose representation its instantiation supplies. `box` reaches
    /// `obj` or an interface at either instantiation.
    | Boxable of typar: FrozenType

/// Variable loads, tuple destructuring, the match-test compiler and the irrefutable
/// bound variable. Nothing here takes a `Recur`, so nothing calls back into `buildExpr`.
module EmitPattern =

    /// Load a variable for the current method: a method parameter (`ldarg.i`),
    /// the recursive self of a closure (`this`, `ldarg.0`), a capture
    /// (`ldarg.0; ldfld`), or a local slot (`ldloc`).
    let buildVarLoad (env: EmitEnv) (b: IlBuilder) (key: BoundVarId) : unit =
        match env.Args.TryGetValue key with
        | true, i ->
            b.Add(ILInstr.Ldarg i)

            // `this` of a value-type member is `ldarg.0` the managed pointer; a VALUE use of
            // it (match scrutinee, equality operand, return) deref-copies. A consumer needing
            // the pointer (`loadStructThisPtr`, the primary-ctor stores) loads `ldarg.0` itself.
            match env.SelfValueType with
            | ValueSome ty when env.SelfKey = ValueSome key -> b.Add(ILInstr.Ldobj(env.Provider.TypeToken ty))
            | _ -> ()
        | false, _ ->

            match env.SelfKey with
            | ValueSome s when s = key -> b.Add(ILInstr.Ldarg 0)
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
                        // `public static` field on its module class, so `ldsfld`.
                        match env.ModuleValues.TryGetValue key with
                        | true, field -> b.Add(ILInstr.Ldsfld field)
                        | false, _ ->
                            failwithf
                                "Emit: no binding for variable %O (selfKey=%A captures=%d slots=%d args=%d)"
                                key
                                env.SelfKey
                                env.CaptureFields.Count
                                env.Slots.Count
                                env.Args.Count

    let tupleElemTys (ty: FrozenType) : FrozenType list =
        match ty with
        | FTTuple xs -> EqArray.toList xs
        | other -> failwithf "Emit: expected a tuple type, got: %A" other

    /// Read element `index` of a `ValueTuple` already on the stack. Arity ≤ 7 is one
    /// `Item{i+1}` load; in the nested ≥ 8 layout an index ≥ 7 loads `Rest` and chases
    /// the residual index into the nested tuple.
    let rec emitTupleItemLoad (b: IlBuilder) (refs: ValueTupleHandles) (index: int) : unit =
        match refs.Rest with
        | ValueSome rest when index >= 7 ->
            b.Add(ILInstr.Ldfld rest.RestField)
            emitTupleItemLoad b rest.Nested (index - 7)
        | _ -> b.Add(ILInstr.Ldfld refs.ItemFields.[index])

    /// Decompose a `ValueTuple` held in local `srcSlot`: load each non-wildcard
    /// element into a fresh local and hand that to `recur`. A wildcard binds nothing,
    /// so its field load is skipped.
    let destructureTuple
        (env: EmitEnv)
        (b: IlBuilder)
        (srcSlot: int)
        (ty: FrozenType)
        (items: TastAccessor.PatId[])
        (recur: int -> TastAccessor.PatId -> unit)
        : unit =
        let refs = env.Provider.ValueTupleRefs(tupleElemTys ty)

        items
        |> Array.iteri (fun i subPat ->
            match TastAccessor.patKind subPat with
            | PatShape.Wildcard -> ()
            | _ ->
                let fldSlot = b.Local(typeOfPat subPat)
                b.Add(ILInstr.Ldloc srcSlot)
                emitTupleItemLoad b refs i
                b.Add(ILInstr.Stloc fldSlot)
                recur fldSlot subPat
        )

    /// The `[<Struct>]` a type EMITTED HERE asked for. `Unsettled` for a referenced name, which
    /// the provider holds instead; the two sets are disjoint.
    let private declaredHere (env: EmitEnv) (key: TypeKey) : TypeLayout =
        match env.Classes.TryGetValue key with
        | true, c -> TypeLayout.ofValueness c.IsValueType
        | false, _ ->
            match env.Records.TryGetValue key with
            | true, r -> TypeLayout.ofValueness r.IsValueType
            | false, _ ->
                match env.Unions.TryGetValue key with
                | true, u -> TypeLayout.ofValueness u.IsValueType
                | false, _ -> TypeLayout.Unsettled

    /// A referenced name arrives already settled, target-then-declaration; only a type EMITTED
    /// HERE still has its `[<Struct>]` as an open request.
    let private oracle (env: EmitEnv) : ILayoutOracle =
        { new ILayoutOracle with
            member _.Settled key = env.Provider.ExternalLayout key
            member _.Declared key = declaredHere env key
            member _.Platform = env.Provider.Platform
        }

    /// The CLR representation of a value of `ty`, over the same projection the front end typed
    /// against. Forking the two ends drops a `box` at every `:>` / `:?` / addressed call.
    let clrRepr (env: EmitEnv) (ty: FrozenType) : ClrRepr =
        match ty, TypeLayout.resolve (oracle env) (TypeLayout.shapeOfFrozen ty) with
        | _, TypeLayout.Value -> ClrRepr.Value
        // `FTLocalTypar` stays `Reference`: it has no CLR token to `box` against.
        | FTTypar _, _ -> ClrRepr.Boxable ty
        | _, _ -> ClrRepr.Reference

    /// Is a value of this type laid out flat? A typar answers `false`; `clrRepr` separates it
    /// from a reference.
    let isValueType (env: EmitEnv) (ty: FrozenType) : bool =
        match clrRepr env ty with
        | ClrRepr.Value -> true
        | ClrRepr.Reference
        | ClrRepr.Boxable _ -> false

    /// Where a union arm's payload extraction reads a case field from.
    [<RequireQualifiedAccess>]
    type private UnionArmSource =
        /// A flat union: the fields sit on the scrutinee itself.
        | Scrutinee
        /// A hierarchy case emitted in this compilation: its registered `FrozenType` can
        /// type a local, so the arm casts the scrutinee once and reads off that.
        | CaseLocal of ty: FrozenType * token: EntityHandle
        /// A referenced package's hierarchy case: a token no signature can name, so each
        /// extraction casts the scrutinee in place.
        | CastScrutinee of token: EntityHandle

    /// One match arm's resolution of a union case: the test that settles it, a payload
    /// field ref per index, and where extraction reads those fields from.
    type private UnionArmPlan =
        {
            Test: UnionCaseTest
            /// Minted on demand, so a wildcard sub-pattern adds no `MemberRef` row.
            FieldRef: int -> EntityHandle
            Source: UnionArmSource
        }

    /// Resolve `caseName` of the union at `key` / `tyArgs` for one match arm — either from
    /// the union emitted here or from the provider's refs for one in a referenced package
    /// (`match o with Some x -> …`).
    let private resolveUnionArm
        (env: EmitEnv)
        (key: TypeKey)
        (tyArgs: FrozenType list)
        (qualName: string)
        (caseName: string)
        : UnionArmPlan =
        match env.Unions.TryGetValue key with
        | true, u ->
            let c = u.Cases.[caseName]

            // Tag / field access is a `Def` token for a monomorphic union, but a
            // `MemberRef` on the instantiated `TypeSpec` for a generic one
            // (`List<int>::_tag`).
            let fieldRef i =
                memberRef
                    env
                    u.Typars
                    key
                    tyArgs
                    (UserMemberKind.UnionMember(UnionMember.Field(caseName, i)))
                    c.Fields.[i]

            let caseTyToken =
                c.CaseType
                |> ValueOption.map (fun caseKey ->
                    let ty = FTClass(caseKey, EqArray.ofList tyArgs)
                    ty, env.Provider.TypeToken ty
                )

            // A match arm sits outside the union, so it reads the discriminant through the
            // accessor rather than the private field.
            let tagGetterRef () =
                match u.Tag with
                | ValueSome tag ->
                    memberRef env u.Typars key tyArgs (UserMemberKind.UnionMember UnionMember.GetTag) tag.Getter
                | ValueNone -> failwithf "Emit: union '%s' is not discriminated by a tag field" qualName

            let caseTypeToken () =
                match caseTyToken with
                | ValueSome(_, token) -> token
                | ValueNone -> failwithf "Emit: type-tested union '%s' nests no type for case '%s'" qualName caseName

            {
                Test = UnionCaseTest.ofRegime u.ValueKind u.Regime c.Tag tagGetterRef caseTypeToken
                FieldRef = fieldRef
                Source =
                    match caseTyToken with
                    | ValueSome(ty, token) -> UnionArmSource.CaseLocal(ty, token)
                    | ValueNone -> UnionArmSource.Scrutinee
            }
        | false, _ ->
            match env.Provider.ExternalUnionCaseTest(key, tyArgs, caseName) with
            | ValueSome test ->
                let fieldRef i =
                    match env.Provider.ExternalUnionCaseField(key, tyArgs, caseName, i) with
                    | ValueSome(fieldRef, _) -> fieldRef
                    | ValueNone -> failwithf "Emit: external union '%s' case '%s' has no field %d" qualName caseName i

                {
                    Test = test
                    FieldRef = fieldRef
                    Source =
                        match env.Provider.ExternalUnionCaseType(key, tyArgs, caseName) with
                        | ValueSome token -> UnionArmSource.CastScrutinee token
                        | ValueNone -> UnionArmSource.Scrutinee
                }
            | ValueNone -> failwithf "Emit: no emitted union for match on '%s'" qualName

    /// Test a pattern against the value in local `scrutSlot`: branch to `nextLabel` on
    /// a mismatch, and bind any pattern variables. `NamedSimple` aliases its bound variable to
    /// `scrutSlot` rather than copying, so a later load resolves to the same local.
    let rec buildMatchTest
        (env: EmitEnv)
        (b: IlBuilder)
        (scrutSlot: int)
        (nextLabel: int)
        (pat: TastAccessor.PatId)
        : unit =
        // `ldfld` a field off the value `pushSource` leaves on the stack into a fresh
        // local, then test the sub-pattern against that local. A record and a flat union
        // read straight off the scrutinee; a hierarchy union's case declares its payload on
        // the case type, so its source casts first.
        let extractFieldVia (pushSource: unit -> unit) (fieldRef: EntityHandle) (subPat: TastAccessor.PatId) =
            let fldSlot = b.Local(typeOfPat subPat)
            pushSource ()
            b.Add(ILInstr.Ldfld fieldRef)
            b.Add(ILInstr.Stloc fldSlot)
            buildMatchTest env b fldSlot nextLabel subPat

        let extractField = extractFieldVia (fun () -> b.Add(ILInstr.Ldloc scrutSlot))

        match TastAccessor.patKind pat with
        | PatShape.Wildcard -> ()
        | PatShape.NamedSimple ->
            match TastAccessor.patBoundVar pat with
            | ValueSome k -> env.Slots.[k] <- scrutSlot
            | ValueNone -> ()
        | PatShape.EnumCase ->
            let enumCase = TastAccessor.patEnumCase pat
            let enumKey = enumCase.EnumKey
            let caseName = enumCase.CaseName

            // A named case is a singleton, so this is equality only and binds nothing. A
            // string / mixed enum compares its backing field via
            // `EqualityComparer<field>.Default.Equals`, not a reference `ceq`.
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
        | PatShape.Null ->
            b.Add(ILInstr.Ldloc scrutSlot)
            b.Add(ILInstr.Brtrue nextLabel)
        | PatShape.Const ->
            let value = TastAccessor.patConstValue pat
            b.Add(ILInstr.Ldloc scrutSlot)

            match value with
            // The load carries the constant's own kind, including the pointer-width
            // conversion a `nativeint` needs before `bne.un` compares it.
            | TConstValue.Integral(k, bits) -> pushIntConst b k bits
            | TConstValue.Bool v -> b.Add(ILInstr.LdcI4(if v then 1 else 0))
            | TConstValue.Char c -> b.Add(ILInstr.LdcI4(int c))
            | other -> failwithf "Emit: match on constant %A is out of scope" other

            b.Add(ILInstr.BneUn nextLabel)
        | PatShape.Union ->
            let caseName = TastAccessor.patUnionCaseName pat
            let nominal = nominalOfPat pat
            let key, tyArgs = keyAndTyArgs nominal
            let qualName = SymbolKeyOps.typeMetaName key
            let plan = resolveUnionArm env key tyArgs qualName caseName

            let extracts =
                TastAccessor.patChildren pat
                |> Array.exists (fun subPat ->
                    match TastAccessor.patKind subPat with
                    | PatShape.Wildcard -> false
                    | _ -> true
                )

            // A local hierarchy case with payload to extract lands in a case-typed local:
            // the `isinst` test fills it directly, a tag test follows with one `castclass`.
            // An arm extracting nothing needs neither the local nor the cast.
            let caseLocal, pushSource =
                match plan.Source with
                | UnionArmSource.CaseLocal(ty, token) when extracts ->
                    let local = b.Local ty
                    ValueSome(local, token), (fun () -> b.Add(ILInstr.Ldloc local))
                | UnionArmSource.CaseLocal _
                | UnionArmSource.Scrutinee -> ValueNone, (fun () -> b.Add(ILInstr.Ldloc scrutSlot))
                | UnionArmSource.CastScrutinee token ->
                    ValueNone,
                    (fun () ->
                        b.Add(ILInstr.Ldloc scrutSlot)
                        b.Add(ILInstr.Castclass token)
                    )

            match plan.Test with
            | UnionCaseTest.Irrefutable -> ()
            | UnionCaseTest.TagEquals t ->
                // Skip the arm unless `scrut.Tag = case.Tag`. A value type is called on
                // its address.
                b.Add(
                    if t.ValueKind.IsValueType then
                        ILInstr.Ldloca scrutSlot
                    else
                        ILInstr.Ldloc scrutSlot
                )

                b.Add(ILInstr.Call(t.Getter, 1, 1))
                b.Add(ILInstr.LdcI4 t.Tag)
                b.Add(ILInstr.BneUn nextLabel)

                match caseLocal with
                | ValueSome(local, token) ->
                    // `castclass` rather than a second `isinst`: the tag test settled it.
                    b.Add(ILInstr.Ldloc scrutSlot)
                    b.Add(ILInstr.Castclass token)
                    b.Add(ILInstr.Stloc local)
                | ValueNone -> ()
            | UnionCaseTest.IsInst token ->
                b.Add(ILInstr.Ldloc scrutSlot)
                b.Add(ILInstr.Isinst token)

                match caseLocal with
                | ValueSome(local, _) ->
                    b.Add(ILInstr.Stloc local)
                    b.Add(ILInstr.Ldloc local)
                | ValueNone -> ()

                b.Add(ILInstr.Brfalse nextLabel)

            TastAccessor.patChildren pat
            |> Array.iteri (fun i subPat ->
                match TastAccessor.patKind subPat with
                | PatShape.Wildcard -> ()
                | _ -> extractFieldVia pushSource (plan.FieldRef i) subPat
            )
        | PatShape.Record ->
            let fields = TastAccessor.patRecordFields pat
            // A record pattern has no tag to compare, so it never fails on shape and
            // only its sub-patterns can branch to `nextLabel`.
            let nominal = nominalOfPat pat
            let key, tyArgs = keyAndTyArgs nominal

            match env.Records.TryGetValue key with
            | true, r ->
                for (fieldName, subPat) in fields do
                    match TastAccessor.patKind subPat with
                    | PatShape.Wildcard -> ()
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
        | PatShape.Tuple ->
            // A `ValueTuple` has no tag, so only the sub-patterns can branch.
            destructureTuple
                env
                b
                scrutSlot
                (TastAccessor.patTy pat)
                (TastAccessor.patChildren pat)
                (fun s p -> buildMatchTest env b s nextLabel p)
        | PatShape.TypeTestAs ->
            let testTy = TastAccessor.patTypeTestTestTy pat
            let inner = TastAccessor.patChild pat 0
            // `:? T as x` → `isinst T` then a null check (`brfalse` skips the arm). `isinst`
            // leaves a BOXED `T`, so a value-type or open-typar target is null-checked as the
            // `obj` it is and then `unbox.any`ed into the `T`-typed local the inner pattern
            // binds against. Only a reference `T` can be stored and branched on directly.
            let token = env.Provider.TypeToken testTy
            b.Add(ILInstr.Ldloc scrutSlot)
            b.Add(ILInstr.Isinst token)

            match clrRepr env testTy with
            | ClrRepr.Reference ->
                let castSlot = b.Local testTy
                b.Add(ILInstr.Stloc castSlot)
                b.Add(ILInstr.Ldloc castSlot)
                b.Add(ILInstr.Brfalse nextLabel)
                buildMatchTest env b castSlot nextLabel inner
            | ClrRepr.Value
            | ClrRepr.Boxable _ ->
                let boxedSlot = b.Local(FTConst(RuntimeNames.objKey, EqArray.empty))
                b.Add(ILInstr.Stloc boxedSlot)
                b.Add(ILInstr.Ldloc boxedSlot)
                b.Add(ILInstr.Brfalse nextLabel)
                let valSlot = b.Local testTy
                b.Add(ILInstr.Ldloc boxedSlot)
                b.Add(ILInstr.UnboxAny token)
                b.Add(ILInstr.Stloc valSlot)
                buildMatchTest env b valSlot nextLabel inner
        | PatShape.Or ->
            let alts = TastAccessor.patChildren pat
            // Every alternative tests the same scrutinee and binds nothing: an or-pattern
            // that binds names is rejected before lowering.
            let matchedLabel = b.Label()
            let n = alts.Length

            alts
            |> Array.iteri (fun i alt ->
                if i = n - 1 then
                    buildMatchTest env b scrutSlot nextLabel alt
                else
                    let altFail = b.Label()
                    buildMatchTest env b scrutSlot altFail alt
                    b.Add(ILInstr.Br matchedLabel)
                    b.Add(ILInstr.Mark altFail)
            )

            b.Add(ILInstr.Mark matchedLabel)

    /// Bind an *irrefutable* pattern against the value in local `srcSlot`, the `let` /
    /// `for-in` destructuring bound variable. It emits no branch at all: shape is assumed to
    /// match rather than tested, and `NamedSimple` aliases `srcSlot` rather than copying.
    let rec bindPattern (env: EmitEnv) (b: IlBuilder) (srcSlot: int) (pat: TastAccessor.PatId) : unit =
        match TastAccessor.patKind pat with
        | PatShape.Wildcard -> ()
        | PatShape.Const -> () // irrefutable in a binding position, so no compare and no bind
        | PatShape.NamedSimple ->
            match TastAccessor.patBoundVar pat with
            | ValueSome k -> env.Slots.[k] <- srcSlot
            | ValueNone -> ()
        | PatShape.Tuple ->
            destructureTuple env b srcSlot (TastAccessor.patTy pat) (TastAccessor.patChildren pat) (bindPattern env b)
        | _ -> failwithf "Emit: destructuring pattern is out of scope: %A" pat

    /// The fallthrough when no arm matched: a `throw` terminates the path off the last
    /// arm, and gives a non-exhaustive match defined behaviour.
    let buildMatchFailure (env: EmitEnv) (b: IlBuilder) : unit =
        b.Add(ILInstr.Ldstr(env.Ctx.UserString "The match cases were incomplete"))
        b.Add(ILInstr.Newobj(env.Provider.ExceptionCtor, 1))
        b.Add ILInstr.Throw
