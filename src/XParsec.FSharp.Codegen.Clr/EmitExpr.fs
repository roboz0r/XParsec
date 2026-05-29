namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis
open EmitTypes
open EmitLower

module EmitExpr =
    /// Load a variable for the current method: a method parameter (`ldarg.i`),
    /// the recursive self of a closure (`this`, `ldarg.0`), a capture
    /// (`ldarg.0; ldfld`), or a local slot (`ldloc`).
    let private buildVarLoad (env: EmitEnv) (b: IlBuilder) (key: NodeKey) : unit =
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
                    | false, _ -> failwithf "Emit: no binding for variable %O" key

    /// Test a pattern against the value already stored in local `scrutSlot`:
    /// branch to `nextLabel` on mismatch, and bind any pattern variables. A
    /// `Const` compares (`bne.un` skips the arm); `Wildcard` / `NamedSimple`
    /// always match (the latter aliases its binding to `scrutSlot`, so
    /// `emitVarLoad` resolves it to the same local — no copy). Union / tuple /
    /// record patterns land in later rung-2 slices.
    let rec private buildMatchTest (env: EmitEnv) (b: IlBuilder) (scrutSlot: int) (nextLabel: int) (pat: TPat) : unit =
        match pat with
        | TPat.Wildcard _ -> ()
        | TPat.NamedSimple(binding, _) -> env.Slots.[binding] <- scrutSlot
        | TPat.Const(value, _) ->
            b.Add(ILInstr.Ldloc scrutSlot)

            match value with
            | TConstValue.Int n -> b.Add(ILInstr.LdcI4 n)
            | TConstValue.Bool v -> b.Add(ILInstr.LdcI4(if v then 1 else 0))
            | TConstValue.Byte n -> b.Add(ILInstr.LdcI4(int n))
            | TConstValue.Char c -> b.Add(ILInstr.LdcI4(int c))
            | other -> failwithf "Emit: match on constant %A is out of scope" other

            b.Add(ILInstr.BneUn nextLabel)
        | TPat.Union(caseName, subPats, ty) ->
            let typeName, tyArgs =
                match receiverShape ty with
                | ValueSome(n, xs) -> n, xs
                | ValueNone -> failwithf "Emit: union pattern with non-nominal type %A" ty

            match env.Unions.TryGetValue typeName with
            | true, u ->
                let c = u.Cases.[caseName]

                // Tag / field access is a `Def` token for a monomorphic union, but
                // a `MemberRef` on the instantiated `TypeSpec` for a generic one
                // (`List<int>::_tag` etc.) — see `EmittedUnion.Typars` (P3d.4).
                let tagRef =
                    if List.isEmpty u.Typars then
                        u.TagField
                    else
                        env.Provider.UserGenericMemberRef(typeName, tyArgs, UserMemberKind.UnionMember UnionMember.Tag)

                // Skip the arm unless `scrut._tag = case.Tag`.
                b.Add(ILInstr.Ldloc scrutSlot)
                b.Add(ILInstr.Ldfld tagRef)
                b.Add(ILInstr.LdcI4 c.Tag)
                b.Add(ILInstr.BneUn nextLabel)

                // Extract each non-wildcard field into a fresh local, then test
                // its sub-pattern (a named sub-pattern just aliases that local).
                subPats
                |> EqArray.iteri (fun i subPat ->
                    match subPat with
                    | TPat.Wildcard _ -> ()
                    | _ ->
                        let fieldRef =
                            if List.isEmpty u.Typars then
                                c.Fields.[i]
                            else
                                env.Provider.UserGenericMemberRef(
                                    typeName,
                                    tyArgs,
                                    UserMemberKind.UnionMember(UnionMember.Field(caseName, i))
                                )

                        let fldSlot = b.Local(typeOfPat subPat)
                        b.Add(ILInstr.Ldloc scrutSlot)
                        b.Add(ILInstr.Ldfld fieldRef)
                        b.Add(ILInstr.Stloc fldSlot)
                        buildMatchTest env b fldSlot nextLabel subPat
                )
            | false, _ -> failwithf "Emit: no emitted union for match on '%s'" typeName
        | TPat.Record(fields, ty) ->
            // A record pattern never fails on shape (no tag to compare): for each
            // named sub-pattern, `ldfld` the field into a fresh local and recurse
            // — only the sub-patterns themselves can branch to `nextLabel`. A
            // wildcard sub-pattern is skipped (it would always match), exactly
            // like the union arm above.
            let typeName, tyArgs =
                match receiverShape ty with
                | ValueSome(n, xs) -> n, xs
                | ValueNone -> failwithf "Emit: record pattern with non-nominal type %A" ty

            match env.Records.TryGetValue typeName with
            | true, r ->
                for (fieldName, subPat) in fields do
                    match subPat with
                    | TPat.Wildcard _ -> ()
                    | _ ->
                        match r.Fields |> List.tryFind (fun (n, _, _) -> n = fieldName) with
                        | Some(_, handle, _) ->
                            let fieldRef =
                                if List.isEmpty r.Typars then
                                    handle
                                else
                                    env.Provider.UserGenericMemberRef(
                                        typeName,
                                        tyArgs,
                                        UserMemberKind.RecordMember(RecordMember.Field fieldName)
                                    )

                            let fldSlot = b.Local(typeOfPat subPat)
                            b.Add(ILInstr.Ldloc scrutSlot)
                            b.Add(ILInstr.Ldfld fieldRef)
                            b.Add(ILInstr.Stloc fldSlot)
                            buildMatchTest env b fldSlot nextLabel subPat
                        | None -> failwithf "Emit: record '%s' has no field '%s'" typeName fieldName
            | false, _ -> failwithf "Emit: no emitted record for pattern on '%s'" typeName
        | other -> failwithf "Emit: match pattern is out of scope: %A" other

    /// The fallthrough a `match` reaches when no arm matched — `throw new
    /// System.Exception("…")`. An exhaustive match never reaches it at runtime,
    /// but it keeps the emitted IL well-formed (and gives a non-exhaustive one
    /// defined behaviour).
    let private buildMatchFailure (env: EmitEnv) (b: IlBuilder) : unit =
        b.Add(ILInstr.Ldstr(env.Ctx.UserString "The match cases were incomplete"))
        b.Add(ILInstr.Newobj(env.Provider.ExceptionCtor, 1))
        b.Add ILInstr.Throw

    /// Resolve the member-call handle for an instance access on `receiverTy`
    /// (P3d.3, generalised to generic unions in R2 and to classes in Phase 1 /
    /// B-1). A monomorphic union/class uses the member's `Def` token directly;
    /// a *generic* one goes through a `MemberRef` on the receiver's
    /// instantiated `TypeSpec` (`List<int>::get_Head`, `Box<int>::get_Value`).
    let private resolveInstanceMember (env: EmitEnv) (receiverTy: SemType) (name: string) : EntityHandle =
        let typeName, tyArgs =
            match receiverShape receiverTy with
            | ValueSome(n, xs) -> n, xs
            | ValueNone -> failwithf "Emit: member '%s' access on non-nominal receiver %A" name receiverTy

        match env.Unions.TryGetValue typeName with
        | true, u ->
            match u.Members.TryGetValue name with
            | true, m ->
                if List.isEmpty u.Typars then
                    m.Handle
                else
                    env.Provider.UserGenericMemberRef(
                        typeName,
                        tyArgs,
                        UserMemberKind.UnionMember(UnionMember.Member(m.MetaName, false, m.ParamTys, m.RetTy))
                    )
            | false, _ -> failwithf "Emit: union '%s' has no emitted member '%s'" typeName name
        | false, _ ->
            match env.Classes.TryGetValue typeName with
            | true, c ->
                match c.Members.TryGetValue name with
                | true, m ->
                    if List.isEmpty c.Typars then
                        m.Handle
                    else
                        env.Provider.UserGenericMemberRef(
                            typeName,
                            tyArgs,
                            UserMemberKind.ClassMember(ClassMember.Member(m.MetaName, false, m.ParamTys, m.RetTy))
                        )
                | false, _ -> failwithf "Emit: class '%s' has no emitted member '%s'" typeName name
            | false, _ -> failwithf "Emit: no emitted type carrying members for receiver '%s'" typeName

    /// The static-member equivalent. Generic-union *static* augmentation members
    /// are out of scope in R2 (a static member's typars aren't tied to the type's
    /// via `this`, so the front-end leaves them un-remapped — the type's generic
    /// `Cons` / `Empty` come from its case factories instead), so a generic union
    /// fails here loudly rather than minting a malformed `Def` call. Classes
    /// route through the same `Member` arm as instances; a generic class's
    /// static member uses the class `MemberRef` instead of the union one.
    let private resolveStaticMember (env: EmitEnv) (typeName: string) (name: string) : EntityHandle =
        match env.Unions.TryGetValue typeName with
        | true, u ->
            match u.Members.TryGetValue name with
            | true, m ->
                if List.isEmpty u.Typars then
                    m.Handle
                else
                    failwithf
                        "Emit: generic-union static augmentation member '%s.%s' is out of scope (R2)"
                        typeName
                        name
            | false, _ -> failwithf "Emit: union '%s' has no emitted static member '%s'" typeName name
        | false, _ ->
            match env.Classes.TryGetValue typeName with
            | true, c ->
                match c.Members.TryGetValue name with
                | true, m ->
                    if List.isEmpty c.Typars then
                        m.Handle
                    else
                        env.Provider.UserGenericMemberRef(
                            typeName,
                            [ for t in c.Typars -> TyConst t ],
                            UserMemberKind.ClassMember(ClassMember.Member(m.MetaName, true, m.ParamTys, m.RetTy))
                        )
                | false, _ -> failwithf "Emit: class '%s' has no emitted static member '%s'" typeName name
            | false, _ -> failwithf "Emit: no emitted type carrying static members for '%s'" typeName

    /// Resolve a class `static let` backing field to its `ldsfld`/`stsfld` handle
    /// (vesper-set-sprint-plan §1.8 / B-10). Only monomorphic classes declare
    /// `static let`s (generic `static let` is deferred), so the field handle is
    /// always a `Def` token — no `MemberRef`-on-`TypeSpec` path.
    let private resolveStaticField (env: EmitEnv) (typeName: string) (name: string) : EntityHandle =
        match env.Classes.TryGetValue typeName with
        | true, c ->
            match c.StaticFields.TryGetValue name with
            | true, h -> h
            | false, _ -> failwithf "Emit: class '%s' has no emitted static field '%s'" typeName name
        | false, _ -> failwithf "Emit: no emitted class carrying static fields for '%s'" typeName

    /// Resolve a field by name on a record / class receiver to its emit
    /// handle and declared type. A monomorphic type returns the field's
    /// `Def` token; a *generic* one returns a `MemberRef` on the receiver's
    /// instantiated `TypeSpec` (`Box<int>::Value`) — the records-plan §B3
    /// mirror of `resolveInstanceMember`. A referenced-assembly record
    /// (records-plan §B7) goes through the provider's
    /// `TryResolveExternalRecordField`. Classes reach here for primary-
    /// ctor parameter accesses rewritten to `FieldGet(this, name)` by
    /// `Freeze.translateClassMember` (vesper-set-sprint-plan Phase 1 / B-1).
    let private resolveRecordField (env: EmitEnv) (receiverTy: SemType) (fieldName: string) : EntityHandle * SemType =
        let typeName, tyArgs =
            match receiverShape receiverTy with
            | ValueSome(n, xs) -> n, xs
            | ValueNone -> failwithf "Emit: field '%s' access on non-nominal receiver %A" fieldName receiverTy

        match env.Records.TryGetValue typeName with
        | true, r ->
            match r.Fields |> List.tryFind (fun (n, _, _) -> n = fieldName) with
            | Some(_, h, ty) ->
                let handle =
                    if List.isEmpty r.Typars then
                        h
                    else
                        env.Provider.UserGenericMemberRef(
                            typeName,
                            tyArgs,
                            UserMemberKind.RecordMember(RecordMember.Field fieldName)
                        )

                handle, ty
            | None -> failwithf "Emit: record '%s' has no field '%s'" typeName fieldName
        | false, _ ->
            match env.Classes.TryGetValue typeName with
            | true, c ->
                match c.Fields |> List.tryFind (fun (n, _, _) -> n = fieldName) with
                | Some(_, h, ty) ->
                    let handle =
                        if List.isEmpty c.Typars then
                            h
                        else
                            env.Provider.UserGenericMemberRef(
                                typeName,
                                tyArgs,
                                UserMemberKind.ClassMember(ClassMember.Field fieldName)
                            )

                    handle, ty
                | None -> failwithf "Emit: class '%s' has no field '%s'" typeName fieldName
            | false, _ ->
                match env.Provider.TryResolveExternalRecordField(typeName, tyArgs, fieldName) with
                | ValueSome(handle, ty) -> handle, ty
                | ValueNone -> failwithf "Emit: no emitted type for field access on '%s'" typeName

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

    let rec buildExpr (env: EmitEnv) (b: IlBuilder) (e: TExpr) : unit =
        match e with
        | TExpr.Const(TConstValue.String s, _) -> b.Add(ILInstr.Ldstr(env.Ctx.UserString s))
        | TExpr.Const(TConstValue.Int n, _) -> b.Add(ILInstr.LdcI4 n)
        | TExpr.Const(TConstValue.Bool v, _) -> b.Add(ILInstr.LdcI4(if v then 1 else 0))
        | TExpr.Const(TConstValue.Byte n, _) -> b.Add(ILInstr.LdcI4(int n))
        | TExpr.Const(TConstValue.Float x, _) -> b.Add(ILInstr.LdcR8 x)
        | TExpr.Const(TConstValue.Char c, _) -> b.Add(ILInstr.LdcI4(int c))
        | TExpr.Const(TConstValue.Decimal d, _) ->
            // Materialise via `Decimal..ctor(lo, mid, hi, isNegative, scale)` from
            // the value's bit representation — the same shape F#/Roslyn emit.
            let bits = System.Decimal.GetBits d
            let flags = bits.[3]
            b.Add(ILInstr.LdcI4 bits.[0]) // lo
            b.Add(ILInstr.LdcI4 bits.[1]) // mid
            b.Add(ILInstr.LdcI4 bits.[2]) // hi
            b.Add(ILInstr.LdcI4(if flags < 0 then 1 else 0)) // sign (high bit of flags)
            b.Add(ILInstr.LdcI4((flags >>> 16) &&& 0xFF)) // scale
            b.Add(ILInstr.Newobj(env.Provider.DecimalCtor, 5))
        | TExpr.Const(TConstValue.Unit, _) ->
            // `()` literal — the unit value is `Unit`'s null (encodeTypeCore
            // maps `unit` to `FSharp.Core.Unit`, whose canonical value is
            // `null`). Pushed when a closure invocation needs a unit arg
            // (`c ()`) or a unit value is otherwise reified — F3 (Phase 2 §1
            // mkCounter pattern).
            b.Add ILInstr.Ldnull

        | TExpr.Var(binding, _) -> buildVarLoad env b binding

        | TExpr.Let(TPat.NamedSimple(binding, ty), value, body, _) ->
            let slot = b.Local ty
            env.Slots.[binding] <- slot
            buildExpr env b value
            b.Add(ILInstr.Stloc slot)
            buildExpr env b body
        | TExpr.Let(pat, _, _, _) -> failwithf "Emit: destructuring let-binding is out of scope: %A" pat

        | TExpr.Sequential(items, _) ->
            // Every item but the last is a unit-typed statement: emit it and
            // discard whatever value it leaves (popping back to the pre-item
            // depth); the last item leaves the sequence's result.
            let n = items.Length

            items
            |> EqArray.iteri (fun i it ->
                if i = n - 1 then
                    buildExpr env b it
                else
                    let baseDepth = b.Depth
                    buildExpr env b it

                    while b.Depth > baseDepth do
                        b.Add ILInstr.Pop
            )

        | TExpr.IfThenElse(cond, thenExpr, elseExpr, _) ->
            // `<cond>; brfalse else; <then>; br end; else: <else>; end:`. Both
            // arms leave one value; the builder's linear depth tracker (which
            // follows only the then-arm) is reset to the post-`brfalse` base
            // before the else-arm so subsequent statement-discards stay correct —
            // the *buffer's* merge depths are re-derived by `IlIr.analyze`.
            let elseLabel = b.Label()
            let endLabel = b.Label()
            buildExpr env b cond
            b.Add(ILInstr.Brfalse elseLabel)
            let baseDepth = b.Depth
            buildExpr env b thenExpr
            b.Add(ILInstr.Br endLabel)
            b.SetDepth baseDepth
            b.Add(ILInstr.Mark elseLabel)
            buildExpr env b elseExpr
            b.Add(ILInstr.Mark endLabel)

        | TExpr.Match(scrutinee, arms, _) ->
            // Evaluate the scrutinee once into a local, then test each arm in
            // order: on a mismatch branch to the next arm; on a match (and a
            // passing guard) emit the body and branch to the shared end. The
            // builder's depth tracker is reset to the post-scrutinee base before
            // each arm and before the end label (every body leaves one result);
            // `IlIr.analyze` re-derives the buffer's merge depths.
            let scrutSlot = b.Local(typeOfExpr scrutinee)
            buildExpr env b scrutinee
            b.Add(ILInstr.Stloc scrutSlot)
            let baseDepth = b.Depth
            let endLabel = b.Label()

            for arm in arms do
                let nextLabel = b.Label()
                buildMatchTest env b scrutSlot nextLabel arm.Pat

                match arm.Guard with
                | Some g ->
                    buildExpr env b g
                    b.Add(ILInstr.Brfalse nextLabel)
                | None -> ()

                buildExpr env b arm.Body
                b.Add(ILInstr.Br endLabel)
                b.SetDepth baseDepth
                b.Add(ILInstr.Mark nextLabel)

            buildMatchFailure env b
            b.SetDepth(baseDepth + 1)
            b.Add(ILInstr.Mark endLabel)

        | TExpr.Lambda _ ->
            // A function value: construct its closure. Captures are pushed via
            // the *current* resolver (a local in `Main`, the param or a capture
            // inside an enclosing closure), then `newobj` its ctor.
            //
            // A *generic* closure (function-representation-plan §Generic closures, C3) routes the `Newobj`
            // through a `MemberRef` on `<closure>$n<args>`, where `args` is the
            // closure's typars zonked at the call site (`!!i` inside the
            // enclosing static method's body, `!i` inside an enclosing closure's
            // `Invoke`) — both encodings reference the same TypeVar roots, and
            // the parent's `TypeSpec` captures the use-site instantiation.
            match env.ClosureByNode.TryGetValue e with
            | true, closure ->
                for (k, _) in closure.Captures do
                    buildVarLoad env b k

                let ctorHandle =
                    if List.isEmpty closure.Typars then
                        match env.CtorHandleByNode.TryGetValue e with
                        | true, ctor -> ctor
                        | false, _ ->
                            failwith "Emit: closure constructor not yet emitted (leaves-first ordering broken)"
                    else
                        env.Provider.UserGenericMemberRef(
                            closure.Name,
                            closure.Typars |> List.map TyVar,
                            UserMemberKind.ClosureMember ClosureMember.Ctor
                        )

                b.Add(ILInstr.Newobj(ctorHandle, List.length closure.Captures))
            | false, _ -> failwith "Emit: a Lambda value was not discovered as a closure"

        | TExpr.New(className, args, ty) ->
            for a in args do
                buildExpr env b a

            let tyArgs =
                match ty with
                | TyClass(_, xs) -> EqArray.toList xs
                | _ -> []

            // Call-site arg types let the external-ctor path disambiguate ctor
            // overloads (v1 picker is arity-only — see `ClrProvider.externalCtor`).
            let argTypes = [ for a in args -> typeOfExpr a ]

            match env.Classes.TryGetValue className with
            | true, c ->
                // A user class emitted into this assembly (vesper-set-sprint-plan
                // Phase 1 / B-1). Monomorphic: use the ctor's `Def` token directly.
                // Generic: mint a `MemberRef` on the receiver's instantiated
                // `TypeSpec` (`Box<int>::.ctor`), mirroring the generic-record
                // ctor path.
                let ctorRef =
                    if List.isEmpty c.Typars then
                        c.Ctor
                    else
                        env.Provider.UserGenericMemberRef(
                            className,
                            tyArgs,
                            UserMemberKind.ClassMember ClassMember.Ctor
                        )

                b.Add(ILInstr.Newobj(ctorRef, List.length c.Fields))
            | false, _ ->
                match env.Provider.TryEmitCtor(className, tyArgs, argTypes) with
                | ValueSome recipe -> b.Add(ILInstr.Newobj(recipe.Handle, recipe.ArgCount))
                | ValueNone -> failwithf "Emit: no constructor recipe for '%s'" className

        | TExpr.App _ -> buildAppCall env b e

        | TExpr.RecordCons(srcFields, ty) ->
            // The source-order initialiser list (`{ Y = …; X = … }`) is reordered
            // to the type's *declaration* order before the ctor is invoked
            // (records-plan §B3): the ctor's parameter slots correspond to
            // declaration order so the field-store sequence in `buildRecordCtor`
            // lines up. A generic record's `.ctor` is a `MemberRef` on its own
            // `TypeSpec` (`Box\`1<!0>::.ctor`), exactly like a generic union's
            // factory.
            let typeName, tyArgs =
                match receiverShape ty with
                | ValueSome(n, xs) -> n, xs
                | ValueNone -> failwithf "Emit: RecordCons with non-nominal type %A" ty

            match env.Records.TryGetValue typeName with
            | true, r ->
                let srcMap = Map.ofSeq srcFields.Underlying

                for (fieldName, _, _) in r.Fields do
                    match Map.tryFind fieldName srcMap with
                    | Some e -> buildExpr env b e
                    | None ->
                        failwithf
                            "Emit: record literal for '%s' is missing initialiser for field '%s'"
                            typeName
                            fieldName

                let ctor =
                    if List.isEmpty r.Typars then
                        r.Ctor
                    else
                        env.Provider.UserGenericMemberRef(
                            typeName,
                            tyArgs,
                            UserMemberKind.RecordMember RecordMember.Ctor
                        )

                b.Add(ILInstr.Newobj(ctor, List.length r.Fields))
            | false, _ ->
                // Records-handoff Phase 2 follow-up F2: the record lives in a
                // referenced assembly (`Vesper.Ref\`1` in `Vesper.Core.dll`,
                // routed here from `RefCellPromotion`). The provider mints a
                // `MemberRef` on its instantiated `TypeSpec`; field arguments are
                // pushed in source order (the contract layer's field order is
                // also the declaration order, which matches the ctor's parameter
                // layout, so no reorder is required for the supported one-field
                // `Ref<'T>` shape — multi-field external records will revisit).
                let fieldNames = [ for (n, _) in srcFields -> n ]

                match env.Provider.TryEmitRecordCons(typeName, tyArgs, fieldNames) with
                | ValueSome recipe ->
                    for (_, e) in srcFields do
                        buildExpr env b e

                    b.Add(ILInstr.Newobj(recipe.Handle, recipe.ArgCount))
                | ValueNone -> failwithf "Emit: no emitted record for '%s'" typeName

        | TExpr.FieldGet(receiver, name, _) ->
            // `r.X` — load the receiver and `ldfld` the field. The field handle is
            // a `Def` token for a monomorphic record, a `MemberRef` on the receiver's
            // `TypeSpec` for a generic one (`resolveRecordField`). A
            // referenced-assembly record (F2) routes through the provider.
            let handle, _ = resolveRecordField env (typeOfExpr receiver) name
            buildExpr env b receiver
            b.Add(ILInstr.Ldfld handle)

        | TExpr.FieldSet(receiver, name, value, _) ->
            // `r.X <- v` on a `mutable` field. Validation has rejected the
            // immutable case before we reach here. `stfld` consumes both pushes
            // and leaves nothing on the stack, but a `FieldSet` is *unit-typed*
            // — every consumer (`Sequential` middle items, the body of a
            // unit-returning closure / static method) expects a unit value to be
            // present. Push `ldnull` (Unit's value) to match F#'s emission and
            // keep the IL verifier happy when the body is just a FieldSet
            // (`fun () -> n <- n + 1`, F3 §1).
            let handle, _ = resolveRecordField env (typeOfExpr receiver) name
            buildExpr env b receiver
            buildExpr env b value
            b.Add(ILInstr.Stfld handle)
            b.Add ILInstr.Ldnull // unit value

        | TExpr.RecordClone(source, overrides, ty) ->
            // `{ r with X = v; … }` — evaluate `r` into a local, then per
            // declaration-order field: push the override expression if it's in
            // the override list, else `ldloc; ldfld` from the saved source. Then
            // `newobj` the ctor. Direct field reads (no `MemberwiseClone`) keeps
            // it BCL-only and works identically for a generic record.
            let typeName, tyArgs =
                match receiverShape ty with
                | ValueSome(n, xs) -> n, xs
                | ValueNone -> failwithf "Emit: RecordClone with non-nominal type %A" ty

            match env.Records.TryGetValue typeName with
            | true, r ->
                let overrideMap = Map.ofSeq overrides.Underlying
                let srcSlot = b.Local ty
                buildExpr env b source
                b.Add(ILInstr.Stloc srcSlot)

                for (fieldName, handle, _) in r.Fields do
                    match Map.tryFind fieldName overrideMap with
                    | Some e -> buildExpr env b e
                    | None ->
                        let fieldRef =
                            if List.isEmpty r.Typars then
                                handle
                            else
                                env.Provider.UserGenericMemberRef(
                                    typeName,
                                    tyArgs,
                                    UserMemberKind.RecordMember(RecordMember.Field fieldName)
                                )

                        b.Add(ILInstr.Ldloc srcSlot)
                        b.Add(ILInstr.Ldfld fieldRef)

                let ctor =
                    if List.isEmpty r.Typars then
                        r.Ctor
                    else
                        env.Provider.UserGenericMemberRef(
                            typeName,
                            tyArgs,
                            UserMemberKind.RecordMember RecordMember.Ctor
                        )

                b.Add(ILInstr.Newobj(ctor, List.length r.Fields))
            | false, _ -> failwithf "Emit: no emitted record for '%s'" typeName

        | TExpr.UnionCons(caseName, args, ty) ->
            let typeName, tyArgs =
                match receiverShape ty with
                | ValueSome(n, xs) -> n, xs
                | ValueNone -> failwithf "Emit: UnionCons with non-nominal type %A" ty

            for a in args do
                buildExpr env b a

            match env.Unions.TryGetValue typeName with
            | true, u ->
                // Our own emitted union: `call` the case's static factory (the
                // fields are already on the stack in declaration order). A
                // monomorphic factory is a `Def` token; a generic one is a
                // `MemberRef` on the instantiated `TypeSpec` (`List<int>::Cons`).
                let factoryRef =
                    if List.isEmpty u.Typars then
                        u.Cases.[caseName].Factory
                    else
                        env.Provider.UserGenericMemberRef(
                            typeName,
                            tyArgs,
                            UserMemberKind.UnionMember(UnionMember.Factory caseName)
                        )

                b.Add(ILInstr.Call(factoryRef, args.Length, 1))
            | false, _ ->
                // The provider's special-case (FSharp.Core list) for `[]` / `::`.
                match env.Provider.TryEmitUnionCons(typeName, caseName, tyArgs) with
                | ValueSome recipe -> b.Add(ILInstr.Recipe recipe)
                | ValueNone -> failwithf "Emit: no union-cons recipe for %s.%s" typeName caseName

        | TExpr.PropertyGet(receiver, name, _) ->
            // Instance property read: load the receiver, then dispatch.
            // Unions/records are sealed (rung 2) so `call` is safe and avoids
            // the null check. User classes (vesper-set-sprint-plan §1.7 /
            // B-1) emit `callvirt` uniformly — non-`override` members would
            // accept `call`, but `callvirt` is the safer default per the
            // plan, and Phase 2's `CallVia.Base` will switch the
            // base-dispatch case to `call`.
            let receiverTy = typeOfExpr receiver
            let handle = resolveInstanceMember env receiverTy name
            buildExpr env b receiver

            match receiverTy with
            | TyClass _ -> b.Add(ILInstr.Callvirt(handle, 1, 1))
            | _ -> b.Add(ILInstr.Call(handle, 1, 1))

        | TExpr.MethodCall(receiver, name, args, _) ->
            // Instance method call: receiver then args. Unions/records use
            // `call` (sealed, no virtual dispatch needed). User classes
            // (vesper-set-sprint-plan §1.7 / B-1) emit `callvirt` uniformly
            // for safety; Phase 2's base-dispatch switches to `call`.
            let receiverTy = typeOfExpr receiver
            let handle = resolveInstanceMember env receiverTy name
            buildExpr env b receiver

            for a in args do
                buildExpr env b a

            match receiverTy with
            | TyClass _ -> b.Add(ILInstr.Callvirt(handle, 1 + args.Length, 1))
            | _ -> b.Add(ILInstr.Call(handle, 1 + args.Length, 1))

        | TExpr.StaticPropertyGet(className, name, _) ->
            let handle = resolveStaticMember env className name
            b.Add(ILInstr.Call(handle, 0, 1))

        | TExpr.StaticFieldGet(className, name, _) ->
            let handle = resolveStaticField env className name
            b.Add(ILInstr.Ldsfld handle)

        | TExpr.StaticMethodCall(className, name, args, _) ->
            let handle = resolveStaticMember env className name

            for a in args do
                buildExpr env b a

            b.Add(ILInstr.Call(handle, args.Length, 1))

        | TExpr.ExternalMember(receiver, key, _, true, ty) ->
            // A standalone external *property* get (P4): a static one (`call
            // get_<name>()`) or an instance one reached as the receiver of an outer
            // access (`<receiver>; callvirt get_<name>()`). The keyed member ref is
            // minted from the node's `SymbolKey` (`ExternalMemberRef`).
            let isStatic = ValueOption.isNone receiver
            let handle = env.Provider.ExternalMemberRef(key, true, isStatic, zonk ty)

            match receiver with
            | ValueNone -> b.Add(ILInstr.Call(handle, 0, 1))
            | ValueSome r ->
                buildExpr env b r
                b.Add(ILInstr.Callvirt(handle, 1, 1))

        | TExpr.ExternalMember(_, _, _, false, _) ->
            // An external method used as a first-class value (a method group, not
            // applied) needs closure synthesis — out of scope. Applied methods are
            // handled as an `App` head above.
            failwith "Emit: external method used as a first-class value is out of scope"

        | TExpr.Format(sink, segments, _) -> buildFormat env b sink segments

        | TExpr.ILIntrinsic(opCode, args, _) ->
            // Push each operand, then append the mapped opcode. The dispatch
            // (which opcode for which operator/primitive) lives in the operator
            // `.fs` body this node was lowered from, not here — codegen only
            // interprets the IL. See docs/operators-plan.md.
            for a in args do
                buildExpr env b a

            // `throw` is terminal — it pops the exception and ends the path,
            // so it doesn't fit `tryOpCodeOfMnemonic`'s balanced-result shape.
            // Tolerated in value position the same way a non-exhaustive `match`
            // fallthrough is (`buildMatchFailure`): `Throw` never returns, so
            // no result is left on the stack.
            if opCode = "throw" then
                if args.Length <> 1 then
                    failwithf "Emit: %d-ary inline-IL instruction 'throw' is out of scope" args.Length

                b.Add ILInstr.Throw
            else
                match Cil.tryOpCodeOfMnemonic opCode with
                | ValueSome code ->
                    match args.Length with
                    | 2 -> b.Add(ILInstr.Bin code)
                    | 1 -> b.Add(ILInstr.Un code)
                    | n -> failwithf "Emit: %d-ary inline-IL instruction '%s' is out of scope" n opCode
                | ValueNone -> failwithf "Emit: unsupported inline-IL instruction '%s'" opCode

        | TExpr.StaticOptimization(_, def, _) ->
            // Reaching codegen unresolved means the function was never
            // inline-expanded against a concrete operand type (used as a
            // first-class value, or declared without `inline`). F#'s semantics
            // fall back to the leading (dynamic) expression in that case.
            buildExpr env b def

        | other -> failwithf "Emit: unsupported expression: %A" other

    /// Lower a `TExpr.App` chain. Split out of `buildExpr` so the upcoming
    /// class-spine work (B-1 `New(className, args)`, B-9 `Raise`, B-4
    /// `:>`/`:?`/`:?>`) can grow App-head shapes near here instead of inside a
    /// 600-line `buildExpr` match (vesper-set-sprint-plan §0.2 / M2). The head
    /// dispatch is shape-by-shape:
    /// - `TExpr.External(name, key, _)` — a provider-resolved call. The
    ///   recipe's generic instantiation is read from the head's full curried
    ///   type. `key` (the Freeze-stamped `SymbolKey.ValueKey`) lets codegen
    ///   route by identity, not name (Phase 0 §0.1).
    /// - `TExpr.Var k` where `env.StaticMethods.ContainsKey k` — a top-level
    ///   function emitted as a static method (P3b); generic instantiations are
    ///   recovered by matching declared param types against the actual arg
    ///   types (R3).
    /// - `TExpr.ExternalMember(receiver, key, name, false, memberTy)` — an
    ///   external method call (P4); tupled per .NET convention, so the call
    ///   consumes one spine element (the arg list) and the param count comes
    ///   from the key's `argSig` length.
    /// - otherwise — the head is itself a function value (a closure local or a
    ///   partially applied result); emit it, then `Invoke` each arg.
    and private buildAppCall (env: EmitEnv) (b: IlBuilder) (e: TExpr) : unit =
        let head, spineArgs = collectSpine [] e

        match head with
        | TExpr.External(name, key, _) ->
            // The recipe reads its generic instantiation from the head's
            // full curried type (`fnTy`). `key` is the resolved
            // `SymbolKey.ValueKey` stamped by Freeze when the front-end
            // resolved the name through the symbol provider — codegen
            // routes by identity, not name suffix
            // (vesper-set-sprint-plan §0.1 / M1).
            match env.Provider.TryEmitCall(name, key, typeOfExpr head) with
            | ValueSome recipe ->
                let leading, rest = List.splitAt recipe.ArgCount spineArgs

                for (a, _) in leading do
                    buildExpr env b a

                b.Add(ILInstr.Recipe recipe)

                // Whatever the recipe left on the stack — a function value
                // the rest of the spine is applied to.
                let funcTy =
                    match List.tryLast leading with
                    | Some(_, ty) -> ty
                    | None -> typeOfExpr head

                // The cold printf printer is an FSharp.Core `FSharpFunc`, so it
                // is applied via `FSharpFunc::Invoke`; every other recipe result
                // is a native `Vesper.Fun` (R1).
                if isColdPrintf key name then
                    foldInvokeFSharpFunc env b funcTy rest
                else
                    foldInvoke env b funcTy rest
            | ValueNone -> failwithf "Emit: no call recipe for external '%s'" name

        | TExpr.Var(k, _) when env.StaticMethods.ContainsKey k ->
            // A top-level function emitted as a static method (P3b): `call`
            // it with the first `Arity` args (always present — a non-saturated
            // use would have escaped to a closure, see `collectStaticFns`),
            // then `Invoke` the result with any remainder. A *generic* static
            // method (R3) `call`s a `MethodSpec` instantiating it — recovered
            // by matching its declared parameter types against the actual
            // argument types (recursion yields the method's own typars ⇒ `!!i`).
            let sm = env.StaticMethods.[k]
            let leading, rest = List.splitAt sm.Arity spineArgs

            for (a, _) in leading do
                buildExpr env b a

            let callHandle =
                if List.isEmpty sm.Typars then
                    sm.Handle
                else
                    // Each spine arg's *own* type (`collectSpine` pairs it with
                    // the application's *result* type instead), matched against
                    // the declared parameter types to recover the instantiation.
                    let actualTys = leading |> List.map (fun (a, _) -> typeOfExpr a)
                    let inst = matchInstantiation sm.Typars sm.ParamTys actualTys
                    env.Provider.StaticFnMethodSpec(sm.Handle, inst)

            b.Add(ILInstr.Call(callHandle, sm.Arity, 1))
            foldInvoke env b sm.ResultTy rest

        | TExpr.ExternalMember(receiver, key, name, false, memberTy) ->
            // An external instance/static *method* call (P4): push the receiver
            // (instance only) beneath the arguments, then `call` (static) /
            // `callvirt` (instance) the keyed member ref. A .NET method is
            // **tupled** (`m(a, b)` = one application to `(a, b)`), so the call
            // consumes a single spine element — the argument list — and the
            // parameter count comes from the chosen key's `argSig` length
            // (authoritative: `memberTy` alone can't tell a flattened 2-param
            // method from a genuine single `(int*int)` param — type-args-bug.md
            // Layer 3). A literal `TExpr.Tuple` argument is pushed element-wise
            // (no tuple object is constructed).
            let isStatic = ValueOption.isNone receiver

            let argCount =
                match key with
                | SymbolKey.MemberKey(_, _, argSig, _) -> argSig.Length
                | other -> failwithf "Emit: ExternalMember key is not a MemberKey: %A" other

            // The method consumes one spine element (its argument list); any
            // remainder is further application of the result (rare).
            let argList, rest =
                match spineArgs with
                | first :: more -> ValueSome first, more
                | [] -> ValueNone, []

            match receiver with
            | ValueSome r -> buildExpr env b r
            | ValueNone -> ()

            let pushedArgs =
                match argList with
                | ValueNone -> 0 // no argument supplied (a 0-param method)
                | ValueSome(argExpr, _) ->
                    if argCount >= 2 then
                        match argExpr with
                        | TExpr.Tuple(elems, _) when elems.Length = argCount ->
                            for el in elems do
                                buildExpr env b el

                            argCount
                        | _ ->
                            failwithf
                                "Emit: external member '%s' expects %d tupled arguments but the argument is not a literal %d-tuple"
                                name
                                argCount
                                argCount
                    elif argCount = 1 then
                        buildExpr env b argExpr
                        1
                    else
                        // argCount = 0: a `unit → ret` method; the lone arg is
                        // `()`, which has no IL value to push.
                        0

            let handle = env.Provider.ExternalMemberRef(key, false, isStatic, zonk memberTy)
            let total = (if isStatic then 0 else 1) + pushedArgs

            if isStatic then
                b.Add(ILInstr.Call(handle, total, 1))
            else
                b.Add(ILInstr.Callvirt(handle, total, 1))

            // A method returning a function value applied further (rare): the
            // result type is the consumed `App` node's type.
            let resultTy =
                match argList with
                | ValueSome(_, ty) -> ty
                | ValueNone -> typeOfExpr head

            foldInvoke env b resultTy rest

        | _ ->
            // The head is itself a function value (a closure local or a
            // partially applied result): emit it, then `Invoke` each arg.
            buildExpr env b head
            foldInvoke env b (typeOfExpr head) spineArgs

    /// Lower a `TExpr.Format` to the `Vesper.Formatter` write-through handler: a
    /// ref-struct local constructed in place, then each segment folded
    /// left-to-right (`AppendLiteral` for a literal run, `AppendFormatted<T>`
    /// for a hole — its arg evaluated *here*, at its position), then a trailing
    /// newline (printfn-style sinks) and flush, or `ToStringAndClear` for the
    /// string sink. The node yields a value: the `unit` (null) of the writing
    /// sinks, or the result string of `sprintf`. Not a `CallRecipe` — the recipe
    /// model can't interleave literals/args around a ref-struct local + sink.
    and private buildFormat (env: EmitEnv) (b: IlBuilder) (sink: FormatSink) (segments: EqArray<FormatSeg>) : unit =
        let fh = env.Provider.FormatHandles()
        let slot = b.Local fh.HandlerLocal

        // Capacity hints for the ctor; the handler grows past them as needed, so
        // they need not be exact.
        let mutable litLen = 0
        let mutable holeCount = 0

        for seg in segments do
            match seg with
            | FormatSeg.Lit s -> litLen <- litLen + s.Length
            | FormatSeg.Hole _ -> holeCount <- holeCount + 1

        // Construct in place: `ldloca h; ldc litLen; ldc holeCount; <sink?>; call .ctor`.
        b.Add(ILInstr.Ldloca slot)
        b.Add(ILInstr.LdcI4 litLen)
        b.Add(ILInstr.LdcI4 holeCount)

        match sink with
        | FormatSink.ToString -> b.Add(ILInstr.Call(fh.CtorString, 3, 0))
        | FormatSink.ToStdOut _ ->
            b.Add(ILInstr.Call(fh.ConsoleOut, 0, 1))
            b.Add(ILInstr.Call(fh.CtorWriter, 4, 0))
        | FormatSink.ToStdErr _ ->
            b.Add(ILInstr.Call(fh.ConsoleError, 0, 1))
            b.Add(ILInstr.Call(fh.CtorWriter, 4, 0))
        | FormatSink.ToWriter w ->
            buildExpr env b w
            b.Add(ILInstr.Call(fh.CtorWriter, 4, 0))
        | FormatSink.ToBuilder _ -> failwith "Emit: bprintf (ToBuilder) is not yet supported"

        for seg in segments do
            match seg with
            | FormatSeg.Lit s ->
                b.Add(ILInstr.Ldloca slot)
                b.Add(ILInstr.Ldstr(env.Ctx.UserString s))
                b.Add(ILInstr.Call(fh.AppendLiteral, 2, 0))
            | FormatSeg.Hole(hole, arg) ->
                match hole.Kind with
                | PrintfSpec.HoleKind.Formatted ->
                    b.Add(ILInstr.Ldloca slot)
                    buildExpr env b arg

                    // Push optional args in the C# parameter order: alignment, then format.
                    match hole.Alignment with
                    | Some a -> b.Add(ILInstr.LdcI4 a)
                    | None -> ()

                    match hole.Format with
                    | Some f -> b.Add(ILInstr.Ldstr(env.Ctx.UserString f))
                    | None -> ()

                    let handle = fh.AppendFormatted(hole.Ty, hole.Alignment.IsSome, hole.Format.IsSome)

                    let argc =
                        2
                        + (if hole.Alignment.IsSome then 1 else 0)
                        + (if hole.Format.IsSome then 1 else 0)

                    b.Add(ILInstr.Call(handle, argc, 0))

                | PrintfSpec.HoleKind.BoolText
                | PrintfSpec.HoleKind.Octal
                | PrintfSpec.HoleKind.Unsigned ->
                    // A dedicated handler member `(value, int alignment)` — no
                    // .NET format string. The alignment is always pushed (0 ⇒ no
                    // padding); `%u`'s `int`→`uint` is a free CLI-stack
                    // reinterpret, so the arg is emitted unchanged.
                    let handle =
                        match hole.Kind with
                        | PrintfSpec.HoleKind.BoolText -> fh.AppendBool
                        | PrintfSpec.HoleKind.Octal -> fh.AppendOctal
                        | _ -> fh.AppendUnsigned

                    b.Add(ILInstr.Ldloca slot)
                    buildExpr env b arg
                    b.Add(ILInstr.LdcI4(defaultArg hole.Alignment 0))
                    b.Add(ILInstr.Call(handle, 3, 0))

                | PrintfSpec.HoleKind.ZeroPaddedFloat ->
                    // `AppendZeroPaddedFloat(value, "F<prec>", width)` — the
                    // `"F<prec>"` body rides in `Format`, the field width in
                    // `Alignment` (both guaranteed present by `tryHoleFormat`).
                    let fmt =
                        match hole.Format with
                        | Some f -> f
                        | None -> failwith "Emit: ZeroPaddedFloat hole missing its format string"

                    let width =
                        match hole.Alignment with
                        | Some w -> w
                        | None -> failwith "Emit: ZeroPaddedFloat hole missing its width"

                    b.Add(ILInstr.Ldloca slot)
                    buildExpr env b arg
                    b.Add(ILInstr.Ldstr(env.Ctx.UserString fmt))
                    b.Add(ILInstr.LdcI4 width)
                    b.Add(ILInstr.Call(fh.AppendZeroPaddedFloat, 4, 0))

        match sink with
        | FormatSink.ToString ->
            // Leaves the built string on the stack (the `sprintf` result).
            b.Add(ILInstr.Ldloca slot)
            b.Add(ILInstr.Call(fh.ToStringAndClear, 1, 1))
        | FormatSink.ToStdOut nl
        | FormatSink.ToStdErr nl ->
            if nl then
                b.Add(ILInstr.Ldloca slot)
                b.Add(ILInstr.Ldstr(env.Ctx.UserString "\n"))
                b.Add(ILInstr.Call(fh.AppendLiteral, 2, 0))

            b.Add(ILInstr.Ldloca slot)
            b.Add(ILInstr.Call(fh.Flush, 1, 0))
            b.Add ILInstr.Ldnull // unit value
        | FormatSink.ToWriter _ ->
            b.Add(ILInstr.Ldloca slot)
            b.Add(ILInstr.Call(fh.Flush, 1, 0))
            b.Add ILInstr.Ldnull // unit value
        | FormatSink.ToBuilder _ -> failwith "Emit: bprintf (ToBuilder) is not yet supported"

    /// Apply each remaining argument to the function value on the stack via
    /// `FSharpFunc.Invoke`, threading the running function type.
    and private foldInvoke (env: EmitEnv) (b: IlBuilder) (funcTy0: SemType) (args: (TExpr * SemType) list) : unit =
        let mutable funcTy = funcTy0

        for (arg, resTy) in args do
            match env.Provider.TryEmitInvoke funcTy with
            | ValueSome recipe ->
                buildExpr env b arg
                b.Add(ILInstr.Recipe recipe)
                funcTy <- resTy
            | ValueNone -> failwithf "Emit: cannot apply argument to value of type %A" funcTy

    /// Apply a curried FSharp.Core `FSharpFunc` value (the cold printf printer)
    /// argument by argument via `FSharpFunc::Invoke` — the FSharpFunc twin of
    /// `foldInvoke` (R1; retargeted with the printf engine, handoff §R9).
    and private foldInvokeFSharpFunc
        (env: EmitEnv)
        (b: IlBuilder)
        (funcTy0: SemType)
        (args: (TExpr * SemType) list)
        : unit =
        let mutable funcTy = funcTy0

        for (arg, resTy) in args do
            match env.Provider.TryEmitFSharpFuncInvoke funcTy with
            | ValueSome recipe ->
                buildExpr env b arg
                b.Add(ILInstr.Recipe recipe)
                funcTy <- resTy
            | ValueNone -> failwithf "Emit: cannot apply argument to FSharpFunc value of type %A" funcTy

    /// Emit an expression as a statement: evaluate it and discard any value.
    let buildStatement (env: EmitEnv) (b: IlBuilder) (e: TExpr) : unit =
        buildExpr env b e

        while b.Depth > 0 do
            b.Add ILInstr.Pop
