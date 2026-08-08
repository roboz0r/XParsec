namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaborateNominals
open XParsec.FSharp.SemanticAnalysis.ElaborateObjArgs

// Call / construction / field-access node builders for the Elaborate pass. Each
// `mk*` applies the implicit value→`obj` upcast for its node kind's parameter
// model, so an arm supplies only the resolved callee and the peeled arguments.

module internal ElaborateCalls =

    // A member is TUPLED — one argument whatever its parameter count — so a tuple
    // VALUE selects a 2-parameter member exactly as the literal `(3, 4)` does. But its
    // elements are not expressions, and a spliced `member inline` body needs one each.

    /// The `let`s an opened call now sits inside (outermost first), and its rewritten
    /// applied function + argument.
    [<NoEquality; NoComparison>]
    type OpenedTupledCall =
        {
            Binds: (TPat * TExpr) list
            Fn: TExpr
            Arg: TExpr
        }

    /// `w.M t` ⟶ `let r = w in let (a, b) = t in r.M(a, b)`. `t` binds once, so it is
    /// evaluated once; the receiver binds FIRST, since an instance member evaluates it
    /// before its argument and the argument's `let` would otherwise hoist above it.
    let openTupledMemberArg (ctx: PassContext) (fn: TExpr) (arg: TExpr) : OpenedTupledCall voption =
        match fn with
        | TExpr.ExternalMember(receiver, key, name, MemberStorage.Method, memberTy, memberTok) ->
            let arity = SymbolKeyOps.memberArity (sprintf "Elaborate: member '%s'" name) key

            let argTy = Unification.zonk ctx.Store (TastWalk.exprTy arg)

            match arg, argTy with
            | TExpr.Tuple _, _ -> ValueNone
            | _, TyTuple elemTys when arity >= 2 && elemTys.Length = arity ->
                let argTok = TastWalk.exprTok arg

                let elems = [ for elemTy in EqArray.toList elemTys -> ctx.NewSynthBinder(), elemTy ]

                let tuplePat =
                    TPat.Tuple(
                        EqArray.ofSeq (seq { for (k, ty) in elems -> TPat.NamedSimple(k, ty, argTok) }),
                        argTy,
                        argTok
                    )

                let recvBind, fn' =
                    match receiver with
                    | ValueSome r ->
                        let rKey = ctx.NewSynthBinder()
                        let rTy = TastWalk.exprTy r

                        [ TPat.NamedSimple(rKey, rTy, memberTok), r ],
                        TExpr.ExternalMember(
                            ValueSome(TExpr.Var(rKey, rTy, memberTok)),
                            key,
                            name,
                            MemberStorage.Method,
                            memberTy,
                            memberTok
                        )
                    | ValueNone -> [], fn

                ValueSome
                    {
                        Binds = recvBind @ [ tuplePat, arg ]
                        Fn = fn'
                        Arg =
                            TExpr.Tuple(
                                EqArray.ofSeq (seq { for (k, ty) in elems -> TExpr.Var(k, ty, argTok) }),
                                argTy,
                                argTok
                            )
                    }
            | _ -> ValueNone
        | _ -> ValueNone

    /// Wrap a call in the `let`s `openTupledMemberArg` produced, outermost first.
    let wrapOpenedBinds (binds: (TPat * TExpr) list) (call: TExpr) : TExpr =
        List.foldBack
            (fun (pat, value) body -> TExpr.Let(pat, value, body, TastWalk.exprTy body, TastWalk.patTok pat))
            binds
            call

    /// A `base.M(...)` / `base.X` receiver translates to a `TExpr.Var` whose binding
    /// site is some class's `BaseKey`; that must dispatch non-virtually, so an
    /// `override` calling `base.M()` does not recurse into itself.
    let viaOfReceiver (ctx: PassContext) (receiver: TExpr) : CallVia<SemType> =
        match receiver with
        | TExpr.Var(bindingSite, _, _) ->
            let mutable isBase = false

            for kv in ctx.Types.Class do
                if
                    not isBase
                    && kv.Value.BaseType.IsSome
                    && BinderKey.identity kv.Value.BaseKey = bindingSite
                then
                    isBase <- true

            if isBase then CallVia.Base else CallVia.Self
        | _ -> CallVia.Self

    /// `New` for a class construction. The front-end-chosen external `.ctor` key is in
    /// `ExternalCtor` under this node's `key`; recording it lets codegen select that
    /// exact `.ctor`. Absent ⇒ codegen resolves by result-type key + arity.
    let mkNew
        (ctx: PassContext)
        (className: string)
        (key: NodeKey)
        (ty: SemType)
        (args: EqArray<TExpr>)
        (tok: SyntaxToken)
        : TExpr =
        TExpr.New(
            className,
            ctx.Resolution.ExternalCtor.TryGetValue key,
            wrapObjArgsEq ctx.Store (ctorParamTys ctx ty args.Length) args,
            ty,
            tok
        )

    /// Instance `MethodCall` resolved to `declKey.memberName`. For an OVERLOADED name,
    /// Unification recorded the chosen overload's frozen `SymbolKey` in `LocalMemberCall`
    /// under `callKey`; a non-overloaded name has no entry and mints its own.
    let mkMethodCall
        (ctx: PassContext)
        (callKey: NodeKey)
        (receiver: TExpr)
        (declKey: TypeKey)
        (memberName: string)
        (args: EqArray<TExpr>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let key =
            match ctx.Resolution.LocalMemberCall.TryGetValue callKey with
            | ValueSome frozen -> ValueSome frozen
            | ValueNone ->
                // External overload discrimination reads the receiver's declaring-type args
                // and the ground operand element types; a non-ground operand declines to the
                // best-by-arity single inside the minter.
                let operands =
                    LocalMemberKeys.externalOperands
                        ctx.Store
                        (LocalMemberKeys.nominalArgs ctx.Store (TastWalk.exprTy receiver))
                        [ for a in args -> TastWalk.exprTy a ]

                LocalMemberKeys.totalMemberKey ctx declKey memberName operands

        match key with
        | ValueSome key ->
            let argsList = wrapObjArgsEq ctx.Store (memberParamTys ctx declKey memberName) args
            TExpr.MethodCall(receiver, key, viaOfReceiver ctx receiver, argsList, ty, tok)
        | ValueNone ->
            // Post-inference a miss is an internal invariant break, not mis-typed source.
            ctx.Report(
                tok,
                Kind.Internal(InternalBreak.MemberNotResolvable("mkMethodCall", string declKey, memberName))
            )

            TExpr.Null(ty, tok)

    /// Instance `MethodCall` dispatched through an *interface* the receiver's typar is
    /// coerced to (`'T :> IFace`); `CallVia.Interface` tells codegen to emit
    /// `constrained. <receiver-typar> callvirt`.
    let mkInterfaceMethodCall
        (ctx: PassContext)
        (receiver: TExpr)
        (ifaceKey: TypeKey)
        (ifaceArgs: EqArray<SemType>)
        (memberName: string)
        (args: EqArray<TExpr>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        // The interface's own type args are the declaring-type args; operand element
        // types discriminate a same-arity overloaded abstract slot.
        let operands =
            LocalMemberKeys.externalOperands
                ctx.Store
                (EqArray.toArray ifaceArgs)
                [ for a in args -> TastWalk.exprTy a ]

        match LocalMemberKeys.totalMemberKey ctx ifaceKey memberName operands with
        | ValueSome key ->
            let argsList = wrapObjArgsEq ctx.Store (memberParamTys ctx ifaceKey memberName) args
            TExpr.MethodCall(receiver, key, CallVia.Interface ifaceArgs, argsList, ty, tok)
        | ValueNone ->
            ctx.Report(
                tok,
                Kind.Internal(InternalBreak.MemberNotResolvable("mkInterfaceMethodCall", string ifaceKey, memberName))
            )

            TExpr.Null(ty, tok)

    /// `StaticMethodCall` resolved to `declKey.memberName`.
    let mkStaticMethodCall
        (ctx: PassContext)
        (declKey: TypeKey)
        (memberName: string)
        (args: EqArray<TExpr>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        // A folded / type-qualified static call names a non-generic declaring type (generics
        // need `<>`, handled at the receiver), so it carries no declaring-type args; the
        // operand element types alone discriminate a same-arity overload (e.g. an operator).
        let operands =
            LocalMemberKeys.externalOperands ctx.Store [||] [ for a in args -> TastWalk.exprTy a ]

        match LocalMemberKeys.totalMemberKey ctx declKey memberName operands with
        | ValueSome key ->
            TExpr.StaticMethodCall(key, wrapObjArgsEq ctx.Store (memberParamTys ctx declKey memberName) args, ty, tok)
        | ValueNone ->
            ctx.Report(
                tok,
                Kind.Internal(InternalBreak.MemberNotResolvable("mkStaticMethodCall", string declKey, memberName))
            )

            TExpr.Null(ty, tok)

    /// `UnionCons` for case `caseName` of union `ty`.
    let mkUnionCons
        (ctx: PassContext)
        (caseName: string)
        (ty: SemType)
        (args: EqArray<TExpr>)
        (tok: SyntaxToken)
        : TExpr =
        TExpr.UnionCons(caseName, wrapObjArgsEq ctx.Store (unionCaseFieldTys ctx ty caseName) args, ty, tok)

    /// Recover segment `segName`'s declared type from receiver type `recvTy` — a record
    /// field, or a union / class instance-member return type — instantiated at the
    /// receiver's type arguments. `ValueNone` if it is not a known nominal's member.
    let recoverFieldStepTy (ctx: PassContext) (recvTy: SemType) (segName: string) : SemType voption =
        let memberTy (typeParams, args) (members: TypeMemberInfo[]) =
            members
            |> Array.tryPick (fun m ->
                if m.Name = segName && not m.IsStatic then
                    Some(Unification.instantiateMember ctx.Store (typeParams, args) m.Type)
                else
                    None
            )

        let resolved =
            match Unification.zonk ctx.Store recvTy with
            | TyRecord(recKey, args) ->
                match TypeRegistry.tryRecordByKey ctx.Types recKey with
                | ValueSome info ->
                    // A record's chain segment is a field OR an instance-member property
                    // — check fields first (a record has no inheritance to walk).
                    let fieldTy =
                        info.Fields
                        |> Array.tryPick (fun f ->
                            if f.Name = segName then
                                Some(Unification.instantiateMember ctx.Store (info.TypeParams, args) f.Type)
                            else
                                None
                        )

                    match fieldTy with
                    | Some _ -> fieldTy
                    | None -> memberTy (info.TypeParams, args) info.Members
                | ValueNone -> None
            | TyUnion(unionKey, args) ->
                match TypeRegistry.tryUnionByKey ctx.Types unionKey with
                | ValueSome info -> memberTy (info.TypeParams, args) info.Members
                | ValueNone -> None
            | TyClass(clsKey, args) ->
                match TypeRegistry.tryClassByKey ctx.Types clsKey with
                | ValueSome info ->
                    // A `this.x` chain segment may be a `val` field or a primary-ctor
                    // parameter, not a member — members-only would fall back to the
                    // chain's FINAL type, typing `this.stack.IsEmpty`'s receiver `bool`.
                    let fieldTy =
                        Seq.append
                            (info.InstanceFields |> Seq.map (fun f -> f.Name, f.Type))
                            (info.CtorParams |> Seq.map (fun p -> p.Name, p.Type))
                        |> Seq.tryPick (fun (n, t) ->
                            if n = segName then
                                Some(Unification.instantiateMember ctx.Store (info.TypeParams, args) t)
                            else
                                None
                        )

                    match fieldTy with
                    | Some _ -> fieldTy
                    | None -> memberTy (info.TypeParams, args) info.Members
                | ValueNone -> None
            | _ -> None

        match resolved with
        | Some t -> ValueSome(Unification.zonk ctx.Store t)
        | None -> ValueNone

    /// One `receiver.seg` access node: `PropertyGet` for a class / union member,
    /// `FieldGet` otherwise. `chainKey` is the enclosing `LongIdent` chain's key,
    /// under which Unification stamped the `arr.Length` intrinsic read below.
    let fieldStep
        (ctx: PassContext)
        (chainKey: NodeKey)
        (receiver: TExpr)
        (recvTy: SemType)
        (segName: string)
        (stepTy: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let isMember (members: TypeMemberInfo[]) =
            members |> Array.exists (fun m -> m.Name = segName)

        // A flat nominal (union or record — neither has an inheritance chain): a
        // member name is a `PropertyGet`, a record / union case field a `FieldGet`.
        let flatNominalStep (typeKey: TypeKey) : TExpr =
            match tryNominalMemberByKey ctx typeKey segName with
            | ValueSome(declKey, _) ->
                let key = LocalSymbolKey.ofProperty declKey segName
                TExpr.PropertyGet(receiver, key, viaOfReceiver ctx receiver, stepTy, tok)
            | ValueNone -> TExpr.FieldGet(receiver, segName, stepTy, tok)

        match Unification.zonk ctx.Store recvTy with
        | TyClass(clsKey, args) ->
            match TypeRegistry.tryClassByKey ctx.Types clsKey with
            | ValueSome info when isMember info.Members ->
                let key = LocalSymbolKey.ofProperty info.TypeKey segName
                TExpr.PropertyGet(receiver, key, viaOfReceiver ctx receiver, stepTy, tok)
            | _ ->
                // An *inherited* member (`node.Key` where `Key` is on the parent
                // `SetTree`): upcast the receiver to the declaring ancestor so
                // codegen resolves `get_<seg>` on the class that emits it.
                match Unification.tryClassChainMemberDecl ctx clsKey args segName with
                | ValueSome cm ->
                    let key =
                        LocalSymbolKey.ofProperty (nominalDeclKey ctx.Store cm.DeclaringTy) segName

                    TExpr.PropertyGet(
                        TExpr.Upcast(receiver, cm.DeclaringTy, TastWalk.exprTok receiver),
                        key,
                        viaOfReceiver ctx receiver,
                        stepTy,
                        tok
                    )
                | ValueNone -> TExpr.FieldGet(receiver, segName, stepTy, tok)
        // `TyClass` matched above, so this catches only union and record.
        | TyNominal(nominalKey, _) -> flatNominalStep nominalKey
        // `arr.Length` on a rank-1 array desugars to the core `GetArrayLength` inline
        // function (`ldlen`). `array.Length` parses as a LongIdent field chain anchored on
        // a local, not a `DotLookup`, so this arm is the one that fires.
        | TyArray _ when segName = "Length" ->
            let lenKey = ctx.Resolution.IntrinsicKey.TryGetValue chainKey
            TExpr.App(TExpr.External("GetArrayLength", lenKey, TyFun(recvTy, stepTy), tok), receiver, stepTy, tok)
        | _ -> TExpr.FieldGet(receiver, segName, stepTy, tok)
