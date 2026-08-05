namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaborateNominals
open XParsec.FSharp.SemanticAnalysis.ElaborateObjArgs

// Call / construction / field-access node builders for the Elaborate pass.
//
// Every TAST node that flows arguments into possibly-`obj` parameter slots is
// built through one of the `mk*` smart constructors, so the implicit value→`obj`
// upcast (`wrapObjArgsEq`) can never be forgotten by a `translateExpr` arm — the
// single home of the box decision, in the TAST layer where the `Upcast` node
// lives. Each picks the parameter model appropriate to its node kind; the arms
// supply only the resolved callee and the peeled (un-wrapped) arguments.

module internal ElaborateCalls =

    // --- Tuple-VALUED argument at a multi-parameter member ------------------
    //
    // A member is TUPLED — one argument whatever its parameter count — and overload
    // resolution reads that argument's TYPE, so a tuple VALUE selects a 2-parameter
    // member exactly as a literal `(3, 4)` does. Its elements are then not expressions,
    // which is what a spliced `member inline` body needs one of per parameter.

    /// The `let`s an opened call now sits inside (outermost first), and its rewritten
    /// head + argument.
    [<NoEquality; NoComparison>]
    type OpenedTupledCall =
        {
            Binds: (TPat * TExpr) list
            Head: TExpr
            Arg: TExpr
        }

    /// `w.M t` ⟶ `let r = w in let (a, b) = t in r.M(a, b)`. `t` binds once, so it is
    /// evaluated once; the receiver binds FIRST, because an instance member evaluates it
    /// before its argument and the argument's `let` would otherwise hoist above it.
    ///
    /// `ValueNone` leaves the call alone: a non-method head, a value member (empty
    /// `ArgSig`), arity 1, an argument the source already opened, or one whose type is
    /// not the tuple its arity needs.
    let openTupledMemberArg (ctx: PassContext) (head: TExpr) (arg: TExpr) : OpenedTupledCall voption =
        match head with
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

                let recvBind, head' =
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
                    | ValueNone -> [], head

                ValueSome
                    {
                        Binds = recvBind @ [ tuplePat, arg ]
                        Head = head'
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

    /// Dispatch discriminator for an instance member access. A `base.M(...)` / `base.X` receiver translates to a
    /// `TExpr.Var` whose binding site is some class's `BaseKey`; that must
    /// dispatch non-virtually so an `override` calling `base.M()` doesn't recurse
    /// into itself. The check is O(classes) per access — the gap doc accepts this
    /// for v1 (most files declare a handful of classes); a reverse index is a
    /// later optimisation.
    let viaOfReceiver (ctx: PassContext) (receiver: TExpr) : CallVia<SemType> =
        match receiver with
        | TExpr.Var(bindingSite, _, _) ->
            let mutable isBase = false

            for kv in ctx.Types.Class do
                // A `Var` names its binder in the reference domain, so the class's `base`
                // binder is compared there.
                if
                    not isBase
                    && kv.Value.BaseType.IsSome
                    && BinderKey.identity kv.Value.BaseKey = bindingSite
                then
                    isBase <- true

            if isBase then CallVia.Base else CallVia.Self
        | _ -> CallVia.Self

    /// `New` for a class construction. Reads the front-end-chosen external `.ctor`'s
    /// `SymbolKey.MemberKey` from `Resolution.ExternalCtor`, keyed by the construction
    /// node's `key`, and records it on the node so codegen selects that exact `.ctor` by
    /// identity. `ValueNone` (absent) ⇒ a project-local / scratch class codegen resolves
    /// by result-type key + arity. Centralising the read here keeps every construction
    /// syntax's identity handshake in one place.
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

    /// Instance `MethodCall` resolved to `declKey.memberName`, with the `CallVia`
    /// derived from the receiver. `callKey` is the call node's `NodeKey`: for an OVERLOADED
    /// name, Unification recorded the chosen overload's TOTAL frozen `MemberKey` there
    /// (`Resolution.LocalMemberCall`), read back verbatim so Freeze resolves the identical
    /// member by identity — no second name-based pick. A non-overloaded name has no entry
    /// and mints the member's TOTAL key from the resolved member itself
    /// (`LocalMemberKeys.totalMemberKey`), which is already unique.
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
        // The overloaded-instance handshake first: inference stamped the chosen overload's
        // frozen key here. Only the non-overloaded fallback mints from the resolved member.
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
            // Post-inference the resolved member is committed, so a miss is an internal
            // invariant break, not mis-typed source — degrade to a diagnostic, never a crash.
            ctx.Report(
                tok,
                Kind.Internal(InternalBreak.MemberNotResolvable("mkMethodCall", string declKey, memberName))
            )

            TExpr.Null(ty, tok)

    /// Instance `MethodCall` dispatched through an *interface* the receiver's typar is
    /// coerced to (`'T :> IFace`). `ifaceKey` is the interface's declaring `SymbolKey`
    /// (the member key's `decl`); `CallVia.Interface` tells codegen to emit
    /// `constrained. <receiver-typar> callvirt`. The parameter model for the obj-upcast
    /// comes from the interface's own member (the abstract slot).
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
        // A local interface resolves via the local-registry arm; an external-coerced one
        // (`'T :> IFace` where `IFace` is an imported contract) via the provider arm — the
        // shared minter routes both.
        // The interface's own type args are the declaring-type args; operand element types
        // discriminate a same-arity overloaded abstract slot.
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
        // A folded / type-qualified static head names a non-generic declaring type (generics
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

    /// Recover segment `segName`'s declared type from receiver type `recvTy` — a
    /// record field, or a union / class instance-member return type — instantiated
    /// at the receiver's type arguments. `ValueNone` when the receiver isn't a
    /// known nominal or has no such member (the caller picks a fallback type).
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
                    // A record's chain segment is a field OR an instance member
                    // (property) — check fields first, then members, mirroring the
                    // class arm below (a record has no inheritance, so no chain walk).
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
                    // A `this.x` chain segment may be an explicit `val` instance
                    // field or a primary-ctor parameter (both emitted as fields),
                    // not an instance member — `memberTy` alone misses it, and the
                    // caller would then fall back to the chain's *final* type,
                    // mis-typing the receiver (e.g. `this.stack.IsEmpty` typing
                    // `this.stack` as `bool`). Check fields first, then members.
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
    /// `FieldGet` otherwise. `recvTy` is the receiver's (un-zonked) type; `stepTy`
    /// is the segment's already-resolved result type.
    /// `chainKey` is the enclosing `LongIdent` chain's NodeKey — the identity
    /// Unification stamped the resolved `GetArrayLength` intrinsic under for the
    /// `arr.Length` array-length arm below (so the `ldlen` body splices by KEY).
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
        // member-name segment is a `PropertyGet`, a non-member (a record field, a
        // union tag/case field) a `FieldGet`. The two kinds share ONE arm through
        // the member-key read (`tryNominalMemberByKey`); only which registry it
        // consults differs, and that is hidden inside the read.
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
                // An *inherited* member (declared on a base class, e.g. `node.Key`
                // where `Key` is on the parent `SetTree`): upcast the receiver to the
                // declaring ancestor so codegen's receiver-keyed
                // `resolveInstanceMember` resolves `get_<seg>` on the class that
                // emits it (a reference-type upcast is a codegen no-op). Reuses
                // inference's `inherit`-chain walk (`tryClassChainMemberDecl`) so this
                // read isn't a second chain walk that must stay in sync. Falls through
                // to `FieldGet` only when no ancestor declares it — a genuine ctor-param
                // / `val` field access. (The own-class case is the `isMember` arm
                // above, so the walk only ever resolves a strict ancestor here.)
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
        // A flat nominal — union or record. The `TyClass` arm above forks for its
        // inheritance chain; union and record share `flatNominalStep` (a member is a
        // `PropertyGet`, a record field / union case field a `FieldGet`), so they land
        // here through `TyNominal` (class is already handled above, so this only ever
        // catches union/record).
        | TyNominal(nominalKey, _) -> flatNominalStep nominalKey
        // `arr.Length` on an intrinsic rank-1 array desugars to the core
        // `GetArrayLength` inline function (the `ldlen` mnemonic lives in
        // `ops-platform.clr.fs`, spliced here by `InlineExpansion`). `array.Length`
        // parses as a local-headed LongIdent field chain (not `DotLookup`), so this
        // `fieldStep` arm is the one that fires; mirrors the `DotLookup` array guard.
        | TyArray _ when segName = "Length" ->
            let lenKey = ctx.Resolution.IntrinsicKey.TryGetValue chainKey
            TExpr.App(TExpr.External("GetArrayLength", lenKey, TyFun(recvTy, stepTy), tok), receiver, stepTy, tok)
        | _ -> TExpr.FieldGet(receiver, segName, stepTy, tok)
