namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaborateNominals
open XParsec.FSharp.SemanticAnalysis.ElaborateCalls
open XParsec.FSharp.SemanticAnalysis.ElaborateExprArgs

// Element and member *access* lowering for the Elaborate pass. The get/set pairs
// (`get_Item`/`set_Item`, `GetIndex`/`SetIndex`, `op_Dynamic`/
// `op_DynamicAssignment`) mirror each other, which is why they live together.

module internal ElaborateAccess =

    /// The project-local nominal declaring `accessorName`, the `Decl` of the method call an
    /// accessor use lowers to.
    let private tryAccessorDecl (ctx: PassContext) (objArg: TExpr) (accessorName: string) : TypeKey voption =
        match Unification.zonk ctx.Store (TastWalk.exprTy objArg) with
        | TyNominal(nominalKey, _) ->
            match tryNominalMemberByKey ctx nominalKey accessorName with
            | ValueSome(declKey, m) when not m.IsStatic -> ValueSome declKey
            | _ -> ValueNone
        | _ -> ValueNone

    /// A call to the EXTERNAL accessor recorded in `ExternalAccess`. A byref-returning one
    /// (`Span<char>.get_Item : T&`) hands back a managed pointer, so the call is followed by an
    /// `ldobj <elem>` deref; a by-value one (`string.get_Item : char`) is the plain call.
    let private mkExternalAccessorCall
        (ctx: PassContext)
        (info: ResolvedExternalMember)
        (objArg: TExpr)
        (arg: TExpr)
        (argTy: SemType)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let retIsByref =
            match Unification.zonk ctx.Store info.Signature with
            | TyFun(_, TyByref _) -> true
            | _ -> false

        // The CALL's own return; `ty` is what the ACCESS yields.
        let callTy =
            if retIsByref then
                TyConst(RuntimeNames.byrefKey, EqArray.singleton ty)
            else
                ty

        let accessor =
            TExpr.ExternalMember(
                ValueSome objArg,
                info.Key,
                SymbolKeyOps.intrinsicName info.Key,
                MemberStorage.Method,
                TyFun(argTy, callTy),
                tok
            )

        let call = TExpr.App(accessor, arg, callTy, tok)

        if retIsByref then
            TExpr.ILIntrinsic("ldobj", ValueSome ty, EqArray.singleton call, ty, tok)
        else
            call

    /// `x.P <- v` through a declared `set_P` accessor method.
    let private trySetterCall
        (translateExpr: TranslateExpr)
        (ctx: PassContext)
        (key: NodeKey)
        (objArg: TExpr)
        (name: string)
        (right: Expr<SyntaxToken>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr voption =
        let setName = AccessorNames.setterName name

        match tryAccessorDecl ctx objArg setName with
        | ValueSome declKey ->
            let args = EqArray.singleton (translateExpr ctx right)
            ValueSome(mkMethodCall ctx key objArg declKey setName args ty tok)
        | ValueNone -> ValueNone

    /// The object argument of a folded `a.b.P <- v` chain: the anchor binding, then a field
    /// read per intermediate segment. The assigned slot is the last segment, left to the caller.
    let private translateChainPrefix
        (ctx: PassContext)
        (li: LongIdent<SyntaxToken>)
        (access: NodeSite)
        (tok: SyntaxToken)
        : TExpr =
        let segments = li.Idents
        let lastIdx = segments.Length - 1

        let anchorIdent = segments.[0]
        let anchorKey = NodeKey.ofToken anchorIdent NodeKind.ExprIdent
        let anchorBinding = ctx.Bindings.Binding.TryGetValue anchorKey

        let anchorTy =
            match anchorBinding with
            | ValueSome rb -> typeOfKey ctx rb.BindingSite
            | ValueNone -> typeOfKey ctx access.Key

        let anchorExpr =
            match anchorBinding with
            | ValueSome rb -> TExpr.Var(rb.BindingSite, anchorTy, tok)
            | ValueNone -> TExpr.External(ctx.NameOf anchorIdent, ValueNone, anchorTy, tok)

        let mutable curr = anchorExpr
        let mutable currTy = anchorTy

        for i = 1 to lastIdx - 1 do
            let segName = ctx.NameOf segments.[i]

            let stepTy =
                match recoverFieldStepTy ctx currTy segName with
                | ValueSome t -> t
                | ValueNone -> currTy

            curr <- fieldStep ctx curr currTy segName stepTy tok
            currTy <- stepTy

        curr

    /// `r.X <- v` folds to a `set_X` call or a FieldSet; everything else to Assignment.
    let translateAssignment
        (translateExpr: TranslateExpr)
        (ctx: PassContext)
        (key: NodeKey)
        (left: Expr<SyntaxToken>)
        (right: Expr<SyntaxToken>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let lhs = AssignTarget.ofExpr ctx left

        match lhs.Target with
        | AssignTarget.Slot(objArg, slotTok) ->
            let slotName = ctx.NameOf slotTok

            let objArgExpr =
                match objArg with
                | AssignObjArg.Expr r -> translateExpr ctx r
                | AssignObjArg.ChainPrefix li -> translateChainPrefix ctx li lhs.Access tok

            match trySetterCall translateExpr ctx key objArgExpr slotName right ty tok with
            | ValueSome call -> call
            | ValueNone -> TExpr.FieldSet(objArgExpr, slotName, translateExpr ctx right, ty, tok)
        | AssignTarget.Indexed(arrE, idxE) ->
            let objArg = translateExpr ctx arrE

            match tryAccessorDecl ctx objArg AccessorNames.itemSetter with
            | ValueSome declKey ->
                let args = EqArray.ofList [ translateExpr ctx idxE; translateExpr ctx right ]
                mkMethodCall ctx key objArg declKey AccessorNames.itemSetter args ty tok
            | ValueNone ->

                // `arr.[i] <- v` through the EXTERNAL `set_Item` recorded in `ExternalAccess`
                // under this assignment's key. Two .NET parameters, so ONE tupled argument.
                match ctx.Resolution.ExternalAccess.TryGetValue key with
                | ValueSome info ->
                    let idxArg = translateExpr ctx idxE
                    let valArg = translateExpr ctx right

                    let argsTy =
                        TyTuple(EqArray.ofList [ TastWalk.exprTy idxArg; TastWalk.exprTy valArg ])

                    let args = TExpr.Tuple(EqArray.ofList [ idxArg; valArg ], argsTy, tok)
                    mkExternalAccessorCall ctx info objArg args argsTy ty tok
                // An index-signature object argument has no `set_Item`: the `$0[$1] = $2` bracket IS
                // its accessor, emitted as a curried `External` whose type is rebuilt from the
                // operands (`ty` is the `unit` result).
                | ValueNone ->
                    let arrTy = typeOfKey ctx (CstKeys.ofExpr arrE)
                    let idxTy = typeOfKey ctx (CstKeys.ofExpr idxE)
                    let valTy = typeOfKey ctx (CstKeys.ofExpr right)
                    let valuePartial = TyFun(valTy, ty)
                    let idxPartial = TyFun(idxTy, valuePartial)

                    // Unification (`inferAssignment`) stamped the resolved `SetIndex` identity under
                    // this `Assignment` key; carry it so the `$0[$1] = $2` body splices by KEY.
                    let setKey = ctx.Resolution.IntrinsicKey.TryGetValue key
                    let setExpr = TExpr.External("SetIndex", setKey, TyFun(arrTy, idxPartial), tok)
                    let app1 = TExpr.App(setExpr, objArg, idxPartial, tok)
                    let app2 = TExpr.App(app1, translateExpr ctx idxE, valuePartial, tok)
                    TExpr.App(app2, translateExpr ctx right, ty, tok)
        // `x?name <- v` → `(?<-) x "name" v` → the `op_DynamicAssignment` body
        // `$0[$1] = $2` splices to `x["name"] = v`. The name is a compile-time
        // string literal (the ident text), NOT a value reference.
        | AssignTarget.Dynamic(r, idTok) ->
            let objArgTy = typeOfKey ctx (CstKeys.ofExpr r)
            let valTy = typeOfKey ctx (CstKeys.ofExpr right)

            let nameLit =
                TExpr.Const(TConstValue.String(ctx.NameOf idTok), ctx.Intrinsics.String, tok)

            let valuePartial = TyFun(valTy, ty)
            let namePartial = TyFun(ctx.Intrinsics.String, valuePartial)

            let opKey = ctx.Resolution.IntrinsicKey.TryGetValue key

            let opExpr =
                TExpr.External(OperatorData.OpDynamicAssignment, opKey, TyFun(objArgTy, namePartial), tok)

            let app1 = TExpr.App(opExpr, translateExpr ctx r, namePartial, tok)
            let app2 = TExpr.App(app1, nameLit, valuePartial, tok)
            TExpr.App(app2, translateExpr ctx right, ty, tok)
        | AssignTarget.Plain -> TExpr.Assignment(translateExpr ctx left, translateExpr ctx right, ty, tok)

    /// Single-segment `r.X` read on a *project-local* object argument (the
    /// external forms peel off in the dispatcher first).
    let translateDotLookup
        (translateExpr: TranslateExpr)
        (ctx: PassContext)
        (key: NodeKey)
        (r: Expr<SyntaxToken>)
        (memberName: string)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let rTy = Unification.zonk ctx.Store (typeOfKey ctx (CstKeys.ofExpr r))
        let objArg = translateExpr ctx r

        // A record exposes BOTH fields and instance-member properties by dot-access, so the
        // decision is made here: a member name to `PropertyGet`, a field name to `FieldGet`.
        match rTy with
        | TyRecord(recKey, _) ->
            match tryNominalMemberByKey ctx recKey memberName with
            | ValueSome(declKey, _) ->
                let key = LocalSymbolKey.ofProperty declKey memberName
                TExpr.PropertyGet(objArg, key, viaOfObjArg ctx objArg, ty, tok)
            | ValueNone -> TExpr.FieldGet(objArg, memberName, ty, tok)
        // A class/union object argument's `.X` is always a member, so a `PropertyGet` (a
        // method-as-value keeps the shape; codegen eta-expands).
        | TyNominal(nominalKey, _) ->
            let key = LocalSymbolKey.ofProperty nominalKey memberName

            TExpr.PropertyGet(objArg, key, viaOfObjArg ctx objArg, ty, tok)
        | _ -> TExpr.FieldGet(objArg, memberName, ty, tok)

    /// `x?name` → `(?) x "name"` → the `op_Dynamic` body `$0[$1]` splices to
    /// `x["name"]`. The name is a compile-time string literal (the ident text),
    /// NOT a value reference.
    let translateDynamicLookup
        (translateExpr: TranslateExpr)
        (ctx: PassContext)
        (key: NodeKey)
        (r: Expr<SyntaxToken>)
        (idTok: SyntaxToken)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let objArgTy = typeOfKey ctx (CstKeys.ofExpr r)

        let nameLit =
            TExpr.Const(TConstValue.String(ctx.NameOf idTok), ctx.Intrinsics.String, tok)

        let partialTy = TyFun(ctx.Intrinsics.String, ty)
        let opKey = ctx.Resolution.IntrinsicKey.TryGetValue key

        let opExpr =
            TExpr.External(OperatorData.OpDynamic, opKey, TyFun(objArgTy, partialTy), tok)

        let app1 = TExpr.App(opExpr, translateExpr ctx r, partialTy, tok)
        TExpr.App(app1, nameLit, ty, tok)

    /// `x.[i]` through a declared `get_Item`, the object argument's own or an external one;
    /// failing both, through the `GetIndex` intrinsic, a curried `External` call whose type
    /// is rebuilt from the operands.
    let translateIndexedLookup
        (translateExpr: TranslateExpr)
        (ctx: PassContext)
        (key: NodeKey)
        (r: Expr<SyntaxToken>)
        (idx: Expr<SyntaxToken>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let objArg = translateExpr ctx r

        match tryAccessorDecl ctx objArg AccessorNames.itemGetter with
        | ValueSome declKey ->
            let args = EqArray.singleton (translateExpr ctx idx)
            mkMethodCall ctx key objArg declKey AccessorNames.itemGetter args ty tok
        | ValueNone ->
            match ctx.Resolution.ExternalAccess.TryGetValue key with
            | ValueSome info ->
                let idxTy = typeOfKey ctx (CstKeys.ofExpr idx)
                mkExternalAccessorCall ctx info objArg (translateExpr ctx idx) idxTy ty tok
            // An index-signature object argument (an external type carrying `{ [k: K]: V }`)
            // has no `get_Item`: the `$0[$1]` bracket IS its accessor. Every other object
            // argument reaching here failed to resolve one, which inference has reported.
            | ValueNone ->
                let arrTy = typeOfKey ctx (CstKeys.ofExpr r)
                let idxTy = typeOfKey ctx (CstKeys.ofExpr idx)
                let partialTy = TyFun(idxTy, ty)
                let getTy = TyFun(arrTy, partialTy)

                // Unification (`inferIndexedLookup`) stamped the resolved `GetIndex` identity
                // under this `IndexedLookup` key; carry it so the `$0[$1]` body splices by KEY.
                let getKey = ctx.Resolution.IntrinsicKey.TryGetValue key
                let getExpr = TExpr.External("GetIndex", getKey, getTy, tok)
                let app1 = TExpr.App(getExpr, objArg, partialTy, tok)
                TExpr.App(app1, translateExpr ctx idx, ty, tok)
