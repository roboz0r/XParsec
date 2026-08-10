namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaborateNominals
open XParsec.FSharp.SemanticAnalysis.ElaborateCalls
open XParsec.FSharp.SemanticAnalysis.ElaborateExprArgs

// Element and member *access* lowering for the Elaborate pass. The get/set pairs
// (`GetArray`/`SetArray`, `GetIndex`/`SetIndex`, `op_Dynamic`/
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

        // The chain's NodeKey, required by `fieldStep` for its array-length lookup. A
        // read-only `.Length` can never be an assigned segment, so here it is only ever
        // passed through.
        let liKey =
            NodeKey.ofToken (CstKeys.firstTokenOfLongIdent li) NodeKind.ExprLongIdent

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

            curr <- fieldStep ctx liKey curr currTy segName stepTy tok
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
            // `arr.[i] <- v` desugars to the core `SetArray` inline function (`stelem`),
            // the write mirror of the `IndexedLookup` read path. Emit a curried `External`
            // whose type is rebuilt from the operands; `ty` is the `unit` result.
            | ValueNone ->
                let arrTy = typeOfKey ctx (CstKeys.ofExpr arrE)
                let idxTy = typeOfKey ctx (CstKeys.ofExpr idxE)
                let valTy = typeOfKey ctx (CstKeys.ofExpr right)
                let valuePartial = TyFun(valTy, ty)
                let idxPartial = TyFun(idxTy, valuePartial)

                // An index-signature object argument writes through `SetIndex` (the `$0[$1] = $2`
                // bracket), every other through `SetArray` (`stelem`), the same
                // classification the read branch makes.
                let setName =
                    match Unification.zonk ctx.Store arrTy with
                    | TyClass(clsKey, _) when
                        not (ctx.Provider.TryLookupIndexSignature(SymbolKey.Type clsKey) |> List.isEmpty)
                        ->
                        "SetIndex"
                    | _ -> "SetArray"

                // Unification (`inferAssignment`) stamped the resolved `SetArray`/
                // `SetIndex` identity under this `Assignment` key; carry it so the
                // `stelem` / `$0[$1] = $2` body splices by KEY.
                let setKey = ctx.Resolution.IntrinsicKey.TryGetValue key
                let setExpr = TExpr.External(setName, setKey, TyFun(arrTy, idxPartial), tok)
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
        // `(expr).Length` on a rank-1 array desugars to the core `GetArrayLength`
        // inline function (`ldlen`); the LongIdent-chain form mirrors this.
        | TyArray _ when memberName = "Length" ->
            let lenKey = ctx.Resolution.IntrinsicKey.TryGetValue key
            TExpr.App(TExpr.External("GetArrayLength", lenKey, TyFun(rTy, ty), tok), objArg, ty, tok)
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
    /// failing both, through the core `GetArray` inline function (`ldelem`), a curried
    /// `External` call whose type is rebuilt from the operands.
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
                // `span.[i]` on an external indexer (`Span<char>.get_Item(i) : T&`),
                // recorded in `ExternalAccess`. The BCL accessor returns a managed pointer,
                // so call it and deref with `ldobj <elem>`: the call's type is `elem&`.
                let idxTy = typeOfKey ctx (CstKeys.ofExpr idx)
                let memberName = SymbolKeyOps.intrinsicName info.Key

                // A byref-returning accessor (`Span<char>.get_Item : T&`) needs the
                // `ldobj` deref; a by-value one (`string.get_Chars : char`) is a plain
                // call. Read the declared return off the recorded signature.
                let retIsByref =
                    match Unification.zonk ctx.Store info.Signature with
                    | TyFun(_, TyByref _) -> true
                    | _ -> false

                if retIsByref then
                    let byrefTy = TyConst(RuntimeNames.byrefKey, EqArray.singleton ty)

                    let memberFnTy = TyFun(idxTy, byrefTy)

                    let getItem =
                        TExpr.ExternalMember(
                            ValueSome objArg,
                            info.Key,
                            memberName,
                            MemberStorage.Method,
                            memberFnTy,
                            tok
                        )

                    let callExpr = TExpr.App(getItem, translateExpr ctx idx, byrefTy, tok)
                    TExpr.ILIntrinsic("ldobj", ValueSome ty, EqArray.singleton callExpr, ty, tok)
                else
                    let memberFnTy = TyFun(idxTy, ty)

                    let getItem =
                        TExpr.ExternalMember(
                            ValueSome objArg,
                            info.Key,
                            memberName,
                            MemberStorage.Method,
                            memberFnTy,
                            tok
                        )

                    TExpr.App(getItem, translateExpr ctx idx, ty, tok)
            | ValueNone ->
                let arrTy = typeOfKey ctx (CstKeys.ofExpr r)
                let idxTy = typeOfKey ctx (CstKeys.ofExpr idx)
                let partialTy = TyFun(idxTy, ty)
                let getTy = TyFun(arrTy, partialTy)

                // A `string` object argument lowers through `GetString`, an index-signature
                // one (an external type carrying `{ [k: K]: V }`) through `GetIndex`,
                // every other through `GetArray`. Only WHETHER, never WHICH entry matched.
                let getName =
                    match Unification.zonk ctx.Store arrTy with
                    | TyString -> "GetString"
                    | TyClass(clsKey, _) when
                        not (ctx.Provider.TryLookupIndexSignature(SymbolKey.Type clsKey) |> List.isEmpty)
                        ->
                        "GetIndex"
                    | _ -> "GetArray"

                // Unification (`inferIndexedLookup`) stamped the resolved
                // `GetArray`/`GetString`/`GetIndex` identity under this `IndexedLookup`
                // key; carry it so the `ldelem` / `$0[$1]` body splices by KEY.
                let getKey = ctx.Resolution.IntrinsicKey.TryGetValue key
                let getExpr = TExpr.External(getName, getKey, getTy, tok)
                let app1 = TExpr.App(getExpr, objArg, partialTy, tok)
                TExpr.App(app1, translateExpr ctx idx, ty, tok)
