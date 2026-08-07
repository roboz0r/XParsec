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

    /// `r.X <- v` folds to FieldSet; everything else to Assignment.
    let translateAssignment
        (translateExpr: TranslateExpr)
        (ctx: PassContext)
        (key: NodeKey)
        (left: Expr<SyntaxToken>)
        (right: Expr<SyntaxToken>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let unwrapped =
            let rec unwrap e =
                match e with
                | Expr.EnclosedBlock(expr = inner)
                | Expr.TypeAnnotation(expr = inner) -> unwrap inner
                | _ -> e

            unwrap left

        match unwrapped with
        | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
            let fieldName = ctx.NameOf li.Idents.[0]
            TExpr.FieldSet(translateExpr ctx r, fieldName, translateExpr ctx right, ty, tok)
        // `arr.[i] <- v` desugars to the core `SetArray` inline function (`stelem`),
        // the write mirror of the `IndexedLookup` read path. Emit a curried `External`
        // whose type is rebuilt from the operands; `ty` is the `unit` result.
        | Expr.IndexedLookup(expr = arrE; indexExpr = idxE) ->
            let arrTy = typeOfKey ctx (CstKeys.ofExpr arrE)
            let idxTy = typeOfKey ctx (CstKeys.ofExpr idxE)
            let valTy = typeOfKey ctx (CstKeys.ofExpr right)
            let valuePartial = TyFun(valTy, ty)
            let idxPartial = TyFun(idxTy, valuePartial)

            // An index-signature receiver writes through `SetIndex` (the `$0[$1] = $2`
            // bracket), every other receiver through `SetArray` (`stelem`) — the same
            // receiver classification the read branch makes.
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
            let app1 = TExpr.App(setExpr, translateExpr ctx arrE, idxPartial, tok)
            let app2 = TExpr.App(app1, translateExpr ctx idxE, valuePartial, tok)
            TExpr.App(app2, translateExpr ctx right, ty, tok)
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length > 1
            && ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent)
            ->
            // `r.X <- v` parsed as Assignment(LongIdent[r;X], <-, v). The
            // head-resolved chain peels into FieldGet for the intermediate
            // segments and a final FieldSet for the assigned slot.
            let receiverIdents = li.Idents
            let lastIdx = receiverIdents.Length - 1

            // The chain's NodeKey, required by `fieldStep` for its array-length lookup.
            // A read-only `.Length` can never be an assignment-receiver segment, so
            // here it is only ever passed through.
            let liKey =
                NodeKey.ofToken (CstKeys.firstTokenOfLongIdent li) NodeKind.ExprLongIdent

            let receiverChain =
                let head = receiverIdents.[0]
                let headKey = NodeKey.ofToken head NodeKind.ExprIdent
                let headBinding = ctx.Bindings.Binding.TryGetValue headKey

                let headTy =
                    match headBinding with
                    | ValueSome rb -> typeOfKey ctx rb.BindingSite
                    | ValueNone -> typeOfKey ctx (CstKeys.ofExpr unwrapped)

                let headExpr =
                    match headBinding with
                    | ValueSome rb -> TExpr.Var(rb.BindingSite, headTy, tok)
                    | ValueNone -> TExpr.External(ctx.NameOf head, ValueNone, headTy, tok)

                let mutable curr = headExpr
                let mutable currTy = headTy

                // Field reads for the intermediate segments; the assigned slot is the
                // final one, handled by the `FieldSet` below.
                for i = 1 to lastIdx - 1 do
                    let segName = ctx.NameOf receiverIdents.[i]

                    let stepTy =
                        match recoverFieldStepTy ctx currTy segName with
                        | ValueSome t -> t
                        | ValueNone -> currTy

                    curr <- fieldStep ctx liKey curr currTy segName stepTy tok
                    currTy <- stepTy

                curr

            let lastName = ctx.NameOf receiverIdents.[lastIdx]
            TExpr.FieldSet(receiverChain, lastName, translateExpr ctx right, ty, tok)
        // `recv?name <- v` → `(?<-) recv "name" v` → the `op_DynamicAssignment` body
        // `$0[$1] = $2` splices to `recv["name"] = v`. The name is a compile-time
        // string literal (the ident text), NOT a value reference.
        | Expr.DynamicLookup(expr = r; ident = idTok) ->
            let recvTy = typeOfKey ctx (CstKeys.ofExpr r)
            let valTy = typeOfKey ctx (CstKeys.ofExpr right)

            let nameLit =
                TExpr.Const(TConstValue.String(ctx.NameOf idTok), ctx.Intrinsics.String, tok)

            let valuePartial = TyFun(valTy, ty)
            let namePartial = TyFun(ctx.Intrinsics.String, valuePartial)

            let opKey = ctx.Resolution.IntrinsicKey.TryGetValue key

            let opExpr =
                TExpr.External(OperatorData.OpDynamicAssignment, opKey, TyFun(recvTy, namePartial), tok)

            let app1 = TExpr.App(opExpr, translateExpr ctx r, namePartial, tok)
            let app2 = TExpr.App(app1, nameLit, valuePartial, tok)
            TExpr.App(app2, translateExpr ctx right, ty, tok)
        | _ -> TExpr.Assignment(translateExpr ctx left, translateExpr ctx right, ty, tok)

    /// Single-segment `r.X` read on a *project-local* receiver (the external
    /// receiver forms peel off in the dispatcher first).
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
        let receiver = translateExpr ctx r

        // A record exposes BOTH fields and instance-member properties by dot-access, so
        // — unlike a class/union, whose `.X` is always a member — the decision is made
        // here: a member name to `PropertyGet`, a field name to `FieldGet`.
        match rTy with
        | TyRecord(recKey, _) ->
            match tryNominalMemberByKey ctx recKey memberName with
            | ValueSome(declKey, _) ->
                let key = LocalSymbolKey.ofProperty declKey memberName
                TExpr.PropertyGet(receiver, key, viaOfReceiver ctx receiver, ty, tok)
            | ValueNone -> TExpr.FieldGet(receiver, memberName, ty, tok)
        // A class/union receiver's `.X` is always a member — a `PropertyGet` (a
        // method-as-value keeps the shape; codegen eta-expands).
        | TyNominal(nominalKey, _) ->
            let key = LocalSymbolKey.ofProperty nominalKey memberName

            TExpr.PropertyGet(receiver, key, viaOfReceiver ctx receiver, ty, tok)
        // `(expr).Length` on a rank-1 array desugars to the core `GetArrayLength`
        // inline function (`ldlen`); the LongIdent-chain form mirrors this.
        | TyArray _ when memberName = "Length" ->
            let lenKey = ctx.Resolution.IntrinsicKey.TryGetValue key
            TExpr.App(TExpr.External("GetArrayLength", lenKey, TyFun(rTy, ty), tok), receiver, ty, tok)
        | _ -> TExpr.FieldGet(receiver, memberName, ty, tok)

    /// `recv?name` → `(?) recv "name"` → the `op_Dynamic` body `$0[$1]` splices to
    /// `recv["name"]`. The name is a compile-time string literal (the ident text),
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
        let recvTy = typeOfKey ctx (CstKeys.ofExpr r)

        let nameLit =
            TExpr.Const(TConstValue.String(ctx.NameOf idTok), ctx.Intrinsics.String, tok)

        let partialTy = TyFun(ctx.Intrinsics.String, ty)
        let opKey = ctx.Resolution.IntrinsicKey.TryGetValue key

        let opExpr =
            TExpr.External(OperatorData.OpDynamic, opKey, TyFun(recvTy, partialTy), tok)

        let app1 = TExpr.App(opExpr, translateExpr ctx r, partialTy, tok)
        TExpr.App(app1, nameLit, ty, tok)

    /// `arr.[i]` desugars to the core `GetArray` inline function (`ldelem`). Like the
    /// operator path, emit a curried `External` call whose type is rebuilt from the
    /// resolved operand types; `ty` is the element type.
    let translateIndexedLookup
        (translateExpr: TranslateExpr)
        (ctx: PassContext)
        (key: NodeKey)
        (r: Expr<SyntaxToken>)
        (idx: Expr<SyntaxToken>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
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
                        ValueSome(translateExpr ctx r),
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
                        ValueSome(translateExpr ctx r),
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

            // A `string` receiver lowers through `GetString`, an index-signature
            // receiver (an external type carrying `{ [k: K]: V }`) through `GetIndex`,
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
            let app1 = TExpr.App(getExpr, translateExpr ctx r, partialTy, tok)
            TExpr.App(app1, translateExpr ctx idx, ty, tok)
