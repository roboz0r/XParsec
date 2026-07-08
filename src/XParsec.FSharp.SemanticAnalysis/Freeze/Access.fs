namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.FreezeResolve
open XParsec.FSharp.SemanticAnalysis.FreezeExprArgs

// Element and member *access* lowering for the Freeze pass: assignment
// (`<-` in its FieldSet / indexer-set / dynamic-set forms), indexer reads
// (`arr.[i]`), dynamic-member reads (`recv?name`), and single-segment
// `r.X` property/field reads. The get/set pairs mirror each other
// (`GetArray`/`SetArray`, `GetIndex`/`SetIndex`, `op_Dynamic`/
// `op_DynamicAssignment`), which is why they live together.

module internal FreezeAccess =

    /// `r.X <- v` folds to FieldSet; everything else to Assignment.
    let translateAssignment
        (translateExpr: TranslateExpr)
        (ctx: PassContext)
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
        // `arr.[i] <- v` desugars to the core `SetArray` inline function (the
        // write mirror of the `IndexedLookup` → `GetArray` read path below):
        // the `stelem` mnemonic lives in Vesper.Core's `ops-platform.fs`,
        // spliced at this use site by `InlineExpansion` — never invented in this
        // target-agnostic pass. Emit a curried `External` call whose type is
        // rebuilt from the resolved operand types (`ty` is the assignment's
        // `unit` result).
        | Expr.IndexedLookup(expr = arrE; indexExpr = idxE) ->
            let arrTy = typeOfKey ctx (CstKeys.ofExpr arrE)
            let idxTy = typeOfKey ctx (CstKeys.ofExpr idxE)
            let valTy = typeOfKey ctx (CstKeys.ofExpr right)
            let valuePartial = TyFun(valTy, ty)
            let idxPartial = TyFun(idxTy, valuePartial)

            // An index-signature receiver writes through `SetIndex` (the `$0[$1] = $2`
            // bracket), the write mirror of the read path's `GetIndex`; every other
            // receiver through `SetArray` (`stelem`). Same receiver classification as
            // the read branch — whether the external type carries an index signature.
            let setName =
                match Unification.zonk arrTy with
                | TyClass(clsKey, _) when
                    not (
                        ctx.Provider.TryLookupIndexSignature(SymbolKeyOps.qualifiedName clsKey)
                        |> List.isEmpty
                    )
                    ->
                    "SetIndex"
                | _ -> "SetArray"

            let setExpr = TExpr.External(setName, ValueNone, TyFun(arrTy, idxPartial), tok)
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

                // Field reads for the intermediate segments — the assigned slot
                // is the final one, handled by the `FieldSet` below. Same chain
                // walk as `translateLongIdentFieldChain`, stopping one short.
                for i = 1 to lastIdx - 1 do
                    let segName = ctx.NameOf receiverIdents.[i]

                    let stepTy =
                        match recoverFieldStepTy ctx currTy segName with
                        | ValueSome t -> t
                        | ValueNone -> currTy

                    curr <- fieldStep ctx curr currTy segName stepTy tok
                    currTy <- stepTy

                curr

            let lastName = ctx.NameOf receiverIdents.[lastIdx]
            TExpr.FieldSet(receiverChain, lastName, translateExpr ctx right, ty, tok)
        // `recv?name <- v` → `(?<-) recv "name" v` → the `op_DynamicAssignment`
        // inline body `$0[$1] = $2` splices to the computed-member write
        // `recv["name"] = v`. The name is a compile-time string literal (the ident
        // text), NOT a value reference. Mirrors the `SetArray` curried-External shape.
        | Expr.DynamicLookup(expr = r; ident = idTok) ->
            let recvTy = typeOfKey ctx (CstKeys.ofExpr r)
            let valTy = typeOfKey ctx (CstKeys.ofExpr right)

            let nameLit =
                TExpr.Const(TConstValue.String(ctx.NameOf idTok), ctx.Intrinsics.String, tok)

            let valuePartial = TyFun(valTy, ty)
            let namePartial = TyFun(ctx.Intrinsics.String, valuePartial)

            let opExpr =
                TExpr.External("op_DynamicAssignment", ValueNone, TyFun(recvTy, namePartial), tok)

            let app1 = TExpr.App(opExpr, translateExpr ctx r, namePartial, tok)
            let app2 = TExpr.App(app1, nameLit, valuePartial, tok)
            TExpr.App(app2, translateExpr ctx right, ty, tok)
        | _ -> TExpr.Assignment(translateExpr ctx left, translateExpr ctx right, ty, tok)

    /// Single-segment `r.X` read on a *project-local* receiver (the external
    /// receiver forms peel off in the dispatcher first).
    let translateDotLookup
        (translateExpr: TranslateExpr)
        (ctx: PassContext)
        (r: Expr<SyntaxToken>)
        (memberName: string)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let rTy = Unification.zonk (typeOfKey ctx (CstKeys.ofExpr r))
        let receiver = translateExpr ctx r

        // A class/union receiver's member access is a `PropertyGet` (a
        // method-as-value keeps the same shape — codegen eta-expands);
        // anything else reads a record/tuple field.
        match rTy with
        | TyClass _
        | TyUnion _ ->
            let key =
                LocalSymbolKey.ofMember (nominalDeclKey rTy) memberName 0 MemberKind.Property

            TExpr.PropertyGet(receiver, key, viaOfReceiver ctx receiver, ty, tok)
        // `(expr).Length` on an intrinsic rank-1 array desugars to the core
        // `GetArrayLength` inline function — the `ldlen` mnemonic lives in
        // `ops-platform.fs`, spliced by `InlineExpansion`. Mirrors the
        // `fieldStep` array guard (the LongIdent-chain form).
        | TyArray _ when memberName = "Length" ->
            TExpr.App(TExpr.External("GetArrayLength", ValueNone, TyFun(rTy, ty), tok), receiver, ty, tok)
        | _ -> TExpr.FieldGet(receiver, memberName, ty, tok)

    /// `recv?name` → `(?) recv "name"` → the `op_Dynamic` inline body `$0[$1]`
    /// splices to the computed-member read `recv["name"]`. The name is a compile-time
    /// string literal (the ident text), NOT a value reference. `ty` is the (possibly
    /// target-typed) result. Mirrors the `GetArray` curried-External shape.
    let translateDynamicLookup
        (translateExpr: TranslateExpr)
        (ctx: PassContext)
        (r: Expr<SyntaxToken>)
        (idTok: SyntaxToken)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let recvTy = typeOfKey ctx (CstKeys.ofExpr r)

        let nameLit =
            TExpr.Const(TConstValue.String(ctx.NameOf idTok), ctx.Intrinsics.String, tok)

        let partialTy = TyFun(ctx.Intrinsics.String, ty)
        let opExpr = TExpr.External("op_Dynamic", ValueNone, TyFun(recvTy, partialTy), tok)
        let app1 = TExpr.App(opExpr, translateExpr ctx r, partialTy, tok)
        TExpr.App(app1, nameLit, ty, tok)

    /// `arr.[i]` desugars to the core `GetArray` inline function (mirroring F#'s
    /// `IntrinsicFunctions.GetArray`): the `ldelem` mnemonic lives in
    /// Vesper.Core's `ops-platform.fs`, spliced at this use site by
    /// `InlineExpansion` — never invented in this target-agnostic pass. Mirrors
    /// the operator path (`translateInfix`): emit a curried `External` call whose
    /// type is rebuilt from the resolved operand types. `ty` is the element type.
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
            // `span.[i]` on an external indexer (`Span<char>.get_Item(i) : T&`):
            // Unification recorded the resolved `get_Item` in `ExternalAccess`.
            // The BCL accessor returns a managed pointer and has no by-value
            // form, so call it through the external-instance-method machinery
            // (PP2a address dispatch — the receiver is an unboxed struct) and
            // dereference the result with `ldobj <elem>`. `ty` is the
            // value-position element; the call's static type is `elem&`
            // (`TyConst(byrefName, [elem])`), which `ldobj` loads.
            let idxTy = typeOfKey ctx (CstKeys.ofExpr idx)
            let memberName = SymbolKeyOps.intrinsicName info.Key

            // A byref-returning accessor (`Span<char>.get_Item : T&`) needs the
            // `ldobj` deref; a by-value one (`string.get_Chars : char`) is a plain
            // call. Read the declared return off the recorded signature.
            let retIsByref =
                match Unification.zonk info.Signature with
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

            // A `string` receiver lowers through `GetString` (its inline body emits
            // the native `s[i]` on JS); an index-signature receiver (an external type
            // carrying `{ [k: K]: V }`) through `GetIndex` (the same `$0[$1]` bracket);
            // every other receiver through `GetArray` (`ldelem`). The inference picked
            // the matching intrinsic (`inferIndexedLookup`), so the names line up. The
            // bracket lowering is identical for every index entry, so Freeze checks only
            // WHETHER the receiver has an index signature, never WHICH entry matched.
            let getName =
                match Unification.zonk arrTy with
                | TyString -> "GetString"
                | TyClass(clsKey, _) when
                    not (
                        ctx.Provider.TryLookupIndexSignature(SymbolKeyOps.qualifiedName clsKey)
                        |> List.isEmpty
                    )
                    ->
                    "GetIndex"
                | _ -> "GetArray"

            let getExpr = TExpr.External(getName, ValueNone, getTy, tok)
            let app1 = TExpr.App(getExpr, translateExpr ctx r, partialTy, tok)
            TExpr.App(app1, translateExpr ctx idx, ty, tok)
