namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.FreezeResolve

// Identifier and LongIdent-chain projection for the Freeze pass: a resolved
// local becomes a `Var`, a provider hit an `External`, and a multi-segment
// `r.X.Y` chain nested field/property reads. Pure leaves — nothing here
// recurses into expression translation.

module internal FreezeIdents =

    /// A module-qualified reference (`Unchecked.defaultof`) whose target is a
    /// NULLARY zero-operand intrinsic `inline` VALUE (`defaultof` / `undefined` —
    /// `Inline.nullaryIntrinsicValueBody`) the provider serves as a cross-package
    /// inline body under its SIMPLE (last-segment) name, but NOT under the qualified
    /// spelling or the resolved holder key. Returns that simple name so the
    /// `External` head mirrors the bare form and reaches `InlineExpansion`'s
    /// splice-in-place arm; `ValueNone` leaves the dotted name untouched.
    ///
    /// The scope is deliberately narrow. A qualified read of an ordinary inline
    /// module function (`Set.empty`) must stay a keyed `External` codegen emits a
    /// CALL for — rewriting it to its simple name would splice the function's own
    /// body (a `SetTree` construction) into every consumer, minting locals the
    /// backend cannot encode. Only a zero-operand intrinsic value is a compile-time
    /// alias for its `(# … #)` body with nothing to lose or duplicate, so only it is
    /// re-pointed at the splice path.
    let private splicedInlineSimpleName
        (ctx: PassContext)
        (e: Expr<SyntaxToken>)
        (symKey: SymbolKey voption)
        (name: string)
        : string voption =
        match e with
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length > 1 ->
            let simple = ctx.NameOf li.Idents.[li.Idents.Length - 1]

            // Only rewrite when the reference as written can't already find a body
            // (so a resolvable inline is left alone) but the simple name resolves to
            // a nullary zero-operand intrinsic value.
            let alreadyResolves =
                (match symKey with
                 | ValueSome k -> (ctx.Provider.TryLookupInlineBody k).IsSome
                 | ValueNone -> false)
                || (ctx.Provider.TryLookupInlineBodyByName name).IsSome

            let simpleIsNullaryIntrinsic =
                match ctx.Provider.TryLookupInlineBodyByName simple with
                | ValueSome ib -> (Inline.nullaryIntrinsicValueBody ib.Decl).IsSome
                | ValueNone -> false

            if simple <> name && not alreadyResolves && simpleIsNullaryIntrinsic then
                ValueSome simple
            else
                ValueNone
        | _ -> ValueNone

    let translateIdent
        (ctx: PassContext)
        (e: Expr<SyntaxToken>)
        (key: NodeKey)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        match ctx.Bindings.Binding.TryGetValue key with
        | ValueSome rb -> TExpr.Var(rb.BindingSite, ty, tok)
        | ValueNone ->
            // No Binding entry => NameResolution resolved through the provider.
            // Multi-segment names are joined with `.` so `External` carries the
            // same key the provider sees.
            let name =
                match e with
                | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) -> li.Idents |> Seq.map ctx.NameOf |> String.concat "."
                | Expr.LongIdentOrOp(LongIdentOrOp.Op(IdentOrOp.ParenOp(opName = OpName.SymbolicOp op))) ->
                    // `(+)`-as-a-value: carry the operator's compiled name so the
                    // External matches what the provider (and codegen) key on.
                    match Desugar.symbolicOpCompiledName op.Token with
                    | ValueSome n -> n
                    | ValueNone -> ctx.NameOf(CstKeys.firstTokenOfExpr e)
                // `A.B.(+)` — qualified operator form: carry the same `A.B.op_Addition` key NameResolution resolved and
                // the provider keys on.
                | Expr.LongIdentOrOp(LongIdentOrOp.QualifiedOp(longIdent = li; op = idOp)) ->
                    match OperatorNames.qualifiedOpName ctx.NameOf li idOp with
                    | ValueSome n -> n
                    | ValueNone -> ctx.NameOf(CstKeys.firstTokenOfExpr e)
                | _ -> ctx.NameOf(CstKeys.firstTokenOfExpr e)

            // An own-class static-operator member used by value (`Set.(+)`) resolves
            // to that member, not the built-in operator: eta-expand to a closure
            // calling it, ahead of the generic `External` value path.
            match tryOwnOperatorValue ctx key name ty with
            | ValueSome lam -> lam
            | ValueNone ->
                // Stamp the resolved `SymbolKey.ValueKey` when NameResolution recorded
                // one (provider hit). Lets codegen distinguish a canonical
                // `Vesper.Printf.printfn` from a user shadow `MyMod.printfn` by
                // identity rather than name suffix.
                let symKey = ctx.Resolution.ExternalValue.TryGetValue key

                // A module-qualified read of an `inline` intrinsic value
                // (`Unchecked.defaultof`) resolves to a DOTTED `name` and a
                // holder-scoped `key` that the cross-package inline-body index —
                // keyed by the binding's SIMPLE name, and by the bare-reference key —
                // does not carry. Left dotted, the reference misses
                // `InlineExpansion`'s splice arm and codegen emits a `call` into the
                // never-materialised holder (a TypeLoadException at runtime). When the
                // provider serves an inline body under the simple (last-segment) name
                // but neither the qualified spelling nor the resolved key does, carry
                // the simple name so the reference lands on the SAME splice the bare
                // form uses — codegen never sees the `External` (the splice replaces
                // it), so the narrower name only steers body resolution.
                let name =
                    match splicedInlineSimpleName ctx e symKey name with
                    | ValueSome simple -> simple
                    | ValueNone -> name

                TExpr.External(name, symKey, ty, tok)

    /// Fold a multi-segment `r.X.Y…` LongIdent into nested `FieldGet` nodes. The
    /// head segment's TAST node is a `Var` pointing back at the local binding.
    let translateLongIdentFieldChain
        (ctx: PassContext)
        (li: LongIdent<SyntaxToken>)
        (finalTy: SemType)
        (lastExternal: ResolvedExternalMember voption)
        (tok: SyntaxToken)
        : TExpr =
        let head = li.Idents.[0]
        let headKey = NodeKey.ofToken head NodeKind.ExprIdent
        let headBinding = ctx.Bindings.Binding.TryGetValue headKey

        let headTy =
            // Unification didn't allocate a side-table entry for the synthetic
            // head key, so fall back to the binding site's TyVar.
            match headBinding with
            | ValueSome rb -> typeOfKey ctx rb.BindingSite
            | ValueNone -> finalTy

        let headExpr =
            match headBinding with
            | ValueSome rb -> TExpr.Var(rb.BindingSite, headTy, tok)
            | ValueNone -> TExpr.External(ctx.NameOf head, ValueNone, headTy, tok)

        // Wall B (rung 3): the chain's *last* segment may be a property read on a
        // typar receiver constrained to an interface (`this.Source.Current` where
        // `Source : 'E :> IStructEnumerator`). Unification resolved it through the
        // interface and recorded the interface key in `TyparInterfaceCall`, keyed by
        // the LongIdent's NodeKey (the same `CstKeys.ofExpr` identity the inference
        // step used). The receiver never grounds to a nominal, so `fieldStep` would
        // emit a bogus `FieldGet` on an `FTTypar` — route it to a `CallVia.Interface`
        // `PropertyGet` (codegen → `constrained. callvirt get_<name>`) instead.
        let liKey =
            NodeKey.ofToken (CstKeys.firstTokenOfLongIdent li) NodeKind.ExprLongIdent

        let mutable currTy = headTy
        let mutable curr = headExpr

        for i = 1 to li.Idents.Length - 1 do
            let segName = ctx.NameOf li.Idents.[i]
            // Intermediate steps recover the segment's declared type from the
            // receiver — a record/union/class field, or a union/class *instance
            // member* return type (so a chain through a member returning a union,
            // `xs.Tail.Head`, keeps `xs.Tail : Lst<_>` instead of collapsing to the
            // chain's final type). The last step uses the whole chain's `finalTy`.
            let stepTy =
                if i = li.Idents.Length - 1 then
                    finalTy
                else
                    match recoverFieldStepTy ctx currTy segName with
                    | ValueSome t -> t
                    | ValueNone -> finalTy

            // PropertyGet for a class/union member (codegen calls its `get_<name>`,
            // eta-expanding a method-as-value if needed), FieldGet otherwise. The
            // last segment of an external instance access (`e.Current`) emits a
            // keyed `TExpr.ExternalMember` against the receiver built so far — the
            // BCL interface/class member-ref path, not a project-local field.
            curr <-
                match lastExternal with
                | ValueSome info when i = li.Idents.Length - 1 && not info.IsStatic ->
                    TExpr.ExternalMember(ValueSome curr, info.Key, segName, info.Storage, stepTy, tok)
                | _ ->
                    // The `TyparInterfaceCall` entry is keyed by the chain's first
                    // token, which a method call's receiver *prefix* (`this.Source` of
                    // `this.Source.MoveNext()`) shares with the full chain — so also
                    // require the receiver `currTy` to be a typar (the entry is only
                    // ever recorded for a typar receiver), distinguishing the genuine
                    // property read `this.Source.Current` (receiver `'E`) from a nominal
                    // field step `this.Source` (receiver the enclosing class).
                    let isTyparRecv =
                        match Unification.zonk currTy with
                        | TyTypar _
                        | TyVar _ -> true
                        | _ -> false

                    match
                        (if i = li.Idents.Length - 1 && isTyparRecv then
                             ctx.Resolution.TyparInterfaceCall.TryGetValue liKey
                         else
                             ValueNone)
                    with
                    | ValueSome(ifaceKey, ifaceArgs) ->
                        let key = LocalSymbolKey.ofMember ifaceKey segName 0 MemberKind.Property
                        TExpr.PropertyGet(curr, key, CallVia.Interface ifaceArgs, stepTy, tok)
                    | ValueNone -> fieldStep ctx curr currTy segName stepTy tok

            currTy <- stepTy

        curr
