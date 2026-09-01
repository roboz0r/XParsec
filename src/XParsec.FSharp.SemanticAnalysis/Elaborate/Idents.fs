namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaborateNominals
open XParsec.FSharp.SemanticAnalysis.ElaborateCalls

// Identifier and LongIdent-chain projection for the Elaborate pass: a resolved local becomes
// a `Var`, a provider hit an `External`, and a multi-segment `r.X.Y` chain nested
// field/property reads. Nothing here recurses into expression translation.

module internal ElaborateIdents =

    let translateIdent (ctx: PassContext) (key: NodeKey) (ty: SemType) (tok: SyntaxToken) : TExpr =
        match ctx.Bindings.Binding.TryGetValue key with
        | ValueSome rb -> TExpr.Var(rb.BindingSite, ty, tok)
        // No Binding entry => NameResolution resolved through the provider, and stamped the
        // identity it reached. Every spelling of one symbol carries the same key, so the
        // written form is not re-derived here.
        | ValueNone -> externalRef ctx.Resolution.ExternalValue key ty tok

    /// Fold a multi-segment `r.X.Y…` LongIdent into nested `FieldGet` nodes. The
    /// anchor segment (`r`) becomes a `Var` pointing back at the local binding.
    let translateLongIdentFieldChain
        (ctx: PassContext)
        (li: LongIdent<SyntaxToken>)
        (finalTy: SemType)
        (lastExternal: ResolvedExternalMember voption)
        (tok: SyntaxToken)
        : TExpr =
        let anchorIdent = li.Idents.[0]
        let anchorKey = NodeKey.ofToken anchorIdent NodeKind.ExprIdent
        let anchorBinding = ctx.Bindings.Binding.TryGetValue anchorKey

        let anchorTy =
            // Unification didn't allocate a side-table entry for the synthetic
            // anchor key, so fall back to the binding site's TyVar.
            match anchorBinding with
            | ValueSome rb -> typeOfKey ctx rb.BindingSite
            | ValueNone -> finalTy

        let anchorExpr =
            match anchorBinding with
            | ValueSome rb -> TExpr.Var(rb.BindingSite, anchorTy, tok)
            | ValueNone -> externalRef ctx.Resolution.ExternalValue anchorKey anchorTy tok

        // The chain's *last* segment may be a property read on a typar object argument constrained
        // to an interface (`this.Source.Current` where `Source : 'E :> IStructEnumerator<'T>`).
        // It never grounds to a nominal, so a plain field step would be bogus.
        let liKey =
            NodeKey.ofToken (CstKeys.firstTokenOfLongIdent li) NodeKind.ExprLongIdent

        let mutable currTy = anchorTy
        let mutable curr = anchorExpr

        for i = 1 to li.Idents.Length - 1 do
            let segName = ctx.NameOf li.Idents.[i]
            // An intermediate step recovers its own declared type from the object argument, so
            // `xs.Tail.Head` keeps `xs.Tail : Lst<_>` rather than collapsing to the chain's
            // final type. The last step is the only one that uses `finalTy`.
            let stepTy =
                if i = li.Idents.Length - 1 then
                    finalTy
                else
                    match recoverFieldStepTy ctx currTy segName with
                    | ValueSome t -> t
                    | ValueNone -> finalTy

            // `PropertyGet` for a class/union member, `FieldGet` otherwise. The last segment
            // of an external instance access (`e.Current`) is a keyed `TExpr.ExternalMember`
            // against the object argument built so far, not a project-local field.
            curr <-
                match lastExternal with
                | ValueSome info when i = li.Idents.Length - 1 && not info.IsStatic ->
                    TExpr.ExternalMember(
                        ValueSome curr,
                        info.Key,
                        segName,
                        info.Storage,
                        info.ArgGroupWidths,
                        stepTy,
                        tok
                    )
                | _ ->
                    // The entry is keyed by the chain's first token, which `this.Source` (the
                    // object argument of `this.Source.MoveNext()`) shares with the full chain.
                    // Requiring a typar object argument separates the two: `'E` vs the class.
                    let isTyparObjArg =
                        match Unification.zonk ctx.Store currTy with
                        | TyTypar _
                        | TyVar _ -> true
                        | _ -> false

                    match
                        (if i = li.Idents.Length - 1 && isTyparObjArg then
                             ctx.Resolution.TyparInterfaceCall.TryGetValue liKey
                         else
                             ValueNone)
                    with
                    | ValueSome(ifaceKey, ifaceArgs) ->
                        let key = LocalSymbolKey.ofProperty ifaceKey segName
                        TExpr.PropertyGet(curr, key, CallVia.Interface ifaceArgs, stepTy, tok)
                    | ValueNone -> fieldStep ctx curr currTy segName stepTy tok

            currTy <- stepTy

        curr
