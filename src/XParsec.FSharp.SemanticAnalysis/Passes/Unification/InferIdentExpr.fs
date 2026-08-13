namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationEngine
open UnificationTranslate
open UnificationInferGeneralize
open UnificationInferLiterals
open UnificationInferResolve
open UnificationInferPat
open UnificationInferOverload
open UnificationInferForwardSchemes
open UnificationInferDispatch
open UnificationInferRecordAccess

module internal UnificationInferIdentExpr =

    let rec inferIdent (ctx: PassContext) (e: Expr<SyntaxToken>) (node: NodeSite) : SemType =
        match e with
        // `(+)` used as a value: resolve the operator's compiled name through the provider
        // and instantiate its scheme like any other external symbol. Nothing type-directed
        // is needed here, because the SRTP trait call in the operator's contract body makes that choice.
        | Expr.LongIdentOrOp(LongIdentOrOp.Op(IdentOrOp.ParenOp(opName = OpName.SymbolicOp op))) ->
            match Desugar.symbolicOpCompiledName op.Token with
            | ValueSome name ->
                // NameResolution stamped the resolved `ExternalSymbol` here; instantiate
                // the scheme by key rather than re-resolving the spelling.
                match ctx.Resolution.ExternalSymbolStamp.TryGetValue node.Key with
                | ValueSome sym -> ExternalSymbols.instantiateSymbol ctx.Store sym ctx.CurrentLevel
                | ValueNone ->
                    errorTy
                        ctx
                        node.Tok
                        (Kind.Message(sprintf "Operator '%s' is not available from the symbol provider" name))
            | ValueNone -> TyVar(freshTyVar ctx)
        // A multi-segment LongIdent anchored on a local binding is a record-field access
        // chain (`r.X.Y`): the parser rides these inside one `Expr.LongIdentOrOp`.
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length > 1
            && ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent)
            ->
            inferLongIdentFieldChain ctx node li
        // An EXTERNAL enum-case access `E.C1`, whose anchor names a provider enum, not a
        // project-local one. Types as the nominal `TyEnum key`, the same key an `(x: E)`
        // annotation resolves to, so the two unify.
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent _) & Stamped ctx.Resolution.ExternalEnumCaseStamp node.Key enumKey when
            not (ctx.Bindings.Binding.ContainsKey node.Key)
            ->
            TyEnum enumKey
        // A project-local enum-case access `E.C1`. Types as the enum nominal `TyEnum Key`,
        // NOT its underlying int/string; an unknown case is a resolution error.
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length = 2
            && not (ctx.Bindings.Binding.ContainsKey node.Key)
            && (TypeRegistry.tryEnum ctx.Types (ctx.UseSiteAt node.Key) (ctx.NameOf li.Idents.[0])).IsSome
            ->
            let einfo =
                (TypeRegistry.tryEnum ctx.Types (ctx.UseSiteAt node.Key) (ctx.NameOf li.Idents.[0])).Value

            let caseName = ctx.NameOf li.Idents.[1]

            if einfo.HasCase caseName then
                TyEnum einfo.TypeKey
            else
                errorTy ctx node.Tok (Kind.NoCase(CaseOwner.Enum, einfo.Name, caseName))
        // Two-segment qualified reference whose anchor is *not* a local binding:
        // `Math.Pi` / `Lst.Empty` / `Result2.Ok`.
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length = 2 && not (ctx.Bindings.Binding.ContainsKey node.Key)
            ->
            let anchorName = ctx.NameOf li.Idents.[0]
            let tailName = ctx.NameOf li.Idents.[1]
            // The qualifier resolves AS SEEN FROM this node: a class / union / record declared
            // below it does not answer for the name, so `Foo.Bar` above `type Foo` falls
            // through to the external cascade and lands unresolved.
            let useSite = ctx.UseSiteAt node.Key

            // Reached once every reading of the name has failed. A `set_P` declaring it makes
            // the read itself the error; a write never arrives here, because the assignment
            // path types its LHS off that setter without inferring a read.
            let orWriteOnly (fallback: unit -> SemType) : SemType =
                let setterName = AccessorNames.setterName tailName

                match TypeRegistry.tryStaticMember ctx.Types useSite anchorName setterName with
                | ValueSome _ ->
                    errorTy ctx node.Tok (Kind.Message(sprintf "Property '%s.%s' is write-only" anchorName tailName))
                | ValueNone -> fallback ()

            match TypeRegistry.tryStaticMember ctx.Types useSite anchorName tailName with
            | ValueSome hit -> freshMemberInstance ctx hit
            | ValueNone ->
                match TypeRegistry.tryUnionBare ctx.Types useSite anchorName with
                | ValueSome _ ->
                    // Qualified ctor reference, resolved through the union registry
                    // and so bypassing the `CtorIndex` ambiguity check.
                    match resolveQualifiedCtor ctx useSite anchorName tailName with
                    | ValueSome info -> ctorType ctx info
                    | ValueNone ->
                        orWriteOnly (fun () ->
                            errorTy ctx node.Tok (Kind.NoCase(CaseOwner.Union, anchorName, tailName))
                        )
                | ValueNone ->
                    // Qualified external union case (`Option.Some`); NameResolution
                    // stamped the resolved case at this node's key.
                    match tryExternalCtorType ctx node.Key with
                    | ValueSome t -> t
                    | ValueNone -> orWriteOnly (fun () -> inferIdentDefault ctx e node)
        | _ -> inferIdentDefault ctx e node

    and inferIdentDefault (ctx: PassContext) (e: Expr<SyntaxToken>) (node: NodeSite) : SemType =

        match ctx.Bindings.Binding.TryGetValue node.Key with
        | ValueSome rb -> instantiateBinding ctx rb
        | ValueNone ->
            // Provider hits beat ctor-name resolution when both exist; a bare ident
            // absent from the provider falls to the ctor registry below.
            match ctx.Resolution.ExternalSymbolStamp.TryGetValue node.Key with
            | ValueSome sym -> ExternalSymbols.instantiateSymbol ctx.Store sym ctx.CurrentLevel
            | ValueNone ->

                match tryExternalStaticLongIdent ctx node.Key e with
                | ValueSome ty -> ty
                | ValueNone ->
                    let singleSegName =
                        match e with
                        | Expr.Ident t -> ValueSome(ctx.NameOf t)
                        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                            ValueSome(ctx.NameOf li.Idents.[0])
                        | _ -> ValueNone

                    match singleSegName with
                    | ValueSome n ->
                        let info, count = resolveCtorName ctx (ctx.UseSiteAt node.Key) n

                        match info with
                        | ValueSome i -> ctorType ctx i
                        | ValueNone when count >= 2 -> errorTy ctx node.Tok (Kind.AmbiguousConstructor(n, count))
                        | ValueNone ->
                            // External union case ctor (`Some` / `None` from a referenced
                            // package): typed as `field… -> TyUnion(union, …)`, so the
                            // bare nullary form (`None`) lands as the union value.
                            match tryExternalCtorType ctx node.Key with
                            | ValueSome t -> t
                            | ValueNone ->
                                // Class-name-as-function: `Point(3, 4)` parses as
                                // `Expr.App(Expr.Ident "Point", …)`, so return the ctor
                                // as a function value and let the function arm type it.
                                classCtorAsFunction ctx (ctx.UseSiteAt node.Key) n
                    | ValueNone ->
                        // A qualified name: `A.Point(3, 4)` names a TYPE through its module,
                        // so it resolves through the type registry as a ctor reference.
                        let localCtor =
                            match e with
                            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) ->
                                tryWrittenClassCtorAsFunction ctx (ctx.UseSiteAt node.Key) (ctx.WrittenTypeNameOf li)
                            | _ -> ValueNone

                        match localCtor with
                        | ValueSome ty -> ty
                        | ValueNone ->
                            // A multi-segment qualified name that resolved to nothing. If its
                            // qualifier names a known external union/record, the tail is a
                            // missing member (`Option.Nope`), so diagnose rather than mint a TyVar.
                            match tryQualifiedExternalMemberMiss ctx e with
                            | ValueSome miss ->
                                errorTy
                                    ctx
                                    node.Tok
                                    (Kind.NoMember(miss.Qualifier, MemberNoun.ValueOrMember, miss.MemberName))
                            | ValueNone -> TyVar(freshTyVar ctx)

    and qualifiedNameOf (ctx: PassContext) (e: Expr<SyntaxToken>) : string =
        match e with
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) -> li.Idents |> Seq.map ctx.NameOf |> String.concat "."
        // `A.B.(+)`: rebuild the same compiled name (`A.B.op_Addition`) NameResolution
        // resolved through the provider, so the round-trip matches its key.
        | Expr.LongIdentOrOp(LongIdentOrOp.QualifiedOp(longIdent = li; op = idOp)) ->
            match OperatorNames.qualifiedOpName ctx.NameOf li idOp with
            | ValueSome n -> n
            | ValueNone -> ctx.NameOf(CstKeys.firstTokenOfExpr e)
        | _ -> ctx.NameOf(CstKeys.firstTokenOfExpr e)

    /// `Set<'T>.Empty` parses as `DotLookup(TypeApp(ClassName, <'args>), .Member)`, and
    /// inferring that prefix as a value would yield the ctor function type.
    and tryLocalTypeAppStaticMember
        (ctx: PassContext)
        (qualifier: Expr<SyntaxToken>)
        (memberTok: SyntaxToken)
        : SemType voption =
        match qualifier with
        // The written class name's own token IS the use site.
        | Expr.TypeApp(expr = CstKeys.SingleIdent classTok) ->
            let useSite = ctx.UseSiteAt(NodeKey.ofToken classTok NodeKind.ExprIdent)

            TypeRegistry.tryStaticMember ctx.Types useSite (ctx.NameOf classTok) (ctx.NameOf memberTok)
            |> ValueOption.map (freshMemberInstance ctx)
        | _ -> ValueNone
