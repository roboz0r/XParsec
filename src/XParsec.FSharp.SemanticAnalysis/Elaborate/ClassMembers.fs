namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open Vesper
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaborateNominals
open XParsec.FSharp.SemanticAnalysis.ElaborateCalls
open XParsec.FSharp.SemanticAnalysis.ElaboratePatterns
open XParsec.FSharp.SemanticAnalysis.ElaborateExprArgs
open XParsec.FSharp.SemanticAnalysis.ElaborateExpr
open XParsec.FSharp.SemanticAnalysis.ElaborateTypars
open XParsec.FSharp.SemanticAnalysis.ElaborateMembers

// Class-host lowering for the Elaborate pass. What separates a class from the other
// nominal hosts is that some of its bound variables are FIELDS, not locals: a primary-ctor
// param, an instance `let`, a `static let`.

module internal ElaborateClassMembers =

    /// Every reference to such a bound variable rewrites to a field access, so codegen never sees
    /// the bound variable's `NodeKey`. `MkSet` carries the write side: a `let mutable` bound variable IS the
    /// field, so `c <- c + 1` must store to it, not fork storage into a promoted ref cell.
    [<NoEquality; NoComparison>]
    type FieldRewrite =
        {
            Names: Map<NodeKey, string>
            MkGet: string -> SemType -> SyntaxToken -> TExpr
            MkSet: string -> TExpr -> SemType -> SyntaxToken -> TExpr
        }

    let rewriteFieldRefs (r: FieldRewrite) (body: TExpr) : TExpr =
        if Map.isEmpty r.Names then
            body
        else
            TastWalk.mapExpr
                { TastWalk.identityMapper with
                    OverrideExpr =
                        fun m e ->
                            match e with
                            | TExpr.Assignment(TExpr.Var(k, _, _), rhs, ty, tok) ->
                                match Map.tryFind k r.Names with
                                | Some name -> ValueSome(r.MkSet name (TastWalk.mapExpr m rhs) ty tok)
                                | None -> ValueNone
                            | TExpr.Var(k, ty, tok) ->
                                match Map.tryFind k r.Names with
                                | Some name -> ValueSome(r.MkGet name ty tok)
                                | None -> ValueNone
                            | _ -> ValueNone
                }
                body

    let staticFieldRewrite (info: ClassTypeInfo) : FieldRewrite =
        {
            Names =
                ClassPreamble.lets info.StaticPreamble
                |> Array.map (fun l -> l.DeclKey, l.Name)
                |> Map.ofArray
            MkGet = fun name ty tok -> TExpr.StaticFieldGet(info.TypeKey, name, ty, tok)
            MkSet = fun name rhs ty tok -> TExpr.StaticFieldSet(info.TypeKey, name, rhs, ty, tok)
        }

    /// Primary-ctor params AND instance-`let` bound variables share ONE map: an instance `let` is a
    /// ctor param whose value comes from an initialiser rather than an argument. Keying by
    /// `NodeKey` stays exact even though the two families share one source name space.
    let instanceFieldRewrite (info: ClassTypeInfo) (classTy: SemType) : FieldRewrite =
        let names =
            (Map.empty, info.CtorParams)
            ||> Array.fold (fun acc p -> Map.add (BoundVarKey.identity p.DeclSite.BoundVar) p.Name acc)

        let names =
            (names, ClassPreamble.lets info.InstancePreamble)
            ||> Array.fold (fun acc l -> Map.add l.DeclKey l.Name acc)

        {
            Names = names
            MkGet =
                fun name ty tok ->
                    TExpr.FieldGet(TExpr.Var(BoundVarKey.identity info.ThisKey, classTy, tok), name, ty, tok)
            MkSet =
                fun name rhs ty tok ->
                    TExpr.FieldSet(TExpr.Var(BoundVarKey.identity info.ThisKey, classTy, tok), name, rhs, ty, tok)
        }

    /// The class as a member-declaring type. In an INSTANCE body a ctor param or
    /// instance-`let` bound variable becomes a `FieldGet`/`FieldSet` on `this`; a static
    /// member sees only `static let`.
    let classDeclaringType (ctx: PassContext) (info: ClassTypeInfo) : DeclaringType =
        // The instantiated self-type the synthesised `this` Var carries. Declaring typars
        // are still `TyVar` roots; the cut to `TyTypar` is made over the whole decl.
        let classTy = TyClass(info.TypeKey, declTyparArgs ctx.Store info.TypeParams)

        let staticRewrite = staticFieldRewrite info
        let instanceRewrite = instanceFieldRewrite info classTy

        let lowerBody (site: MemberSite) (e: Expr<SyntaxToken>) : TExpr =
            let body = translateExpr ctx e |> rewriteFieldRefs staticRewrite

            if site.IsStatic then
                body
            else
                rewriteFieldRefs instanceRewrite body

        {
            TypeKey = info.TypeKey
            TypeParams = info.TypeParams
            Members = hostMembers info.Body.Members info.Body.InterfaceImpls
            ThisKey = info.ThisKey
            ThisTy = classTy
            // `base` is in scope only when the class has an `inherit` clause; an instance
            // member then carries the shared `BaseKey` so a `base.M(...)` object argument resolves.
            BaseKey =
                if info.Base.IsSome then
                    ValueSome info.BaseKey
                else
                    ValueNone
            LowerBody = lowerBody
        }

    /// Each `let`-preamble binding becomes a `TCtorLet`, and the final chain call's arguments
    /// become the `Chain` body. The TAST keeps only the chain (`rest` / `before` /
    /// `thenBranch`); a discarded statement, condition, or else branch is diagnosed as
    /// unsupported rather than silently lost.
    let translateSecondaryCtor (ctx: PassContext) (className: string) (sc: ClassSecondaryCtorInfo) : TSecondaryCtor =
        let parms =
            Block.ofSeq (seq { for p in sc.Params -> (p.DeclSite.BoundVar, Unification.zonk ctx.Store p.Type) })

        let chainArgs (e: Expr<SyntaxToken>) : Block<TExpr> =
            match e with
            | Expr.HighPrecedenceApp(argExpr = arg) -> peelOneArg (translateExpr ctx) arg
            | Expr.App(argExprs = args) -> peelCtorArgs (translateExpr ctx) args
            | _ -> Block.empty

        let lets = ResizeArray<TCtorLet>()

        // The explicit field-init form `new(args) = { f = e; … }` stores into declared
        // instance fields and has no primary-ctor chain; the LAST `LongIdent` segment
        // identifies the field.
        let fieldInitsOf (inits: ImmutableArray<FieldInitializer<SyntaxToken>>) : Block<TCtorFieldInit> =
            Block.ofSeq (
                seq {
                    for FieldInitializer(longIdent = li; expr = e) in inits do
                        if not li.Idents.IsEmpty then
                            {
                                Field = ctx.NameOf li.Idents.[li.Idents.Length - 1]
                                Init = translateExpr ctx e
                            }
                }
            )

        let diagnoseDropped (tok: SyntaxToken) (what: string) =
            ctx.Report(tok, Kind.NotYetSupported(sprintf "%s in a secondary constructor of '%s'" what className))

        // Every form reaches an `Init`, so the body is a return value rather than a slot the
        // walk fills: an unwritten one is not a state a secondary constructor has.
        let rec go (ace: AdditionalConstrExpr<SyntaxToken>) : TSecondaryCtorBodyG<TExpr> =
            match ace with
            | AdditionalConstrExpr.LetIn(binding = b; body = body) ->
                // Only a simple name binds. The slot keeps just the bound variable key, so the
                // pattern's own token is spelled into the context here.
                match BoundVarKey.siteOfCstPat b.pattern with
                | ValueSome site ->
                    let boundVar = site.BoundVar
                    ctx.SetBoundVarName(boundVar, site.Tok)

                    lets.Add
                        {
                            BoundVar = boundVar
                            Type = typeOfKey ctx (BoundVarKey.identity boundVar)
                            Init = translateExpr ctx b.expr
                        }
                | ValueNone -> ()

                go body
            | AdditionalConstrExpr.SequenceAfter(stmt = stmt; rest = rest) ->
                diagnoseDropped (CstKeys.siteOfExpr stmt).Tok "a statement before the constructor chain call"
                go rest
            | AdditionalConstrExpr.SequenceBefore(before = before; expr = e) ->
                diagnoseDropped (CstKeys.siteOfExpr e).Tok "a 'then' statement after the constructor chain call"
                go before
            | AdditionalConstrExpr.Conditional(ifToken = ifTok; thenBranch = t) ->
                diagnoseDropped ifTok "a conditional constructor chain"
                go t
            | AdditionalConstrExpr.Init initExpr ->
                match initExpr with
                | AdditionalConstrInitExpr.Expression e
                | AdditionalConstrInitExpr.Delegated(expr = e) -> TSecondaryCtorBodyG.Chain(chainArgs e)
                | AdditionalConstrInitExpr.Explicit(initializers = inits) ->
                    TSecondaryCtorBodyG.ExplicitFieldInit(fieldInitsOf inits)

        let body = go sc.Body

        {
            Params = parms
            Lets = Block.ofSeq lets
            Body = body
        }
