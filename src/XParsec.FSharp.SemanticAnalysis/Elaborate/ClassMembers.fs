namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
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
// nominal hosts is that some of its binders are FIELDS, not locals: a primary-ctor
// param, an instance `let`, a `static let`.

module internal ElaborateClassMembers =

    /// Every reference to such a binder rewrites to a field access, so codegen never sees
    /// the binder's `NodeKey`. `MkSet` carries the write side: a `let mutable` binder IS the
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
            MkGet = fun name ty tok -> TExpr.StaticFieldGet(info.Key, name, ty, tok)
            MkSet = fun name rhs ty tok -> TExpr.StaticFieldSet(info.Key, name, rhs, ty, tok)
        }

    /// Primary-ctor params AND instance-`let` binders share ONE map: an instance `let` is a
    /// ctor param whose value comes from an initialiser rather than an argument. Keying by
    /// `NodeKey` stays exact even though the two families share one source name space.
    let instanceFieldRewrite (info: ClassTypeInfo) (classTy: SemType) : FieldRewrite =
        let names =
            (Map.empty, info.CtorParams)
            ||> Array.fold (fun acc p -> Map.add (BinderKey.identity p.DeclSite.Binder) p.Name acc)

        let names =
            (names, ClassPreamble.lets info.InstancePreamble)
            ||> Array.fold (fun acc l -> Map.add l.DeclKey l.Name acc)

        {
            Names = names
            MkGet =
                fun name ty tok ->
                    TExpr.FieldGet(TExpr.Var(BinderKey.identity info.ThisKey, classTy, tok), name, ty, tok)
            MkSet =
                fun name rhs ty tok ->
                    TExpr.FieldSet(TExpr.Var(BinderKey.identity info.ThisKey, classTy, tok), name, rhs, ty, tok)
        }

    /// Translate one class member element into a `TTypeMember`. A reference to a ctor param
    /// or an instance-`let` binder in an INSTANCE body becomes a `FieldGet`/`FieldSet` on
    /// `this`; a static member sees neither, so only the `static let` rewrite applies there.
    let translateClassMember
        (ctx: PassContext)
        (info: ClassTypeInfo)
        (el: TypeDefnElement<SyntaxToken>)
        : TTypeMember voption =
        // The instantiated self-type the synthesised `this` Var carries. Declaring typars
        // ride as `TyVar` roots here; the cut to `TyTypar` is made over the whole decl.
        let classTy = TyClass(info.TypeKey, declTyparArgs ctx.Store info.TypeParams)

        // `base` is in scope only when the class has an `inherit` clause; an instance
        // member then carries the shared `BaseKey` so a `base.M(...)` receiver resolves.
        let baseKey =
            if info.BaseType.IsSome then
                ValueSome info.BaseKey
            else
                ValueNone

        let staticRewrite = staticFieldRewrite info
        let instanceRewrite = instanceFieldRewrite info classTy

        match el with
        | TypeDefnElement.Member(MemberDefn.Member(
            staticToken = s; keyword = kw; inlineToken = inlineTok; access = memberAccess; defn = d)) ->
            let isStatic = s.IsSome
            let isOverride = isOverrideKeyword kw
            let isInline = inlineTok.IsSome
            let memberAccessibility = accessibilityOfToken memberAccess

            let lowerBody (e: Expr<SyntaxToken>) : TExpr =
                let body = translateExpr ctx e |> rewriteFieldRefs staticRewrite

                if isStatic then
                    body
                else
                    rewriteFieldRefs instanceRewrite body

            // The member's own generic parameters, recovered from the registered
            // `TypeMemberInfo.CanonicalTypars`. That order flows through UNCHANGED, so the
            // ABI index a frozen `TyTypar(Method, i)` marker names stays valid.
            let methodTypeParams
                (n: string)
                (kind: TMemberKind)
                (declKey: NodeKey voption)
                : EqArray<string * SemType> =
                // Materialise each root as a plain `TyVar root`, so the later cut flips it
                // to `TyTypar(Method, i)` like every other embedded type — the tree field
                // never holds a union-find carrier.
                let ofRoots (g: GeneralizedTypars) : EqArray<string * SemType> =
                    GeneralizedTypars.toArray g
                    |> Array.map (fun (name, root) -> name, TyVar root)
                    |> EqArray.ofArray

                let kindMatches (mi: TypeMemberInfo) =
                    match mi.Kind, kind with
                    | ClassMemberKind.Method, TMemberKind.Method
                    | ClassMemberKind.Property, TMemberKind.Property -> true
                    | _ -> false

                // Match the exact overload by its registration `DeclKey` first: same-name
                // overloads share `Name`/`Kind`/`IsStatic`, so a name-only find would give
                // every one the FIRST overload's typars, dropping the others' own `'T`.
                let byKey =
                    match declKey with
                    | ValueSome k -> info.Members |> Array.tryFind (fun mi -> mi.DeclSite.Key = k)
                    | ValueNone -> None

                match
                    byKey
                    |> Option.orElseWith (fun () ->
                        info.Members
                        |> Array.tryFind (fun mi -> mi.Name = n && mi.IsStatic = isStatic && kindMatches mi)
                    )
                with
                | Some mi ->
                    // A root unioned away since generalise keys the body's frozen typar
                    // markers on its SURVIVOR; a root linked to a concrete type is no
                    // longer a typar, and keeping it would inflate the GenericParam arity.
                    mi.CanonicalTypars
                    |> GeneralizedTypars.refreshRoots (fun tv ->
                        match Unification.zonk ctx.Store (TyVar tv) with
                        | TyVar r -> ValueSome r
                        | _ -> ValueNone
                    )
                    |> ofRoots
                | None -> EqArray.empty

            let build (kind: TMemberKind) (b: Binding<SyntaxToken>) : TTypeMember voption =
                match memberNameOfBinding ctx b with
                | ValueSome n ->
                    ValueSome
                        {
                            Name = n
                            IsStatic = isStatic
                            Accessibility = memberAccessibility
                            IsInline = isInline
                            Kind = kind
                            IsOverride = isOverride
                            ThisKey = (if isStatic then ValueNone else ValueSome info.ThisKey)
                            BaseKey = (if isStatic then ValueNone else baseKey)
                            ThisTy = TyClass(info.TypeKey, EqArray.empty)
                            Params = memberParams ctx b
                            Body = lowerBody b.expr
                            ReturnTy = typeOfKey ctx (CstKeys.ofExpr b.expr)
                            MethodTypeParams = methodTypeParams n kind (memberKeyOfBinding b)
                        }
                | ValueNone -> ValueNone

            match d with
            | MethodOrPropDefn.Method(defn = b) -> build TMemberKind.Method b
            | MethodOrPropDefn.Property(defn = b) -> build TMemberKind.Property b
            | MethodOrPropDefn.AutoProperty(access = acc; ident = id; expr = e) ->
                ValueSome
                    {
                        Name = ctx.NameOf id
                        IsStatic = isStatic
                        Accessibility = autoPropertyAccess memberAccessibility acc
                        IsInline = isInline
                        Kind = TMemberKind.Property
                        IsOverride = isOverride
                        ThisKey = (if isStatic then ValueNone else ValueSome info.ThisKey)
                        BaseKey = (if isStatic then ValueNone else baseKey)
                        ThisTy = TyClass(info.TypeKey, EqArray.empty)
                        Params = EqArray.empty
                        Body = lowerBody e
                        ReturnTy = typeOfKey ctx (CstKeys.ofExpr e)
                        // Auto-properties never carry their own generic params.
                        MethodTypeParams = EqArray.empty
                    }
            | _ -> ValueNone
        | _ -> ValueNone

    /// Each `let`-preamble binding becomes a `TCtorLet`, the final chain call's arguments
    /// become `PrimaryArgs`. Sequencing / conditional preambles recurse to the chain and
    /// DROP the statements they pass over.
    let translateSecondaryCtor (ctx: PassContext) (sc: ClassSecondaryCtorInfo) : TSecondaryCtor =
        let parms =
            EqArray.ofSeq (seq { for p in sc.Params -> (p.DeclSite.Binder, Unification.zonk ctx.Store p.Type) })

        let chainArgs (e: Expr<SyntaxToken>) : EqArray<TExpr> =
            let raw =
                match e with
                | Expr.HighPrecedenceApp(argExpr = arg) -> peelOneArg (translateExpr ctx) arg
                | Expr.App(argExprs = args) -> peelCtorArgs (translateExpr ctx) args
                | _ -> EqArray.empty

            raw

        let lets = ResizeArray<TCtorLet>()
        let mutable primaryArgs = EqArray.empty
        let fieldInits = ResizeArray<TCtorFieldInit>()

        // The explicit field-init form `new(args) = { f = e; … }` stores into declared
        // instance fields and has no primary-ctor chain; the LAST `LongIdent` segment
        // names the field.
        let fieldInitsOf (inits: ImmutableArray<FieldInitializer<SyntaxToken>>) =
            for FieldInitializer(longIdent = li; expr = e) in inits do
                if not li.Idents.IsEmpty then
                    fieldInits.Add
                        {
                            Field = ctx.NameOf li.Idents.[li.Idents.Length - 1]
                            Init = translateExpr ctx e
                        }

        let rec go (ace: AdditionalConstrExpr<SyntaxToken>) =
            match ace with
            | AdditionalConstrExpr.LetIn(binding = b; body = body) ->
                // Only a simple-name head binds. The slot keeps just the binder key, so
                // the head's own token is spelled into the context here.
                match BinderKey.siteOfCstPat b.headPat with
                | ValueSome site ->
                    let binder = site.Binder
                    ctx.SpellBinder(binder, site.Tok)

                    lets.Add
                        {
                            Binder = binder
                            Type = typeOfKey ctx (BinderKey.identity binder)
                            Init = translateExpr ctx b.expr
                        }
                | ValueNone -> ()

                go body
            | AdditionalConstrExpr.SequenceAfter(rest = rest) -> go rest
            | AdditionalConstrExpr.SequenceBefore(before = before) -> go before
            | AdditionalConstrExpr.Conditional(thenBranch = t) -> go t
            | AdditionalConstrExpr.Init initExpr ->
                match initExpr with
                | AdditionalConstrInitExpr.Expression e
                | AdditionalConstrInitExpr.Delegated(expr = e) -> primaryArgs <- chainArgs e
                | AdditionalConstrInitExpr.Explicit(initializers = inits) -> fieldInitsOf inits

        go sc.Body

        {
            Params = parms
            Lets = EqArray.ofSeq lets
            PrimaryArgs = primaryArgs
            FieldInits = EqArray.ofSeq fieldInits
        }
