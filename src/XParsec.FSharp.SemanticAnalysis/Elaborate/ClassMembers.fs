namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaborateResolve
open XParsec.FSharp.SemanticAnalysis.ElaboratePatterns
open XParsec.FSharp.SemanticAnalysis.ElaborateExprArgs
open XParsec.FSharp.SemanticAnalysis.ElaborateExpr
open XParsec.FSharp.SemanticAnalysis.ElaborateTypars
open XParsec.FSharp.SemanticAnalysis.ElaborateMembers

// Class-host lowering for the Elaborate pass. What separates a class from the other
// nominal hosts is that some of its binders are FIELDS, not locals: a primary-ctor
// param, an instance `let`, a `static let`. Every body lowered here runs through the
// rewrites that turn a reference to one into a field access, so codegen never sees
// the binder's `NodeKey`.

module internal ElaborateClassMembers =

    /// The class binders that are not locals but FIELDS: a primary-ctor param and an
    /// instance-`let` binder (instance fields), a `static let` binder (a static field).
    /// Every reference to one — in a member body, in a `.cctor` initialiser, in a later
    /// preamble entry — must be rewritten to a field access, so codegen never sees the
    /// binder's `NodeKey`. `MkSet` rewrites the WRITE side with the read side: a
    /// `let mutable` binder IS the field, so a `c <- c + 1` must store to it (a `TExpr.Let`
    /// binder would instead be promoted to a ref cell and fork the storage). An instance
    /// field stores via `TExpr.FieldSet` on `this`; a `static let mutable` via
    /// `TExpr.StaticFieldSet`. `MkSet` stays `ValueNone` only where no store node exists
    /// for the target; the "assignment to immutable binding" check already rejects a write
    /// to a non-`mutable` binder upstream, so a plain `static let` never reaches this.
    [<NoEquality; NoComparison>]
    type FieldRewrite =
        {
            /// Field name, by binder `NodeKey`.
            Names: Map<NodeKey, string>
            MkGet: string -> SemType -> SyntaxToken -> TExpr
            MkSet: (string -> TExpr -> SemType -> SyntaxToken -> TExpr) voption
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
                                match r.MkSet, Map.tryFind k r.Names with
                                | ValueSome mkSet, Some name -> ValueSome(mkSet name (TastWalk.mapExpr m rhs) ty tok)
                                | _ -> ValueNone
                            | TExpr.Var(k, ty, tok) ->
                                match Map.tryFind k r.Names with
                                | Some name -> ValueSome(r.MkGet name ty tok)
                                | None -> ValueNone
                            | _ -> ValueNone
                }
                body

    /// `static let` binders → `TExpr.StaticFieldGet` on the declaring class.
    let staticFieldRewrite (info: ClassTypeInfo) : FieldRewrite =
        {
            Names =
                ClassPreamble.lets info.StaticPreamble
                |> Array.map (fun l -> l.DeclKey, l.Name)
                |> Map.ofArray
            MkGet = fun name ty tok -> TExpr.StaticFieldGet(info.Key, name, ty, tok)
            MkSet = ValueSome(fun name rhs ty tok -> TExpr.StaticFieldSet(info.Key, name, rhs, ty, tok))
        }

    /// Primary-ctor params AND instance-`let` binders → `TExpr.FieldGet`/`FieldSet` on
    /// `this`. ONE map, because they are one kind of thing: an instance `let` is a ctor
    /// param whose value comes from an initialiser rather than an argument. The map is
    /// keyed by `NodeKey`, so it stays exact even though the two families share a name
    /// space — which `NameResolution` separately requires to be collision-free, since a
    /// field is emitted under its SOURCE name.
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
                ValueSome(fun name rhs ty tok ->
                    TExpr.FieldSet(TExpr.Var(BinderKey.identity info.ThisKey, classTy, tok), name, rhs, ty, tok)
                )
        }

    /// Translate one class member element into a `TTypeMember`. Parallel to the nominal
    /// (union / record) member translation — only differs in the `ThisTy` shape
    /// (`TyClass` vs `TyUnion`/`TyRecord`) and in the field rewrites: a
    /// reference to a ctor param or an instance-`let` binder in an *instance* body becomes
    /// a `FieldGet`/`FieldSet` on `this`, and one to a `static let` binder becomes a
    /// `StaticFieldGet` — so the back end resolves them through the same field mechanism
    /// every other nominal type uses (codegen never sees the binder's NodeKey). Static
    /// members see neither `this` nor the instance binders (front-end's `staticScope`), so
    /// only the static rewrite applies there.
    let translateClassMember
        (ctx: PassContext)
        (info: ClassTypeInfo)
        (el: TypeDefnElement<SyntaxToken>)
        : TTypeMember voption =
        // The instantiated self-type the synthesised `this` Var carries. Empty
        // typar list for a monomorphic class; the declaring typars ride as
        // `TyVar` roots (not `TyTypar`), which `freezeTypars` cuts over the
        // whole member body.
        let classTy = TyClass(info.TypeKey, declTyparArgs ctx.Store info.TypeParams)

        // `base` is in scope only when the class has an `inherit` clause; an
        // instance member then carries the shared `BaseKey` so codegen maps a
        // `base.M(...)` receiver to `ldarg.0` (CallVia.Base drives the
        // non-virtual opcode — see `viaOfReceiver`).
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
            // Member-level accessibility (`member private this.M`) rides
            // `MemberDefn.Member.access`, not the inner `Binding.access`.
            let memberAccessibility = accessibilityOfToken memberAccess

            let lowerBody (e: Expr<SyntaxToken>) : TExpr =
                let body = translateExpr ctx e |> rewriteFieldRefs staticRewrite

                if isStatic then
                    body
                else
                    rewriteFieldRefs instanceRewrite body

            // The member's own generic parameters (B-12), recovered from the
            // registered `TypeMemberInfo`'s canonical `Generalized` order. The order
            // flows through UNCHANGED (the carrier mints no new order); each entry's
            // root is refreshed to its current union-find / link representative and
            // any that pinned to a concrete type since generalise is DROPPED — both
            // ORDER-PRESERVING, so the ABI index is untouched. Codegen installs these
            // roots as ambient method typars so they encode to `!!i`.
            let methodTypeParams
                (n: string)
                (kind: TMemberKind)
                (declKey: NodeKey voption)
                : EqArray<string * SemType> =
                // Materialize the canonical carrier into the tree as the typars' own
                // types (`TyVar root`), so `freezeTypars` / `TastConvert` flip them to
                // `TyTypar(Method, i)` → `FTTypar(Method, i)` exactly like every other
                // embedded type — the tree field holds no union-find carrier.
                let ofRoots (g: GeneralizedTypars) : EqArray<string * SemType> =
                    GeneralizedTypars.toArray g
                    |> Array.map (fun (name, root) -> name, TyVar root)
                    |> EqArray.ofArray

                let kindMatches (mi: TypeMemberInfo) =
                    match mi.Kind, kind with
                    | ClassMemberKind.Method, TMemberKind.Method
                    | ClassMemberKind.Property, TMemberKind.Property -> true
                    | _ -> false

                // Match the *exact* overload by its registration `DeclKey` first —
                // same-name overloads share `Name`/`Kind`/`IsStatic`, so a name-only
                // `tryFind` would return the first overload's typars for every one,
                // dropping the others' own `'T`. Fall back to the name match
                // for any member whose binding key didn't resolve (operator heads,
                // auto-properties — none of which overload generically).
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
                    // Refresh each canonical root to its CURRENT union-find / link
                    // representative and DROP any that pinned to a concrete type since
                    // generalise — ORDER-PRESERVING, so the ABI index is untouched.
                    // Per-entry `zonk`+drop: a root unioned away keys the body's frozen
                    // `TyTypar(Method, i)` markers on its survivor, and a root linked to a
                    // concrete type is no longer a real typar (keeping it would inflate
                    // the GenericParam arity).
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

    /// Translate one secondary constructor into a `TSecondaryCtor`. The
    /// params / preamble / chain-call args are translated verbatim; each
    /// `let`-preamble binding becomes a `TCtorLet`, the final chain call's
    /// arguments become `PrimaryArgs`. A generic class's declaring typars ride as
    /// `TyVar` roots and are cut over the whole decl by `freezeTypars` (the
    /// declaring env `tryClassType` collects), so no per-ctor remap is needed here.
    /// v1 supports a `let` preamble followed by the chain call; sequencing /
    /// conditional preambles recurse to the chain and drop intervening statements.
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

        // The explicit field-init form `new(args) = { f = e; … }`:
        // each `FieldInitializer` stores into a declared
        // instance field. The `LongIdent` is a single field-name segment (the
        // last segment names the field); there is no primary-ctor chain.
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
                // A `let`-preamble head binds a simple name in v1; its key is the one
                // `translatePat` mints, so a body reference resolves to this local. The
                // slot keeps only the key, so the head's token is recorded here.
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
