namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaborateNominals
open XParsec.FSharp.SemanticAnalysis.ElaborateCalls

// Name resolution for the Elaborate pass: the `try*` resolvers that answer what a written
// name / dotted chain denotes, wrapped as the active patterns each `translateExpr` arm
// guards on. None depend on the recursive `translateExpr`.

module internal ElaborateResolve =

    /// A class name only where there is no local `Binding` entry, so a shadowing local wins.
    /// Answers for a project-local class, or an external type whose `TypeKey` the `ResolvedType`
    /// stamp carries. No backend lowers from the returned string; it survives for diagnostics only.
    let rec private tryClassRef (ctx: PassContext) (e: Expr<SyntaxToken>) : string voption =
        let key = CstKeys.ofExpr e

        if ctx.Bindings.Binding.ContainsKey key then
            ValueNone
        else
            let stampedExternal () =
                match ctx.Resolution.ResolvedType.TryGetValue key with
                | ValueSome k -> ValueSome(SymbolKeyOps.typeMetaName k)
                | ValueNone -> ValueNone

            // Scoped by the reference's own position: a class declared BELOW it is not in scope
            // here, so it is not a class reference. The local read comes first, so a local
            // class is never mistaken for an external type of the same spelling.
            let localClass (written: WrittenTypeName) : string voption =
                if (TypeRegistry.tryWrittenClass ctx.Types (ctx.UseSiteAt key) written).IsSome then
                    ValueSome written.Written
                else
                    stampedExternal ()

            match e with
            | Expr.Ident t -> localClass (WrittenTypeName.bare (ctx.NameOf t))
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) -> localClass (ctx.WrittenTypeNameOf li)
            | Expr.TypeApp(expr = inner) -> tryClassRef ctx inner
            | _ -> ValueNone

    /// Resolve `r.M` when the anchor `r` is a local binding of a `TyClass`/`TyUnion`
    /// with a known member `M`. The parser folds the dot into the long ident
    /// rather than emitting `DotLookup` when the anchor is a regular identifier.
    let private tryLongIdentClassAnchor
        (ctx: PassContext)
        (li: LongIdent<SyntaxToken>)
        : (NodeKey * SemType * TypeMemberInfo) voption =
        if li.Idents.Length <> 2 then
            ValueNone
        else
            let anchorKey = NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent

            match ctx.Bindings.Binding.TryGetValue anchorKey with
            | ValueNone -> ValueNone
            | ValueSome rb ->
                match ctx.Bindings.TypeVar.TryGetValue rb.BindingSite with
                | ValueNone -> ValueNone
                | ValueSome tv ->
                    let anchorTy = Unification.zonk ctx.Store (TyVar tv)

                    match anchorTy with
                    | TyNominal(typeKey, _) ->
                        let memberName = ctx.NameOf li.Idents.[1]

                        // By the arity-qualified key: an arity-overloaded type
                        // (`Fun`2`/`Fun`3`) does not resolve by bare name, so a bare lookup
                        // would miss and `f.Invoke(a, b)` mis-lower to a function application.
                        match TypeRegistry.tryNominalMemberByKey ctx.Types typeKey memberName with
                        | ValueSome nm -> ValueSome(rb.BindingSite, anchorTy, nm.Member)
                        | ValueNone -> ValueNone
                    | _ -> ValueNone

    /// Resolve `ClassName.MemberName` to its static member info. The written qualifier's own
    /// token IS the use site: `Foo.Bar` above `type Foo` does not resolve to a type there, so
    /// it must not lower to a static access on the class below.
    let private tryLongIdentStaticMember
        (ctx: PassContext)
        (li: LongIdent<SyntaxToken>)
        : TypeRegistry.NominalMember voption =
        if li.Idents.Length <> 2 then
            ValueNone
        else
            let useSite = ctx.UseSiteAt(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent)

            TypeRegistry.tryStaticMember ctx.Types useSite (ctx.NameOf li.Idents.[0]) (ctx.NameOf li.Idents.[1])

    /// DU ctor reference (`Circle`, `Result2.Ok`, or an external `Some` / `None`), returning
    /// the case name alone, because a caller reads the declaring union off the node's resolved
    /// `TyUnion`. A local binding of the same name has a `Binding` entry and is excluded.
    let private tryCtorRef (ctx: PassContext) (e: Expr<SyntaxToken>) : string voption =
        let key = CstKeys.ofExpr e

        if ctx.Bindings.Binding.ContainsKey key then
            ValueNone
        else
            // A local *or* external union declares this node's name as a case; the external
            // leg is the stamp's presence. A bare reference to an RQA external case is never
            // stamped; only its qualified form is, in the LongIdent arm below.
            let isCase (n: string) =
                TypeRegistry.isCaseName ctx.Types (ctx.UseSiteAt key) n
                || (ResolvedStamps.tryExternalUnionCase ctx.Resolution.Resolved key).IsSome

            match e with
            | Expr.Ident t ->
                let n = ctx.NameOf t

                if isCase n then ValueSome n else ValueNone
            // A case reached through its module (`Test.A.M.Red`), as NameResolution stamped it.
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
                (ResolvedStamps.tryUnionCase ctx.Resolution.Resolved key).IsSome
                ->
                ValueSome(ctx.NameOf li.Idents.[li.Idents.Length - 1])
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                let n = ctx.NameOf li.Idents.[0]

                if isCase n then ValueSome n else ValueNone
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
                li.Idents.Length = 2
                && TypeRegistry.localQualifiedCase
                    ctx.Types
                    (ctx.UseSiteAt key)
                    (ctx.NameOf li.Idents.[0])
                    (ctx.NameOf li.Idents.[1])
                ->
                // `localQualifiedCase` already confirmed the case belongs to the
                // qualifier's union (arity-safe over `Choice\`2`…`Choice\`7`).
                ValueSome(ctx.NameOf li.Idents.[1])
            | _ -> ValueNone

    [<return: Struct>]
    let (|ClassRef|_|) (ctx: PassContext) (e: Expr<SyntaxToken>) : string voption = tryClassRef ctx e

    [<return: Struct>]
    let (|CtorRef|_|) (ctx: PassContext) (e: Expr<SyntaxToken>) : string voption = tryCtorRef ctx e

    [<return: Struct>]
    let (|ClassAnchorMethod|_|) (ctx: PassContext) (li: LongIdent<SyntaxToken>) : (NodeKey * SemType * string) voption =
        match tryLongIdentClassAnchor ctx li with
        | ValueSome(bs, ty, m) when m.Kind = ClassMemberKind.Method ->
            ValueSome(bs, ty, ctx.NameOf li.Idents.[li.Idents.Length - 1])
        | _ -> ValueNone

    [<return: Struct>]
    let (|ClassAnchorProperty|_|)
        (ctx: PassContext)
        (li: LongIdent<SyntaxToken>)
        : (NodeKey * SemType * string) voption =
        match tryLongIdentClassAnchor ctx li with
        | ValueSome(bs, ty, m) when m.Kind = ClassMemberKind.Property ->
            ValueSome(bs, ty, ctx.NameOf li.Idents.[li.Idents.Length - 1])
        | _ -> ValueNone

    [<return: Struct>]
    let (|StaticMethod|_|) (ctx: PassContext) (li: LongIdent<SyntaxToken>) : (TypeKey * string) voption =
        match tryLongIdentStaticMember ctx li with
        | ValueSome nm when nm.Member.Kind = ClassMemberKind.Method ->
            ValueSome(nm.Decl.TypeKey, ctx.NameOf li.Idents.[li.Idents.Length - 1])
        | _ -> ValueNone

    [<return: Struct>]
    let (|StaticMember|_|) (ctx: PassContext) (li: LongIdent<SyntaxToken>) : (TypeKey * string) voption =
        match tryLongIdentStaticMember ctx li with
        | ValueSome nm -> ValueSome(nm.Decl.TypeKey, ctx.NameOf li.Idents.[li.Idents.Length - 1])
        | ValueNone -> ValueNone

    /// `ClassName<'args>.Member` — a static member access on an *explicitly* instantiated
    /// generic class, which parses as `DotLookup(TypeApp(ClassName, <'args>), .Member)`, not
    /// the folded `LongIdent[ClassName; Member]` the bare form takes.
    [<return: Struct>]
    let (|TypeAppStaticMember|_|)
        (ctx: PassContext)
        (e: Expr<SyntaxToken>)
        : (TypeKey * string * ClassMemberKind) voption =
        match e with
        | Expr.DotLookup(
            expr = Expr.TypeApp(expr = CstKeys.SingleIdent classTok); longIdentOrOp = LongIdentOrOp.LongIdent li) when
            li.Idents.Length = 1
            ->
            let memberName = ctx.NameOf li.Idents.[0]
            let useSite = ctx.UseSiteAt(NodeKey.ofToken classTok NodeKind.ExprIdent)

            TypeRegistry.tryStaticMember ctx.Types useSite (ctx.NameOf classTok) memberName
            |> ValueOption.map (fun nm -> nm.Decl.TypeKey, memberName, nm.Member.Kind)
        | _ -> ValueNone

    /// `r.M(...)` where `r` has a class / union type and `M` is one of its instance methods.
    /// Returns the object-argument expr and resolved member name, so the `App` and
    /// `HighPrecedenceApp` invocation arms share one guard.
    [<return: Struct>]
    let (|InstanceMethodCall|_|)
        (ctx: PassContext)
        (funcExpr: Expr<SyntaxToken>)
        : (Expr<SyntaxToken> * TypeKey * string) voption =
        match funcExpr with
        | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
            let memberName = ctx.NameOf li.Idents.[0]

            match Unification.zonk ctx.Store (typeOfKey ctx (CstKeys.ofExpr r)) with
            | TyNominal(typeKey, _) ->
                match TypeRegistry.tryNominalMemberByKey ctx.Types typeKey memberName with
                | ValueSome nm when nm.Member.Kind = ClassMemberKind.Method -> ValueSome(r, nm.Decl.TypeKey, memberName)
                | _ -> ValueNone
            | _ -> ValueNone
        | _ -> ValueNone

    /// The object-argument type a folded LongIdent chain's prefix segments `[1 .. n-2]` land on:
    /// the anchor segment's bound type walked one field / property step at a time. `ValueNone`
    /// if the anchor is not a local binding or any step cannot be typed.
    let private tryChainObjArgTy (ctx: PassContext) (li: LongIdent<SyntaxToken>) : SemType voption =
        let anchorKey = NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent

        match ctx.Bindings.Binding.TryGetValue anchorKey with
        | ValueNone -> ValueNone
        | ValueSome rb ->
            let mutable objArgTy = Unification.zonk ctx.Store (typeOfKey ctx rb.BindingSite)
            let mutable ok = true

            for i in 1 .. li.Idents.Length - 2 do
                if ok then
                    match recoverFieldStepTy ctx objArgTy (ctx.NameOf li.Idents.[i]) with
                    | ValueSome t -> objArgTy <- Unification.zonk ctx.Store t
                    | ValueNone -> ok <- false

            if ok then ValueSome objArgTy else ValueNone

    /// The object-argument chain of a folded member call: the same LongIdent with its
    /// trailing member segment (and the dot before it) dropped, so the caller rebuilds
    /// the object argument via `translateLongIdentFieldChain`.
    let private chainPrefix (li: LongIdent<SyntaxToken>) : LongIdent<SyntaxToken> =
        {
            Idents = li.Idents.RemoveAt(li.Idents.Length - 1)
            Dots = li.Dots.RemoveAt(li.Dots.Length - 1)
        }

    /// `r.f.…g.M(args)` — a method call whose object argument is a *multi-segment* folded chain:
    /// `this.Source.MoveNext` arrives as one `LongIdent[this; Source; MoveNext]`. Returns the
    /// prefix (the chain, last segment dropped), the object-argument type and the method.
    [<return: Struct>]
    let (|ClassChainMethod|_|)
        (ctx: PassContext)
        (li: LongIdent<SyntaxToken>)
        : (LongIdent<SyntaxToken> * SemType * string) voption =
        let n = li.Idents.Length

        if n < 3 then
            // 2-segment `var.M(args)` is `ClassAnchorMethod`; this is the 3+ case.
            ValueNone
        else
            match tryChainObjArgTy ctx li with
            | ValueSome(TyNominal(typeKey, _) as objArgTy) ->
                let memberName = ctx.NameOf li.Idents.[n - 1]

                match TypeRegistry.tryNominalMemberByKey ctx.Types typeKey memberName with
                | ValueSome nm when nm.Member.Kind = ClassMemberKind.Method ->
                    ValueSome(chainPrefix li, objArgTy, memberName)
                | _ -> ValueNone
            | _ -> ValueNone

    /// `r.…M(args)` whose object-argument type is a generic typar coerced to a project-local
    /// interface (`'T :> IFace`), which never grounds to a nominal. The interface's `TypeKey`
    /// was recorded in `TyparInterfaceCall`; returns it alongside `ClassChainMethod`'s shape.
    [<return: Struct>]
    let (|TyparInterfaceMethod|_|)
        (ctx: PassContext)
        (li: LongIdent<SyntaxToken>)
        : (LongIdent<SyntaxToken> * SemType * TypeKey * EqArray<SemType> * string) voption =
        if li.Idents.Length < 2 then
            ValueNone
        else
            let key = NodeKey.ofToken (CstKeys.firstTokenOfLongIdent li) NodeKind.ExprLongIdent

            match ctx.Resolution.TyparInterfaceCall.TryGetValue key with
            | ValueNone -> ValueNone
            | ValueSome(ifaceKey, ifaceArgs) ->
                match tryChainObjArgTy ctx li with
                | ValueNone -> ValueNone
                | ValueSome objArgTy ->
                    let memberName = ctx.NameOf li.Idents.[li.Idents.Length - 1]
                    ValueSome(chainPrefix li, objArgTy, ifaceKey, ifaceArgs, memberName)

    /// The `ResolvedExternalMember` Unification recorded for this node, if any.
    [<return: Struct>]
    let (|ExternalAccess|_|) (ctx: PassContext) (e: Expr<SyntaxToken>) : ResolvedExternalMember voption =
        ctx.Resolution.ExternalAccess.TryGetValue(CstKeys.ofExpr e)
