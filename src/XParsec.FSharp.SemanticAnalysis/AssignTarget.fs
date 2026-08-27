namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp
open XParsec.FSharp.Parser

/// What an assignment's left-hand side is written on. `x.P <- v` and `x.[i] <- v` carry the
/// object argument as a whole expression; `a.b.P <- v` rides in ONE `Expr.LongIdentOrOp`, so
/// there the object argument is the chain minus its last segment.
[<RequireQualifiedAccess>]
type AssignObjArg =
    | Expr of Expr<SyntaxToken>
    | ChainPrefix of LongIdent<SyntaxToken>

/// What an assignment's left-hand side denotes. The type-check and the lowering both match on
/// this, so the two cannot disagree about which writes go through a `set_` accessor.
[<RequireQualifiedAccess>]
type AssignTarget =
    /// `x.P <- v`: a member slot, written through `set_P` when the object argument declares
    /// one, and as a field otherwise.
    | Slot of objArg: AssignObjArg * slot: SyntaxToken
    /// `C.P <- v`: a STATIC member slot, written through the resolved `setter` the
    /// qualifier's type declares. `tyArgs` is the qualifier's written `<'args>`
    /// (`C<int>.P <- v`), empty for the bare form.
    | StaticSlot of
        setter: TypeRegistry.NominalMember *
        qualifier: SyntaxToken *
        tyArgs: ImArr<Type<SyntaxToken>> *
        slot: SyntaxToken
    /// `x.[i] <- v`, written through `set_Item` or an element-store intrinsic.
    | Indexed of objArg: Expr<SyntaxToken> * index: Expr<SyntaxToken>
    /// `x?n <- v`, written through `op_DynamicAssignment`.
    | Dynamic of objArg: Expr<SyntaxToken> * name: SyntaxToken
    /// A local, a static member, anything else: a plain assignment.
    | Plain

/// The classified left-hand side, with the node the resolved access files under.
[<NoEquality; NoComparison>]
type AssignLhs =
    {
        Target: AssignTarget
        /// The LHS with its parens and annotations peeled — the expression the access
        /// resolves against, whose key carries the resolved member.
        Access: NodeSite
    }

module AssignTarget =

    /// `((x: C).P) <- v` writes the same slot `x.P <- v` does.
    let rec private unwrap (e: Expr<SyntaxToken>) =
        match e with
        | Expr.EnclosedBlock(expr = inner)
        | Expr.TypeAnnotation(expr = inner) -> unwrap inner
        | _ -> e

    /// `C.P <- v` where the qualifier resolves to a type declaring a static `set_P`. The written
    /// qualifier's own token IS the use site, so a type declared below the write does not
    /// answer for its name.
    let private tryStaticSlot
        (ctx: PassContext)
        (qualifier: SyntaxToken)
        (tyArgs: ImArr<Type<SyntaxToken>>)
        (slot: SyntaxToken)
        : AssignTarget voption =
        let useSite = ctx.UseSiteAt(NodeKey.ofToken qualifier NodeKind.ExprIdent)
        let setterName = AccessorNames.setterName (ctx.NameOf slot)

        TypeRegistry.tryStaticMember ctx.Types useSite (ctx.NameOf qualifier) setterName
        |> ValueOption.map (fun setter ->
            AssignTarget.StaticSlot(setter = setter, qualifier = qualifier, tyArgs = tyArgs, slot = slot)
        )

    let ofExpr (ctx: PassContext) (left: Expr<SyntaxToken>) : AssignLhs =
        let unwrapped = unwrap left

        let target =
            match unwrapped with
            // `C<int>.P <- v`: the qualifier is a TYPE, not a value to resolve an object
            // argument from, so the write reaches its slot through the static setter.
            | Expr.DotLookup(
                expr = Expr.TypeApp(expr = CstKeys.SingleIdent qualifier; types = tyArgs)
                longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                tryStaticSlot ctx qualifier tyArgs li.Idents.[0]
                |> ValueOption.defaultValue AssignTarget.Plain
            | Expr.DotLookup(expr = Expr.TypeApp _) -> AssignTarget.Plain
            | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                AssignTarget.Slot(AssignObjArg.Expr r, li.Idents.[0])
            | Expr.IndexedLookup(expr = objArg; indexExpr = index) -> AssignTarget.Indexed(objArg, index)
            | Expr.DynamicLookup(expr = objArg; ident = name) -> AssignTarget.Dynamic(objArg, name)
            // The READ path folds a chain under this same anchor-is-a-local-binding test, so
            // `a.b.P` is a field chain on both sides rather than a qualified static name.
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
                li.Idents.Length > 1
                && ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent)
                ->
                AssignTarget.Slot(AssignObjArg.ChainPrefix li, li.Idents.[li.Idents.Length - 1])
            // `C.P <- v` folds into ONE LongIdent too. The arm above already took every anchor
            // that IS a local binding, so what reaches here is a type qualifier.
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 2 ->
                tryStaticSlot ctx li.Idents.[0] ImArr.Empty li.Idents.[1]
                |> ValueOption.defaultValue AssignTarget.Plain
            | _ -> AssignTarget.Plain

        {
            Target = target
            Access = CstKeys.siteOfExpr unwrapped
        }
