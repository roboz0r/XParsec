namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

/// What an assignment's left-hand side is written on. `x.P <- v` and `x.[i] <- v` name the
/// object argument as a whole expression; `a.b.P <- v` rides in ONE `Expr.LongIdentOrOp`, so
/// there the object argument is the chain minus its last segment.
[<RequireQualifiedAccess>]
type AssignObjArg =
    | Expr of Expr<SyntaxToken>
    | ChainPrefix of LongIdent<SyntaxToken>

/// What an assignment's left-hand side names. The type-check and the lowering both match on
/// this, so the two cannot disagree about which writes go through a `set_` accessor.
[<RequireQualifiedAccess>]
type AssignTarget =
    /// `x.P <- v`: a member slot, written through `set_P` when the object argument declares
    /// one, and as a field otherwise.
    | Slot of objArg: AssignObjArg * slot: SyntaxToken
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

    let ofExpr (ctx: PassContext) (left: Expr<SyntaxToken>) : AssignLhs =
        let unwrapped = unwrap left

        let target =
            match unwrapped with
            // `C<int>.Static <- v`: the qualifier is a TYPE, not a value to resolve an
            // object argument from, so the static-member paths type the whole LHS.
            | Expr.DotLookup(expr = Expr.TypeApp _) -> AssignTarget.Plain
            | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                AssignTarget.Slot(AssignObjArg.Expr r, li.Idents.[0])
            | Expr.IndexedLookup(expr = objArg; indexExpr = index) -> AssignTarget.Indexed(objArg, index)
            | Expr.DynamicLookup(expr = objArg; ident = name) -> AssignTarget.Dynamic(objArg, name)
            // The anchor-is-a-local-binding test is the one the READ path folds a chain
            // under, so both agree on which `a.b.P` is a field chain rather than a
            // qualified static name.
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
                li.Idents.Length > 1
                && ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent)
                ->
                AssignTarget.Slot(AssignObjArg.ChainPrefix li, li.Idents.[li.Idents.Length - 1])
            | _ -> AssignTarget.Plain

        {
            Target = target
            Access = CstKeys.siteOfExpr unwrapped
        }
