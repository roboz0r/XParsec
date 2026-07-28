namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

// NodeKey is (firstToken.StartIndex, kind). Shapes outside the handled subset fall
// through to `failwith` — extend the match arms as new node kinds need keying.

module CstKeys =

    let firstTokenOfLongIdent (li: LongIdent<SyntaxToken>) : SyntaxToken = li.Idents.[0]

    let firstTokenOfIdentOrOp (op: IdentOrOp<SyntaxToken>) : SyntaxToken =
        match op with
        | IdentOrOp.Ident t -> t
        | IdentOrOp.ParenOp(lParen, _, _) -> lParen

    let firstTokenOfLongIdentOrOp (lio: LongIdentOrOp<SyntaxToken>) : SyntaxToken =
        match lio with
        | LongIdentOrOp.LongIdent li -> firstTokenOfLongIdent li
        | LongIdentOrOp.Op op -> firstTokenOfIdentOrOp op
        | LongIdentOrOp.QualifiedOp(li, _, _) -> firstTokenOfLongIdent li

    let firstTokenOfConstant (c: Constant<SyntaxToken>) : SyntaxToken =
        match c with
        | Constant.Literal t -> t
        | Constant.MeasuredLiteral(value = t) -> t

    let firstTokenOfParenKind (pk: ParenKind<SyntaxToken>) : SyntaxToken =
        match pk with
        | ParenKind.Paren t
        | ParenKind.BeginEnd t
        | ParenKind.List t
        | ParenKind.Array t
        | ParenKind.Brace t
        | ParenKind.BraceBar t
        | ParenKind.Quoted t
        | ParenKind.DoubleQuoted t -> t

    let firstTokenOfLetOrUseKeyword (kw: LetOrUseKeyword<SyntaxToken>) : SyntaxToken =
        match kw with
        | LetOrUseKeyword.Let t
        | LetOrUseKeyword.LetBang t
        | LetOrUseKeyword.Use t
        | LetOrUseKeyword.UseBang t -> t

    let rec firstTokenOfExpr (e: Expr<SyntaxToken>) : SyntaxToken =
        match e with
        | Expr.Const c -> firstTokenOfConstant c
        | Expr.Ident t -> t
        | Expr.LongIdentOrOp lio -> firstTokenOfLongIdentOrOp lio
        | Expr.App(funcExpr, _) -> firstTokenOfExpr funcExpr
        | Expr.HighPrecedenceApp(funcExpr = funcExpr) -> firstTokenOfExpr funcExpr
        // Use the operator token (not the left expr) so nested same-kind
        // InfixApps in left-assoc chains and operator-precedence stacks
        // don't collide on NodeKey.
        | Expr.InfixApp(_, op, _) -> op
        | Expr.PrefixApp(op, _) -> op
        | Expr.Range(fromExpr = fromE) -> firstTokenOfExpr fromE
        | Expr.SteppedRange(fromExpr = fromE) -> firstTokenOfExpr fromE
        | Expr.Null(nullToken = t) -> t
        | Expr.LetOrUse(keyword = kw) -> firstTokenOfLetOrUseKeyword kw
        | Expr.Fun(funToken = t) -> t
        | Expr.EnclosedBlock(lParen = pk) -> firstTokenOfParenKind pk
        | Expr.EmptyBlock(lParen = pk) -> firstTokenOfParenKind pk
        | Expr.IfThenElse(ifToken = t) -> t
        | Expr.Tuple(exprs = exprs) when exprs.Length > 0 -> firstTokenOfExpr exprs.[0]
        | Expr.Sequential(exprs = exprs) when exprs.Length > 0 -> firstTokenOfExpr exprs.[0]
        | Expr.TypeAnnotation(expr = inner) -> firstTokenOfExpr inner
        // The cast operator token, so chained casts (`x :> A :> B`) don't collide
        // with each other or the inner expr (the `InfixApp` operator-token rule).
        | Expr.StaticUpcast(colonGreaterThan = t) -> t
        | Expr.DynamicTypeTest(colonQuestionMark = t) -> t
        | Expr.DynamicDowncast(colonQuestionMarkGreaterThan = t) -> t
        | Expr.While(whileToken = t) -> t
        | Expr.ForTo(forToken = t) -> t
        | Expr.ForIn(forToken = t) -> t
        | Expr.Match(matchToken = t) -> t
        | Expr.Function(functionToken = t) -> t
        | Expr.TryWith(tryToken = t) -> t
        | Expr.TryFinally(tryToken = t) -> t
        | Expr.Assignment(arrow = t) -> t
        | Expr.String(kind = kind) ->
            match kind with
            | StringKind.String t
            | StringKind.VerbatimString t
            | StringKind.String3 t
            | StringKind.InterpolatedString t
            | StringKind.VerbatimInterpolatedString t
            | StringKind.Interpolated3String t -> t
        | Expr.DotLookup(expr = inner) -> firstTokenOfExpr inner
        // The `?` operator token, so a chain `o?a?b` keys each node distinctly and
        // never collides with the receiver (the `InfixApp` operator-token rule).
        | Expr.DynamicLookup(questionMark = t) -> t
        // A generic-type / generic-value application's first token is the applied
        // expr's (`EqualityComparer` in `EqualityComparer<int>`). Reached when an
        // enclosing node (a static-member `DotLookup`) keys off its first token.
        | Expr.TypeApp(expr = inner) -> firstTokenOfExpr inner
        | Expr.Record(lBrace = pk) -> firstTokenOfParenKind pk
        | Expr.RecordClone(lBrace = pk) -> firstTokenOfParenKind pk
        | Expr.New(newToken = t) -> t
        | Expr.ILIntrinsic(lHashParen = t) -> t
        // The clause's `when` token, so chained clauses key distinctly (the
        // `InfixApp` operator-token rule).
        | Expr.LibraryOnlyStaticOptimization(whenToken = t) -> t
        // The `[` token, so a lookup never collides with its receiver (the
        // `InfixApp` operator-token rule).
        | Expr.IndexedLookup(lBracket = t) -> t
        // The invocation's opening `(` — unique to this node, so it never collides
        // with the inner argument expression.
        | Expr.StaticMemberInvocation(lParen = t) -> t
        | _ -> failwithf "CstKeys.firstTokenOfExpr: TODO %A" e

    let rec firstTokenOfPat (p: Pat<SyntaxToken>) : SyntaxToken =
        match p with
        | Pat.Const c -> firstTokenOfConstant c
        | Pat.NamedSimple t -> t
        | Pat.Named(longIdent = li) -> firstTokenOfLongIdent li
        | Pat.Wildcard t -> t
        | Pat.EnclosedBlock(lParen = pk) -> firstTokenOfParenKind pk
        | Pat.EmptyBlock(lParen = pk) -> firstTokenOfParenKind pk
        | Pat.Tuple(patterns = pats) when pats.Length > 0 -> firstTokenOfPat pats.[0]
        | Pat.Typed(pat = inner) -> firstTokenOfPat inner
        | Pat.Attributed(pat = inner) -> firstTokenOfPat inner
        | Pat.As(pat = inner) -> firstTokenOfPat inner
        | Pat.Or(left = inner) -> firstTokenOfPat inner
        | Pat.Record(lBrace = t) -> t
        | Pat.Op io -> firstTokenOfIdentOrOp io
        // The `::` token (the `InfixApp` operator-token rule).
        | Pat.Cons(consToken = t) -> t
        // The `:?` token, not the inner binder, so the test node never collides with
        // its inner sub-pattern (the operator-token rule). `TypeTest` is the bare
        // `:? T` with no binder.
        | Pat.TypeTestAs(colonQuestion = t) -> t
        | Pat.TypeTest(colonQuestion = t) -> t
        | Pat.Null t -> t
        | _ -> failwithf "CstKeys.firstTokenOfPat: TODO %A" p

    /// The leftmost token a `TypeName` retains: its first attribute set's `[<`, else the
    /// declared name (access modifier and prefix typars sit between the two, so the name
    /// is a safe floor — no reference can be WRITTEN in either).
    let private firstTokenOfTypeName (tn: TypeName<SyntaxToken>) : SyntaxToken voption =
        let (TypeName(attributes = attrs; ident = li)) = tn

        match attrs with
        | ValueSome sets when sets.Length > 0 ->
            let (AttributeSet(lBracket = lb)) = sets.[0]
            ValueSome lb
        | _ ->
            if li.Idents.Length > 0 then
                ValueSome li.Idents.[0]
            else
                ValueNone

    /// The leftmost token the CST retains for a type definition. The `type` / `and`
    /// keyword introducing it is consumed by the parser and NOT kept, so this is as far
    /// left as a declaration can be anchored — but it still lies strictly between that
    /// keyword and everything the declaration writes, and nothing can be written between
    /// the keyword and the type's attributes/name. That is exactly the precision the
    /// file-order visibility rule needs: a use above the `type` keyword is below this
    /// offset, and everything the declaration contains is above it.
    let tryFirstTokenOfTypeDefn (td: TypeDefn<SyntaxToken>) : SyntaxToken voption =
        match td with
        | TypeDefn.Abbrev(typeName = tn)
        | TypeDefn.Record(typeName = tn)
        | TypeDefn.Union(typeName = tn)
        | TypeDefn.Anon(typeName = tn)
        | TypeDefn.Class(typeName = tn)
        | TypeDefn.Struct(typeName = tn)
        | TypeDefn.Interface(typeName = tn)
        | TypeDefn.Enum(typeName = tn)
        | TypeDefn.Delegate(typeName = tn)
        | TypeDefn.TypeExtension(typeName = tn)
        | TypeDefn.AbstractType(typeName = tn) -> firstTokenOfTypeName tn
        | TypeDefn.Missing -> ValueNone
        | TypeDefn.SkipsTokens tokens ->
            if tokens.Length > 0 then
                ValueSome tokens.[0]
            else
                ValueNone

    /// The token a node's key is PROJECTED FROM — which is therefore where a diagnostic
    /// about the node points. NOT `firstTokenOfExpr`, and the difference is load-bearing:
    ///
    /// A dotted member access keys off its *member-name* token, not the receiver's first
    /// token. Chained accesses on a complex receiver (`T<x>.A.B`) are nested `DotLookup`s
    /// that all share the receiver's first token, so keying off `firstTokenOfExpr` would
    /// collide them onto one `NodeKey`. (A simple-head chain `r.A.B` is a single
    /// multi-segment LongIdent, not nested DotLookups, so it never reaches here.)
    ///
    /// A method-call application `recv.M(args)` whose head is a `DotLookup` keys off the
    /// same member token, for the same reason: a chained receiver `f.Invoke(a).Invoke(b)`
    /// (a call on the result of a call) would otherwise collide — the outer `App` and the
    /// inner `App` both keying off the leftmost `f`, so one application's inferred result
    /// overwrites the other's on one shared key. The member token is unique per call level.
    /// The `DotLookup` head itself stays distinct from the application by KIND.
    let diagTokenOfExpr (e: Expr<SyntaxToken>) : SyntaxToken =
        match e with
        | Expr.DotLookup(longIdentOrOp = lio)
        | Expr.App(funcExpr = Expr.DotLookup(longIdentOrOp = lio))
        | Expr.HighPrecedenceApp(funcExpr = Expr.DotLookup(longIdentOrOp = lio)) -> firstTokenOfLongIdentOrOp lio
        | _ -> firstTokenOfExpr e

    let private kindOfExpr (e: Expr<SyntaxToken>) : NodeKind =
        match e with
        | Expr.Const _ -> NodeKind.ExprConst
        | Expr.Ident _ -> NodeKind.ExprIdent
        | Expr.LongIdentOrOp _ -> NodeKind.ExprLongIdent
        | Expr.App _ -> NodeKind.ExprApp
        | Expr.InfixApp _ -> NodeKind.ExprInfixApp
        | Expr.PrefixApp _ -> NodeKind.ExprPrefixApp
        | Expr.Fun _ -> NodeKind.ExprLambda
        | Expr.LetOrUse _ -> NodeKind.ExprLet
        | Expr.EnclosedBlock _ -> NodeKind.ExprEnclosedBlock
        | Expr.IfThenElse _ -> NodeKind.ExprIfThenElse
        | Expr.Tuple _ -> NodeKind.ExprTuple
        | Expr.Sequential _ -> NodeKind.ExprSequential
        | Expr.TypeAnnotation _ -> NodeKind.ExprTypeAnnotation
        | Expr.StaticUpcast _ -> NodeKind.ExprStaticUpcast
        | Expr.DynamicTypeTest _ -> NodeKind.ExprDynamicTypeTest
        | Expr.DynamicDowncast _ -> NodeKind.ExprDynamicDowncast
        | Expr.EmptyBlock _ -> NodeKind.ExprEmptyBlock
        | Expr.While _ -> NodeKind.ExprWhile
        | Expr.ForTo _ -> NodeKind.ExprForTo
        | Expr.ForIn _ -> NodeKind.ExprForIn
        | Expr.String _ -> NodeKind.ExprString
        | Expr.Match _ -> NodeKind.ExprMatch
        | Expr.Function _ -> NodeKind.ExprFunction
        | Expr.TryWith _ -> NodeKind.ExprTryWith
        | Expr.TryFinally _ -> NodeKind.ExprTryFinally
        | Expr.Assignment _ -> NodeKind.ExprAssignment
        | Expr.HighPrecedenceApp _ -> NodeKind.ExprHighPrecApp
        | Expr.Range _ -> NodeKind.ExprRange
        | Expr.SteppedRange _ -> NodeKind.ExprSteppedRange
        | Expr.Null _ -> NodeKind.ExprNull
        | Expr.DotLookup _ -> NodeKind.ExprDotLookup
        | Expr.Record _ -> NodeKind.ExprRecord
        | Expr.RecordClone _ -> NodeKind.ExprRecordClone
        | Expr.New _ -> NodeKind.ExprNew
        | Expr.ILIntrinsic _ -> NodeKind.ExprILIntrinsic
        | Expr.LibraryOnlyStaticOptimization _ -> NodeKind.ExprStaticOptimization
        | Expr.IndexedLookup _ -> NodeKind.ExprIndexedLookup
        | Expr.StaticMemberInvocation _ -> NodeKind.ExprStaticMemberInvocation
        | _ -> NodeKind.Unknown

    /// A node's identity paired with the token it is projected from. THE way a pass names
    /// "this expression" once: `Key` files a side table, `Tok` places a diagnostic, and the
    /// two are one construction so they cannot come apart.
    let siteOfExpr (e: Expr<SyntaxToken>) : NodeSite =
        NodeSite.ofToken (kindOfExpr e) (diagTokenOfExpr e)

    let ofExpr (e: Expr<SyntaxToken>) : NodeKey = (siteOfExpr e).Key

    let siteOfPat (p: Pat<SyntaxToken>) : NodeSite =
        let kind =
            match p with
            | Pat.Const _ -> NodeKind.PatConst
            | Pat.NamedSimple _ -> NodeKind.PatIdent
            | Pat.Named _ -> NodeKind.PatLongIdent
            | Pat.Wildcard _ -> NodeKind.PatWildcard
            | Pat.EnclosedBlock _ -> NodeKind.PatEnclosedBlock
            | Pat.Tuple _ -> NodeKind.PatTuple
            | Pat.As _ -> NodeKind.PatAs
            | Pat.Typed _ -> NodeKind.PatTyped
            | Pat.Attributed _ -> NodeKind.PatAttributed
            | Pat.Or _ -> NodeKind.PatOr
            | Pat.EmptyBlock _ -> NodeKind.PatEmptyBlock
            | Pat.Record _ -> NodeKind.PatRecord
            | Pat.Op _ -> NodeKind.PatOp
            | Pat.Cons _ -> NodeKind.PatCons
            | Pat.TypeTestAs _ -> NodeKind.PatTypeTestAs
            | Pat.TypeTest _ -> NodeKind.PatTypeTest
            | Pat.Null _ -> NodeKind.PatNull
            | _ -> NodeKind.Unknown

        NodeSite.ofToken kind (firstTokenOfPat p)

    let ofPat (p: Pat<SyntaxToken>) : NodeKey = (siteOfPat p).Key

    /// A written external *type head* decomposed ONCE: its site (anchor key + the head
    /// token that spells it), the head long-ident, and its syntactic type-arg arity. Both
    /// faces of the resolve-once boundary go through `ofTypeHead` — NameResolution stamps
    /// `ResolvedTypeHead` on `Site.Key`, `Translate` reads that same key — so the write and
    /// read keys agree by construction rather than by two hand-spelled
    /// `NodeKey.ofToken … TypeNamed` derivations kept in sync by comment.
    [<NoEquality; NoComparison>]
    type TypeHead =
        {
            /// Anchored on the head's first ident token (`li.Idents.[0]`), kinded
            /// `TypeNamed` for a bare/dotted name or `TypeGeneric` for an applied one.
            Site: NodeSite
            /// The head's long-ident — its segments name the type, its first token is
            /// the anchor.
            LongIdent: LongIdent<SyntaxToken>
            /// Syntactic type-arg count, the arity both faces resolve at: `NamedType`
            /// ⇒ 0, `GenericType` ⇒ arg count, `SuffixedType` ⇒ 1 (postfix `'T list`).
            TyparArity: int
        }

    /// Decompose a `Type` node's *head*, when it has one. Only the three head-bearing
    /// shapes resolve to an external type: `NamedType` (a bare/dotted name), and
    /// `GenericType` / `SuffixedType` (a name applied to type args — `List<int>` /
    /// `int list`). Every non-head shape (`FunctionType`, `TupleType`, `VarType`, …)
    /// resolves structurally in `translateType` and never reaches the resolver, so it
    /// has no head — `ValueNone`.
    let ofTypeHead (ty: Type<SyntaxToken>) : TypeHead voption =
        match ty with
        | Type.NamedType li ->
            ValueSome
                {
                    Site = NodeSite.ofToken NodeKind.TypeNamed li.Idents.[0]
                    LongIdent = li
                    TyparArity = 0
                }
        | Type.GenericType(longIdent = li; typeArgs = args) ->
            ValueSome
                {
                    Site = NodeSite.ofToken NodeKind.TypeGeneric li.Idents.[0]
                    LongIdent = li
                    TyparArity = args.Length
                }
        | Type.SuffixedType(longIdent = li) ->
            ValueSome
                {
                    Site = NodeSite.ofToken NodeKind.TypeGeneric li.Idents.[0]
                    LongIdent = li
                    TyparArity = 1
                }
        | _ -> ValueNone

    /// The head SITE of a type that syntactically HAS a head — the total
    /// projection of `ofTypeHead` for a call site already inside a
    /// `NamedType`/`GenericType`/`SuffixedType` match arm, so the guarantee is
    /// local to the arm rather than a bare `.Value` whose crash would point
    /// nowhere. A headless shape here is an invariant break: fail with the shape.
    let typeHeadSite (ty: Type<SyntaxToken>) : NodeSite =
        match ofTypeHead ty with
        | ValueSome head -> head.Site
        | ValueNone -> failwithf "CstKeys.typeHeadSite: type node carries no resolvable head: %A" ty

    /// A binding's site is its headPat's — that's the pattern that introduced the
    /// name(s) being bound.
    let siteOfBinding (b: Binding<SyntaxToken>) : NodeSite = siteOfPat b.headPat

    let ofBinding (b: Binding<SyntaxToken>) : NodeKey = (siteOfBinding b).Key

    /// The loop variable of a `for i = …` introduces a binding whose site has
    /// no Pat wrapper in the CST — key on the ident token directly.
    let ofForToVar (ident: SyntaxToken) : NodeKey =
        NodeKey.ofToken ident NodeKind.PatForToVar
