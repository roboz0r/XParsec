namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

// A node wrapping an inner expr keys off its OWN operator/bracket token, so nesting the
// same kind (`x :> A :> B`, `o?a?b`, `f.Invoke(a).Invoke(b)`) cannot collide on one key.

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
        | Expr.DynamicLookup(questionMark = t) -> t
        | Expr.TypeApp(expr = inner) -> firstTokenOfExpr inner
        | Expr.Record(lBrace = pk) -> firstTokenOfParenKind pk
        | Expr.RecordClone(lBrace = pk) -> firstTokenOfParenKind pk
        | Expr.New(newToken = t) -> t
        | Expr.ILIntrinsic(lHashParen = t) -> t
        | Expr.LibraryOnlyStaticOptimization(clauses = clauses) when clauses.Length > 0 -> clauses.[0].WhenToken
        | Expr.IndexedLookup(lBracket = t) -> t
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
        | Pat.Cons(consToken = t) -> t
        | Pat.TypeTestAs(colonQuestion = t) -> t
        | Pat.TypeTest(colonQuestion = t) -> t
        | Pat.Null t -> t
        | _ -> failwithf "CstKeys.firstTokenOfPat: TODO %A" p

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

    /// The leftmost token the CST retains for a type definition — its attributes' `[<`,
    /// else the declared name. The `type` / `and` keyword itself is not kept, but nothing
    /// can be written between it and this token, so file-order visibility is exact here.
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

    /// The token a node's key is projected from, and so where its diagnostic points. A
    /// dotted access and a call on one take the MEMBER-NAME token: `T<x>.A.B` and
    /// `f.Invoke(a).Invoke(b)` nest, and every level shares the leftmost token.
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

    /// The written head of a type reference: `List` in `List<int>`, `int` in `int list`.
    [<NoEquality; NoComparison>]
    type TypeHead =
        {
            /// Anchored on the head's first ident token, kinded `TypeNamed` for a
            /// bare/dotted name or `TypeGeneric` for an applied one.
            Site: NodeSite
            LongIdent: LongIdent<SyntaxToken>
            /// Syntactic type-arg count: `NamedType` ⇒ 0, `GenericType` ⇒ arg count,
            /// `SuffixedType` ⇒ 1 (postfix `'T list`).
            TyparArity: int
        }

    /// Only `NamedType`, `GenericType` and `SuffixedType` name a type; the structural
    /// shapes (`FunctionType`, `TupleType`, `VarType`, …) have no head — `ValueNone`.
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

    /// `ofTypeHead` for a caller already inside a head-bearing arm, so the `ValueSome`
    /// is local to the arm; a headless shape here fails with the offending shape.
    let typeHeadSite (ty: Type<SyntaxToken>) : NodeSite =
        match ofTypeHead ty with
        | ValueSome head -> head.Site
        | ValueNone -> failwithf "CstKeys.typeHeadSite: type node carries no resolvable head: %A" ty

    let siteOfBinding (b: Binding<SyntaxToken>) : NodeSite = siteOfPat b.headPat

    let ofBinding (b: Binding<SyntaxToken>) : NodeKey = (siteOfBinding b).Key

    /// A `for i = …` loop variable binds with no Pat wrapper in the CST, so its site
    /// is keyed on the ident token directly.
    let ofForToVar (ident: SyntaxToken) : NodeKey =
        NodeKey.ofToken ident NodeKind.PatForToVar
