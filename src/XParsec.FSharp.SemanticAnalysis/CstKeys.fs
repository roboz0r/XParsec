namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
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

    let rec firstTokenOfMeasure (m: Measure<SyntaxToken>) : SyntaxToken =
        match m with
        | Measure.Named li -> firstTokenOfLongIdent li
        | Measure.One t
        | Measure.Anonymous t -> t
        | Measure.Typar(Typar.Anon t)
        | Measure.Typar(Typar.Named(quote = t))
        | Measure.Typar(Typar.Static(caret = t)) -> t
        | Measure.Juxtaposition(elems, _) -> firstTokenOfMeasure elems.[0]
        | Measure.Power(inner, _, _, _)
        | Measure.Product(inner, _, _)
        | Measure.Quotient(inner, _, _) -> firstTokenOfMeasure inner
        | Measure.Reciprocal(t, _)
        | Measure.Paren(t, _, _) -> t

    let typarToken (t: Typar<SyntaxToken>) : SyntaxToken voption =
        match t with
        | Typar.Named(ident = id)
        | Typar.Static(ident = id) -> ValueSome id
        | Typar.Anon _ -> ValueNone

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

    let firstTokenOfControlFlowKeyword (kw: ControlFlowKeyword<SyntaxToken>) : SyntaxToken =
        match kw with
        | ControlFlowKeyword.Yield t
        | ControlFlowKeyword.YieldBang t
        | ControlFlowKeyword.Return t
        | ControlFlowKeyword.ReturnBang t
        | ControlFlowKeyword.Do t
        | ControlFlowKeyword.DoBang t -> t

    /// The opening delimiter of a string literal, which precedes every part of it.
    let firstTokenOfStringKind (kind: StringKind<SyntaxToken>) : SyntaxToken =
        match kind with
        | StringKind.String t
        | StringKind.VerbatimString t
        | StringKind.String3 t
        | StringKind.InterpolatedString t
        | StringKind.VerbatimInterpolatedString t
        | StringKind.Interpolated3String t -> t

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
        | Expr.Tuple(exprs = exprs) ->
            match exprs.Length with
            | 0 -> failwithf "CstKeys.firstTokenOfExpr: tuple expression retains no token: %A" e
            | _ -> firstTokenOfExpr exprs.[0]
        | Expr.Sequential(exprs = exprs) ->
            match exprs.Length with
            | 0 -> failwithf "CstKeys.firstTokenOfExpr: sequential expression retains no token: %A" e
            | _ -> firstTokenOfExpr exprs.[0]
        | Expr.StructTuple(structToken = t) -> t
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
        | Expr.String(kind = kind) -> firstTokenOfStringKind kind
        | Expr.DotLookup(expr = inner) -> firstTokenOfExpr inner
        | Expr.DynamicLookup(questionMark = t) -> t
        | Expr.TypeApp(expr = inner) -> firstTokenOfExpr inner
        | Expr.Record(lBrace = pk) -> firstTokenOfParenKind pk
        | Expr.RecordClone(lBrace = pk) -> firstTokenOfParenKind pk
        | Expr.New(newToken = t) -> t
        | Expr.Object(lBrace = t) -> t
        | Expr.ILIntrinsic(lHashParen = t) -> t
        | Expr.LibraryOnlyStaticOptimization(defaultExpr = defaultExpr; clauses = clauses) ->
            match clauses.Length with
            | 0 -> firstTokenOfExpr defaultExpr
            | _ -> clauses.[0].WhenToken
        | Expr.IndexedLookup(lBracket = t) -> t
        | Expr.StaticMemberInvocation(lParen = t) -> t
        | Expr.OptionalArgExpr(questionMark = t) -> t
        | Expr.ControlFlow(keyword = kw) -> firstTokenOfControlFlowKeyword kw
        | Expr.Wildcard(underscore = t) -> t
        | Expr.Pat(pattern = inner) -> firstTokenOfPat inner
        | Expr.SliceFrom(expr = inner) -> firstTokenOfExpr inner
        | Expr.SliceTo(dotdot = t) -> t
        | Expr.SliceFromTo(startExpr = inner) -> firstTokenOfExpr inner
        | Expr.SliceAll(star = t) -> t
        | Expr.SkipsTokens tokens ->
            match tokens.Length with
            | 0 -> failwithf "CstKeys.firstTokenOfExpr: skipped-token expression retains no token: %A" e
            | _ -> tokens.[0]
        | Expr.Missing -> failwithf "CstKeys.firstTokenOfExpr: missing expression retains no token"

    and firstTokenOfPat (p: Pat<SyntaxToken>) : SyntaxToken =
        match p with
        | Pat.Const c -> firstTokenOfConstant c
        | Pat.NamedSimple t -> t
        | Pat.Named(longIdent = li) -> firstTokenOfLongIdent li
        | Pat.Wildcard t -> t
        | Pat.EnclosedBlock(lParen = pk) -> firstTokenOfParenKind pk
        | Pat.EmptyBlock(lParen = pk) -> firstTokenOfParenKind pk
        | Pat.Tuple(patterns = pats) ->
            match pats.Length with
            | 0 -> failwithf "CstKeys.firstTokenOfPat: tuple pattern retains no token: %A" p
            | _ -> firstTokenOfPat pats.[0]
        | Pat.StructTuple(structToken = t) -> t
        | Pat.Elems(pats = pats) ->
            match pats.Length with
            | 0 -> failwithf "CstKeys.firstTokenOfPat: element-list pattern retains no token: %A" p
            | _ -> firstTokenOfPat pats.[0]
        | Pat.Typed(pat = inner) -> firstTokenOfPat inner
        | Pat.Attributed(pat = inner) -> firstTokenOfPat inner
        | Pat.As(pat = inner) -> firstTokenOfPat inner
        | Pat.Or(left = inner) -> firstTokenOfPat inner
        | Pat.And(left = inner) -> firstTokenOfPat inner
        | Pat.Record(lBrace = t) -> t
        | Pat.Op io -> firstTokenOfIdentOrOp io
        | Pat.OpNamed(ident = io) -> firstTokenOfIdentOrOp io
        | Pat.NamedFieldPats(longIdent = li) -> firstTokenOfLongIdent li
        | Pat.Cons(consToken = t) -> t
        | Pat.TypeTestAs(colonQuestion = t) -> t
        | Pat.TypeTest(colonQuestion = t) -> t
        | Pat.Null t -> t
        | Pat.Optional(questionMark = t) -> t
        | Pat.String(kind = kind) -> firstTokenOfStringKind kind
        | Pat.Expr(expr = inner) -> firstTokenOfExpr inner
        | Pat.SkipsTokens tokens ->
            match tokens.Length with
            | 0 -> failwithf "CstKeys.firstTokenOfPat: skipped-token pattern retains no token: %A" p
            | _ -> tokens.[0]
        | Pat.Missing -> failwithf "CstKeys.firstTokenOfPat: missing pattern retains no token"

    /// The leftmost token a type header retains: its attributes' `[<`, else the declared name.
    let tryFirstTokenOfTypeName (tn: TypeName<SyntaxToken>) : SyntaxToken voption =
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

    /// The leftmost token the CST retains for a type definition: its attributes' `[<`,
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
        | TypeDefn.AbstractType(typeName = tn) -> tryFirstTokenOfTypeName tn
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

    /// The lone ident token of a one-segment expression. The parser produces `Ident x` and
    /// `LongIdent[x]` for the same source, so both forms are accepted; anything longer misses.
    let trySingleIdent (e: Expr<SyntaxToken>) : SyntaxToken voption =
        match e with
        | Expr.Ident t -> ValueSome t
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 -> ValueSome li.Idents.[0]
        | _ -> ValueNone

    [<return: Struct>]
    let (|SingleIdent|_|) (e: Expr<SyntaxToken>) : SyntaxToken voption = trySingleIdent e

    /// The items of a `[1; 2; 3]` / `[| 1; 2; 3 |]` body: the parser wraps two or more in
    /// `Expr.Sequential`, and a one-item literal is the item alone.
    let listLiteralItems (body: Expr<SyntaxToken>) : Expr<SyntaxToken> list =
        match body with
        | Expr.Sequential(exprs = items) -> [ for x in items -> x ]
        | single -> [ single ]

    /// The expression inside every grouping paren: `((e))`, `begin e end` and `e` all yield
    /// `e`.
    let rec ungroup (e: Expr<SyntaxToken>) : Expr<SyntaxToken> =
        match e with
        | Expr.EnclosedBlock(lParen = (ParenKind.Paren _ | ParenKind.BeginEnd _); expr = inner) -> ungroup inner
        | _ -> e

    /// The segment tokens of an identifier expression, `Ident x` and `LongIdent[x; …]`
    /// alike.
    [<return: Struct>]
    let (|IdentPath|_|) (e: Expr<SyntaxToken>) : ImmutableArray<SyntaxToken> voption =
        match e with
        | Expr.Ident t -> ValueSome(ImmutableArray.Create t)
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) -> ValueSome li.Idents
        | _ -> ValueNone

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

    /// The name a type reference APPLIES, with where it is written and at what arity:
    /// `List` in `List<int>`, `list` in `int list`, but never the argument.
    [<NoEquality; NoComparison>]
    type TypeRef =
        {
            /// Anchored on the name's first ident token, kinded `TypeNamed` for a
            /// bare/dotted name or `TypeGeneric` for an applied one.
            Site: NodeSite
            LongIdent: LongIdent<SyntaxToken>
            /// Syntactic type-arg count: `NamedType` ⇒ 0, `GenericType` ⇒ arg count,
            /// `SuffixedType` ⇒ 1 (postfix `'T list`).
            TyparArity: int
        }

    /// The reference a bare or dotted NAME applies, at arity 0, whether written as a type
    /// (`m` in `x: m`) or as a measure atom (`m` in `<m/s>`). Both key alike.
    let namedTypeRef (li: LongIdent<SyntaxToken>) : TypeRef =
        {
            Site = NodeSite.ofToken NodeKind.TypeNamed li.Idents.[0]
            LongIdent = li
            TyparArity = 0
        }

    /// The measure a type written in MEASURE position denotes. The parser produces a lone
    /// name (`float<m>`, `type v = m`) as `NamedType` and a lone typar (`float<'u>`) as
    /// `VarType`, because the measure grammar is retried only after an operator; a
    /// `MeasureType` is already a measure.
    let measureOfType (ty: Type<SyntaxToken>) : Measure<SyntaxToken> voption =
        match ty with
        | Type.MeasureType m -> ValueSome m
        | Type.NamedType li -> ValueSome(Measure.Named li)
        | Type.VarType tp -> ValueSome(Measure.Typar tp)
        | _ -> ValueNone

    /// Only `NamedType`, `GenericType` and `SuffixedType` name a type; the structural
    /// shapes (`FunctionType`, `TupleType`, `VarType`, …) apply none, so they yield `ValueNone`.
    let ofTypeRef (ty: Type<SyntaxToken>) : TypeRef voption =
        match ty with
        | Type.NamedType li -> ValueSome(namedTypeRef li)
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

    /// `ofTypeRef` for a caller already inside a name-bearing arm, so the `ValueSome`
    /// is local to the arm; a structural shape here fails with the offending shape.
    let typeRefSite (ty: Type<SyntaxToken>) : NodeSite =
        match ofTypeRef ty with
        | ValueSome typeRef -> typeRef.Site
        | ValueNone -> failwithf "CstKeys.typeRefSite: type node applies no type name: %A" ty

    let siteOfBinding (b: Binding<SyntaxToken>) : NodeSite = siteOfPat b.pattern

    let ofBinding (b: Binding<SyntaxToken>) : NodeKey = (siteOfBinding b).Key

    /// A `for i = …` loop variable binds with no Pat wrapper in the CST, so its site
    /// is keyed on the ident token directly.
    let ofForToVar (ident: SyntaxToken) : NodeKey =
        NodeKey.ofToken ident NodeKind.PatForToVar
