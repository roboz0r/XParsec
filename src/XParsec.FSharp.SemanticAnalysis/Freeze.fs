namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

// The only tree-to-tree transformation in the pipeline. Every previous pass
// annotated via side tables; freeze projects those annotations into a fresh
// TAST.
//
// Side tables can be discarded after this returns. The TAST is sharable;
// the CST + side tables are scoped to one compilation.

module Freeze =

    let private typeOfKey (ctx: PassContext) (key: NodeKey) : SemType =
        match ctx.TypeVar.TryGetValue key with
        | ValueSome tv -> Unification.zonk (TyVar tv)
        | ValueNone -> TyVar(TypeVar())

    /// Strip type/format suffixes the lexer leaves on a numeric token (e.g.
    /// `42L` -> "42", `0xFFuy` -> "0xFF", `1.5f` -> "1.5"). We only strip the
    /// suffixes that map to literal kinds Unification recognises; the
    /// remainder is fed to the corresponding BCL parser.
    let private stripSuffix (suffix: string) (text: string) =
        if text.EndsWith(suffix, System.StringComparison.OrdinalIgnoreCase) then
            text.Substring(0, text.Length - suffix.Length)
        else
            text

    let private parseConst (ctx: PassContext) (c: Constant<SyntaxToken>) : TConstValue =
        let parseLiteral (t: SyntaxToken) : TConstValue =
            let text = ctx.NameOf t

            match t.Token with
            | Token.KWTrue -> TConstValue.Bool true
            | Token.KWFalse -> TConstValue.Bool false
            | Token.NumIEEE64
            | Token.NumIEEE64Hex
            | Token.NumIEEE64Octal
            | Token.NumIEEE64Binary ->
                TConstValue.Float(System.Double.Parse(text, System.Globalization.CultureInfo.InvariantCulture))
            | Token.NumInt64
            | Token.NumInt64Hex
            | Token.NumInt64Octal
            | Token.NumInt64Binary -> TConstValue.Int64(System.Int64.Parse(stripSuffix "L" text))
            | Token.NumByte
            | Token.NumByteHex
            | Token.NumByteOctal
            | Token.NumByteBinary -> TConstValue.Byte(System.Byte.Parse(stripSuffix "uy" text))
            | _ ->
                // Falls through for NumInt32 family and anything Unification
                // hasn't classified — they're treated as plain ints.
                TConstValue.Int(Int32.Parse text)

        match c with
        | Constant.Literal t -> parseLiteral t
        | Constant.MeasuredLiteral(value = t) -> parseLiteral t

    /// Walk a CST pattern into a TPat. EnclosedBlock and Typed peel; Tuple
    /// recurses. Patterns Unification doesn't understand yet fall through
    /// loudly so the gap surfaces at translation time.
    let rec private translatePat (ctx: PassContext) (p: Pat<SyntaxToken>) : TPat =
        let key = CstKeys.ofPat p
        let ty = typeOfKey ctx key

        match p with
        | Pat.NamedSimple t when
            let n = ctx.NameOf t
            n.Length > 0 && System.Char.IsUpper n.[0] && ctx.CtorIndex.ContainsKey n
            ->
            // Nullary ctor in pattern position — reinterpret as a ctor
            // pattern that binds nothing. Must precede the plain
            // NamedSimple arm.
            TPat.Union(ctx.NameOf t, [], ty)
        | Pat.NamedSimple _ -> TPat.NamedSimple(key, ty)
        | Pat.Wildcard _ -> TPat.Wildcard ty
        | Pat.EnclosedBlock(pat = inner) -> translatePat ctx inner
        | Pat.Tuple(patterns = pats) -> TPat.Tuple([ for sub in pats -> translatePat ctx sub ], ty)
        | Pat.Const c -> TPat.Const(parseConst ctx c, ty)
        | Pat.As(pat = inner) ->
            // The `as`-name itself isn't surfaced in TPat yet — translate the
            // inner pattern and rely on downstream Var lookups to find the
            // alias via the CST + side tables.
            translatePat ctx inner
        | Pat.Typed(pat = inner) ->
            // Annotation is consumed by Unification; runtime shape is the
            // inner pattern.
            translatePat ctx inner
        | Pat.Or(left = leftPat) ->
            // Both sides must bind the same names with matching types
            // (Validation's job). Pick the left side for shape — until or-
            // patterns are first-class in TPat, downstream consumers see
            // only one arm of the alternation.
            translatePat ctx leftPat
        | Pat.EmptyBlock _ -> TPat.Const(TConstValue.Unit, ty)
        | Pat.Record(fieldPats = fieldPats) ->
            let fields =
                [
                    for FieldPat(longIdent = li; pat = sub) in fieldPats ->
                        let idents = li.Idents
                        ctx.NameOf idents.[idents.Length - 1], translatePat ctx sub
                ]

            TPat.Record(fields, ty)
        | Pat.Named(longIdent = li; argumentPats = args) when
            li.Idents.Length >= 1
            && (let last = ctx.NameOf li.Idents.[li.Idents.Length - 1]

                last.Length > 0
                && System.Char.IsUpper last.[0]
                && (li.Idents.Length = 1 && ctx.CtorIndex.ContainsKey last
                    || li.Idents.Length = 2 && ctx.UnionTypes.ContainsKey(ctx.NameOf li.Idents.[0])))
            ->
            let caseName = ctx.NameOf li.Idents.[li.Idents.Length - 1]

            let subPats =
                if args.Length = 1 then
                    // Strip an `EnclosedBlock(Tuple [...])` or `Tuple [...]`
                    // wrapper for multi-field ctor patterns.
                    match args.[0] with
                    | Pat.EnclosedBlock(pat = Pat.Tuple(patterns = pats)) -> [ for sub in pats -> translatePat ctx sub ]
                    | Pat.EnclosedBlock(pat = inner) -> [ translatePat ctx inner ]
                    | Pat.Tuple(patterns = pats) -> [ for sub in pats -> translatePat ctx sub ]
                    | sub -> [ translatePat ctx sub ]
                else
                    [ for sub in args -> translatePat ctx sub ]

            TPat.Union(caseName, subPats, ty)
        | _ -> failwithf "Freeze.translatePat: TODO %A" p

    /// `()` literal. Distinct entry point because `Expr.EmptyBlock` carries
    /// `ParenKind` + closing token, not a `Constant`.
    let private unitConst (ctx: PassContext) (e: Expr<SyntaxToken>) : TExpr =
        let key = CstKeys.ofExpr e
        TExpr.Const(TConstValue.Unit, typeOfKey ctx key)

    /// Try to interpret `e` as a DU ctor reference and return the case
    /// name. Handles single-segment `Circle`, two-segment `Result2.Ok`,
    /// and either inside an `Expr.Ident` or `Expr.LongIdentOrOp`. Returns
    /// `ValueNone` for anything else (including local bindings whose
    /// names happen to match a ctor — they have a `Binding` entry).
    let private tryCtorRef (ctx: PassContext) (e: Expr<SyntaxToken>) : string voption =
        let key = CstKeys.ofExpr e

        if ctx.Binding.ContainsKey key then
            ValueNone
        else
            match e with
            | Expr.Ident t ->
                let n = ctx.NameOf t

                if ctx.CtorIndex.ContainsKey n then
                    ValueSome n
                else
                    ValueNone
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                let n = ctx.NameOf li.Idents.[0]

                if ctx.CtorIndex.ContainsKey n then
                    ValueSome n
                else
                    ValueNone
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
                li.Idents.Length = 2 && ctx.UnionTypes.ContainsKey(ctx.NameOf li.Idents.[0])
                ->
                let typeName = ctx.NameOf li.Idents.[0]
                let caseName = ctx.NameOf li.Idents.[1]
                let info = ctx.UnionTypes.[typeName]

                if info.Cases |> Array.exists (fun c -> c.Name = caseName) then
                    ValueSome caseName
                else
                    ValueNone
            | _ -> ValueNone

    let rec private translateExpr (ctx: PassContext) (e: Expr<SyntaxToken>) : TExpr =
        let key = CstKeys.ofExpr e
        let ty = typeOfKey ctx key

        match e with
        | Expr.Const c -> TExpr.Const(parseConst ctx c, ty)
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length > 1
            && ctx.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent)
            ->
            // `r.X` (or chained `r.X.Y`) parsed as a single multi-segment
            // LongIdent. The head segment was resolved by NameResolution
            // as a local binding; subsequent segments are field accesses.
            translateLongIdentFieldChain ctx li ty
        | _ when (tryCtorRef ctx e).IsSome ->
            // Bare or qualified ctor reference outside an App. Nullary
            // ctors translate as `UnionCons(name, [], ty)`; ctor-as-value
            // (`let f = Circle`) types as `TyFun(_, TyUnion _)` — in that
            // case the TAST node is still a UnionCons-like reference, but
            // we fall through to a function-typed External (lowering can
            // eta-expand if needed). v1 distinguishes by the result type.
            match Unification.zonk ty with
            | TyUnion(_, _) ->
                let caseName = (tryCtorRef ctx e).Value
                TExpr.UnionCons(caseName, [], ty)
            | _ ->
                // Function-typed ctor reference (ctor-as-value). Emit a
                // External — codegen can eta-expand to a UnionCons lambda.
                let caseName = (tryCtorRef ctx e).Value
                TExpr.External(caseName, ty)
        | Expr.Ident _
        | Expr.LongIdentOrOp _ -> translateIdent ctx e key ty
        | Expr.App(fn, args) when (tryCtorRef ctx fn).IsSome ->
            // Ctor application: `Circle 1.0` or `Rectangle(2.0, 3.0)`.
            // The parser hands `Rectangle(2.0, 3.0)` as `Expr.App` with a
            // single `Expr.EnclosedBlock(Tuple)` argument; F# treats DU
            // arguments as a single tuple, but the TAST flattens it back
            // to a per-field list so consumers see the ctor's declared
            // arity directly.
            let caseName = (tryCtorRef ctx fn).Value

            let argsList =
                if args.Length = 1 then
                    match args.[0] with
                    | Expr.EnclosedBlock(expr = Expr.Tuple(exprs = items)) -> [ for a in items -> translateExpr ctx a ]
                    | Expr.Tuple(exprs = items) -> [ for a in items -> translateExpr ctx a ]
                    | a -> [ translateExpr ctx a ]
                else
                    [ for a in args -> translateExpr ctx a ]

            TExpr.UnionCons(caseName, argsList, ty)
        | Expr.HighPrecedenceApp(funcExpr = fn; argExpr = arg) when (tryCtorRef ctx fn).IsSome ->
            let caseName = (tryCtorRef ctx fn).Value

            let argsList =
                match arg with
                | Expr.EnclosedBlock(expr = Expr.Tuple(exprs = items)) -> [ for a in items -> translateExpr ctx a ]
                | Expr.Tuple(exprs = items) -> [ for a in items -> translateExpr ctx a ]
                | a -> [ translateExpr ctx a ]

            TExpr.UnionCons(caseName, argsList, ty)
        | Expr.App(fn, args) -> translateApp ctx fn args
        | Expr.HighPrecedenceApp(funcExpr = fn; argExpr = arg) ->
            TExpr.App(translateExpr ctx fn, translateExpr ctx arg, ty)
        | Expr.InfixApp(left, _, right) -> translateInfix ctx key left right ty
        | Expr.PrefixApp(_, operand) -> translatePrefix ctx key operand ty
        | Expr.Fun(argumentPats = argPats; expr = body) -> translateFun ctx argPats body
        | Expr.LetOrUse(bindings = bindings; body = body) -> translateLet ctx bindings body
        | Expr.EnclosedBlock(expr = inner) -> translateExpr ctx inner
        | Expr.IfThenElse(condition = cond; thenExpr = thenE; elifBranches = elifs; elseBranch = elseB) ->
            translateIfThenElse ctx cond thenE elifs elseB ty
        | Expr.Tuple(exprs = items) -> TExpr.Tuple([ for x in items -> translateExpr ctx x ], ty)
        | Expr.Sequential(exprs = items) -> TExpr.Sequential([ for x in items -> translateExpr ctx x ], ty)
        // The annotation only constrained types in Unification; the TAST
        // carries the inferred type inline, so the annotation node has no
        // runtime representation — return the (now-constrained) inner.
        | Expr.TypeAnnotation(expr = inner) -> translateExpr ctx inner
        | Expr.EmptyBlock _ -> unitConst ctx e
        | Expr.While(condition = cond; body = body) -> TExpr.While(translateExpr ctx cond, translateExpr ctx body, ty)
        | Expr.ForTo(ident = ident; startExpr = startE; endExpr = endE; body = body) ->
            let varKey = CstKeys.ofForToVar ident
            TExpr.ForTo(varKey, translateExpr ctx startE, translateExpr ctx endE, translateExpr ctx body, ty)
        | Expr.ForIn(pat = pat; enumerableExpr = src; body = body) ->
            TExpr.ForIn(translatePat ctx pat, translateExpr ctx src, translateExpr ctx body, ty)
        | Expr.String _ -> translateString ctx e ty
        | Expr.Match(matchExpr = scrutinee; rules = Rules(rules = rules)) ->
            TExpr.Match(translateExpr ctx scrutinee, translateRules ctx rules, ty)
        | Expr.Function(rules = Rules(rules = rules)) ->
            // `function …` ~ `fun x -> match x with …`. The synthesised
            // parameter has no source token, so mint a synthetic key under
            // the function-keyword's offset and let the Match scrutinee
            // reference it.
            let funcKey = CstKeys.ofExpr e

            let paramKey = NodeKey.ofSynthetic funcKey.Offset NodeKind.SynthLambdaBody

            let paramTy, resultTy =
                match ty with
                | TyFun(p, r) -> p, r
                | _ -> failwithf "Freeze.Function: expected function type, got %A" ty

            let scrutinee = TExpr.Var(paramKey, paramTy)
            let body = TExpr.Match(scrutinee, translateRules ctx rules, resultTy)
            TExpr.Lambda(TPat.NamedSimple(paramKey, paramTy), body, ty)
        | Expr.TryWith(expr = body; rules = Rules(rules = rules)) ->
            TExpr.TryWith(translateExpr ctx body, translateRules ctx rules, ty)
        | Expr.TryFinally(tryExpr = body; finallyExpr = finallyE) ->
            TExpr.TryFinally(translateExpr ctx body, translateExpr ctx finallyE, ty)
        | Expr.Assignment(leftExpr = left; rightExpr = right) ->
            // `r.X <- v` folds to FieldSet; everything else falls through
            // to Assignment.
            let unwrapped =
                let rec unwrap e =
                    match e with
                    | Expr.EnclosedBlock(expr = inner)
                    | Expr.TypeAnnotation(expr = inner) -> unwrap inner
                    | _ -> e

                unwrap left

            match unwrapped with
            | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                let fieldName = ctx.NameOf li.Idents.[0]
                TExpr.FieldSet(translateExpr ctx r, fieldName, translateExpr ctx right, ty)
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
                li.Idents.Length > 1
                && ctx.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent)
                ->
                // `r.X <- v` parsed as Assignment(LongIdent[r;X], <-, v).
                // The head-resolved chain peels into FieldGet for the
                // intermediate segments and a final FieldSet for the
                // assigned slot.
                let receiverIdents = li.Idents
                let lastIdx = receiverIdents.Length - 1

                let receiverChain =
                    // Translate everything up to the last segment as a
                    // FieldGet chain; the last segment becomes FieldSet.
                    let head = receiverIdents.[0]
                    let headKey = NodeKey.ofToken head NodeKind.ExprIdent

                    let headBinding = ctx.Binding.TryGetValue headKey

                    let headTy =
                        match headBinding with
                        | ValueSome rb -> typeOfKey ctx rb.BindingSite
                        | ValueNone -> typeOfKey ctx (CstKeys.ofExpr unwrapped)

                    let headExpr =
                        match headBinding with
                        | ValueSome rb -> TExpr.Var(rb.BindingSite, headTy)
                        | ValueNone -> TExpr.External(ctx.NameOf head, headTy)

                    let mutable curr = headExpr
                    let mutable currTy = headTy

                    for i = 1 to lastIdx - 1 do
                        let seg = receiverIdents.[i]
                        let segName = ctx.NameOf seg

                        let stepTy =
                            match Unification.zonk currTy with
                            | TyRecord(recName, args) ->
                                match ctx.RecordTypes.TryGetValue recName with
                                | true, info ->
                                    match info.Fields |> Array.tryFind (fun f -> f.Name = segName) with
                                    | Some field ->
                                        let subst = Unification.mkNamedTypeSubst info.TypeParams args
                                        Unification.zonk (Unification.substituteWith subst field.Type)
                                    | None -> currTy
                                | false, _ -> currTy
                            | _ -> currTy

                        curr <- TExpr.FieldGet(curr, segName, stepTy)
                        currTy <- stepTy

                    curr

                let lastName = ctx.NameOf receiverIdents.[lastIdx]
                TExpr.FieldSet(receiverChain, lastName, translateExpr ctx right, ty)
            | _ -> TExpr.Assignment(translateExpr ctx left, translateExpr ctx right, ty)
        | Expr.Record(fieldInitializers = inits) ->
            let fields =
                [
                    for FieldInitializer(longIdent = li; expr = e) in inits ->
                        let idents = li.Idents
                        ctx.NameOf idents.[idents.Length - 1], translateExpr ctx e
                ]

            TExpr.RecordCons(fields, ty)
        | Expr.RecordClone(expr = src; fieldInitializers = inits) ->
            let overrides =
                [
                    for FieldInitializer(longIdent = li; expr = e) in inits ->
                        let idents = li.Idents
                        ctx.NameOf idents.[idents.Length - 1], translateExpr ctx e
                ]

            TExpr.RecordClone(translateExpr ctx src, overrides, ty)
        | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
            let fieldName = ctx.NameOf li.Idents.[0]
            TExpr.FieldGet(translateExpr ctx r, fieldName, ty)
        | Expr.Null _ -> TExpr.Null ty
        | Expr.Range(fromExpr = a; toExpr = b) -> TExpr.Range(translateExpr ctx a, None, translateExpr ctx b, ty)
        | Expr.SteppedRange(fromExpr = a; stepExpr = s; toExpr = b) ->
            TExpr.Range(translateExpr ctx a, Some(translateExpr ctx s), translateExpr ctx b, ty)
        | _ ->
            // TODO: extend as the subset grows. Until then, surface the
            // unhandled case loudly rather than emitting a broken TExpr.
            failwithf "Freeze.translateExpr: TODO %A" e

    and private translateRules (ctx: PassContext) (rules: ImmutableArray<Rule<SyntaxToken>>) : TMatchArm list =
        [
            for r in rules do
                match r with
                | Rule.Rule(pat = pat; guard = guard; expr = body) ->
                    let guardT =
                        match guard with
                        | ValueSome(PatternGuard(expr = g)) -> Some(translateExpr ctx g)
                        | ValueNone -> None

                    yield
                        {
                            Pat = translatePat ctx pat
                            Guard = guardT
                            Body = translateExpr ctx body
                        }
                | _ -> ()
        ]

    and private translateString (ctx: PassContext) (e: Expr<SyntaxToken>) (ty: SemType) : TExpr =
        // Tiny subset: stitch together the source text of every Text /
        // EscapeSequence / VerbatimEscapeQuote part. Interpolation holes are
        // not yet rendered — surface them as `{<expr>}` placeholders so test
        // output stays deterministic.
        match e with
        | Expr.String(parts = parts) ->
            let sb = System.Text.StringBuilder()

            for part in parts do
                match part with
                | StringPart.Text t
                | StringPart.EscapeSequence t
                | StringPart.FormatSpecifier t
                | StringPart.EscapePercent t
                | StringPart.VerbatimEscapeQuote t -> sb.Append(ctx.NameOf t) |> ignore
                | StringPart.Expr _ -> sb.Append("{<expr>}") |> ignore
                | StringPart.OrphanFormatSpecifier t -> sb.Append(ctx.NameOf t) |> ignore
                | StringPart.InvalidText t -> sb.Append(ctx.NameOf t) |> ignore

            TExpr.Const(TConstValue.String(sb.ToString()), ty)
        | _ -> failwithf "Freeze.translateString: not a String expr: %A" e

    and private translateIdent (ctx: PassContext) (e: Expr<SyntaxToken>) (key: NodeKey) (ty: SemType) : TExpr =
        match ctx.Binding.TryGetValue key with
        | ValueSome rb -> TExpr.Var(rb.BindingSite, ty)
        | ValueNone ->
            // No Binding entry => NameResolution resolved through the
            // provider. Re-query for the name. Multi-segment qualified
            // names are joined with `.` so `External` carries the same key
            // the provider sees.
            let name =
                match e with
                | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) -> li.Idents |> Seq.map ctx.NameOf |> String.concat "."
                | _ -> ctx.NameOf(CstKeys.firstTokenOfExpr e)

            TExpr.External(name, ty)

    /// Fold a multi-segment `r.X.Y…` LongIdent into nested `FieldGet`
    /// nodes. The head segment's TAST node is a `Var` pointing back at the
    /// local binding; each subsequent segment unwraps one field.
    and private translateLongIdentFieldChain
        (ctx: PassContext)
        (li: LongIdent<SyntaxToken>)
        (finalTy: SemType)
        : TExpr =
        let head = li.Idents.[0]
        let headKey = NodeKey.ofToken head NodeKind.ExprIdent
        let headBinding = ctx.Binding.TryGetValue headKey

        let headTy =
            // The head's TyVar lives under the same key NameResolution
            // wrote — but Unification didn't allocate a Side-table entry
            // for the synthetic head key, so fall back to the binding
            // site's TyVar.
            match headBinding with
            | ValueSome rb -> typeOfKey ctx rb.BindingSite
            | ValueNone -> finalTy

        let headExpr =
            match headBinding with
            | ValueSome rb -> TExpr.Var(rb.BindingSite, headTy)
            | ValueNone -> TExpr.External(ctx.NameOf head, headTy)

        let mutable currTy = headTy
        let mutable curr = headExpr

        for i = 1 to li.Idents.Length - 1 do
            let seg = li.Idents.[i]
            let segName = ctx.NameOf seg
            // Walk through TyRecord to find the field's declared type for
            // each intermediate step; the last step uses finalTy.
            let stepTy =
                if i = li.Idents.Length - 1 then
                    finalTy
                else
                    match Unification.zonk currTy with
                    | TyRecord(recName, args) ->
                        match ctx.RecordTypes.TryGetValue recName with
                        | true, info ->
                            match info.Fields |> Array.tryFind (fun f -> f.Name = segName) with
                            | Some field ->
                                let subst = Unification.mkNamedTypeSubst info.TypeParams args
                                Unification.zonk (Unification.substituteWith subst field.Type)
                            | None -> finalTy
                        | false, _ -> finalTy
                    | _ -> finalTy

            curr <- TExpr.FieldGet(curr, segName, stepTy)
            currTy <- stepTy

        curr

    and private translateApp
        (ctx: PassContext)
        (fn: Expr<SyntaxToken>)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        : TExpr =
        let mutable result = translateExpr ctx fn
        let mutable currTy = typeOfKey ctx (CstKeys.ofExpr fn)

        for a in args do
            let argT = translateExpr ctx a

            let resTy =
                match currTy with
                | TyFun(_, r) -> r
                | _ ->
                    failwithf
                        "Freeze.translateApp: expected function type for application, got %A (Unification bug or free TypeVar)"
                        currTy

            result <- TExpr.App(result, argT, resTy)
            currTy <- resTy

        result

    and private translateInfix
        (ctx: PassContext)
        (key: NodeKey)
        (left: Expr<SyntaxToken>)
        (right: Expr<SyntaxToken>)
        (resultTy: SemType)
        : TExpr =
        match ctx.Desugared.TryGetValue key with
        | ValueSome(DesugaredForm.OpName name) ->
            // Reconstruct the operator's type from the known arms instead
            // of re-instantiating the scheme. Re-instantiation would mint
            // fresh TypeVars that the existing TyVar table doesn't link
            // anywhere, so the External's carried type would not match the
            // App chain's resolved arms.
            let leftTy = typeOfKey ctx (CstKeys.ofExpr left)
            let rightTy = typeOfKey ctx (CstKeys.ofExpr right)
            let partialTy = TyFun(rightTy, resultTy)
            let opTy = TyFun(leftTy, partialTy)
            let opExpr = TExpr.External(name, opTy)
            let app1 = TExpr.App(opExpr, translateExpr ctx left, partialTy)
            TExpr.App(app1, translateExpr ctx right, resultTy)
        | ValueNone ->
            // Either Desugar didn't recognise the operator (bug) or the
            // InfixApp is malformed. Surface loudly.
            failwithf "Freeze: InfixApp at %O missing DesugaredForm entry" key

    and private translatePrefix
        (ctx: PassContext)
        (key: NodeKey)
        (operand: Expr<SyntaxToken>)
        (resultTy: SemType)
        : TExpr =
        match ctx.Desugared.TryGetValue key with
        | ValueSome(DesugaredForm.OpName name) ->
            // See translateInfix: reconstruct the operator's type from the
            // resolved operand + result rather than re-instantiating the
            // scheme.
            let operandTy = typeOfKey ctx (CstKeys.ofExpr operand)
            let opTy = TyFun(operandTy, resultTy)
            let opExpr = TExpr.External(name, opTy)
            TExpr.App(opExpr, translateExpr ctx operand, resultTy)
        | ValueNone -> failwithf "Freeze: PrefixApp at %O missing DesugaredForm entry" key

    and private translateIfThenElse
        (ctx: PassContext)
        (cond: Expr<SyntaxToken>)
        (thenE: Expr<SyntaxToken>)
        (elifs: ImmutableArray<ElifBranch<SyntaxToken>>)
        (elseB: ElseBranch<SyntaxToken> voption)
        (resultTy: SemType)
        : TExpr =
        let elseExpr =
            match elseB with
            | ValueSome(ElseBranch(expr = e)) -> e
            | ValueNone -> failwith "Freeze: if-then without else not yet supported"

        // Fold elifs right-to-left, nesting each as the else-branch of the
        // previous. Result is `if cond then thenE else (if c1 then e1 else (… else elseExpr))`.
        let mutable nestedElse = translateExpr ctx elseExpr

        for i = elifs.Length - 1 downto 0 do
            let elifCond, elifThen =
                match elifs.[i] with
                | ElifBranch.Elif(condition = c; expr = e)
                | ElifBranch.ElseIf(condition = c; expr = e) -> c, e

            nestedElse <- TExpr.IfThenElse(translateExpr ctx elifCond, translateExpr ctx elifThen, nestedElse, resultTy)

        TExpr.IfThenElse(translateExpr ctx cond, translateExpr ctx thenE, nestedElse, resultTy)

    and private translateFun
        (ctx: PassContext)
        (argPats: ImmutableArray<Pat<SyntaxToken>>)
        (body: Expr<SyntaxToken>)
        : TExpr =
        let mutable result = translateExpr ctx body
        let mutable resultTy = typeOfKey ctx (CstKeys.ofExpr body)

        for i = argPats.Length - 1 downto 0 do
            let p = argPats.[i]
            let tpat = translatePat ctx p
            let pTy = typeOfKey ctx (CstKeys.ofPat p)
            let lamTy = TyFun(pTy, resultTy)
            result <- TExpr.Lambda(tpat, result, lamTy)
            resultTy <- lamTy

        result

    and private translateLet
        (ctx: PassContext)
        (bindings: ImmutableArray<Binding<SyntaxToken>>)
        (body: Expr<SyntaxToken> voption)
        : TExpr =
        match body with
        | ValueSome bodyExpr ->
            let mutable result = translateExpr ctx bodyExpr
            let mutable resultTy = typeOfKey ctx (CstKeys.ofExpr bodyExpr)

            for i = bindings.Length - 1 downto 0 do
                let b = bindings.[i]
                let tpat = translatePat ctx b.headPat
                let valT = translateBinding ctx b
                result <- TExpr.Let(tpat, valT, result, resultTy)

            result
        | ValueNone ->
            // `Expr.LetOrUse(body = ValueNone)` is `use fixed`, which the
            // tiny subset doesn't support. Surface loudly when it appears.
            failwith "Freeze: Expr.LetOrUse with no body (UseFixed) not supported"

    and private translateBinding (ctx: PassContext) (b: Binding<SyntaxToken>) : TExpr =
        if b.argumentPats.IsEmpty then
            translateExpr ctx b.expr
        else
            // `let f x y = body` is `let f = fun x y -> body`.
            translateFun ctx b.argumentPats b.expr

    let private translateModuleElem (ctx: PassContext) (m: ModuleElem<SyntaxToken>) : TDecl list =
        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
            [
                for b in bindings ->
                    let tpat = translatePat ctx b.headPat
                    let valT = translateBinding ctx b
                    TDecl.Let(tpat, valT, typeOfKey ctx (CstKeys.ofBinding b))
            ]
        | ModuleElem.Expression e ->
            let eT = translateExpr ctx e
            [ TDecl.Expression(eT, typeOfKey ctx (CstKeys.ofExpr e)) ]
        | _ -> []

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : TastFile =
        let decls =
            match file with
            | ImplementationFile.AnonymousModule elems -> elems |> Seq.collect (translateModuleElem ctx) |> List.ofSeq
            | ImplementationFile.NamedModule(NamedModule.NamedModule(elements = elems)) ->
                elems |> Seq.collect (translateModuleElem ctx) |> List.ofSeq
            | ImplementationFile.Namespaces _ -> []

        {
            Decls = decls
            Diagnostics = List.ofSeq ctx.Diagnostics
        }
