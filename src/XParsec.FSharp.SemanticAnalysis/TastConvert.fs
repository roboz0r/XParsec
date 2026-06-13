namespace XParsec.FSharp.SemanticAnalysis

// Cross-type structural rebuild of the TAST term/declaration cluster: maps every
// embedded `.ty` field through `f`, producing a tree at a *different* type
// parameter (`TExprG<'a> -> TExprG<'b>`). This is the engine behind the genuine
// freeze (`Freeze.run = TastConvert.file toFrozen`) and its
// inverse bridge (`TastConvert.file ofFrozen`).
//
// Distinct from `TastWalk`, whose `Mapper` is same-`'ty` (it rewrites a tree in
// place with override hooks); this changes the type parameter and has no hooks, so
// it is a separate, total functor. F#'s incomplete-match check fires here when the
// TAST grows a case — the same enumeration guarantee `TastWalk` gives.
//
// Non-`'ty` payload is copied verbatim: NodeKey / SymbolKey / CallVia / TConstValue
// / TMemberKind / PrintfSpec.HoleKind / the verdict fields / the
// side maps, AND two fields that are deliberately not `'ty`-typed —
// `TTypeMemberG.MethodTypeParams : EqArray<string * TypeVar>` (its `TypeVar` roots
// only feed the GenericParam row names + arity post-freeze; the body's open typars
// already rode `TyTypar`) and `TStaticOptClauseG.Constraints :
// EqArray<TStaticOptConstraint>` (no `StaticOptimization` survives the inline pass,
// so these never reach the frozen tree in practice).

[<RequireQualifiedAccess>]
module TastConvert =

    let rec pat (f: 'a -> 'b) (p: TPatG<'a>) : TPatG<'b> =
        match p with
        | TPatG.NamedSimple(k, ty) -> TPatG.NamedSimple(k, f ty)
        | TPatG.Wildcard ty -> TPatG.Wildcard(f ty)
        | TPatG.Const(v, ty) -> TPatG.Const(v, f ty)
        | TPatG.Tuple(items, ty) -> TPatG.Tuple(EqArray.map (pat f) items, f ty)
        | TPatG.Record(fields, ty) -> TPatG.Record(EqArray.map (fun (n, sub) -> n, pat f sub) fields, f ty)
        | TPatG.Union(c, fields, ty) -> TPatG.Union(c, EqArray.map (pat f) fields, f ty)
        | TPatG.TypeTestAs(testTy, inner, ty) -> TPatG.TypeTestAs(f testTy, pat f inner, f ty)

    let hole (f: 'a -> 'b) (h: HoleSpecG<'a>) : HoleSpecG<'b> =
        {
            Ty = f h.Ty
            Kind = h.Kind
            Format = h.Format
            Alignment = h.Alignment
        }

    let forInEnumerator (f: 'a -> 'b) (en: ForInEnumeratorG<'a>) : ForInEnumeratorG<'b> =
        match en with
        | ForInEnumeratorG.Interface -> ForInEnumeratorG.Interface
        // `getEnumerator` / `members` carry only `SymbolKey`s (no `'ty`), so only the
        // enumerator type is remapped through `f`.
        | ForInEnumeratorG.Pattern(enumTy, ge, members, isVal, disp) ->
            ForInEnumeratorG.Pattern(f enumTy, ge, members, isVal, disp)

    let rec expr (f: 'a -> 'b) (e: TExprG<'a>) : TExprG<'b> =
        let pe = expr f
        let pp = pat f
        let pa = arm f

        match e with
        | TExprG.Const(v, ty) -> TExprG.Const(v, f ty)
        | TExprG.Var(k, ty) -> TExprG.Var(k, f ty)
        | TExprG.External(n, k, ty) -> TExprG.External(n, k, f ty)
        | TExprG.Null ty -> TExprG.Null(f ty)
        | TExprG.Lambda(p, b, ty) -> TExprG.Lambda(pp p, pe b, f ty)
        | TExprG.App(fn, a, ty) -> TExprG.App(pe fn, pe a, f ty)
        | TExprG.Let(p, v, body, ty) -> TExprG.Let(pp p, pe v, pe body, f ty)
        | TExprG.Use(p, v, body, dispose, ty) -> TExprG.Use(pp p, pe v, pe body, dispose, f ty)
        | TExprG.IfThenElse(c, t, el, ty) -> TExprG.IfThenElse(pe c, pe t, pe el, f ty)
        | TExprG.Tuple(items, ty) -> TExprG.Tuple(EqArray.map pe items, f ty)
        | TExprG.Sequential(items, ty) -> TExprG.Sequential(EqArray.map pe items, f ty)
        | TExprG.While(c, b, ty) -> TExprG.While(pe c, pe b, f ty)
        | TExprG.ForTo(k, s, e2, b, ty) -> TExprG.ForTo(k, pe s, pe e2, pe b, f ty)
        | TExprG.ForIn(p, src, b, en, ty) -> TExprG.ForIn(pp p, pe src, pe b, forInEnumerator f en, f ty)
        | TExprG.Match(sc, arms, ty) -> TExprG.Match(pe sc, EqArray.map pa arms, f ty)
        | TExprG.TryWith(b, arms, ty) -> TExprG.TryWith(pe b, EqArray.map pa arms, f ty)
        | TExprG.TryFinally(b, c, ty) -> TExprG.TryFinally(pe b, pe c, f ty)
        | TExprG.Assignment(l, r, ty) -> TExprG.Assignment(pe l, pe r, f ty)
        | TExprG.Range(s, step, e2, ty) -> TExprG.Range(pe s, Option.map pe step, pe e2, f ty)
        | TExprG.RecordCons(fields, ty) -> TExprG.RecordCons(EqArray.map (fun (n, v) -> n, pe v) fields, f ty)
        | TExprG.RecordClone(src, ov, ty) -> TExprG.RecordClone(pe src, EqArray.map (fun (n, v) -> n, pe v) ov, f ty)
        | TExprG.FieldGet(r, n, ty) -> TExprG.FieldGet(pe r, n, f ty)
        | TExprG.FieldSet(r, n, v, ty) -> TExprG.FieldSet(pe r, n, pe v, f ty)
        | TExprG.UnionCons(c, args, ty) -> TExprG.UnionCons(c, EqArray.map pe args, f ty)
        | TExprG.New(c, args, ty) -> TExprG.New(c, EqArray.map pe args, f ty)
        | TExprG.MethodCall(r, k, via, args, ty) -> TExprG.MethodCall(pe r, k, via, EqArray.map pe args, f ty)
        | TExprG.PropertyGet(r, k, via, ty) -> TExprG.PropertyGet(pe r, k, via, f ty)
        | TExprG.StaticMethodCall(k, args, ty) -> TExprG.StaticMethodCall(k, EqArray.map pe args, f ty)
        | TExprG.StaticPropertyGet(k, ty) -> TExprG.StaticPropertyGet(k, f ty)
        | TExprG.StaticFieldGet(k, n, ty) -> TExprG.StaticFieldGet(k, n, f ty)
        | TExprG.ExternalMember(r, k, n, isProp, ty) -> TExprG.ExternalMember(ValueOption.map pe r, k, n, isProp, f ty)
        | TExprG.Format(sink, segs, ty) -> TExprG.Format(sinkOf f sink, EqArray.map (segOf f) segs, f ty)
        | TExprG.ILIntrinsic(op, operand, args, ty) ->
            TExprG.ILIntrinsic(op, ValueOption.map f operand, EqArray.map pe args, f ty)
        | TExprG.StaticOptimization(clauses, def, ty) ->
            TExprG.StaticOptimization(EqArray.map (clause f) clauses, pe def, f ty)
        | TExprG.Upcast(src, ty) -> TExprG.Upcast(pe src, f ty)
        | TExprG.Downcast(src, ty) -> TExprG.Downcast(pe src, f ty)
        | TExprG.TraitCall(recv, n, args, ty) -> TExprG.TraitCall(f recv, n, EqArray.map pe args, f ty)
        | TExprG.TypeTest(src, testTy, ty) -> TExprG.TypeTest(pe src, f testTy, f ty)

    and arm (f: 'a -> 'b) (a: TMatchArmG<'a>) : TMatchArmG<'b> =
        {
            Pat = pat f a.Pat
            Guard = Option.map (expr f) a.Guard
            Body = expr f a.Body
        }

    and sinkOf (f: 'a -> 'b) (s: FormatSinkG<'a>) : FormatSinkG<'b> =
        match s with
        | FormatSinkG.ToStdOut nl -> FormatSinkG.ToStdOut nl
        | FormatSinkG.ToStdErr nl -> FormatSinkG.ToStdErr nl
        | FormatSinkG.ToWriter w -> FormatSinkG.ToWriter(expr f w)
        | FormatSinkG.ToBuilder w -> FormatSinkG.ToBuilder(expr f w)
        | FormatSinkG.ToString -> FormatSinkG.ToString

    and segOf (f: 'a -> 'b) (seg: FormatSegG<'a>) : FormatSegG<'b> =
        match seg with
        | FormatSegG.Lit lit -> FormatSegG.Lit lit
        | FormatSegG.Hole(h, a) -> FormatSegG.Hole(hole f h, expr f a)

    and clause (f: 'a -> 'b) (c: TStaticOptClauseG<'a>) : TStaticOptClauseG<'b> =
        {
            Constraints = c.Constraints
            Body = expr f c.Body
        }

    let unionCase (f: 'a -> 'b) (c: TUnionCaseG<'a>) : TUnionCaseG<'b> =
        {
            Name = c.Name
            Fields = EqArray.map (fun (n, ty) -> n, f ty) c.Fields
        }

    let recordField (f: 'a -> 'b) (fld: TRecordFieldG<'a>) : TRecordFieldG<'b> =
        {
            Name = fld.Name
            Type = f fld.Type
            IsMutable = fld.IsMutable
        }

    let typeMember (f: 'a -> 'b) (m: TTypeMemberG<'a>) : TTypeMemberG<'b> =
        {
            Name = m.Name
            IsStatic = m.IsStatic
            Kind = m.Kind
            IsOverride = m.IsOverride
            ThisKey = m.ThisKey
            BaseKey = m.BaseKey
            ThisTy = f m.ThisTy
            Params = EqArray.map (fun (k, ty) -> k, f ty) m.Params
            Body = expr f m.Body
            ReturnTy = f m.ReturnTy
            MethodTypeParams = m.MethodTypeParams
        }

    let staticLet (f: 'a -> 'b) (sl: TStaticLetG<'a>) : TStaticLetG<'b> =
        {
            Name = sl.Name
            Type = f sl.Type
            Init = expr f sl.Init
        }

    let ctorLet (f: 'a -> 'b) (cl: TCtorLetG<'a>) : TCtorLetG<'b> =
        {
            Binder = cl.Binder
            Type = f cl.Type
            Init = expr f cl.Init
        }

    let ctorFieldInit (f: 'a -> 'b) (fi: TCtorFieldInitG<'a>) : TCtorFieldInitG<'b> =
        {
            Field = fi.Field
            Init = expr f fi.Init
        }

    let secondaryCtor (f: 'a -> 'b) (sc: TSecondaryCtorG<'a>) : TSecondaryCtorG<'b> =
        {
            Params = EqArray.map (fun (k, ty) -> k, f ty) sc.Params
            Lets = EqArray.map (ctorLet f) sc.Lets
            PrimaryArgs = EqArray.map (expr f) sc.PrimaryArgs
            FieldInits = EqArray.map (ctorFieldInit f) sc.FieldInits
        }

    let baseCtorCall (f: 'a -> 'b) (bc: TBaseCtorCallG<'a>) : TBaseCtorCallG<'b> =
        {
            CtorParams = EqArray.map (fun (k, ty) -> k, f ty) bc.CtorParams
            Args = EqArray.map (expr f) bc.Args
        }

    let abstractMethod (f: 'a -> 'b) (am: TAbstractMethodG<'a>) : TAbstractMethodG<'b> =
        {
            Name = am.Name
            MethodTypeParams = am.MethodTypeParams
            Signature = f am.Signature
        }

    let kind (f: 'a -> 'b) (k: TTypeKindG<'a>) : TTypeKindG<'b> =
        match k with
        | TTypeKindG.Interface methods -> TTypeKindG.Interface(EqArray.map (abstractMethod f) methods)
        | TTypeKindG.Union(cases, members) ->
            TTypeKindG.Union(EqArray.map (unionCase f) cases, EqArray.map (typeMember f) members)
        | TTypeKindG.Record(fields, members) ->
            TTypeKindG.Record(EqArray.map (recordField f) fields, EqArray.map (typeMember f) members)
        | TTypeKindG.Class(fields,
                           ctorParams,
                           members,
                           baseType,
                           interfaces,
                           isSealed,
                           staticLets,
                           secondaryCtors,
                           baseCtor,
                           isStruct) ->
            TTypeKindG.Class(
                EqArray.map (recordField f) fields,
                EqArray.map (recordField f) ctorParams,
                EqArray.map (typeMember f) members,
                ValueOption.map f baseType,
                EqArray.map (fun (ity, ms) -> f ity, EqArray.map (typeMember f) ms) interfaces,
                isSealed,
                EqArray.map (staticLet f) staticLets,
                EqArray.map (secondaryCtor f) secondaryCtors,
                ValueOption.map (baseCtorCall f) baseCtor,
                isStruct
            )

    let typeDecl (f: 'a -> 'b) (td: TTypeDeclG<'a>) : TTypeDeclG<'b> =
        {
            Name = td.Name
            Key = td.Key
            Namespace = td.Namespace
            TypeParams = td.TypeParams
            Kind = kind f td.Kind
            EqualitySupport = td.EqualitySupport
            ComparisonSupport = td.ComparisonSupport
        }

    let decl (f: 'a -> 'b) (d: TDeclG<'a>) : TDeclG<'b> =
        match d with
        | TDeclG.Let(binding, value, isInline, ty) -> TDeclG.Let(pat f binding, expr f value, isInline, f ty)
        | TDeclG.Expression(e, ty) -> TDeclG.Expression(expr f e, f ty)
        | TDeclG.Type td -> TDeclG.Type(typeDecl f td)

    /// The whole-file rebuild: `Decls` mapped through `f`, the non-`'ty` snapshot
    /// fields (`Diagnostics` / `IntrinsicReprTypes` / `ModuleMembers`) carried over.
    let file (f: 'a -> 'b) (tf: TastFileG<'a>) : TastFileG<'b> =
        {
            Decls = EqArray.map (decl f) tf.Decls
            Diagnostics = tf.Diagnostics
            IntrinsicReprTypes = tf.IntrinsicReprTypes
            ModuleMembers = tf.ModuleMembers
        }
