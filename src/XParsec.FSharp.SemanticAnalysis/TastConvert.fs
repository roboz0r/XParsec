namespace XParsec.FSharp.SemanticAnalysis

// Cross-type structural rebuild of the TAST cluster: every embedded `'ty` through `f`,
// every POSITION through `fTok`, no hooks. Other payload is copied verbatim, but NOT
// `MethodTypeParams`, whose entries ride `'ty` and would carry a live `UnionFind` cell.

[<RequireQualifiedAccess>]
module TastConvert =

    let rec pat (f: 'a -> 'b) (fTok: 'ta -> 'tb) (p: TPatG<'a, 'ta, 'id>) : TPatG<'b, 'tb, 'id> =
        let pp = pat f fTok
        let tk = fTok

        match p with
        | TPatG.NamedSimple(k, ty, tok) -> TPatG.NamedSimple(k, f ty, tk tok)
        | TPatG.Wildcard(ty, tok) -> TPatG.Wildcard(f ty, tk tok)
        | TPatG.Const(v, ty, tok) -> TPatG.Const(v, f ty, tk tok)
        | TPatG.Tuple(items, ty, tok) -> TPatG.Tuple(EqArray.map pp items, f ty, tk tok)
        | TPatG.Record(fields, ty, tok) -> TPatG.Record(EqArray.map (fun (n, sub) -> n, pp sub) fields, f ty, tk tok)
        | TPatG.Union(c, fields, ty, tok) -> TPatG.Union(c, EqArray.map pp fields, f ty, tk tok)
        | TPatG.TypeTestAs(testTy, inner, ty, tok) -> TPatG.TypeTestAs(f testTy, pp inner, f ty, tk tok)
        | TPatG.Null(ty, tok) -> TPatG.Null(f ty, tk tok)
        | TPatG.EnumCase(k, n, ty, tok) -> TPatG.EnumCase(k, n, f ty, tk tok)
        | TPatG.Or(alts, ty, tok) -> TPatG.Or(EqArray.map pp alts, f ty, tk tok)

    /// A format hole across BOTH axes: the one node that carries a token of its own and no
    /// sub-expression, so its anchor is widened here rather than at a node.
    let hole (f: 'a -> 'b) (fTok: 'ta -> 'tb) (h: HoleSpecG<'a, 'ta>) : HoleSpecG<'b, 'tb> =
        {
            Ty = f h.Ty
            Source = h.Source
            Tok = fTok h.Tok
        }

    let forInGetEnum (f: 'a -> 'b) (ge: ForInGetEnumG<'a>) : ForInGetEnumG<'b> =
        match ge with
        | ForInGetEnumG.External k -> ForInGetEnumG.External k
        | ForInGetEnumG.Local -> ForInGetEnumG.Local
        | ForInGetEnumG.ConstrainedInterface(iface, args) ->
            ForInGetEnumG.ConstrainedInterface(iface, EqArray.map f args)

    let forInEnumMembers (f: 'a -> 'b) (m: ForInEnumMembersG<'a>) : ForInEnumMembersG<'b> =
        match m with
        | ForInEnumMembersG.External(mn, cur) -> ForInEnumMembersG.External(mn, cur)
        | ForInEnumMembersG.Local -> ForInEnumMembersG.Local
        | ForInEnumMembersG.ConstrainedInterface(iface, args) ->
            ForInEnumMembersG.ConstrainedInterface(iface, EqArray.map f args)

    let forInEnumerator (f: 'a -> 'b) (en: ForInEnumeratorG<'a>) : ForInEnumeratorG<'b> =
        match en with
        | ForInEnumeratorG.Interface -> ForInEnumeratorG.Interface
        | ForInEnumeratorG.Pattern(enumTy, ge, members, isVal, disp) ->
            ForInEnumeratorG.Pattern(f enumTy, forInGetEnum f ge, forInEnumMembers f members, isVal, disp)

    let rec expr (f: 'a -> 'b) (fTok: 'ta -> 'tb) (e: TExprG<'a, 'ta, 'id>) : TExprG<'b, 'tb, 'id> =
        let pe = expr f fTok
        let pp = pat f fTok
        let pa = arm f fTok
        let tk = fTok

        match e with
        | TExprG.Const(v, ty, tok) -> TExprG.Const(v, f ty, tk tok)
        | TExprG.Var(k, ty, tok) -> TExprG.Var(k, f ty, tk tok)
        | TExprG.External(n, k, ty, tok) -> TExprG.External(n, k, f ty, tk tok)
        | TExprG.Null(ty, tok) -> TExprG.Null(f ty, tk tok)
        | TExprG.Lambda(p, b, ty, tok) -> TExprG.Lambda(pp p, pe b, f ty, tk tok)
        | TExprG.App(fn, a, ty, tok) -> TExprG.App(pe fn, pe a, f ty, tk tok)
        | TExprG.Let(p, v, body, ty, tok) -> TExprG.Let(pp p, pe v, pe body, f ty, tk tok)
        | TExprG.Use(p, v, body, dispose, ty, tok) -> TExprG.Use(pp p, pe v, pe body, dispose, f ty, tk tok)
        | TExprG.IfThenElse(c, t, el, ty, tok) -> TExprG.IfThenElse(pe c, pe t, pe el, f ty, tk tok)
        | TExprG.Tuple(items, ty, tok) -> TExprG.Tuple(EqArray.map pe items, f ty, tk tok)
        | TExprG.Sequential(items, ty, tok) -> TExprG.Sequential(EqArray.map pe items, f ty, tk tok)
        | TExprG.While(c, b, ty, tok) -> TExprG.While(pe c, pe b, f ty, tk tok)
        | TExprG.ForTo(k, it, s, e2, b, ty, tok) -> TExprG.ForTo(k, tk it, pe s, pe e2, pe b, f ty, tk tok)
        | TExprG.ForIn(p, src, b, en, ty, tok) -> TExprG.ForIn(pp p, pe src, pe b, forInEnumerator f en, f ty, tk tok)
        | TExprG.Match(sc, arms, ty, tok) -> TExprG.Match(pe sc, EqArray.map pa arms, f ty, tk tok)
        | TExprG.TryWith(b, arms, ty, tok) -> TExprG.TryWith(pe b, EqArray.map pa arms, f ty, tk tok)
        | TExprG.TryFinally(b, c, ty, tok) -> TExprG.TryFinally(pe b, pe c, f ty, tk tok)
        | TExprG.Assignment(l, r, ty, tok) -> TExprG.Assignment(pe l, pe r, f ty, tk tok)
        | TExprG.Range(s, step, e2, ty, tok) -> TExprG.Range(pe s, Option.map pe step, pe e2, f ty, tk tok)
        | TExprG.RecordCons(fields, ty, tok) ->
            TExprG.RecordCons(EqArray.map (fun (n, v) -> n, pe v) fields, f ty, tk tok)
        | TExprG.RecordClone(src, ov, ty, tok) ->
            TExprG.RecordClone(pe src, EqArray.map (fun (n, v) -> n, pe v) ov, f ty, tk tok)
        | TExprG.FieldGet(r, n, ty, tok) -> TExprG.FieldGet(pe r, n, f ty, tk tok)
        | TExprG.FieldSet(r, n, v, ty, tok) -> TExprG.FieldSet(pe r, n, pe v, f ty, tk tok)
        | TExprG.UnionCons(c, args, ty, tok) -> TExprG.UnionCons(c, EqArray.map pe args, f ty, tk tok)
        | TExprG.New(c, k, args, ty, tok) -> TExprG.New(c, k, EqArray.map pe args, f ty, tk tok)
        | TExprG.MethodCall(r, k, via, args, ty, tok) ->
            TExprG.MethodCall(pe r, k, viaOf f via, EqArray.map pe args, f ty, tk tok)
        | TExprG.PropertyGet(r, k, via, ty, tok) -> TExprG.PropertyGet(pe r, k, viaOf f via, f ty, tk tok)
        | TExprG.StaticMethodCall(k, args, ty, tok) -> TExprG.StaticMethodCall(k, EqArray.map pe args, f ty, tk tok)
        | TExprG.StaticPropertyGet(k, ty, tok) -> TExprG.StaticPropertyGet(k, f ty, tk tok)
        | TExprG.StaticFieldGet(k, n, ty, tok) -> TExprG.StaticFieldGet(k, n, f ty, tk tok)
        | TExprG.StaticFieldSet(k, n, v, ty, tok) -> TExprG.StaticFieldSet(k, n, pe v, f ty, tk tok)
        | TExprG.ExternalMember(r, k, n, isProp, widths, ty, tok) ->
            TExprG.ExternalMember(ValueOption.map pe r, k, n, isProp, widths, f ty, tk tok)
        | TExprG.Format(sink, segs, ty, tok) ->
            TExprG.Format(sinkOf f fTok sink, EqArray.map (segOf f fTok) segs, f ty, tk tok)
        | TExprG.ILIntrinsic(op, operand, args, ty, tok) ->
            TExprG.ILIntrinsic(op, ValueOption.map f operand, EqArray.map pe args, f ty, tk tok)
        | TExprG.StaticOptimization(clauses, def, ty, tok) ->
            TExprG.StaticOptimization(EqArray.map (clause f fTok) clauses, pe def, f ty, tk tok)
        | TExprG.Upcast(src, ty, tok) -> TExprG.Upcast(pe src, f ty, tk tok)
        | TExprG.Downcast(src, ty, tok) -> TExprG.Downcast(pe src, f ty, tk tok)
        | TExprG.TraitCall(supportTy, n, args, ty, tok) ->
            TExprG.TraitCall(f supportTy, n, EqArray.map pe args, f ty, tk tok)
        | TExprG.TypeTest(src, testTy, ty, tok) -> TExprG.TypeTest(pe src, f testTy, f ty, tk tok)
        // The `spec` index is domain-free: the table it indexes is remapped whole alongside
        // the tree. An `origin` is a file IDENTITY, not a position, so `tk` never sees it.
        | TExprG.InlineCall(spec, args, origin, ty, tok) ->
            TExprG.InlineCall(spec, EqArray.map pe args, origin, f ty, tk tok)
        | TExprG.CallerExpr(body, origin, ty, tok) -> TExprG.CallerExpr(pe body, origin, f ty, tk tok)

    and arm
        (f: 'a -> 'b)
        (fTok: 'ta -> 'tb)
        (a: TMatchArmG<TPatG<'a, 'ta, 'id>, TExprG<'a, 'ta, 'id>>)
        : TMatchArmG<TPatG<'b, 'tb, 'id>, TExprG<'b, 'tb, 'id>> =
        {
            Pat = pat f fTok a.Pat
            Guard = ValueOption.map (expr f fTok) a.Guard
            Body = expr f fTok a.Body
        }

    and viaOf (f: 'a -> 'b) (v: CallVia<'a>) : CallVia<'b> =
        match v with
        | CallVia.Self -> CallVia.Self
        | CallVia.Base -> CallVia.Base
        | CallVia.Interface ifaceArgs -> CallVia.Interface(EqArray.map f ifaceArgs)

    and sinkOf
        (f: 'a -> 'b)
        (fTok: 'ta -> 'tb)
        (s: FormatSinkG<TExprG<'a, 'ta, 'id>>)
        : FormatSinkG<TExprG<'b, 'tb, 'id>> =
        match s with
        | FormatSinkG.ToStdOut nl -> FormatSinkG.ToStdOut nl
        | FormatSinkG.ToStdErr nl -> FormatSinkG.ToStdErr nl
        | FormatSinkG.ToWriter(w, nl) -> FormatSinkG.ToWriter(expr f fTok w, nl)
        | FormatSinkG.ToBuilder w -> FormatSinkG.ToBuilder(expr f fTok w)
        | FormatSinkG.ToString -> FormatSinkG.ToString

    and segOf
        (f: 'a -> 'b)
        (fTok: 'ta -> 'tb)
        (seg: FormatSegG<'a, 'ta, TExprG<'a, 'ta, 'id>>)
        : FormatSegG<'b, 'tb, TExprG<'b, 'tb, 'id>> =
        let spec = hole f fTok
        let pe = expr f fTok

        match seg with
        | FormatSegG.Lit lit -> FormatSegG.Lit lit
        | FormatSegG.Hole(h, a) -> FormatSegG.Hole(spec h, pe a)
        | FormatSegG.DynHole d ->
            FormatSegG.DynHole
                {
                    Width = ValueOption.map pe d.Width
                    Precision = ValueOption.map pe d.Precision
                    Spec = spec d.Spec
                    Value = pe d.Value
                }
        | FormatSegG.CallbackHole(h, residue) -> FormatSegG.CallbackHole(spec h, pe residue)

    and constraintOf (f: 'a -> 'b) (c: TStaticOptConstraintG<'a>) : TStaticOptConstraintG<'b> =
        match c with
        | TStaticOptConstraintG.TyconEquals(tp, req) -> TStaticOptConstraintG.TyconEquals(f tp, f req)
        | TStaticOptConstraintG.IsStruct tp -> TStaticOptConstraintG.IsStruct(f tp)

    and clause (f: 'a -> 'b) (fTok: 'ta -> 'tb) (c: TStaticOptClauseG<'a, 'ta, 'id>) : TStaticOptClauseG<'b, 'tb, 'id> =
        {
            Constraints = EqArray.map (constraintOf f) c.Constraints
            Body = expr f fTok c.Body
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

    // The type-declaration cluster: a functor in `('ty, 'tok, 'id, 'body)`. Every function
    // below maps the four axes independently, so a `'ty`-only rebuild, a body-pooling pass
    // and its inverse are instantiations of one traversal.

    /// The four mappings a declaration rebuild runs under, one per axis. Named, because
    /// `Ty`, `Tok` and `Body` are bare functions that positionally could transpose.
    type DeclRebuild<'ta, 'tb, 'toka, 'tokb, 'ida, 'idb, 'bodya, 'bodyb> =
        {
            Ty: 'ta -> 'tb
            /// An enum case's identifier token: the cluster's only `'tok`, every other slot
            /// having gone to `'body`.
            Tok: 'toka -> 'tokb
            Id: BoundVarKeyG<'ida> -> 'idb
            Body: 'bodya -> 'bodyb
        }

    let typeMember (m': DeclRebuild<'a, 'b, _, _, 'ia, 'ib, 'ba, 'bb>) (m: TTypeMemberG<'a, 'ia, 'ba>) =
        let fTy = m'.Ty
        let fBody = m'.Body
        let slot = BoundVarKey.refile m'.Id

        {
            Name = m.Name
            IsStatic = m.IsStatic
            Accessibility = m.Accessibility
            IsInline = m.IsInline
            Kind = m.Kind
            IsOverride = m.IsOverride
            ThisKey = ValueOption.map slot m.ThisKey
            BaseKey = ValueOption.map slot m.BaseKey
            ThisTy = fTy m.ThisTy
            Params = EqArray.map (fun (k, ty) -> slot k, fTy ty) m.Params
            Body = fBody m.Body
            ReturnTy = fTy m.ReturnTy
            MethodTypeParams = EqArray.map (fun (n, ty) -> n, fTy ty) m.MethodTypeParams
        }

    let classLet (fTy: 'a -> 'b) (fBody: 'ba -> 'bb) (l: TClassLetG<'a, 'ba>) : TClassLetG<'b, 'bb> =
        {
            Name = l.Name
            Type = fTy l.Type
            IsMutable = l.IsMutable
            Init = fBody l.Init
        }

    let preambleEntry (fTy: 'a -> 'b) (fBody: 'ba -> 'bb) (p: TPreambleEntryG<'a, 'ba>) : TPreambleEntryG<'b, 'bb> =
        match p with
        | TPreambleEntryG.Let l -> TPreambleEntryG.Let(classLet fTy fBody l)
        | TPreambleEntryG.Do e -> TPreambleEntryG.Do(fBody e)

    let ctorLet (m: DeclRebuild<'a, 'b, _, _, 'ia, 'ib, 'ba, 'bb>) (cl: TCtorLetG<'a, 'ia, 'ba>) =
        {
            BoundVar = BoundVarKey.refile m.Id cl.BoundVar
            Type = m.Ty cl.Type
            Init = m.Body cl.Init
        }

    let ctorFieldInit (fBody: 'ba -> 'bb) (fi: TCtorFieldInitG<'ba>) : TCtorFieldInitG<'bb> =
        {
            Field = fi.Field
            Init = fBody fi.Init
        }

    let secondaryCtor (m: DeclRebuild<'a, 'b, _, _, 'ia, 'ib, 'ba, 'bb>) (sc: TSecondaryCtorG<'a, 'ia, 'ba>) =
        {
            Params = EqArray.map (fun (k, ty) -> BoundVarKey.refile m.Id k, m.Ty ty) sc.Params
            Lets = EqArray.map (ctorLet m) sc.Lets
            PrimaryArgs = EqArray.map m.Body sc.PrimaryArgs
            FieldInits = EqArray.map (ctorFieldInit m.Body) sc.FieldInits
        }

    let baseCtorCall (m: DeclRebuild<'a, 'b, _, _, 'ia, 'ib, 'ba, 'bb>) (bc: TBaseCtorCallG<'a, 'ia, 'ba>) =
        {
            CtorParams = EqArray.map (fun (k, ty) -> BoundVarKey.refile m.Id k, m.Ty ty) bc.CtorParams
            Args = EqArray.map m.Body bc.Args
            ChosenCtor = bc.ChosenCtor
        }

    let abstractMethod (f: 'a -> 'b) (am: TAbstractMethodG<'a>) : TAbstractMethodG<'b> =
        {
            Name = am.Name
            MethodTypeParams = am.MethodTypeParams
            Signature = f am.Signature
            IsProperty = am.IsProperty
        }

    let enumCase (fTok: 'ta -> 'tb) (c: TEnumCaseG<'ta>) : TEnumCaseG<'tb> =
        {
            Name = c.Name
            Value = c.Value
            Tok = fTok c.Tok
        }

    let kind (m: DeclRebuild<'a, 'b, 'ta, 'tb, 'ia, 'ib, 'ba, 'bb>) (k: TTypeKindG<'a, 'ta, 'ia, 'ba>) =
        let fTy = m.Ty
        let fBody = m.Body
        let mem = typeMember m
        let ifaces = EqArray.map (fun (ity, ms) -> fTy ity, EqArray.map mem ms)

        match k with
        | TTypeKindG.Interface methods -> TTypeKindG.Interface(EqArray.map (abstractMethod fTy) methods)
        | TTypeKindG.Union(cases, members, interfaces) ->
            TTypeKindG.Union(EqArray.map (unionCase fTy) cases, EqArray.map mem members, ifaces interfaces)
        | TTypeKindG.Record(fields, members, interfaces, valueKind) ->
            TTypeKindG.Record(
                EqArray.map (recordField fTy) fields,
                EqArray.map mem members,
                ifaces interfaces,
                valueKind
            )
        // Enum cases carry no `'ty` (the value is a resolved literal) and no body, so the
        // case identifier's token is all the mapping can touch.
        | TTypeKindG.Enum cases -> TTypeKindG.Enum(EqArray.map (enumCase m.Tok) cases)
        | TTypeKindG.Class c ->
            TTypeKindG.Class
                {
                    Fields = EqArray.map (recordField fTy) c.Fields
                    CtorParams = EqArray.map (recordField fTy) c.CtorParams
                    Members = EqArray.map mem c.Members
                    BaseType = ValueOption.map fTy c.BaseType
                    Interfaces = ifaces c.Interfaces
                    Declared = c.Declared
                    StaticPreamble = EqArray.map (preambleEntry fTy fBody) c.StaticPreamble
                    InstancePreamble = EqArray.map (preambleEntry fTy fBody) c.InstancePreamble
                    ThisKey = BoundVarKey.refile m.Id c.ThisKey
                    SecondaryCtors = EqArray.map (secondaryCtor m) c.SecondaryCtors
                    BaseCtorCall = ValueOption.map (baseCtorCall m) c.BaseCtorCall
                    ValueKind = c.ValueKind
                    HasPrimaryCtor = c.HasPrimaryCtor
                }

    let typeDecl (m: DeclRebuild<'a, 'b, 'ta, 'tb, 'ia, 'ib, 'ba, 'bb>) (td: TTypeDeclG<'a, 'ta, 'ia, 'ba>) =
        {
            Name = td.Name
            TypeKey = td.TypeKey
            Namespace = td.Namespace
            TypeParams = td.TypeParams
            IsRequireQualifiedAccess = td.IsRequireQualifiedAccess
            Kind = kind m td.Kind
            EqualitySupport = td.EqualitySupport
            ComparisonSupport = td.ComparisonSupport
        }

    let decl (f: 'a -> 'b) (fTok: 'ta -> 'tb) (d: TDeclG<'a, 'ta, 'id>) : TDeclG<'b, 'tb, 'id> =
        match d with
        | TDeclG.Let(binding, value, isInline, ty) -> TDeclG.Let(pat f fTok binding, expr f fTok value, isInline, f ty)
        | TDeclG.Expression(e, ty) -> TDeclG.Expression(expr f fTok e, f ty)
        // A tree-shaped rebuild leaves the identity axis alone: the key slots stay in the
        // space they were in, and the bodies are the expression rebuild itself.
        | TDeclG.Type td ->
            TDeclG.Type(
                typeDecl
                    {
                        Ty = f
                        Tok = fTok
                        Id = BoundVarKey.identity
                        Body = expr f fTok
                    }
                    td
            )

    let inlineBody (f: 'a -> 'b) (fTok: 'ta -> 'tb) (ib: TInlineBodyG<'a, 'ta, 'id>) : TInlineBodyG<'b, 'tb, 'id> =
        {
            Decl = decl f fTok ib.Decl
            ParamAttrs = ib.ParamAttrs
        }

    // The compiled-form cluster, likewise a trifunctor, in `('ty, 'pat, 'id)`. The `'ty`-only
    // conversion runs it at `fPat = pat fTy` and `fId = id`; pooling runs it at `fTy = id`,
    // `fPat = <pat → pool id>`, `fId = <key → bound variable id>`, and unpooling at its inverse.
    let argGroup
        (fTy: 'a -> 'b)
        (fPat: 'pa -> 'pb)
        (fId: 'ia -> 'ib)
        (g: ArgGroupG<'a, 'pa, 'ia>)
        : ArgGroupG<'b, 'pb, 'ib> =
        match g with
        | ArgGroupG.GUnit ty -> ArgGroupG.GUnit(fTy ty)
        | ArgGroupG.GSimple(slot, ty) -> ArgGroupG.GSimple(fId slot, fTy ty)
        | ArgGroupG.GTuple p -> ArgGroupG.GTuple(fPat p)

    let valRepr
        (fTy: 'a -> 'b)
        (fPat: 'pa -> 'pb)
        (fId: 'ia -> 'ib)
        (vr: ValReprG<'a, 'pa, 'ia>)
        : ValReprG<'b, 'pb, 'ib> =
        {
            Typars = vr.Typars
            Groups = vr.Groups |> List.map (argGroup fTy fPat fId)
            ResultTy = fTy vr.ResultTy
        }

    let inlineValue (f: 'a -> 'b) (fTok: 'ta -> 'tb) (iv: TInlineValueG<'a, 'ta, 'id>) : TInlineValueG<'b, 'tb, 'id> =
        {
            Key = iv.Key
            Body = inlineBody f fTok iv.Body
        }

    /// A specialization-table entry. Its KEY's type arguments map too: they are the
    /// grounding the entry was resolved at, so they must land in the body's domain.
    let specialization
        (f: 'a -> 'b)
        (fTok: 'ta -> 'tb)
        (s: TSpecializationG<'a, 'ta, 'id>)
        : TSpecializationG<'b, 'tb, 'id> =
        {
            Key =
                {
                    Template = s.Key.Template
                    TypeArgs = EqArray.map f s.Key.TypeArgs
                }
            // NOT mapped by `fTok`: the origin names which file the anchors index, and a change
            // of the position REPRESENTATION does not move the body to another file.
            Origin = s.Origin
            Decl = decl f fTok s.Decl
        }

    /// The whole-file rebuild: `Decls`, `InlineBodies` and `Specializations` mapped through
    /// `f`; every non-`'ty` snapshot field carried over verbatim.
    let file (f: 'a -> 'b) (fTok: 'ta -> 'tb) (tf: TastFileG<'a, 'ta, 'id>) : TastFileG<'b, 'tb, 'id> =
        {
            Decls = EqArray.map (decl f fTok) tf.Decls
            InlineBodies = EqArray.map (inlineValue f fTok) tf.InlineBodies
            Specializations = EqArray.map (specialization f fTok) tf.Specializations
            Diagnostics = tf.Diagnostics
            IntrinsicReprKeys = tf.IntrinsicReprKeys
            GlobalValueKeys = tf.GlobalValueKeys
            ModuleMembers = tf.ModuleMembers
            ClosureReprs = tf.ClosureReprs
            FunVerdicts = tf.FunVerdicts
            GenericFnSchemes = tf.GenericFnSchemes
            Accessibility = tf.Accessibility
            BindingTyparArities = tf.BindingTyparArities
        }
