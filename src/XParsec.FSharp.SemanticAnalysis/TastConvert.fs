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
// Two of the clusters take more than `'ty`: the type declaration is generic in
// `('ty, 'id, 'body)` and the compiled form in `('ty, 'pat)`. The `'ty` freeze is just the
// diagonal (`fId = BinderKey.identity`, `fBody = expr f`, `fPat = pat f`), while the frozen
// POOLS instantiate the other parameters at dense ids and run the very same traversals in
// both directions. Keeping them here rather than re-walking those shapes in `TastPools` is
// what stops the declaration spine — the seven body slots and the six key slots especially
// — from being enumerated twice.
//
// The `'id` axis stops at the DECLARATION cluster: a `Var` reference and a `NamedSimple`
// binding are named by the same axis, but nothing here re-files them — the tree-shaped
// mappings are identity-preserving, and re-filing a whole tree between identity spaces is
// the pooling walk's job (`TastPools` / `TastUnpool`), which rebuilds node by node anyway.
//
// Other non-`'ty` payload is copied verbatim: SymbolKey / CallVia / TConstValue
// / TMemberKind / PrintfSpec.HoleKind / the verdict fields / the side maps.
// `TTypeMemberG.MethodTypeParams : EqArray<string * 'ty>` is NOT such a payload — it
// rides `'ty` (each entry's typar as its own `TyVar root`), so `f` maps it like every
// other embedded type, flipping the root to `FTTypar(Method, i)`. It used to be a
// cell-bearing `GeneralizedTypars` copied verbatim — the one field that smuggled a
// live `UnionFind` cell across the freeze.
//
// `TStaticOptClauseG.Constraints` is NOT in that list: it is
// `EqArray<TStaticOptConstraintG<'ty>>` and is mapped like any other `'ty` payload.
// It used to be a raw-`SemType` hole here, on the premise that no
// `StaticOptimization` survives the inline pass and so none could reach the frozen
// tree. That premise dies with the frozen inline-body channel (an inline template
// IS frozen, `StaticOptimization` nodes and all), and a verbatim copy would have
// smuggled a live `UnionFind` cell across the freeze — the exact hazard freeze
// exists to close.

[<RequireQualifiedAccess>]
module TastConvert =

    let rec pat (f: 'a -> 'b) (p: TPatG<'a, 'tok, 'id>) : TPatG<'b, 'tok, 'id> =
        match p with
        | TPatG.NamedSimple(k, ty, tok) -> TPatG.NamedSimple(k, f ty, tok)
        | TPatG.Wildcard(ty, tok) -> TPatG.Wildcard(f ty, tok)
        | TPatG.Const(v, ty, tok) -> TPatG.Const(v, f ty, tok)
        | TPatG.Tuple(items, ty, tok) -> TPatG.Tuple(EqArray.map (pat f) items, f ty, tok)
        | TPatG.Record(fields, ty, tok) -> TPatG.Record(EqArray.map (fun (n, sub) -> n, pat f sub) fields, f ty, tok)
        | TPatG.Union(c, fields, ty, tok) -> TPatG.Union(c, EqArray.map (pat f) fields, f ty, tok)
        | TPatG.TypeTestAs(testTy, inner, ty, tok) -> TPatG.TypeTestAs(f testTy, pat f inner, f ty, tok)
        | TPatG.Null(ty, tok) -> TPatG.Null(f ty, tok)
        | TPatG.EnumCase(k, n, ty, tok) -> TPatG.EnumCase(k, n, f ty, tok)
        | TPatG.Or(alts, ty, tok) -> TPatG.Or(EqArray.map (pat f) alts, f ty, tok)

    let hole (f: 'a -> 'b) (h: HoleSpecG<'a, 'tok>) : HoleSpecG<'b, 'tok> =
        {
            Ty = f h.Ty
            Source = h.Source
            Tok = h.Tok
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
        // Both axes now carry an interface instantiation for the constrained-typar
        // case, so they are remapped through `f` alongside the enumerator type.
        | ForInEnumeratorG.Pattern(enumTy, ge, members, isVal, disp) ->
            ForInEnumeratorG.Pattern(f enumTy, forInGetEnum f ge, forInEnumMembers f members, isVal, disp)

    let rec expr (f: 'a -> 'b) (e: TExprG<'a, 'tok, 'id>) : TExprG<'b, 'tok, 'id> =
        let pe = expr f
        let pp = pat f
        let pa = arm f

        match e with
        | TExprG.Const(v, ty, tok) -> TExprG.Const(v, f ty, tok)
        | TExprG.Var(k, ty, tok) -> TExprG.Var(k, f ty, tok)
        | TExprG.External(n, k, ty, tok) -> TExprG.External(n, k, f ty, tok)
        | TExprG.Null(ty, tok) -> TExprG.Null(f ty, tok)
        | TExprG.Lambda(p, b, ty, tok) -> TExprG.Lambda(pp p, pe b, f ty, tok)
        | TExprG.App(fn, a, ty, tok) -> TExprG.App(pe fn, pe a, f ty, tok)
        | TExprG.Let(p, v, body, ty, tok) -> TExprG.Let(pp p, pe v, pe body, f ty, tok)
        | TExprG.Use(p, v, body, dispose, ty, tok) -> TExprG.Use(pp p, pe v, pe body, dispose, f ty, tok)
        | TExprG.IfThenElse(c, t, el, ty, tok) -> TExprG.IfThenElse(pe c, pe t, pe el, f ty, tok)
        | TExprG.Tuple(items, ty, tok) -> TExprG.Tuple(EqArray.map pe items, f ty, tok)
        | TExprG.Sequential(items, ty, tok) -> TExprG.Sequential(EqArray.map pe items, f ty, tok)
        | TExprG.While(c, b, ty, tok) -> TExprG.While(pe c, pe b, f ty, tok)
        | TExprG.ForTo(k, it, s, e2, b, ty, tok) -> TExprG.ForTo(k, it, pe s, pe e2, pe b, f ty, tok)
        | TExprG.ForIn(p, src, b, en, ty, tok) -> TExprG.ForIn(pp p, pe src, pe b, forInEnumerator f en, f ty, tok)
        | TExprG.Match(sc, arms, ty, tok) -> TExprG.Match(pe sc, EqArray.map pa arms, f ty, tok)
        | TExprG.TryWith(b, arms, ty, tok) -> TExprG.TryWith(pe b, EqArray.map pa arms, f ty, tok)
        | TExprG.TryFinally(b, c, ty, tok) -> TExprG.TryFinally(pe b, pe c, f ty, tok)
        | TExprG.Assignment(l, r, ty, tok) -> TExprG.Assignment(pe l, pe r, f ty, tok)
        | TExprG.Range(s, step, e2, ty, tok) -> TExprG.Range(pe s, Option.map pe step, pe e2, f ty, tok)
        | TExprG.RecordCons(fields, ty, tok) -> TExprG.RecordCons(EqArray.map (fun (n, v) -> n, pe v) fields, f ty, tok)
        | TExprG.RecordClone(src, ov, ty, tok) ->
            TExprG.RecordClone(pe src, EqArray.map (fun (n, v) -> n, pe v) ov, f ty, tok)
        | TExprG.FieldGet(r, n, ty, tok) -> TExprG.FieldGet(pe r, n, f ty, tok)
        | TExprG.FieldSet(r, n, v, ty, tok) -> TExprG.FieldSet(pe r, n, pe v, f ty, tok)
        | TExprG.UnionCons(c, args, ty, tok) -> TExprG.UnionCons(c, EqArray.map pe args, f ty, tok)
        | TExprG.New(c, k, args, ty, tok) -> TExprG.New(c, k, EqArray.map pe args, f ty, tok)
        | TExprG.MethodCall(r, k, via, args, ty, tok) ->
            TExprG.MethodCall(pe r, k, viaOf f via, EqArray.map pe args, f ty, tok)
        | TExprG.PropertyGet(r, k, via, ty, tok) -> TExprG.PropertyGet(pe r, k, viaOf f via, f ty, tok)
        | TExprG.StaticMethodCall(k, args, ty, tok) -> TExprG.StaticMethodCall(k, EqArray.map pe args, f ty, tok)
        | TExprG.StaticPropertyGet(k, ty, tok) -> TExprG.StaticPropertyGet(k, f ty, tok)
        | TExprG.StaticFieldGet(k, n, ty, tok) -> TExprG.StaticFieldGet(k, n, f ty, tok)
        | TExprG.StaticFieldSet(k, n, v, ty, tok) -> TExprG.StaticFieldSet(k, n, pe v, f ty, tok)
        | TExprG.ExternalMember(r, k, n, isProp, ty, tok) ->
            TExprG.ExternalMember(ValueOption.map pe r, k, n, isProp, f ty, tok)
        | TExprG.Format(sink, segs, ty, tok) -> TExprG.Format(sinkOf f sink, EqArray.map (segOf f) segs, f ty, tok)
        | TExprG.ILIntrinsic(op, operand, args, ty, tok) ->
            TExprG.ILIntrinsic(op, ValueOption.map f operand, EqArray.map pe args, f ty, tok)
        | TExprG.StaticOptimization(clauses, def, ty, tok) ->
            TExprG.StaticOptimization(EqArray.map (clause f) clauses, pe def, f ty, tok)
        | TExprG.Upcast(src, ty, tok) -> TExprG.Upcast(pe src, f ty, tok)
        | TExprG.Downcast(src, ty, tok) -> TExprG.Downcast(pe src, f ty, tok)
        | TExprG.TraitCall(recv, n, args, ty, tok) -> TExprG.TraitCall(f recv, n, EqArray.map pe args, f ty, tok)
        | TExprG.TypeTest(src, testTy, ty, tok) -> TExprG.TypeTest(pe src, f testTy, f ty, tok)

    and arm
        (f: 'a -> 'b)
        (a: TMatchArmG<TPatG<'a, 'tok, 'id>, TExprG<'a, 'tok, 'id>>)
        : TMatchArmG<TPatG<'b, 'tok, 'id>, TExprG<'b, 'tok, 'id>> =
        {
            Pat = pat f a.Pat
            Guard = ValueOption.map (expr f) a.Guard
            Body = expr f a.Body
        }

    and viaOf (f: 'a -> 'b) (v: CallVia<'a>) : CallVia<'b> =
        match v with
        | CallVia.Self -> CallVia.Self
        | CallVia.Base -> CallVia.Base
        | CallVia.Interface ifaceArgs -> CallVia.Interface(EqArray.map f ifaceArgs)

    and sinkOf (f: 'a -> 'b) (s: FormatSinkG<TExprG<'a, 'tok, 'id>>) : FormatSinkG<TExprG<'b, 'tok, 'id>> =
        match s with
        | FormatSinkG.ToStdOut nl -> FormatSinkG.ToStdOut nl
        | FormatSinkG.ToStdErr nl -> FormatSinkG.ToStdErr nl
        | FormatSinkG.ToWriter(w, nl) -> FormatSinkG.ToWriter(expr f w, nl)
        | FormatSinkG.ToBuilder w -> FormatSinkG.ToBuilder(expr f w)
        | FormatSinkG.ToString -> FormatSinkG.ToString

    and segOf
        (f: 'a -> 'b)
        (seg: FormatSegG<'a, 'tok, TExprG<'a, 'tok, 'id>>)
        : FormatSegG<'b, 'tok, TExprG<'b, 'tok, 'id>> =
        match seg with
        | FormatSegG.Lit lit -> FormatSegG.Lit lit
        | FormatSegG.Hole(h, a) -> FormatSegG.Hole(hole f h, expr f a)
        | FormatSegG.DynHole d ->
            FormatSegG.DynHole
                {
                    Width = ValueOption.map (expr f) d.Width
                    Precision = ValueOption.map (expr f) d.Precision
                    Spec = hole f d.Spec
                    Value = expr f d.Value
                }
        | FormatSegG.CallbackHole(spec, residue) -> FormatSegG.CallbackHole(hole f spec, expr f residue)

    and constraintOf (f: 'a -> 'b) (c: TStaticOptConstraintG<'a>) : TStaticOptConstraintG<'b> =
        match c with
        | TStaticOptConstraintG.TyconEquals(tp, req) -> TStaticOptConstraintG.TyconEquals(f tp, f req)
        | TStaticOptConstraintG.IsStruct tp -> TStaticOptConstraintG.IsStruct(f tp)

    and clause (f: 'a -> 'b) (c: TStaticOptClauseG<'a, 'tok, 'id>) : TStaticOptClauseG<'b, 'tok, 'id> =
        {
            Constraints = EqArray.map (constraintOf f) c.Constraints
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

    // ── the type-declaration cluster: a TRIFUNCTOR in `('ty, 'id, 'body)` ───
    //
    // Every function below maps the embedded types through `fTy`, the pattern-less binder
    // SLOTS through `fId`, and the member / preamble / ctor BODIES through `fBody`,
    // independently. The `'ty`-only freeze runs it at `fId = BinderKey.identity` and `fBody = expr fTy`
    // (`decl` below); a body-POOLING pass runs it at `fTy = id`, `fBody = <expr → pool id>`
    // and `fId = <key → binder id>`, and its inverse at the opposite two. So the seven body
    // slots, the six key slots, and the declaration spine around them are enumerated in ONE
    // place, and neither pooling direction is a second hand-written walk of this shape.
    //
    // `fId` sees a `BinderKeyG`, not a raw slot: a declaration's key slots are definition
    // sites by construction (`BinderKey.ofDeclSlot`), and passing the projection rather than
    // the bare identity is what keeps a re-filing pass on the sanctioned interning path.
    let typeMember
        (fTy: 'a -> 'b)
        (fId: BinderKeyG<'ia> -> 'ib)
        (fBody: 'ba -> 'bb)
        (m: TTypeMemberG<'a, 'ia, 'ba>)
        : TTypeMemberG<'b, 'ib, 'bb> =
        let slot k = fId (BinderKey.ofDeclSlot k)

        {
            Name = m.Name
            IsStatic = m.IsStatic
            Accessibility = m.Accessibility
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

    let ctorLet
        (fTy: 'a -> 'b)
        (fId: BinderKeyG<'ia> -> 'ib)
        (fBody: 'ba -> 'bb)
        (cl: TCtorLetG<'a, 'ia, 'ba>)
        : TCtorLetG<'b, 'ib, 'bb> =
        {
            Binder = fId (BinderKey.ofDeclSlot cl.Binder)
            Type = fTy cl.Type
            Init = fBody cl.Init
        }

    let ctorFieldInit (fBody: 'ba -> 'bb) (fi: TCtorFieldInitG<'ba>) : TCtorFieldInitG<'bb> =
        {
            Field = fi.Field
            Init = fBody fi.Init
        }

    let secondaryCtor
        (fTy: 'a -> 'b)
        (fId: BinderKeyG<'ia> -> 'ib)
        (fBody: 'ba -> 'bb)
        (sc: TSecondaryCtorG<'a, 'ia, 'ba>)
        : TSecondaryCtorG<'b, 'ib, 'bb> =
        {
            Params = EqArray.map (fun (k, ty) -> fId (BinderKey.ofDeclSlot k), fTy ty) sc.Params
            Lets = EqArray.map (ctorLet fTy fId fBody) sc.Lets
            PrimaryArgs = EqArray.map fBody sc.PrimaryArgs
            FieldInits = EqArray.map (ctorFieldInit fBody) sc.FieldInits
        }

    let baseCtorCall
        (fTy: 'a -> 'b)
        (fId: BinderKeyG<'ia> -> 'ib)
        (fBody: 'ba -> 'bb)
        (bc: TBaseCtorCallG<'a, 'ia, 'ba>)
        : TBaseCtorCallG<'b, 'ib, 'bb> =
        {
            CtorParams = EqArray.map (fun (k, ty) -> fId (BinderKey.ofDeclSlot k), fTy ty) bc.CtorParams
            Args = EqArray.map fBody bc.Args
            ChosenCtor = bc.ChosenCtor
        }

    let abstractMethod (f: 'a -> 'b) (am: TAbstractMethodG<'a>) : TAbstractMethodG<'b> =
        {
            Name = am.Name
            MethodTypeParams = am.MethodTypeParams
            Signature = f am.Signature
            IsProperty = am.IsProperty
        }

    let kind
        (fTy: 'a -> 'b)
        (fId: BinderKeyG<'ia> -> 'ib)
        (fBody: 'ba -> 'bb)
        (k: TTypeKindG<'a, 'tok, 'ia, 'ba>)
        : TTypeKindG<'b, 'tok, 'ib, 'bb> =
        let mem = typeMember fTy fId fBody
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
        // kind passes through unchanged whichever mapping is running.
        | TTypeKindG.Enum cases -> TTypeKindG.Enum cases
        | TTypeKindG.Class c ->
            TTypeKindG.Class
                {
                    Fields = EqArray.map (recordField fTy) c.Fields
                    CtorParams = EqArray.map (recordField fTy) c.CtorParams
                    Members = EqArray.map mem c.Members
                    BaseType = ValueOption.map fTy c.BaseType
                    Interfaces = ifaces c.Interfaces
                    IsSealed = c.IsSealed
                    StaticPreamble = EqArray.map (preambleEntry fTy fBody) c.StaticPreamble
                    InstancePreamble = EqArray.map (preambleEntry fTy fBody) c.InstancePreamble
                    ThisKey = fId (BinderKey.ofDeclSlot c.ThisKey)
                    SecondaryCtors = EqArray.map (secondaryCtor fTy fId fBody) c.SecondaryCtors
                    BaseCtorCall = ValueOption.map (baseCtorCall fTy fId fBody) c.BaseCtorCall
                    ValueKind = c.ValueKind
                    HasPrimaryCtor = c.HasPrimaryCtor
                }

    let typeDecl
        (fTy: 'a -> 'b)
        (fId: BinderKeyG<'ia> -> 'ib)
        (fBody: 'ba -> 'bb)
        (td: TTypeDeclG<'a, 'tok, 'ia, 'ba>)
        : TTypeDeclG<'b, 'tok, 'ib, 'bb> =
        {
            Name = td.Name
            TypeKey = td.TypeKey
            Namespace = td.Namespace
            TypeParams = td.TypeParams
            IsRequireQualifiedAccess = td.IsRequireQualifiedAccess
            Kind = kind fTy fId fBody td.Kind
            EqualitySupport = td.EqualitySupport
            ComparisonSupport = td.ComparisonSupport
        }

    let decl (f: 'a -> 'b) (d: TDeclG<'a, 'tok, 'id>) : TDeclG<'b, 'tok, 'id> =
        match d with
        | TDeclG.Let(binding, value, isInline, ty) -> TDeclG.Let(pat f binding, expr f value, isInline, f ty)
        | TDeclG.Expression(e, ty) -> TDeclG.Expression(expr f e, f ty)
        // The `'ty`-only conversion is the trifunctor at `fId = identity` (the slots stay in
        // the space they were in) and `fBody = expr f`.
        | TDeclG.Type td -> TDeclG.Type(typeDecl f BinderKey.identity (expr f) td)

    let inlineBody (f: 'a -> 'b) (ib: TInlineBodyG<'a, 'tok, 'id>) : TInlineBodyG<'b, 'tok, 'id> =
        {
            Decl = decl f ib.Decl
            ParamAttrs = ib.ParamAttrs
        }

    // The compiled-form cluster, likewise a bifunctor — in `('ty, 'pat)`. The `'ty`-only
    // conversion runs it at `fPat = pat fTy`; the file's own `ValRepr`s are POOLED by
    // running it at `fTy = id` and `fPat = <pat → pool id>`, and rebuilt by its inverse.
    let argGroup (fTy: 'a -> 'b) (fPat: 'pa -> 'pb) (g: ArgGroupG<'a, 'pa>) : ArgGroupG<'b, 'pb> =
        match g with
        | ArgGroupG.GUnit ty -> ArgGroupG.GUnit(fTy ty)
        | ArgGroupG.GSimple(slot, ty) -> ArgGroupG.GSimple(slot, fTy ty)
        | ArgGroupG.GTuple p -> ArgGroupG.GTuple(fPat p)

    let valRepr (fTy: 'a -> 'b) (fPat: 'pa -> 'pb) (vr: ValReprG<'a, 'pa>) : ValReprG<'b, 'pb> =
        {
            Typars = vr.Typars
            Groups = vr.Groups |> List.map (argGroup fTy fPat)
            ResultTy = fTy vr.ResultTy
        }

    let inlineValue (f: 'a -> 'b) (iv: TInlineValueG<'a, 'tok, 'id>) : TInlineValueG<'b, 'tok, 'id> =
        {
            Key = iv.Key
            Body = inlineBody f iv.Body
        }

    /// The whole-file rebuild: `Decls` and `InlineBodies` mapped through `f`, the
    /// non-`'ty` snapshot fields (`Diagnostics` / `IntrinsicReprKeys` /
    /// `ModuleMembers` / `ClosureReprs`) carried over.
    let file (f: 'a -> 'b) (tf: TastFileG<'a, 'tok, 'id>) : TastFileG<'b, 'tok, 'id> =
        {
            Decls = EqArray.map (decl f) tf.Decls
            InlineBodies = EqArray.map (inlineValue f) tf.InlineBodies
            Diagnostics = tf.Diagnostics
            IntrinsicReprKeys = tf.IntrinsicReprKeys
            ModuleMembers = tf.ModuleMembers
            TopLevelNames = tf.TopLevelNames
            ClosureReprs = tf.ClosureReprs
            FunVerdicts = tf.FunVerdicts
            GenericFnSchemes = tf.GenericFnSchemes
            // `'ty`-free snapshot fields carried verbatim, like `IntrinsicReprKeys`.
            Accessibility = tf.Accessibility
            BindingTyparArities = tf.BindingTyparArities
        }
