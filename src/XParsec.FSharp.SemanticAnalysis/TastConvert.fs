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

    let rec pat (f: 'a -> 'b) (p: TPatG<'a, 'tok>) : TPatG<'b, 'tok> =
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

    let rec expr (f: 'a -> 'b) (e: TExprG<'a, 'tok>) : TExprG<'b, 'tok> =
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

    and arm (f: 'a -> 'b) (a: TMatchArmG<'a, 'tok>) : TMatchArmG<'b, 'tok> =
        {
            Pat = pat f a.Pat
            Guard = Option.map (expr f) a.Guard
            Body = expr f a.Body
        }

    and viaOf (f: 'a -> 'b) (v: CallVia<'a>) : CallVia<'b> =
        match v with
        | CallVia.Self -> CallVia.Self
        | CallVia.Base -> CallVia.Base
        | CallVia.Interface ifaceArgs -> CallVia.Interface(EqArray.map f ifaceArgs)

    and sinkOf (f: 'a -> 'b) (s: FormatSinkG<'a, 'tok>) : FormatSinkG<'b, 'tok> =
        match s with
        | FormatSinkG.ToStdOut nl -> FormatSinkG.ToStdOut nl
        | FormatSinkG.ToStdErr nl -> FormatSinkG.ToStdErr nl
        | FormatSinkG.ToWriter(w, nl) -> FormatSinkG.ToWriter(expr f w, nl)
        | FormatSinkG.ToBuilder w -> FormatSinkG.ToBuilder(expr f w)
        | FormatSinkG.ToString -> FormatSinkG.ToString

    and segOf (f: 'a -> 'b) (seg: FormatSegG<'a, 'tok>) : FormatSegG<'b, 'tok> =
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

    and clause (f: 'a -> 'b) (c: TStaticOptClauseG<'a, 'tok>) : TStaticOptClauseG<'b, 'tok> =
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

    let typeMember (f: 'a -> 'b) (m: TTypeMemberG<'a, TExprG<'a, 'tok>>) : TTypeMemberG<'b, TExprG<'b, 'tok>> =
        {
            Name = m.Name
            IsStatic = m.IsStatic
            Accessibility = m.Accessibility
            Kind = m.Kind
            IsOverride = m.IsOverride
            ThisKey = m.ThisKey
            BaseKey = m.BaseKey
            ThisTy = f m.ThisTy
            Params = EqArray.map (fun (k, ty) -> k, f ty) m.Params
            Body = expr f m.Body
            ReturnTy = f m.ReturnTy
            MethodTypeParams = EqArray.map (fun (n, ty) -> n, f ty) m.MethodTypeParams
        }

    let classLet (f: 'a -> 'b) (l: TClassLetG<'a, TExprG<'a, 'tok>>) : TClassLetG<'b, TExprG<'b, 'tok>> =
        {
            Name = l.Name
            Type = f l.Type
            IsMutable = l.IsMutable
            Init = expr f l.Init
        }

    let preambleEntry (f: 'a -> 'b) (p: TPreambleEntryG<'a, TExprG<'a, 'tok>>) : TPreambleEntryG<'b, TExprG<'b, 'tok>> =
        match p with
        | TPreambleEntryG.Let l -> TPreambleEntryG.Let(classLet f l)
        | TPreambleEntryG.Do e -> TPreambleEntryG.Do(expr f e)

    let ctorLet (f: 'a -> 'b) (cl: TCtorLetG<'a, TExprG<'a, 'tok>>) : TCtorLetG<'b, TExprG<'b, 'tok>> =
        {
            Binder = cl.Binder
            Type = f cl.Type
            Init = expr f cl.Init
        }

    let ctorFieldInit (f: 'a -> 'b) (fi: TCtorFieldInitG<TExprG<'a, 'tok>>) : TCtorFieldInitG<TExprG<'b, 'tok>> =
        {
            Field = fi.Field
            Init = expr f fi.Init
        }

    let secondaryCtor
        (f: 'a -> 'b)
        (sc: TSecondaryCtorG<'a, TExprG<'a, 'tok>>)
        : TSecondaryCtorG<'b, TExprG<'b, 'tok>> =
        {
            Params = EqArray.map (fun (k, ty) -> k, f ty) sc.Params
            Lets = EqArray.map (ctorLet f) sc.Lets
            PrimaryArgs = EqArray.map (expr f) sc.PrimaryArgs
            FieldInits = EqArray.map (ctorFieldInit f) sc.FieldInits
        }

    let baseCtorCall (f: 'a -> 'b) (bc: TBaseCtorCallG<'a, TExprG<'a, 'tok>>) : TBaseCtorCallG<'b, TExprG<'b, 'tok>> =
        {
            CtorParams = EqArray.map (fun (k, ty) -> k, f ty) bc.CtorParams
            Args = EqArray.map (expr f) bc.Args
            ChosenCtor = bc.ChosenCtor
        }

    let abstractMethod (f: 'a -> 'b) (am: TAbstractMethodG<'a>) : TAbstractMethodG<'b> =
        {
            Name = am.Name
            MethodTypeParams = am.MethodTypeParams
            Signature = f am.Signature
            IsProperty = am.IsProperty
        }

    let kind (f: 'a -> 'b) (k: TTypeKindG<'a, 'tok, TExprG<'a, 'tok>>) : TTypeKindG<'b, 'tok, TExprG<'b, 'tok>> =
        match k with
        | TTypeKindG.Interface methods -> TTypeKindG.Interface(EqArray.map (abstractMethod f) methods)
        | TTypeKindG.Union(cases, members, interfaces) ->
            TTypeKindG.Union(
                EqArray.map (unionCase f) cases,
                EqArray.map (typeMember f) members,
                EqArray.map (fun (ity, ms) -> f ity, EqArray.map (typeMember f) ms) interfaces
            )
        | TTypeKindG.Record(fields, members, interfaces, valueKind) ->
            TTypeKindG.Record(
                EqArray.map (recordField f) fields,
                EqArray.map (typeMember f) members,
                EqArray.map (fun (ity, ms) -> f ity, EqArray.map (typeMember f) ms) interfaces,
                valueKind
            )
        // Enum cases carry no `'ty` (the value is a resolved literal), so the kind
        // passes through the SemType→FrozenType convert unchanged.
        | TTypeKindG.Enum cases -> TTypeKindG.Enum cases
        | TTypeKindG.Class c ->
            TTypeKindG.Class
                {
                    Fields = EqArray.map (recordField f) c.Fields
                    CtorParams = EqArray.map (recordField f) c.CtorParams
                    Members = EqArray.map (typeMember f) c.Members
                    BaseType = ValueOption.map f c.BaseType
                    Interfaces = EqArray.map (fun (ity, ms) -> f ity, EqArray.map (typeMember f) ms) c.Interfaces
                    IsSealed = c.IsSealed
                    StaticPreamble = EqArray.map (preambleEntry f) c.StaticPreamble
                    InstancePreamble = EqArray.map (preambleEntry f) c.InstancePreamble
                    ThisKey = c.ThisKey
                    SecondaryCtors = EqArray.map (secondaryCtor f) c.SecondaryCtors
                    BaseCtorCall = ValueOption.map (baseCtorCall f) c.BaseCtorCall
                    ValueKind = c.ValueKind
                    HasPrimaryCtor = c.HasPrimaryCtor
                }

    let typeDecl (f: 'a -> 'b) (td: TTypeDeclG<'a, 'tok, TExprG<'a, 'tok>>) : TTypeDeclG<'b, 'tok, TExprG<'b, 'tok>> =
        {
            Name = td.Name
            TypeKey = td.TypeKey
            Namespace = td.Namespace
            TypeParams = td.TypeParams
            IsRequireQualifiedAccess = td.IsRequireQualifiedAccess
            Kind = kind f td.Kind
            EqualitySupport = td.EqualitySupport
            ComparisonSupport = td.ComparisonSupport
        }

    let decl (f: 'a -> 'b) (d: TDeclG<'a, 'tok>) : TDeclG<'b, 'tok> =
        match d with
        | TDeclG.Let(binding, value, isInline, ty) -> TDeclG.Let(pat f binding, expr f value, isInline, f ty)
        | TDeclG.Expression(e, ty) -> TDeclG.Expression(expr f e, f ty)
        | TDeclG.Type td -> TDeclG.Type(typeDecl f td)

    let inlineBody (f: 'a -> 'b) (ib: TInlineBodyG<'a, 'tok>) : TInlineBodyG<'b, 'tok> =
        {
            Decl = decl f ib.Decl
            ParamAttrs = ib.ParamAttrs
        }

    let argGroup (f: 'a -> 'b) (g: ArgGroupG<'a, 'tok>) : ArgGroupG<'b, 'tok> =
        match g with
        | ArgGroupG.GUnit ty -> ArgGroupG.GUnit(f ty)
        | ArgGroupG.GSimple(slot, ty) -> ArgGroupG.GSimple(slot, f ty)
        | ArgGroupG.GTuple p -> ArgGroupG.GTuple(pat f p)

    let valRepr (f: 'a -> 'b) (vr: ValReprG<'a, 'tok>) : ValReprG<'b, 'tok> =
        {
            Typars = vr.Typars
            Groups = vr.Groups |> List.map (argGroup f)
            ResultTy = f vr.ResultTy
        }

    let inlineValue (f: 'a -> 'b) (iv: TInlineValueG<'a, 'tok>) : TInlineValueG<'b, 'tok> =
        {
            Key = iv.Key
            Body = inlineBody f iv.Body
        }

    /// The whole-file rebuild: `Decls` and `InlineBodies` mapped through `f`, the
    /// non-`'ty` snapshot fields (`Diagnostics` / `IntrinsicReprKeys` /
    /// `ModuleMembers` / `ClosureReprs`) carried over.
    let file (f: 'a -> 'b) (tf: TastFileG<'a, 'tok>) : TastFileG<'b, 'tok> =
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
            // Generic in `'ty`; converted per-entry. Empty pre-freeze (`Freeze.run`
            // fills the frozen table from the frozen lambda spines), so this is an
            // identity on the empty map on the one path that runs it.
            BindingValReprs = tf.BindingValReprs |> Map.map (fun _ vr -> valRepr f vr)
        }
