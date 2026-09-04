namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

// Every type position a CST node WRITES, walked through one `TypeIter`. What a visitor does
// with each is its own.

module CstTypeWalk =

    /// `VisitType` fires on every `Type` node before its children; returning `false`
    /// skips the default child recursion. `VisitMeasureName` fires on every measure ATOM
    /// (`m` in `<m/s>`), a type reference carried outside any `Type` node.
    [<NoEquality; NoComparison>]
    type TypeIter =
        {
            VisitType: TypeIter -> Type<SyntaxToken> -> bool
            VisitMeasureName: LongIdent<SyntaxToken> -> unit
        }

    let identityTypeIter: TypeIter =
        {
            VisitType = fun _ _ -> true
            VisitMeasureName = ignore
        }

    /// Every name a measure term applies.
    let rec iterMeasure (it: TypeIter) (m: Measure<SyntaxToken>) : unit =
        match m with
        | Measure.Named li -> it.VisitMeasureName li
        | Measure.Power(inner, _, _, _)
        | Measure.Reciprocal(_, inner)
        | Measure.Paren(_, inner, _) -> iterMeasure it inner
        | Measure.Product(l, _, r)
        | Measure.Quotient(l, _, r) ->
            iterMeasure it l
            iterMeasure it r
        | Measure.Juxtaposition(elems, _) ->
            for e in elems do
                iterMeasure it e
        | Measure.One _
        | Measure.Anonymous _
        | Measure.Typar _ -> ()

    let rec iterType (it: TypeIter) (ty: Type<SyntaxToken>) : unit =
        if it.VisitType it ty then
            let walk = iterType it

            match ty with
            | Type.ParenType(typ = inner)
            | Type.SuffixedType(baseType = inner)
            | Type.DottedType(baseType = inner)
            | Type.ArrayType(baseType = inner)
            | Type.AnonymousSubtype(typ = inner)
            | Type.SubtypeConstraint(typ = inner) -> walk inner
            | Type.FunctionType(fromType = f; toType = t) ->
                walk f
                walk t
            | Type.TupleType(types = ts)
            | Type.StructTupleType(types = ts) ->
                for t in ts do
                    walk t
            | Type.GenericType(typeArgs = args) ->
                for a in args do
                    match a with
                    | TypeArg.Type at -> walk at
                    | TypeArg.Measure m -> iterMeasure it m
            | Type.WhenConstrainedType(typ = inner; constraints = cs) ->
                walk inner
                iterTypeConstraints it cs
            | Type.UnionType(left = l; right = r) ->
                walk l
                walk r
            | Type.AnonRecordType(fields = fs) ->
                for AnonRecordField(typ = t) in fs do
                    walk t
            | Type.MeasureType m -> iterMeasure it m
            // Leaves: no nested `Type`.
            | Type.VarType _
            | Type.NamedType _
            | Type.Null _
            | Type.ILIntrinsic _
            | Type.Missing
            | Type.SkipsTokens _ -> ()

    and iterTypeConstraints (it: TypeIter) (cs: TyparConstraints<SyntaxToken>) : unit =
        for c in cs.Constraints do
            iterTypeConstraint it c

    and iterTypeConstraint (it: TypeIter) (c: Constraint<SyntaxToken>) : unit =
        match c with
        | Constraint.Coercion(typ = t)
        | Constraint.Enum(typ = t)
        | Constraint.Default(typ = t) -> iterType it t
        | Constraint.Delegate(type1 = t1; type2 = t2) ->
            iterType it t1
            iterType it t2
        | Constraint.MemberTrait(membersign = ms) -> iterTypeMemberSig it ms
        // Constraints with no embedded `Type`.
        | Constraint.Nullness _
        | Constraint.DefaultConstructor _
        | Constraint.Struct _
        | Constraint.ReferenceType _
        | Constraint.NotNull _
        | Constraint.Unmanaged _
        | Constraint.Equality _
        | Constraint.Comparison _ -> ()

    and iterTypeMemberSig (it: TypeIter) (ms: MemberSig<SyntaxToken>) : unit =
        match ms with
        | MemberSig.MethodOrPropSig(sign = cs)
        | MemberSig.PropSig(sign = cs) -> iterTypeCurriedSig it cs

    and iterTypeCurriedSig (it: TypeIter) (cs: CurriedSig<SyntaxToken>) : unit =
        let (CurriedSig(args = argGroups; returnType = ret)) = cs

        for struct (argsSpec, _) in argGroups do
            let (ArgsSpec.ArgsSpec(args = args)) = argsSpec

            for (ArgSpec(typ = t)) in args do
                iterType it t

        iterType it ret

    /// An uncurried signature: a `DelegateSig`, or a GADT-syntax union case's
    /// `Name : arg * arg -> ret`.
    let iterTypeUncurriedSig (it: TypeIter) (sign: UncurriedSig<SyntaxToken>) : unit =
        let (UncurriedSig(args = ArgsSpec.ArgsSpec(args = args); returnType = ret)) = sign

        for (ArgSpec(typ = t)) in args do
            iterType it t

        iterType it ret

    /// The type positions ONE union case writes, in either grammar: its field types, or the
    /// signature a GADT-syntax case spells them with.
    let iterTypeUnionCase (it: TypeIter) (UnionTypeCase(data = data)) : unit =
        match data with
        | UnionTypeCaseData.Nullary _ -> ()
        | UnionTypeCaseData.Nary(fields = fs) ->
            for f in fs do
                match f with
                | UnionTypeField.Unnamed(typ = t)
                | UnionTypeField.Named(typ = t) -> iterType it t
        | UnionTypeCaseData.GadtNary(sign = s) -> iterTypeUncurriedSig it s
        | UnionTypeCaseData.GadtNullary(typ = t) -> iterType it t

    /// The header `when` clause of a `TypeName`, reachable from no field, member or parameter.
    let iterTypeNameConstraints (it: TypeIter) (tn: TypeName<SyntaxToken>) : unit =
        let (TypeName(typarDefns = tds; postfixConstraints = post)) = tn

        match tds with
        | ValueSome(TyparDefns(constraints = ValueSome cs)) -> iterTypeConstraints it cs
        | _ -> ()

        match post with
        | ValueSome cs -> iterTypeConstraints it cs
        | ValueNone -> ()

    let iterBindingReturnType (onType: Type<SyntaxToken> -> unit) (b: Binding<SyntaxToken>) : unit =
        match b.returnType with
        | ValueSome(ReturnType(typ = t)) -> onType t
        | ValueNone -> ()

    /// The type positions ONE member definition's SIGNATURE writes: a member's return
    /// annotation, an auto-property's type, an abstract slot's signature and a `val` field's
    /// type. Argument annotations, and a secondary constructor's parameters, are
    /// pattern-embedded and go to `onPat`.
    let iterMemberDefnSigTypes (it: TypeIter) (onPat: Pat<SyntaxToken> -> unit) (md: MemberDefn<SyntaxToken>) : unit =
        let onType = iterType it
        let onMemberSig = iterTypeMemberSig it

        let bindingSig (b: Binding<SyntaxToken>) =
            for ap in b.argumentPats do
                onPat ap

            iterBindingReturnType onType b

        match md with
        | MemberDefn.Member(defn = d) ->
            match d with
            | MethodOrPropDefn.Method(defn = b)
            | MethodOrPropDefn.Property(defn = b) -> bindingSig b
            | MethodOrPropDefn.PropertyWithGetSet(defns = bs) ->
                for b in bs do
                    bindingSig b
            | MethodOrPropDefn.AutoProperty(returnType = ValueSome(ReturnType(typ = t))) -> onType t
            | MethodOrPropDefn.AutoProperty _ -> ()
            | MethodOrPropDefn.AbstractSignature sign -> onMemberSig sign
        | MemberDefn.Value(typ = t) -> onType t
        | MemberDefn.AdditionalConstructor(pat = p) -> onPat p

    /// The type positions a `type` definition's DECLARED STRUCTURE writes; member bodies
    /// are not part of it.
    let iterTypeDefnTypes
        (it: TypeIter)
        (onPat: Pat<SyntaxToken> -> unit)
        (onInherit: Type<SyntaxToken> -> unit)
        (td: TypeDefn<SyntaxToken>)
        : unit =
        let ty (t: Type<SyntaxToken>) = iterType it t

        let memberDefn (md: MemberDefn<SyntaxToken>) = iterMemberDefnSigTypes it onPat md

        let element (el: TypeDefnElement<SyntaxToken>) =
            match el with
            | TypeDefnElement.Member md -> memberDefn md
            | TypeDefnElement.InterfaceImpl(InterfaceImpl.InterfaceImpl(typ = t; objectMembers = oms)) ->
                ty t

                match oms with
                | ValueSome(ObjectMembers(memberDefns = mds)) ->
                    for md in mds do
                        memberDefn md
                | ValueNone -> ()
            | TypeDefnElement.InterfaceSpec(InterfaceSpec.InterfaceSpec(typ = t)) -> ty t
            | TypeDefnElement.Inherit(ClassInheritsDecl(typ = t)) -> onInherit t

        // A `[static] let` in a class preamble is a BODY, not declared structure, so only its
        // return annotation is part of the type's surface.
        let preamble (d: ClassFunctionOrValueDefn<SyntaxToken>) =
            match d with
            | ClassFunctionOrValueDefn.LetBindings(bindings = bs) ->
                for b in bs do
                    iterBindingReturnType ty b
            | ClassFunctionOrValueDefn.Do _ -> ()

        let body (b: ObjectModelBody<SyntaxToken>) =
            match b.inherits with
            | ValueSome(ClassInheritsDecl(typ = t)) -> onInherit t
            | ValueNone -> ()

            for d in b.classPreamble do
                preamble d

            for el in b.elements do
                element el

        let extensions (ext: TypeExtensionElements<SyntaxToken> voption) =
            match ext with
            | ValueSome(TypeExtensionElements(elements = els)) ->
                for el in els do
                    element el
            | ValueNone -> ()

        let headerConstraints (tn: TypeName<SyntaxToken>) = iterTypeNameConstraints it tn

        match td with
        | TypeDefn.Abbrev(typeName = tn)
        | TypeDefn.Record(typeName = tn)
        | TypeDefn.Union(typeName = tn)
        | TypeDefn.Anon(typeName = tn)
        | TypeDefn.Class(typeName = tn)
        | TypeDefn.Struct(typeName = tn)
        | TypeDefn.Interface(typeName = tn)
        | TypeDefn.Delegate(typeName = tn)
        | TypeDefn.TypeExtension(typeName = tn)
        | TypeDefn.Enum(typeName = tn)
        | TypeDefn.AbstractType(typeName = tn) -> headerConstraints tn
        | TypeDefn.Missing
        | TypeDefn.SkipsTokens _ -> ()

        match td with
        | TypeDefn.Abbrev(typ = t; extensions = ext) ->
            ty t
            extensions ext
        | TypeDefn.Record(fields = fs; extensions = ext) ->
            for RecordField(typ = t) in fs do
                ty t

            extensions ext
        | TypeDefn.Union(cases = cs; extensions = ext) ->
            for c in cs do
                iterTypeUnionCase it c

            extensions ext
        | TypeDefn.Anon(primaryConstr = pc; body = b)
        | TypeDefn.Class(primaryConstr = pc; body = b)
        | TypeDefn.Struct(primaryConstr = pc; body = b) ->
            // Primary-constructor parameter annotations (`type Point(x: int, …)`) are
            // pattern-embedded.
            match pc with
            | ValueSome(PrimaryConstrArgs(pat = ValueSome p)) -> onPat p
            | _ -> ()

            body b
        | TypeDefn.Interface(body = b) -> body b
        | TypeDefn.Delegate(sign = DelegateSig(sign = s)) -> iterTypeUncurriedSig it s
        | TypeDefn.TypeExtension(elements = TypeExtensionElements(elements = els)) ->
            for el in els do
                element el
        | TypeDefn.Enum _
        | TypeDefn.AbstractType _
        | TypeDefn.Missing
        | TypeDefn.SkipsTokens _ -> ()

    /// The type positions a signature body's STRUCTURE writes: its base, its interfaces and
    /// its `val` fields. A member signature is not structure, and is walked per member.
    let iterTypeElementsSignatureStructure (it: TypeIter) (els: TypeElementsSignature<SyntaxToken>) : unit =
        for el in els do
            match el with
            | TypeSignatureElement.Interface(InterfaceSpec.InterfaceSpec(typ = t))
            | TypeSignatureElement.Value(typ = t)
            | TypeSignatureElement.Inherit(ClassInheritsDecl(typ = t)) -> iterType it t
            | TypeSignatureElement.Constructor _
            | TypeSignatureElement.Member _
            | TypeSignatureElement.Abstract _
            | TypeSignatureElement.Override _
            | TypeSignatureElement.Default _
            | TypeSignatureElement.StaticMember _ -> ()

    /// Every type position a `val` signature writes.
    let iterValSigTypes (it: TypeIter) (ValSig(typars = tds; signature = cs)) : unit =
        match tds with
        | ValueSome(TyparDefns(constraints = ValueSome tcs)) -> iterTypeConstraints it tcs
        | _ -> ()

        iterTypeCurriedSig it cs
