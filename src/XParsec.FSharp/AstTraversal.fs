module XParsec.FSharp.AstTraversal

open XParsec.FSharp
open XParsec.FSharp.Parser

/// Callbacks invoked by AST walk functions in source order.
/// EnterSection/ExitSection bracket structural sections; an empty name means
/// indent-only (no header line is emitted by a typical debug visitor).
type AstVisitor<'T> =
    {
        /// Called for every leaf token; the string is the role label (e.g. "if", "then", "=").
        VisitToken: string -> 'T -> unit
        /// Called on entering a structural section. Empty name = indent only, no header.
        EnterSection: string -> unit
        /// Called on exiting a section entered via EnterSection.
        ExitSection: string -> unit
        /// Called for literal text output (e.g. "Missing", unimplemented stubs).
        WriteLine: string -> unit
        /// Token equality used to suppress duplicate token output
        /// (e.g. Typar.Named where quote and ident are the same lexed token).
        EqualTokens: 'T -> 'T -> bool
    }

// ---------------------------------------------------------------------------
// Private helper — not part of the mutual recursion group
// ---------------------------------------------------------------------------

let inline visitTokenOpt (visitor: AstVisitor<'T>) (label: string) (token: 'T voption) : unit =
    match token with
    | ValueSome t -> visitor.VisitToken label t
    | ValueNone -> ()

let walkAccess (visitor: AstVisitor<'T>) (access: Access<'T>) : unit =
    match access with
    | Access.Private t
    | Access.Internal t
    | Access.Public t -> visitor.VisitToken "access" t

let inline walkAccessOpt (visitor: AstVisitor<'T>) (access: Access<'T> voption) : unit =
    match access with
    | ValueSome a -> walkAccess visitor a
    | ValueNone -> ()

let parenKind lParen =
    match lParen with
    | ParenKind.Paren t -> "ParenBlock", t
    | ParenKind.BeginEnd t -> "BeginEndBlock", t
    | ParenKind.List t -> "List", t
    | ParenKind.Array t -> "Array", t
    | ParenKind.Brace t -> "ComputationBlock", t
    | ParenKind.BraceBar t -> "AnonRecordBlock", t
    | ParenKind.Quoted t -> "<@", t
    | ParenKind.DoubleQuoted t -> "<@@", t

// ---------------------------------------------------------------------------
// Main walk functions — one big let rec / and block
// ---------------------------------------------------------------------------

let rec walkConstant (visitor: AstVisitor<'T>) (label: string) (x: Constant<'T>) : unit =
    match x with
    | Constant.Literal value -> visitor.VisitToken label value
    | Constant.MeasuredLiteral(value, lAngle, measure, rAngle) ->
        visitor.EnterSection label
        visitor.VisitToken "Literal" value
        visitor.VisitToken "<" lAngle
        visitor.EnterSection "Measure"
        walkMeasure visitor measure
        visitor.ExitSection "Measure"
        visitor.VisitToken ">" rAngle
        visitor.ExitSection label

and walkMeasure (visitor: AstVisitor<'T>) (measure: Measure<'T>) : unit =
    match measure with
    | Measure.Named longIdent ->
        visitor.EnterSection "Measure.Named"
        walkLongIdentOrOp visitor (LongIdentOrOp.LongIdent longIdent)
        visitor.ExitSection "Measure.Named"
    | Measure.One oneToken -> visitor.VisitToken "Measure.One" oneToken
    | Measure.Anonymous underscore -> visitor.VisitToken "Measure.Anonymous" underscore
    | Measure.Typar typar ->
        visitor.EnterSection "Measure.Typar"
        walkTypar visitor typar
        visitor.ExitSection "Measure.Typar"
    | Measure.Juxtaposition(measures, _ops) ->
        visitor.EnterSection "Measure.Juxtaposition"

        for m in measures do
            walkMeasure visitor m

        visitor.ExitSection "Measure.Juxtaposition"
    | Measure.Power(baseMeasure, powerToken, neg, exponent) ->
        visitor.EnterSection "Measure.Power"
        walkMeasure visitor baseMeasure
        visitor.VisitToken "" powerToken

        match neg with
        | ValueSome n -> visitor.VisitToken "" n
        | ValueNone -> ()

        visitor.VisitToken "" exponent
        visitor.ExitSection "Measure.Power"
    | Measure.Product(left, mulToken, right) ->
        visitor.EnterSection "Measure.Product"
        walkMeasure visitor left
        visitor.VisitToken "" mulToken
        walkMeasure visitor right
        visitor.ExitSection "Measure.Product"
    | Measure.Quotient(numerator, divToken, denominator) ->
        visitor.EnterSection "Measure.Quotient"
        walkMeasure visitor numerator
        visitor.VisitToken "" divToken
        walkMeasure visitor denominator
        visitor.ExitSection "Measure.Quotient"
    | Measure.Reciprocal(divToken, measure) ->
        visitor.EnterSection "Measure.Reciprocal"
        visitor.VisitToken "" divToken
        walkMeasure visitor measure
        visitor.ExitSection "Measure.Reciprocal"
    | Measure.Paren(lParen, measure, rParen) ->
        visitor.EnterSection "Measure.Paren"
        visitor.VisitToken "" lParen
        walkMeasure visitor measure
        visitor.VisitToken "" rParen
        visitor.ExitSection "Measure.Paren"

and walkAttributeTarget (visitor: AstVisitor<'T>) (target: AttributeTarget<'T>) : unit =
    match target with
    | AttributeTarget.Assembly t -> visitor.VisitToken "target" t
    | AttributeTarget.Module t -> visitor.VisitToken "target" t
    | AttributeTarget.Return t -> visitor.VisitToken "target" t
    | AttributeTarget.Field t -> visitor.VisitToken "target" t
    | AttributeTarget.Property t -> visitor.VisitToken "target" t
    | AttributeTarget.Param t -> visitor.VisitToken "target" t
    | AttributeTarget.Type t -> visitor.VisitToken "target" t
    | AttributeTarget.Constructor t -> visitor.VisitToken "target" t
    | AttributeTarget.Event t -> visitor.VisitToken "target" t

and walkObjectConstruction (visitor: AstVisitor<'T>) (oc: ObjectConstruction<'T>) : unit =
    match oc with
    | ObjectConstruction.ObjectConstruction(typ, expr) ->
        walkType visitor typ
        walkExpr visitor expr
    | ObjectConstruction.InterfaceConstruction typ -> walkType visitor typ

and walkAttribute (visitor: AstVisitor<'T>) (attr: Attribute<'T>) : unit =
    let (Attribute.Attribute(target, construction)) = attr

    match target with
    | ValueSome(attrTarget, colon) ->
        walkAttributeTarget visitor attrTarget
        visitor.VisitToken ":" colon
    | ValueNone -> ()

    walkObjectConstruction visitor construction

and walkAttributeSet (visitor: AstVisitor<'T>) (attrSet: AttributeSet<'T>) : unit =
    let (AttributeSet.AttributeSet(lBracket, attributes, rBracket)) = attrSet
    visitor.VisitToken "[<" lBracket

    for attr, sep in attributes do
        walkAttribute visitor attr
        visitTokenOpt visitor ";" sep

    visitor.VisitToken ">]" rBracket

and walkAttributes (visitor: AstVisitor<'T>) (attrs: Attributes<'T>) : unit =
    for attrSet in attrs do
        walkAttributeSet visitor attrSet

and walkAttributesOpt (visitor: AstVisitor<'T>) (attrs: Attributes<'T> voption) : unit =
    match attrs with
    | ValueSome a -> walkAttributes visitor a
    | ValueNone -> ()

and walkIdentOrOp (visitor: AstVisitor<'T>) (identOrOp: IdentOrOp<'T>) : unit =
    match identOrOp with
    | IdentOrOp.Ident ident -> visitor.VisitToken "Ident" ident
    | IdentOrOp.ParenOp(lParen, opName, rParen) ->
        visitor.EnterSection "ParenOp"
        visitor.VisitToken "" lParen
        walkOpName visitor opName
        visitor.VisitToken "" rParen
        visitor.ExitSection "ParenOp"
    | IdentOrOp.StarOp(lParen, star, rParen) ->
        visitor.EnterSection "StarOp"
        visitor.VisitToken "" lParen
        visitor.VisitToken "" star
        visitor.VisitToken "" rParen
        visitor.ExitSection "StarOp"

and walkOpName (visitor: AstVisitor<'T>) (opName: OpName<'T>) : unit =
    match opName with
    | OpName.SymbolicOp op -> visitor.VisitToken "SymbolicOp" op
    | OpName.RangeOp rangeOp ->
        visitor.EnterSection "RangeOp"
        walkRangeOpName visitor rangeOp
        visitor.ExitSection "RangeOp"
    | OpName.ActivePatternOp activePatternOp ->
        visitor.EnterSection "ActivePatternOp"
        walkActivePatternOpName visitor activePatternOp
        visitor.ExitSection "ActivePatternOp"

and walkRangeOpName (visitor: AstVisitor<'T>) (rangeOpName: RangeOpName<'T>) : unit =
    match rangeOpName with
    | RangeOpName.DotDot dotDot -> visitor.VisitToken "DotDot" dotDot
    | RangeOpName.DotDotDotDot dotDotDotDot -> visitor.VisitToken "DotDotDotDot" dotDotDotDot

and walkActivePatternOpName (visitor: AstVisitor<'T>) (apn: ActivePatternOpName<'T>) : unit =
    match apn with
    | ActivePatternOpName.ActivePatternOp(lBar, idents, finalUnderscore, rBar) ->
        visitor.EnterSection "ActivePatternOp"
        visitor.VisitToken "" lBar

        for ident in idents do
            visitor.VisitToken "" ident

        visitTokenOpt visitor "" finalUnderscore

        visitor.VisitToken "" rBar
        visitor.ExitSection "ActivePatternOp"

and walkLongIdentOrOp (visitor: AstVisitor<'T>) (longIdentOrOp: LongIdentOrOp<'T>) : unit =
    match longIdentOrOp with
    | LongIdentOrOp.LongIdent idents ->
        visitor.EnterSection "LongIdent"

        for ident in idents do
            visitor.VisitToken "" ident

        visitor.ExitSection "LongIdent"
    | LongIdentOrOp.Op identOrOp -> walkIdentOrOp visitor identOrOp
    | LongIdentOrOp.QualifiedOp(longIdent, dot, identOrOp) ->
        visitor.EnterSection "QualifiedOp"

        for ident in longIdent do
            visitor.VisitToken "" ident

        visitor.VisitToken "." dot
        walkIdentOrOp visitor identOrOp
        visitor.ExitSection "QualifiedOp"

and walkPat (visitor: AstVisitor<'T>) (pat: Pat<'T>) : unit =
    match pat with
    | Pat.Const value -> walkConstant visitor "Pat.Const" value
    | Pat.NamedSimple ident -> visitor.VisitToken "Pat.NamedSimple" ident
    | Pat.Wildcard underscore -> visitor.VisitToken "Pat.Wildcard" underscore
    | Pat.Named(longIdent, param, innerPat) ->
        visitor.EnterSection "Pat.Named"

        for ident in longIdent do
            visitor.VisitToken "Ident" ident

        match param with
        | ValueSome p ->
            visitor.EnterSection "Param"
            walkPat visitor p
            visitor.ExitSection "Param"
        | ValueNone -> ()

        match innerPat with
        | ValueSome p ->
            visitor.EnterSection "Pat"
            walkPat visitor p
            visitor.ExitSection "Pat"
        | ValueNone -> ()

        visitor.ExitSection "Pat.Named"
    | Pat.OpNamed(head, param, innerPat) ->
        visitor.EnterSection "Pat.OpNamed"
        walkIdentOrOp visitor head

        match param with
        | ValueSome p ->
            visitor.EnterSection "Param"
            walkPat visitor p
            visitor.ExitSection "Param"
        | ValueNone -> ()

        match innerPat with
        | ValueSome p ->
            visitor.EnterSection "Pat"
            walkPat visitor p
            visitor.ExitSection "Pat"
        | ValueNone -> ()

        visitor.ExitSection "Pat.OpNamed"
    | Pat.NamedFieldPats(longIdent, lParen, args, _commas, rParen) ->
        visitor.EnterSection "Pat.NamedFieldPats"

        for ident in longIdent do
            visitor.VisitToken "Ident" ident

        visitor.VisitToken "(" lParen

        for arg in args do
            match arg with
            | UnionArgPat.Named(fieldLongIdent, equals, innerPat) ->
                visitor.EnterSection "UnionArgPat.Named"

                for ident in fieldLongIdent do
                    visitor.VisitToken "Ident" ident

                visitor.VisitToken "=" equals
                visitor.EnterSection "Pat"
                walkPat visitor innerPat
                visitor.ExitSection "Pat"
                visitor.ExitSection "UnionArgPat.Named"
            | UnionArgPat.Positional innerPat ->
                visitor.EnterSection "UnionArgPat.Positional"
                visitor.EnterSection "Pat"
                walkPat visitor innerPat
                visitor.ExitSection "Pat"
                visitor.ExitSection "UnionArgPat.Positional"

        visitor.VisitToken ")" rParen
        visitor.ExitSection "Pat.NamedFieldPats"
    | Pat.As(innerPat, asToken, ident) ->
        visitor.EnterSection "Pat.As"
        walkPat visitor innerPat
        visitor.VisitToken "As" asToken
        visitor.VisitToken "Ident" ident
        visitor.ExitSection "Pat.As"
    | Pat.Or(left, bar, right) ->
        visitor.VisitToken "Pat.Or" bar
        visitor.EnterSection ""
        walkPat visitor left
        walkPat visitor right
        visitor.ExitSection ""
    | Pat.And(left, ampersand, right) ->
        visitor.VisitToken "Pat.And" ampersand
        visitor.EnterSection ""
        walkPat visitor left
        walkPat visitor right
        visitor.ExitSection ""
    | Pat.Cons(head, consToken, tail) ->
        visitor.VisitToken "Pat.Cons" consToken
        visitor.EnterSection ""
        walkPat visitor head
        walkPat visitor tail
        visitor.ExitSection ""
    | Pat.Typed(innerPat, colon, typ) ->
        visitor.EnterSection "Pat.Typed"
        walkPat visitor innerPat
        visitor.VisitToken "Colon" colon
        walkType visitor typ
        visitor.ExitSection "Pat.Typed"
    | Pat.Tuple(patterns, commas) ->
        visitor.EnterSection "Pat.Tuple"

        for i in 0 .. patterns.Length - 1 do
            walkPat visitor patterns[i]

            if i < commas.Length then
                visitor.VisitToken "," commas[i]

        visitor.ExitSection "Pat.Tuple"
    | Pat.StructTuple(structToken, _lParen, patterns, _commas, _rParen) ->
        visitor.EnterSection "Pat.StructTuple"
        visitor.VisitToken "" structToken

        for p in patterns do
            walkPat visitor p

        visitor.ExitSection "Pat.StructTuple"

    | Pat.EnclosedBlock(lParen, innerPat, r) ->
        let label, lTok = parenKind lParen
        visitor.VisitToken label lTok
        visitor.EnterSection ""
        walkPat visitor innerPat
        visitor.ExitSection ""
        visitor.VisitToken "" r
    | Pat.EmptyBlock(lParen, r) ->
        let label, lTok = parenKind lParen

        visitor.VisitToken label lTok
        visitor.VisitToken "" r
    | Pat.Elems(patterns, _seps) ->
        visitor.EnterSection "Pat.Elems"

        for p in patterns do
            walkPat visitor p

        visitor.ExitSection "Pat.Elems"
    | Pat.Record(lBrace, fieldPats, _, rBrace) ->
        visitor.VisitToken "Pat.Record" lBrace
        visitor.EnterSection ""

        for FieldPat(longIdent, equals, innerPat) in fieldPats do
            visitor.EnterSection "FieldPat"

            for ident in longIdent do
                visitor.VisitToken "Ident" ident

            visitor.VisitToken "=" equals
            visitor.EnterSection "Pat"
            walkPat visitor innerPat
            visitor.ExitSection "Pat"
            visitor.ExitSection "FieldPat"

        visitor.ExitSection ""
        visitor.VisitToken "" rBrace
    | Pat.TypeTest(colonQuestion, typ) ->
        visitor.EnterSection "Pat.TypeTest"
        visitor.VisitToken "ColonQuestion" colonQuestion
        walkType visitor typ
        visitor.ExitSection "Pat.TypeTest"
    | Pat.TypeTestAs(colonQuestion, typ, asToken, pat) ->
        visitor.EnterSection "Pat.TypeTestAs"
        visitor.VisitToken "ColonQuestion" colonQuestion
        walkType visitor typ
        visitor.VisitToken "As" asToken
        walkPat visitor pat
        visitor.ExitSection "Pat.TypeTestAs"
    | Pat.Null nullToken -> visitor.VisitToken "Pat.Null" nullToken
    | Pat.Attributed(attributes, innerPat) ->
        visitor.EnterSection "Pat.Attributed"
        walkAttributes visitor attributes
        walkPat visitor innerPat
        visitor.ExitSection "Pat.Attributed"
    | Pat.Struct(structToken, innerPat) ->
        visitor.EnterSection "Pat.Struct"
        visitor.VisitToken "" structToken
        walkPat visitor innerPat
        visitor.ExitSection "Pat.Struct"
    | Pat.Optional(questionMark, innerPat) ->
        visitor.EnterSection "Pat.Optional"
        visitor.VisitToken "?" questionMark
        walkPat visitor innerPat
        visitor.ExitSection "Pat.Optional"
    | Pat.Op identOrOp ->
        visitor.EnterSection "Pat.Op"
        walkIdentOrOp visitor identOrOp
        visitor.ExitSection "Pat.Op"
    | Pat.String(kind, parts, closing) -> walkStringKindAndParts visitor kind parts closing
    | Pat.Missing -> visitor.WriteLine "Missing"
    | Pat.SkipsTokens(skippedTokens) ->
        visitor.EnterSection "SkipsTokens"

        for t in skippedTokens do
            visitor.VisitToken "(skipped)" t

        visitor.ExitSection "SkipsTokens"

and walkTypar (visitor: AstVisitor<'T>) (typar: Typar<'T>) : unit =
    match typar with
    | Typar.Anon underscore -> visitor.VisitToken "Typar.Anon" underscore
    | Typar.Named(quote, ident) ->
        visitor.EnterSection "Typar.Named"
        visitor.VisitToken "" quote
        // When quote and ident are the same token (e.g. 'T lexed as one token), only print once.
        if not (visitor.EqualTokens quote ident) then
            visitor.VisitToken "" ident

        visitor.ExitSection "Typar.Named"
    | Typar.Static(caret, ident) ->
        visitor.EnterSection "Typar.Static"
        visitor.VisitToken "" caret
        visitor.VisitToken "" ident
        visitor.ExitSection "Typar.Static"

and walkStaticTypars (visitor: AstVisitor<'T>) (staticTypars: StaticTypars<'T>) : unit =
    match staticTypars with
    | StaticTypars.Single typar -> walkTypar visitor typar
    | StaticTypars.OrList(_lParen, typars, _ors, _rParen) ->
        for typar in typars do
            walkTypar visitor typar

and walkStaticOptimizationConstraint (visitor: AstVisitor<'T>) (c: StaticOptimizationConstraint<'T>) : unit =
    match c with
    | StaticOptimizationConstraint.WhenTyparTyconEqualsTycon(typar, colon, rhsType) ->
        visitor.EnterSection "WhenTyparTyconEqualsTycon"
        walkTypar visitor typar
        visitor.VisitToken ":" colon
        walkType visitor rhsType
        visitor.ExitSection "WhenTyparTyconEqualsTycon"
    | StaticOptimizationConstraint.WhenTyparIsStruct(typar, colon, structToken) ->
        visitor.EnterSection "WhenTyparIsStruct"
        walkTypar visitor typar
        visitor.VisitToken ":" colon
        visitor.VisitToken "struct" structToken
        visitor.ExitSection "WhenTyparIsStruct"

and walkConstraint (visitor: AstVisitor<'T>) (c: Constraint<'T>) : unit =
    match c with
    | Constraint.Coercion(typar, colonGT, typ) ->
        visitor.EnterSection "Constraint.Coercion"
        walkTypar visitor typar
        visitor.VisitToken ":>" colonGT
        walkType visitor typ
        visitor.ExitSection "Constraint.Coercion"
    | Constraint.Nullness(typar, colon, nullToken) ->
        visitor.EnterSection "Constraint.Nullness"
        walkTypar visitor typar
        visitor.VisitToken ":" colon
        visitor.VisitToken "null" nullToken
        visitor.ExitSection "Constraint.Nullness"
    | Constraint.MemberTrait(staticTypars, colon, lParen, staticToken, memberToken, membersign, rParen) ->
        visitor.EnterSection "Constraint.MemberTrait"
        walkStaticTypars visitor staticTypars
        visitor.VisitToken ":" colon
        visitor.VisitToken "(" lParen
        visitTokenOpt visitor "static" staticToken
        visitor.VisitToken "member" memberToken
        walkMemberSig visitor membersign
        visitor.VisitToken ")" rParen
        visitor.ExitSection "Constraint.MemberTrait"
    | Constraint.DefaultConstructor(typar, colon, lParen, newToken, colonUnit, unitToken, arrow, resultTypar, rParen) ->
        visitor.EnterSection "Constraint.DefaultConstructor"
        walkTypar visitor typar
        visitor.VisitToken ":" colon
        visitor.VisitToken "(" lParen
        visitor.VisitToken "new" newToken
        visitor.VisitToken ":" colonUnit
        visitor.VisitToken "unit" unitToken
        visitor.VisitToken "->" arrow
        walkTypar visitor resultTypar
        visitor.VisitToken ")" rParen
        visitor.ExitSection "Constraint.DefaultConstructor"
    | Constraint.Struct(typar, colon, structToken) ->
        visitor.EnterSection "Constraint.Struct"
        walkTypar visitor typar
        visitor.VisitToken ":" colon
        visitor.VisitToken "struct" structToken
        visitor.ExitSection "Constraint.Struct"
    | Constraint.ReferenceType(typar, colon, notToken, structToken) ->
        visitor.EnterSection "Constraint.ReferenceType"
        walkTypar visitor typar
        visitor.VisitToken ":" colon
        visitor.VisitToken "not" notToken
        visitor.VisitToken "struct" structToken
        visitor.ExitSection "Constraint.ReferenceType"
    | Constraint.NotNull(typar, colon, notToken, nullToken) ->
        visitor.EnterSection "Constraint.NotNull"
        walkTypar visitor typar
        visitor.VisitToken ":" colon
        visitor.VisitToken "not" notToken
        visitor.VisitToken "null" nullToken
        visitor.ExitSection "Constraint.NotNull"
    | Constraint.Enum(typar, _colon, _enumToken, _lAngle, typ, _rAngle) ->
        visitor.EnterSection "Constraint.Enum"
        walkTypar visitor typar
        walkType visitor typ
        visitor.ExitSection "Constraint.Enum"
    | Constraint.Unmanaged(typar, colon, unmanagedToken) ->
        visitor.EnterSection "Constraint.Unmanaged"
        walkTypar visitor typar
        visitor.VisitToken ":" colon
        visitor.VisitToken "unmanaged" unmanagedToken
        visitor.ExitSection "Constraint.Unmanaged"
    | Constraint.Delegate(typar, _colon, _delegateToken, _lAngle, type1, _comma, type2, _rAngle) ->
        visitor.EnterSection "Constraint.Delegate"
        walkTypar visitor typar
        walkType visitor type1
        walkType visitor type2
        visitor.ExitSection "Constraint.Delegate"
    | Constraint.Equality(typar, colon, equalityToken) ->
        visitor.EnterSection "Constraint.Equality"
        walkTypar visitor typar
        visitor.VisitToken ":" colon
        visitor.VisitToken "equality" equalityToken
        visitor.ExitSection "Constraint.Equality"
    | Constraint.Comparison(typar, colon, comparisonToken) ->
        visitor.EnterSection "Constraint.Comparison"
        walkTypar visitor typar
        visitor.VisitToken ":" colon
        visitor.VisitToken "comparison" comparisonToken
        visitor.ExitSection "Constraint.Comparison"

and walkTyparDefns (visitor: AstVisitor<'T>) (typars: TyparDefns<'T>) : unit =
    let (TyparDefns(lAngle, defns, constraints, rAngle)) = typars
    visitor.EnterSection "TyparDefns"
    visitor.VisitToken "" lAngle

    for (TyparDefn(attrs, typar)) in defns do
        walkAttributesOpt visitor attrs
        walkTypar visitor typar

    match constraints with
    | ValueSome(TyparConstraints(whenToken, constraintList, ands)) ->
        visitor.VisitToken "when" whenToken

        for i in 0 .. constraintList.Length - 1 do
            walkConstraint visitor constraintList[i]

            if i < ands.Length then
                visitor.VisitToken "and" ands[i]
    | ValueNone -> ()

    visitor.VisitToken "" rAngle
    visitor.ExitSection "TyparDefns"

and walkTypeArg (visitor: AstVisitor<'T>) (typeArg: TypeArg<'T>) : unit =
    match typeArg with
    | TypeArg.Type ty ->
        visitor.EnterSection "TypeArg.Type"
        walkType visitor ty
        visitor.ExitSection "TypeArg.Type"
    | TypeArg.Measure measure ->
        visitor.EnterSection "TypeArg.Measure"
        walkMeasure visitor measure
        visitor.ExitSection "TypeArg.Measure"
    | TypeArg.StaticParameter staticParam -> visitor.VisitToken "TypeArg.StaticParameter" staticParam

and walkType (visitor: AstVisitor<'T>) (ty: Type<'T>) : unit =
    match ty with
    | Type.ParenType(lParen, typ, rParen) ->
        visitor.VisitToken "ParenType" lParen
        visitor.EnterSection ""
        walkType visitor typ
        visitor.ExitSection ""
        visitor.VisitToken "" rParen
    | Type.VarType typar ->
        visitor.EnterSection "VarType"
        walkTypar visitor typar
        visitor.ExitSection "VarType"
    | Type.NamedType longIdent ->
        visitor.EnterSection "NamedType"
        walkLongIdentOrOp visitor (LongIdentOrOp.LongIdent longIdent)
        visitor.ExitSection "NamedType"
    | Type.GenericType(longIdent, lAngle, typeArgs, _, rAngle) ->
        visitor.EnterSection "GenericType"
        walkLongIdentOrOp visitor (LongIdentOrOp.LongIdent longIdent)
        visitor.VisitToken "<" lAngle

        for typeArg in typeArgs do
            walkTypeArg visitor typeArg

        visitor.VisitToken ">" rAngle
        visitor.ExitSection "GenericType"
    | Type.FunctionType(fromType, arrow, toType) ->
        visitor.EnterSection "FunctionType"
        walkType visitor fromType
        visitor.VisitToken "->" arrow
        walkType visitor toType
        visitor.ExitSection "FunctionType"
    | Type.TupleType(types, asterisks) ->
        visitor.EnterSection "TupleType"

        for i in 0 .. types.Length - 1 do
            walkType visitor types[i]

            if i < asterisks.Length then
                visitor.VisitToken "*" asterisks[i]

        visitor.ExitSection "TupleType"
    | Type.StructTupleType(structToken, _lParen, types, _, _rParen) ->
        visitor.EnterSection "StructTupleType"
        visitor.VisitToken "" structToken

        for t in types do
            walkType visitor t

        visitor.ExitSection "StructTupleType"
    | Type.IncompleteGenericType(longIdent, lAngle, rAngle) ->
        visitor.EnterSection "IncompleteGenericType"
        walkLongIdentOrOp visitor (LongIdentOrOp.LongIdent longIdent)
        visitor.VisitToken "<" lAngle
        visitor.VisitToken ">" rAngle
        visitor.ExitSection "IncompleteGenericType"
    | Type.SuffixedType(baseType, longIdent) ->
        visitor.EnterSection "SuffixedType"
        walkType visitor baseType
        walkLongIdentOrOp visitor (LongIdentOrOp.LongIdent longIdent)
        visitor.ExitSection "SuffixedType"
    | Type.DottedType(baseType, dot, longIdent) ->
        visitor.EnterSection "DottedType"
        walkType visitor baseType
        visitor.VisitToken "." dot
        walkLongIdentOrOp visitor (LongIdentOrOp.LongIdent longIdent)
        visitor.ExitSection "DottedType"
    | Type.ArrayType(baseType, lBracket, commas, rBracket) ->
        visitor.EnterSection "ArrayType"
        walkType visitor baseType
        visitor.VisitToken "" lBracket

        for comma in commas do
            visitor.VisitToken "" comma

        visitor.VisitToken "" rBracket
        visitor.ExitSection "ArrayType"
    | Type.ConstrainedType(typ, constraints) ->
        visitor.EnterSection "ConstrainedType"
        walkType visitor typ
        walkTyparDefns visitor constraints
        visitor.ExitSection "ConstrainedType"
    | Type.WhenConstrainedType(typ, TyparConstraints.TyparConstraints(whenTok, constraints, ands)) ->
        visitor.EnterSection "WhenConstrainedType"
        walkType visitor typ
        visitor.VisitToken "when" whenTok

        for i in 0 .. constraints.Length - 1 do
            walkConstraint visitor constraints[i]

            if i < ands.Length then
                visitor.VisitToken "and" ands[i]

        visitor.ExitSection "WhenConstrainedType"
    | Type.SubtypeConstraint(typar, colonGreaterThan, typ) ->
        visitor.EnterSection "SubtypeConstraint"
        walkTypar visitor typar
        visitor.VisitToken ":>" colonGreaterThan
        walkType visitor typ
        visitor.ExitSection "SubtypeConstraint"
    | Type.AnonymousSubtype(hash, typ) ->
        visitor.EnterSection "AnonymousSubtype"
        visitor.VisitToken "" hash
        walkType visitor typ
        visitor.ExitSection "AnonymousSubtype"
    | Type.Null nullToken -> visitor.VisitToken "null" nullToken
    | Type.UnionType(left, bar, right) ->
        visitor.EnterSection "UnionType"
        walkType visitor left
        visitor.VisitToken "|" bar
        walkType visitor right
        visitor.ExitSection "UnionType"
    | Type.ILIntrinsic(lHashParen, instrKind, instrParts, instrClose, rHashParen) ->
        visitor.EnterSection "ILIntrinsic"
        visitor.VisitToken "(#" lHashParen
        walkStringKindAndParts visitor instrKind instrParts instrClose
        visitor.VisitToken "#)" rHashParen
        visitor.ExitSection "ILIntrinsic"
    | Type.AnonRecordType(lBraceBar, fields, _, rBraceBar) ->
        visitor.EnterSection "AnonRecordType"
        visitor.VisitToken "{|" lBraceBar

        for (AnonRecordField(ident, colon, typ)) in fields do
            visitor.VisitToken "" ident
            visitor.VisitToken ":" colon
            walkType visitor typ

        visitor.VisitToken "|}" rBraceBar
        visitor.ExitSection "AnonRecordType"
    | Type.MeasureType measure ->
        visitor.EnterSection "MeasureType"
        walkMeasure visitor measure
        visitor.ExitSection "MeasureType"
    | Type.Missing -> visitor.WriteLine "Missing"
    | Type.SkipsTokens(skippedTokens) ->
        visitor.EnterSection "SkipsTokens"

        for t in skippedTokens do
            visitor.VisitToken "(skipped)" t

        visitor.ExitSection "SkipsTokens"

and walkBinding (visitor: AstVisitor<'T>) (binding: Binding<'T>) : unit =
    // Note: attributes are NOT visited here — they are visited by the caller
    // (ModuleFunctionOrValueDefn, ClassFunctionOrValueDefn, MemberDefn, etc.)
    // to avoid duplication, since the same attrs value is stored both on the
    // declaration wrapper and on the binding record.
    visitTokenOpt visitor "inline" binding.inlineToken
    visitTokenOpt visitor "mutable" binding.mutableToken
    visitTokenOpt visitor "fixed" binding.fixedToken
    visitTokenOpt visitor "access" binding.access

    walkPat visitor binding.headPat

    match binding.typarDefns with
    | ValueSome typars -> walkTyparDefns visitor typars
    | ValueNone -> ()

    if not binding.argumentPats.IsEmpty then
        visitor.EnterSection "Pats"

        for pat in binding.argumentPats do
            walkPat visitor pat

        visitor.ExitSection "Pats"

    match binding.returnType with
    | ValueSome(ReturnType(colon, typ)) ->
        visitor.VisitToken "ReturnColon" colon
        walkType visitor typ
    | ValueNone -> ()

    visitor.VisitToken "=" binding.equals
    visitor.EnterSection ""
    walkExpr visitor binding.expr
    visitor.ExitSection ""

and walkFieldInitializer (visitor: AstVisitor<'T>) (fieldInit: FieldInitializer<'T>) : unit =
    let (FieldInitializer(longIdent, equals, fieldExpr)) = fieldInit
    visitor.EnterSection "Field"
    walkLongIdentOrOp visitor (LongIdentOrOp.LongIdent longIdent)
    visitor.VisitToken "=" equals
    visitor.EnterSection "Expr"
    walkExpr visitor fieldExpr
    visitor.ExitSection "Expr"
    visitor.ExitSection "Field"

and walkRules (visitor: AstVisitor<'T>) (rules: Rules<'T>) : unit =
    let (Rules(firstBar, ruleList, bars)) = rules

    let walkOneRule (rule: Rule<'T>) =
        match rule with
        | Rule.Rule(pat, guard, arrow, ruleExpr) ->
            visitor.EnterSection "Pat"
            walkPat visitor pat
            visitor.ExitSection "Pat"

            match guard with
            | ValueSome(PatternGuard(whenToken, guardExpr)) ->
                visitor.VisitToken "When" whenToken
                visitor.EnterSection "Guard"
                walkExpr visitor guardExpr
                visitor.ExitSection "Guard"
            | ValueNone -> ()

            visitor.VisitToken "Arrow" arrow
            visitor.EnterSection "Expr"
            walkExpr visitor ruleExpr
            visitor.ExitSection "Expr"
        | Rule.Missing -> visitor.WriteLine "Missing"
        | Rule.SkipsTokens _ -> visitor.WriteLine "SkipsTokens (nested)"

    for i in 0 .. ruleList.Length - 1 do
        let bar = if i = 0 then firstBar else ValueSome bars[i - 1]
        visitor.EnterSection "Rule"

        visitTokenOpt visitor "Bar" bar

        match ruleList[i] with
        | Rule.Rule _
        | Rule.Missing as rule -> walkOneRule rule
        | Rule.SkipsTokens(skippedTokens) ->
            visitor.EnterSection "SkipsTokens"

            for t in skippedTokens do
                visitor.VisitToken "(skipped)" t

            visitor.ExitSection "SkipsTokens"

        visitor.ExitSection "Rule"

and walkStringKindAndParts
    (visitor: AstVisitor<'T>)
    (kind: StringKind<'T>)
    (parts: ImArr<StringPart<'T>>)
    (closing: 'T)
    : unit =
    let label, opening =
        match kind with
        | StringKind.String t -> "StringLiteral", t
        | StringKind.VerbatimString t -> "StringLiteral", t
        | StringKind.String3 t -> "StringLiteral", t
        | StringKind.InterpolatedString t -> "InterpolatedString", t
        | StringKind.VerbatimInterpolatedString t -> "InterpolatedString", t
        | StringKind.Interpolated3String t -> "InterpolatedString", t

    visitor.VisitToken label opening

    if not parts.IsEmpty then
        visitor.EnterSection "Parts"

        for part in parts do
            match part with
            | StringPart.Text t -> visitor.VisitToken "Text" t
            | StringPart.EscapeSequence e -> visitor.VisitToken "Escape" e
            | StringPart.FormatSpecifier f -> visitor.VisitToken "FormatSpec" f
            | StringPart.EscapePercent e -> visitor.VisitToken "EscapePercent" e
            | StringPart.VerbatimEscapeQuote e -> visitor.VisitToken "VerbatimEscapeQuote" e
            | StringPart.Expr(formatSpec, lBrace, expr, formatClause, rBrace) ->
                visitTokenOpt visitor "FormatSpec" formatSpec
                visitor.VisitToken "{" lBrace
                visitor.EnterSection "Expr"
                walkExpr visitor expr
                visitor.ExitSection "Expr"
                visitTokenOpt visitor "FormatClause" formatClause
                visitor.VisitToken "}" rBrace
            | StringPart.OrphanFormatSpecifier fs -> visitor.VisitToken "OrphanFormatSpec" fs
            | StringPart.InvalidText t -> visitor.VisitToken "InvalidText" t

        visitor.ExitSection "Parts"

    visitor.VisitToken "" closing

and walkExpr (visitor: AstVisitor<'T>) (expr: Expr<'T>) : unit =
    match expr with
    | Expr.Const value -> walkConstant visitor "Const" value
    | Expr.Ident ident -> visitor.VisitToken "Ident" ident
    | Expr.LetOrUse(keyword, isRec, bindings, _, inToken, body) ->
        let kwLabel =
            match keyword with
            | LetOrUseKeyword.Let t ->
                visitor.VisitToken "Let" t
                "Let"
            | LetOrUseKeyword.LetBang t ->
                visitor.VisitToken "LetBang" t
                "LetBang"
            | LetOrUseKeyword.Use t ->
                visitor.VisitToken "Use" t
                "Use"
            | LetOrUseKeyword.UseBang t ->
                visitor.VisitToken "UseBang" t
                "UseBang"

        visitTokenOpt visitor "rec" isRec

        visitor.EnterSection ""

        for binding in bindings do
            walkBinding visitor binding

        visitor.ExitSection ""

        visitTokenOpt visitor "in" inToken

        match body with
        | ValueSome bodyExpr ->
            visitor.EnterSection "Body"
            walkExpr visitor bodyExpr
            visitor.ExitSection "Body"
        | ValueNone -> ()
    | Expr.InfixApp(left, op, right) ->
        visitor.VisitToken "InfixApp" op
        visitor.EnterSection ""
        walkExpr visitor left
        walkExpr visitor right
        visitor.ExitSection ""
    | Expr.PrefixApp(op, expr) ->
        visitor.VisitToken "PrefixApp" op
        visitor.EnterSection ""
        walkExpr visitor expr
        visitor.ExitSection ""
    | Expr.OptionalArgExpr(qmark, ident) ->
        visitor.EnterSection "OptionalArg"
        visitor.VisitToken "?" qmark
        visitor.VisitToken "Ident" ident
        visitor.ExitSection "OptionalArg"
    | Expr.Sequential(exprs, _ops) ->
        visitor.EnterSection "Sequential"

        for e in exprs do
            walkExpr visitor e

        visitor.ExitSection "Sequential"
    | Expr.Tuple(elements, _) ->
        visitor.EnterSection "Tuple"

        for elem in elements do
            walkExpr visitor elem

        visitor.ExitSection "Tuple"
    | Expr.StructTuple(_kw, _lParen, elements, _, _rParen) ->
        visitor.EnterSection "StructTuple"

        for elem in elements do
            walkExpr visitor elem

        visitor.ExitSection "StructTuple"
    | Expr.EnclosedBlock(lParen, expr, r) ->
        let label, lTok = parenKind lParen
        visitor.VisitToken label lTok
        visitor.EnterSection ""
        walkExpr visitor expr
        visitor.ExitSection ""
        visitor.VisitToken "" r
    | Expr.EmptyBlock(lParen, r) ->
        let label, lTok = parenKind lParen

        visitor.VisitToken label lTok
        visitor.VisitToken "" r
    | Expr.LongIdentOrOp longIdentOrOp ->
        visitor.EnterSection "LongIdentOrOp"
        walkLongIdentOrOp visitor longIdentOrOp
        visitor.ExitSection "LongIdentOrOp"
    | Expr.TypeApp(expr, lAngle, types, _, rAngle) ->
        visitor.EnterSection "TypeApp"
        visitor.EnterSection "Expr"
        walkExpr visitor expr
        visitor.ExitSection "Expr"
        visitor.EnterSection "Types"

        for ty in types do
            walkType visitor ty

        visitor.ExitSection "Types"
        visitor.ExitSection "TypeApp"
    | Expr.DotLookup(expr, dot, longIdentOrOp) ->
        visitor.EnterSection "DotLookup"
        visitor.EnterSection "Expr"
        walkExpr visitor expr
        visitor.ExitSection "Expr"
        visitor.VisitToken "Dot" dot
        visitor.EnterSection "LongIdentOrOp"
        walkLongIdentOrOp visitor longIdentOrOp
        visitor.ExitSection "LongIdentOrOp"
        visitor.ExitSection "DotLookup"
    | Expr.IfThenElse(ifToken, condition, thenToken, thenExpr, elifBranches, elseBranch) ->
        visitor.EnterSection "IfThenElse"
        visitor.VisitToken "IfToken" ifToken
        visitor.EnterSection "Condition"
        walkExpr visitor condition
        visitor.ExitSection "Condition"
        visitor.VisitToken "ThenToken" thenToken
        visitor.EnterSection "ThenExpr"
        walkExpr visitor thenExpr
        visitor.ExitSection "ThenExpr"

        for elif' in elifBranches do
            visitor.EnterSection "ElifBranch"

            match elif' with
            | ElifBranch.Elif(elifToken, elifCondition, elifThenToken, elifExpr) ->
                visitor.VisitToken "ElifToken" elifToken
                visitor.EnterSection "ElifCondition"
                walkExpr visitor elifCondition
                visitor.ExitSection "ElifCondition"
                visitor.VisitToken "ThenToken" elifThenToken
                visitor.EnterSection "ElifExpr"
                walkExpr visitor elifExpr
                visitor.ExitSection "ElifExpr"
                visitor.ExitSection "ElifBranch"

            | ElifBranch.ElseIf(elseTok, ifToken, elifCondition, elifThenToken, elifExpr) ->
                visitor.VisitToken "ElseToken" elseTok
                visitor.VisitToken "IfToken" ifToken
                visitor.EnterSection "ElifCondition"
                walkExpr visitor elifCondition
                visitor.ExitSection "ElifCondition"
                visitor.VisitToken "ThenToken" elifThenToken
                visitor.EnterSection "ElifExpr"
                walkExpr visitor elifExpr
                visitor.ExitSection "ElifExpr"
                visitor.ExitSection "ElifBranch"

        match elseBranch with
        | ValueSome(ElseBranch.ElseBranch(elseToken, elseExpr)) ->
            visitor.VisitToken "ElseToken" elseToken
            visitor.EnterSection "ElseExpr"
            walkExpr visitor elseExpr
            visitor.ExitSection "ElseExpr"
        | ValueNone -> ()

        visitor.ExitSection "IfThenElse"
    | Expr.Match(matchToken, matchExpr, withToken, rules) ->
        visitor.EnterSection "Match"
        visitor.VisitToken "MatchToken" matchToken
        visitor.EnterSection "MatchExpr"
        walkExpr visitor matchExpr
        visitor.ExitSection "MatchExpr"
        visitor.VisitToken "WithToken" withToken
        walkRules visitor rules
        visitor.ExitSection "Match"
    | Expr.Fun(funToken, pats, arrow, expr) ->
        visitor.VisitToken "Fun" funToken
        visitor.EnterSection "Pats"

        for pat in pats do
            walkPat visitor pat

        visitor.ExitSection "Pats"
        visitor.VisitToken "Arrow" arrow
        visitor.EnterSection "Body"
        walkExpr visitor expr
        visitor.ExitSection "Body"
    | Expr.TryWith(tryToken, tryExpr, withToken, rules) ->
        visitor.VisitToken "TryWith" tryToken
        visitor.EnterSection "TryExpr"
        walkExpr visitor tryExpr
        visitor.ExitSection "TryExpr"
        visitor.VisitToken "WithToken" withToken
        walkRules visitor rules
    | Expr.TryFinally(tryToken, tryExpr, finallyToken, finallyExpr) ->
        visitor.VisitToken "TryFinally" tryToken
        visitor.EnterSection "TryExpr"
        walkExpr visitor tryExpr
        visitor.ExitSection "TryExpr"
        visitor.VisitToken "FinallyToken" finallyToken
        visitor.EnterSection "FinallyExpr"
        walkExpr visitor finallyExpr
        visitor.ExitSection "FinallyExpr"
    | Expr.While(whileToken, cond, doToken, body, doneToken) ->
        visitor.VisitToken "While" whileToken
        visitor.EnterSection "Cond"
        walkExpr visitor cond
        visitor.ExitSection "Cond"
        visitor.VisitToken "DoToken" doToken
        visitor.EnterSection "Body"
        walkExpr visitor body
        visitor.ExitSection "Body"
        visitor.VisitToken "DoneToken" doneToken
    | Expr.ForTo(forToken, ident, equals, startExpr, toToken, endExpr, doToken, body, doneToken) ->
        visitor.VisitToken "ForTo" forToken
        visitor.VisitToken "Ident" ident
        visitor.VisitToken "Equals" equals
        visitor.EnterSection "Start"
        walkExpr visitor startExpr
        visitor.ExitSection "Start"
        visitor.VisitToken "ToToken" toToken
        visitor.EnterSection "End"
        walkExpr visitor endExpr
        visitor.ExitSection "End"
        visitor.VisitToken "DoToken" doToken
        visitor.EnterSection "Body"
        walkExpr visitor body
        visitor.ExitSection "Body"
        visitor.VisitToken "DoneToken" doneToken
    | Expr.ForIn(forToken, pat, inToken, range, doToken, body, doneToken) ->
        visitor.VisitToken "ForIn" forToken
        visitor.EnterSection "Pat"
        walkPat visitor pat
        visitor.ExitSection "Pat"
        visitor.VisitToken "InToken" inToken
        walkExpr visitor range
        visitor.VisitToken "DoToken" doToken
        visitor.EnterSection "Body"
        walkExpr visitor body
        visitor.ExitSection "Body"
        visitor.VisitToken "DoneToken" doneToken
    | Expr.App(funcExpr, argExprs) ->
        visitor.EnterSection "App"
        visitor.EnterSection "Func"
        walkExpr visitor funcExpr
        visitor.ExitSection "Func"
        visitor.EnterSection "Args"

        for argExpr in argExprs do
            walkExpr visitor argExpr

        visitor.ExitSection "Args"
        visitor.ExitSection "App"
    | Expr.HighPrecedenceApp(funcExpr, lParen, argExpr, rParen) ->
        visitor.EnterSection "HighPrecedenceApp"
        visitor.EnterSection "Func"
        walkExpr visitor funcExpr
        visitor.ExitSection "Func"
        visitor.VisitToken "(" lParen
        visitor.EnterSection "Arg"
        walkExpr visitor argExpr
        visitor.ExitSection "Arg"
        visitor.VisitToken "" rParen
        visitor.ExitSection "HighPrecedenceApp"
    | Expr.TypeAnnotation(innerExpr, colon, typ) ->
        visitor.EnterSection "TypeAnnotation"
        visitor.EnterSection "Expr"
        walkExpr visitor innerExpr
        visitor.ExitSection "Expr"
        visitor.VisitToken "Colon" colon
        walkType visitor typ
        visitor.ExitSection "TypeAnnotation"
    | Expr.Lazy(lazyToken, innerExpr) ->
        visitor.VisitToken "Lazy" lazyToken
        visitor.EnterSection ""
        walkExpr visitor innerExpr
        visitor.ExitSection ""
    | Expr.Assert(assertToken, innerExpr) ->
        visitor.VisitToken "Assert" assertToken
        visitor.EnterSection ""
        walkExpr visitor innerExpr
        visitor.ExitSection ""
    | Expr.Fixed(fixedToken, innerExpr) ->
        visitor.VisitToken "Fixed" fixedToken
        visitor.EnterSection ""
        walkExpr visitor innerExpr
        visitor.ExitSection ""
    | Expr.Null nullToken -> visitor.VisitToken "Null" nullToken
    | Expr.Function(functionToken, rules) ->
        visitor.VisitToken "Function" functionToken
        visitor.EnterSection ""
        walkRules visitor rules
        visitor.ExitSection ""
    | Expr.New(newToken, typ, newExpr) ->
        visitor.VisitToken "New" newToken
        visitor.EnterSection ""
        walkType visitor typ
        visitor.EnterSection "Arg"
        walkExpr visitor newExpr
        visitor.ExitSection "Arg"
        visitor.ExitSection ""
    | Expr.Assignment(leftExpr, arrow, rightExpr) ->
        visitor.VisitToken "Assignment" arrow
        visitor.EnterSection ""
        walkExpr visitor leftExpr
        walkExpr visitor rightExpr
        visitor.ExitSection ""
    | Expr.Record(lBrace, fieldInitializers, _, rBrace) ->
        let label, lTok =
            match lBrace with
            | ParenKind.BraceBar t -> "AnonRecord", t
            | ParenKind.Brace t -> "Record", t
            | _ -> parenKind lBrace

        visitor.VisitToken label lTok
        visitor.EnterSection ""

        for fi in fieldInitializers do
            walkFieldInitializer visitor fi

        visitor.ExitSection ""
        visitor.VisitToken "" rBrace
    | Expr.RecordClone(lBrace, baseExpr, withToken, fieldInitializers, _, rBrace) ->
        let label, lTok =
            match lBrace with
            | ParenKind.BraceBar t -> "AnonRecordClone", t
            | ParenKind.Brace t -> "RecordClone", t
            | _ -> parenKind lBrace

        visitor.VisitToken label lTok
        visitor.EnterSection ""
        visitor.EnterSection "Base"
        walkExpr visitor baseExpr
        visitor.ExitSection "Base"
        visitor.VisitToken "with" withToken

        for fi in fieldInitializers do
            walkFieldInitializer visitor fi

        visitor.ExitSection ""
        visitor.VisitToken "" rBrace
    | Expr.IndexedLookup(indexedExpr, dot, lBracket, indexArgExpr, rBracket) ->
        visitor.EnterSection "IndexedLookup"
        visitor.EnterSection "Expr"
        walkExpr visitor indexedExpr
        visitor.ExitSection "Expr"

        visitTokenOpt visitor "." dot

        visitor.VisitToken "[" lBracket
        visitor.EnterSection "Index"
        walkExpr visitor indexArgExpr
        visitor.ExitSection "Index"
        visitor.VisitToken "" rBracket
        visitor.ExitSection "IndexedLookup"
    | Expr.StaticUpcast(castExpr, colonGT, typ) ->
        visitor.EnterSection "StaticUpcast"
        visitor.EnterSection "Expr"
        walkExpr visitor castExpr
        visitor.ExitSection "Expr"
        visitor.VisitToken ":>" colonGT
        walkType visitor typ
        visitor.ExitSection "StaticUpcast"
    | Expr.DynamicTypeTest(testExpr, colonQ, typ) ->
        visitor.EnterSection "DynamicTypeTest"
        visitor.EnterSection "Expr"
        walkExpr visitor testExpr
        visitor.ExitSection "Expr"
        visitor.VisitToken ":?" colonQ
        walkType visitor typ
        visitor.ExitSection "DynamicTypeTest"
    | Expr.DynamicDowncast(castExpr, colonQGT, typ) ->
        visitor.EnterSection "DynamicDowncast"
        visitor.EnterSection "Expr"
        walkExpr visitor castExpr
        visitor.ExitSection "Expr"
        visitor.VisitToken ":?>" colonQGT
        walkType visitor typ
        visitor.ExitSection "DynamicDowncast"
    | Expr.Upcast(upcastToken, castExpr) ->
        visitor.VisitToken "Upcast" upcastToken
        visitor.EnterSection ""
        walkExpr visitor castExpr
        visitor.ExitSection ""
    | Expr.Downcast(downcastToken, castExpr) ->
        visitor.VisitToken "Downcast" downcastToken
        visitor.EnterSection ""
        walkExpr visitor castExpr
        visitor.ExitSection ""
    | Expr.ILIntrinsic(lHashParen, instrKind, instrParts, instrClose, typeArg, args, returnType, rHashParen) ->
        visitor.EnterSection "ILIntrinsic"
        visitor.VisitToken "(#" lHashParen
        walkStringKindAndParts visitor instrKind instrParts instrClose

        match typeArg with
        | ValueSome(ILTypeArg(typeKw, lParen, typeTokens, rParen)) ->
            visitor.EnterSection "ILTypeArg"
            visitor.VisitToken "type" typeKw
            visitor.VisitToken "(" lParen

            for t in typeTokens do
                visitor.VisitToken "typeToken" t

            visitor.VisitToken ")" rParen
            visitor.ExitSection "ILTypeArg"
        | ValueNone -> ()

        for arg in args do
            visitor.EnterSection "Arg"
            walkExpr visitor arg
            visitor.ExitSection "Arg"

        match returnType with
        | ValueSome(ReturnType(colon, typ)) ->
            visitor.EnterSection "ILReturnType"
            visitor.VisitToken ":" colon
            walkType visitor typ
            visitor.ExitSection "ILReturnType"
        | ValueNone -> ()

        visitor.VisitToken "#)" rHashParen
        visitor.ExitSection "ILIntrinsic"
    | Expr.Wildcard underscore -> visitor.VisitToken "Wildcard" underscore
    | Expr.Missing -> visitor.WriteLine "Missing"
    | Expr.SkipsTokens(skippedTokens) ->
        visitor.EnterSection "SkipsTokens"

        for t in skippedTokens do
            visitor.VisitToken "(skipped)" t

        visitor.ExitSection "SkipsTokens"
    | Expr.Pat innerPat ->
        visitor.EnterSection "Pat"
        walkPat visitor innerPat
        visitor.ExitSection "Pat"
    | Expr.ControlFlow(keyword, expr) ->
        let kwLabel =
            match keyword with
            | ControlFlowKeyword.Yield t ->
                visitor.VisitToken "yield" t
                "Yield"
            | ControlFlowKeyword.YieldBang t ->
                visitor.VisitToken "yield!" t
                "YieldBang"
            | ControlFlowKeyword.Return t ->
                visitor.VisitToken "return" t
                "Return"
            | ControlFlowKeyword.ReturnBang t ->
                visitor.VisitToken "return!" t
                "ReturnBang"
            | ControlFlowKeyword.Do t ->
                visitor.VisitToken "do" t
                "Do"
            | ControlFlowKeyword.DoBang t ->
                visitor.VisitToken "do!" t
                "DoBang"

        visitor.EnterSection kwLabel
        walkExpr visitor expr
        visitor.ExitSection kwLabel
    | Expr.ExpressionSplice(percent, expr) ->
        visitor.VisitToken "%" percent
        visitor.EnterSection ""
        walkExpr visitor expr
        visitor.ExitSection ""
    | Expr.WeaklyTypedExpressionSplice(percentPercent, expr) ->
        visitor.VisitToken "%%" percentPercent
        visitor.EnterSection ""
        walkExpr visitor expr
        visitor.ExitSection ""
    | Expr.StaticMemberInvocation(lParen,
                                  staticTypars,
                                  colon,
                                  lParenMember,
                                  staticToken,
                                  memberToken,
                                  membersign,
                                  rParenMember,
                                  expr,
                                  rParen) ->
        visitor.EnterSection "StaticMemberInvocation"
        visitor.VisitToken "(" lParen
        walkStaticTypars visitor staticTypars
        visitor.VisitToken ":" colon
        visitor.VisitToken "(" lParenMember
        visitTokenOpt visitor "static" staticToken
        visitor.VisitToken "member" memberToken
        walkMemberSig visitor membersign
        visitor.VisitToken ")" rParenMember
        walkExpr visitor expr
        visitor.VisitToken ")" rParen
        visitor.ExitSection "StaticMemberInvocation"
    | Expr.LibraryOnlyStaticOptimization(expr, whenToken, constraints, ands, equalsToken, optimizedExpr) ->
        visitor.EnterSection "LibraryOnlyStaticOptimization"
        walkExpr visitor expr
        visitor.VisitToken "when" whenToken

        if constraints.Length > 0 then
            walkStaticOptimizationConstraint visitor constraints.[0]

            for i in 0 .. ands.Length - 1 do
                visitor.VisitToken "and" ands.[i]
                walkStaticOptimizationConstraint visitor constraints.[i + 1]

        visitor.VisitToken "=" equalsToken
        walkExpr visitor optimizedExpr
        visitor.ExitSection "LibraryOnlyStaticOptimization"
    | Expr.String(kind, parts, closing) -> walkStringKindAndParts visitor kind parts closing
    | Expr.Object(lBrace, newKeyword, baseCall, members, interfaceImpls, rBrace) ->
        visitor.EnterSection "ObjectExpr"
        visitor.VisitToken "{" lBrace

        match newKeyword with
        | ValueSome tok -> visitor.VisitToken "new" tok
        | ValueNone -> ()

        match baseCall with
        | BaseCall.AnonBaseCall construction -> walkObjectConstruction visitor construction
        | BaseCall.NamedBaseCall(construction, asTok, ident) ->
            walkObjectConstruction visitor construction
            visitor.VisitToken "as" asTok
            visitor.VisitToken "" ident

        let (ObjectMembers.ObjectMembers(withTok, memberDefs, endTok)) = members
        visitor.VisitToken "with" withTok
        visitor.EnterSection ""

        for m in memberDefs do
            walkMemberDefn visitor m

        visitor.ExitSection ""
        visitor.VisitToken "end" endTok

        for InterfaceImpl.InterfaceImpl(interfaceTok, typ, objMembers) in interfaceImpls do
            visitor.EnterSection "InterfaceImpl"
            visitor.VisitToken "interface" interfaceTok
            walkType visitor typ

            match objMembers with
            | ValueSome(ObjectMembers.ObjectMembers(wTok, intfMembers, intfEndTok)) ->
                visitor.VisitToken "with" wTok
                visitor.EnterSection ""

                for m in intfMembers do
                    walkMemberDefn visitor m

                visitor.ExitSection ""
                visitor.VisitToken "end" intfEndTok
            | ValueNone -> ()

            visitor.ExitSection "InterfaceImpl"

        visitor.VisitToken "}" rBrace
        visitor.ExitSection "ObjectExpr"
    | Expr.Range(fromExpr, dotdot, toExpr) ->
        visitor.EnterSection "Range"
        walkExpr visitor fromExpr
        visitor.VisitToken ".." dotdot
        walkExpr visitor toExpr
        visitor.ExitSection "Range"
    | Expr.SteppedRange(fromExpr, dotdot1, stepExpr, dotdot2, toExpr) ->
        visitor.EnterSection "SteppedRange"
        walkExpr visitor fromExpr
        visitor.VisitToken ".." dotdot1
        walkExpr visitor stepExpr
        visitor.VisitToken ".." dotdot2
        walkExpr visitor toExpr
        visitor.ExitSection "SteppedRange"
    | Expr.SliceFrom(expr, dotdot) ->
        visitor.EnterSection "SliceFrom"
        walkExpr visitor expr
        visitor.VisitToken ".." dotdot
        visitor.ExitSection "SliceFrom"
    | Expr.SliceTo(dotdot, expr) ->
        visitor.EnterSection "SliceTo"
        visitor.VisitToken ".." dotdot
        walkExpr visitor expr
        visitor.ExitSection "SliceTo"
    | Expr.SliceFromTo(startExpr, dotdot, endExpr) ->
        visitor.EnterSection "SliceFromTo"
        walkExpr visitor startExpr
        visitor.VisitToken ".." dotdot
        walkExpr visitor endExpr
        visitor.ExitSection "SliceFromTo"
    | Expr.SliceAll(star) ->
        visitor.EnterSection "SliceAll"
        visitor.VisitToken "*" star
        visitor.ExitSection "SliceAll"


and walkArgSpec (visitor: AstVisitor<'T>) (argSpec: ArgSpec<'T>) : unit =
    let (ArgSpec(attrs, name, typ)) = argSpec

    walkAttributesOpt visitor attrs

    match name with
    | ValueSome(ArgNameSpec(_, ident, colon)) ->
        visitor.VisitToken "arg" ident
        visitor.VisitToken ":" colon
    | ValueNone -> ()

    walkType visitor typ

and walkUncurriedSig (visitor: AstVisitor<'T>) (sign: UncurriedSig<'T>) : unit =
    let (UncurriedSig(argsSpec, arrow, retType)) = sign
    let (ArgsSpec.ArgsSpec(args, _)) = argsSpec

    for arg in args do
        walkArgSpec visitor arg

    visitor.VisitToken "->" arrow
    walkType visitor retType

and walkCurriedSig (visitor: AstVisitor<'T>) (sign: CurriedSig<'T>) : unit =
    let (CurriedSig(argGroups, ret)) = sign

    for struct (argsSpec, arrowTok) in argGroups do
        let (ArgsSpec.ArgsSpec(args, _)) = argsSpec

        for arg in args do
            walkArgSpec visitor arg

        visitor.VisitToken "->" arrowTok

    walkType visitor ret

and walkMemberSig (visitor: AstVisitor<'T>) (sign: MemberSig<'T>) : unit =
    match sign with
    | MemberSig.MethodOrPropSig(ident, _, colon, sigType) ->
        visitor.VisitToken "ident" ident
        visitor.VisitToken ":" colon
        walkCurriedSig visitor sigType
    | MemberSig.PropSig(ident, _, colon, sigType, withTok, (first, second)) ->
        visitor.VisitToken "ident" ident
        visitor.VisitToken ":" colon
        walkCurriedSig visitor sigType
        visitor.VisitToken "with" withTok
        visitor.VisitToken "get/set" first

        visitTokenOpt visitor "set/get" second

and walkMethodOrPropDefn (visitor: AstVisitor<'T>) (defn: MethodOrPropDefn<'T>) : unit =
    match defn with
    | MethodOrPropDefn.Property(identPrefix, binding) ->
        match identPrefix with
        | ValueSome struct (ip, _dot) -> visitor.VisitToken "self" ip
        | ValueNone -> ()

        walkBinding visitor binding
    | MethodOrPropDefn.Method(identPrefix, binding) ->
        match identPrefix with
        | ValueSome struct (ip, _dot) -> visitor.VisitToken "self" ip
        | ValueNone -> ()

        walkBinding visitor binding
    | MethodOrPropDefn.PropertyWithGetSet(_, ident, withTok, bindings, _) ->
        visitor.VisitToken "ident" ident
        visitor.VisitToken "with" withTok

        for binding in bindings do
            walkBinding visitor binding
    | MethodOrPropDefn.AutoProperty(valTok, access, ident, returnType, eq, expr, withClause) ->
        visitor.VisitToken "val" valTok
        visitTokenOpt visitor "access" access
        visitor.VisitToken "ident" ident

        match returnType with
        | ValueSome(ReturnType(colon, typ)) ->
            visitor.VisitToken ":" colon
            walkType visitor typ
        | ValueNone -> ()

        visitor.VisitToken "=" eq
        visitor.EnterSection ""
        walkExpr visitor expr

        match withClause with
        | ValueSome struct (withTok, acc1, acc2) ->
            visitor.VisitToken "with" withTok
            visitor.VisitToken "accessor" acc1

            visitTokenOpt visitor "accessor" acc2
        | ValueNone -> ()

        visitor.ExitSection ""
    | MethodOrPropDefn.AbstractSignature sign -> walkMemberSig visitor sign

and walkMemberDefn (visitor: AstVisitor<'T>) (memberDefn: MemberDefn<'T>) : unit =
    match memberDefn with
    | MemberDefn.Value(attrs, staticTok, valTok, mutableTok, _, ident, colon, typ) ->
        visitor.EnterSection "Val"
        walkAttributesOpt visitor attrs
        visitTokenOpt visitor "static" staticTok
        visitor.VisitToken "val" valTok
        visitTokenOpt visitor "mutable" mutableTok
        visitor.VisitToken "ident" ident
        visitor.VisitToken ":" colon
        walkType visitor typ
        visitor.ExitSection "Val"
    | MemberDefn.Member(attrs, staticTok, keyword, inlineTok, _, defn) ->
        walkAttributesOpt visitor attrs
        visitTokenOpt visitor "static" staticTok

        match keyword with
        | MemberKeyword.Member tok -> visitor.VisitToken "member" tok
        | MemberKeyword.Override tok -> visitor.VisitToken "override" tok
        | MemberKeyword.Default tok -> visitor.VisitToken "default" tok
        | MemberKeyword.Abstract(abstractTok, memberTok) ->
            visitor.VisitToken "abstract" abstractTok
            visitTokenOpt visitor "member" memberTok

        visitTokenOpt visitor "inline" inlineTok

        walkMethodOrPropDefn visitor defn
    | MemberDefn.AdditionalConstructor(attrs, _, newTok, pat, _, eq, body) ->
        visitor.EnterSection "AdditionalConstructor"
        walkAttributesOpt visitor attrs

        visitor.VisitToken "new" newTok
        walkPat visitor pat
        visitor.VisitToken "=" eq

        walkAdditionalConstrExpr visitor body

        visitor.ExitSection "AdditionalConstructor"

and walkAdditionalConstrExpr (visitor: AstVisitor<'T>) (body: AdditionalConstrExpr<'T>) : unit =
    match body with
    | AdditionalConstrExpr.Init initExpr ->
        match initExpr with
        | AdditionalConstrInitExpr.Explicit(lBrace, inherits, inits, rBrace) ->
            visitor.VisitToken "{" lBrace

            match inherits with
            | ValueSome(ClassInheritsDecl(inhTok, typ, expr)) ->
                visitor.VisitToken "inherit" inhTok
                walkType visitor typ

                match expr with
                | ValueSome e -> walkExpr visitor e
                | ValueNone -> ()
            | ValueNone -> ()

            for FieldInitializer(longIdent, eq, expr) in inits do
                walkLongIdentOrOp visitor (LongIdentOrOp.LongIdent longIdent)
                visitor.VisitToken "=" eq
                walkExpr visitor expr

            visitor.VisitToken "}" rBrace
        | AdditionalConstrInitExpr.Delegated(newTok2, typ, expr) ->
            visitor.VisitToken "new" newTok2
            walkType visitor typ
            walkExpr visitor expr
        | AdditionalConstrInitExpr.Expression(expr) -> walkExpr visitor expr
    | AdditionalConstrExpr.SequenceBefore(before, thenToken, expr) ->
        walkAdditionalConstrExpr visitor before
        visitor.VisitToken "then" thenToken
        walkExpr visitor expr
    | _ -> visitor.WriteLine "<complex constructor body>"

and walkTypeDefnElement (visitor: AstVisitor<'T>) (elem: TypeDefnElement<'T>) : unit =
    match elem with
    | TypeDefnElement.Member m -> walkMemberDefn visitor m
    | TypeDefnElement.InterfaceImpl(InterfaceImpl.InterfaceImpl(interfaceTok, typ, objMembers)) ->
        visitor.EnterSection "InterfaceImpl"
        visitor.VisitToken "interface" interfaceTok
        walkType visitor typ

        match objMembers with
        | ValueSome(ObjectMembers(withTok, members, endTok)) ->
            visitor.VisitToken "with" withTok
            visitor.EnterSection ""

            for m in members do
                walkMemberDefn visitor m

            visitor.ExitSection ""
            visitor.VisitToken "end" endTok
        | ValueNone -> ()

        visitor.ExitSection "InterfaceImpl"
    | TypeDefnElement.InterfaceSpec _ -> visitor.WriteLine "<interface spec>"
    | TypeDefnElement.Inherit(ClassInheritsDecl(inhTok, typ, expr)) ->
        visitor.VisitToken "inherit" inhTok
        walkType visitor typ

        match expr with
        | ValueSome e -> walkExpr visitor e
        | ValueNone -> ()

and walkTypeName (visitor: AstVisitor<'T>) (typeName: TypeName<'T>) : unit =
    let (TypeName(attrs, access, prefixTypars, ident, typars, postfixConstraints)) =
        typeName

    walkAttributesOpt visitor attrs
    visitTokenOpt visitor "access" access

    match prefixTypars with
    | ValueSome(PrefixTypars.Single typar) ->
        visitor.EnterSection "PrefixTypar"
        walkTypar visitor typar
        visitor.ExitSection "PrefixTypar"
    | ValueSome(PrefixTypars.Multiple(lParen, typars, _, rParen)) ->
        visitor.EnterSection "PrefixTypars"
        visitor.VisitToken "(" lParen

        for tp in typars do
            walkTypar visitor tp

        visitor.VisitToken ")" rParen
        visitor.ExitSection "PrefixTypars"
    | ValueNone -> ()

    for tok in ident do
        visitor.VisitToken "TypeName" tok

    match typars with
    | ValueSome tp -> walkTyparDefns visitor tp
    | ValueNone -> ()

    match postfixConstraints with
    | ValueSome(TyparConstraints(whenTok, constrs, ands)) ->
        visitor.VisitToken "when" whenTok

        for i in 0 .. constrs.Length - 1 do
            walkConstraint visitor constrs[i]

            if i < ands.Length then
                visitor.VisitToken "and" ands[i]
    | ValueNone -> ()

and walkPrimaryConstrArgs (visitor: AstVisitor<'T>) (args: PrimaryConstrArgs<'T>) : unit =
    let (PrimaryConstrArgs(_, _, lParen, pat, rParen)) = args
    visitor.VisitToken "(" lParen

    match pat with
    | ValueSome p -> walkPat visitor p
    | ValueNone -> ()

    visitor.VisitToken ")" rParen

and walkUnionCaseData (visitor: AstVisitor<'T>) (data: UnionTypeCaseData<'T>) : unit =
    match data with
    | UnionTypeCaseData.Nullary ident -> visitor.VisitToken "ident" ident
    | UnionTypeCaseData.Nary(ident, ofTok, fields, _) ->
        visitor.VisitToken "ident" ident
        visitor.VisitToken "of" ofTok

        for field in fields do
            match field with
            | UnionTypeField.Unnamed typ -> walkType visitor typ
            | UnionTypeField.Named(id, colon, typ) ->
                visitor.VisitToken "ident" id
                visitor.VisitToken ":" colon
                walkType visitor typ
    | UnionTypeCaseData.NaryUncurried(ident, colon, sign) ->
        visitor.VisitToken "ident" ident
        visitor.VisitToken ":" colon
        walkUncurriedSig visitor sign

and walkExceptionDefn (visitor: AstVisitor<'T>) (exnDefn: ExceptionDefn<'T>) : unit =
    match exnDefn with
    | ExceptionDefn.Full(attrs, exTok, caseData, extensions) ->
        visitor.EnterSection "ExceptionDefn.Full"
        walkAttributesOpt visitor attrs
        visitor.VisitToken "exception" exTok
        walkUnionCaseData visitor caseData

        match extensions with
        | ValueSome(TypeExtensionElements(withTok, elems, endTok)) ->
            visitor.VisitToken "with" withTok
            visitor.EnterSection ""

            for e in elems do
                walkTypeDefnElement visitor e

            visitor.ExitSection ""
            visitor.VisitToken "end" endTok
        | ValueNone -> ()

        visitor.ExitSection "ExceptionDefn.Full"
    | ExceptionDefn.Abbreviation(attrs, exTok, ident, eq, longIdent) ->
        visitor.EnterSection "ExceptionDefn.Abbrev"
        walkAttributesOpt visitor attrs
        visitor.VisitToken "exception" exTok
        visitor.VisitToken "ident" ident
        visitor.VisitToken "=" eq

        for id in longIdent do
            visitor.VisitToken "" id

        visitor.ExitSection "ExceptionDefn.Abbrev"

and walkObjectModelBody (visitor: AstVisitor<'T>) (body: ObjectModelBody<'T>) : unit =
    match body.inherits with
    | ValueSome(ClassInheritsDecl(inhTok, typ, expr)) ->
        visitor.VisitToken "inherit" inhTok
        walkType visitor typ

        match expr with
        | ValueSome e -> walkExpr visitor e
        | ValueNone -> ()
    | ValueNone -> ()

    for preamble in body.classPreamble do
        match preamble with
        | ClassFunctionOrValueDefn.LetBindings(attrs, staticTok, letTok, isRec, bindings, _) ->
            walkAttributesOpt visitor attrs
            visitTokenOpt visitor "static" staticTok
            visitor.VisitToken "let" letTok
            visitTokenOpt visitor "rec" isRec

            for i = 0 to bindings.Length - 1 do
                let binding = bindings.[i]

                if i > 0 then
                    walkAttributesOpt visitor binding.attributes

                walkBinding visitor binding
        | ClassFunctionOrValueDefn.Do(attrs, staticTok, doTok, expr) ->
            walkAttributesOpt visitor attrs
            visitTokenOpt visitor "static" staticTok

            visitor.VisitToken "do" doTok
            walkExpr visitor expr

    for e in body.elements do
        walkTypeDefnElement visitor e

and walkTypeDefn (visitor: AstVisitor<'T>) (typeDefn: TypeDefn<'T>) : unit =
    match typeDefn with
    | TypeDefn.Abbrev(typeName, equals, typ) ->
        visitor.EnterSection "TypeDefn.Abbrev"
        walkTypeName visitor typeName
        visitor.VisitToken "=" equals
        visitor.EnterSection ""
        walkType visitor typ
        visitor.ExitSection ""
        visitor.ExitSection "TypeDefn.Abbrev"
    | TypeDefn.Union(typeName, equals, cases, _, _ext) ->
        visitor.EnterSection "TypeDefn.Union"
        walkTypeName visitor typeName
        visitor.VisitToken "=" equals
        visitor.EnterSection ""

        for (UnionTypeCase(attrs, data)) in cases do
            visitor.EnterSection "Case"
            walkAttributesOpt visitor attrs

            match data with
            | UnionTypeCaseData.Nullary ident -> visitor.VisitToken "ident" ident
            | UnionTypeCaseData.Nary(ident, ofTok, fields, _) ->
                visitor.VisitToken "ident" ident
                visitor.VisitToken "of" ofTok

                for field in fields do
                    match field with
                    | UnionTypeField.Unnamed typ -> walkType visitor typ
                    | UnionTypeField.Named(id, colon, typ) ->
                        visitor.VisitToken "ident" id
                        visitor.VisitToken ":" colon
                        walkType visitor typ
            | UnionTypeCaseData.NaryUncurried(ident, colon, _sign) ->
                visitor.VisitToken "ident" ident
                visitor.VisitToken ":" colon
                visitor.WriteLine "<uncurried sig>"

            visitor.ExitSection "Case"

        visitor.ExitSection ""
        visitor.ExitSection "TypeDefn.Union"
    | TypeDefn.Record(typeName, equals, lBrace, fields, rBrace, ext) ->
        visitor.EnterSection "TypeDefn.Record"
        walkTypeName visitor typeName
        visitor.VisitToken "=" equals
        visitor.VisitToken "{" lBrace
        visitor.EnterSection ""

        for (RecordField(attrs, mutableTok, access, id, colon, typ)) in fields do
            visitor.EnterSection "Field"
            walkAttributesOpt visitor attrs
            visitTokenOpt visitor "mutable" mutableTok
            visitTokenOpt visitor "access" access
            visitor.VisitToken "ident" id
            visitor.VisitToken ":" colon
            walkType visitor typ
            visitor.ExitSection "Field"

        visitor.ExitSection ""
        visitor.VisitToken "}" rBrace

        match ext with
        | ValueSome(TypeExtensionElements(withTok, elems, endTok)) ->
            visitor.VisitToken "with" withTok
            visitor.EnterSection ""

            for e in elems do
                walkTypeDefnElement visitor e

            visitor.ExitSection ""
            visitor.VisitToken "end" endTok
        | ValueNone -> ()

        visitor.ExitSection "TypeDefn.Record"
    | TypeDefn.Enum(typeName, equals, cases, _) ->
        visitor.EnterSection "TypeDefn.Enum"
        walkTypeName visitor typeName
        visitor.VisitToken "=" equals
        visitor.EnterSection ""

        for (EnumTypeCase(id, eq, value)) in cases do
            visitor.EnterSection "Case"
            visitor.VisitToken "ident" id
            visitor.VisitToken "=" eq
            walkExpr visitor value
            visitor.ExitSection "Case"

        visitor.ExitSection ""
        visitor.ExitSection "TypeDefn.Enum"
    | TypeDefn.Delegate(typeName, equals, DelegateSig(delTok, ofTok, sign)) ->
        visitor.EnterSection "TypeDefn.Delegate"
        walkTypeName visitor typeName
        visitor.VisitToken "=" equals
        visitor.VisitToken "delegate" delTok
        visitor.VisitToken "of" ofTok
        visitor.EnterSection ""
        walkUncurriedSig visitor sign
        visitor.ExitSection ""
        visitor.ExitSection "TypeDefn.Delegate"
    | TypeDefn.Class(typeName, _primaryConstr, _, equals, classTok, body, endTok) ->
        visitor.EnterSection "TypeDefn.Class"
        walkTypeName visitor typeName
        visitor.VisitToken "=" equals
        visitor.VisitToken "class" classTok
        visitor.EnterSection ""
        walkObjectModelBody visitor body
        visitor.ExitSection ""
        visitor.VisitToken "end" endTok
        visitor.ExitSection "TypeDefn.Class"
    | TypeDefn.Struct(typeName, _primaryConstr, _, equals, structTok, body, endTok) ->
        visitor.EnterSection "TypeDefn.Struct"
        walkTypeName visitor typeName
        visitor.VisitToken "=" equals
        visitor.VisitToken "struct" structTok
        visitor.EnterSection ""
        walkObjectModelBody visitor body
        visitor.ExitSection ""
        visitor.VisitToken "end" endTok
        visitor.ExitSection "TypeDefn.Struct"
    | TypeDefn.Interface(typeName, equals, intfTok, body, endTok) ->
        visitor.EnterSection "TypeDefn.Interface"
        walkTypeName visitor typeName
        visitor.VisitToken "=" equals
        visitor.VisitToken "interface" intfTok
        visitor.EnterSection ""
        walkObjectModelBody visitor body
        visitor.ExitSection ""
        visitor.VisitToken "end" endTok
        visitor.ExitSection "TypeDefn.Interface"
    | TypeDefn.TypeExtension(typeName, TypeExtensionElements(withTok, elems, endTok)) ->
        visitor.EnterSection "TypeDefn.TypeExtension"
        walkTypeName visitor typeName
        visitor.VisitToken "with" withTok
        visitor.EnterSection ""

        for e in elems do
            walkTypeDefnElement visitor e

        visitor.ExitSection ""
        visitor.VisitToken "end" endTok
        visitor.ExitSection "TypeDefn.TypeExtension"
    | TypeDefn.Anon(typeName, primaryConstr, _, equals, beginTok, body, endTok) ->
        visitor.EnterSection "TypeDefn.Anon"
        walkTypeName visitor typeName

        match primaryConstr with
        | ValueSome args -> walkPrimaryConstrArgs visitor args
        | ValueNone -> ()

        visitor.VisitToken "=" equals
        visitor.VisitToken "begin" beginTok
        visitor.EnterSection ""
        walkObjectModelBody visitor body
        visitor.ExitSection ""
        visitor.VisitToken "end" endTok
        visitor.ExitSection "TypeDefn.Anon"
    | TypeDefn.AbstractType(typeName) ->
        visitor.EnterSection "TypeDefn.Abstract"
        walkTypeName visitor typeName
        visitor.ExitSection "TypeDefn.Abstract"
    | TypeDefn.Missing -> visitor.WriteLine "TypeDefn.Missing"
    | TypeDefn.SkipsTokens(skippedTokens) ->
        visitor.EnterSection "TypeDefn.SkipsTokens"

        for t in skippedTokens do
            visitor.VisitToken "(skipped)" t

        visitor.ExitSection "TypeDefn.SkipsTokens"

and walkModuleFunctionOrValueDefn (visitor: AstVisitor<'T>) (defn: ModuleFunctionOrValueDefn<'T>) : unit =
    match defn with
    | ModuleFunctionOrValueDefn.Let(attrs, letToken, isRec, bindings, _) ->
        walkAttributesOpt visitor attrs
        visitor.VisitToken "let" letToken
        visitTokenOpt visitor "rec" isRec

        visitor.EnterSection ""

        for i = 0 to bindings.Length - 1 do
            let binding = bindings.[i]

            if i > 0 then
                walkAttributesOpt visitor binding.attributes

            walkBinding visitor binding

        visitor.ExitSection ""
    | ModuleFunctionOrValueDefn.Do(attrs, doToken, expr) ->
        walkAttributesOpt visitor attrs
        visitor.VisitToken "do" doToken
        visitor.EnterSection ""
        walkExpr visitor expr
        visitor.ExitSection ""

and walkImportDecl (visitor: AstVisitor<'T>) (decl: ImportDecl<'T>) : unit =
    match decl with
    | ImportDecl.ImportDecl(openToken, longIdent) ->
        visitor.VisitToken "open" openToken

        for ident in longIdent do
            visitor.VisitToken "" ident
    | ImportDecl.ImportDeclType(openToken, typeToken, longIdent) ->
        visitor.VisitToken "open" openToken
        visitor.VisitToken "type" typeToken

        for ident in longIdent do
            visitor.VisitToken "" ident

and walkModuleAbbrev (visitor: AstVisitor<'T>) (abbrev: ModuleAbbrev<'T>) : unit =
    let (ModuleAbbrev.ModuleAbbrev(moduleToken, ident, equals, longIdent)) = abbrev
    visitor.VisitToken "module" moduleToken
    visitor.VisitToken "ident" ident
    visitor.VisitToken "=" equals

    for id in longIdent do
        visitor.VisitToken "" id

and walkCompilerDirective (visitor: AstVisitor<'T>) (decl: CompilerDirectiveDecl<'T>) : unit =
    let (CompilerDirectiveDecl.CompilerDirectiveDecl(hash, ident, strings)) = decl
    visitor.VisitToken "#" hash
    visitor.VisitToken "directive" ident

    for s in strings do
        visitor.VisitToken "" s

and walkModuleElem (visitor: AstVisitor<'T>) (elem: ModuleElem<'T>) : unit =
    match elem with
    | ModuleElem.FunctionOrValue defn ->
        visitor.EnterSection "FunctionOrValue"
        walkModuleFunctionOrValueDefn visitor defn
        visitor.ExitSection "FunctionOrValue"
    | ModuleElem.Type typeDefns ->
        for typeDefn in typeDefns do
            walkTypeDefn visitor typeDefn
    | ModuleElem.Exception exnDefn -> walkExceptionDefn visitor exnDefn
    | ModuleElem.Module moduleDefn ->
        visitor.EnterSection "Module"
        walkModuleDefn visitor moduleDefn
        visitor.ExitSection "Module"
    | ModuleElem.ModuleAbbrev abbrev ->
        visitor.EnterSection "ModuleAbbrev"
        walkModuleAbbrev visitor abbrev
        visitor.ExitSection "ModuleAbbrev"
    | ModuleElem.Import importDecl ->
        visitor.EnterSection "Import"
        walkImportDecl visitor importDecl
        visitor.ExitSection "Import"
    | ModuleElem.CompilerDirective directive ->
        visitor.EnterSection "CompilerDirective"
        walkCompilerDirective visitor directive
        visitor.ExitSection "CompilerDirective"
    | ModuleElem.Expression expr -> walkExpr visitor expr
    | ModuleElem.Missing -> visitor.WriteLine "Missing"
    | ModuleElem.SkipsTokens(skippedTokens) ->
        visitor.EnterSection "SkipsTokens"

        for t in skippedTokens do
            visitor.VisitToken "(skipped)" t

        visitor.ExitSection "SkipsTokens"

and walkModuleDefn (visitor: AstVisitor<'T>) (defn: ModuleDefn<'T>) : unit =
    let (ModuleDefn.ModuleDefn(attrs, moduleToken, access, isRec, ident, equals, body)) =
        defn

    walkAttributesOpt visitor attrs
    visitor.VisitToken "module" moduleToken
    walkAccessOpt visitor access
    visitTokenOpt visitor "rec" isRec

    visitor.VisitToken "ident" ident
    visitor.VisitToken "=" equals
    let (ModuleDefnBody(beginToken, elems, endToken)) = body
    visitor.VisitToken "begin" beginToken

    match elems with
    | ValueSome elems ->
        visitor.EnterSection ""
        walkModuleElems visitor elems
        visitor.ExitSection ""
    | ValueNone -> ()

    visitor.VisitToken "end" endToken

and walkModuleElems (visitor: AstVisitor<'T>) (elems: ModuleElems<'T>) : unit =
    for elem in elems do
        walkModuleElem visitor elem

and walkNamespaceDeclGroup (visitor: AstVisitor<'T>) (group: NamespaceDeclGroup<'T>) : unit =
    match group with
    | NamespaceDeclGroup.Named(nsTok, isRec, longIdent, elems) ->
        visitor.VisitToken "namespace" nsTok
        visitTokenOpt visitor "rec" isRec

        for id in longIdent do
            visitor.VisitToken "" id

        visitor.EnterSection ""
        walkModuleElems visitor elems
        visitor.ExitSection ""
    | NamespaceDeclGroup.Global(nsTok, globalTok, elems) ->
        visitor.VisitToken "namespace" nsTok
        visitor.VisitToken "global" globalTok
        visitor.EnterSection ""
        walkModuleElems visitor elems
        visitor.ExitSection ""

and walkImplementationFile (visitor: AstVisitor<'T>) (file: ImplementationFile<'T>) : unit =
    match file with
    | ImplementationFile.AnonymousModule elems ->
        visitor.WriteLine "AnonymousModule:"
        walkModuleElems visitor elems
    | ImplementationFile.NamedModule namedModule ->
        let (NamedModule.NamedModule(attrs, modTok, access, isRec, longIdent, elems)) =
            namedModule

        walkAttributesOpt visitor attrs
        visitor.VisitToken "module" modTok
        walkAccessOpt visitor access
        visitTokenOpt visitor "rec" isRec

        for id in longIdent do
            visitor.VisitToken "" id

        walkModuleElems visitor elems
    | ImplementationFile.Namespaces groups ->
        for group in groups do
            walkNamespaceDeclGroup visitor group

and walkFSharpAst (visitor: AstVisitor<'T>) (ast: FSharpAst<'T>) : unit =
    match ast with
    | FSharpAst.ImplementationFile file -> walkImplementationFile visitor file
    | FSharpAst.SignatureFile _ -> visitor.WriteLine "SignatureFile: <not yet implemented>"
    | FSharpAst.ScriptFile _ -> visitor.WriteLine "ScriptFile: <not yet implemented>"
    | FSharpAst.ScriptFragment(ScriptFragment.ScriptFragment elems) ->
        visitor.WriteLine "ScriptFragment:"
        walkModuleElems visitor elems
