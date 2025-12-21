namespace rec XParsec.FSharp.Parser

// 13. Custom Attributes and Reflection
// Represents: attribute-target
type AttributeTarget<'T> =
    | Assembly of assemblyToken: 'T
    | Module of moduleToken: 'T
    | Return of returnToken: 'T
    | Field of fieldToken: 'T
    | Property of propertyToken: 'T
    | Param of paramToken: 'T
    | Type of typeToken: 'T
    | Constructor of constructorToken: 'T
    | Event of eventToken: 'T

// Represents: attribute := attribute-target : opt object-construction
type Attribute<'T> =
    | Attribute of target: (AttributeTarget<'T> * 'T (* colon *) ) voption * construction: ObjectConstruction<'T>

// Represents: attribute-set := [< attribute ; ... ; attribute >]
and AttributeSet<'T> =
    | AttributeSet of
        lBracket: 'T *  // Represents the [< token
        attributes: (Attribute<'T> * 'T (* semicolon *) option) list *
        rBracket: 'T // Represents the >] token

// Represents: attributes := attribute-set ... attribute-set
// This is the final type that should replace the placeholder.
and Attributes<'T> = AttributeSet<'T> list

// Represents: ident-or-op
type IdentOrOp<'T> =
    | Ident of ident: 'T
    | ParenOp of lParen: 'T * opName: OpName<'T> * rParen: 'T
    | StarOp of lParen: 'T * star: 'T * rParen: 'T

// Represents: op-name and its variations
and OpName<'T> =
    | SymbolicOp of op: 'T
    | RangeOp of rangeOp: RangeOpName<'T>
    | ActivePatternOp of activePatternOp: ActivePatternOpName<'T>

and RangeOpName<'T> =
    | DotDot of 'T
    | DotDotDotDot of 'T

and ActivePatternOpName<'T> = | ActivePatternOp of lBar: 'T * idents: 'T list * finalUnderscore: 'T voption * rBar: 'T

// Represents: long-ident and long-ident-or-op
type LongIdent<'T> = 'T list

type LongIdentOrOp<'T> =
    | LongIdent of LongIdent<'T>
    | Op of IdentOrOp<'T>
    | QualifiedOp of longIdent: LongIdent<'T> * dot: 'T * op: IdentOrOp<'T>

// Represents: type and related grammar constructs
type Type<'T> =
    | ParenType of lParen: 'T * typ: Type<'T> * rParen: 'T
    | FunctionType of fromType: Type<'T> * arrow: 'T * toType: Type<'T>
    | TupleType of types: Type<'T> list
    | StructTupleType of structToken: 'T * lParen: 'T * types: Type<'T> list * rParen: 'T
    | VarType of typar: Typar<'T>
    | NamedType of longIdent: LongIdent<'T>
    | GenericType of longIdent: LongIdent<'T> * lAngle: 'T * typeArgs: TypeArg<'T> list * rAngle: 'T
    | IncompleteGenericType of longIdent: LongIdent<'T> * lAngle: 'T * rAngle: 'T
    | SuffixedType of baseType: Type<'T> * longIdent: LongIdent<'T>
    | ArrayType of baseType: Type<'T> * lBracket: 'T * commas: 'T list * rBracket: 'T
    | ConstrainedType of typ: Type<'T> * constraints: TyparDefns<'T>
    | SubtypeConstraint of typar: Typar<'T> * colonGreaterThan: 'T * typ: Type<'T>
    | AnonymousSubtype of hash: 'T * typ: Type<'T>

and TypeArg<'T> =
    | Type of Type<'T>
    | Measure of 'T // Placeholder for measure grammar
    | StaticParameter of 'T // Placeholder for static-parameter grammar

and Typar<'T> =
    | Anon of underscore: 'T
    | Named of quote: 'T * ident: 'T
    | Static of caret: 'T * ident: 'T

and TyparDefns<'T> =
    | TyparDefns of lAngle: 'T * defns: TyparDefn<'T> list * constraints: TyparConstraints<'T> voption * rAngle: 'T

and TyparDefn<'T> = | TyparDefn of attributes: Attributes<'T> voption * typar: Typar<'T>
and TyparConstraints<'T> = | TyparConstraints of whenToken: 'T * constraints: Constraint<'T> list

and Constraint<'T> =
    | Coercion of typar: Typar<'T> * colonGreaterThan: 'T * typ: Type<'T>
    | Nullness of typar: Typar<'T> * colon: 'T * nullToken: 'T
    | MemberTrait of staticTypars: StaticTypars<'T> * colon: 'T * lParen: 'T * membersign: MemberSig<'T> * rParen: 'T
    | DefaultConstructor of
        typar: Typar<'T> *
        colon: 'T *
        lParen: 'T *
        newToken: 'T *
        colonUnit: 'T *
        arrow: 'T *
        quoteT: 'T *
        rParen: 'T
    | Struct of typar: Typar<'T> * colon: 'T * structToken: 'T
    | ReferenceType of typar: Typar<'T> * colon: 'T * notToken: 'T * structToken: 'T
    | Enum of typar: Typar<'T> * colon: 'T * enumToken: 'T * lAngle: 'T * typ: Type<'T> * rAngle: 'T
    | Unmanaged of typar: Typar<'T> * colon: 'T * unmanagedToken: 'T
    | Delegate of
        typar: Typar<'T> *
        colon: 'T *
        delegateToken: 'T *
        lAngle: 'T *
        type1: Type<'T> *
        comma: 'T *
        type2: Type<'T> *
        rAngle: 'T
    | Equality of typar: Typar<'T> * colon: 'T * equalityToken: 'T
    | Comparison of typar: Typar<'T> * colon: 'T * comparisonToken: 'T

and StaticTypars<'T> =
    | Single of caret: 'T * ident: 'T
    | OrList of lParen: 'T * typars: (Typar<'T> * 'T (* or *) ) list * rParen: 'T

// Represents: elif-branch and else-branch
type ElifBranch<'T> = | ElifBranch of elifToken: 'T * condition: Expr<'T> * thenToken: 'T * expr: Expr<'T>
type ElseBranch<'T> = | ElseBranch of elseToken: 'T * expr: Expr<'T>

// Represents: function-or-value-defn and related grammar
type ReturnType<'T> = | ReturnType of colon: 'T * typ: Type<'T>

type FunctionDefn<'T> =
    | FunctionDefn of
        inlineToken: 'T voption *
        access: 'T voption *
        identOrOp: IdentOrOp<'T> *
        typarDefns: TyparDefns<'T> voption *
        argumentPats: Pat<'T> list *
        returnType: ReturnType<'T> voption *
        equals: 'T *
        expr: Expr<'T>

type ValueDefn<'T> =
    | ValueDefn of
        mutableToken: 'T voption *
        access: 'T voption *
        pat: Pat<'T> *
        typarDefns: TyparDefns<'T> voption *
        returnType: ReturnType<'T> voption *
        equals: 'T *
        expr: Expr<'T>

type FunctionOrValueDefn<'T> =
    | Function of FunctionDefn<'T>
    | Value of ValueDefn<'T>

// Represents: record, object, and interface implementation grammar
type FieldInitializer<'T> = | FieldInitializer of longIdent: LongIdent<'T> * equals: 'T * expr: Expr<'T>

type ObjectConstruction<'T> =
    | ObjectConstruction of typ: Type<'T> * expr: Expr<'T>
    | InterfaceConstruction of typ: Type<'T>

type BaseCall<'T> =
    | AnonBaseCall of construction: ObjectConstruction<'T>
    | NamedBaseCall of construction: ObjectConstruction<'T> * asToken: 'T * ident: 'T

type ObjectMembers<'T> = | ObjectMembers of withToken: 'T * memberDefns: MemberDefn<'T> list * endToken: 'T
type InterfaceImpl<'T> = | InterfaceImpl of interfaceToken: 'T * typ: Type<'T> * objectMembers: ObjectMembers<'T> option

// Represents: range and slice grammar
type SliceRange<'T> =
    | Single of expr: Expr<'T>
    | From of expr: Expr<'T> * dotdot: 'T
    | To of dotdot: 'T * expr: Expr<'T>
    | FromTo of startExpr: Expr<'T> * dotdot: 'T * endExpr: Expr<'T>
    | All of star: 'T

type RangeExpr<'T> =
    | SimpleRange of fromExpr: Expr<'T> * dotdot: 'T * toExpr: Expr<'T>
    | SteppedRange of fromExpr: Expr<'T> * dotdot1: 'T * stepExpr: Expr<'T> * dotdot2: 'T * toExpr: Expr<'T>

[<RequireQualifiedAccess>]
type ExprOrRange<'T> =
    | Expr of Expr<'T>
    | Range of RangeExpr<'T>

// Represents: computation expression grammar
type CompExpr<'T> =
    | LetBang of letBang: 'T * pat: Pat<'T> * equals: 'T * expr: Expr<'T> * inToken: 'T * comp: CompExpr<'T>
    | Let of letToken: 'T * pat: Pat<'T> * equals: 'T * expr: Expr<'T> * inToken: 'T * comp: CompExpr<'T>
    | DoBang of doBang: 'T * expr: Expr<'T> * inToken: 'T * comp: CompExpr<'T>
    | Do of doToken: 'T * expr: Expr<'T> * inToken: 'T * comp: CompExpr<'T>
    | UseBang of useBang: 'T * pat: Pat<'T> * equals: 'T * expr: Expr<'T> * inToken: 'T * comp: CompExpr<'T>
    | Use of useToken: 'T * pat: Pat<'T> * equals: 'T * expr: Expr<'T> * inToken: 'T * comp: CompExpr<'T>
    | YieldBang of yieldBang: 'T * expr: Expr<'T>
    | Yield of yieldToken: 'T * expr: Expr<'T>
    | ReturnBang of returnBang: 'T * expr: Expr<'T>
    | Return of returnToken: 'T * expr: Expr<'T>
    | IfThen of ifToken: 'T * cond: Expr<'T> * thenToken: 'T * comp: CompExpr<'T>
    | IfThenElse of
        ifToken: 'T *
        cond: Expr<'T> *
        thenToken: 'T *
        thenExpr: Expr<'T> *
        elseToken: 'T *
        elseComp: CompExpr<'T>
    | MatchBang of matchBang: 'T * expr: Expr<'T> * withToken: 'T * rules: Rules<'T>
    | Match of matchToken: 'T * expr: Expr<'T> * withToken: 'T * rules: Rules<'T>
    | TryWith of tryToken: 'T * comp: CompExpr<'T> * withToken: 'T * rules: Rules<'T>
    | TryFinally of tryToken: 'T * tryComp: CompExpr<'T> * finallyToken: 'T * finallyExpr: Expr<'T>
    | While of whileToken: 'T * cond: Expr<'T> * doToken: 'T * comp: CompExpr<'T> * doneToken: 'T
    | ForTo of
        forToken: 'T *
        ident: 'T *
        equals: 'T *
        start: Expr<'T> *
        toToken: 'T *
        endExpr: Expr<'T> *
        doToken: 'T *
        comp: CompExpr<'T> *
        doneToken: 'T
    | ForIn of
        forToken: 'T *
        pat: Pat<'T> *
        inToken: 'T *
        exprOrRange: ExprOrRange<'T> *
        doToken: 'T *
        comp: CompExpr<'T> *
        doneToken: 'T
    | Sequential of comp1: CompExpr<'T> * semicolon: 'T * comp2: CompExpr<'T>
    | BaseExpr of expr: Expr<'T>

type ShortCompExpr<'T> =
    | ShortCompExpr of
        forToken: 'T *
        pat: Pat<'T> *
        inToken: 'T *
        exprOrRange: ExprOrRange<'T> *
        arrow: 'T *
        expr: Expr<'T>

type CompOrRangeExpr<'T> =
    | Comp of CompExpr<'T>
    | ShortComp of ShortCompExpr<'T>
    | Range of RangeExpr<'T>

// The complete expression type
and [<RequireQualifiedAccess>] Expr<'T> =
    // Constants and Blocks
    | Const of value: Constant<'T>
    | ParenBlock of lParen: 'T * expr: Expr<'T> * rParen: 'T
    | BeginEndBlock of beginToken: 'T * expr: Expr<'T> * endToken: 'T
    // Lookups and Applications
    | LongIdentOrOp of longIdentOrOp: LongIdentOrOp<'T>
    | DotLookup of expr: Expr<'T> * dot: 'T * longIdentOrOp: LongIdentOrOp<'T>
    | App of funcExpr: Expr<'T> * argExpr: Expr<'T>
    | HighPrecedenceApp of funcExpr: Expr<'T> * lParen: 'T * argExpr: Expr<'T> * rParen: 'T
    | TypeApp of expr: Expr<'T> * lAngle: 'T * types: Type<'T> list * rAngle: 'T
    | InfixApp of leftExpr: Expr<'T> * infixOp: 'T * rightExpr: Expr<'T>
    | PrefixApp of prefixOp: 'T * expr: Expr<'T>
    | IndexedLookup of expr: Expr<'T> * lDotBracket: 'T * indexExpr: Expr<'T> * rBracket: 'T
    | Slice of expr: Expr<'T> * lDotBracket: 'T * sliceRanges: SliceRange<'T> list * rBracket: 'T
    // Data Structures
    | Assignment of leftExpr: Expr<'T> * arrow: 'T * rightExpr: Expr<'T>
    | Tuple of exprs: Expr<'T> list
    | StructTuple of structToken: 'T * lParen: 'T * exprs: Expr<'T> list * rParen: 'T
    | List of lBracket: 'T * exprs: Expr<'T> list * rBracket: 'T
    | Array of lBar: 'T * exprs: Expr<'T> list * rBar: 'T
    // Objects and Records
    | New of newToken: 'T * typ: Type<'T> * expr: Expr<'T>
    | Object of
        lBrace: 'T *
        newKeyword: 'T voption *
        baseCall: BaseCall<'T> *
        members: ObjectMembers<'T> *
        interfaceImpls: InterfaceImpl<'T> list *
        rBrace: 'T
    | Record of lBrace: 'T * fieldInitializers: FieldInitializer<'T> list * rBrace: 'T
    | RecordClone of
        lBrace: 'T *
        expr: Expr<'T> *
        withToken: 'T *
        fieldInitializers: FieldInitializer<'T> list *
        rBrace: 'T
    // Computation Expressions
    | Computation of expr: Expr<'T> * lBrace: 'T * compOrRange: CompOrRangeExpr<'T> * rBrace: 'T
    | ComputedList of lBracket: 'T * compOrRange: CompOrRangeExpr<'T> * rBracket: 'T
    | ComputedArray of lBar: 'T * compOrRange: CompOrRangeExpr<'T> * rBar: 'T
    | Lazy of lazyToken: 'T * expr: Expr<'T>
    | Null of nullToken: 'T
    // Type-related Expressions
    | TypeAnnotation of expr: Expr<'T> * colon: 'T * typ: Type<'T>
    | StaticUpcast of expr: Expr<'T> * colonGreaterThan: 'T * typ: Type<'T>
    | DynamicTypeTest of expr: Expr<'T> * colonQuestionMark: 'T * typ: Type<'T>
    | DynamicDowncast of expr: Expr<'T> * colonQuestionMarkGreaterThan: 'T * typ: Type<'T>
    | Upcast of upcastToken: 'T * expr: Expr<'T>
    | Downcast of downcastToken: 'T * expr: Expr<'T>
    // Let and Use Bindings
    | LetFunction of letToken: 'T * functionDefn: FunctionDefn<'T> * inToken: 'T * expr: Expr<'T>
    | LetValue of letToken: 'T * valueDefn: ValueDefn<'T> * inToken: 'T * expr: Expr<'T>
    | LetRec of letToken: 'T * recToken: 'T * defns: FunctionOrValueDefn<'T> list * inToken: 'T * expr: Expr<'T>
    | Use of useToken: 'T * ident: 'T * equals: 'T * expr: Expr<'T> * inToken: 'T * bodyExpr: Expr<'T>
    | UseFixed of useToken: 'T * ident: 'T * equals: 'T * fixedToken: 'T * expr: Expr<'T>
    // Functions and Matching
    | Fun of funToken: 'T * argumentPats: Pat<'T> list * arrow: 'T * expr: Expr<'T>
    | Function of functionToken: 'T * rules: Rules<'T>
    | Sequential of expr1: Expr<'T> * semicolon: 'T * expr2: Expr<'T>
    | Match of matchToken: 'T * expr: Expr<'T> * withToken: 'T * rules: Rules<'T>
    | TryWith of tryToken: 'T * expr: Expr<'T> * withToken: 'T * rules: Rules<'T>
    | TryFinally of tryToken: 'T * tryExpr: Expr<'T> * finallyToken: 'T * finallyExpr: Expr<'T>
    // Control Flow
    | IfThenElse of
        ifToken: 'T *
        condition: Expr<'T> *
        thenToken: 'T *
        thenExpr: Expr<'T> *
        elifBranches: ElifBranch<'T> list *
        elseBranch: ElseBranch<'T> option
    | While of whileToken: 'T * condition: Expr<'T> * doToken: 'T * body: Expr<'T> * doneToken: 'T
    | ForTo of
        forToken: 'T *
        ident: 'T *
        equals: 'T *
        startExpr: Expr<'T> *
        toToken: 'T *
        endExpr: Expr<'T> *
        doToken: 'T *
        body: Expr<'T> *
        doneToken: 'T
    | ForIn of
        forToken: 'T *
        pat: Pat<'T> *
        inToken: 'T *
        enumerableExpr: ExprOrRange<'T> *
        doToken: 'T *
        body: Expr<'T> *
        doneToken: 'T
    // Other
    | Assert of assertToken: 'T * expr: Expr<'T>
    | Quoted of lAngleAt: 'T * expr: Expr<'T> * rAtAngle: 'T
    | DoubleQuoted of lAngleAtAt: 'T * expr: Expr<'T> * rAtAtAngle: 'T
    | ExpressionSplice of percent: 'T * expr: Expr<'T>
    | WeaklyTypedExpressionSplice of percentPercent: 'T * expr: Expr<'T>
    | StaticMemberInvocation of
        lParen: 'T *
        staticTypars: StaticTypars<'T> *
        colon: 'T *
        lParenMember: 'T *
        membersign: MemberSig<'T> *
        rParenMember: 'T *
        expr: Expr<'T> *
        rParen: 'T
    // Incomplete or Placeholder
    | Missing // Placeholder for missing expression
    | SkipsTokens of skippedTokens: 'T list * expr: Expr<'T> // Placeholder for skipped tokens
    // Added to make things work
    | Ident of ident: 'T
    | Pat of pattern: Pat<'T>


// Patterns


// Represents: pat-param
type PatParam<'T> =
    | Const of value: 'T
    | LongIdent of ident: LongIdent<'T>
    | List of lBracket: 'T * parameters: PatParam<'T> list * rBracket: 'T
    | Tuple of lParen: 'T * parameters: PatParam<'T> list * rParen: 'T
    | App of ident: LongIdent<'T> * param: PatParam<'T>
    | Typed of param: PatParam<'T> * colon: 'T * typ: Type<'T>
    | Quoted of lAngleAt: 'T * expr: Expr<'T> * rAtAngle: 'T
    | DoubleQuoted of lAngleAtAt: 'T * expr: Expr<'T> * rAtAtAngle: 'T
    | Null of nullToken: 'T

// Represents: field-pat := long-ident = pat
and FieldPat<'T> = | FieldPat of longIdent: LongIdent<'T> * equals: 'T * pat: Pat<'T>

// Represents: list-pat
and ListPat<'T> = | ListPat of lBracket: 'T * patterns: Pat<'T> list * rBracket: 'T

// Represents: array-pat
and ArrayPat<'T> = | ArrayPat of lBarBracket: 'T * patterns: Pat<'T> list * rBarBracket: 'T

// Represents: record-pat
and RecordPat<'T> = | RecordPat of lBrace: 'T * fieldPats: FieldPat<'T> list * rBrace: 'T

// Represents: pat and its variations
and [<RequireQualifiedAccess>] Pat<'T> =
    | Const of value: 'T
    | NamedSimple of ident: 'T
    | Named of longIdent: LongIdent<'T> * param: PatParam<'T> voption * pat: Pat<'T> option
    | Wildcard of underscore: 'T
    | As of pat: Pat<'T> * asToken: 'T * ident: 'T
    | Or of left: Pat<'T> * bar: 'T * right: Pat<'T>
    | And of left: Pat<'T> * ampersand: 'T * right: Pat<'T>
    | Cons of head: Pat<'T> * consToken: 'T * tail: Pat<'T>
    | Typed of pat: Pat<'T> * colon: 'T * typ: Type<'T>
    | Tuple of patterns: Pat<'T> list
    | StructTuple of structToken: 'T * lParen: 'T * patterns: Pat<'T> list * rParen: 'T
    | Paren of lParen: 'T * pat: Pat<'T> * rParen: 'T
    | List of listPat: ListPat<'T>
    | Array of arrayPat: ArrayPat<'T>
    | Record of recordPat: RecordPat<'T>
    | TypeTest of colonQuestion: 'T * typ: Type<'T>
    | TypeTestAs of colonQuestion: 'T * typ: Type<'T> * asToken: 'T * ident: 'T
    | Null of nullToken: 'T
    | Attributed of attributes: Attributes<'T> * pat: Pat<'T>

// Represents: pattern-guard := when expr
and PatternGuard<'T> = | PatternGuard of whenToken: 'T * expr: Expr<'T>

// Represents: rule := pat pattern-guard~opt -> expr
and Rule<'T> = | Rule of pat: Pat<'T> * guard: PatternGuard<'T> voption * arrow: 'T * expr: Expr<'T>

// Represents: rules := '|'~opt rule '|' ... '|' rule
and Rules<'T> = | Rules of rules: ('T (* bar *) voption * Rule<'T>) list


// 8 Type Definitions

// Below are the F# types that model the provided grammar for type definitions.

// Represents: simple-pat
type SimplePat<'T> =
    | Ident of ident: 'T
    | Typed of pat: SimplePat<'T> * colon: 'T * typ: Type<'T>

// Represents: primary-constr-args
type PrimaryConstrArgs<'T> =
    | PrimaryConstrArgs of
        attributes: Attributes<'T> voption *
        access: 'T voption *  // Placeholder for access modifier
        lParen: 'T *
        pats: SimplePat<'T> list *
        rParen: 'T

// Represents: type-name
type TypeName<'T> =
    | TypeName of
        attributes: Attributes<'T> voption *
        access: 'T voption *  // Placeholder for access modifier
        ident: 'T *
        typarDefns: TyparDefns<'T> option

// Represents: type-defn-element and related constructs
type TypeDefnElement<'T> =
    | Member of MemberDefn<'T>
    | InterfaceImpl of InterfaceImpl<'T>
    | InterfaceSpec of InterfaceSpec<'T>

and InterfaceSpec<'T> = | InterfaceSpec of interfaceToken: 'T * typ: Type<'T>

and TypeDefnElements<'T> = TypeDefnElement<'T> list

// Represents: member-sig and related signature constructs
and ArgNameSpec<'T> = | ArgNameSpec of optional: 'T voption * ident: 'T * colon: 'T

and ArgSpec<'T> = | ArgSpec of attributes: Attributes<'T> voption * name: ArgNameSpec<'T> voption * typ: Type<'T>

and ArgsSpec<'T> = ArgSpec<'T> list

and CurriedSig<'T> = | CurriedSig of args: ArgsSpec<'T> list * arrow: 'T * returnType: Type<'T>

and UncurriedSig<'T> = | UncurriedSig of args: ArgsSpec<'T> * arrow: 'T * returnType: Type<'T>

and MemberSig<'T> =
    | MethodOrPropSig of ident: 'T * typarDefns: TyparDefns<'T> voption * colon: 'T * sign: CurriedSig<'T>
    | PropSig of
        ident: 'T *
        typarDefns: TyparDefns<'T> voption *
        colon: 'T *
        sign: CurriedSig<'T> *
        withToken: 'T *
        getSet: ('T * 'T option) // (get, option<set>) or (set, option<get>)

// Represents: method-or-prop-defn
and MethodOrPropDefn<'T> =
    | Method of ident: 'T voption * defn: FunctionDefn<'T>
    | Property of ident: 'T voption * defn: ValueDefn<'T>
    | PropertyWithGetSet of identPrefix: 'T voption * ident: 'T * withToken: 'T * defns: FunctionOrValueDefn<'T> list
    | AutoProperty of
        memberToken: 'T *
        ident: 'T *
        equals: 'T *
        expr: Expr<'T> *
        withClause: ('T * 'T * 'T option) voption // with, get/set, optional comma and other get/set

// Represents: additional-constr-defn and its expression body
and AdditionalConstrExpr<'T> =
    | SequenceAfter of stmt: 'T * semicolon: 'T * rest: AdditionalConstrExpr<'T> // Placeholder for 'stmt'
    | SequenceBefore of before: AdditionalConstrExpr<'T> * thenToken: 'T * expr: Expr<'T>
    | Conditional of
        ifToken: 'T *
        cond: Expr<'T> *
        thenToken: 'T *
        thenBranch: AdditionalConstrExpr<'T> *
        elseToken: 'T *
        elseBranch: AdditionalConstrExpr<'T>
    | LetIn of letToken: 'T * defn: FunctionOrValueDefn<'T> * inToken: 'T * body: AdditionalConstrExpr<'T>
    | Init of initExpr: AdditionalConstrInitExpr<'T>

and AdditionalConstrInitExpr<'T> =
    | Explicit of lBrace: 'T * inherits: ClassInheritsDecl<'T> * initializers: FieldInitializer<'T> list * rBrace: 'T
    | Delegated of newToken: 'T * typ: Type<'T> * expr: Expr<'T>

and AsDefn<'T> = | AsDefn of asToken: 'T * ident: 'T

and AdditionalConstrDefn<'T> =
    | AdditionalConstrDefn of
        attributes: Attributes<'T> voption *
        access: 'T voption *
        newToken: 'T *
        pat: Pat<'T> *
        asDefn: AsDefn<'T> voption *
        equals: 'T *
        body: AdditionalConstrExpr<'T>

// Represents: member-defn
and MemberDefn<'T> =
    | Concrete of
        attributes: Attributes<'T> voption *
        staticToken: 'T voption *
        memberToken: 'T *
        access: 'T voption *
        defn: MethodOrPropDefn<'T>
    | Abstract of
        attributes: Attributes<'T> voption *
        abstractToken: 'T *
        memberToken: 'T voption *
        access: 'T voption *
        sign: MemberSig<'T>
    | Override of
        attributes: Attributes<'T> voption *
        overrideToken: 'T *
        access: 'T voption *
        defn: MethodOrPropDefn<'T>
    | Default of attributes: Attributes<'T> voption * defaultToken: 'T * access: 'T voption * defn: MethodOrPropDefn<'T>
    | Value of
        attributes: Attributes<'T> voption *
        staticToken: 'T voption *
        valToken: 'T *
        mutableToken: 'T voption *
        access: 'T voption *
        ident: 'T *
        colon: 'T *
        typ: Type<'T>
    | AdditionalConstructor of constr: AdditionalConstrDefn<'T>

// Represents: class, struct, and interface bodies
and ClassInheritsDecl<'T> = | ClassInheritsDecl of inheritToken: 'T * typ: Type<'T> * expr: Expr<'T> option

and ClassFunctionOrValueDefn<'T> =
    | LetRecDefns of
        attributes: Attributes<'T> voption *
        staticToken: 'T voption *
        letToken: 'T *
        recToken: 'T voption *
        defns: FunctionOrValueDefn<'T> list
    | Do of attributes: Attributes<'T> voption * staticToken: 'T voption * doToken: 'T * expr: Expr<'T>

and ClassTypeBody<'T> =
    | ClassTypeBody of
        inherits: ClassInheritsDecl<'T> voption *
        defns: ClassFunctionOrValueDefn<'T> list *
        elements: TypeDefnElements<'T> option

and StructTypeBody<'T> = | StructTypeBody of elements: TypeDefnElements<'T>

and InterfaceTypeBody<'T> = | InterfaceTypeBody of elements: TypeDefnElements<'T>

// Represents: union-type-defn and its cases
and UnionTypeField<'T> =
    | Unnamed of typ: Type<'T>
    | Named of ident: 'T * colon: 'T * typ: Type<'T>

and UnionTypeCaseData<'T> =
    | Nullary of ident: 'T
    | Nary of ident: 'T * ofToken: 'T * fields: UnionTypeField<'T> list
    | NaryUncurried of ident: 'T * colon: 'T * sign: UncurriedSig<'T>

and UnionTypeCase<'T> = | UnionTypeCase of attributes: Attributes<'T> voption * data: UnionTypeCaseData<'T>

and UnionTypeCases<'T> = UnionTypeCase<'T> list

// Represents: record-type-defn and its fields
and RecordField<'T> =
    | RecordField of
        attributes: Attributes<'T> voption *
        mutableToken: 'T voption *
        access: 'T voption *
        ident: 'T *
        colon: 'T *
        typ: Type<'T>

and RecordFields<'T> = RecordField<'T> list

// Represents: enum-type-defn and its cases
and EnumTypeCase<'T> = | EnumTypeCase of ident: 'T * equals: 'T * constValue: 'T

and EnumTypeCases<'T> = EnumTypeCase<'T> list

// Represents: type-extension
and TypeExtensionElements<'T> = | TypeExtensionElements of withToken: 'T * elements: TypeDefnElements<'T> * endToken: 'T

// Represents: exception-defn
and ExceptionDefn<'T> =
    | Full of attributes: Attributes<'T> voption * exceptionToken: 'T * caseData: UnionTypeCaseData<'T>
    | Abbreviation of
        attributes: Attributes<'T> voption *
        exceptionToken: 'T *
        ident: 'T *
        equals: 'T *
        longIdent: LongIdent<'T>

// Represents: delegate-type-defn
and DelegateSig<'T> = | DelegateSig of delegateToken: 'T * ofToken: 'T * sign: UncurriedSig<'T>

// Represents: type-defn, the top-level definition
and TypeDefn<'T> =
    | Abbrev of typeName: TypeName<'T> * equals: 'T * typ: Type<'T>
    | Record of
        typeName: TypeName<'T> *
        equals: 'T *
        lBrace: 'T *
        fields: RecordFields<'T> *
        rBrace: 'T *
        extensions: TypeExtensionElements<'T> option
    | Union of
        typeName: TypeName<'T> *
        equals: 'T *
        cases: UnionTypeCases<'T> *
        extensions: TypeExtensionElements<'T> option
    | Anon of
        typeName: TypeName<'T> *
        primaryConstr: PrimaryConstrArgs<'T> voption *
        objectVal: 'T voption *  // Placeholder
        equals: 'T *
        beginToken: 'T *
        body: ClassTypeBody<'T> *
        endToken: 'T
    | Class of
        typeName: TypeName<'T> *
        primaryConstr: PrimaryConstrArgs<'T> voption *
        objectVal: 'T voption *  // Placeholder
        equals: 'T *
        classToken: 'T *
        body: ClassTypeBody<'T> *
        endToken: 'T
    | Struct of
        typeName: TypeName<'T> *
        primaryConstr: PrimaryConstrArgs<'T> voption *
        asDefn: AsDefn<'T> voption *
        equals: 'T *
        structToken: 'T *
        body: StructTypeBody<'T> *
        endToken: 'T
    | Interface of typeName: TypeName<'T> * equals: 'T * interfaceToken: 'T * body: InterfaceTypeBody<'T> * endToken: 'T
    | Enum of typeName: TypeName<'T> * equals: 'T * cases: EnumTypeCases<'T>
    | Delegate of typeName: TypeName<'T> * equals: 'T * sign: DelegateSig<'T>
    | TypeExtension of typeName: TypeName<'T> * elements: TypeExtensionElements<'T>


// 9 Units of Measure

// Represents: measure-literal-atom
type MeasureLiteralAtom<'T> =
    | LongIdent of LongIdent<'T>
    | Paren of lParen: 'T * measure: MeasureLiteralSimp<'T> * rParen: 'T

// Represents: measure-literal-power
and MeasureLiteralPower<'T> =
    | Atom of MeasureLiteralAtom<'T>
    | Power of atom: MeasureLiteralAtom<'T> * caret: 'T * power: 'T // int32 token

// Represents: measure-literal-seq (implicit product)
and MeasureLiteralSeq<'T> = MeasureLiteralPower<'T> list

// Represents: measure-literal-simp
and MeasureLiteralSimp<'T> =
    | Seq of MeasureLiteralSeq<'T>
    | Product of left: MeasureLiteralSimp<'T> * star: 'T * right: MeasureLiteralSimp<'T>
    | Quotient of left: MeasureLiteralSimp<'T> * slash: 'T * right: MeasureLiteralSimp<'T>
    | Reciprocal of slash: 'T * measure: MeasureLiteralSimp<'T>
    | One of oneToken: 'T

// Represents: measure-literal, the top-level type for literals
and MeasureLiteral<'T> =
    | Anon of underscore: 'T
    | Simple of measure: MeasureLiteralSimp<'T>

// Represents: measure-atom
type MeasureAtom<'T> =
    | Typar of Typar<'T>
    | LongIdent of LongIdent<'T>
    | Paren of lParen: 'T * measure: MeasureSimp<'T> * rParen: 'T

// Represents: measure-power
and MeasurePower<'T> =
    | Atom of MeasureAtom<'T>
    | Power of atom: MeasureAtom<'T> * caret: 'T * power: 'T // int32 token

// Represents: measure-seq (implicit product)
and MeasureSeq<'T> = MeasurePower<'T> list

// Represents: measure-simp
and MeasureSimp<'T> =
    | Seq of MeasureSeq<'T>
    | Product of left: MeasureSimp<'T> * star: 'T * right: MeasureSimp<'T>
    | Quotient of left: MeasureSimp<'T> * slash: 'T * right: MeasureSimp<'T>
    | Reciprocal of slash: 'T * measure: MeasureSimp<'T>
    | One of oneToken: 'T

// Represents: measure, the top-level type for type-level measures
and Measure<'T> =
    | Anon of underscore: 'T
    | Simple of measure: MeasureSimp<'T>

// Represents: const, simplified to leverage the rich Token type
[<RequireQualifiedAccess>]
type Constant<'T> =
    /// A simple literal token, such as `123`, `4.5f`, `"hello"`, or `true`.
    /// The specific kind of literal is encoded in the 'T token itself.
    | Literal of value: 'T

    /// A numeric literal followed by a unit of measure annotation.
    /// e.g., `10<kg>` or `9.8<m/s^2>`
    | MeasuredLiteral of value: 'T * lAngle: 'T * measure: MeasureLiteral<'T> * rAngle: 'T
