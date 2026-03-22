namespace rec XParsec.FSharp.Parser

// 13. Custom Attributes and Reflection
// Represents: attribute-target
[<RequireQualifiedAccess>]
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
        attributes: (Attribute<'T> * 'T (* semicolon *) voption) list *
        rBracket: 'T // Represents the >] token

// Represents: attributes := attribute-set ... attribute-set
and Attributes<'T> = AttributeSet<'T> list

// Represents: ident-or-op
[<RequireQualifiedAccess>]
type IdentOrOp<'T> =
    | Ident of ident: 'T
    | ParenOp of lParen: 'T * opName: OpName<'T> * rParen: 'T
    | StarOp of lParen: 'T * star: 'T * rParen: 'T

// Represents: op-name and its variations
and [<RequireQualifiedAccess>] OpName<'T> =
    | SymbolicOp of op: 'T
    | RangeOp of rangeOp: RangeOpName<'T>
    | ActivePatternOp of activePatternOp: ActivePatternOpName<'T>

and [<RequireQualifiedAccess>] RangeOpName<'T> =
    | DotDot of 'T
    | DotDotDotDot of 'T

and ActivePatternOpName<'T> = | ActivePatternOp of lBar: 'T * idents: 'T list * finalUnderscore: 'T voption * rBar: 'T

// Represents: long-ident and long-ident-or-op
type LongIdent<'T> = 'T list

[<RequireQualifiedAccess>]
type LongIdentOrOp<'T> =
    | LongIdent of LongIdent<'T>
    | Op of IdentOrOp<'T>
    | QualifiedOp of longIdent: LongIdent<'T> * dot: 'T * op: IdentOrOp<'T>

// Represents: type and related grammar constructs
[<RequireQualifiedAccess>]
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
    | DottedType of baseType: Type<'T> * dot: 'T * longIdent: LongIdent<'T>
    | ArrayType of baseType: Type<'T> * lBracket: 'T * commas: 'T list * rBracket: 'T
    | ConstrainedType of typ: Type<'T> * constraints: TyparDefns<'T>
    | SubtypeConstraint of typar: Typar<'T> * colonGreaterThan: 'T * typ: Type<'T>
    | AnonymousSubtype of hash: 'T * typ: Type<'T>
    | Null of nullToken: 'T
    | UnionType of left: Type<'T> * bar: 'T * right: Type<'T>
    | Missing
    | SkipsTokens of skippedTokens: 'T list

and [<RequireQualifiedAccess>] TypeArg<'T> =
    | Type of Type<'T>
    | Measure of Measure<'T>
    | StaticParameter of 'T // Placeholder for static-parameter grammar

and [<RequireQualifiedAccess>] Typar<'T> =
    | Anon of underscore: 'T
    | Named of quote: 'T * ident: 'T
    | Static of caret: 'T * ident: 'T

and TyparDefns<'T> =
    | TyparDefns of lAngle: 'T * defns: TyparDefn<'T> list * constraints: TyparConstraints<'T> voption * rAngle: 'T

and TyparDefn<'T> = | TyparDefn of attributes: Attributes<'T> voption * typar: Typar<'T>
and TyparConstraints<'T> = | TyparConstraints of whenToken: 'T * constraints: Constraint<'T> list

and [<RequireQualifiedAccess>] Constraint<'T> =
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

and [<RequireQualifiedAccess>] StaticTypars<'T> =
    | Single of caret: 'T * ident: 'T
    | OrList of lParen: 'T * typars: (Typar<'T> * 'T (* or *) ) list * rParen: 'T

// Represents: elif-branch and else-branch
// Note: The spec doesn't mention 'else if' branches, but they are present
// in the language in that 'else if' can be interleaved with 'elif' at the same indentation level.
[<RequireQualifiedAccess>]
type ElifBranch<'T> =
    | Elif of elifToken: 'T * condition: Expr<'T> * thenToken: 'T * expr: Expr<'T>
    | ElseIf of elseToken: 'T * ifToken: 'T * condition: Expr<'T> * thenToken: 'T * expr: Expr<'T>

type ElseBranch<'T> = | ElseBranch of elseToken: 'T * expr: Expr<'T>

// Represents: function-or-value-defn and related grammar
type ReturnType<'T> = | ReturnType of colon: 'T * typ: Type<'T>

type Binding<'T> =
    {
        attributes: Attributes<'T> voption
        inlineToken: 'T voption
        mutableToken: 'T voption
        fixedToken: 'T voption
        access: 'T voption
        /// For values: the binding pattern (Pat.NamedSimple, Pat.Tuple, etc.)
        /// For functions with a plain name: Pat.NamedSimple
        /// For functions with an operator name: Pat.Op
        headPat: Pat<'T>
        typarDefns: TyparDefns<'T> voption
        /// Non-empty for function-style bindings; empty for value bindings
        argumentPats: Pat<'T> list
        returnType: ReturnType<'T> voption
        equals: 'T
        expr: Expr<'T>
    }

[<RequireQualifiedAccess>]
type LetOrUseKeyword<'T> =
    | Let of 'T
    | LetBang of 'T
    | Use of 'T
    | UseBang of 'T

// Represents: record, object, and interface implementation grammar
type FieldInitializer<'T> = | FieldInitializer of longIdent: LongIdent<'T> * equals: 'T * expr: Expr<'T>

type ObjectConstruction<'T> =
    | ObjectConstruction of typ: Type<'T> * expr: Expr<'T>
    | InterfaceConstruction of typ: Type<'T>

type BaseCall<'T> =
    | AnonBaseCall of construction: ObjectConstruction<'T>
    | NamedBaseCall of construction: ObjectConstruction<'T> * asToken: 'T * ident: 'T

type ObjectMembers<'T> = | ObjectMembers of withToken: 'T * memberDefns: MemberDefn<'T> list * endToken: 'T

type InterfaceImpl<'T> =
    | InterfaceImpl of interfaceToken: 'T * typ: Type<'T> * objectMembers: ObjectMembers<'T> voption

// Keywords used in control-flow CE forms: yield, return, do, and their bang variants
[<Struct; RequireQualifiedAccess>]
type ControlFlowKeyword<'T> =
    | Yield of 'T
    | YieldBang of 'T
    | Return of 'T
    | ReturnBang of 'T
    | Do of 'T
    | DoBang of 'T

[<Struct; RequireQualifiedAccess>]
type ParenKind<'T> =
    /// ( ... )
    | Paren of 'T
    /// begin ... end
    | BeginEnd of 'T
    /// [ ... ]
    | List of 'T
    /// [| ... |]
    | Array of 'T
    /// { ... }  (Used for Computation Expressions)
    | Brace of 'T
    /// &lt;@ ... @&gt; (Used for Quotations)
    | Quoted of 'T
    /// &lt;@@ ... @@&gt; (Used for Untyped Quotations)
    | DoubleQuoted of 'T

// The complete expression type
[<RequireQualifiedAccess>]
type Expr<'T> =
    // Constants and Blocks
    | Const of value: Constant<'T>
    | EnclosedBlock of lParen: ParenKind<'T> * expr: Expr<'T> * rParen: 'T
    | EmptyBlock of lParen: ParenKind<'T> * rParen: 'T
    // Lookups and Applications
    | LongIdentOrOp of longIdentOrOp: LongIdentOrOp<'T>
    | DotLookup of expr: Expr<'T> * dot: 'T * longIdentOrOp: LongIdentOrOp<'T>
    // Note: This is an optimization for multiple arguments; individual applications interspersed with type applications will be represented as multiple App nodes or as HighPrecedenceApp nodes.
    | App of funcExpr: Expr<'T> * argExprs: Expr<'T> list
    | HighPrecedenceApp of funcExpr: Expr<'T> * lParen: 'T * argExpr: Expr<'T> * rParen: 'T
    | TypeApp of expr: Expr<'T> * lAngle: 'T * types: Type<'T> list * rAngle: 'T
    | InfixApp of leftExpr: Expr<'T> * infixOp: 'T * rightExpr: Expr<'T>
    | PrefixApp of prefixOp: 'T * expr: Expr<'T>
    | IndexedLookup of expr: Expr<'T> * dot: 'T voption * lBracket: 'T * indexExpr: Expr<'T> * rBracket: 'T
    // Data Structures
    | Assignment of leftExpr: Expr<'T> * arrow: 'T * rightExpr: Expr<'T>
    | Tuple of exprs: Expr<'T> list
    | StructTuple of structToken: 'T * lParen: 'T * exprs: Expr<'T> list * rParen: 'T
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
    | ControlFlow of keyword: ControlFlowKeyword<'T> * expr: Expr<'T>
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
    | LetOrUse of
        keyword: LetOrUseKeyword<'T> *
        isRec: 'T voption *
        bindings: Binding<'T> list *
        inToken: 'T voption *  // ValueNone for UseFixed (no body follows)
        body: Expr<'T> voption
    // Functions and Matching
    | Fun of funToken: 'T * argumentPats: Pat<'T> list * arrow: 'T * expr: Expr<'T>
    | Function of functionToken: 'T * rules: Rules<'T>
    | Sequential of exprs: Expr<'T> list * semicolons: 'T list
    | Match of matchToken: 'T * matchExpr: Expr<'T> * withToken: 'T * rules: Rules<'T>
    | TryWith of tryToken: 'T * expr: Expr<'T> * withToken: 'T * rules: Rules<'T>
    | TryFinally of tryToken: 'T * tryExpr: Expr<'T> * finallyToken: 'T * finallyExpr: Expr<'T>
    // Control Flow
    | IfThenElse of
        ifToken: 'T *
        condition: Expr<'T> *
        thenToken: 'T *
        thenExpr: Expr<'T> *
        elifBranches: ElifBranch<'T> list *
        elseBranch: ElseBranch<'T> voption
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
        enumerableExpr: Expr<'T> *
        doToken: 'T *
        body: Expr<'T> *
        doneToken: 'T
    // Other
    | Assert of assertToken: 'T * expr: Expr<'T>
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
    | InterpolatedString of opening: 'T * parts: InterpolatedStringPart<'T> list * closing: 'T
    // Incomplete or Placeholder
    | Missing // Placeholder for missing expression
    | SkipsTokens of skippedTokens: 'T list // Placeholder for skipped tokens
    // Added to make things work
    | Ident of ident: 'T
    | Pat of pattern: Pat<'T>
    // Ranges and Slices Note: Spec has these as separate grammar productions, but they are closely related and it's easier to represent them with a single set of types.
    | Range of fromExpr: Expr<'T> * dotdot: 'T * toExpr: Expr<'T>
    | SteppedRange of fromExpr: Expr<'T> * dotdot1: 'T * stepExpr: Expr<'T> * dotdot2: 'T * toExpr: Expr<'T>
    | SliceFrom of expr: Expr<'T> * dotdot: 'T
    | SliceTo of dotdot: 'T * expr: Expr<'T>
    | SliceFromTo of startExpr: Expr<'T> * dotdot: 'T * endExpr: Expr<'T>
    | SliceAll of star: 'T


// Patterns


// Represents: field-pat := long-ident = pat
and FieldPat<'T> = | FieldPat of longIdent: LongIdent<'T> * equals: 'T * pat: Pat<'T>

// Represents: pat and its variations
and [<RequireQualifiedAccess>] Pat<'T> =
    | Const of value: Constant<'T>
    | EnclosedBlock of lParen: ParenKind<'T> * pat: Pat<'T> * rParen: 'T
    | EmptyBlock of lParen: ParenKind<'T> * rParen: 'T
    /// Only valid in List and Array patterns; a compiler error will be raised if this appears in a different context
    | Elems of pats: Pat<'T> list * separators: 'T list
    | NamedSimple of ident: 'T
    | Named of longIdent: LongIdent<'T> * param: Pat<'T> voption * pat: Pat<'T> voption
    | Wildcard of underscore: 'T
    | As of pat: Pat<'T> * asToken: 'T * ident: 'T
    | Or of left: Pat<'T> * bar: 'T * right: Pat<'T>
    | And of left: Pat<'T> * ampersand: 'T * right: Pat<'T>
    | Cons of head: Pat<'T> * consToken: 'T * tail: Pat<'T>
    | Typed of pat: Pat<'T> * colon: 'T * typ: Type<'T>
    | Tuple of patterns: Pat<'T> list * commas: 'T list
    | StructTuple of structToken: 'T * lParen: 'T * patterns: Pat<'T> list * commas: 'T list * rParen: 'T
    | Record of lBrace: 'T * fieldPats: FieldPat<'T> list * rBrace: 'T
    | TypeTest of colonQuestion: 'T * typ: Type<'T>
    | TypeTestAs of colonQuestion: 'T * typ: Type<'T> * asToken: 'T * ident: 'T
    | Null of nullToken: 'T
    | Attributed of attributes: Attributes<'T> * pat: Pat<'T>
    | Struct of structToken: 'T * pat: Pat<'T> // For error recovery
    | Optional of questionMark: 'T * pat: Pat<'T>
    | Op of IdentOrOp<'T> // For operator/active-pattern names in function binding heads
    | Missing
    | SkipsTokens of skippedTokens: 'T list

// Represents: pattern-guard := when expr
and PatternGuard<'T> = | PatternGuard of whenToken: 'T * expr: Expr<'T>

// Represents: rule := pat pattern-guard~opt -> expr
and [<RequireQualifiedAccess>] Rule<'T> =
    | Rule of pat: Pat<'T> * guard: PatternGuard<'T> voption * arrow: 'T * expr: Expr<'T>
    | Missing
    | SkipsTokens of skippedTokens: 'T list

// Represents: rules := '|'~opt rule '|' ... '|' rule
and Rules<'T> = | Rules of leadingBar: 'T voption * rules: Rule<'T> list * bars: 'T list


// 8 Type Definitions

// Below are the F# types that model the provided grammar for type definitions.

// Represents: primary-constr-args
type PrimaryConstrArgs<'T> =
    | PrimaryConstrArgs of
        attributes: Attributes<'T> voption *
        access: 'T voption *
        lParen: 'T *
        // Should be simple-pat, but we allow full patterns here for better error recovery and to simplify the parser; the compiler will report an error if the pattern is not simple.
        pat: Pat<'T> voption *
        rParen: 'T

// Represents: type-name
type TypeName<'T> =
    | TypeName of
        attributes: Attributes<'T> voption *
        access: 'T voption *
        ident: LongIdent<'T> *
        typarDefns: TyparDefns<'T> voption

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

and CurriedSig<'T> = | CurriedSig of args: (struct (ArgsSpec<'T> * 'T)) list * returnType: Type<'T>

and UncurriedSig<'T> = | UncurriedSig of args: ArgsSpec<'T> * arrow: 'T * returnType: Type<'T>

and [<RequireQualifiedAccess>] MemberSig<'T> =
    | MethodOrPropSig of ident: 'T * typarDefns: TyparDefns<'T> voption * colon: 'T * sign: CurriedSig<'T>
    | PropSig of
        ident: 'T *
        typarDefns: TyparDefns<'T> voption *
        colon: 'T *
        sign: CurriedSig<'T> *
        withToken: 'T *
        getSet: ('T * 'T voption) // (get, option<set>) or (set, option<get>)

// Represents: method-or-prop-defn
and [<RequireQualifiedAccess>] MethodOrPropDefn<'T> =
    | Method of ident: struct ('T * 'T) voption * defn: Binding<'T>
    | Property of ident: struct ('T * 'T) voption * defn: Binding<'T>
    | PropertyWithGetSet of identPrefix: struct ('T * 'T) voption * ident: 'T * withToken: 'T * defns: Binding<'T> list
    | AutoProperty of
        valToken: 'T *
        access: 'T voption *
        ident: 'T *
        returnType: ReturnType<'T> voption *
        equals: 'T *
        expr: Expr<'T> *
        withClause: struct ('T * 'T * 'T voption) voption // with, get/set, optional comma and other get/set
    | AbstractSignature of sign: MemberSig<'T>

and [<RequireQualifiedAccess>] MemberKeyword<'T> =
    | Member of 'T
    | Override of 'T
    | Default of 'T
    | Abstract of abstractToken: 'T * memberToken: 'T voption

// Represents: additional-constr-defn and its expression body
and [<RequireQualifiedAccess>] AdditionalConstrExpr<'T> =
    | SequenceAfter of stmt: 'T * semicolon: 'T * rest: AdditionalConstrExpr<'T> // Placeholder for 'stmt'
    | SequenceBefore of before: AdditionalConstrExpr<'T> * thenToken: 'T * expr: Expr<'T>
    | Conditional of
        ifToken: 'T *
        cond: Expr<'T> *
        thenToken: 'T *
        thenBranch: AdditionalConstrExpr<'T> *
        elseToken: 'T *
        elseBranch: AdditionalConstrExpr<'T>
    | LetIn of letToken: 'T * binding: Binding<'T> * inToken: 'T * body: AdditionalConstrExpr<'T>
    | Init of initExpr: AdditionalConstrInitExpr<'T>

and AdditionalConstrInitExpr<'T> =
    | Explicit of
        lBrace: 'T *
        inherits: ClassInheritsDecl<'T> voption *
        initializers: FieldInitializer<'T> list *
        rBrace: 'T
    | Delegated of newToken: 'T * typ: Type<'T> * expr: Expr<'T>
    | Expression of expr: Expr<'T>

and AsDefn<'T> = | AsDefn of asToken: 'T * ident: 'T

// Represents: member-defn
and [<RequireQualifiedAccess>] MemberDefn<'T> =
    | Member of
        attributes: Attributes<'T> voption *
        staticToken: 'T voption *
        keyword: MemberKeyword<'T> *
        inlineToken: 'T voption *
        access: 'T voption *
        defn: MethodOrPropDefn<'T>
    | Value of
        attributes: Attributes<'T> voption *
        staticToken: 'T voption *
        valToken: 'T *
        mutableToken: 'T voption *
        access: 'T voption *
        ident: 'T *
        colon: 'T *
        typ: Type<'T>
    | AdditionalConstructor of
        attributes: Attributes<'T> voption *
        access: 'T voption *
        newToken: 'T *
        pat: Pat<'T> *
        asDefn: AsDefn<'T> voption *
        equals: 'T *
        body: AdditionalConstrExpr<'T>

// Represents: class, struct, and interface bodies
and ClassInheritsDecl<'T> = | ClassInheritsDecl of inheritToken: 'T * typ: Type<'T> * expr: Expr<'T> voption

and ClassFunctionOrValueDefn<'T> =
    | LetBindings of
        attributes: Attributes<'T> voption *
        staticToken: 'T voption *
        letToken: 'T *
        isRec: 'T voption *
        bindings: Binding<'T> list
    | Do of attributes: Attributes<'T> voption * staticToken: 'T voption * doToken: 'T * expr: Expr<'T>

// Unified body type for class, struct, and interface definitions.
// For structs and interfaces: inherits=ValueNone, classPreamble=[]
and ObjectModelBody<'T> =
    {
        inherits: ClassInheritsDecl<'T> voption
        classPreamble: ClassFunctionOrValueDefn<'T> list
        elements: TypeDefnElement<'T> list
    }

// Represents: union-type-defn and its cases
and [<RequireQualifiedAccess>] UnionTypeField<'T> =
    | Unnamed of typ: Type<'T>
    | Named of ident: 'T * colon: 'T * typ: Type<'T>

and [<RequireQualifiedAccess>] UnionTypeCaseData<'T> =
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
and EnumTypeCase<'T> = | EnumTypeCase of ident: 'T * equals: 'T * constValue: Expr<'T>

and EnumTypeCases<'T> = EnumTypeCase<'T> list

// Represents: type-extension
and TypeExtensionElements<'T> = | TypeExtensionElements of withToken: 'T * elements: TypeDefnElements<'T> * endToken: 'T

// Represents: exception-defn
and [<RequireQualifiedAccess>] ExceptionDefn<'T> =
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
and [<RequireQualifiedAccess>] TypeDefn<'T> =
    | Abbrev of typeName: TypeName<'T> * equals: 'T * typ: Type<'T>
    | Record of
        typeName: TypeName<'T> *
        equals: 'T *
        lBrace: 'T *
        fields: RecordFields<'T> *
        rBrace: 'T *
        extensions: TypeExtensionElements<'T> voption
    | Union of
        typeName: TypeName<'T> *
        equals: 'T *
        cases: UnionTypeCases<'T> *
        extensions: TypeExtensionElements<'T> voption
    | Anon of
        typeName: TypeName<'T> *
        primaryConstr: PrimaryConstrArgs<'T> voption *
        objectVal: 'T voption *  // Placeholder
        equals: 'T *
        beginToken: 'T *
        body: ObjectModelBody<'T> *
        endToken: 'T
    | Class of
        typeName: TypeName<'T> *
        primaryConstr: PrimaryConstrArgs<'T> voption *
        objectVal: 'T voption *  // Placeholder
        equals: 'T *
        classToken: 'T *
        body: ObjectModelBody<'T> *
        endToken: 'T
    | Struct of
        typeName: TypeName<'T> *
        primaryConstr: PrimaryConstrArgs<'T> voption *
        asDefn: AsDefn<'T> voption *
        equals: 'T *
        structToken: 'T *
        body: ObjectModelBody<'T> *
        endToken: 'T
    | Interface of typeName: TypeName<'T> * equals: 'T * interfaceToken: 'T * body: ObjectModelBody<'T> * endToken: 'T
    | Enum of typeName: TypeName<'T> * equals: 'T * cases: EnumTypeCases<'T>
    | Delegate of typeName: TypeName<'T> * equals: 'T * sign: DelegateSig<'T>
    | TypeExtension of typeName: TypeName<'T> * elements: TypeExtensionElements<'T>
    | AbstractType of typeName: TypeName<'T>
    | Missing
    | SkipsTokens of skippedTokens: 'T list


// 9 Units of Measure
// Below are the F# types that model the provided grammar for units of measure.
// Represents: measure and measure-literal (simplified to a single type)
[<RequireQualifiedAccess>]
type Measure<'T> =
    // Base cases
    | Named of LongIdent<'T> // e.g., kg or Microsoft.FSharp.SI.kg
    | One of oneToken: 'T // 1 (Dimensionless)
    | Anonymous of underscore: 'T // _ (The wildcard)

    // Type variable, permitted on type-level `measure` but not `measure-literal`s
    | Typar of Typar<'T> // e.g., 'u

    // Recursive operations
    | Juxtaposition of Measure<'T> list * ops: 'T list // e.g., m s (implicit multiplication)
    | Power of Measure<'T> * power: 'T * int: 'T // e.g., m^2
    | Product of Measure<'T> * prod: 'T * Measure<'T> // e.g., m * s
    | Quotient of Measure<'T> * div: 'T * Measure<'T> // e.g., m / s
    | Reciprocal of div: 'T * Measure<'T> // e.g., / s
    | Paren of l: 'T * Measure<'T> * r: 'T // e.g., (m / s)

// Represents: const, simplified to leverage the rich Token type
[<RequireQualifiedAccess>]
type Constant<'T> =
    /// A simple literal token, such as `123`, `4.5f`, `"hello"`, or `true`.
    /// The specific kind of literal is encoded in the 'T token itself.
    | Literal of value: 'T

    /// A numeric literal followed by a unit of measure annotation.
    /// e.g., `10<kg>` or `9.8<m/s^2>`
    | MeasuredLiteral of value: 'T * lAngle: 'T * measure: Measure<'T> * rAngle: 'T

/// A part of an interpolated string — either a text fragment or an expression hole.
and [<RequireQualifiedAccess>] InterpolatedStringPart<'T> =
    /// A literal text fragment, escaped brace, or format specifier between expression holes.
    | Text of 'T
    /// An expression hole: {expr}, optionally preceded by a format specifier like %4i.
    | Expr of formatSpecifier: 'T voption * lBrace: 'T * expr: Expr<'T> * rBrace: 'T
    /// A format specifier (e.g. %d) not immediately followed by an expression hole.
    | OrphanFormatSpecifier of 'T
    /// A lexer error token inside the string (e.g. UnmatchedInterpolatedRBrace, InvalidFormatPlaceholder).
    | InvalidText of 'T
