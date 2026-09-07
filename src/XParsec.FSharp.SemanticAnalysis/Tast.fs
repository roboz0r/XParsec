namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

// The UNIT-level TAST: what a whole compiled file carries. No trivia, parens or token
// layout survives elaboration, so a consumer wanting those reads the CST.

/// A splice TEMPLATE: an inline binding's retained declaration, plus `ParamAttrs` aligned
/// positionally to its curried parameters and empty when no parameter carries one.
type TInlineBodyG<'ty, 'tok, 'id> =
    {
        Decl: TDeclG<'ty, 'tok, 'id>
        ParamAttrs: EqArray<ParamAttrs>
    }

type TInlineValueG<'ty, 'tok, 'id> =
    {
        Key: SymbolKey
        Body: TInlineBodyG<'ty, 'tok, 'id>
    }

/// The template's identity plus the type arguments a call site grounds it at, in the
/// template's typar order.
type SpecializationKeyG<'ty> =
    {
        Template: SymbolKey
        TypeArgs: EqArray<'ty>
    }

/// One entry of a file's specialization table, addressed by the `SpecializationId` an
/// `InlineCall` carries. `Value` may itself contain an `InlineCall`, so the table is a DAG.
type TSpecializationG<'ty, 'tok, 'id> =
    {
        Key: SpecializationKeyG<'ty>
        /// The file every anchor inside `Pat` and `Value` is an index into, except under a
        /// nested `CallerExpr` / `InlineCall`, which carries its own.
        Path: AssemblyFilePath
        /// The entry's own bound variable, minted at the reduction.
        Pat: TPatG<'ty, 'tok, 'id>
        /// The lambda chain an edge's arguments are positional against.
        Value: TExprG<'ty, 'tok, 'id>
    }

type IntrinsicBindingInfo =
    {
        /// The target's identifier for the type (`Vesper.int` → `"System.Int32"`).
        TypeId: PlatformTypeId
        /// `(# class "System.Attribute" #)`-tagged: a derived file may `inherit` this
        /// primitive (`obj` / `exn` / `Attribute`). A scalar primitive (`int`) is `false`.
        Heritable: bool
    }

type TastFileG<'ty, 'tok, 'id when 'id: comparison> =
    {
        /// Source order, every module-level declaration, `inline` bindings INCLUDED, since
        /// one is emitted as an ordinary module function as well as spliced.
        Decls: EqArray<TDeclG<'ty, 'tok, 'id>>
        /// A diagnostic of error severity means the TAST is best-effort, not safe to emit from.
        Diagnostics: XParsec.FSharp.SemanticAnalysis.Diagnostic list
        /// This file's OWN intrinsics: the `SymbolKey` of a `type x = (# "…" #)` abbrev →
        /// its platform type id. Keyed by identity: a name cannot say WHICH `int` it means.
        IntrinsicBindings: EqDict<TypeKey, IntrinsicBindingInfo>
        /// The `[<Global>]` module-level bindings: values that ARE a target global (JS
        /// `undefined`), so the declaring file emits no definition for one.
        GlobalValueKeys: EqSet<SymbolKey>
        /// Every module this file declares, with what its declaration states: the class it
        /// emits as, `[<RequireQualifiedAccess>]`, `[<AutoOpen>]`.
        Modules: EqDict<ModuleKey, ModuleFacts>
        /// A module-level binding's bound variable → its named-module placement (`module Foo`'s
        /// functions emit on a real `Foo`/`FooModule` static class, not the anonymous
        /// "Program" class).
        ModuleMembers: Map<BoundVarKeyG<'id>, ModuleBindingInfo>
        /// A closure bound variable → its stack-vs-heap verdict. A bound variable absent here, and any
        /// anonymous lambda, is `Heap`.
        ClosureReprs: Map<BoundVarKeyG<'id>, ClosureRepr>
        /// A SOURCE-lambda argument's `LambdaKey` → its value-struct closure verdict; a
        /// lambda absent here is an ordinary curried closure. Keyed by lambda and not by
        /// bound variable because a lambda EXPRESSION is not a definition site.
        FunVerdicts: Map<LambdaKey, FunVerdict>
        /// A project-local generalised binding's bound variable → its typar scheme. A
        /// binding absent here quantifies nothing.
        GenericFnSchemes: Map<BoundVarKeyG<'id>, GenericFnScheme>
        /// Every generalised body-local `let` of the file → the declaration whose body
        /// declares it. A module-level `let` has no entry.
        LocalOwners: Map<LocalBindingId, LocalOwner>
        /// The file's INLINE VOCABULARY: every `let inline` binding and every
        /// nullary-intrinsic value alias (`let undefined = (# "undefined" #)`), as the
        /// UNEXPANDED body, a different tree from the decl of the same name. Empty pre-freeze.
        InlineBodies: EqArray<TInlineValueG<'ty, 'tok, 'id>>
        /// One entry per distinct (template, type-arguments) grounding this file's call
        /// sites reached. Resolved against THIS file's operand types, so it is consumed by
        /// the backends rather than exported.
        Specializations: EqArray<TSpecializationG<'ty, 'tok, 'id>>
        /// Declared accessibility of each top-level entity (type / module value / inline
        /// value); a key ABSENT here is `Public`. A type MEMBER's is stored on the member itself.
        Accessibility: EqDict<SymbolKey, Accessibility>
    }

// Monomorphic `SemType` aliases, over the `NodeKey` identity axis.

type TPat = TPatG<SemType, SyntaxToken, NodeKey>
type HoleSpec = HoleSpecG<SemType, SyntaxToken>
type TExpr = TExprG<SemType, SyntaxToken, NodeKey>
type TMatchArm = TMatchArmG<TPat, TExpr>
type FormatSink = FormatSinkG<TExpr>
type FormatSeg = FormatSegG<SemType, SyntaxToken, TExpr>
type DynFormatHole = DynFormatHoleG<SemType, SyntaxToken, TExpr>
type TStaticOptClause = TStaticOptClauseG<SemType, SyntaxToken, NodeKey>
type TLetMember = TLetMemberG<SemType, SyntaxToken, NodeKey>
type TDecl = TDeclG<SemType, SyntaxToken, NodeKey>
type TTypeDecl = TTypeDeclG<SemType, SyntaxToken, NodeKey, TExpr>
type TTypeKind = TTypeKindG<SemType, SyntaxToken, NodeKey, TExpr>
type TClass = TClassG<SemType, NodeKey, TExpr>
type TUnionCase = TUnionCaseG<SemType>
type TEnumCase = TEnumCaseG<SyntaxToken>
type TRecordField = TRecordFieldG<SemType>
type TTypeMember = TTypeMemberG<SemType, NodeKey, TExpr>
type TClassLet = TClassLetG<SemType, TExpr>
type TPreambleEntry = TPreambleEntryG<SemType, TExpr>
type TCtorLet = TCtorLetG<SemType, NodeKey, TExpr>
type TCtorFieldInit = TCtorFieldInitG<TExpr>
type TSecondaryCtor = TSecondaryCtorG<SemType, NodeKey, TExpr>
type TBase = TBaseG<SemType, NodeKey, TExpr>
type TBaseCtorCall = TBaseCtorCallG<SemType, NodeKey, TExpr>
type TAbstractMethod = TAbstractMethodG<SemType>
type TInlineBody = TInlineBodyG<SemType, SyntaxToken, NodeKey>
type TInlineValue = TInlineValueG<SemType, SyntaxToken, NodeKey>
type SpecializationKey = SpecializationKeyG<SemType>
type TSpecialization = TSpecializationG<SemType, SyntaxToken, NodeKey>
type TastFile = TastFileG<SemType, SyntaxToken, NodeKey>

[<RequireQualifiedAccess>]
module TPreambleEntryG =
    let lets (entries: seq<TPreambleEntryG<'ty, 'body>>) : TClassLetG<'ty, 'body> list =
        [
            for e in entries do
                match e with
                | TPreambleEntryG.Let l -> yield l
                | TPreambleEntryG.Do _ -> ()
        ]

// Parallel `FrozenType` aliases, shared by the freeze step and the backends.

module Frozen =
    type TPat = TPatG<FrozenType, SyntaxToken, NodeKey>
    type HoleSpec = HoleSpecG<FrozenType, SyntaxToken>
    type TExpr = TExprG<FrozenType, SyntaxToken, NodeKey>
    type TMatchArm = TMatchArmG<TPat, TExpr>
    type FormatSink = FormatSinkG<TExpr>
    type FormatSeg = FormatSegG<FrozenType, SyntaxToken, TExpr>
    type DynFormatHole = DynFormatHoleG<FrozenType, SyntaxToken, TExpr>
    type TStaticOptConstraint = TStaticOptConstraintG<FrozenType>
    type TStaticOptClause = TStaticOptClauseG<FrozenType, SyntaxToken, NodeKey>
    type TLetMember = TLetMemberG<FrozenType, SyntaxToken, NodeKey>
    type TDecl = TDeclG<FrozenType, SyntaxToken, NodeKey>
    type TTypeDecl = TTypeDeclG<FrozenType, SyntaxToken, NodeKey, TExpr>
    type TTypeKind = TTypeKindG<FrozenType, SyntaxToken, NodeKey, TExpr>
    type TClass = TClassG<FrozenType, NodeKey, TExpr>
    type TUnionCase = TUnionCaseG<FrozenType>
    // Enum cases are `'ty`-free, so the frozen alias is identical to the SemType one.
    type TEnumCase = TEnumCaseG<SyntaxToken>
    type TRecordField = TRecordFieldG<FrozenType>
    type TTypeMember = TTypeMemberG<FrozenType, NodeKey, TExpr>
    type TClassLet = TClassLetG<FrozenType, TExpr>
    type TPreambleEntry = TPreambleEntryG<FrozenType, TExpr>
    type TCtorLet = TCtorLetG<FrozenType, NodeKey, TExpr>
    type TCtorFieldInit = TCtorFieldInitG<TExpr>
    type TSecondaryCtor = TSecondaryCtorG<FrozenType, NodeKey, TExpr>
    type TBase = TBaseG<FrozenType, NodeKey, TExpr>
    type TBaseCtorCall = TBaseCtorCallG<FrozenType, NodeKey, TExpr>
    type TAbstractMethod = TAbstractMethodG<FrozenType>
    type TInlineBody = TInlineBodyG<FrozenType, SyntaxToken, NodeKey>
    type TInlineValue = TInlineValueG<FrozenType, SyntaxToken, NodeKey>
    type SpecializationKey = SpecializationKeyG<FrozenType>
    type TSpecialization = TSpecializationG<FrozenType, SyntaxToken, NodeKey>
    type TastFile = TastFileG<FrozenType, SyntaxToken, NodeKey>
    type ForInGetEnum = ForInGetEnumG<FrozenType>
    type ForInEnumMembers = ForInEnumMembersG<FrozenType>
    type ForInPattern = ForInPatternG<FrozenType>
    type ForInEnumerator = ForInEnumeratorG<FrozenType>
    // The TREE instantiation of the compiled-form cluster: `'pat` is the pattern node
    // itself, the form an EXTERNAL symbol carries. A file's own identifies a pooled pat by id.
    type StaticParam = StaticParamG<FrozenType, TPat, NodeKey>
    type ArgGroup = ArgGroupG<FrozenType, TPat, NodeKey>
    type ValRepr = ValReprG<FrozenType, TPat, NodeKey>
    type CompiledReturn = CompiledReturnG<FrozenType>
    type CompiledForm = CompiledFormG<FrozenType, TPat, NodeKey>
